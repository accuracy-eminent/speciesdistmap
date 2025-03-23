library(pacman)
pacman::p_load(rgbif, geodata, tidyverse, viridis)

# This was tested on R 4.4.2

raster_path_folder <- tempdir()

# Function is from plyr
round_any <- function(x, accuracy, f=round){f(x/ accuracy) * accuracy}


round_lat_lon <- function(df, res){
  # Resolution is decimal
  out_df <- df %>%
    mutate(lat_round = round_any(lat, res)) %>%
    mutate(lon_round = round_any(lon, res))
  return(out_df)
}

# Example of getting data for a taxon
# occ_search(scientificName="Populus deltoides",hasCoordinate = TRUE, limit=10)$data

get_species_occ <- function(scientific_name, limit=1000){
  #' Get distribution data (latitude and longitude) from GBIF
  #' 
  #' @description Gets a data frame of latitude and longitude observations of a given species from GBIF
  #' 
  #' @param scientific_name String. The scientific name of the species to find observations of.
  #' @param limit Numeric. How many observations to get. Maximum of 100,000 per the GBIF API.
  #'
  #' @return A data.frame of the observations, containing the scientific name, latitude, and longitude
  #' 
  #' @example
  #' # Get 5000 observations of Populus deltoides and save to a data frame
  #' df <- get_species_occ("Populus deltoides", limit=5000)
  

  
  df <- occ_search(scientificName = scientific_name, hasCoordinate = TRUE, limit = limit)$data %>%
    as_tibble() %>%
    dplyr::select(key, scientificName, decimalLatitude, decimalLongitude) %>%
    rename(lat=decimalLatitude, lon=decimalLongitude)
  return(df)
}

get_clim_data <- function(res=10){
  raster_stack <-  worldclim_global("bio",path=raster_path_folder, version="2.1", res=res)
  raster_df <- as.data.frame(raster_stack, xy=TRUE)
  colnames(raster_df) <- gsub('wc[0-9m._]*', '', colnames(raster_df), fixed=FALSE)
  colnames(raster_df) <- gsub('x','lon', colnames(raster_df), fixed=TRUE)
  colnames(raster_df) <- gsub('y','lat', colnames(raster_df), fixed=TRUE)
  # Add additional variables
  # De Martonne aridity index
  raster_df <- raster_df %>% mutate(bio_20 = clamp(bio_12 / (bio_1 + 10), 0, 187))
  return(raster_df)
}
# raster_path_folder = tempdir()
# raster_stack <- worldclim_global("bio",path=raster_path_folder, version="2.1", res=2.5)

clim_and_species <- function(clim_data, species){
  # Convert to Z scores
  bio_vars <- clim_data %>% select(starts_with("bio_")) %>% colnames()
  for(bio_var in bio_vars){
    clim_data[sprintf('%s_%s',bio_var,'z')] <- scale(clim_data[bio_var], center=TRUE, scale=TRUE)
  }
  occ_data <- get_species_occ(species, limit=1000) %>% round_lat_lon(0.1667)
  merge_out <- merge(clim_data, occ_data, by=c("lat_round","lon_round"),all=FALSE)
  return(tibble(merge_out))
}

calc_suitability_map <- function(clim_data, suitability_df){
  suitability_map_df <- clim_data %>% mutate(score=0)
  bio_vars <- clim_data %>% select(starts_with("bio_")) %>% colnames()
  for(i in 1:20){
    suitability_map_df[sprintf('bio_%d_sz',i)] = 0
  }
  for(bio_var in bio_vars){
    s_mean <- suitability_df %>% filter(var_name == bio_var) %>% head(1) %>% pull(species_mean)
    s_stdev <- suitability_df %>% filter(var_name == bio_var) %>% head(1) %>% pull(species_sd)
    suitability_map_df[sprintf('%s_sz',bio_var)] <- (suitability_map_df[bio_var] - s_mean)/s_stdev
  }
  suitability_map_df <- suitability_map_df %>%
    mutate(score = 1/pmax(
      abs(bio_1_sz), abs(bio_2_sz), abs(bio_3_sz), 
      abs(bio_4_sz), abs(bio_5_sz), abs(bio_6_sz),
      abs(bio_7_sz), abs(bio_8_sz), abs(bio_9_sz),
      abs(bio_10_sz), abs(bio_11_sz), abs(bio_12_sz),
      abs(bio_13_sz), abs(bio_14_sz), abs(bio_15_sz),
      abs(bio_16_sz), abs(bio_17_sz), abs(bio_18_sz),
      abs(bio_19_sz), abs(bio_20_sz)
    ))
  
  # Convert to raster
  points <- vect(suitability_map_df, geom=c("lon","lat"))
  r <- rast(res=1/6, ext=ext(-180, 180, -90, 90), crs="EPSG:4326")
  rasterized <- clamp(rasterize(points, r, field="score"), -Inf, 10)
  return(rasterized)
}


# Example

example_func <- function(){
# Load in the data 
clim_data <- get_clim_data() %>% round_lat_lon(0.1667)
# Convert to Z scores
bio_vars <- clim_data %>% select(starts_with("bio_")) %>% colnames()
for(bio_var in bio_vars){
  clim_data[sprintf('%s_%s',bio_var,'z')] <- scale(clim_data[bio_var], center=TRUE, scale=TRUE)
}
# Combine data
species <- "Carnegeia gigantea"
species <- "Populus deltoides"
species <- "Alligator mississippiensis"
species <- "Schizachyrium scoparium"
occ_data <- get_species_occ(species, limit=1000) %>% round_lat_lon(0.1667)
merge_out <- merge(clim_data, occ_data, by=c("lat_round","lon_round"),all=FALSE)


# Choose a specific location to compare against
loc_coords <- c(42.717, -84.593)
#loc_coords <- c(-6.378, -57.667)
#loc_coords = c(43.000, -99.995)
loc_data <- clim_data %>% 
  filter(lat_round==round_any(loc_coords[1],  0.1667), lon_round==round_any(loc_coords[2],0.1667)) %>%
  head(1)

# Combine the data together
combined_data <- merge_out %>% 
  pivot_longer(cols=starts_with("bio_")) %>% 
  rowwise() %>% 
  mutate(loc_value=loc_data[[name]]) %>% 
  ungroup()


# Plot a box plot
ggplot(combined_data %>% filter(grepl("bio_[0-9]+_z$",name))) +
  geom_boxplot(aes(x=name,y=value)) +
  geom_point(aes(x=name, y=loc_value),color='red',shape='x') +
  theme_bw()

# Calculate Z score of location compared to bioclimatic variable
# Mean annual temperature
print((loc_data$bio_1_z - mean(merge_out$bio_1_z)) / sd(merge_out$bio_1_z))
# Mean annual preciptitation
print((loc_data$bio_12_z - mean(merge_out$bio_12_z)) / sd(merge_out$bio_12_z))
# Aridity index
print((loc_data$bio_20_z - mean(merge_out$bio_20_z)) / sd(merge_out$bio_20_z))
}

clim_data <- get_clim_data() %>% round_lat_lon(0.1667)
