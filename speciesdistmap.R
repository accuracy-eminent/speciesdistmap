library(pacman)
pacman::p_load(tidyverse, rgbif, geodata, raster)

# This was tested on R 4.4.2

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
  
  df <- (occ_search(scientificName = scientific_name, hasCoordinate = TRUE, limit = limit)$data) %>%
    select(key, scientificName, decimalLatitude, decimalLongitude) %>%
    rename(lat=decimalLatitude, lon=decimalLongitude)
  return(df)
}

# raster_path_folder = tempdir()
# raster_stack <- worldclim_global("bio",path=raster_path_folder, version="2.1", res=2.5)

