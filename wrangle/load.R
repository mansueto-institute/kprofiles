library(ggplot2)
library(sf)
library(tidyverse)
library(viridis)
library(patchwork)
library(scales)
library(tidygeocoder)
library(readxl)
library(units)
library(ggsn)
library(arrow)
library(sfarrow)
library(osmdata)
ghsl_delin = TRUE


load <- function() {
  # Grab link to zip file
  assign("ghsl_url", 'http://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_STAT_UCDB2015MT_GLOBE_R2019A/V1-2/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.zip',
         envir = .GlobalEnv)
  
  # Define future file path with new created folder (ghsl) that will be in temp directory
  assign("filedir", paste0(tempdir(), '/ghsl/'),
         envir = .GlobalEnv)
  # Delete the file path
  unlink(filedir, recursive = TRUE)
  # Create the above directory/folder
  dir.create(filedir)
  
  # Define the URL as a character object
  assign("ghsl_dir", paste0(ghsl_url),
         envir = .GlobalEnv)
  # Download the zipped data finally
  download.file(url = ghsl_url, destfile = paste0(filedir, basename(ghsl_dir)))
  # Unzip the data
  unzip(paste0(filedir,basename(ghsl_dir)), exdir= filedir)
  
  assign("city_data", read_csv(list.files(path = paste0(filedir,
                                                 list.files(path = filedir)[1]), 
                                   full.names = TRUE, all.files = TRUE)[6]),
         envir = .GlobalEnv)
  
  
  # Get UN pop data for Angola
  assign("location_url", 'https://population.un.org/wpp/Download/Files/4_Metadata/WPP2019_F01_LOCATIONS.XLSX',
         envir = .GlobalEnv)
  assign("tmp_filepath", paste0(tempdir(), '/', basename(location_url)),
         envir = .GlobalEnv)
  download.file(url = paste0(location_url), destfile = tmp_filepath)
  assign("location_codes", read_xlsx(path = tmp_filepath, sheet = 'Location', range = "A17:H306"),
         envir = .GlobalEnv)
  
  assign("location_codes", location_codes %>%
    dplyr::select_all(~gsub("\\s+|\\.|\\/|,|\\*|-", "_", .)) %>%
    dplyr::rename_all(list(tolower)) %>%
    dplyr::filter(name == 'Country/Area') %>%
    dplyr::rename(country_name = region__subregion__country_or_area_) %>%
    dplyr::select(location_code, country_name, iso3_alpha_code),
    envir = .GlobalEnv)
  
  assign("un_population", read_csv('https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv'),
         envir = .GlobalEnv)

}



