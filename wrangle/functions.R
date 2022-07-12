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


load_data <- function() {
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
  
  
  # Get UN pop data
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





load_country <- function(country_code, country_label, city_label) {
  
  # Load in main files for country

  assign("block", 
         sfarrow::st_read_parquet(paste0("./blocks/blocks_", country_code, ".parquet")),
         envir = .GlobalEnv)
  assign("complexity", arrow::read_parquet(paste0("./complexity/complexity_", country_code, ".parquet")),
         envir = .GlobalEnv)
  if (ghsl_delin == TRUE) {
    assign("ghsl_xwalk", arrow::read_parquet(paste0("./ghsl/", country_code, "_ghsl.parquet")),
           envir = .GlobalEnv)
  }
  assign("population", arrow::read_parquet(paste0("./population/population_", country_code, ".parquet")),
         envir = .GlobalEnv)
  
  print("done with block, complexity, etc.")
  

  assign("city_data", city_data %>% 
    dplyr::filter(CTR_MN_ISO == country_code) %>% 
    dplyr::mutate(rank = row_number(desc(P15))) %>% 
    dplyr::ungroup() %>%
    dplyr::filter(rank <= 3) %>%
    dplyr::filter(rank == 1 | P15 >= 500000) %>%
    dplyr::select(CTR_MN_ISO, CTR_MN_NM, UC_NM_MN, P15, GCPNT_LAT, GCPNT_LON) %>%
    sf::st_as_sf(coords = c("GCPNT_LON", "GCPNT_LAT"), 
                 crs = 4326, agr = "constant") %>%
    dplyr::rename(country_iso = CTR_MN_ISO,
                  country_name = CTR_MN_NM, 
                  city_name = UC_NM_MN,
                  ghsl_pop15 = P15),
    envir = .GlobalEnv)
  
  # Load in un population data
  assign("un_population", un_population %>%
    dplyr::filter(Location == country_label) %>%
    dplyr::select_all(~gsub("\\s+|\\.|\\/|,|\\*", "_", .)) %>%
    dplyr::rename_all(list(tolower)) %>%
    dplyr::filter(time == 2020,
                  variant == 'Medium') %>%
    dplyr::mutate(poptotal = poptotal * 1000,
                  popdensity = ((popdensity*.3861)/100)*1000) %>%
    dplyr::inner_join(., location_codes, by = c('locid' = 'location_code')) %>%
    dplyr::select(locid, country_name, 
                  iso3_alpha_code, location, 
                  time, variant, 
                  poptotal, popdensity),
    envir = .GlobalEnv)
  
  # Compile dataframes for country
  assign("block", block %>%
    dplyr::left_join(un_population %>%
                       select(iso3_alpha_code, poptotal, popdensity),
                     by = c('country_code' = 'iso3_alpha_code')),
    envir = .GlobalEnv)
  
  assign("data", block %>%
    dplyr::left_join(x =., 
                     y = complexity %>%
                       select(block_id, block_area, 
                              building_area, building_count, 
                              building_layers, k_complexity),
                     by = c('block_id' = 'block_id')) %>%
    dplyr::left_join(x =.,
                     y = population %>%
                       select(block_id, 
                              landscan_population, 
                              worldpop_population),
                     by = c('block_id' = 'block_id')),
    envir = .GlobalEnv)
  
  assign("data", data %>%
    dplyr::group_by(country_code) %>%
    dplyr::mutate(landscan_population_total = sum(landscan_population),
                  worldpop_population_total = sum(worldpop_population)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(landscan_population = round(poptotal * landscan_population/landscan_population_total,0),
                  worldpop_population = round(poptotal * worldpop_population/worldpop_population_total,0)) %>%
    dplyr::select(-one_of(c('landscan_population_total',
                            'worldpop_population_total',
                            'poptotal','popdensity'))) %>%
    dplyr::mutate(landscan_population_log = replace_na(na_if((log10(landscan_population)), -Inf), 1),
                  landscan_population_log = ifelse(landscan_population_log < 1, 
                                                   1, landscan_population_log),
                  worldpop_population_log = replace_na(na_if((log10(worldpop_population)), -Inf),1),
                  worldpop_population_log = ifelse(worldpop_population_log < 1, 
                                                   1, worldpop_population_log)) %>%
    dplyr::mutate(block_hectares = na_if((block_area*0.0001), 0)) %>%
    tidyr::replace_na(list(block_hectares = 0)) %>% 
    dplyr::mutate(landscan_pop_density_hectare = landscan_population/block_hectares,
                  worldpop_pop_density_hectare = worldpop_population/block_hectares) %>%
    dplyr::mutate(landscan_pop_density_hectare_log = replace_na(na_if((log10(landscan_pop_density_hectare)),
                                                                      -Inf), 1),
                  landscan_pop_density_hectare_log = ifelse(landscan_pop_density_hectare_log < 1, 
                                                            1, landscan_pop_density_hectare_log),
                  worldpop_pop_density_hectare_log = replace_na(na_if((log10(worldpop_pop_density_hectare)),
                                                                      -Inf), 1),
                  worldpop_pop_density_hectare_log = ifelse(worldpop_pop_density_hectare_log  < 1, 
                                                            1, worldpop_pop_density_hectare_log )),
    envir = .GlobalEnv)
  
  
  assign("data_sum", data %>% 
    st_drop_geometry() %>%
    dplyr::mutate(k_complexity = as.factor(as.integer(k_complexity)),
                  block_hectares = na_if((block_area*0.0001), 0)) %>%
    tidyr::replace_na(list(block_hectares = 0)) %>%
    dplyr::group_by(k_complexity) %>%
    dplyr::summarize_at(vars(block_hectares, landscan_population, worldpop_population), 
                        list(sum),
                        na.rm = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(landscan_pop_density_hectare = landscan_population/block_hectares,
                  worldpop_pop_density_hectare = worldpop_population/block_hectares) %>%
    tidyr::replace_na(list(block_pop_density_hectare = 0)) %>%
    dplyr::mutate(landscan_population_sum = sum(landscan_population),
                  worldpop_population_sum = sum(worldpop_population),
                  landscan_population_share = landscan_population/landscan_population_sum,
                  worldpop_population_share = worldpop_population/worldpop_population_sum),
    envir = .GlobalEnv)
  # return(list(block, complexity, population, city_data, un_population, data, data_sum))
}


bounds <- function(city_label) {
  
  if (ghsl_delin == FALSE) {
    
    assign("target_area", city_data %>% 
      filter(country_iso %in% iso_code) %>%
      filter(city_name == city_label) %>%
      st_transform(4326) %>%
      st_transform(3857) %>%
      st_buffer(., 25000) %>% st_bbox(.) %>% st_as_sfc(),
      envir = .GlobalEnv)
    
    assign("blocks_centroids", blocks %>% st_make_valid() %>% 
      st_transform(3857) %>%
      st_centroid(),
      envir = .GlobalEnv)
    
    assign("zoom_zone", blocks_centroids %>%
      st_make_valid() %>%
      st_transform(3857) %>%
      mutate(in_zone = ifelse(sf::st_intersects(., target_area, sparse = F), "Yes", "No")) %>% 
      filter(in_zone == 'Yes') %>%
      st_intersection(., target_area),
      envir = .GlobalEnv)
    
    assign("zoom_zone", data %>% filter(block_id %in% unique(zoom_zone$block_id)),
           envir = .GlobalEnv)
  } else {
    assign("data", data %>% 
      left_join(., ghsl_xwalk %>% 
                  select(block_id, urban_center), by = c('block_id'='block_id')) %>% 
      filter(urban_center %in% c(city_label)),
      envir = .GlobalEnv)
    
    assign("target_area", st_bbox(data %>% st_transform(4326))  %>% st_as_sfc(),
           envir = .GlobalEnv)
    assign("zoom_zone", data,
           envir = .GlobalEnv)
  }
  
  
}



# Visualize country -------------------------------------------------------

viz_variables <- function() {
  assign("road_color", '#ffffff',
         envir = .GlobalEnv)
  assign("grey2", c('#414141','#777777'),
         envir = .GlobalEnv)

  assign("kdist", max(as.integer(data$k_complexity)),
         envir = .GlobalEnv)

  assign("colorhexes", colorRampPalette(c('#93328E','#CF3F80',
                                   '#F7626B','#FF925A',
                                   '#FFC556','#F9F871'))(length(unique(data$k_complexity))-2),
         envir = .GlobalEnv)
  assign("country_bbox", st_bbox(data),
  envir = .GlobalEnv)
}

  
  
 
plot_k_discrete <- function(data) {

  ggplot() + 
    geom_sf(data = data, 
            aes(fill = as.factor(as.integer(k_complexity))), 
            color = alpha(c(road_color), .9), 
            size = .005) +
    scale_fill_manual(values = c(grey2,colorhexes), 
                      name = 'k complexity') +
    labs(subtitle = '',
         caption = paste0('Population weighted average k complexity in area: ',
                          data %>% 
                            st_drop_geometry() %>% 
                            summarise(wm_var = weighted.mean(as.integer(k_complexity), 
                                                             landscan_population)) %>% 
                            pull() %>% 
                            round(.,2))) +
    theme_void() + 
    theme(plot.caption = element_text(size = 9, hjust = .5, vjust = 10,margin=margin(0,0,0,0)),
          text = element_text(color = "#333333"),
          legend.position = c(1.1,.5),
          legend.key.height= unit(13, 'pt'),
          legend.key.width= unit(13, 'pt'),
          legend.text = element_text(size = 8),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.margin=unit(c(t=0,r=40,b=0,l=0), "pt"),
          legend.title = element_text( face="bold", size = 10),
          axis.text = element_blank()) +
    ggsn::scalebar(y.min = st_bbox(data)$ymin - .003, 
                   x.min = st_bbox(data)$xmin,
                   y.max = st_bbox(data)$ymax, 
                   x.max = st_bbox(data)$xmax, 
                   location = 'bottomleft',
                   height = .01, 
                   box.fill = c('#333333','#ffffff'),
                   border.size = .4, 
                   st.color = '#333333', 
                   st.size = 2.5, 
                   box.color = '#333333',
                   dist = round(((country_bbox$xmax - country_bbox$xmin)*.1)/1000,-1), 
                   dist_unit = "km", transform = TRUE, model = "WGS84")
}
  
bar_k_distrib <- function(data) {

  ggplot(data_sum) +
    geom_bar(aes(y = landscan_population, x = k_complexity, fill = k_complexity),
             position="dodge",  
             stat="identity") +
    geom_text(aes(x = k_complexity, y =landscan_population,
                  label = ifelse(landscan_population_share > .01,
                                 paste0(round(landscan_population_share*100,0),"%"),'')),
              size = 3, 
              vjust =-.5, 
              color = '#333333', 
              fontface='bold') +
    scale_fill_manual(values = c(grey2, colorhexes)) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 6),
                       expand = expansion(mult = c(0, .1)),
                       limits = c(0, max(data_sum$landscan_population)),
                       labels = label_comma(accuracy = 1L, scale = .001, suffix = "K") ) +
    theme_bw() + 
    labs(y = 'Population', x = 'k complexity', subtitle = '') + #'Population distribution across k-complexity levels'
    theme(text = element_text(color = "#333333"),
          legend.position= "none",
          #plot.margin = unit(c(1,1,1,1),"cm"),
          axis.ticks =element_blank(),
          axis.text = element_text(size = 11),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.title = element_text(face="bold", size = 11),
          plot.subtitle = element_text(size = 15, face="bold", hjust=.5))
}
  
plot_populaton <- function(data) {
  ggplot() +
    geom_sf(data = data,
            aes(fill = landscan_population_log ), 
            color = alpha(c(road_color), .9), size = .005, alpha = .8) +
    labs(subtitle = '',
         caption = paste0('Total population in area: ',comma(sum(data$landscan_population)),'  |  ',
                          'Average block population: ',comma(round(mean(data$landscan_population),2)))) +
    scale_fill_viridis(name = 'Population', oob = scales::squish, limits= c(1, max(data$landscan_population_log )), 
                       breaks= c(1,2,3,4,5,6,7), labels = c('0',"100","1K","10K","100K","1M","10M")) +
    scale_color_viridis(name = 'Population', 
                        oob = scales::squish, 
                        limits= c(1, max(data$landscan_population_log )), 
                        breaks= c(1,2,3,4,5,6,7), 
                        labels = c('0',"100","1K","10K","100K","1M","10M")) +
    theme_void() +
    theme(plot.subtitle = element_text(size = 14, 
                                       face="bold", 
                                       vjust = -4,
                                       hjust=.5),
          axis.text = element_blank(),
          plot.caption = element_text(size = 9, 
                                      hjust = .5, 
                                      vjust = 7),
          legend.position = c(1.07,.8),
          plot.margin=unit(c(t=0,r=50,b=0,l=5), "pt"),
          legend.title = element_text( face="bold"),
          text = element_text(color = "#333333")) +
    ggsn::scalebar(y.min = st_bbox(data)$ymin - .003,
                   x.min = st_bbox(data)$xmin,
                   y.max = st_bbox(data)$ymax, 
                   x.max = st_bbox(data)$xmax, 
                   location = 'bottomleft',
                   height = .01, 
                   box.fill = c('#333333','#ffffff'),
                   border.size = .4, 
                   st.color = '#333333', 
                   st.size = 2.5, 
                   box.color = '#333333',
                   dist = round(((country_bbox$xmax - country_bbox$xmin)*.1)/1000,-1), 
                   dist_unit = "km",
                   transform = TRUE, model = "WGS84")
  
}

  

  

plot_popdensity_log <- function(data) {
  sd3 = log10(mean(data$landscan_pop_density_hectare) + sd(data$landscan_pop_density_hectare)*5)
  ggplot() +
    geom_sf(data = data,
            aes(fill = landscan_pop_density_hectare_log),
            color = alpha(c(road_color), .9), 
            size = .005, 
            alpha = .8) +
    labs(subtitle = '',
         caption = paste0('Weighted average population density: ',
                          data %>% st_drop_geometry() %>% 
                            summarize(pop_dense = weighted.mean(landscan_pop_density_hectare,
                                                                landscan_population) ) %>% 
                            pull() %>% 
                            round(.,0),
                          ' people per hectare','\n  1 hectare = 10k m^2 = 1.4 soccer fields = 2.2 Manhattan city blocks')) +
    scale_fill_viridis(name = 'Population\nper hectare', 
                       oob = scales::squish, 
                       limits= c(1, sd3), 
                       breaks= c(1,1.477121,2,2.60206,3,3.69897,4,5,6,7), 
                       labels = c('0','30',"100",'400',"1K","5K","10K","100K","1M","10M")) +
    scale_color_viridis(name = 'Population\nper hectare', 
                        oob = scales::squish, 
                        limits= c(1, sd3), 
                        breaks= c(1,1.477121,2,2.60206,3,3.69897,4,5,6,7), 
                        labels = c('0','30',"100",'400',"1K","5K","10K","100K","1M","10M")) +
    theme_void() +
    theme(plot.subtitle = element_text(size = 14, 
                                       face="bold", 
                                       vjust = -4, 
                                       hjust=.5),
          plot.caption = element_text(size = 9, 
                                      hjust = .5, 
                                      vjust = 7),
          legend.position = c(1.07,.8),
          plot.margin=unit(c(t=0,r=70,b=0,l=5), "pt"),
          axis.text = element_blank(),
          legend.title = element_text( face="bold"),
          text = element_text(color = "#333333")) +
    ggsn::scalebar(y.min = st_bbox(data)$ymin - .003,
                   x.min = st_bbox(data)$xmin,
                   y.max = st_bbox(data)$ymax, 
                   x.max = st_bbox(data)$xmax, 
                   location = 'bottomleft',
                   height = .01, 
                   box.fill = c('#333333','#ffffff'),
                   border.size = .4, 
                   st.color = '#333333', 
                   st.size = 2.5, 
                   box.color = '#333333',
                   dist = round(((country_bbox$xmax - country_bbox$xmin)*.1)/1000,-1), 
                   dist_unit = "km", 
                   transform = TRUE, 
                   model = "WGS84")
}

  
plot_k <- function(plot_k_discrete, bar_k_distrib) {
  plot_k_discrete + bar_k_distrib +
    plot_layout(widths = c(1, 1))  +
    plot_annotation(#title = paste0(country_name),
      subtitle = paste0(country_name),
      theme = theme(#plot.title = element_text(face="bold", size = 18, vjust = -2, hjust = .5),
        plot.subtitle = element_text(face="bold", 
                                     size = 13, 
                                     vjust = -8, 
                                     hjust = .5)))

  ggsave(plot = plot_k, filename = paste0('./plotk_', country_name, '.pdf'), 
         dpi = 600, 
         height = 6, 
         width = 12)
  
}
  
  
  
plot_pop <- function(plot_populaton, plot_popdensity_log) {
  plot_populaton + 
    plot_popdensity_log +
    plot_layout(widths = c(1, 1)) +
    plot_annotation(#title = paste0(country_name ),
      subtitle = paste0(country_name ),
      theme = theme(#plot.title = element_text(face="bold", size = 18, vjust = -2, hjust = .5),
        plot.subtitle = element_text(face="bold", 
                                     size = 13, 
                                     vjust = -8, 
                                     hjust = .5)))
  
  ggsave(plot = plot_pop, filename = paste0('./plotp_', country_name, '.pdf'), 
         dpi = 600, 
         height = 7, 
         width = 12)
  
}


