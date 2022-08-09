graphics_for_vis <- function(country_name) {
  df_subset <- df_combined_prep %>% 
    filter(country_name == country_name)
  
  i <- df_subset[["country_code"]][1]
  print(i)
  
  if (file.exists(paste0('./blocks/blocks_', i, '.parquet'))) {
    blocks <- st_read_parquet(paste0('./blocks/blocks_', i, '.parquet'))
  }
  # df_combined_v2 <- df_combined_v2 %>% 
  #   filter(st_is_valid(geometry))
  
  df_subset <- df_subset %>% 
    select(country_code, country_name, conurbation_area_name_short, geometry) %>% 
    filter(st_is_valid(geometry))
    
  
  # df_subset <- df_combined_v2 %>% 
  #   filter(country_name.x == country_name) %>% 
  #   select(country_code.x, country_name.x, conurbation_area_name_short, geometry) %>% 
  #   rename("country_code" = country_code.x, "country_name" = country_name.x, 
  #          "conurbation" = conurbation_area_name_short)
  

  print(head(df_subset))
  df_subset <- st_as_sf(df_subset) %>% 
    st_transform(4326) %>%
    st_transform(3857) %>%
    st_buffer(., 25000) %>% st_bbox(.) %>% st_as_sfc()
  print(head(df_subset))

  assign("df_subset", df_subset, envir = .GlobalEnv)
  

  blocks_centroids <- blocks %>% st_make_valid() %>% 
    st_transform(3857) %>%
    st_centroid() %>% 
    filter(st_is_valid(geometry))
  
  zoom_zone <- blocks_centroids %>%
    st_make_valid() %>%
    st_transform(3857) %>%
    mutate(in_zone = ifelse(sf::st_intersects(., df_subset, sparse = F), "Yes", "No")) %>% 
    filter(in_zone == 'Yes') %>%
    st_intersection(., df_subset) # changed to df_subset
  
  zoom_zone <- df_combined_prep %>% filter(block_id %in% unique(zoom_zone$block_id))
  
  zoom_zone <- zoom_zone %>% 
    mutate(k_complexity_groups = case_when(k_complexity >= 11 & k_complexity <= 15 ~ "11-15",
                                           k_complexity >= 16 & k_complexity <= 20 ~ "16-20",
                                           k_complexity >= 21 ~ "21+",
                                           TRUE ~ as.character(k_complexity))) %>% 
    arrange(k_complexity)
    
  k_order = unique(zoom_zone$k_complexity_groups)
  zoom_zone <- zoom_zone %>% 
    mutate(k_complexity_groups = factor(k_complexity_groups,levels = k_order))
  
  zoom_zone <- st_as_sf(zoom_zone)
  
  assign("zoom_zone", zoom_zone, envir = .GlobalEnv)
  
  zoom_zone_sum <- zoom_zone %>% st_drop_geometry() %>%
    mutate(#k_complexity = as.factor(as.integer(k_complexity)),
      k_complexity = k_complexity_groups,
      block_hectares = na_if((block_area*0.0001), 0)) %>%
    replace_na(list(block_hectares = 0)) %>%
    group_by(k_complexity) %>%
    summarize_at(vars(block_hectares, landscan_population, worldpop_population), list(sum), na.rm = TRUE) %>%
    ungroup() %>%
    mutate(landscan_pop_density_hectare = landscan_population/block_hectares,
           worldpop_pop_density_hectare = worldpop_population/block_hectares) %>% 
    replace_na(list(block_pop_density_hectare = 0)) %>%
    mutate(landscan_population_sum = sum(landscan_population),
           worldpop_population_sum = sum(worldpop_population),
           landscan_population_share = landscan_population/landscan_population_sum,
           worldpop_population_share = worldpop_population/worldpop_population_sum)
  
  assign("zoom_zone_sum", zoom_zone_sum, envir = .GlobalEnv)
  
}


graphics_for_water <- function() {
  # Water data --------------------------------------------------------------

  aoi_box <- df_subset %>% 
    st_transform(4326)
  print("AOI done")
  # Download OSM water features 
  water <- opq(bbox = st_bbox(aoi_box), memsize = 1e+9) %>%
    opq(timeout = 50) %>% 
    add_osm_feature(key = 'water') %>%
    osmdata_sf() 
  print("Water done")
  
  # Download OSM waterway features
  waterway <- opq(bbox = st_bbox(aoi_box), memsize = 1e+9) %>%
    opq(timeout = 50) %>% 
    add_osm_feature(key = 'waterway') %>%
    osmdata_sf() 
  print("Waterway done")
  
  # Download OSM coastline features
  coastline <- opq(bbox = st_bbox(aoi_box), memsize = 1e+9) %>%
    opq(timeout = 50) %>% 
    add_osm_feature(key = 'natural', value = 'coastline') %>%
    osmdata_sf() %>% pluck("osm_lines") %>% rename(feature = natural) %>% 
    dplyr::select(feature, geometry)
  print("Coastline done")
  
  # Download water features
  natural_water <- opq(bbox = st_bbox(aoi_box), memsize = 1e+9) %>%
    opq(timeout = 50) %>% 
    add_osm_feature(key = 'natural',
                    value = c('water')) %>%
    osmdata_sf() 
  print("Coastline done")
  
  water_multipolygons <- water$osm_multipolygons %>% rename(feature = water) %>% dplyr::select(feature, geometry)
  water_polygons <- water$osm_polygons %>% rename(feature = water) %>% dplyr::select(feature, geometry)
  water_lines <- water$osm_lines %>% rename(feature = water) %>% dplyr::select(feature, geometry)
  
  waterway_multipolygons <- waterway$osm_multipolygons %>% rename(feature = waterway) %>% dplyr::select(feature, geometry)
  waterway_polygons <- waterway$osm_polygons  %>% rename(feature = waterway) %>% dplyr::select(feature, geometry)
  waterway_lines <- waterway$osm_lines %>% rename(feature = waterway) %>% dplyr::select(feature, geometry)
  waterway_multilines <- waterway$osm_multilines  %>% rename(feature = waterway) %>% dplyr::select(feature, geometry)
  
  natural_water_multipolygons <- natural_water$osm_multipolygons %>% rename(feature = natural)  %>% dplyr::select(feature, geometry)
  natural_water_polygons <- natural_water$osm_polygons %>% rename(feature = natural)  %>% dplyr::select(feature, geometry)
  
  # Parse and combine water linestrings and polygons
  water_poly <- rbind(get0('natural_water_multipolygons'),get0('natural_water_polygons'),
                      get0("water_multipolygons"),get0("water_polygons"),
                      get0("waterway_multipolygons"),get0("waterway_polygons")) 
  
  water_poly <- st_as_sf(water_poly) %>% 
    filter(st_is_valid(geometry)) %>% 
    st_intersection(.,st_as_sfc(st_bbox(aoi_box))) %>%
    st_transform(4326) %>% dplyr::select(feature, geometry) %>%
    filter(!is.na(feature))

  assign("water_poly", water_poly, envir = .GlobalEnv)
  
  water_line <- rbind(get0('coastline'), get0("water_lines"), 
                      get0("waterway_lines"), get0("waterway_multilines"))
  
  water_line <- st_as_sf(water_line) %>% 
    filter(st_is_valid(geometry)) %>% 
    st_intersection(.,st_as_sfc(st_bbox(aoi_box))) %>%
    st_transform(4326) %>% dplyr::select(feature, geometry)
  
  assign("water_line", water_line, envir = .GlobalEnv)
}


# Visualize zoom area -----------------------------------------------------

graphics_for_colors <- function() {
  assign("road_color", '#ffffff', envir = .GlobalEnv)
  assign("grey2", c('#414141','#777777'), envir = .GlobalEnv)
  assign("kdist", max(as.integer(zoom_zone$k_complexity_groups)), envir = .GlobalEnv)
  assign("colorhexes", colorRampPalette(c('#93328E','#CF3F80','#F7626B','#FF925A','#FFC556','#F9F871'))(length(unique(zoom_zone$k_complexity_groups))-2), envir = .GlobalEnv)
  
  assign("width", st_distance(st_sf(geom = st_sfc(st_point(c(st_bbox(zoom_zone)$xmin, st_bbox(zoom_zone)$ymin)),
                                          st_point(c(st_bbox(zoom_zone)$xmax, st_bbox(zoom_zone)$ymin)), crs = 4326)))[2] %>%  drop_units(), envir = .GlobalEnv)
  assign("height", st_distance(st_sf(geom = st_sfc(st_point(c(st_bbox(zoom_zone)$xmin, st_bbox(zoom_zone)$ymin)),
                                           st_point(c(st_bbox(zoom_zone)$xmin, st_bbox(zoom_zone)$ymax)), crs = 4326)))[2] %>%  drop_units(), envir = .GlobalEnv)
  assign("width_tenth", round((width*.2)/1000,-1), envir = .GlobalEnv)
  if (width_tenth < 1) {
    assign("width_tenth", round((width*.2)/1000,0), envir = .GlobalEnv)
  }
  assign("height_decdegs", abs(unname(st_bbox(zoom_zone)$ymax) - unname(st_bbox(zoom_zone)$ymin)), envir = .GlobalEnv)
  
  assign("sd_int", log10(mean(zoom_zone$landscan_pop_density_hectare, na.rm = TRUE) + sd(zoom_zone$landscan_pop_density_hectare, na.rm = TRUE)*1), envir = .GlobalEnv)
}



plot_k_discrete <- function() {
  ggplot() +
    geom_sf(data = zoom_zone, aes(fill = as.factor(k_complexity_groups)), color = road_color, size = .0075) +   
    geom_sf(data = water_poly, fill = 'white', color = 'white', size = .1) +
    geom_sf(data = water_line, fill = 'white', color = 'white', size = .1) +
    scale_fill_manual(values = c(grey2,colorhexes), name = 'k complexity') + 
    labs(caption = paste0('Population-weighted average k complexity: ',zoom_zone %>% st_drop_geometry() %>% summarise(wm_var = weighted.mean(as.integer(k_complexity), landscan_population)) %>% pull() %>% round(.,2))) +
    guides(color = guide_legend(nrow = 1, label.position = "bottom", keywidth = 2, keyheight = 1),
           fill =  guide_legend(nrow = 1, label.position = "bottom", keywidth = 2, keyheight = 1)) +
    theme_void() + theme(plot.caption = element_text(size = 11, hjust = .5, vjust = 20, margin=margin(0,0,0,0)),
                         text = element_text(color = "#333333"),
                         #legend.position = c(1.05,.7),
                         legend.position = 'bottom',
                         legend.spacing.x = unit(1, 'pt'),
                         #legend.key.height = unit(10, 'pt'), 
                         #legend.key.width = unit(10, 'pt'),
                         legend.text = element_text(size = 10),
                         panel.border = element_blank(),
                         panel.background = element_blank(),
                         plot.margin=unit(c(t=0,r=10,b=0,l=0), "pt"),
                         legend.title = element_blank(),
                         axis.text = element_blank()) +
    ggsn::scalebar(y.min = st_bbox(zoom_zone)$ymin - (height_decdegs*.03), 
                   x.min = st_bbox(zoom_zone)$xmin, 
                   y.max = st_bbox(zoom_zone)$ymax, 
                   x.max = st_bbox(zoom_zone)$xmax, 
                   location = 'bottomleft',
                   height = .01, box.fill = c('#333333','#ffffff'),
                   border.size = .4, st.color = '#333333', st.size = 2.5, box.color = '#333333',
                   dist = width_tenth/2, dist_unit = "km", transform = TRUE, model = "WGS84") 
}





bar_k_distrib <- function() {
  ggplot(zoom_zone_sum) +
    geom_bar(aes(y = landscan_population, x = k_complexity, fill = k_complexity), 
             position="dodge",  stat="identity") +
    geom_text(aes(x = k_complexity, y =landscan_population, 
                  label = ifelse(landscan_population_share > .01, paste0(round(landscan_population_share*100,0),"%"),'')),
              size = 3, vjust =-.5, color = '#333333', fontface='bold') +
    scale_fill_manual(values = c(grey2, colorhexes)) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 6),
                       expand = expansion(mult = c(0, .1)),
                       limits = c(0, max(zoom_zone_sum$landscan_population)),
                       labels = label_comma(accuracy = 1L, scale = .001, suffix = "K") ) +
    theme_bw() + 
    labs(y = 'Population', x = 'k complexity', subtitle = '') + #'Population distribution across k-complexity levels'
    theme(text = element_text(color = "#333333"),
          legend.position= "none",
          plot.margin=unit(c(t=0,r=5,b=0,l=5), "pt"),
          axis.ticks =element_blank(),
          axis.text = element_text(size = 11),
          axis.text.x = element_text(size = 9),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.title = element_text(face="bold", size = 11),
          plot.subtitle = element_text(size = 15, face="bold", hjust=.5))
}



plot_populaton <- function() {
  ggplot() +
    geom_sf(data = zoom_zone,
            aes(fill = landscan_population_log ), 
            color = 'white', size= .0075, alpha = .8) +
    geom_sf(data = water_poly, fill = 'white', color = 'white', size = .1) +
    geom_sf(data = water_line, fill = 'white', color = 'white', size = .1) +
    labs(subtitle = "Population",
         caption = paste0('Total population in area: ',comma(sum(zoom_zone$landscan_population)),'  |  ',
                          'Average block population: ',comma(round(mean(zoom_zone$landscan_population),2)))) + 
    # scale_fill_viridis(name = 'Population', oob = scales::squish, limits= c(1, max(zoom_zone$landscan_population_log )), breaks= c(1,2,3,4,5,6,7), labels = c('0',"100","1K","10K","100K","1M","10M")) + 
    # scale_color_viridis(name = 'Population', oob = scales::squish, limits= c(1, max(zoom_zone$landscan_population_log )), breaks= c(1,2,3,4,5,6,7), labels = c('0',"100","1K","10K","100K","1M","10M")) +
    scale_fill_distiller(palette = 'Spectral', name = 'Population', oob = scales::squish, limits= c(1, max(zoom_zone$landscan_population_log )), breaks= c(1,2,3,4,5,6,7), labels = c('0',"100","1K","10K","100K","1M","10M")) + 
    scale_color_distiller(palette = 'Spectral', oob = scales::squish, limits= c(1, max(zoom_zone$landscan_population_log )), breaks= c(1,2,3,4,5,6,7), labels = c('0',"100","1K","10K","100K","1M","10M")) +
    theme_void() + 
    #guides(color = guide_legend(title.position="top", title.hjust = 0.5),
    #       fill = guide_legend(title.position="top", title.hjust = 0.5)) +
    theme(#plot.subtitle = element_text(size = 14, face="bold", vjust = -4, hjust=.5),axis.text = element_blank(),
      plot.caption = element_text(size = 10, hjust = .5, vjust = 5),
      #legend.position = c(1.05,.7),
      #legend.position = 'bottom',
      #legend.position = 'top',
      #legend.position = c(.5, .01),
      legend.position = c(.5, 1),
      legend.direction = "horizontal",
      legend.key.width=unit(40,"pt"),
      legend.key.height=unit(5,"pt"),
      plot.margin = unit(c(t=15,r=0,b=0,l=0), "pt"),
      plot.subtitle = element_text(size = 11, face="bold", vjust = 5, hjust = .5),
      legend.title = element_blank(),
      #legend.title = element_text(face="bold", hjust = .5),
      text = element_text(color = "#333333")) +
    ggsn::scalebar(y.min = st_bbox(zoom_zone)$ymin - (height_decdegs*.04), 
                   x.min = st_bbox(zoom_zone)$xmin, y.max = st_bbox(zoom_zone)$ymax, x.max = st_bbox(zoom_zone)$xmax, 
                   location = 'bottomleft',
                   height = .01, box.fill = c('#333333','#ffffff'),
                   border.size = .4, st.color = '#333333', st.size = 2.5, box.color = '#333333',
                   dist = width_tenth/2, dist_unit = "km", transform = TRUE, model = "WGS84")
}


plot_popdensity_log <- function() {
  ggplot() +
    geom_sf(data = zoom_zone,
            aes(fill = landscan_pop_density_hectare_log), 
            color = 'white', size= .0075, alpha = .8) +
    geom_sf(data = water_poly, fill = 'white', color = 'white', size = .1) +
    geom_sf(data = water_line, fill = 'white', color = 'white', size = .1) +
    labs(subtitle = "Population per hectare",
         caption = paste0('Weighted average population density: ',
                          zoom_zone %>% st_drop_geometry() %>% summarize(pop_dense = weighted.mean(landscan_pop_density_hectare, landscan_population) ) %>% pull() %>% round(.,0),
                          ' people per hectare','\n 1 hectare = 10k m^2 = 1.4 soccer fields = 2.2 Manhattan city blocks')) +
    # scale_fill_viridis(name = 'Population\nper hectare', oob = scales::squish, limits= c(1, 3), 
    #                    breaks= c(1,2,3,4,5,6,7), 
    #                    labels = c('0',"100","1K","10K","100K","1M","10M")) +
    # scale_color_viridis(name = 'Population\nper hectare', oob = scales::squish, limits= c(1, 3), 
    #                     breaks= c(1,2,3,4,5,6,7), 
    #                     labels = c('0',"100","1K","10K","100K","1M","10M")) +
    scale_fill_distiller(direction = -1, palette = 'Spectral', name = 'Population\nper hectare', oob = scales::squish, limits= c(1, 3), 
                         breaks= c(1,2,3,4,5,6,7), 
                         labels = c('0',"100","1K","10K","100K","1M","10M")) +
    scale_color_distiller(direction = -1, palette = 'Spectral', name = 'Population\nper hectare', oob = scales::squish, limits= c(1, 3), 
                          breaks= c(1,2,3,4,5,6,7), 
                          labels = c('0',"100","1K","10K","100K","1M","10M")) +
    theme_void() + 
    theme(#plot.subtitle = element_text(size = 14, face="bold", vjust = -4, hjust=.5),
      #plot.caption = element_text(size = 10, hjust = .5, vjust = 25),
      plot.caption = element_text(size = 10, hjust = .5, vjust = 5),
      #legend.position = c(1.05,.7),
      #legend.position = 'top',
      legend.position = c(.5, 1),
      legend.direction = "horizontal",
      #legend.position = c(.5, .01),
      legend.key.width=unit(40,"pt"),
      legend.key.height=unit(5,"pt"),
      plot.margin=unit(c(t=15,r=0,b=0,l=0), "pt"),
      #axis.text = element_blank(),
      plot.subtitle = element_text(size = 11, face="bold", vjust = 5, hjust = .5),
      legend.title = element_blank(),
      #legend.title = element_text(face="bold", hjust = .5),
      text = element_text(color = "#333333")) +
    ggsn::scalebar(y.min = st_bbox(zoom_zone)$ymin - (height_decdegs*.04), 
                   x.min = st_bbox(zoom_zone)$xmin, y.max = st_bbox(zoom_zone)$ymax, x.max = st_bbox(zoom_zone)$xmax, 
                   location = 'bottomleft',
                   height = .01, box.fill = c('#333333','#ffffff'),
                   border.size = .4, st.color = '#333333', st.size = 2.5, box.color = '#333333',
                   dist = width_tenth/2, dist_unit = "km", transform = TRUE, model = "WGS84")
}



# if (ghsl_delin == TRUE) {
#   full_city = '_city'
# } else {
#   full_city = ''
# }

# ----
plot_k <- function() {
  plot_k_discrete + bar_k_distrib +#+ plot_spacer() 
    plot_layout(widths = c(1,.8))  + # , height = c(1.5, 1) .01 ,
    plot_annotation(#title = paste0(city_name,', ', country_name),
      subtitle = paste0(city_label,', ', country_label),
      theme = theme(#plot.title = element_text(face="bold", size = 18, vjust = -2, hjust = .5),
        plot.subtitle = element_text(face="bold", size = 13, vjust = -7, hjust = .5)))
  
}


plot_pop <- function() {
  plot_populaton + plot_popdensity_log +
    plot_layout(widths = c(1, 1)) +
    plot_annotation(#title = paste0(city_name,', ', country_name ),
      subtitle = paste0(city_label,', ', country_label),
      theme = theme(#plot.title = element_text(face="bold", size = 18, vjust = -2, hjust = .5),
        plot.subtitle = element_text(face="bold", size = 13, vjust = -5, hjust = .5)))
}




# Visualize conurbation ---------------------------------------------------


#' df_ghsl3 <- df_ghsl2  %>% 
#'   group_by(block_id) %>% 
#'   filter(n()>1)
#' 
#' df_ghsl3 %>% group_by(country_code) %>% tally()
#' 
#' #'BDI' = 34, 'COD' = 202, 'ETH' = 1109, 'MOZ' = 24, 'NGA' = 120, 'TCD' = 90, 'TZA' = 528
#' 
#' #Onitsha — Amaigbo — Oboama Nguru — Owerri — Asaba — Awka — Osina — Umuahia — Ibi — Ata — Lawmu Umunze — Ozu — Umuellem — Amaba — Umu Lawlaw — Ogwashi Uku — Okwuohia — Omo — Orogwe — Itaja — Umu Oye — Avodim — Umuokorola — Umu Ezegu (core urban)
#' #Lagos — Cotonou — Ikorodu — Abeokuta — Porto-Novo — Ogunrun-Ori — Ijebu-Ode — Badagry — Sagamu — Epe — Lakuwe — Whydah — Ago-Iwoye — Ijebu-Igbo — Ilaro — Owode — Idiroko — Ilishan — Allada (core urban)
#' #Uyo — Ibiaku Ikot Ukpong — Ediene — Nsukara Offot — Ukap Itak — Mbikpong Atai — Ikot Obioko (peripheral urban)
#' 
#' rm(df_ghsl)
#' df_ghsl <- read_parquet('/Users/nm/Downloads/dev-ghsl/ghsl_crosswalk.parquet')
#' 
#' rm(df_ago)
#' df_ago <- read_parquet(list.files('/Users/nm/Downloads/dev-ghsl/ghsl_partitioned/country_code=AGO', full.names = TRUE))
#' 
#' names(df_ago)
#' 
#' test <- read_parquet('/Users/nm/Downloads/dev-ghsl/ghsl_crosswalk.parquet',
#'                      col_select = starts_with("b")) # c(block_id))
#' 
#' 
#' ?vars_select
#' 
#' df_xwalk <- df_ghsl %>% 
#'   filter(conurbation_id %in% c('1262'))# '1097') #
#' 
#' conurb_codes <- unique(df_xwalk$country_code)
#' 
#' df_conurb_full <- purrr::map_dfr(.x = conurb_codes, .f = function(i) {
#'   print(i)
#'   df_conurb <-  st_read_parquet(paste0('/Users/nm/Downloads/outputs/blocks/blocks_',i,'.parquet'))
#' })
#' 
#' df_conurb_subset <- df_conurb_full %>% 
#'   inner_join(., df_xwalk, by = c('block_id'='block_id'))
#' 
#' ggplot() +
#'   geom_sf(data = df_conurb_subset, aes(color = area_type_4, fill = area_type_4))
#' 
#' 
#' 
#' 
#' 
#' 
#' 
