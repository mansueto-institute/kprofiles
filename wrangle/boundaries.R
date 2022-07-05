source("./load.R")

zoom_zone <- zoom_zone %>% 
  dplyr::mutate(k_complexity_groups = case_when(k_complexity >= 11 & k_complexity <= 15 ~ "11-15",
                                                k_complexity >= 16 & k_complexity <= 20 ~ "16-20",
                                                k_complexity >= 21 ~ "21+",
                                                TRUE ~ as.character(k_complexity))) %>% 
  dplyr::arrange(k_complexity)

k_order = unique(zoom_zone$k_complexity_groups)
zoom_zone <- zoom_zone %>% 
  dplyr::mutate(k_complexity_groups = factor(k_complexity_groups,levels = k_order)) 

if (ghsl_delin == FALSE) {
  zoom_zone <- zoom_zone %>% st_intersection(., target_area %>% st_transform(4326))
  #zoom_zone_area = zoom_zone %>% select(geometry) %>% st_union() %>% st_transform(3857) %>% st_area()*1e-6 
  #units(zoom_zone_area) <- NULL
}

plot(zoom_zone %>% select(block_id))

zoom_zone_sum <- zoom_zone %>% st_drop_geometry() %>%
  dplyr::mutate(#k_complexity = as.factor(as.integer(k_complexity)),
    k_complexity = k_complexity_groups,
    block_hectares = na_if((block_area*0.0001), 0)) %>%
  tidyr::replace_na(list(block_hectares = 0)) %>%
  dplyr::group_by(k_complexity) %>%
  dplyr::summarize_at(vars(block_hectares, landscan_population, worldpop_population), list(sum), na.rm = TRUE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(landscan_pop_density_hectare = landscan_population/block_hectares,
                worldpop_pop_density_hectare = worldpop_population/block_hectares) %>% 
  tidyr::replace_na(list(block_pop_density_hectare = 0)) %>%
  dplyr::mutate(landscan_population_sum = sum(landscan_population),
                worldpop_population_sum = sum(worldpop_population),
                landscan_population_share = landscan_population/landscan_population_sum,
                worldpop_population_share = worldpop_population/worldpop_population_sum) 

# Water data --------------------------------------------------------------

aoi_box <- target_area %>% st_transform(4326)

# Download OSM water features 
water <- opq(bbox = st_bbox(aoi_box), memsize = 1e+9) %>%
  add_osm_feature(key = 'water') %>%
  osmdata_sf() 

# Download OSM waterway features
waterway <- opq(bbox = st_bbox(aoi_box), memsize = 1e+9) %>%
  add_osm_feature(key = 'waterway') %>%
  osmdata_sf() 

# Download OSM coastline features
coastline <- opq(bbox = st_bbox(aoi_box), memsize = 1e+9) %>%
  add_osm_feature(key = 'natural', value = 'coastline') %>%
  osmdata_sf() %>% pluck("osm_lines") %>% rename(feature = natural) %>% 
  dplyr::select(feature, geometry)

# Download water features
natural_water <- opq(bbox = st_bbox(aoi_box), memsize = 1e+9) %>%
  add_osm_feature(key = 'natural',
                  value = c('water')) %>%
  osmdata_sf() 

# water_multipolygons <- water$osm_multipolygons %>% 
#   rename(feature = water) %>% 
#   dplyr::select(feature, geometry)
water_polygons <- water$osm_polygons %>% 
  rename(feature = water) %>% 
  dplyr::select(feature, geometry)
# water_lines <- water$osm_lines %>% 
#   rename(feature = water) %>% 
#   dplyr::select(feature, geometry)

# waterway_multipolygons <- waterway$osm_multipolygons %>% rename(feature = waterway) %>% dplyr::select(feature, geometry)
waterway_polygons <- waterway$osm_polygons  %>% 
  rename(feature = waterway) %>% 
  dplyr::select(feature, geometry)
waterway_lines <- waterway$osm_lines %>% 
  rename(feature = waterway) %>% 
  dplyr::select(feature, geometry)
waterway_multilines <- waterway$osm_multilines %>%
  rename(feature = waterway) %>% 
  dplyr::select(feature, geometry)

natural_water_multipolygons <- natural_water$osm_multipolygons %>% 
  rename(feature = natural) %>%
  dplyr::select(feature, geometry)
natural_water_polygons <- natural_water$osm_polygons %>% 
  rename(feature = natural) %>%
  dplyr::select(feature, geometry)

# Parse and combine water linestrings and polygons
water_poly <- rbind(get0('natural_water_multipolygons'),get0('natural_water_polygons'),
                    get0("water_multipolygons"),get0("water_polygons"),
                    get0("waterway_multipolygons"),get0("waterway_polygons")) %>%
  st_intersection(.,st_as_sfc(st_bbox(aoi_box))) %>%
  st_transform(4326) %>% dplyr::select(feature, geometry) %>%
  filter(!is.na(feature))

water_line <- rbind(get0('coastline'), get0("water_lines"), 
                    get0("waterway_lines"), get0("waterway_multilines")) %>%
  st_intersection(.,st_as_sfc(st_bbox(aoi_box))) %>%
  st_transform(4326) %>% dplyr::select(feature, geometry)



