# vis functions ---------------------------------------------------------------

area_4 = c("Core urban","Peripheral urban","Peri-urban","Non-urban")
# continent viz----------------------------------------------------------------

x_pop_k_complexity_point <- function(country_name) {
  country_dviz <- out_k_0 %>% 
    filter(country_name == name, geography_group == "2c - Country by urban hierarchy") %>%
    mutate(urban_hierarchy = factor(urban_hierarchy, levels = area_4)) %>% 
    mutate(k_reweight = landscan_population_un*k_complexity_weighted_landscan) %>%
    group_by(country_name) %>%
    mutate(k_reweight = sum(k_reweight)) %>%
    ungroup() %>%
    mutate(k_reweight = k_reweight/area_landscan_population_un)

  
  # X population Size k-complexity for sub-Saharan Africa
  (africa_dots_xpop_sizek <- ggplot() +
      geom_point(data = country_dviz %>% filter(landscan_population_un >= 10000), 
                 aes(x = reorder(country_name, (area_landscan_population_un)), y = log10(landscan_population_un),
                     fill = urban_hierarchy, color = urban_hierarchy, size = k_complexity_weighted_landscan), alpha = .75) +
      scale_size(range = c(1,10)) + 
      geom_text(data = country_dviz %>% filter(landscan_population_un >= 10000), 
                aes(x =  reorder(country_name, (area_landscan_population_un)), y = log10(landscan_population_un), label = round(k_complexity_weighted_landscan , 1)), size = 3,
                vjust = .5, color = '#333333', fontface='bold', check_overlap = TRUE) +
      scale_y_continuous(oob = scales::squish, breaks= c(1,2,3,4,5,6,7,8,9), labels = c('0',"100","1K","10K","100K","1M","10M","100M","1B"))+
      scale_fill_manual(values = c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B'), guide = 'none') +
      scale_color_manual(values = c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B'), guide = 'none') +
      labs(x= '', y = 'Population', size = 'Average k-complexity',
           fill = 'Urban hierarchy', color =  'Urban hierarchy') +
      #guides(color = guide_legend(override.aes = list(size = 6))) +
      coord_flip() +
      theme(legend.position = 'bottom',
            legend.key=element_blank()))
}





# africa_dviz <- out_k_0 %>% filter(geography_group == "2c - Country by urban hierarchy") %>%
#   mutate(urban_hierarchy = factor(urban_hierarchy, levels = area_4)) %>%
#   mutate(k_reweight = landscan_population_un*k_complexity_weighted_landscan) %>%
#   group_by(country_name) %>%
#   mutate(k_reweight = sum(k_reweight)) %>%
#   ungroup() %>%
#   mutate(k_reweight = k_reweight/area_landscan_population_un)


x_complexity_size_point <- function(country_name) {
  country_dviz <- out_k_0 %>% 
    filter(country_name == name, geography_group == "2c - Country by urban hierarchy") %>%
    mutate(urban_hierarchy = factor(urban_hierarchy, levels = area_4)) %>% 
    mutate(k_reweight = landscan_population_un*k_complexity_weighted_landscan) %>%
    group_by(country_name) %>%
    mutate(k_reweight = sum(k_reweight)) %>%
    ungroup() %>%
    mutate(k_reweight = k_reweight/area_landscan_population_un)
  
  # X k-complexity Size population for sub-Saharan Africa
  (africa_dots_xk_sizepop <- ggplot() +
      geom_point(data = country_dviz, 
                 aes(x =  reorder(country_name, (k_reweight)), y = k_complexity_weighted_landscan,
                     fill = urban_hierarchy, color = urban_hierarchy, size = landscan_population_un), alpha = .75) +
      geom_text(data = country_dviz %>% filter(landscan_population_un >= 10000), 
                aes(x =  reorder(country_name, (k_reweight)), y =  k_complexity_weighted_landscan,
                    label = ifelse(landscan_population_un >= 500000, paste0(round(landscan_population_un/1000000,1),'M'),'' ) ), 
                size = 3,
                vjust = .5, color = '#333333', fontface='bold', check_overlap = TRUE) +
      labs(x= '', y = 'Average k-complexity', size = 'Population',
           fill = 'Urban hierarchy', color =  'Urban hierarchy') +
      coord_flip() +
      scale_fill_manual(values = c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B')) +
      scale_color_manual(values = c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B')) +
      scale_size_continuous(range = c(1,10), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
      #scale_y_continuous( expand = c(.01,.01), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
      guides(color = guide_legend(override.aes = list(size = 8, alpha = 1))) +
      theme(legend.position = 'bottom',
            #axis.text.y = element_blank(),
            #axis.ticks.y = element_blank(),
            legend.key=element_blank()))  
  
}

y_complexity_pop_point <- function(country_name) {
  country_dviz <- out_k_0 %>% 
    filter(country_name == name, geography_group == "2c - Country by urban hierarchy") %>%
    mutate(urban_hierarchy = factor(urban_hierarchy, levels = area_4)) %>% 
    mutate(k_reweight = landscan_population_un*k_complexity_weighted_landscan) %>%
    group_by(country_name) %>%
    mutate(k_reweight = sum(k_reweight)) %>%
    ungroup() %>%
    mutate(k_reweight = k_reweight/area_landscan_population_un)
  
  # Y k-complexity X population for sub-Saharan Africa
  (africa_x_y_scatter <- ggplot() +
      geom_point(data = country_dviz %>% filter(landscan_population_un >= 10000), 
                 aes(x = k_complexity_weighted_landscan, y = log10(landscan_population_un),
                     fill = urban_hierarchy, color = urban_hierarchy,
                     size = k_complexity_weighted_landscan), alpha = .7) +
      coord_flip() +
      labs( x= 'Average k-complexity', y = 'Population', size = 'k-complexity',
            fill = 'Urban hierarchy', color =  'Urban hierarchy') +
      #scale_y_continuous( expand = c(.005,.005), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) + 
      scale_fill_manual(values = c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B'), guide = 'none') + # 
      scale_color_manual(values = c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B'), guide = 'none') + # 
      scale_y_continuous(oob = scales::squish, breaks= c(1,2,3,4,5,6,7,8,9), labels = c('0',"100","1K","10K","100K","1M","10M","100M","1B"))+
      geom_text(data = country_dviz %>% filter(landscan_population_un >= 10000), aes(x = k_complexity_weighted_landscan, y = log10(landscan_population_un),
                                                                                    label = country_name), check_overlap = TRUE,
                size = 2.5, vjust =.5, color = '#333333', fontface='bold') +
      #guides(color = guide_legend(override.aes = list(size = 6))) +
      theme(legend.position = 'bottom',
            legend.key=element_blank()))
  
}



# Histogram for sub-Saharan Africa ----------------------------------------


country_histogram <- function(country_name) {
  k_5_order = c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31+','Off\nnetwork')        
  
  country_hist <- out_k_5 %>% 
    filter(country_name == country_name, 
           geographic_level == "4 - Conurbation", 
           !is.na(k_5),
           !is.na(landscan_population_un),
           !is.na(urban_hierarchy)) %>%  #geography_group == '1c - Urban hierarchy' ) %>%
    mutate(k_5 = str_wrap(k_5,width=3),
           k_5 = factor(k_5, levels = k_5_order)) %>%
    group_by(k_5) %>%
    mutate(k_sum = sum(landscan_population_un)) %>%
    ungroup() %>%
    mutate(share = landscan_population_un / k_sum ) %>%
    mutate(urban_hierarchy = factor(urban_hierarchy, levels = area_4)) %>%
    arrange(factor(urban_hierarchy, levels = rev(area_4)), k_5) %>%
    group_by(k_5) %>%
    mutate(pos_id_val = (cumsum(landscan_population_un) - 0.5*landscan_population_un)) %>%
    ungroup()
  
  (africa_hist_k_5 <- ggplot() +
      geom_bar(data = country_hist , 
               aes(y = k_5, x = landscan_population_un, fill = urban_hierarchy), color = 'white', size = .3, stat="identity") + 
      coord_flip() + 
      labs(y= 'k-complexity', x = 'Population', fill = 'Urban hierarchy', color =  'Urban hierarchy') + 
      geom_text(data = country_hist , aes(y = k_5, x = pos_id_val, label = ifelse(landscan_population_un >= 5000000, paste0(round(share*100,0),"%"),'')),
                size = 3, vjust = .5, color = '#333333', fontface='bold') +
      scale_fill_manual(values = c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B')) +
      scale_x_continuous(expand = c(.005,.005), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
      # theme_classic() +
      theme(legend.position = 'bottom'))
    
}



# africa_hist <- out_k_5 %>% filter(geography_group == '1c - Urban hierarchy') %>%
#   mutate(k_5 = str_wrap(k_5,width=3),
#          k_5 = factor(k_5, levels = k_5_order)) %>%
#   group_by(k_5) %>%
#   mutate(k_sum = sum(landscan_population_un)) %>%
#   ungroup() %>%
#   mutate(share = landscan_population_un / k_sum ) %>%
#   mutate(urban_hierarchy = factor(urban_hierarchy, levels = area_4)) %>%
#   arrange(factor(urban_hierarchy, levels = rev(area_4)), k_5) %>%
#   group_by(k_5) %>%
#   mutate(pos_id_val = (cumsum(landscan_population_un) - 0.5*landscan_population_un)) %>%
#   ungroup()




# Conurbations ------------------------------------------------------------

# unique(out_k_0$geography_group)
# area_4 = c("Core urban","Peripheral urban","Peri-urban","Non-urban")
# 
# conurbation_dviz <- out_k_0 %>% filter(geography_group == "4c - Conurbation by urban hierarchy") %>%
#   filter(urban_periurban_nonurban != 'Non-urban') %>%
#   mutate(country_name =  (gsub('Democratic Republic of the Congo', 'DR Congo', as.character(country_name))),
#          conurbation_area_name_short = gsub('Ambatolampy Tsimahafotsy', 'Ambatolampy', as.character(conurbation_area_name_short))
#   ) %>% 
#   mutate(conurbation_name = paste0(conurbation_area_name_short,', ',country_name)) %>%
#   mutate(conurbation_first_name = conurbation_area_name_short) %>%
#   separate(col =conurbation_first_name , sep = ' — ', into = c('conurbation_first_name'), extra = 'drop') %>%
#   filter(area_landscan_population_un >= 1500000) %>%
#   mutate(urban_hierarchy = factor(urban_hierarchy, levels = area_4)) %>% 
#   mutate(k_reweight = landscan_population_un*k_complexity_weighted_landscan) %>%
#   group_by(country_name) %>%
#   mutate(k_reweight = sum(k_reweight)) %>%
#   ungroup() %>%
#   mutate(k_reweight = k_reweight/area_landscan_population_un) 
# 
# (conurbation_dots_xpop_sizek <- ggplot() +
#     geom_point(data = conurbation_dviz %>% filter(landscan_population_un >= 10000), 
#                aes(x =  reorder(conurbation_name, (area_landscan_population_un)), y = log10(landscan_population_un),
#                    fill = urban_hierarchy, color = urban_hierarchy, size = k_complexity_weighted_landscan), alpha = .75) +
#     scale_size(range = c(1,10)) + 
#     geom_text(data = conurbation_dviz %>% filter(landscan_population_un >= 10000) , 
#               aes(x =  reorder(conurbation_name, (area_landscan_population_un)), y = log10(landscan_population_un), label = round(k_complexity_weighted_landscan , 1)), size = 3,
#               vjust = .5, color = '#333333', fontface='bold', check_overlap = TRUE) +
#     scale_y_continuous(oob = scales::squish, breaks= c(1,2,3,4,5,6,7,8,9), labels = c('0',"100","1K","10K","100K","1M","10M","100M","1B"))+
#     scale_fill_manual(values = c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B')) +
#     scale_color_manual(values = c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B')) +
#     labs(x= '', y = 'Population', size = 'Average K-complexity',
#          fill = 'Urban hierarchy', color =  'Urban hierarchy') +
#     guides(color = guide_legend(override.aes = list(size = 6))) +
#     coord_flip() +
#     theme(legend.position = 'bottom',
#           legend.key=element_blank()))
# 
# (conurbation_dots_xk_sizepop <- ggplot() +
#     geom_point(data = conurbation_dviz, 
#                aes(x =  reorder(conurbation_name, (k_reweight)), y = k_complexity_weighted_landscan,
#                    fill = urban_hierarchy, color = urban_hierarchy,
#                    size = landscan_population_un
#                ), alpha = .75) +
#     geom_text(data = conurbation_dviz, 
#               aes(x =  reorder(conurbation_name, (k_reweight)), y =  k_complexity_weighted_landscan,
#                   label = ifelse(landscan_population_un >= 100000, paste0(round(landscan_population_un/1000000,1),'M'),'' ) ), 
#               size = 3,
#               vjust = .5, color = '#333333', fontface='bold', check_overlap = TRUE) +
#     labs(x= '', y = 'Average K-complexity', size = 'Population',
#          fill = 'Urban hierarchy', color =  'Urban hierarchy') +
#     coord_flip() +
#     scale_fill_manual(values = c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B')) +
#     scale_color_manual(values = c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B')) +
#     scale_size_continuous(range = c(1,10), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
#     #scale_y_continuous( expand = c(.01,.01), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
#     guides(color = guide_legend(override.aes = list(size = 6))) +
#     theme(legend.position = 'bottom',
#           #axis.text.y = element_blank(),
#           #axis.ticks.y = element_blank(),
#           legend.key=element_blank()))
# 
# (conurbation_x_y_scatter <- ggplot() +
#     geom_point(data = conurbation_dviz %>% filter(landscan_population_un >= 10000), 
#                aes(x = k_complexity_weighted_landscan, y = log10(landscan_population_un),
#                    fill = urban_hierarchy, color = urban_hierarchy,
#                    size = k_complexity_weighted_landscan), alpha = .7) +
#     coord_flip() +
#     labs( x= 'Average K-complexity', y = 'Population', size = 'k-complexity',
#           fill = 'Urban hierarchy', color =  'Urban hierarchy') +
#     #scale_y_continuous( expand = c(.005,.005), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) + 
#     scale_fill_manual(values = c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B')) +
#     scale_color_manual(values = c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B')) +
#     scale_y_continuous(oob = scales::squish, breaks= c(1,2,3,4,5,6,7,8,9), labels = c('0',"100","1K","10K","100K","1M","10M","100M","1B"))+
#     geom_text(data = conurbation_dviz %>% filter(landscan_population_un >= 10000), 
#               aes(x = k_complexity_weighted_landscan, y = log10(landscan_population_un),
#                   label = conurbation_first_name), check_overlap = TRUE,
#               size = 3, vjust =.5, color = '#333333', fontface='bold') +
#     guides(color = guide_legend(override.aes = list(size = 6))) +
#     theme(legend.position = 'bottom',
#           legend.key=element_blank()))
# 
# # Urban center bars -------------------------------------------------------
# 
# urban_bchart <- out_k_4 %>%
#   filter(geography_group == '3 - Urban center',
#          urban_hierarchy %in% c('Core urban','Peripheral urban'),
#          area_landscan_population_un >= 1000000) %>%
#   mutate(country_name =  (gsub('Democratic Republic of the Congo ', 'DR Congo', as.character(country_name)))) %>%
#   mutate(urban_name = paste0(urban_center_name,', ',country_name)) %>%
#   mutate(k_reweight = landscan_population_un*k_complexity_weighted_landscan) %>%
#   group_by(geography_area) %>%
#   mutate(k_reweight = sum(k_reweight)) %>%
#   ungroup() %>%
#   mutate(k_reweight = k_reweight/area_landscan_population_un) 
# 
# grey2 <- c('#414141','#777777')
# kdist = max(as.integer(urban_bchart$k_4))
# colorhexes <- colorRampPalette(c('#93328E','#CF3F80','#F7626B','#FF925A','#FFC556','#F9F871'))(length(unique(urban_bchart$k_4))-2)
# 
# (a <- ggplot() +
#     geom_bar(data = urban_bchart, aes(y = landscan_population_un, x = reorder(urban_name, area_landscan_population_un), 
#                                       fill = k_4), 
#              stat="identity") +
#     coord_flip() +
#     scale_fill_manual(values = c(grey2, colorhexes)) +
#     scale_y_continuous( expand = c(0, 0), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
#     theme_bw() + 
#     labs(y = 'Population', x = 'k-complexity', subtitle = '') + 
#     theme(text = element_text(color = "#333333"),
#           plot.margin=unit(c(t=3,r=3,b=5,l=5), "pt"),
#           axis.text = element_text(size = 11),
#           axis.text.x = element_text(size = 9),
#           legend.title = element_blank(),
#           legend.position = 'none',
#           panel.border = element_blank(),
#           panel.background = element_blank(),
#           axis.title = element_text(face="bold", size = 11),
#           plot.subtitle = element_text(size = 15, face="bold", hjust=.5)))
# 
# (b <- ggplot() +
#     geom_bar(data = urban_bchart, aes(y = share_landscan_population_un, 
#                                       x = reorder(urban_name, k_reweight), fill = k_4), 
#              stat="identity") +
#     coord_flip() +
#     scale_fill_manual(values = c(grey2, colorhexes)) +
#     scale_y_continuous( expand = c(0, 0), labels = scales::percent ) +
#     theme_bw() + 
#     labs(y = 'Population', x = 'k-complexity', subtitle = '') + #'Population distribution across k-complexity levels'
#     theme(text = element_text(color = "#333333"),
#           plot.margin=unit(c(t=3,r=3,b=5,l=5), "pt"),
#           axis.text = element_text(size = 11),
#           axis.text.x = element_text(size = 9),
#           # axis.text.y = element_blank(),
#           axis.title.y = element_blank(),
#           legend.title = element_blank(),
#           panel.border = element_blank(),
#           panel.background = element_blank(),
#           axis.title = element_text(face="bold", size = 11),
#           plot.subtitle = element_text(size = 15, face="bold", hjust=.5)))
# 
# (urban_bars <- a+b)
# 
# # Conurbation bars --------------------------------------------------------
# 
# conurbation_bchart <- out_k_4 %>%
#   filter(geography_group == "4 - Conurbation",
#          urban_nonurban == "Core, peripheral, & peri-urban",
#          area_landscan_population_un >= 1500000) %>%
#   mutate(country_name =  (gsub('Democratic Republic of the Congo ', 'DR Congo', as.character(country_name))),
#          conurbation_area_name_short = gsub('Ambatolampy Tsimahafotsy', 'Ambatolampy', as.character(conurbation_area_name_short))) %>%
#   mutate(conurbation_name = paste0(conurbation_area_name_short,', ',country_name)) %>%
#   mutate(k_reweight = landscan_population_un*k_complexity_weighted_landscan) %>%
#   group_by(geography_area) %>%
#   mutate(k_reweight = sum(k_reweight)) %>%
#   ungroup() %>%
#   mutate(k_reweight = k_reweight/area_landscan_population_un) 
# 
# grey2 <- c('#414141','#777777')
# kdist = max(as.integer(conurbation_bchart$k_4))
# colorhexes <- colorRampPalette(c('#93328E','#CF3F80','#F7626B','#FF925A','#FFC556','#F9F871'))(length(unique(conurbation_bchart$k_4))-2)
# 
# (a <- ggplot() +
#     geom_bar(data = conurbation_bchart, aes(y = landscan_population_un, x = reorder(conurbation_name, area_landscan_population_un), 
#                                             fill = k_4), 
#              stat="identity") +
#     coord_flip() +
#     scale_fill_manual(values = c(grey2, colorhexes)) +
#     scale_y_continuous( expand = c(0, 0), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
#     theme_bw() + 
#     labs(y = 'Population', x = 'k-complexity', subtitle = '') + 
#     theme(text = element_text(color = "#333333"),
#           plot.margin=unit(c(t=3,r=3,b=5,l=5), "pt"),
#           axis.text = element_text(size = 11),
#           axis.text.x = element_text(size = 9),
#           legend.title = element_blank(),
#           legend.position = 'none',
#           panel.border = element_blank(),
#           panel.background = element_blank(),
#           axis.title = element_text(face="bold", size = 11),
#           plot.subtitle = element_text(size = 15, face="bold", hjust=.5)))
# 
# (b <- ggplot() +
#     geom_bar(data = conurbation_bchart, aes(y = share_landscan_population_un, 
#                                             x = reorder(conurbation_name, k_reweight), fill = k_4), 
#              stat="identity") +
#     coord_flip() +
#     scale_fill_manual(values = c(grey2, colorhexes)) +
#     scale_y_continuous( expand = c(0, 0), labels = scales::percent ) +
#     theme_bw() + 
#     labs(y = 'Population', x = 'k-complexity', subtitle = '') + #'Population distribution across k-complexity levels'
#     theme(text = element_text(color = "#333333"),
#           plot.margin=unit(c(t=3,r=3,b=5,l=5), "pt"),
#           axis.text = element_text(size = 11),
#           axis.text.x = element_text(size = 9),
#           # axis.text.y = element_blank(),
#           axis.title.y = element_blank(),
#           legend.title = element_blank(),
#           panel.border = element_blank(),
#           panel.background = element_blank(),
#           axis.title = element_text(face="bold", size = 11),
#           plot.subtitle = element_text(size = 15, face="bold", hjust=.5)))
# 
# (conurbation_bars <- a+b)
# 
# 
# # -------------------------------------------------------------------------
# 
# 
# 
# 
