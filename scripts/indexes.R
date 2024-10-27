library(ggplot2)
library(dplyr)
library(sf)

# Import species_with_iucn_status edited
species_data <- read.csv(file = "D:\\QGIS Projects\\Shp\\WA\\species_data.csv")

# update west_africa_protected_area_species.csv
score <- data.frame(
  category_code = c("LC", "NT", "VU", "EN", "NE","CR", "DD"),
  score = c(1, 2, 3, 4, 0, 5, 0)
)

wa_species_with_iucn_status <- read.csv("D:\\QGIS Projects\\Shp\\WA\\west_africa_protected_area_species.csv") %>%
  filter(species %in% species_data$species) %>%
  distinct(species, NAME, .keep_all = TRUE) %>%
  left_join(x = ., y = species_data, by = "species") %>%
  filter(!class %in% c("Dipneusti", "Elasmobranchii", "") & species != "") %>%
  mutate(class = case_when(class %in% c("Squamata", "Testudines", "Crocodylia") ~ "Reptiles",
                           class == "Mammalia" ~ "Mammals",
                           class == "Aves" ~ "Birds",
                           class == "Amphibia" ~ "Amphibians"
  ))

# Threatened species index
threatened_species_index <- wa_species_with_iucn_status %>%
  dplyr::summarise(n = n(), .by = c("NAME","class", "category_code")) %>%
  left_join(y = score, by = "category_code") %>%
  summarise(threat_index = sum(n*score), .by = c("NAME","class"))

#  Specific richness by class and protected area
specific_richness <- wa_species_with_iucn_status %>%
  dplyr::summarise(specific_richness = n(), .by = c("NAME", "class", "species")) %>%
  dplyr::summarise(specific_richness = sum(specific_richness), .by = c("NAME", "class"))

#  Calculate phylogenetic diversity by protected area
library(adiv)
library(ape)

# Dsitance matrix
commat <- wa_species_with_iucn_status %>%
  select(NAME, species) %>%
  summarise(n = n(), .by = c("NAME", "species")) %>%
  tidyr::pivot_wider(names_from = species, values_from = n,
                     id_cols = NAME, values_fill = 0) %>%
  as.data.frame()
row_id <- commat$NAME
commat <- commat[, -1]
rownames(commat) <- row_id

pa_hclust <- stats::dist(x = commat %>% t() %>% as.data.frame()) %>%
  hclust() %>%
  ape::as.phylo()

phylo_diversity <- evodiv(phyl = pa_hclust, commat, method = "richness")

################## GGPLOT #################
color_plt <- c("#aa0000", "#FFEC5C", "#44803F", "#146152")#c("#6C5B7B","#C06C84","#F67280","#F8B195") #
wa_pa_shp <- sf::read_sf("D:\\QGIS Projects\\Shp\\WA\\Shp_AP_WA_500km2\\AP_wa_50000ha_ok.shp") %>%
  rename(NAME = name) %>%
  filter(!NAME %in% c("Ouénou-Bénou", "Ouémé Boukou", "Toui-Kilibo", "Dogo"))

# Import west africa boundary
west_africa <- st_read("D:/QGIS Projects/Shp/WA/WA/Afrique_ouest_pol_ok.shp") %>%
  filter(NAME_0 != "Cape Verde") %>%
  st_transform(crs = st_crs(wa_pa_shp))

# Richness
for (cl in unique(specific_richness$class)) {

  rs_mammal_shp <- specific_richness %>%
    filter(class == cl) %>%
    left_join(y = wa_pa_shp, by = "NAME")
  lvl <- median(rs_mammal_shp$specific_richness, na.rm = TRUE)
  rs_mammal_shp <- rs_mammal_shp %>%
    mutate(richness = case_when(specific_richness <= lvl ~ "Low",
                                TRUE ~ "High"))
  break_seq <- seq(min(rs_mammal_shp$specific_richness, na.rm = T),
                   max(rs_mammal_shp$specific_richness, na.rm = T),
                   length.out = 5)

  p <- ggplot(data = rs_mammal_shp)+
    geom_sf(data =wa_pa_shp, aes(geometry = geometry), fill = "gray", color = NA)+
    geom_sf(aes(fill = specific_richness, geometry = geometry), color = NA)+
    geom_sf(data = west_africa, fill = NA, color = "gray50")+
    geom_sf_text(data = west_africa, mapping = aes( label = NAME_0), size = 6, color = "gray10")+
    scale_fill_gradientn(colours = color_plt,
                         breaks = break_seq, label = c("Low", "", "", "", "High"))+
    guides(fill = guide_colorbar(title = "Specific richness",
                                 keywidth = unit(.5, units = "lines"),
                                 keyheight = unit(12, units = "lines")))+

    theme_void()+
    #facet_wrap(facets = ~ class)+
    theme(
      # legend
      legend.title.position = "left",
      legend.title = element_text(angle = 90,hjust = .5, size = 15),
      legend.text = element_text(angle = 90, size = 13, hjust = .5,
                                 margin = margin(r = 1, unit = "lines")),
      strip.background = element_rect(fill = "gray",colour = NA),
      strip.text = element_text(size = 20, face = "bold",
                                margin = margin(t = .5, b = .5, unit = "lines"))
    )

  # save
  save_path <- paste0("D:\\QGIS Projects\\Shp\\WA\\plots\\SR_", cl, ".jpeg")
  ggsave(plot = p, filename = save_path, width =40, height =  27, units = "cm")


}

# Threatened Species Index
for (cl in unique(threatened_species_index$class)) {

  rs_mammal_shp <- threatened_species_index %>%
    filter(class == cl) %>%
    left_join(y = wa_pa_shp, by = "NAME")
  lvl <- median(rs_mammal_shp$threat_index, na.rm = TRUE)
  rs_mammal_shp <- rs_mammal_shp %>%
    mutate(richness = case_when(threat_index <= lvl ~ "Low",
                                TRUE ~ "High"))
  break_seq <- seq(min(rs_mammal_shp$threat_index, na.rm = T),
                   max(rs_mammal_shp$threat_index, na.rm = T),
                   length.out = 5)

  p <- ggplot(data = rs_mammal_shp)+
    geom_sf(data =wa_pa_shp, aes(geometry = geometry), fill = "gray", color = NA)+
    geom_sf(aes(fill = threat_index, geometry = geometry), color = NA)+
    geom_sf(data = west_africa, fill = NA, color = "gray50")+
    geom_sf_text(data = west_africa, mapping = aes( label = NAME_0), size = 6, color = "gray10")+
    scale_fill_gradientn(colours = color_plt,
                         breaks = break_seq, label = c("Low", "", "", "", "High"))+
    guides(fill = guide_colorbar(title = "Specific richness",
                                 keywidth = unit(.5, units = "lines"),
                                 keyheight = unit(12, units = "lines")))+

    theme_void()+
    #facet_wrap(facets = ~ class)+
    theme(
      # legend
      legend.title.position = "left",
      legend.title = element_text(angle = 90,hjust = .5, size = 15),
      legend.text = element_text(angle = 90, size = 13, hjust = .5,
                                 margin = margin(r = 1, unit = "lines")),
      strip.background = element_rect(fill = "gray",colour = NA),
      strip.text = element_text(size = 20, face = "bold",
                                margin = margin(t = .5, b = .5, unit = "lines"))
    )

  # save
  save_path <- paste0("D:\\QGIS Projects\\Shp\\WA\\plots\\TSI_", cl, ".jpeg")
  ggsave(plot = p, filename = save_path, width = 40, height = 27, units = "cm")


}


# Phylogenetic diversity index
phylo_diversity <- evodiv(phyl = pa_hclust, commat, method = "richness")
lvl <- median(phylo_diversity %>% as.data.frame() %>% pull(richness), na.rm = TRUE)
phylo_diversity <- phylo_diversity %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "NAME") %>%
  left_join(y = wa_pa_shp, by = "NAME") #%>%
  # mutate(richness_lvl = case_when(richness <= lvl ~ "Low",
  #                             TRUE ~ "High"))

break_seq <- seq(min(phylo_diversity$richness, na.rm = T),
                 max(phylo_diversity$richness, na.rm = T),
                 length.out = 5)

p <- ggplot(data = phylo_diversity)+
  geom_sf(data =wa_pa_shp, aes(geometry = geometry), fill = "gray", color = NA)+
  geom_sf(aes(fill = threat_index, geometry = geometry), color = NA)+
  geom_sf(data = west_africa, fill = NA, color = "gray50")+
  geom_sf_text(data = west_africa, mapping = aes( label = NAME_0), size = 6, color = "gray10")+
  scale_fill_gradientn(colours = color_plt,
                       breaks = break_seq, label = c("Low", "", "", "", "High"))+
  guides(fill = guide_colorbar(title = "Specific richness",
                               keywidth = unit(.5, units = "lines"),
                               keyheight = unit(12, units = "lines")))+

  theme_void()+
  #facet_wrap(facets = ~ class)+
  theme(
    # legend
    legend.title.position = "left",
    legend.title = element_text(angle = 90,hjust = .5, size = 15),
    legend.text = element_text(angle = 90, size = 13, hjust = .5,
                               margin = margin(r = 1, unit = "lines")),
    strip.background = element_rect(fill = "gray",colour = NA),
    strip.text = element_text(size = 20, face = "bold",
                              margin = margin(t = .5, b = .5, unit = "lines"))
  )

# save
save_path <- paste0("D:\\QGIS Projects\\Shp\\WA\\plots\\TSI_", cl, ".jpeg")
ggsave(plot = p, filename = save_path, width = 40, height = 27, units = "cm")

