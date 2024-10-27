library(ggplot2)
library(dplyr)
library(sf)
library(adiv)
library(ape)
library(readxl)
source("scripts/04_prepare_benin_data.R")

# import a data that has PA and country name
country_and_pa <- read.csv("datasets/wa_protected_area.csv") %>% 
  filter(COUNTRY == "BEN")

bj_pa_name <- c("Oueme-Boukou", "Monts-Kouffe", "Wari-Maro", "Agoua",
                "Toui-Kilibo", "Oueme-Superieur", "Trois-Rivieres", "Ouenou-Benou",     
                "Pendjari", "Sota", "Dogo", "W", "Alibori-Superieur","Goungoun")
# Import benin protected area
bj_pa_shp <- sf::read_sf("D:\\QGIS Projects\\Shp\\WA\\Shp_AP_WA_500km2\\AP_wa_50000ha_ok.shp") %>%
  rename(NAME = name) %>%
  filter(iso3 == "BEN" & NAME != "Séri") %>% 
  mutate(NAME = case_when(NAME == "La Sota" ~ "Sota", 
                          NAME == "Trois Rivières" ~ "Trois-Rivieres",
                          NAME == "L'Alibori Supérieur" ~ "Alibori-Superieur",
                          NAME == "Ouémé Superieur" ~ "Oueme-Superieur",
                          NAME == "Ouari Maro" ~ "Wari-Maro",
                          NAME == "Monts Kouffé" ~ "Monts-Kouffe",
                          NAME == "Agoua" ~ "Agoua",
                          NAME %in% c("W / Bénin Parc National", "Djona ZC") ~ "W",
                          NAME %in% c("PArc de Pendjari", "ZC Pendjari", "Zone cynégétique de Atakora") ~ "Pendjari",
                          NAME == "Ouénou-Bénou" ~ "Ouenou-Benou",
                          NAME == "Ouémé Boukou" ~ "Oueme-Boukou",
                          TRUE ~ NAME)) %>% 
  summarise(geometry = st_union(geometry), .by = "NAME")

# Import species_with_iucn_status edited
bj_species_data <- read_excel("datasets/Biodiversité_Bénin_Classe des amphibiens_AP.xlsx") %>% 
  rename(species = 1, class = 2, category_code = 4) %>% 
  select(-3, -5) %>% 
  tidyr::pivot_longer(cols = !c(1:3), names_to = "NAME", values_to = "occurrence") %>% 
  filter(occurrence != 0)

# update west_africa_protected_area_species.csv
score <- data.frame(
  category_code = c("LC", "NT", "VU", "EN", "NE","CR", "DD"),
  score = c(1, 2, 3, 4, 0, 5, 0)
)

# Threatened species index
bj_threatened_species_index <- threat_index(benin_species, "category_code", "NAME")
#  Specific richness by class and protected area
bj_specific_richness <- richness_index(data = benin_species, "species", "NAME")
#  Calculate phylogenetic diversity by protected area
commat <- bj_commat %>% select(-c(2, 3)) %>% 
  mutate(species = stringr::str_squish(species)) %>% 
  filter(!is.na(species)) %>% 
  distinct(species, .keep_all = T)
bj_phylo_diversity <- phylo_diversity_index(commat, "species", transpose = T)

################## GGPLOT #################
color_plt <- c("#aa0000", "#FFEC5C", "#FFEC5C", "#146152")#c("#6C5B7B","#C06C84","#F67280","#F8B195") #

# Import west africa boundary
benin_shp <- st_read("D:/QGIS Projects/Shp/WA/WA/Afrique_ouest_pol_ok.shp") %>%
  filter(NAME_0 == "Benin") %>%
  st_transform(crs = st_crs(bj_pa_shp))

# Richness
color_plt <- c("#aa0000", "#FFEC5C", "#FFEC5C", "#146152")
rs_shp <- bj_specific_richness %>%
  left_join(y = bj_pa_shp, by = "NAME")
index_plot(data = rs_shp, index = "specific_richness", break_length = 4, digit = 0)
# save
save_path <- paste0("plots\\Benin Specific richness.jpeg")
ggsave(filename = save_path, width = 20, height =  27, units = "cm")

# Threatened Species Index
color_plt <- c("#aa0000", "#FFEC5C", "#44803F", "#146152")
tsi_shp <- bj_threatened_species_index %>%
  left_join(y = bj_pa_shp, by = "NAME")
index_plot(data = tsi_shp, index = "threat_index", 
           break_length = 6, legend_title = "Threat index", digit = 0)

# save
save_path <- paste0("plots\\Benin Threatened Species Index.jpeg")
ggsave(filename = save_path, width = 20, height =  27, units = "cm")

# Phylogenetic Diversity Index
phylo_diversity_shp <- bj_phylo_diversity %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "NAME") %>%
  left_join(y = bj_pa_shp, by = "NAME")
index_plot(data = phylo_diversity_shp, index = "richness", 
           legend_title = "Phylogenetic diversity", break_length = 5)
# save
save_path <- paste0("plots\\Benin Phylogenetic Diversity Index.jpeg")
ggsave(filename = save_path, width = 20, height =  27, units = "cm")

# Degradation
color_plt <- rev(c("#aa0000", "#FFEC5C", "#44803F", "#146152"))
degradation_rate <- read_excel("datasets/Taux_degradation_AP_BEN.xlsx") %>% 
  summarise(Taux_degradation = sum(Taux_degradation), .by = "NOM_AP") %>%
  rename(NAME = NOM_AP) %>% 
  left_join(y = bj_pa_shp, by = "NAME")
index_plot(data = degradation_rate, index = "Taux_degradation", 
           legend_title = "Habitat loss", break_length = 5)

# Create final index data for Benin
benin_index_data <- bind_cols(
  rs_shp, 
  tsi_shp %>% select(threat_index), 
  phylo_diversity_shp %>% 
    select(richness) %>% 
    rename(phylo_diversity = richness), 
  degradation_rate %>% 
    select(Taux_degradation) %>% 
    rename(loss = Taux_degradation)
)
# Build table for protected area prioritizing
priority_rank <- c(rep("Top priority", 5),
                   rep("Midlevel priority", 5), rep("Low priority", 4))
priority_data <- bind_cols(degradation_rate, bj_phylo_diversity, 
            bj_specific_richness, bj_threatened_species_index) %>%
  mutate(
    rank_sr = (specific_richness),
    rank_tsi = (threat_index),
    rank_phylo = (richness),
    degrad = (-Taux_degradation),
    score_sum = rank_sr + rank_tsi+rank_phylo +degrad,
    rank = rank(score)
  ) %>%
  rename(NAME = 1) %>% 
  left_join(y = bj_pa_shp, by = "NAME") %>%
  arrange(desc(rank)) %>% 
  mutate(priority_rank = priority_rank)

priority_data$priority_rank <- factor(priority_data$priority_rank,
                                      levels = c("Top priority",
                                                 "Midlevel priority", "Low priority"))
ggplot(data = priority_data)+
  geom_sf(data = bj_pa_shp, aes(geometry = geometry), fill = "gray", color = NA)+
  geom_sf(aes(fill = priority_rank, geometry = geometry.x), color = NA)+
  geom_sf(data = benin_shp, fill = NA, color = "gray50")+
  scale_fill_manual(values = rev(c("#aa0000", "#FFEC5C", "#146152")))+
  guides(fill = guide_legend(
    keywidth = unit(2, "lines"),
    keyheight = unit(.5, "lines")
  ))+
  labs(fill = "")+
  theme_void()+
  theme(
    # axis
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    legend.margin = margin(b = unit(5, "lines"))
  )

# save
save_path <- paste0("plots\\Benin Prioritized_protected_area.jpeg")
ggsave(filename = save_path, width = 20, height =  27, units = "cm")


