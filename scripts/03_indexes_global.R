library(ggplot2)
library(dplyr)
library(sf)
library(adiv)
library(ape)
library(readxl)

# Import species_with_iucn_status edited
species_data <- read.csv(file = "datasets/species_data.csv")

# update west_africa_protected_area_species.csv
score <- data.frame(
  category_code = c("LC", "NT", "VU", "EN", "NE","CR", "DD"),
  score = c(1, 2, 3, 4, 0, 5, 0)
)

wa_species_with_iucn_status <- read.csv("datasets/west_africa_protected_area_species.csv") %>%
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
  dplyr::summarise(n = n(), .by = c("NAME", "category_code")) %>%
  left_join(y = score, by = "category_code") %>%
  summarise(threat_index = sum(n*score), .by = c("NAME"))

#  Specific richness by class and protected area
specific_richness <- wa_species_with_iucn_status %>%
  dplyr::summarise(specific_richness = n(), .by = c("NAME"))

#  Calculate phylogenetic diversity by protected area
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


# Function to plot index
wa_index_plot <- function(data, index, break_length = 5, 
                       legend_title = "Specific richness", digit = 2) {
  idx <- data[[index]][is.finite(data[[index]])]
  break_seq <- round(seq(min(idx, na.rm = T),
                         max(idx, na.rm = T),
                         length.out = break_length), digit)
  
  p <- ggplot(data = data)+
    geom_sf(data = wa_pa_shp, aes(geometry = geometry), fill = "gray", color = NA)+
    geom_sf(data = west_africa, fill = NA, color = "gray50")+
    geom_sf_text(data = west_africa, mapping = aes( label = NAME_0), size = 6, color = "gray10")+
    geom_sf(aes(fill = !!sym(index), geometry = geometry), color = NA)+
    scale_fill_gradientn(colours = color_plt,
                         breaks = break_seq, 
                         label = c("Low", rep(" ", length(break_seq)-2), "High"))+
    guides(fill = guide_colorbar(title = legend_title,
                                 keywidth = unit(.5, units = "lines"),
                                 keyheight = unit(12, units = "lines")))+
    
    theme_void()+
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
  
  return(p)
}

# Richness
specific_richness_shp <- specific_richness %>%
  left_join(y = wa_pa_shp, by = "NAME")

# Threatened Species Index
tsi_shp <- threatened_species_index %>%
  left_join(y = wa_pa_shp, by = "NAME")

# Phylogenetic Diversity Index
phylo_diversity_shp <- phylo_diversity %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "NAME") %>%
  left_join(y = wa_pa_shp, by = "NAME") %>% 
  rename(phylo_diversity = richness)

# Degradation rate
excluded_class <- c("Nuage", "Formation savanicole", "Formation marécageuse",
                    "Formation forestière naturelle", "Plan d'eau")
occupation <- read_excel(path = "datasets/Base_occupation_AP_wa_21102024.xlsx",
                         sheet = "Base_occup_AP_WA") 
anthro_rate <- occupation%>% 
  filter(Occupation %in% c("Sol nu", "Champ et jachère", "Habitation")) %>% 
  summarise(anthro_2017 = sum(Superficie_ha_2017),
            anthro_2023 = sum(Superficie_ha_2023),
            .by = "AP")

natural_rate <- occupation %>% 
  filter(Occupation %in% c("Formation savanicole", "Formation marécageuse")) %>% 
  summarise(natural_2017 = sum(Superficie_ha_2017),
            natural_2023 = sum(Superficie_ha_2023),
            .by = "AP")

degradation_rate <- natural_rate %>% 
  left_join(y = anthro_rate, by = "AP") %>% 
  mutate(loss = round((natural_2017 - natural_2023)*100/natural_2017, 2)) %>% 
  rename(NAME = AP) %>% 
  left_join(y = wa_pa_shp, by = "NAME")

# Create finale index data
index_data <- bind_cols(
  specific_richness_shp %>% 
    select(c(NAME, specific_richness, iso3, geometry)),
  
  tsi_shp %>% 
    select(threat_index),
  
  phylo_diversity_shp %>% 
    select(phylo_diversity)
  
) %>% 
  left_join(y = degradation_rate %>% 
              filter(NAME %in% unique(specific_richness_shp$NAME)) %>% 
              select(NAME, loss),
            by = "NAME") %>% 
  # Replace Benin
  filter(!iso3 == "BEN") %>% 
  relocate(geometry, .after = loss) %>% 
  select(-iso3) %>%
  bind_rows(benin_index_data)

################## GGPLOT #################
color_plt <- c("#aa0000", "#FFEC5C", "#44803F", "#146152")#c("#6C5B7B","#C06C84","#F67280","#F8B195") #
wa_pa_shp <- sf::read_sf("D:\\QGIS Projects\\Shp\\WA\\Shp_AP_WA_500km2\\AP_wa_50000ha_ok.shp") %>%
  rename(NAME = name) %>%
  filter(!NAME %in% c("Ouénou-Bénou", "Ouémé Boukou", "Toui-Kilibo", "Dogo"))
wa_pa_shp$area <- as.numeric(st_area(wa_pa_shp)/1e+06)
wa_pa_shp <- wa_pa_shp %>% filter(area >= 500)

# Import west africa boundary
west_africa <- st_read("D:/QGIS Projects/Shp/WA/WA/Afrique_ouest_pol_ok.shp") %>%
  filter(NAME_0 != "Cape Verde") %>%
  st_transform(crs = st_crs(wa_pa_shp))

color_plt_dg <- c("#aa0000", "#FFEC5C", "#B4CF66", "#44803F", "#004c18", "#146152")

## Richness
wa_index_plot(data = index_data, index = "specific_richness")
save_path <- paste0("plots/WA_Specific_richness.jpeg")
ggsave(filename = save_path, width = 40, height = 27, units = "cm")

## Threatened Species Index
wa_index_plot(data = index_data, index = "threat_index", 
              legend_title = "Threatened species index")
save_path <- paste0("plots/WA_Threatened_species_index.jpeg")
ggsave(filename = save_path, width = 40, height = 27, units = "cm")

## Phylogenetic Diversity Index
wa_index_plot(data = index_data, index = "phylo_diversity", 
              legend_title = "Phylogenetic diversity")
save_path <- paste0("plots/WA_Phylogenetic_diversity_index.jpeg")
ggsave(filename = save_path, width = 40, height = 27, units = "cm")

## Degradation
wa_index_plot(data = index_data, index = "loss", 
              legend_title = "Habitat loss")
save_path <- paste0("plots/WA_Habitat_loss.jpeg")
ggsave(filename = save_path, width = 40, height = 27, units = "cm")

# Build table for protected area prioritizing
priority_rank <- c(rep("Top priority", 30), rep("High priority", 30),
                   rep("Midlevel priority", 30), rep("Low priority", 30),
                   rep("Least priority", 38))
priority_data <- index_data %>%
  mutate(
    rank_sr = (specific_richness),
    rank_tsi = (threat_index),
    rank_phylo = (phylo_diversity),
    rank_sum = rank_sr+ rank_tsi+rank_phylo,
    rank = rank(rank_sum)
  ) %>%
  left_join(y = wa_pa_shp, by = "NAME") %>%
  arrange(desc(rank)) %>%
  mutate(priority_rank = priority_rank)

priority_data$priority_rank <- factor(priority_data$priority_rank,
                                      levels = c("Top priority", "High priority",
                                                 "Midlevel priority", "Low priority",
                                                 "Least priority"))
ggplot(data = priority_data)+
  geom_sf(data = wa_pa_shp, aes(geometry = geometry), fill = "gray", color = NA)+
  geom_sf(aes(fill = priority_rank, geometry = geometry), color = NA)+
  geom_sf(data = west_africa, fill = NA, color = "gray50")+
  scale_fill_manual(values = rev(c("#aa0000", "#FFEC5C","#f69700", "#44803F", "#146152")))+
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
save_path <- paste0("plots\\WA Prioritized_protected_area.jpeg")
ggsave(filename = save_path, width = 40, height = 27, units = "cm")


