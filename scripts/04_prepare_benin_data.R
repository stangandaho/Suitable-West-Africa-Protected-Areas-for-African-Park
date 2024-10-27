library(dplyr)
library(readxl)

# Function to compute specific richness
richness_index <- function(data, species_col, community){
  #  Specific richness by class and protected area
  bj_specific_richness <- data %>%
    distinct(.keep_all = TRUE) %>% 
    dplyr::summarise(specific_richness = length(unique(!!sym(species_col))), .by = c(community))
  return(bj_specific_richness)
}

# Function tocompute threatened species index
threat_index <- function(data, redlist_status, community){
  ti <- data %>%
    select(!!sym(community), !!sym(redlist_status)) %>% 
    dplyr::reframe(n = length(unique(!!sym(redlist_status))), 
                   .by = c(community),
                   "{redlist_status}" := !!sym(redlist_status)) %>%
    left_join(y = score, by = "category_code") %>%
    summarise(threat_index = sum(n*score, na.rm = TRUE), .by = c(community))
  
  return(ti)
}

#  Function to compute phylogenetic diversity index
phylo_diversity_index <- function(data, community, transpose = F, method = "richness"){
  row_id <- data[[community]]
  data <- data %>% select(- !!sym(community))
  
  if (transpose) {
    data <- data %>% t()
    colnames(data) <- row_id
    data <- data %>% t()
  }
  
  pa_hclust <- stats::dist(x = data) %>%
    hclust() %>%
    ape::as.phylo()
  
  pv <- evodiv(phyl = pa_hclust, data %>% t(), method = method)
  
  return(pv)
}

# Function to plot index
index_plot <- function(data, index, break_length = 5, 
                       legend_title = "Specific richness", digit = 2) {
  
  break_seq <- round(seq(min(data[[index]], na.rm = T),
                         max(data[[index]], na.rm = T),
                         length.out = break_length), digit)
  
  p <- ggplot(data = data)+
    geom_sf(data = bj_pa_shp, aes(geometry = geometry), fill = "gray", color = NA)+
    geom_sf(aes(fill = !!sym(index), geometry = geometry), color = NA)+
    geom_sf(data = benin_shp, fill = NA, color = "gray50")+
    scale_fill_gradientn(colours = color_plt,
                         breaks = break_seq, 
                         label = as.character(break_seq))+
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

# Import species_with_iucn_status edited
bj_amphibian <- read_excel("datasets/Biodiversité_Bénin_Classe des amphibiens_AP.xlsx") %>% 
  rename(species = 1, class = 2, category_code = 4) %>% 
  select(-3, -5) %>% 
  tidyr::pivot_longer(cols = !c(1:3), names_to = "NAME", values_to = "occurrence") %>% 
  filter(occurrence != 0)

bj_mammal <- read_excel("datasets/meta_données_mammifères_Bénin_AP.xlsx") %>% 
  rename(species = 1, class = 2, category_code = 6) %>% 
  select(-3, -4, -5) %>% 
  tidyr::pivot_longer(cols = !c(1:3), names_to = "NAME", values_to = "occurrence") %>% 
  filter(occurrence != 0)


bj_repetile <- read_excel("datasets/Copie de Reptiles_BENIN_ok_2024.xlsx") %>% 
  rename(species = 1, category_code = 5) %>% 
  mutate(class = "Reptiles") %>% relocate(class, .before = 2) %>% 
  select(- c(3:5)) %>% 
  tidyr::pivot_longer(cols = !c(1:3), names_to = "NAME", values_to = "occurrence") %>% 
  filter(occurrence != 0)

benin_species <- bind_rows(bj_amphibian, bj_mammal, bj_repetile)

bj_commat <- bind_rows(
  read_excel("datasets/Biodiversité_Bénin_Classe des amphibiens_AP.xlsx") %>% 
    rename(species = 1, class = 2, category_code = 4) %>% 
    select(-3, -5) ,
  
  read_excel("datasets/meta_données_mammifères_Bénin_AP.xlsx") %>% 
    rename(species = 1, class = 2, category_code = 6) %>% 
    select(-3, -4, -5) ,
  
  read_excel("datasets/Copie de Reptiles_BENIN_ok_2024.xlsx") %>% 
    rename(species = 1, category_code = 5) %>% 
    mutate(class = "Reptiles") %>% relocate(class, .before = 2) %>% 
    select(- c(3:5))
)
#