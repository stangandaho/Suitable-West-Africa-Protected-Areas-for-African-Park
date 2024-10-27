fish_order <- c(
  "Characiformes",
  "Perciformes",
  "Cypriniformes",
  "Osteoglossiformes",
  "Siluriformes",
  "Ceratodontiformes",
  "Syngnathiformes",
  "Scorpaeniformes",
  "Cyprinodontiformes",
  "Mugiliformes",
  "Cetacea",  # Note: Cetacea are aquatic mammals, not fish.
  "Gobiesociformes",
  "Myliobatiformes",
  "Beloniformes",
  "Clupeiformes",
  "Batrachoidiformes",
  "Pleuronectiformes",
  "Polypteriformes"
)

diversity_index_plot <- function(data, shp, class_col, value_col) {
  if (hasArg(class_col)) {
    cls <- unique(data[[class_col]])
  }else{
    cls <- NA
  }

  for (cl in cls) {
    rs_mammal_shp <- data %>%
      filter(!!sym(class_col) == cl) %>%
      left_join(y = shp, by = "NAME")
    lvl <- median(rs_mammal_shp[[value_col]], na.rm = TRUE)
    rs_mammal_shp <- rs_mammal_shp %>%
      mutate(richness = case_when(value_col <= lvl ~ "Low",
                                  TRUE ~ "High"))
    break_seq <- seq(min(rs_mammal_shp[[value_col]], na.rm = T),
                     max(rs_mammal_shp[[value_col]], na.rm = T),
                     length.out = 5)

    p <- ggplot(data = rs_mammal_shp)+
      geom_sf(data = shp, aes(geometry = geometry), fill = "gray", color = NA)+
      geom_sf(aes(fill = !!sym(value_col), geometry = geometry), color = NA)+
      geom_sf(data = west_africa, fill = NA, color = "gray50")+
      geom_sf_text(data = west_africa, mapping = aes(label = NAME_0), size = 6, color = "gray10")+
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
    save_path <- paste0("plots/SR_", cl, ".jpeg")
    ggsave(plot = p, filename = save_path, width =40, height =  27, units = "cm")

  }
}


pend <- wa_species_with_iucn_status %>%
  filter(NAME == "PArc de Pendjari" & class == "Mammals")
