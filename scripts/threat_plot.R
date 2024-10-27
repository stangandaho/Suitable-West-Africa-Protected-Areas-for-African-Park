library(dplyr)
library(ggplot2)
library(readxl)

# import data
threatened_sps <- read_excel("D:\\QGIS Projects\\Shp\\WA\\Data_all_species_groups.xlsx") %>%
  select(1:4) %>%
  rename(taxonomic_grouyp = 2, total_assessed = 3, threatened = 4) %>%
  rowwise() %>%
  mutate(prop_threat = threatened*100/total_assessed,
         prop_non_threat = 100 - prop_threat) %>%
  select(Country, taxonomic_grouyp, prop_threat,prop_non_threat) %>%
  tidyr::pivot_longer(cols = !c(Country, taxonomic_grouyp),
                      names_to = "threat", values_to = "prop") %>%
  mutate(threat = case_when(threat == "prop_threat" ~ "Threatened",
                            TRUE ~ "Unthreatened"),
         prop = round(prop, 2),
         Country = toupper(Country))


for (tg in unique(threatened_sps$taxonomic_grouyp)) {
  fplot <- threatened_sps %>% filter(taxonomic_grouyp == tg)

  p <- ggplot(data = fplot,
         mapping = aes(x = Country, y = prop, group = threat, fill = threat))+
    geom_col(aes(), position = position_fill(), color = NA)+
    scale_fill_manual(values = c("#ef0000", "#146152"))+
    geom_text(aes(label = paste0(prop, "%"), y = prop/2), size = 5,
              color = if_else(fplot$threat == "Unthreatened", "white", "black"),
              position = position_fill(vjust = .8))+

    scale_y_continuous(expand = c(-.22, .3))+
    coord_polar()+
    labs(fill = "")+
    guides(fill = guide_legend(
      keywidth = unit(2, "lines"),
      keyheight = unit(.5, "lines")
    ))+
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_blank(),
      # axis
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = 12),
      legend.direction = "horizontal",
      legend.position = "bottom",
      legend.text = element_text(size = 12)
    )
  save_path <- paste0("D:\\QGIS Projects\\Shp\\WA\\plots\\", tg, "_Threated_prop.jpeg")
  ggsave(plot = p, filename = save_path, width = 25, height = 25, units = "cm")

}

# Global
threatened_sps_gl <- read_excel("D:\\QGIS Projects\\Shp\\WA\\Data_all_species_groups.xlsx") %>%
  select(1:4) %>%
  rename(taxonomic_grouyp = 2, total_assessed = 3, threatened = 4) %>%
  summarise(total_assessed = sum(total_assessed),
            threatened = sum(threatened),
            .by = "Country") %>%
  rowwise() %>%
  mutate(prop_threat = threatened*100/total_assessed,
         prop_non_threat = 100 - prop_threat) %>%
  select(Country, prop_threat, prop_non_threat) %>%
  tidyr::pivot_longer(cols = !c(Country),
                      names_to = "threat", values_to = "prop") %>%
  mutate(threat = case_when(threat == "prop_threat" ~ "Threatened",
                            TRUE ~ "Unthreatened"),
         prop = round(prop, 2),
         Country = toupper(Country))


p <- ggplot(data = threatened_sps_gl,
            mapping = aes(x = Country, y = prop, group = threat, fill = threat))+
  geom_col(aes(), position = position_fill(), color = NA)+
  scale_fill_manual(values = c("#ef0000", "#146152"))+
  geom_text(aes(label = paste0(prop, "%"), y = prop/2), size = 5,
            color = if_else(fplot$threat == "Unthreatened", "white", "black"),
            position = position_fill(vjust = .8))+

  scale_y_continuous(expand = c(-.22, .3))+
  coord_polar()+
  labs(fill = "")+
  guides(fill = guide_legend(
    keywidth = unit(2, "lines"),
    keyheight = unit(.5, "lines")
  ))+
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_blank(),
    # axis
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 12),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  )
save_path <- paste0("D:\\QGIS Projects\\Shp\\WA\\plots\\", "Global", "_Threated_prop.jpeg")
ggsave(plot = p, filename = save_path, width = 25, height = 25, units = "cm")
