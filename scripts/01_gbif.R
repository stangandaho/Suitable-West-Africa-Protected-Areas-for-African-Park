library(dplyr)
library(sf)

wa_spec <- read.csv("datasets/0015384-241007104925546.csv",
                    sep = "\t", header = T)

wa_spec_fl <- wa_spec %>%
  mutate(obs = paste0(decimalLongitude, decimalLatitude)) %>%
  distinct(obs, .keep_all = TRUE) %>%
  select(-obs) %>%
  filter(phylum == "Chordata")

write.csv(wa_spec_fl,
          "datasets/west_africa_species.csv",
          row.names = FALSE)

# import the shapefile
wa_pa_shp <- sf::read_sf("west africa/Shp_AP_WA_500km2/AP_wa_50000ha_ok.shp") %>%
  filter(Area >= 500)

st_delete("D:\\QGIS Projects\\Shp\\WA\\wa_protected_area_high_area.shp")
sf::write_sf(wa_pa_shp, "west africa/wa_protected_area_high_area.shp")

# convert to point shape
wa_spec_fl <- read.csv("datasets/west_africa_species.csv")
wa_spec_shp <- wa_spec_fl %>%
  select(class, order,family, genus, scientificName, species, decimalLatitude, decimalLongitude) %>%
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>%
  st_transform(crs = st_crs(wa_pa_shp))

countryco <- countrycode::codelist %>%
  filter(iso3c %in% unique(wa_pa_shp$iso3)) %>%
  select(country.name.fr, iso3c) %>%
  rename(iso3 = iso3c, COUNTRY = country.name.fr)

wa_pa_shp <- wa_pa_shp %>%
  left_join(y = countryco, by = "iso3")

##
#clip_list <- list()
for (ft in 1:nrow(wa_pa_shp)) {
  print(ft)
  get_inter <- st_intersects(wa_pa_shp[ft, 1] %>%
                               st_make_valid() %>%
                               st_buffer(dist = 5000),
                             y = wa_spec_shp)
  within_pa <- wa_spec_shp[get_inter[[1]], ] %>%
    mutate(NAME = wa_pa_shp[ft, 1]%>% pull(name))
  clip_list[[ft]] <- within_pa
}

# get_inter <- st_intersects(wa_pa_shp[1, 1] %>% pull(NAME) %>% st_buffer(dist = 5000), wa_spec_shp)
# within_pa <- wa_spec_shp[get_inter[[1]], ]

clip_list_df <- bind_rows(clip_list)
write.csv(clip_list_df %>% st_drop_geometry(),
          "datasets/west_africa_protected_area_species.csv",
          row.names = FALSE)

st_delete("west africa/west_africa_protected_area_species.shp")
st_write(clip_list_df, "D:\\QGIS Projects\\Shp\\WA\\west_africa_protected_area_species.shp")


# select unique observation by protected area
source("utils.R")
unique_species <- clip_list_df %>%
  distinct(NAME, species, .keep_all = TRUE) %>%
  filter(!order %in% fish_order)

# All kept species
write.csv(data.frame(species = unique(unique_species$species)),
          "datasets/kept_species.csv",
          row.names = FALSE)
