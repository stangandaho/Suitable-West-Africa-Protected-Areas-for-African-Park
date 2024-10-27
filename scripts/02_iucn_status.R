# load packages
library(ggplot2)
library(dplyr)
library(httr2)

# import unique species across west-africa
spec <- read.csv("datasets/kept_species.csv")


# import west africa species
wa_species <- read.csv("datasets/west_africa_species.csv")
wa_species <- wa_species %>%
  filter(species %in% spec$species)

# create taxon key
taxon_key <- wa_species %>%
  distinct(species,.keep_all = TRUE) %>%
  select(species, taxonKey)

# use GBIF API to query iucn category
iucn_status <- function(usage_key, timeout = 2) {
  api_url <- paste0("https://api.gbif.org/v1/species/", usage_key, "/iucnRedListCategory")

    rq <- httr2::request(api_url) %>%
      httr2::req_timeout(timeout*60) %>%
      httr2::req_perform()

    if (rq$status == 200) {
      status <- rq %>%
        httr2::resp_body_json()

      status_data <- dplyr::tibble(
        category = gsub("_", replacement = " ", status$category),
        category_code = status$code,
        iucn_taxon_id = status$iucnTaxonID,
        gbif_taxon_key = status$usageKey,
        taxonomic_status = status$taxonomicStatus
      )
    }else{
      status_data <- dplyr::tibble(
        category = NA,
        category_code = NA,
        iucn_taxon_id = NA,
        gbif_taxon_key = usage_key,
        taxonomic_status = NA
      )
    }

  return(status_data)
}

get_iucn_status <- function(data = NULL, species, taxon_key, timeout = 2) {

  # pull sets
  if (!is.null(data) & (hasArg(species) | hasArg(taxon_key))) {
    # Check species and taxon_key are in data provided
    is_in <- c(species, taxon_key) %in% colnames(data)
    if (!any(is_in)) {
      if (length(is_in[is_in == FALSE]) == 1) {
        stop(sprintf("%s is not in data provided", colnames(data)[!is_in]))
      }else{
        stop(sprintf("%s are not in data provided", paste(colnames(data)[!is_in]), sep = ", "))
      }

    }

    taxon_keys <- data %>% pull(taxon_key)
    species <- data %>% pull(species)
  }else{
    taxon_keys <- taxon_key
  }

  if (hasArg(species)) {
    species <- species
  }

  status_list <- list()
  total_key <- length(taxon_keys)
  lvl <- 0

  # Loop through th key
    for (tk in taxon_keys) {
      # message to display the progress
      lvl <- lvl + 1
      print(
        noquote(
          paste0("Query for taxon number ", lvl, "/", total_key,
                 " (", round(lvl/total_key, 3)*100, "%)")
        )
      )
      spec_status <- iucn_status(tk, timeout = timeout)

      #add species name
        sps <- species[lvl]
        spec_status <- spec_status %>%
          mutate(species = sps) %>%
          relocate(species, .before = 1)

      status_list[[tk]] <- spec_status
    }

  return(bind_rows(status_list))
}

all_status <- get_iucn_status(data = taxon_key, timeout = 10,
                              species = "species",
                              taxon_key = "taxonKey")


# Write and edit (compete manually NA data by searching on IUCN redlist)
all_status_edited <- read.csv(file = "datasets/species_with_iucn_status_edited.csv")

# Search new species
new_species <- all_status$species[!unique(all_status$species) %in% unique(all_status_edited$species)]

new_species_without_category <- all_status %>%
  filter(species %in% new_species & is.na(category))
# Export new species without category for editing
write.csv(new_species_without_category, "datasets/species_with_iucn_status.csv",
          row.names = F)
# Import new_species_without_category after editing in excel
nswc_edited <- read.csv("datasets/sw_iucn_status.csv")

# Filter new species with category
ns_with_cat <- all_status %>%
  filter(species %in% new_species & !is.na(category))

# filter not not new species and have NA in category but filled in all_status_edited
has_na <- all_status %>%
  filter(is.na(category))

sp_ <- has_na %>% pull(species)
species_data <- has_na <- all_status %>%
  filter(!is.na(category))

to_ <- bind_rows(all_status_edited, nswc_edited) %>%
  filter(species %in% sp_)

species_data <- species_data %>%
  bind_rows(to_)

write.csv(species_data,
          "datasets/species_data.csv",
          row.names = FALSE)#
