library(httr2)
library(dplyr)

api_key <- "4L4CRqZ9ua3e9qrvPHAtnFS3yish7feHZTo1"

# Send the request with the correct authorization header
# functon to get count of the number of species which have assessments.
get_count <- function(country = "BJ", api_key){
  query_url <- paste0("https://api.iucnredlist.org/api/v4/countries/", country) %>%
    httr2::request() %>%
    httr2::req_headers(
      accept = "application/json",
      Authorization = api_key
    ) %>%
    httr2::req_perform()

  total_count <- as.numeric(query_url$headers$`Total-Count`)
  return(total_count)
}
total_count <- get_count(api_key = api_key)

#function to get all assement by country
get_assements <- function(country = "BJ", api_key, total_count){
  if (total_count%%100 > 0) {
    page_number <- 9406%/%100 + 1
  }else{
    page_number <- 9406%/%100
  }

  assessment <- list()
  for (i in 1:page_number) {
    print(paste0("On page ", i, "/", page_number))

  query_url <- paste0("https://api.iucnredlist.org/api/v4/countries/",
                      country,
                      "?page=",i, "&per_page=100") %>%
    httr2::request() %>%
    httr2::req_headers(
      accept = "application/json",
      Authorization = api_key
    ) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json(simplifyDataFrame = T)

  assessment[[i]] <- query_url$assessments
  Sys.sleep(2)
  }

  return(bind_rows(assessment))

  }


ben_assement <- get_assements(api_key = api_key, total_count = total_count)
write.csv(ben_assement %>% select(-scopes), "datasets/bj_assessments.csv", row.names = FALSE)
# Get unique species assessment id
ben_assement <- read.csv("datasets/bj_assessments.csv")
unique(ben_assement$sis_taxon_id)

get_species_info <- function(sis_id, api_key = api_key) {

  sps_info <<- list()
  for (sid in 1:length(sis_id)) {
    #idx <- which(sis_id == sid)
  print(paste(round(sid/length(sis_id), 4), "%"))
  taxon <- paste0("https://api.iucnredlist.org/api/v4/taxa/sis/",
                  sis_id[sid]) %>%
    httr2::request() %>%
    httr2::req_headers(
      accept = "application/json",
      Authorization = api_key
    ) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json(simplifyDataFrame = T)
 taxon <- taxon$taxon
 #print(taxon$authority)
  df_ <- list(scientific_name = taxon$scientific_name,
                    species_taxa = unlist(taxon$species_taxa),
                    subpopulation_taxa = unlist(taxon$subpopulation_taxa),
                    infrarank_taxa = unlist(taxon$infrarank_taxa),
                    kingdom_name = unlist(taxon$kingdom_name),
                    phylum_name = unlist(taxon$phylum_name),
                    class_name = unlist(taxon$class_name),
                    order_name = unlist(taxon$order_name),
                    family_name = unlist(taxon$family_name),
                    genus_name = unlist(taxon$genus_name),
                    species_name = unlist(taxon$species_name),
                    authority = unlist(taxon$authority))

 sps_info[[sid]] <- df_
  Sys.sleep(1)
  }

  return(bind_rows(sps_info))
}
sps_info <- get_species_info(sis_id = unique(ben_assement$sis_taxon_id),
                 api_key = api_key)

# Get species info by web scrap
#load pac
species_info <- rvest::read_html("https://www.iucnredlist.org/species/137286/522738") %>%
  rvest::html_element("p") %>%
  rvest::html_text()


rD <- rsDriver(browser = "chrome", port = 4545L)
remDr <- rD[["client"]]

#redlist-js > div > header > div > div > div > h1
