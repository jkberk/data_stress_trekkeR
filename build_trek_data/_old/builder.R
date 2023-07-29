library(rtrek)

# Get all available character base data (page 1;last page (75))
stapi_chararacter_pages  <- stapi("character", page = 1:1)

# Extract all character UIDs
uids <- stapi_chararacter_pages$uid

# Init progress bar
pb <- progress::progress_bar$new(total = length(uids))$

# Loop through each UID and get all information on registered characters/UIDs
stapi_raw_characters <- list()  # Init list
for (uid in uids) {
  character_info <- stapi("character", uid = uid)
  stapi_raw_characters[[uid]] <- character_info
  
  # Progress bar
  pb$tick()
}

# Save raw list
readr::write_rds(stapi_raw_characters, "./build_trek_data/data/stapi_raw_character_data.rds")

# Flatten the raw list
stapi_characters_flat <- stapi_raw_characters %>%
  purrr::map_dfr(~ as.data.frame(t(unlist(.))), .id = "uid")

stapi_characters_flat <- map_df(stapi_raw_characters, ~flatten_df(.x))

# Save flat tbl_df 
readr::write_rds(stapi_characters_flat, "./build_trek_data/data/stapi_raw_character_data_flat.rds")





# test
library(rtrek)
library(tidyverse)

# Get all availasble character base data (page 1;last page (75))
stapi_chararacter_pages  <- stapi("character", page = 1:1)

# Extract all character UIDs
uids <- stapi_chararacter_pages$uid

# Init progress bar
pb <- progress::progress_bar$new(total = length(uids))
  
# Loop through each UID and get all information on registered characters/UIDs
stapi_raw_characters <- list()  # Init list
data_episodes <- list()
data_movies  <- list()
data_species <- list()

for (uid in uids) {
  data_episodes[[uid]] <- stapi("character", uid = uid)$episodes
  data_movies[[uid]] <- stapi("character", uid = uid)$movies
  data_species[[uid]] <- stapi("character", uid = uid)$characterSpecies
  
  # Progress bar
  pb$tick()
}

# Flatten the data_episodes list
data_flat_episodes <- bind_rows(data_episodes, .id = "uid") |> jsonlite::flatten()
data_flat_movies <- bind_rows(data_movies, .id = "uid") |> jsonlite::flatten()
data_flat_species <- bind_rows(data_species, .id = "uid") |> jsonlite::flatten()




view(data_flat_episodes)
data_flat_episodes$series$title
data_flat_episodes$series$uid

data_flat_episodes <- data_episodes %>%
  map_dfr(flatten_df, .id = "uid")


stapi_chararacter_pages |> 
  left_join(data_flat_episodes) |> 
  left_join(data_flat_titles) |> 
  filter(uid == "CHMA0000275724") |> 
  view()

# View the flattened dataframe
head(flatten_episodes)

