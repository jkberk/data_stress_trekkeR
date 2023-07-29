library(rtrek)
library(tidyverse)

# Get overview of available entities. 
rtrek::stapiEntities |> view()

# Get all availasble character base data (page 1;last page (75))
stapi_chararacter_pages  <- stapi("character", page = 1:75)
stapi_species_pages <- stapi("species", page = 1:7)

# Extract all character UIDs
chr_uids <- stapi_chararacter_pages$uid
spc_uids <- stapi_species_pages$uid


  
# Loop through each UID and get all information on registered characters/UIDs
data_chr_episodes <- list()
data_chr_movies  <- list()
data_chr_species <- list()
data_chr_organizations <- list()
data_chr_characterRelations <- list()
data_chr_occupations <- list()

data_spc_species <- list()

pb <- progress::progress_bar$new(total = length(chr_uids))
for (uid in chr_uids) {
  # data_chr_episodes[[uid]] <- stapi("character", uid = uid)$episodes
  # data_chr_movies[[uid]] <- stapi("character", uid = uid)$movies
  
  # --> data_chr_species[[uid]] <- stapi("character", uid = uid)$characterSpecies
  # --> data_chr_organizations[[uid]] <- stapi("character", uid = uid)$organizations
  # --> data_chr_characterRelations[[uid]] <- stapi("character", uid = uid)$characterRelations
  # --> data_chr_occupations[[uid]] <- stapi("character", uid = uid)$occupations

  pb$tick() # progress bar  ticker
}

pb <- progress::progress_bar$new(total = length(spc_uids))
for (uid in spc_uids) {
  data_spc_species[[uid]] <- stapi("species", uid = uid)
  
  pb$tick() # progress bar  ticker
}

#readr::write_rds(data_chr_episodes, "./build_trek_data/data/data_chr_episodes.rds")
#readr::write_rds(data_chr_movies, "./build_trek_data/data/data_chr_movies.rds")
readr::write_rds(data_chr_species, "./build_trek_data/data/data_chr_species.rds")
readr::write_rds(data_chr_organizations, "./build_trek_data/data/data_chr_organizations.rds")
readr::write_rds(data_chr_characterRelations, "./build_trek_data/data/data_chr_characterRelations.rds")
readr::write_rds(data_chr_occupations, "./build_trek_data/data/data_chr_occupations.rds")

readr::write_rds(data_spc_species, "./build_trek_data/data/data_spc_species.rds")




# Flatten the data_episodes list
data_flat_chr_episodes <- bind_rows(data_episodes, .id = "uid") |> jsonlite::flatten()
data_flat_chr_movies <- bind_rows(data_movies, .id = "uid") |> jsonlite::flatten()
data_flat_chr_species <- bind_rows(data_species, .id = "uid") |> jsonlite::flatten() # |> select(uid, name) |> rename(species = name)
# ^ -- need to rebuild species for a couple of characters. 

data_flat_spc_species <- bind_rows(data_spc_species, .id = "uid") |> jsonlite::flatten()


# Join with character data: 
data_chr_eps_mov_spec2 <- stapi_chararacter_pages |> 
  left_join(data_flat_episodes, by = "uid") |> 
  left_join(data_flat_movies, by = "uid") |> 
  left_join(data_flat_species, by = "uid")

# write
readr::write_rds(data_chr_eps_mov_spec, "./build_trek_data/data/data_chr_eps_mov_spec.rds")

