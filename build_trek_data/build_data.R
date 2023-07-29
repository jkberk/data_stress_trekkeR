# ~ libs ------------------------------------------------------------------

library(rtrek)
library(tidyverse)



# ~ get data: character section (premade) ---------------------------------

data_chr_characters <- readr::read_rds("./build_trek_data/data/data_chr_characters.rds")



# # ~ get data: character section -------------------------------------------
# 
# # get all availasble character base data (page 1;last page (75))
# stapi_chararacter_pages  <- stapi("character", page = 1:75)
# 
# # extract all character uids 
# chr_uids <- stapi_chararacter_pages$uid
# 
# # init progress bar (used in loop below )
# pb <- progress::progress_bar$new(total = length(chr_uids))
# 
# # get data for one uid at a time 
# data_chr_characters <- list()
# 
# for (uid in uids) {
#   character_info <- stapi("character", uid = uid)
#   data_chr_characters[[uid]] <- character_info
#   
#   pb$tick()
# }



# ~ get data: species section (premade) -----------------------------------

data_spc_species  <- readr::read_rds("./build_trek_data/data/data_spc_species.rds")



# # ~ get data: species section ---------------------------------------------
# 
# # get all available species base data (page 1;last page (7))
# stapi_species_pages <- stapi("species", page = 1:7)
# 
# # extract all species uids 
# spc_uids <- stapi_species_pages$uid
# 
# # init progress bar (used in loop below )
# pb <- progress::progress_bar$new(total = length(spc_uids))
# 
# # get data for one uid at a time 
# data_spc_species <- list()
# 
# for (uid in spc_uids) {
#   data_spc_species[[uid]] <- stapi("species", uid = uid)
#   
#   pb$tick() # progress bar ticker
# }



# ~ save raw data ---------------------------------------------------------

# character section
#readr::write_rds(data_chr_characters, "./build_trek_data/data/data_chr_charactersa.rds")

# species section
#readr::write_rds(data_spc_species, "./build_trek_data/data/data_spc_species.rds")



# ~ extract data to tibble: character section -----------------------------

# base data for characters (same as stapi_character_pages)
df_chr_base <- data_chr_characters |> 
  map_dfr(~ tibble(
    uid = pluck(.x, "uid"),  # character uid 
    name = pluck(.x, "name"),
    gender = pluck(.x, "gender"),
    yearOfBirth = pluck(.x, "yearOfBirth"),
    monthOfBirth = pluck(.x, "monthOfBirth"),
    dayOfBirth = pluck(.x, "dayOfBirth"),
    placeOfBirth = pluck(.x, "placeOfBirth"),
    yearOfDeath = pluck(.x, "yearOfDeath"),
    monthOfDeath = pluck(.x, "monthOfDeath"),
    dayOfDeath = pluck(.x, "dayOfDeath"),
    placeOfDeath = pluck(.x, "placeOfDeath"),
    height = pluck(.x, "height"),
    weight = pluck(.x, "weight"),
    deceased = pluck(.x, "deceased"),
    bloodType = pluck(.x, "bloodType"),
    maritalStatus = pluck(.x, "maritalStatus"),
    serialNumber = pluck(.x, "serialNumber"),
    hologramActivationDate = pluck(.x, "hologramActivationDate"),
    hologramStatus = pluck(.x, "hologramStatus"),
    hologramDateStatus = pluck(.x, "hologramDateStatus"),
    hologram = pluck(.x, "hologram"),
    fictionalCharacter = pluck(.x, "fictionalCharacter"),
    mirror = pluck(.x, "mirror"),
    alternateReality = pluck(.x, "alternateReality")
  ))

# performers = pluck(.x, "performers" ...), # <-- did not include. 

# episode vars: 
df_chr_episodes <- data_chr_characters |> 
  map_dfr(~ tibble(
    uid = pluck(.x, "uid"),  # character uid 
    episodes.title = pluck(.x, "episodes", "title"),
    episodes.series = pluck(.x, "episodes", "series", "title"),
    episodes.seasonNumber = pluck(.x, "episodes", "seasonNumber"),
    episodes.episodeNumber = pluck(.x, "episodes", "episodeNumber"),
    episodes.featureLength = pluck(.x, "episodes", "featureLength"),
    episodes.stardateFrom = pluck(.x, "episodes", "stardateFrom"),
    episodes.stardateTo = pluck(.x, "episodes", "stardateTo"),
    episodes.yearFrom = pluck(.x, "episodes", "yearFrom"),
    episodes.yearTo = pluck(.x, "episodes", "yearTo"),
    episodes.usAirData = pluck(.x, "episodes", "usAirDate"),
    episodes.finalScriptData = pluck(.x, "episodes", "finalScriptDate")
  ))  

# movie vars:
df_chr_movies <- data_chr_characters |> 
  map_dfr(~ tibble(
    uid = pluck(.x, "uid"),  # character uid 
    movies.title = pluck(.x, "movies", "title"),
    movies.mainDirector = pluck(.x, "movies", "mainDirector", "name"),
    movies.stardateFrom = pluck(.x, "movies", "stardateFrom"),
    movies.stardateTo = pluck(.x, "movies", "stardateTo"),
    movies.yearFrom = pluck(.x, "movies", "yearFrom"),
    movies.yearTo = pluck(.x, "movies", "yearTo"),
    movies.usReleaseDate = pluck(.x, "movies", "usReleaseDate")
  ))

# species vars: 
df_chr_species <- data_chr_characters |> 
  map_dfr(~ tibble(
    uid = pluck(.x, "uid"),  # character uid 
    characterSpecies.name = pluck(.x, "characterSpecies", "name"),
    characterSpecies.numerator = pluck(.x, "characterSpecies", "numerator"),
    characterSpecies.denominator = pluck(.x, "characterSpecies", "denominator")
  ))

# character relations (mom, dad, son, wife etc.)
df_chr_relations <- data_chr_characters |> 
  map_dfr(~ tibble(
    uid = pluck(.x, "uid"),  # character uid 
    characterRelations.type = pluck(.x, "characterRelations", "type"),
    characterRelations.source = pluck(.x, "characterRelations", "source", "name"),
    characterRelations.target = pluck(.x, "characterRelations", "target", "name")
  ))

# title vars (rank etc):
df_chr_titles <- data_chr_characters |> 
  map_dfr(~ tibble(
    uid = pluck(.x, "uid"),  # character uid 
    titles.name = pluck(.x, "titles", "name"),
    titles.militaryRank = pluck(.x, "titles", "militaryRank"),
    titles.fleetRank = pluck(.x, "titles", "fleetRank"),
    titles.religiousTitle = pluck(.x, "titles", "religiousTitle"),
    titles.position = pluck(.x, "titles", "position"),
    titles.mirror = pluck(.x, "titles", "mirror")
  ))

# occupation vars: 
df_chr_occupations<- data_chr_characters |> 
  map_dfr(~ tibble(
    uid = pluck(.x, "uid"),  # character uid 
    occupations.name = pluck(.x, "occupations", "name"),
    occupations.legalOccupation = pluck(.x, "occupations", "legalOccupation"),
    occupations.medicalOccupation = pluck(.x, "occupations", "medicalOccupation"),
    occupations.scientificOccupation = pluck(.x, "occupations", "scientificOccupation")
  ))

# organizations vars:
df_chr_organizations <- data_chr_characters |> 
  map_dfr(~ tibble(
    uid = pluck(.x, "uid"),  # character uid 
    organizations.name = pluck(.x, "organizations", "name"),
    organizations.government = pluck(.x, "organizations", "government"),
    organizations.intergovernmentalOrganization = pluck(.x, "organizations", "intergovernmentalOrganization"),
    organizations.researchOrganization = pluck(.x, "organizations", "researchOrganization"),
    organizations.sportOrganization = pluck(.x, "organizations", "sportOrganization"),
    organizations.medicalOrganization = pluck(.x, "organizations", "medicalOrganization"),
    organizations.militaryOrganization = pluck(.x, "organizations", "militaryOrganization"),
    organizations.militaryUnit = pluck(.x, "organizations", "militaryUnit" ),
    organizations.governmentAgency = pluck(.x, "organizations", "governmentAgency" ),
    organizations.lawEnforcementAgency = pluck(.x, "organizations", "lawEnforcementAgency"),
    organizations.prisonOrPenalColony = pluck(.x, "organizations", "prisonOrPenalColony"),
    organizations.mirror = pluck(.x, "organizations", "mirror"),
    organizations.alternateReality = pluck(.x, "organizations", "alternateReality")
  ))



# ~ extract data to tibble: species section -------------------------------

df_spc_species <- data_spc_species |> 
  map_dfr(~ tibble(uid                     = pluck(.x, "uid"),
                   name                    = pluck(.x, "name"),
                   homeworld.name          = pluck(.x, "homeworld", "name"),
                   homeworld.location      = pluck(.x, "homeworld", "location", "name"),
                   quadrant.name           = pluck(.x, "quadrant", "name"),
                   extinctSpecies          = pluck(.x, "extinctSpecies"),
                   warpCapableSpecies      = pluck(.x, "warpCapableSpecies"),
                   extraGalacticSpecies    = pluck(.x, "extraGalacticSpecies"),
                   humanoidSpecies         = pluck(.x, "humanoidSpecies"),
                   reptilianSpecies        = pluck(.x, "reptilianSpecies"),
                   nonCorporealSpecies     = pluck(.x, "nonCorporealSpecies"),
                   shapeshiftingSpecies    = pluck(.x, "shapeshiftingSpecies"),
                   spaceborneSpecies       = pluck(.x, "spaceborneSpecies"),
                   telepathicSpecies       = pluck(.x, "telepathicSpecies"),
                   transDimensionalSpecies = pluck(.x, "transDimensionalSpecies"),
                   unnamedSpecies          = pluck(.x, "unnamedSpecies")
  ))

# test if one row per uid
#df_spc_species|> select(uid) |> group_by(uid) |> count() |> pull() |> unique() == 1



# ~ join eet all ----------------------------------------------------------

df_stapi_flat <- df_chr_base |> 
  left_join(df_chr_episodes, by = "uid", relationship = "many-to-many") |>
  left_join(df_chr_movies, by = "uid", relationship = "many-to-many") |>
  left_join(df_chr_occupations, by = "uid", relationship = "many-to-many") |>
  left_join(df_chr_organizations, by = "uid", relationship = "many-to-many") |>
  left_join(df_chr_relations, by = "uid", relationship = "many-to-many") |>
  left_join(df_chr_species, by = "uid", relationship = "many-to-many") |> 
  left_join(df_chr_titles, by = "uid", relationship = "many-to-many") |> 
  
  left_join(df_spc_species, by = c("characterSpecies.name" = "name"), relationship = "many-to-many") 



# ~ save flat dataframe ---------------------------------------------------

readr::write_rds(df_stapi_flat, "./build_trek_data/data/df_stapi_flat.rds", "gz")


# 
# # make long ---------------------------------------------------------------
# 
# # episodes, movies, characterSpecies, relations, titles, occupations, organizations, ends_with("Species", homeworld.name, homeworld.location,quadrant.name )
# 
# cols_species <- c(
#   # "characterSpecies.name",             
#   # "homeworld.name",         
#   # "homeworld.location",     
#   # "quadrant.name",      
#   "extinctSpecies",      
#   "warpCapableSpecies",     
#   "extraGalacticSpecies",   
#   "humanoidSpecies",        
#   "reptilianSpecies",       
#   "nonCorporealSpecies",    
#   "shapeshiftingSpecies",   
#   "spaceborneSpecies",      
#   "telepathicSpecies",      
#   "transDimensionalSpecies",
#   "unnamedSpecies"
# )
# 
# df_long_<- df_stapi_flat |> 
#   pivot_longer(cols = starts_with("movies"),
#                names_to = "movie_name",
#                values_to = "movie_data") |> 
#   pivot_longer(cols = all_of(cols_species),
#                names_to = "species_details",
#                values_to = "species_data") |> 
#   
#   
#   drop_na(species_data)
