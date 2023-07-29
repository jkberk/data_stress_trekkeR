Q <- "CHMA0000025118"
Q <- stapi("character", uid = Q)

q_eps <- Q$episodes %>% select(uid, title, stardateFrom, stardateTo)
q_eps

eps <- c("Encounter at Farpoint", "All Good Things...")
q_eps <- filter(q_eps, title %in% eps)
q_eps

eaf <- stapi("episode", uid = q_eps$uid[q_eps$title == eps[1]])
agt <- stapi("episode", uid = q_eps$uid[q_eps$title == eps[2]])
characters <- setdiff(intersect(eaf$characters$name, agt$characters$name), "Q")
characters

characters <- eaf$characters %>% select(uid, name) %>% filter(name %in% characters)
characters

eps_count <- rowwise(characters) %>% 
  do(stapi("character", uid = .$uid)$episodes$series %>% 
       summarize(n = sum(title == "Star Trek: The Next Generation"))
  )
eps_count <- select(characters, name) %>% bind_cols(eps_count)


# test
rowwise(characters) %>% 
  do(stapi("character", uid = .$uid)$episodes$series
  )


# -------------------------------------------------------------------------

# Retrieve all characters
df_trek_chr <- stapi("character", page = 2:2) |> 
  select(where(~ any(!is.na(.) & . != FALSE)))

df_trek_chr_filtered <- df_trek_chr  %>%
  filter(if_any(gender:hologramStatus, ~ !is.na(.)))


df <- rowwise(df_trek_chr_filtered) |> 
  do(stapi("character", uid = .$uid))


df_trek_chr <- stapi("character", page = 2:2)
df_trek_chr_eps_test <- lapply(df_trek_chr, function(i) {
  stapi("character", uid = df_trek_chr$uid[i])$episodes 
})

df_eps_test <- stapi("episodes", )

eps_count <- rowwise(characters) %>% 
  do(stapi("character", uid = .$uid)$episodes$series 
  )




stapi("character", uid = df_trek_chr$uid[1])$episodes

df_trek_chr_eps_test <- lapply(df_trek_chr, function(i) {
  character_eps <- stapi("character", uid = df_trek_chr$uid[i])$episodes
  if (length(character_eps) > 0) {
    character_eps
  } else {
    NULL
  }
})





df_trek_chr_eps_fixed <- df_trek_chr_eps |> 
  mutate(
    series_uid = series$uid,
    series_title = series$title,
    season_uid = season$uid,
    season_title = season$title
  ) |> 
  select(-series, -season)



df_trek_chr_eps_fixed <- df_trek_chr_eps %>%
  select(-series, -season)

str(df_trek_chr_eps_fixed)


dput(df_trek_chr_eps_fixed )

view(df_trek_chr_eps_fixed )

df_trek_chr_eps |> view()
cleaned_names <- sub("\\$", "", names(df_trek_chr_eps ))
colnames(df_trek_chr_eps) <- cleaned_names

# Retrieve episodes for each character
character_episodes <- lapply(df_trek_characters$uid, function(uid) {
  character <- stapi("character", uid = uid)
  episodes <- character$episodes
  episodes$name <- character$name
  episodes
})

# Combine all episodes into a single data frame
all_episodes <- do.call(rbind, character_episodes)

# Print the data frame
print(all_episodes)
