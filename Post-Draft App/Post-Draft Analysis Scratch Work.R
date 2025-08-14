#This project was less than a week long, so data cleaning for names, while successful, was not super structured. However, I periodically saved versions
#and was very careful to not overwrite or lose data while ensuring rosters were complete on the site.
#Also had some player data get omitted with some of the joining, so took care of that here as well.

player_playtype_master %>% filter(Player == "Tyler Herro")


player_cut_off_int_sub %>% filter(Player == "Noa Essengue")

player_spot_off_int_sub %>% filter(Player =="Noa Essengue")


nba_combined_summer$Playe

player_playtype_master_dedup %>% filter(Player == "De'Aaron Fox")


nrow(player_playtype_master_dedup)
nrow(nba_combined_summer)



unique(nba_combined_summer$Player[grepl("Fox", nba_combined_summer$Player)])


nba_full_combined_percentiled <- nba_full_combined_percentiled %>%
  mutate(Player = case_when(
    Player == "De Aaron Fox" ~ "De'Aaron Fox",
    TRUE ~ Player
  ))

nba_full_combined_percentiled <- nba_full_combined_percentiled %>%
  filter(!(Player == "De'Aaron Fox" & Team.x == "Sacramento Kings"))
nba_full_combined_percentiled <- nba_full_combined_percentiled %>%
  mutate(Pos.x = ifelse(Player == "De'Aaron Fox" & Team.x == "San Antonio Spurs", "G", Pos.x))

saveRDS(nba_full_combined_percentiled, "nba_full_combined_percentiled.rds")

#nba_full_combined_percentiled_backedup <- readRDS("~/Post-Draft App/data/nba_full_combined_percentiled.rds")
nba_full_combined_percentiled_backedup %>% filter(Player == "Luka Doncic") %>% select(PTS,TRB)



nba_combined_summer %>% filter(Player == "De'Aaron Fox")

nba_combined_summer$`PTS/G`


fox_stats <- nba_combined_summer %>%filter(Player == "DeAaron Fox") %>%select(PTS, PTS_pctile, AST, AST_pctile, ORB, ORB_pctile, TRB, TRB_pctile,PF, PF_pctile, TOV, TOV_pctile, BLK, BLK_pctile, STL, STL_pctile,
         OWS, OWS_pctile, DWS, DWS_pctile, `TS%`, `TS%_pctile`,
         `3P%`, `3P%_pctile`, `FT%`, `FT%_pctile`)



nba_full_combined_percentiled %>% filter(Player == "Caleb Martin")


nba_full_combined_percentiled %>% filter(Player == "De'Aaron Fox") %>% select(Player, Team.x, Pos.x)




player_playtype_master_dedup %>% filter(Player =="Kel'el Ware")





nba_combined_summer <- nba_combined_summer %>%
  mutate(Player = if_else(Player == "Kelel Ware", "Kel'el Ware", Player))

summer_row <- nba_combined_summer %>% filter(Player == "Kel'el Ware")
master_row <- player_playtype_master_dedup %>% filter(Player == "Kel'el Ware")

combined_row <- full_join(master_row, summer_row, by = "Player")
combined_row %>% filter(Player == "Kel'el Ware") %>% select(`PTS/G...3`,`PTS/G...3`,`PTS/G...27`,`PTS/G`)
combined_row$PTS <- combined_row$`PTS/G...27`

nba_full_combined <- bind_rows(nba_full_combined, combined_row)

nba_full_combined <- nba_full_combined %>%
  filter(!(Player == "Kel'el Ware." & is.na(MP))) %>%
  distinct(Player, .keep_all = TRUE)

nba_full_combined %>% filter(Player == "Kel'el Ware") %>% select(MP)



player_playtype_master_dedup %>% filter(Player == "Day'Ron Sharpe")
nba_combined_summer %>% filter(Player == "DayRon Sharpe")


master_row <- player_playtype_master_dedup %>% filter(Player == "Kel'el Ware")
summer_row <- nba_combined_summer %>% filter(Player == "Kel'el Ware")

combined_row <- master_row %>% left_join(summer_row, by = "Player")
combined_row$`PTS/G`

nba_full_combined$`PTS/G` <- nba_full_combined$PTS

nba_full_combined <- bind_rows(nba_full_combined, combined_row)
nba_full_combined <- nba_full_combined %>%
  filter(!(Player == "Kel'el Ware" & is.na(`PTS/G...123`)))

player_playtype_master_dedup <- player_playtype_master_dedup %>%
  mutate(Player = if_else(Player == "Nicolas Claxton", "Nic Claxton", Player))

master_row <- player_playtype_master_dedup %>% filter(Player == "Nic Claxton")
summer_row <- nba_combined_summer %>% filter(Player == "Nic Claxton")

combined_row <- master_row %>% left_join(summer_row, by = "Player")

nba_full_combined <- bind_rows(nba_full_combined, combined_row)

nba_full_combined <- nba_full_combined %>%
  filter(!(Player == "Nic Claxton" & is.na(`PTS/G...123`)))
nba_combined_summer <- nba_combined_summer %>%
  mutate(Player = if_else(Player == "DayRon Sharpe", "Day'Ron Sharpe", Player))

master_row <- player_playtype_master_dedup %>% filter(Player == "Day'Ron Sharpe")
summer_row <- nba_combined_summer %>% filter(Player == "Day'Ron Sharpe")

combined_row <- master_row %>% left_join(summer_row, by = "Player")

nba_full_combined <- bind_rows(nba_full_combined, combined_row)

nba_full_combined <- nba_full_combined %>%
  filter(!(Player == "Day'Ron Sharpe" & is.na(`PTS/G...123`)))
nba_combined_summer <- nba_combined_summer %>%
  mutate(Player = if_else(Player == "DAngelo Russell", "D'Angelo Russell", Player))

master_row <- player_playtype_master_dedup %>% filter(Player == "D'Angelo Russell")
summer_row <- nba_combined_summer %>% filter(Player == "D'Angelo Russell")

combined_row <- master_row %>% left_join(summer_row, by = "Player")

nba_full_combined <- bind_rows(nba_full_combined, combined_row)

nba_full_combined <- nba_full_combined %>%
  filter(!(Player == "D'Angelo Russell" & is.na(`PTS/G...123`)))


nba_full_combined %>% filter(Player == "D'Angelo Russell") %>% select(`PTS/G...123`)

player_playtype_master_dedup %>% filter(Player == "D'Angelo Russell")


nba_combined_summer <- nba_combined_summer %>%
  mutate(Player = if_else(Player == "Royce ONeale", "Royce O'Neale", Player))

master_row <- player_playtype_master_dedup %>% filter(Player == "Royce O'Neale")
summer_row <- nba_combined_summer %>% filter(Player == "Royce O'Neale")

combined_row <- master_row %>% left_join(summer_row, by = "Player")
combined_row$`PTS/G`

nba_full_combined$`PTS/G` <- nba_full_combined$PTS

nba_full_combined <- bind_rows(nba_full_combined, combined_row)
nba_full_combined <- nba_full_combined %>%
  filter(!(Player == "Royce O'Neale" & is.na(`PTS/G...123`)))

nba_full_combined %>% filter(Player == "Royce O'Neale") %>% select(`PTS/G...123`)


setdiff(colnames(combined_row), colnames(nba_full_combined))

player_playtype_master %>% filter(Player == "Nick Richards")


player_playtype_master %>% filter(Player == "Dorian Finney-Smith")
cody_correct <- player_playtype_master %>% filter(Player == "Cody Martin" & Team == "Phoenix Suns")
common_cols_cody <- intersect(colnames(nba_full_combined), colnames(cody_correct))

nba_full_combined <- nba_full_combined %>%
  mutate(across(all_of(common_cols_cody),
                ~ if_else(Player == "Cody Martin", cody_correct[[cur_column()]], .)))

nurkic_correct <- player_playtype_master %>% filter(Player == "Jusuf Nurkic" & Team == "Charlotte Hornets")
common_cols_nurkic <- intersect(colnames(nba_full_combined), colnames(nurkic_correct))

nba_full_combined <- nba_full_combined %>%
  mutate(across(all_of(common_cols_nurkic),
                ~ if_else(Player == "Jusuf Nurkic", nurkic_correct[[cur_column()]], .)))

nick_correct <- player_playtype_master %>% filter(Player == "Nick Richards" & Team == "Charlotte Hornets")
common_cols_nick <- intersect(colnames(nba_full_combined), colnames(nick_correct))

nba_full_combined <- nba_full_combined %>%
  mutate(across(all_of(common_cols_nick),
                ~ if_else(Player == "Nick Richards", nick_correct[[cur_column()]], .)))

okogie_correct <- player_playtype_master %>% filter(Player == "Josh Okogie" & Team == "Charlotte Hornets")
common_cols_okogie <- intersect(colnames(nba_full_combined), colnames(okogie_correct))

nba_full_combined <- nba_full_combined %>%
  mutate(across(all_of(common_cols_okogie),
                ~ if_else(Player == "Josh Okogie", okogie_correct[[cur_column()]], .)))

nba_full_combined <- nba_full_combined %>%
  mutate(Team.x = case_when(
    Player == "Cody Martin" ~ "Phoenix Suns",
    Player == "Jusuf Nurkic" ~ "Charlotte Hornets",
    Player == "Nick Richards" ~ "Charlotte Hornets",
    Player == "Josh Okogie" ~ "Charlotte Hornets",
    TRUE ~ Team.x
  ))


dfs_correct <- player_playtype_master %>% filter(Player == "Dorian Finney-Smith" & Team == "Los Angeles Lakers")

common_cols_dfs <- intersect(colnames(nba_full_combined), colnames(dfs_correct))

nba_full_combined <- nba_full_combined %>%
  mutate(across(all_of(common_cols_dfs),
                ~ if_else(Player == "Dorian Finney-Smith", dfs_correct[[cur_column()]], .)))

nba_full_combined <- nba_full_combined %>%
  mutate(Team.x = case_when(
    Player == "Dorian Finney-Smith" ~ "Los Angeles Lakers",
    TRUE ~ Team.x
  ))


nba_combined_summer %>% filter(Player == "KJ Simpson")
player_playtype_master_dedup %>% filter(Player == "K.J. Simpson")

nba_combined_summer <- nba_combined_summer %>%
  mutate(Player = if_else(Player == "Nick Smith Jr", "Nick Smith Jr.", Player))

master_row <- player_playtype_master_dedup %>% filter(Player == "Nick Smith Jr.")
summer_row <- nba_combined_summer %>% filter(Player == "Nick Smith Jr.")

combined_row <- master_row %>% left_join(summer_row, by = "Player")

nba_full_combined <- bind_rows(nba_full_combined, combined_row)

nba_full_combined <- nba_full_combined %>%
  filter(!(Player == "Nick Smith Jr." & is.na(`PTS/G...123`)))
nba_full_combined %>% filter(Player == "Nick Smith Jr.") %>% select(`PTS/G...123`)

nba_combined_summer <- nba_combined_summer %>%
  mutate(Player = if_else(Player == "KJ Simpson", "K.J. Simpson", Player))

master_row <- player_playtype_master_dedup %>% filter(Player == "K.J. Simpson")
summer_row <- nba_combined_summer %>% filter(Player == "K.J. Simpson")

combined_row <- master_row %>% left_join(summer_row, by = "Player")

nba_full_combined <- bind_rows(nba_full_combined, combined_row)

nba_full_combined <- nba_full_combined %>%
  filter(!(Player == "K.J. Simpson" & is.na(`PTS/G...123`)))

player_playtype_master_dedup %>% filter(Player == "Kelly Oubre Jr.")

nba_combined_summer <- nba_combined_summer %>%
  mutate(Player = if_else(Player == "KJ Simpson", "K.J. Simpson", Player))

master_row <- player_playtype_master_dedup %>% filter(Player == "K.J. Simpson")
summer_row <- nba_combined_summer %>% filter(Player == "K.J. Simpson")

combined_row <- master_row %>% left_join(summer_row, by = "Player")

nba_full_combined <- bind_rows(nba_full_combined, combined_row)

nba_full_combined <- nba_full_combined %>%
  filter(!(Player == "K.J. Simpson" & is.na(`PTS/G...123`)))

nba_combined_summer <- nba_combined_summer %>%
  mutate(Player = if_else(Player == "Kelly Oubre Jr", "Kelly Oubre Jr.", Player))

master_row <- player_playtype_master_dedup %>% filter(Player == "Kelly Oubre Jr.")
summer_row <- nba_combined_summer %>% filter(Player == "Kelly Oubre Jr.")

combined_row <- master_row %>% left_join(summer_row, by = "Player")

nba_full_combined <- bind_rows(nba_full_combined, combined_row)

nba_full_combined <- nba_full_combined %>%
  filter(!(Player == "Kelly Oubre Jr." & is.na(`PTS/G...123`)))

nba_full_combined <- nba_full_combined %>%
  mutate(Team.x = case_when(
    Player == "Desmond Bane" ~ "Orlando Magic",
    Player == "Cole Anthony" ~ "Memphis Grizzlies",
    Player == "Kentavious Caldwell-Pope" ~ "Memphis Grizzlies",
    TRUE ~ Team.x
  ))

player_playtype_master_dedup %>% filter(Player == "Brandon Ingram")
ingram_correct <- player_playtype_master %>% filter(Player == "Brandon Ingram" & Team == "Toronto Raptors")
common_cols_ingram <- intersect(colnames(nba_full_combined), colnames(ingram_correct))
nba_full_combined <- nba_full_combined %>%
  mutate(across(all_of(common_cols_ingram),
                ~ if_else(Player == "Brandon Ingram", ingram_correct[[cur_column()]], .))) %>%
 
  
   nba_full_combined <- nba_full_combined %>% mutate(Team.x = case_when(Player == "Brandon Ingram" ~ "Toronto Raptors",
    TRUE ~ Team.x))

nba_full_combined %>% filter(Player == "Brandon Ingram")

castleton_correct <- player_playtype_master %>% filter(Player == "Colin Castleton" & Team == "Toronto Raptors")
common_cols_castleton <- intersect(colnames(nba_full_combined), colnames(castleton_correct))
nba_full_combined <- nba_full_combined %>%
  mutate(across(all_of(common_cols_castleton),
                ~ if_else(Player == "Colin Castleton", castleton_correct[[cur_column()]], .))) %>%
  mutate(Team.x = case_when(
    Player == "Colin Castleton" ~ "Toronto Raptors",
    TRUE ~ Team.x
  ))

nba_full_combined <- nba_full_combined %>%
  mutate(Team.x = case_when(
    Player == "Brandon Ingram" ~ "Toronto Raptors",
    TRUE ~ Team.x
  ))


rhoden_correct <- player_playtype_master %>% filter(Player == "Jared Rhoden" & Team == "Toronto Raptors")
common_cols_rhoden <- intersect(colnames(nba_full_combined), colnames(rhoden_correct))
nba_full_combined <- nba_full_combined %>%
  mutate(across(all_of(common_cols_rhoden),
                ~ if_else(Player == "Jared Rhoden", rhoden_correct[[cur_column()]], .))) %>%
  mutate(Team.x = case_when(
    Player == "Jared Rhoden" ~ "Toronto Raptors",
    TRUE ~ Team.x
  ))

olynyk_correct <- player_playtype_master %>% filter(Player == "Kelly Olynyk" & Team == "New Orleans Pelicans")
common_cols_olynyk <- intersect(colnames(nba_full_combined), colnames(olynyk_correct))
nba_full_combined <- nba_full_combined %>%
  mutate(across(all_of(common_cols_olynyk),
                ~ if_else(Player == "Kelly Olynyk", olynyk_correct[[cur_column()]], .))) %>%
  mutate(Team.x = case_when(
    Player == "Kelly Olynyk" ~ "Washington Wizards",
    TRUE ~ Team.x
  ))

nba_combined_summer <- nba_combined_summer %>%
  mutate(Player = if_else(Player == "JaKobe Walter", "Ja'Kobe Walter", Player))

master_row <- player_playtype_master_dedup %>% filter(Player == "Ja'Kobe Walter")
summer_row <- nba_combined_summer %>% filter(Player == "Ja'Kobe Walter")

combined_row <- master_row %>% left_join(summer_row, by = "Player")

nba_full_combined <- bind_rows(nba_full_combined, combined_row) %>%
  filter(!(Player == "Ja'Kobe Walter" & is.na(`PTS/G...123`)))


nba_combined_summer <- nba_combined_summer %>%
  mutate(Player = if_else(Player == "GG Jackson", "G.G. Jackson", Player))

master_row <- player_playtype_master_dedup %>% filter(Player == "G.G. Jackson")
summer_row <- nba_combined_summer %>% filter(Player == "G.G. Jackson")

combined_row <- master_row %>% left_join(summer_row, by = "Player")

nba_full_combined <- bind_rows(nba_full_combined, combined_row) %>%
  filter(!(Player == "G.G. Jackson" & is.na(`PTS/G...123`)))
player_playtype_master %>% filter(Player == "A.J. Lawson")

nba_combined_summer <- nba_combined_summer %>%
  mutate(Player = if_else(Player == "AJ Lawson", "A.J. Lawson", Player))

master_row <- player_playtype_master_dedup %>% filter(Player == "A.J. Lawson")
summer_row <- nba_combined_summer %>% filter(Player == "A.J. Lawson")

combined_row <- master_row %>% left_join(summer_row, by = "Player")

nba_full_combined <- bind_rows(nba_full_combined, combined_row) %>%
  filter(!(Player == "A.J. Lawson" & is.na(`PTS/G...123`)))

nba_combined_summer <- nba_combined_summer %>%
  mutate(Player = if_else(Player == "Derrick Jones Jr", "Derrick Jones Jr.", Player))

master_row <- player_playtype_master_dedup %>% filter(Player == "Derrick Jones Jr.")
summer_row <- nba_combined_summer %>% filter(Player == "Derrick Jones Jr.")
combined_row <- master_row %>% left_join(summer_row, by = "Player")
nba_full_combined <- bind_rows(nba_full_combined, combined_row) %>%
  filter(!(Player == "Derrick Jones Jr." & is.na(`PTS/G...123`)))

nba_combined_summer <- nba_combined_summer %>%
  mutate(Player = if_else(Player == "DeAndre Hunter", "De'Andre Hunter", Player))

master_row <- player_playtype_master_dedup %>% filter(Player == "De'Andre Hunter")
summer_row <- nba_combined_summer %>% filter(Player == "De'Andre Hunter")
combined_row <- master_row %>% left_join(summer_row, by = "Player")
nba_full_combined <- bind_rows(nba_full_combined, combined_row) %>%
  filter(!(Player == "De'Andre Hunter" & is.na(`PTS/G...123`)))

nba_combined_summer <- nba_combined_summer %>%
  mutate(Player = if_else(Player == "Brandon Boston Jr", "Brandon Boston Jr.", Player))

master_row <- player_playtype_master_dedup %>% filter(Player == "Brandon Boston Jr.")
summer_row <- nba_combined_summer %>% filter(Player == "Brandon Boston Jr.")
combined_row <- master_row %>% left_join(summer_row, by = "Player")
nba_full_combined <- bind_rows(nba_full_combined, combined_row) %>%
  filter(!(Player == "Brandon Boston Jr." & is.na(`PTS/G...123`)))

nba_combined_summer <- nba_combined_summer %>%
  mutate(Player = if_else(Player == "Terrence Shannon Jr", "Terrence Shannon Jr.", Player))

master_row <- player_playtype_master_dedup %>% filter(Player == "Terrence Shannon Jr.")
summer_row <- nba_combined_summer %>% filter(Player == "Terrence Shannon Jr.")
combined_row <- master_row %>% left_join(summer_row, by = "Player")
nba_full_combined <- bind_rows(nba_full_combined, combined_row) %>%
  filter(!(Player == "Terrence Shannon Jr." & is.na(`PTS/G...123`)))

nba_combined_summer <- nba_combined_summer %>%
  mutate(Player = if_else(Player == "Andre Jackson Jr", "Andre Jackson Jr.", Player))

master_row <- player_playtype_master_dedup %>% filter(Player == "Andre Jackson Jr.")
summer_row <- nba_combined_summer %>% filter(Player == "Andre Jackson Jr.")
combined_row <- master_row %>% left_join(summer_row, by = "Player")
nba_full_combined <- bind_rows(nba_full_combined, combined_row) %>%
  filter(!(Player == "Andre Jackson Jr." & is.na(`PTS/G...123`)))

nba_combined_summer <- nba_combined_summer %>%
  mutate(Player = if_else(Player == "Garry Trent Jr", "Garry Trent Jr.", Player))

master_row <- player_playtype_master_dedup %>% filter(Player == "Garry Trent Jr.")
summer_row <- nba_combined_summer %>% filter(Player == "Garry Trent Jr.")

combined_row <- master_row %>% left_join(summer_row, by = "Player")

nba_full_combined <- bind_rows(nba_full_combined, combined_row) %>%
  filter(!(Player == "Garry Trent Jr." & is.na(`PTS/G...123`)))




nba_full_combined <- nba_full_combined %>%
  mutate(Team.x = case_when(
    Player == "Jordan Poole" ~ "New Orleans Pelicans",
    Player == "CJ McCollum" ~ "Washington Wizards",
    Player == "Isaac Okoro" ~ "Chicago Bulls",
    Player == "Lonzo Ball" ~ "Cleveland Cavaliers",
    Player == "MarJon Beauchamp" ~ "New York Knicks",
    TRUE ~ Team.x
  ))

nba_combined_summer <- nba_combined_summer %>%
  mutate(Player = if_else(Player == "Kevin Porter Jr", "Kevin Porter Jr.", Player))
kpj_correct <- player_playtype_master %>%
  filter(Player == "Kevin Porter Jr." & Team == "Milwaukee Bucks")

common_cols_kpj <- intersect(colnames(nba_full_combined), colnames(kpj_correct))

nba_full_combined <- nba_full_combined %>%
  mutate(across(all_of(common_cols_kpj),
                ~ if_else(Player == "Kevin Porter Jr.", kpj_correct[[cur_column()]], .))) %>%
  mutate(Team.x = case_when(
    Player == "Kevin Porter Jr." ~ "Milwaukee Bucks",
    TRUE ~ Team.x
  ))



nba_full_combined %>% filter(Player == "Kevin Porter Jr.")


kai_correct <- player_playtype_master %>% filter(Player == "Kai Jones" & Team == "Dallas Mavericks")
common_cols_kai <- intersect(colnames(nba_full_combined), colnames(kai_correct))
nba_full_combined <- nba_full_combined %>%
  mutate(across(all_of(common_cols_kai),
                ~ if_else(Player == "Kai Jones", kai_correct[[cur_column()]], .))) %>%
  mutate(Team.x = case_when(
    Player == "Kai Jones" ~ "Dallas Mavericks",
    TRUE ~ Team.x
  ))





hunter_correct <- player_playtype_master %>% filter(Player == "De'Andre Hunter" & Team == "Cleveland Cavaliers")
common_cols_hunter <- intersect(colnames(nba_full_combined), colnames(hunter_correct))

nba_full_combined <- nba_full_combined %>%
  mutate(across(all_of(common_cols_hunter),
                ~ if_else(Player == "De'Andre Hunter", hunter_correct[[cur_column()]], .))) %>%
  mutate(Team.x = case_when(
    Player == "De'Andre Hunter" ~ "Cleveland Cavaliers",
    TRUE ~ Team.x
  ))





nba_combined_summer %>% filter(Player == "Robert Williams")
player_playtype_master %>% filter(Player == "Alex Sarr")

player_playtype_master_dedup <- player_playtype_master_dedup %>%
  mutate(Player = if_else(Player == "Carlton Carrington", "Bub Carrington", Player))

master_row <- player_playtype_master_dedup %>% filter(Player == "Bub Carrington")
summer_row <- nba_combined_summer %>% filter(Player == "Bub Carrington")
combined_row <- master_row %>% left_join(summer_row, by = "Player")
nba_full_combined <- bind_rows(nba_full_combined, combined_row) %>%
  filter(!(Player == "Bub Carrington" & is.na(`PTS/G...123`)))


player_playtype_master_dedup <- player_playtype_master_dedup %>%
  mutate(Player = if_else(Player == "Alexandre Sarr", "Alex Sarr", Player))

master_row <- player_playtype_master_dedup %>% filter(Player == "Alex Sarr")
summer_row <- nba_combined_summer %>% filter(Player == "Alex Sarr")
combined_row <- master_row %>% left_join(summer_row, by = "Player")
nba_full_combined <- bind_rows(nba_full_combined, combined_row) %>%
  filter(!(Player == "Alex Sarr" & is.na(`PTS/G...123`)))


player_playtype_master_dedup <- player_playtype_master_dedup %>%
  mutate(Player = if_else(Player == "Robert Williams III", "Robert Williams", Player))

master_row <- player_playtype_master_dedup %>% filter(Player == "Robert Williams")
summer_row <- nba_combined_summer %>% filter(Player == "Robert Williams")
combined_row <- master_row %>% left_join(summer_row, by = "Player")
nba_full_combined <- bind_rows(nba_full_combined, combined_row) %>%
  filter(!(Player == "Robert Williams" & is.na(`PTS/G...123`)))


player_playtype_master %>% filter(Player == "Marcus Smart")



missing_height_weight <- final_draft_synergy_percentiled %>%
  filter(is.na(player_height) | is.na(player_weight)) %>%
  select(Player, player_height, player_weight)



print(missing_height_weight, n =23)

six29backupfinalsynergy <- readRDS("~/Post-Draft App/data/final_draft_synergy_percentiled.rds")

impute_values <- tibble::tibble(
  Player = c("Taelon Peter", "Jahmai Mashack", "Alijah Martin", 
             "Yanic Konan Niederhauser", "Brooks Barnhizer", "Kobe Sanders"),
  player_height = c(193.0, 193.0, 188.0, 211.0, 198.0, 206.0),
  player_weight = c(95.3, 91.6, 95.3, 110.2, 104.3, 93.9)
)

six29backupfinalsynergy_back <- six29backupfinalsynergy %>%
  left_join(impute_values, by = "Player", suffix = c("", "_imputed")) %>%
  mutate(
    player_height = ifelse(is.na(player_height), player_height_imputed, player_height),
    player_weight = ifelse(is.na(player_weight), player_weight_imputed, player_weight)
  ) %>%
  select(-player_height_imputed, -player_weight_imputed)


nba_full_combined %>% filter(Player == "Kevin Porter Jr.")


six29backupfinalsynergy_back %>%
  filter(Player == "Cooper Flagg") %>%
  select(Player, contains("post"), contains("transition")) %>%
  print(width = Inf)




six29backupfinalsynergy_back %>%
  select(
    Player,
    `%Time_post`, `PPP_post`, `FG%_post`, `3 FG%_post`, `%SF_post`, `%FT_post`, `TO%_post`,
    `%Time_post_pctile`, `PPP_post_pctile`, `FG%_post_pctile`, `%SF_post_pctile`, `%FT_post_pctile`, `TO%_post_pctile`,
    `%Time_transition`, `PPP_transition`, `FG%_transition`, `3 FG%_transition`, `%SF_transition`, `%FT_transition`, `TO%_transition`,
    `%Time_transition_pctile`, `PPP_transition_pctile`, `FG%_transition_pctile`, `3 FG%_transition_pctile`,
    `%SF_transition_pctile`, `%FT_transition_pctile`, `TO%_transition_pctile`
  ) %>%
  arrange(desc(`%Time_post`)) %>%
  print(n=Inf, width = Inf)



saveRDS(six29backupfinalsynergy_back, "final_draft_synergy_percentiled.rds")
#nba629back <- readRDS("~/Post-Draft App/data/nba_full_combined_percentiled.rds")
#nba_full_combined629 <- nba_full_combined





#full_synergy_back630 <- readRDS("~/Post-Draft App/data/nba_full_combined_percentiled.rds")

six29backupfinalsynergy_back <- six29backupfinalsynergy_back %>%
  mutate(
    `%Time_post` = if_else(Player == "Mohamed Diawara", 0.1654, `%Time_post`),
    `PPP_post` = if_else(Player == "Mohamed Diawara", 0.600, `PPP_post`),
    `FG%_post` = if_else(Player == "Mohamed Diawara", 0.345, `FG%_post`),
    `3 FG%_post` = if_else(Player == "Mohamed Diawara", 0, `3 FG%_post`),
    `%SF_post` = if_else(Player == "Mohamed Diawara", 0.2, `%SF_post`),
    `%FT_post` = if_else(Player == "Mohamed Diawara", 0.22, `%FT_post`),
    `TO%_post` = if_else(Player == "Mohamed Diawara", 0.28, `TO%_post`),
    `%Time_post_pctile` = if_else(Player == "Mohamed Diawara", 70, `%Time_post_pctile`),
    `PPP_post_pctile` = if_else(Player == "Mohamed Diawara", 9, `PPP_post_pctile`),
    `FG%_post_pctile` = if_else(Player == "Mohamed Diawara", 11, `FG%_post_pctile`),
    `%SF_post_pctile` = if_else(Player == "Mohamed Diawara", 83, `%SF_post_pctile`),
    `%FT_post_pctile` = if_else(Player == "Mohamed Diawara", 45, `%FT_post_pctile`),
    `TO%_post_pctile` = if_else(Player == "Mohamed Diawara", 4, `TO%_post_pctile`),
    
    `%Time_transition` = if_else(Player == "Mohamed Diawara", 0.162, `%Time_transition`),
    `PPP_transition` = if_else(Player == "Mohamed Diawara", 1.151, `PPP_transition`),
    `FG%_transition` = if_else(Player == "Mohamed Diawara", 0.659, `FG%_transition`),
    `3 FG%_transition` = if_else(Player == "Mohamed Diawara", 0.333, `3 FG%_transition`),
    `%SF_transition` = if_else(Player == "Mohamed Diawara", 0.094, `%SF_transition`),
    `%FT_transition` = if_else(Player == "Mohamed Diawara", 0.113, `%FT_transition`),
    `TO%_transition` = if_else(Player == "Mohamed Diawara", 0.132, `TO%_transition`),
    `%Time_transition_pctile` = if_else(Player == "Mohamed Diawara", 63, `%Time_transition_pctile`),
    `PPP_transition_pctile` = if_else(Player == "Mohamed Diawara", 49, `PPP_transition_pctile`),
    `FG%_transition_pctile` = if_else(Player == "Mohamed Diawara", 82, `FG%_transition_pctile`),
    `3 FG%_transition_pctile` = if_else(Player == "Mohamed Diawara", 40, `3 FG%_transition_pctile`),
    `%SF_transition_pctile` = if_else(Player == "Mohamed Diawara", 20, `%SF_transition_pctile`),
    `%FT_transition_pctile` = if_else(Player == "Mohamed Diawara", 24, `%FT_transition_pctile`),
    `TO%_transition_pctile` = if_else(Player == "Mohamed Diawara", 42, `TO%_transition_pctile`)
  )



nba_full_combined <- nba_full_combined %>%
  mutate(Team.x = case_when(
    Player == "Jalen Green" ~ "Phoenix Suns",
    Player == "Dillon Brooks" ~ "Phoenix Suns",
    Player == "Jusuf Nurkic" ~ "Utah Jazz",
    Player == "Collin Sexton" ~ "Charlotte Hornets",
    Player == "Cam Reddish" ~ "Houston Rockets",
    Player == "Mark Williams" ~ "Phoenix Suns",
    Player == "Kevin Durant" ~ "Houston Rockets",
    TRUE ~ Team.x
  ))


nba_combined_summer %>% filter(Player == "De'Andre Hunter")


player_playtype_master %>% filter(Player == "De'Andre Hunter")

porter_correct <- player_playtype_master %>% filter(Player == "Kevin Porter Jr." & Team == "Milwaukee Bucks")
common_cols_porter <- intersect(colnames(nba_full_combined), colnames(porter_correct))

nba_full_combined <- nba_full_combined %>%
  mutate(across(all_of(common_cols_porter),
                ~ if_else(Player == "Kevin Porter Jr.", porter_correct[[cur_column()]], .))) %>%
  mutate(Team.x = case_when(
    Player == "Kevin Porter Jr." ~ "Milwaukee Bucks",
    TRUE ~ Team.x
  ))
player_playtype_master_dedup %>% filter(Player == "Kelly Oubre Jr.")
nba_combined_summer %>% filter(Player == "Kelly Oubre Jr.") %>% select(`PTS/G...3`)
player_playtype_master_dedup <- player_playtype_master_dedup %>%
  mutate(Player = if_else(Player == "Robert Williams III", "Robert Williams", Player))

master_row <- player_playtype_master_dedup %>% filter(Player == "Robert Williams")
summer_row <- nba_combined_summer %>% filter(Player == "Robert Williams")
combined_row <- master_row %>% left_join(summer_row, by = "Player")
nba_full_combined <- bind_rows(nba_full_combined, combined_row) %>%
  filter(!(Player == "Robert Williams" & is.na(`PTS/G...123`)))




nba_draft_percentiled_back16 <- readRDS("~/Post-Draft App/data/nba_full_combined_percentiled.rds")

nba_full_combined_percentiled2 <- readRDS("~/Post-Draft App/data/nba_full_combined_percentiled.rds")


nba_full_combined <- nba_full_combined %>%
  mutate(Team.x = case_when(
    Player == "Jalen Green" ~ "Phoenix Suns",
    Player == "Dillon Brooks" ~ "Phoenix Suns",
    Player == "Jusuf Nurkic" ~ "Utah Jazz",
    Player == "Collin Sexton" ~ "Charlotte Hornets",
    Player == "Cam Reddish" ~ "Houston Rockets",
    Player == "Mark Williams" ~ "Phoenix Suns",
    Player == "Kevin Durant" ~ "Houston Rockets",
    TRUE ~ Team.x
  ))



nba_full_combined_percentiled2 <- nba_full_combined_percentiled2 %>%
  mutate(Team.x = case_when(
    Player == "Trey Alexander" ~ "New Orleans Pelicans",
    Player == "Nickeil Alexander-Walker" ~ "Atlanta Hawks",
    Player == "Deandre Ayton" ~ "Los Angeles Lakers",
    Player == "Marvin Bagley III" ~ "Washington Wizards",
    Player == "Dominick Barlow" ~ "Philadelphia 76ers",
    
    Player == "Bruce Brown" ~ "Denver Nuggets",
    Player == "Clint Capela" ~ "Houston Rockets",
    Player == "Jordan Clarkson" ~ "New York Knicks",
    Player == "John Collins" ~ "Los Angeles Clippers",
    Player == "Pat Connaughton" ~ "Charlotte Hornets",
    Player == "Gary Harris" ~ "Milwaukee Bucks",
    Player == "Jake Laravia" ~ "Los Angeles Lakers",
    Player == "Caris LeVert" ~ "Detroit Pistons",
    Player == "Ty Jerome" ~ "Memphis Grizzlies",
    Player == "Drew Eubanks" ~ "Sacramento Kings",
    Player == "Dante Exum" ~ "Dallas Mavericks",
    Player == "Dorian Finney-Smith" ~ "Houston Rockets",
    Player == "Damian Lillard" ~ NA_character_, 
    Player == "Kevon Looney" ~ "New Orleans Pelicans",
    Player == "Brook Lopez" ~ "Los Angeles Clippers",
    Player == "Sandro Mamukelashvili" ~ "Toronto Raptors",
    Player == "Tre Mann" ~ "Charlotte Hornets",
    Player == "Trendon Watford" ~ "Philadelphia 76ers",
    Player == "Tyus Jones" ~ "Orlando Magic",
    Player == "Luke Kornet" ~ "San Antonio Spurs",
    Player == "Dennis Schroder" ~ "Sacramento Kings",
    Player == "Duncan Robinson" ~ "Detroit Pistons",
    Player == "Myles Turner" ~ "Milwaukee Bucks",
    Player == "Guerschon Yabusele" ~ "New York Knicks",
    TRUE ~ Team.x
  ))

saveRDS(nba_full_combined_percentiled2,"nba_full_combined_percentiled.rds")
