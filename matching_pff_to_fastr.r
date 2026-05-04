## 2024 Player ID Matching

war21 <- read_csv("war_2021.csv")
war22 <- read_csv("war_2022.csv")
war23 <- read_csv("war_2023.csv")
war24 <- read_csv("war_2024.csv")
war25 <- read_csv("war_2025.csv")


fastr_roster <- cfbfastR::load_cfb_rosters(seasons = 2024)
# pff_roster <- read_csv("roster_feed.csv") |> 
#   select(gsis_player_id, team, team_id:jersey_number) |> 
#   mutate(position = case_when(
#     str_detect(position, "WR") ~ "WR",
#     str_detect(position, "LB") ~ "LB",
#     str_detect(position, "CB") ~ "CB",
#     str_detect(position, "TE") ~ "TE",
#     str_detect(position, "RE|LE") ~ "DE",
#     str_detect(position, "G") ~ "G",
#     str_detect(position, "D.T|NT") ~ "DT",
#     str_detect(position, "RT|LT") ~ "OT",
#     str_detect(position, "SS|FS") ~ "S",
#     TRUE ~ position
#   )) |> 
#   unique()

pff_roster <- read_csv("roster_feed_2024.csv") |> 
  select(gsis_player_id, team, team_id:jersey_number) |> 
  select(-position) |> 
  unique()

offense_positions <- c("WR", "OL", "TE", "RB", "QB", "FB", "G", "C", "OT")
defense_positions <- c("DL", "DB", "CB", "DT", "LB", "S", "NT", "DE", "EDGE")
special_teams_positions <- c("LS", "KR", "PK", "P")

fastr_roster <- fastr_roster |> 
  mutate(pff_jersey = ifelse(jersey < 10, paste0(0, jersey), jersey)) |> 
  mutate(jersey_number = case_when(
    position %in% offense_positions ~ pff_jersey,
    position %in% defense_positions ~ paste0("D", pff_jersey),
    position %in% special_teams_positions ~ paste0("S", pff_jersey)
  ))

pff_team_codes <- read_csv("pff_team_codes.csv") |> clean_names()
fastr_teaminfo <- cfbfastR::cfbd_team_info(only_fbs = FALSE) |> 
  filter(classification %in% c("fbs", "fcs")) |> 
  mutate(match_school = case_when(
    school == "UAlbany" ~ "Albany", 
    school == "Hawai'i" ~ "Hawaii",
    school == "App State" ~ "Appalachian State",
    school == "Bryant" ~ "Bryant University",
    school == "Central Connecticut" ~ "Central Connecticut State",
    school == "UConn" ~ "Connecticut",
    school == "Delaware" ~ "Delaware Fightin",
    school == "Grambling" ~ "Grambling State",
    school == "Long Island University" ~ "LIU",
    school == "UL Monroe" ~ "Louisiana-Monroe",
    school == "McNeese" ~ "McNeese State",
    school == "Miami" ~ "Miami (FL)",
    school == "Ole Miss" ~ "Mississippi",
    school == "NC State" ~ "North Carolina State",
    school == "Sam Houston" ~ "Sam Houston State",
    school == "San José State" ~ "San Jose State",
    school == "SE Louisiana" ~ "Southeastern Louisiana",
    school == "Southern" ~ "Southern University",
    school == "St. Thomas (MN)" ~ "St. Thomas",
    school == "UT Martin" ~ "Tennessee-Martin",
    school == "East Texas A&M" ~ "Texas A&M - Commerce",
    school == "South Florida" ~ "USF",
    school == "VMI" ~ "Virginia Military Institute",
    TRUE ~ school),
    mascot = ifelse(mascot == "Rainbow Warriors", "Warriors", mascot),
    mascot = ifelse(school == "Florida International", "Golden Panthers", mascot)) |> 
  mutate(ultimate_name = paste(match_school, mascot)) |> 
  select(school, ultimate_name)

all_team_codes <- pff_team_codes |> 
  inner_join(fastr_teaminfo) |> 
  rename(fastr_name = school)

fastr_roster <- fastr_roster |> 
  inner_join(all_team_codes |> select(fastr_name, team_code), by = join_by(team == fastr_name)) |> 
  select(athlete_id, contains("name"), jersey, team, position, team_code, jersey_number) |> 
  mutate(pff_identifier = paste(team_code, jersey_number))

pff_roster <- pff_roster |> 
  mutate(pff_identifier = paste(team, jersey_number)) |> 
  select(-team_id, -jersey_number)

duplicates <- fastr_roster |> 
  group_by(pff_identifier) |> 
  summarise(count = n()) |> 
  filter(count > 1) |> 
  inner_join(fastr_roster |> select(athlete_id:last_name, jersey, pff_identifier)) |> 
  filter(!is.na(jersey)) |> 
  inner_join(pff_roster) |> 
  select(-count) |> 
  inner_join(all_team_codes |> select(team_code, fastr_name), by = join_by(team == team_code))

correct_duplicates <- duplicates |> 
  mutate(full_name = paste(first_name, last_name)) |> 
  filter(player_name == full_name) |> 
  select(-full_name)

incorrect_duplicates <- duplicates |> 
  anti_join(correct_duplicates)


### THIS PART BELOW IS UNFINISHED

no_duplicates <- pff_roster |> 
  filter(!(pff_identifier %in% duplicates$pff_identifier)) |> 
  inner_join(all_team_codes |> select(team_code, fastr_name), by = join_by(team == team_code)) |> 
  inner_join(fastr_roster |> 
               filter(!is.na(jersey_number)) |> 
               filter(!(pff_identifier %in% duplicates$pff_identifier)) |> 
               select(athlete_id:last_name, jersey, pff_identifier))

pff_duplicates <- no_duplicates |> 
  group_by(pff_identifier) |> 
  summarise(count = n()) |> 
  filter(count > 1)

no_duplicates <- no_duplicates |> 
  filter((!(pff_identifier) %in% pff_duplicates$pff_identifier) | player_name == paste(first_name, last_name))

# no_duplicates |> 
#   group_by(pff_identifier) |> 
#   summarise(count = n()) |> 
#   filter(count > 1)

full_roster <- bind_rows(no_duplicates, correct_duplicates) |> 
  filter(player_name == paste(first_name, last_name)) |> 
  distinct()

write_csv(full_roster, "names_and_ids_2024.csv")
write_csv(all_team_codes, "team_codes.csv")

war_roster24 <- fastr_roster |> 
  mutate(player = paste(first_name, last_name)) |> 
  select(-team, -position) |> 
  inner_join(war24 |> inner_join(pff_team_codes, by = join_by(team == ultimate_name, franchise_id)),
             by = join_by(player, team_code))

write_csv(war_roster24, "war_and_roster2024.csv")


# 2025 Player ID Matching -------------------------------------------------

fastr_roster <- cfbfastR::load_cfb_rosters(seasons = 2025)
# pff_roster <- read_csv("roster_feed.csv") |> 
#   select(gsis_player_id, team, team_id:jersey_number) |> 
#   mutate(position = case_when(
#     str_detect(position, "WR") ~ "WR",
#     str_detect(position, "LB") ~ "LB",
#     str_detect(position, "CB") ~ "CB",
#     str_detect(position, "TE") ~ "TE",
#     str_detect(position, "RE|LE") ~ "DE",
#     str_detect(position, "G") ~ "G",
#     str_detect(position, "D.T|NT") ~ "DT",
#     str_detect(position, "RT|LT") ~ "OT",
#     str_detect(position, "SS|FS") ~ "S",
#     TRUE ~ position
#   )) |> 
#   unique()

pff_roster <- read_csv("roster_feed_2025.csv") |> 
  select(gsis_player_id, team, team_id:jersey_number) |> 
  select(-position) |> 
  unique()

offense_positions <- c("WR", "OL", "TE", "RB", "QB", "FB", "G", "C", "OT")
defense_positions <- c("DL", "DB", "CB", "DT", "LB", "S", "NT", "DE", "EDGE")
special_teams_positions <- c("LS", "KR", "PK", "P")

fastr_roster <- fastr_roster |> 
  mutate(pff_jersey = ifelse(jersey < 10, paste0(0, jersey), jersey)) |> 
  mutate(jersey_number = case_when(
    position %in% offense_positions ~ pff_jersey,
    position %in% defense_positions ~ paste0("D", pff_jersey),
    position %in% special_teams_positions ~ paste0("S", pff_jersey)
  ))

pff_team_codes <- read_csv("pff_team_codes.csv") |> clean_names()
fastr_teaminfo <- cfbfastR::cfbd_team_info(only_fbs = FALSE) |> 
  filter(classification %in% c("fbs", "fcs")) |> 
  mutate(match_school = case_when(
    school == "UAlbany" ~ "Albany", 
    school == "Hawai'i" ~ "Hawaii",
    school == "App State" ~ "Appalachian State",
    school == "Bryant" ~ "Bryant University",
    school == "Central Connecticut" ~ "Central Connecticut State",
    school == "UConn" ~ "Connecticut",
    school == "Delaware" ~ "Delaware Fightin",
    school == "Grambling" ~ "Grambling State",
    school == "Long Island University" ~ "LIU",
    school == "UL Monroe" ~ "Louisiana-Monroe",
    school == "McNeese" ~ "McNeese State",
    school == "Miami" ~ "Miami (FL)",
    school == "Ole Miss" ~ "Mississippi",
    school == "NC State" ~ "North Carolina State",
    school == "Sam Houston" ~ "Sam Houston State",
    school == "San José State" ~ "San Jose State",
    school == "SE Louisiana" ~ "Southeastern Louisiana",
    school == "Southern" ~ "Southern University",
    school == "St. Thomas (MN)" ~ "St. Thomas",
    school == "UT Martin" ~ "Tennessee-Martin",
    school == "East Texas A&M" ~ "Texas A&M - Commerce",
    school == "South Florida" ~ "USF",
    school == "VMI" ~ "Virginia Military Institute",
    TRUE ~ school),
    mascot = ifelse(mascot == "Rainbow Warriors", "Warriors", mascot),
    mascot = ifelse(school == "Florida International", "Golden Panthers", mascot)) |> 
  mutate(ultimate_name = paste(match_school, mascot)) |> 
  select(school, ultimate_name)

all_team_codes <- pff_team_codes |> 
  inner_join(fastr_teaminfo) |> 
  rename(fastr_name = school)

fastr_roster <- fastr_roster |> 
  inner_join(all_team_codes |> select(fastr_name, team_code), by = join_by(team == fastr_name)) |> 
  select(athlete_id, contains("name"), jersey, team, position, team_code, jersey_number) |> 
  mutate(pff_identifier = paste(team_code, jersey_number))

pff_roster <- pff_roster |> 
  mutate(pff_identifier = paste(team, jersey_number)) |> 
  select(-team_id, -jersey_number)

duplicates <- fastr_roster |> 
  group_by(pff_identifier) |> 
  summarise(count = n()) |> 
  filter(count > 1) |> 
  inner_join(fastr_roster |> select(athlete_id:last_name, jersey, pff_identifier)) |> 
  filter(!is.na(jersey)) |> 
  inner_join(pff_roster) |> 
  select(-count) |> 
  inner_join(all_team_codes |> select(team_code, fastr_name), by = join_by(team == team_code))

correct_duplicates <- duplicates |> 
  mutate(full_name = paste(first_name, last_name)) |> 
  filter(player_name == full_name) |> 
  select(-full_name)

incorrect_duplicates <- duplicates |> 
  anti_join(correct_duplicates)


### THIS PART BELOW IS UNFINISHED

no_duplicates <- pff_roster |> 
  filter(!(pff_identifier %in% duplicates$pff_identifier)) |> 
  inner_join(all_team_codes |> select(team_code, fastr_name), by = join_by(team == team_code)) |> 
  inner_join(fastr_roster |> 
               filter(!is.na(jersey_number)) |> 
               filter(!(pff_identifier %in% duplicates$pff_identifier)) |> 
               select(athlete_id:last_name, jersey, pff_identifier))

pff_duplicates <- no_duplicates |> 
  group_by(pff_identifier) |> 
  summarise(count = n()) |> 
  filter(count > 1)

no_duplicates <- no_duplicates |> 
  filter((!(pff_identifier) %in% pff_duplicates$pff_identifier) | player_name == paste(first_name, last_name))

# no_duplicates |>
#   group_by(pff_identifier) |>
#   summarise(count = n()) |>
#   filter(count > 1)

full_roster <- bind_rows(no_duplicates, correct_duplicates) |> 
  filter(player_name == paste(first_name, last_name)) |> 
  distinct()

write_csv(full_roster, "names_and_ids_2025.csv")

war_roster25 <- fastr_roster |> 
  mutate(player = paste(first_name, last_name)) |> 
  select(-team, -position) |> 
  inner_join(war25 |> inner_join(pff_team_codes, by = join_by(team == ultimate_name, franchise_id)),
             by = join_by(player, team_code))

write_csv(war_roster25, "war_and_roster2025.csv")


# 2023 Player ID Matching -------------------------------------------------

fastr_roster <- cfbfastR::load_cfb_rosters(seasons = 2023)
# pff_roster <- read_csv("roster_feed.csv") |> 
#   select(gsis_player_id, team, team_id:jersey_number) |> 
#   mutate(position = case_when(
#     str_detect(position, "WR") ~ "WR",
#     str_detect(position, "LB") ~ "LB",
#     str_detect(position, "CB") ~ "CB",
#     str_detect(position, "TE") ~ "TE",
#     str_detect(position, "RE|LE") ~ "DE",
#     str_detect(position, "G") ~ "G",
#     str_detect(position, "D.T|NT") ~ "DT",
#     str_detect(position, "RT|LT") ~ "OT",
#     str_detect(position, "SS|FS") ~ "S",
#     TRUE ~ position
#   )) |> 
#   unique()

pff_roster <- read_csv("roster_feed_2023.csv") |> 
  select(gsis_player_id, team, team_id:jersey_number) |> 
  select(-position) |> 
  unique()

offense_positions <- c("WR", "OL", "TE", "RB", "QB", "FB", "G", "C", "OT")
defense_positions <- c("DL", "DB", "CB", "DT", "LB", "S", "NT", "DE", "EDGE")
special_teams_positions <- c("LS", "KR", "PK", "P")

fastr_roster <- fastr_roster |> 
  mutate(pff_jersey = ifelse(jersey < 10, paste0(0, jersey), jersey)) |> 
  mutate(jersey_number = case_when(
    position %in% offense_positions ~ pff_jersey,
    position %in% defense_positions ~ paste0("D", pff_jersey),
    position %in% special_teams_positions ~ paste0("S", pff_jersey)
  ))

pff_team_codes <- read_csv("pff_team_codes.csv") |> clean_names()
fastr_teaminfo <- cfbfastR::cfbd_team_info(only_fbs = FALSE) |> 
  filter(classification %in% c("fbs", "fcs")) |> 
  mutate(match_school = case_when(
    school == "UAlbany" ~ "Albany", 
    school == "Hawai'i" ~ "Hawaii",
    school == "App State" ~ "Appalachian State",
    school == "Bryant" ~ "Bryant University",
    school == "Central Connecticut" ~ "Central Connecticut State",
    school == "UConn" ~ "Connecticut",
    school == "Delaware" ~ "Delaware Fightin",
    school == "Grambling" ~ "Grambling State",
    school == "Long Island University" ~ "LIU",
    school == "UL Monroe" ~ "Louisiana-Monroe",
    school == "McNeese" ~ "McNeese State",
    school == "Miami" ~ "Miami (FL)",
    school == "Ole Miss" ~ "Mississippi",
    school == "NC State" ~ "North Carolina State",
    school == "Sam Houston" ~ "Sam Houston State",
    school == "San José State" ~ "San Jose State",
    school == "SE Louisiana" ~ "Southeastern Louisiana",
    school == "Southern" ~ "Southern University",
    school == "St. Thomas (MN)" ~ "St. Thomas",
    school == "UT Martin" ~ "Tennessee-Martin",
    school == "East Texas A&M" ~ "Texas A&M - Commerce",
    school == "South Florida" ~ "USF",
    school == "VMI" ~ "Virginia Military Institute",
    TRUE ~ school),
    mascot = ifelse(mascot == "Rainbow Warriors", "Warriors", mascot),
    mascot = ifelse(school == "Florida International", "Golden Panthers", mascot)) |> 
  mutate(ultimate_name = paste(match_school, mascot)) |> 
  select(school, ultimate_name)

all_team_codes <- pff_team_codes |> 
  inner_join(fastr_teaminfo) |> 
  rename(fastr_name = school)

fastr_roster <- fastr_roster |> 
  inner_join(all_team_codes |> select(fastr_name, team_code), by = join_by(team == fastr_name)) |> 
  select(athlete_id, contains("name"), jersey, team, position, team_code, jersey_number) |> 
  mutate(pff_identifier = paste(team_code, jersey_number))

pff_roster <- pff_roster |> 
  mutate(pff_identifier = paste(team, jersey_number)) |> 
  select(-team_id, -jersey_number)

duplicates <- fastr_roster |> 
  group_by(pff_identifier) |> 
  summarise(count = n()) |> 
  filter(count > 1) |> 
  inner_join(fastr_roster |> select(athlete_id:last_name, jersey, pff_identifier)) |> 
  filter(!is.na(jersey)) |> 
  inner_join(pff_roster) |> 
  select(-count) |> 
  inner_join(all_team_codes |> select(team_code, fastr_name), by = join_by(team == team_code))

correct_duplicates <- duplicates |> 
  mutate(full_name = paste(first_name, last_name)) |> 
  filter(player_name == full_name) |> 
  select(-full_name)

incorrect_duplicates <- duplicates |> 
  anti_join(correct_duplicates)


### THIS PART BELOW IS UNFINISHED

no_duplicates <- pff_roster |> 
  filter(!(pff_identifier %in% duplicates$pff_identifier)) |> 
  inner_join(all_team_codes |> select(team_code, fastr_name), by = join_by(team == team_code)) |> 
  inner_join(fastr_roster |> 
               filter(!is.na(jersey_number)) |> 
               filter(!(pff_identifier %in% duplicates$pff_identifier)) |> 
               select(athlete_id:last_name, jersey, pff_identifier))

pff_duplicates <- no_duplicates |> 
  group_by(pff_identifier) |> 
  summarise(count = n()) |> 
  filter(count > 1)

no_duplicates <- no_duplicates |> 
  filter((!(pff_identifier) %in% pff_duplicates$pff_identifier) | player_name == paste(first_name, last_name))

# no_duplicates |> 
#   group_by(pff_identifier) |> 
#   summarise(count = n()) |> 
#   filter(count > 1)

full_roster <- bind_rows(no_duplicates, correct_duplicates) |> 
  filter(player_name == paste(first_name, last_name)) |> 
  distinct()

write_csv(full_roster, "names_and_ids_2023.csv")

war_roster23 <- fastr_roster |> 
  mutate(player = paste(first_name, last_name)) |> 
  select(-team, -position) |> 
  inner_join(war23 |> inner_join(pff_team_codes, by = join_by(team == ultimate_name, franchise_id)),
             by = join_by(player, team_code))

write_csv(war_roster23, "war_and_roster2023.csv")


# 2022 Player ID Matching -------------------------------------------------

fastr_roster <- cfbfastR::load_cfb_rosters(seasons = 2022)
# pff_roster <- read_csv("roster_feed.csv") |> 
#   select(gsis_player_id, team, team_id:jersey_number) |> 
#   mutate(position = case_when(
#     str_detect(position, "WR") ~ "WR",
#     str_detect(position, "LB") ~ "LB",
#     str_detect(position, "CB") ~ "CB",
#     str_detect(position, "TE") ~ "TE",
#     str_detect(position, "RE|LE") ~ "DE",
#     str_detect(position, "G") ~ "G",
#     str_detect(position, "D.T|NT") ~ "DT",
#     str_detect(position, "RT|LT") ~ "OT",
#     str_detect(position, "SS|FS") ~ "S",
#     TRUE ~ position
#   )) |> 
#   unique()

pff_roster <- read_csv("roster_feed_2022.csv") |> 
  select(gsis_player_id, team, team_id:jersey_number) |> 
  select(-position) |> 
  unique()

offense_positions <- c("WR", "OL", "TE", "RB", "QB", "FB", "G", "C", "OT")
defense_positions <- c("DL", "DB", "CB", "DT", "LB", "S", "NT", "DE", "EDGE")
special_teams_positions <- c("LS", "KR", "PK", "P")

fastr_roster <- fastr_roster |> 
  mutate(pff_jersey = ifelse(jersey < 10, paste0(0, jersey), jersey)) |> 
  mutate(jersey_number = case_when(
    position %in% offense_positions ~ pff_jersey,
    position %in% defense_positions ~ paste0("D", pff_jersey),
    position %in% special_teams_positions ~ paste0("S", pff_jersey)
  ))

pff_team_codes <- read_csv("pff_team_codes.csv") |> clean_names()
fastr_teaminfo <- cfbfastR::cfbd_team_info(only_fbs = FALSE) |> 
  filter(classification %in% c("fbs", "fcs")) |> 
  mutate(match_school = case_when(
    school == "UAlbany" ~ "Albany", 
    school == "Hawai'i" ~ "Hawaii",
    school == "App State" ~ "Appalachian State",
    school == "Bryant" ~ "Bryant University",
    school == "Central Connecticut" ~ "Central Connecticut State",
    school == "UConn" ~ "Connecticut",
    school == "Delaware" ~ "Delaware Fightin",
    school == "Grambling" ~ "Grambling State",
    school == "Long Island University" ~ "LIU",
    school == "UL Monroe" ~ "Louisiana-Monroe",
    school == "McNeese" ~ "McNeese State",
    school == "Miami" ~ "Miami (FL)",
    school == "Ole Miss" ~ "Mississippi",
    school == "NC State" ~ "North Carolina State",
    school == "Sam Houston" ~ "Sam Houston State",
    school == "San José State" ~ "San Jose State",
    school == "SE Louisiana" ~ "Southeastern Louisiana",
    school == "Southern" ~ "Southern University",
    school == "St. Thomas (MN)" ~ "St. Thomas",
    school == "UT Martin" ~ "Tennessee-Martin",
    school == "East Texas A&M" ~ "Texas A&M - Commerce",
    school == "South Florida" ~ "USF",
    school == "VMI" ~ "Virginia Military Institute",
    TRUE ~ school),
    mascot = ifelse(mascot == "Rainbow Warriors", "Warriors", mascot),
    mascot = ifelse(school == "Florida International", "Golden Panthers", mascot)) |> 
  mutate(ultimate_name = paste(match_school, mascot)) |> 
  select(school, ultimate_name)

all_team_codes <- pff_team_codes |> 
  inner_join(fastr_teaminfo) |> 
  rename(fastr_name = school)

fastr_roster <- fastr_roster |> 
  inner_join(all_team_codes |> select(fastr_name, team_code), by = join_by(team == fastr_name)) |> 
  select(athlete_id, contains("name"), jersey, team, position, team_code, jersey_number) |> 
  mutate(pff_identifier = paste(team_code, jersey_number))

pff_roster <- pff_roster |> 
  mutate(pff_identifier = paste(team, jersey_number)) |> 
  select(-team_id, -jersey_number)

duplicates <- fastr_roster |> 
  group_by(pff_identifier) |> 
  summarise(count = n()) |> 
  filter(count > 1) |> 
  inner_join(fastr_roster |> select(athlete_id:last_name, jersey, pff_identifier)) |> 
  filter(!is.na(jersey)) |> 
  inner_join(pff_roster) |> 
  select(-count) |> 
  inner_join(all_team_codes |> select(team_code, fastr_name), by = join_by(team == team_code))

correct_duplicates <- duplicates |> 
  mutate(full_name = paste(first_name, last_name)) |> 
  filter(player_name == full_name) |> 
  select(-full_name)

incorrect_duplicates <- duplicates |> 
  anti_join(correct_duplicates)


### THIS PART BELOW IS UNFINISHED

no_duplicates <- pff_roster |> 
  filter(!(pff_identifier %in% duplicates$pff_identifier)) |> 
  inner_join(all_team_codes |> select(team_code, fastr_name), by = join_by(team == team_code)) |> 
  inner_join(fastr_roster |> 
               filter(!is.na(jersey_number)) |> 
               filter(!(pff_identifier %in% duplicates$pff_identifier)) |> 
               select(athlete_id:last_name, jersey, pff_identifier))

pff_duplicates <- no_duplicates |> 
  group_by(pff_identifier) |> 
  summarise(count = n()) |> 
  filter(count > 1)

no_duplicates <- no_duplicates |> 
  filter((!(pff_identifier) %in% pff_duplicates$pff_identifier) | player_name == paste(first_name, last_name))

# no_duplicates |>
#   group_by(pff_identifier) |>
#   summarise(count = n()) |>
#   filter(count > 1)

full_roster <- bind_rows(no_duplicates, correct_duplicates) |> 
  filter(player_name == paste(first_name, last_name)) |> 
  distinct()

write_csv(full_roster, "names_and_ids_2022.csv")

war_roster22 <- fastr_roster |> 
  mutate(player = paste(first_name, last_name)) |> 
  select(-team, -position) |> 
  inner_join(war22 |> inner_join(pff_team_codes, by = join_by(team == ultimate_name, franchise_id)),
             by = join_by(player, team_code))

write_csv(war_roster22, "war_and_roster2022.csv")


# 2021 Player ID Matching -------------------------------------------------

fastr_roster <- cfbfastR::load_cfb_rosters(seasons = 2021)
# pff_roster <- read_csv("roster_feed.csv") |> 
#   select(gsis_player_id, team, team_id:jersey_number) |> 
#   mutate(position = case_when(
#     str_detect(position, "WR") ~ "WR",
#     str_detect(position, "LB") ~ "LB",
#     str_detect(position, "CB") ~ "CB",
#     str_detect(position, "TE") ~ "TE",
#     str_detect(position, "RE|LE") ~ "DE",
#     str_detect(position, "G") ~ "G",
#     str_detect(position, "D.T|NT") ~ "DT",
#     str_detect(position, "RT|LT") ~ "OT",
#     str_detect(position, "SS|FS") ~ "S",
#     TRUE ~ position
#   )) |> 
#   unique()

pff_roster <- read_csv("roster_feed_2021.csv") |> 
  select(gsis_player_id, team, team_id:jersey_number) |> 
  select(-position) |> 
  unique()

offense_positions <- c("WR", "OL", "TE", "RB", "QB", "FB", "G", "C", "OT")
defense_positions <- c("DL", "DB", "CB", "DT", "LB", "S", "NT", "DE", "EDGE")
special_teams_positions <- c("LS", "KR", "PK", "P")

fastr_roster <- fastr_roster |> 
  mutate(pff_jersey = ifelse(jersey < 10, paste0(0, jersey), jersey)) |> 
  mutate(jersey_number = case_when(
    position %in% offense_positions ~ pff_jersey,
    position %in% defense_positions ~ paste0("D", pff_jersey),
    position %in% special_teams_positions ~ paste0("S", pff_jersey)
  ))

pff_team_codes <- read_csv("pff_team_codes.csv") |> clean_names()
fastr_teaminfo <- cfbfastR::cfbd_team_info(only_fbs = FALSE) |> 
  filter(classification %in% c("fbs", "fcs")) |> 
  mutate(match_school = case_when(
    school == "UAlbany" ~ "Albany", 
    school == "Hawai'i" ~ "Hawaii",
    school == "App State" ~ "Appalachian State",
    school == "Bryant" ~ "Bryant University",
    school == "Central Connecticut" ~ "Central Connecticut State",
    school == "UConn" ~ "Connecticut",
    school == "Delaware" ~ "Delaware Fightin",
    school == "Grambling" ~ "Grambling State",
    school == "Long Island University" ~ "LIU",
    school == "UL Monroe" ~ "Louisiana-Monroe",
    school == "McNeese" ~ "McNeese State",
    school == "Miami" ~ "Miami (FL)",
    school == "Ole Miss" ~ "Mississippi",
    school == "NC State" ~ "North Carolina State",
    school == "Sam Houston" ~ "Sam Houston State",
    school == "San José State" ~ "San Jose State",
    school == "SE Louisiana" ~ "Southeastern Louisiana",
    school == "Southern" ~ "Southern University",
    school == "St. Thomas (MN)" ~ "St. Thomas",
    school == "UT Martin" ~ "Tennessee-Martin",
    school == "East Texas A&M" ~ "Texas A&M - Commerce",
    school == "South Florida" ~ "USF",
    school == "VMI" ~ "Virginia Military Institute",
    TRUE ~ school),
    mascot = ifelse(mascot == "Rainbow Warriors", "Warriors", mascot),
    mascot = ifelse(school == "Florida International", "Golden Panthers", mascot)) |> 
  mutate(ultimate_name = paste(match_school, mascot)) |> 
  select(school, ultimate_name)

all_team_codes <- pff_team_codes |> 
  inner_join(fastr_teaminfo) |> 
  rename(fastr_name = school)

fastr_roster <- fastr_roster |> 
  inner_join(all_team_codes |> select(fastr_name, team_code), by = join_by(team == fastr_name)) |> 
  select(athlete_id, contains("name"), jersey, team, position, team_code, jersey_number) |> 
  mutate(pff_identifier = paste(team_code, jersey_number))

pff_roster <- pff_roster |> 
  mutate(pff_identifier = paste(team, jersey_number)) |> 
  select(-team_id, -jersey_number)

duplicates <- fastr_roster |> 
  group_by(pff_identifier) |> 
  summarise(count = n()) |> 
  filter(count > 1) |> 
  inner_join(fastr_roster |> select(athlete_id:last_name, jersey, pff_identifier)) |> 
  filter(!is.na(jersey)) |> 
  inner_join(pff_roster) |> 
  select(-count) |> 
  inner_join(all_team_codes |> select(team_code, fastr_name), by = join_by(team == team_code))

correct_duplicates <- duplicates |> 
  mutate(full_name = paste(first_name, last_name)) |> 
  filter(player_name == full_name) |> 
  select(-full_name)

incorrect_duplicates <- duplicates |> 
  anti_join(correct_duplicates)


### THIS PART BELOW IS UNFINISHED

no_duplicates <- pff_roster |> 
  filter(!(pff_identifier %in% duplicates$pff_identifier)) |> 
  inner_join(all_team_codes |> select(team_code, fastr_name), by = join_by(team == team_code)) |> 
  inner_join(fastr_roster |> 
               filter(!is.na(jersey_number)) |> 
               filter(!(pff_identifier %in% duplicates$pff_identifier)) |> 
               select(athlete_id:last_name, jersey, pff_identifier))

pff_duplicates <- no_duplicates |> 
  group_by(pff_identifier) |> 
  summarise(count = n()) |> 
  filter(count > 1)

no_duplicates <- no_duplicates |> 
  filter((!(pff_identifier) %in% pff_duplicates$pff_identifier) | player_name == paste(first_name, last_name))

# no_duplicates |>
#   group_by(pff_identifier) |>
#   summarise(count = n()) |>
#   filter(count > 1)

full_roster <- bind_rows(no_duplicates, correct_duplicates) |> 
  filter(player_name == paste(first_name, last_name)) |> 
  distinct()

write_csv(full_roster, "names_and_ids_2021.csv")


war_roster21 <- fastr_roster |> 
  mutate(player = paste(first_name, last_name)) |> 
  select(-team, -position) |> 
  inner_join(war21 |> inner_join(pff_team_codes, by = join_by(team == ultimate_name, franchise_id)),
             by = join_by(player, team_code))

write_csv(war_roster21, "war_and_roster2021.csv")
  