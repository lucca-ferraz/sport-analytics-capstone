library(tidyverse)

team_codes <- read_csv("team_codes.csv")

war_25 <- read_csv("war_2025.csv")
war_24 <- read_csv("war_2024.csv")
war_23 <- read_csv("war_2023.csv")
war_22 <- read_csv("war_2022.csv")
war_21 <- read_csv("war_2021.csv")

projected_war_25 <- read_csv("projected_war_25.csv") |> 
  filter(projected_season == 2025)
projected_war_24 <- read_csv("projected_war_24.csv") |> 
  filter(projected_season == 2024)
projected_war_23 <- read_csv("projected_war_23.csv") |> 
  filter(projected_season == 2023)
projected_war_22 <- read_csv("projected_war_22.csv") |> 
  filter(projected_season == 2022)

war_proj_war_25 <- war_25 |> 
  inner_join(projected_war_25 |> select(-season, -team), by = join_by(player_id))
war_proj_war_24 <- war_24 |> 
  inner_join(projected_war_24 |> select(-season, -team), by = join_by(player_id))
war_proj_war_23 <- war_23 |> 
  inner_join(projected_war_23 |> select(-season, -team), by = join_by(player_id))
war_proj_war_22 <- war_22 |> 
  inner_join(projected_war_22 |> select(-season, -team), by = join_by(player_id))

summary(lm(waa ~ projected_waa, data = war_proj_war_25))
summary(lm(waa ~ projected_waa, data = war_proj_war_24))
summary(lm(waa ~ projected_waa, data = war_proj_war_23))
summary(lm(waa ~ projected_waa, data = war_proj_war_22))

summary(lm(waa ~ projected_waa, data = bind_rows(war_proj_war_22, 
                                                 war_proj_war_23,
                                                 war_proj_war_24,
                                                 war_proj_war_25)))

bind_rows(war_proj_war_22, 
          war_proj_war_23,
          war_proj_war_24,
          war_proj_war_25) |> 
  ggplot(aes(projected_waa, waa)) +
  geom_point() +
  geom_smooth(method = "lm")


library(cfbfastR)
schedule_25 <- load_cfb_schedules(2025) |> 
  mutate(home_win = ifelse(home_points > away_points, 1, 0))
schedule_24 <- load_cfb_schedules(2024) |> 
  mutate(home_win = ifelse(home_points > away_points, 1, 0)) |> 
  filter(!is.na(home_points)) # take out cancelled games
schedule_23 <- load_cfb_schedules(2023) |> 
  mutate(home_win = ifelse(home_points > away_points, 1, 0))
schedule_22 <- load_cfb_schedules(2022) |> 
  mutate(home_win = ifelse(home_points > away_points, 1, 0))
schedule_21 <- load_cfb_schedules(2021) |> 
  mutate(home_win = ifelse(home_points > away_points, 1, 0))

home_records_25 <- schedule_25 |> 
  filter(home_division == "fbs") |> 
  group_by(home_team) |> 
  summarise(home_games = n(), home_wins = sum(home_win),
            home_losses = home_games - home_wins,
            home_pts = sum(home_points), home_pts_allowed = sum(away_points),
            home_pt_diff = sum(home_points) - sum(away_points)) |> 
  ungroup() |> 
  rename(team = home_team)

away_records_25 <- schedule_25 |> 
  filter(away_division == "fbs") |> 
  group_by(away_team) |> 
  summarise(away_games = n(), away_losses = sum(home_win),
            away_wins = away_games - away_losses,
            away_pts = sum(away_points), away_pts_allowed = sum(home_points),
            away_pt_diff = sum(away_points) - sum(home_points)) |> 
  ungroup() |> 
  rename(team = away_team)

records_25 <- home_records_25 |> 
  inner_join(away_records_25) |> 
  mutate(games = home_games + away_games, wins = home_wins + away_wins,
         losses = home_losses + away_losses, win_pct = wins / games,
         pts = home_pts + away_pts, pts_allowed = home_pts_allowed + away_pts_allowed,
         pt_diff = home_pt_diff + away_pt_diff) |> 
  select(team, games, wins, losses, win_pct, pts, pts_allowed, pt_diff)

team_war_25 <- war_proj_war_25 |> 
  group_by(team) |> 
  summarise(war = sum(waa),
            proj_war = sum(projected_waa)) |> 
  ungroup()

record_and_war_25 <- records_25 |> 
  inner_join(team_codes, by = join_by(team == fastr_name)) |> 
  inner_join(team_war_25, by = join_by(ultimate_name == team))

record_and_war_25 |> 
  ggplot(aes(war, wins)) +
  geom_point() +
  geom_smooth(method = "lm")

record_and_war_25 |> 
  ggplot(aes(war, win_pct)) +
  geom_point() +
  geom_smooth(method = "lm")


summary(lm(wins ~ war, data = record_and_war_25))
summary(lm(wins ~ proj_war, data = record_and_war_25))


home_records_24 <- schedule_24 |> 
  filter(home_division == "fbs") |> 
  group_by(home_team) |> 
  summarise(home_games = n(), home_wins = sum(home_win),
            home_losses = home_games - home_wins,
            home_pts = sum(home_points), home_pts_allowed = sum(away_points),
            home_pt_diff = sum(home_points) - sum(away_points)) |> 
  ungroup() |> 
  rename(team = home_team)

away_records_24 <- schedule_24 |> 
  filter(away_division == "fbs") |> 
  group_by(away_team) |> 
  summarise(away_games = n(), away_losses = sum(home_win),
            away_wins = away_games - away_losses,
            away_pts = sum(away_points), away_pts_allowed = sum(home_points),
            away_pt_diff = sum(away_points) - sum(home_points)) |> 
  ungroup() |> 
  rename(team = away_team)

records_24 <- home_records_24 |> 
  inner_join(away_records_24) |> 
  mutate(games = home_games + away_games, wins = home_wins + away_wins,
         losses = home_losses + away_losses, win_pct = wins / games,
         pts = home_pts + away_pts, pts_allowed = home_pts_allowed + away_pts_allowed,
         pt_diff = home_pt_diff + away_pt_diff) |> 
  select(team, games, wins, losses, win_pct, pts, pts_allowed, pt_diff)

team_war_24 <- war_proj_war_24 |> 
  group_by(team) |> 
  summarise(war = sum(waa),
            proj_war = sum(projected_waa)) |> 
  ungroup()

record_and_war_24 <- records_24 |> 
  inner_join(team_codes, by = join_by(team == fastr_name)) |> 
  inner_join(team_war_24, by = join_by(ultimate_name == team))

record_and_war_24 |> 
  ggplot(aes(war, wins)) +
  geom_point() +
  geom_smooth(method = "lm")

summary(lm(wins ~ war, data = record_and_war_24))
summary(lm(wins ~ proj_war, data = record_and_war_24))

home_records_23 <- schedule_23 |> 
  filter(home_division == "fbs") |> 
  group_by(home_team) |> 
  summarise(home_games = n(), home_wins = sum(home_win),
            home_losses = home_games - home_wins,
            home_pts = sum(home_points), home_pts_allowed = sum(away_points),
            home_pt_diff = sum(home_points) - sum(away_points)) |> 
  ungroup() |> 
  rename(team = home_team)

away_records_23 <- schedule_23 |> 
  filter(away_division == "fbs") |> 
  group_by(away_team) |> 
  summarise(away_games = n(), away_losses = sum(home_win),
            away_wins = away_games - away_losses,
            away_pts = sum(away_points), away_pts_allowed = sum(home_points),
            away_pt_diff = sum(away_points) - sum(home_points)) |> 
  ungroup() |> 
  rename(team = away_team)

records_23 <- home_records_23 |> 
  inner_join(away_records_23) |> 
  mutate(games = home_games + away_games, wins = home_wins + away_wins,
         losses = home_losses + away_losses, win_pct = wins / games,
         pts = home_pts + away_pts, pts_allowed = home_pts_allowed + away_pts_allowed,
         pt_diff = home_pt_diff + away_pt_diff) |> 
  select(team, games, wins, losses, win_pct, pts, pts_allowed, pt_diff)

team_war_23 <- war_proj_war_23 |> 
  group_by(team) |> 
  summarise(war = sum(waa),
            proj_war = sum(projected_waa)) |> 
  ungroup()

record_and_war_23 <- records_23 |> 
  inner_join(team_codes, by = join_by(team == fastr_name)) |> 
  inner_join(team_war_23, by = join_by(ultimate_name == team))

record_and_war_23 |> 
  ggplot(aes(proj_war, wins)) +
  geom_point() +
  geom_smooth(method = "lm")

summary(lm(wins ~ war, data = record_and_war_23))
summary(lm(wins ~ proj_war, data = record_and_war_23))


home_records_22 <- schedule_22 |> 
  filter(home_division == "fbs") |> 
  group_by(home_team) |> 
  summarise(home_games = n(), home_wins = sum(home_win),
            home_losses = home_games - home_wins,
            home_pts = sum(home_points), home_pts_allowed = sum(away_points),
            home_pt_diff = sum(home_points) - sum(away_points)) |> 
  ungroup() |> 
  rename(team = home_team)

away_records_22 <- schedule_22 |> 
  filter(away_division == "fbs") |> 
  group_by(away_team) |> 
  summarise(away_games = n(), away_losses = sum(home_win),
            away_wins = away_games - away_losses,
            away_pts = sum(away_points), away_pts_allowed = sum(home_points),
            away_pt_diff = sum(away_points) - sum(home_points)) |> 
  ungroup() |> 
  rename(team = away_team)

records_22 <- home_records_22 |> 
  inner_join(away_records_22) |> 
  mutate(games = home_games + away_games, wins = home_wins + away_wins,
         losses = home_losses + away_losses, win_pct = wins / games,
         pts = home_pts + away_pts, pts_allowed = home_pts_allowed + away_pts_allowed,
         pt_diff = home_pt_diff + away_pt_diff) |> 
  select(team, games, wins, losses, win_pct, pts, pts_allowed, pt_diff)

team_war_22 <- war_proj_war_22 |> 
  group_by(team) |> 
  summarise(war = sum(waa),
            proj_war = sum(projected_waa)) |> 
  ungroup()

record_and_war_22 <- records_22 |> 
  inner_join(team_codes, by = join_by(team == fastr_name)) |> 
  inner_join(team_war_22, by = join_by(ultimate_name == team))

record_and_war_22 |> 
  ggplot(aes(war, wins)) +
  geom_point() +
  geom_smooth(method = "lm")

summary(lm(wins ~ war, data = record_and_war_22))
summary(lm(wins ~ proj_war, data = record_and_war_22))

home_records_21 <- schedule_21 |> 
  filter(home_division == "fbs") |> 
  group_by(home_team) |> 
  summarise(home_games = n(), home_wins = sum(home_win),
            home_losses = home_games - home_wins,
            home_pts = sum(home_points), home_pts_allowed = sum(away_points),
            home_pt_diff = sum(home_points) - sum(away_points)) |> 
  ungroup() |> 
  rename(team = home_team)

away_records_21 <- schedule_21 |> 
  filter(away_division == "fbs") |> 
  group_by(away_team) |> 
  summarise(away_games = n(), away_losses = sum(home_win),
            away_wins = away_games - away_losses,
            away_pts = sum(away_points), away_pts_allowed = sum(home_points),
            away_pt_diff = sum(away_points) - sum(home_points)) |> 
  ungroup() |> 
  rename(team = away_team)

records_21 <- home_records_21 |> 
  inner_join(away_records_21) |> 
  mutate(games = home_games + away_games, wins = home_wins + away_wins,
         losses = home_losses + away_losses, win_pct = wins / games,
         pts = home_pts + away_pts, pts_allowed = home_pts_allowed + away_pts_allowed,
         pt_diff = home_pt_diff + away_pt_diff) |> 
  select(team, games, wins, losses, win_pct, pts, pts_allowed, pt_diff)

team_war_21 <- war_21 |> 
  group_by(team) |> 
  summarise(war = sum(waa)) |> 
  ungroup()

record_and_war_21 <- records_21 |> 
  inner_join(team_codes, by = join_by(team == fastr_name)) |> 
  inner_join(team_war_21, by = join_by(ultimate_name == team))

record_and_war_21 |> 
  ggplot(aes(war, wins)) +
  geom_point() +
  geom_smooth(method = "lm")

summary(lm(wins ~ war, data = record_and_war_21))

all_record_and_war <- bind_rows(record_and_war_21 |> mutate(year = 2021), 
                                record_and_war_22 |> mutate(year = 2022), 
                                record_and_war_23 |> mutate(year = 2023),
                                record_and_war_24 |> mutate(year = 2024),
                                record_and_war_25 |> mutate(year = 2025))

all_record_and_war |> 
  ggplot(aes(war, wins)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggthemes::theme_clean() +
  labs(x = "PFF WAA", y = "Win Percentage", 
       title = "PFF WAA Shows Strong Relationship With Observed Wins",
       subtitle = "PFF WAA vs Wins, 2021-2025 CFB Seasons")

summary(lm(wins ~ war, data = all_record_and_war))

all_record_and_war |> 
  ggplot(aes(proj_war, win_pct)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggthemes::theme_clean() +
  labs(x = "PFF WAA", y = "Win Percentage", 
       title = "PFF WAA Shows Strong Relationship With Win Percentage",
       subtitle = "PFF WAA vs Win %, 2021-2025 CFB Seasons")

summary(lm(wins ~ proj_war, data = all_record_and_war))

