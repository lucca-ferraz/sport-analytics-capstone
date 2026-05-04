library(tidyverse)
library(cfbfastR)
library(ranger)
library(vip)
library(xgboost)
library(caret)
team_codes <- read_csv("team_codes.csv")

roster21 <- cfbfastR::cfbd_team_roster(year = 2021) |> 
  mutate(season = 2021)
roster22 <- cfbfastR::cfbd_team_roster(year = 2022) |> 
  mutate(season = 2022)
roster23 <- cfbfastR::cfbd_team_roster(year = 2023) |> 
  mutate(season = 2023)
roster24 <- cfbfastR::cfbd_team_roster(year = 2024) |> 
  mutate(season = 2024)
roster25 <- cfbfastR::cfbd_team_roster(year = 2025) |> 
  mutate(season = 2025)

all_rosters <- bind_rows(roster21, roster22, roster23, roster24, roster25) |> 
  filter(year < 8) |> 
  filter(jersey != -1) |> 
  filter(!is.na(weight)) |> 
  mutate(athlete_id = as.double(athlete_id)) |> 
  select(athlete_id, season, weight, height) |> 
  unique()

war25 <- read_csv("war_and_roster2025.csv")
war24 <- read_csv("war_and_roster2024.csv")
war23 <- read_csv("war_and_roster2023.csv")
war22 <- read_csv("war_and_roster2022.csv")
war21 <- read_csv("war_and_roster2021.csv")

projected_war_26 <- read_csv("projected_war_26.csv") |> 
  filter(projected_season == 2026)
projected_war_25 <- read_csv("projected_war_25.csv") |> 
  filter(projected_season == 2025)
projected_war_24 <- read_csv("projected_war_24.csv") |> 
  filter(projected_season == 2024)
projected_war_23 <- read_csv("projected_war_23.csv") |> 
  filter(projected_season == 2023)
projected_war_22 <- read_csv("projected_war_22.csv") |> 
  filter(projected_season == 2022)

athleticism25 <- read_csv("athleticism_2025.csv")
athleticism24 <- read_csv("athleticism_2024.csv")
athleticism23 <- read_csv("athleticism_2023.csv")
athleticism22 <- read_csv("athleticism_2022.csv")
athleticism21 <- read_csv("athleticism_2021.csv")
all_athleticism <- bind_rows(athleticism25, athleticism24, athleticism23, 
                             athleticism22, athleticism21) |> 
  select(player_id, season, contains("value"))

stats25 <- cfbd_stats_season_player(year = 2025, season_type = "regular")
stats24 <- cfbd_stats_season_player(year = 2024, season_type = "regular")
stats23 <- cfbd_stats_season_player(year = 2023, season_type = "regular")
stats22 <- cfbd_stats_season_player(year = 2022, season_type = "regular")
stats21 <- cfbd_stats_season_player(year = 2021, season_type = "regular")

all_stats <- bind_rows(stats25, stats24, stats23, stats22, stats21) |> 
  select(year:defensive_td) |> 
  filter(!(position %in% c("P", "PK", "LS", "ATH", "KR", "PR", "?"))) |> 
  mutate(athlete_id = as.double(athlete_id))

all_conferences <- all_stats |> 
  select(year, team, conference) |> 
  unique()

qb_stats <- all_stats |> 
  filter(position == "QB") |> 
  select(year:rushing_long, fumbles_fum:fumbles_lost) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))
skill_pos_stats <- all_stats |> 
  filter(position %in% c("WR", "TE", "RB", "FB")) |> 
  select(year:position, rushing_car:fumbles_lost) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))
rb_stats <- skill_pos_stats |> 
  filter(position == "RB" | position == "FB")
wr_stats <- skill_pos_stats |> 
  filter(position == "WR")
te_stats <- skill_pos_stats |> 
  filter(position == "TE")
defensive_stats <- all_stats |> 
  filter(position %in% c("CB", "DB", "DL", "LB", "DT", "S", "DE", "EDGE", "NT", 
                         "OLB")) |> 
  select(year:position, defensive_solo:defensive_td) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

recruiting_16 <- cfbd_recruiting_player(year = 2016)
recruiting_17 <- cfbd_recruiting_player(year = 2017)
recruiting_18 <- cfbd_recruiting_player(year = 2018)
recruiting_19 <- cfbd_recruiting_player(year = 2019)
recruiting_20 <- cfbd_recruiting_player(year = 2020)
recruiting_21 <- cfbd_recruiting_player(year = 2021)
recruiting_22 <- cfbd_recruiting_player(year = 2022)
recruiting_23 <- cfbd_recruiting_player(year = 2023)
recruiting_24 <- cfbd_recruiting_player(year = 2024)
recruiting_25 <- cfbd_recruiting_player(year = 2025)
all_recruiting <- bind_rows(recruiting_16, recruiting_17, recruiting_18,
                            recruiting_19, recruiting_20, recruiting_21, 
                            recruiting_22, recruiting_23, recruiting_24, 
                            recruiting_25) |> 
  mutate(athlete_id = as.double(athlete_id)) |> 
  filter(!(name == "Sione Finau" & position == "RB"))

portal_21 <- cfbd_recruiting_transfer_portal(year = 2021)
portal_22 <- cfbd_recruiting_transfer_portal(year = 2022)
portal_23 <- cfbd_recruiting_transfer_portal(year = 2023)
portal_24 <- cfbd_recruiting_transfer_portal(year = 2024)
portal_25 <- cfbd_recruiting_transfer_portal(year = 2025)
all_portal <- bind_rows(portal_21, portal_22, portal_23, portal_24, portal_25) |> 
  select(season:destination, stars) |> 
  unique() |> 
  mutate(stars = ifelse(is.na(stars), 0, stars)) |> 
  filter(!(first_name == "Sione" & last_name == "Finau" & position == "RB"))

# coverage_21 <- read_csv("season_all22_coverage_grade21.csv")
# coverage_22 <- read_csv("season_all22_coverage_grade22.csv")
# coverage_23 <- read_csv("season_all22_coverage_grade23.csv")
# coverage_24 <- read_csv("season_all22_coverage_grade24.csv")
# coverage_25 <- read_csv("season_all22_coverage_grade25.csv")

all_war24 <- war24 |> 
  inner_join(war25 |> 
               rename(waa_next_year = waa, team_next_year = team, snaps_next_year = snaps) |> 
               select(waa_next_year, team_next_year, snaps_next_year, athlete_id)) |> 
  inner_join(projected_war_25 |> select(projected_waa, player_id))
all_war23 <- war23 |> 
  inner_join(war24 |> 
               rename(waa_next_year = waa, team_next_year = team, snaps_next_year = snaps) |> 
               select(waa_next_year, team_next_year, snaps_next_year, athlete_id)) |> 
  inner_join(projected_war_24 |> select(projected_waa, player_id))
all_war22 <- war22 |> 
  inner_join(war23 |> 
               rename(waa_next_year = waa, team_next_year = team, snaps_next_year = snaps) |> 
               select(waa_next_year, team_next_year, snaps_next_year, athlete_id)) |> 
  inner_join(projected_war_23 |> select(projected_waa, player_id))
all_war21 <- war21 |> 
  inner_join(war22 |> 
               rename(waa_next_year = waa, team_next_year = team, snaps_next_year = snaps) |> 
               select(waa_next_year, team_next_year, snaps_next_year, athlete_id)) |> 
  inner_join(projected_war_22 |> select(projected_waa, player_id))
all_wars <- bind_rows(all_war24, all_war23, all_war22, all_war21) |> 
  filter(!(position %in% c("K", "LS", "P"))) |> 
  inner_join(team_codes |> select(team_code, fastr_name))

all_model_data <- all_wars |> 
  filter(!(player == "Sione Moa" & pff_identifier == "UTBY D08" & position == "HB")) |> 
  filter(!(player == "Sione Moa" & pff_identifier == "UTBY 30" & position == "LB")) |> 
  left_join(all_recruiting |> 
              select(athlete_id, name, ranking, hs_stars = stars, rating), 
            by = join_by(athlete_id, player == name)) |> 
  left_join(all_portal |> 
              mutate(season = season - 1) |> 
              select(season, first_name, last_name, origin, tp_stars = stars), 
            by = join_by(first_name, last_name, season, fastr_name == origin)) |> 
  inner_join(all_rosters) |> 
  mutate(ranking = ifelse(is.na(ranking), 9999, ranking),
         hs_stars = ifelse(is.na(hs_stars), 0, hs_stars),
         rating = ifelse(is.na(rating), 0, rating),
         tp_stars = ifelse(is.na(tp_stars), 9999, tp_stars)) |> 
  inner_join(all_conferences, by = join_by(season == year, fastr_name == team)) |> 
  select(athlete_id, player, season, player_id, position, snaps, year_in_league, 
         team, ranking:conference, waa, waa_next_year, team_next_year, snaps_next_year, 
         projected_waa)

all_wars |> 
  ggplot(aes(waa, waa_next_year)) +
  geom_point() +
  geom_smooth(method = "lm")
summary(lm(waa_next_year ~ waa, weights = snaps, data = all_wars))
summary(lm(waa_next_year ~ projected_waa, weights = snaps, data = all_wars))

unique(all_model_data$position)

qb_model_data <- all_model_data |> 
  filter(position == "QB") |> 
  left_join(qb_stats |> select(year, athlete_id, passing_completions:fumbles_lost),
             by = join_by(athlete_id, season == year)) |> 
  inner_join(all_athleticism |> 
               select(player_id, season, max_speed_value, total_distance_value,
                      contains("speed"), contains("accel"), contains("decel")) |> 
               select(-closing_speed_on_target_value)) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

set.seed(123)
full_qb_mat <- model.matrix(waa_next_year ~ 
                              snaps + year_in_league + ranking + hs_stars +
                              rating + tp_stars + weight + height + conference + waa +
                              passing_completions + passing_att + passing_pct + passing_yds +
                              passing_td + passing_int + passing_ypa + rushing_car + 
                              rushing_yds + rushing_td + rushing_ypc + rushing_long +
                              fumbles_fum + fumbles_rec + fumbles_lost,
                              # avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                              # max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                              # accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                              # accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                              # decel_1_5_value + decel_2_0_value, 
                            data = qb_model_data)
xg_grid <- crossing(nrounds = seq(50, 150, 10),
                    eta = c(0.01, 0.05, 0.1), gamma = 0,
                    max_depth = c(2, 3, 4, 5), colsample_bytree = 1,
                    min_child_weight = 1, subsample = 1)
xg_tune_qb <- train(x = full_qb_mat,
                               y = qb_model_data |> pull(waa_next_year),
                               tuneGrid = xg_grid,
                               trControl = trainControl(method = "cv", number = 5),
                               method = "xgbTree",
                               verbosity = 0)

qb_model <- xgboost(data = full_qb_mat,
                    label = qb_model_data |> pull(waa_next_year),
                    nrounds = xg_tune_qb$bestTune$nrounds,
                    params = as.list(select(xg_tune_qb$bestTune, -nrounds)),
                    objective = "reg:squarederror",
                    verbosity = 0)

qb_oos_preds <- data.frame()
for (i in 2021:2024){
  train <- qb_model_data |> filter(season != i)
  rf_qb_model <- ranger(waa_next_year ~ 
                          snaps + year_in_league + ranking + hs_stars +
                          rating + tp_stars + weight + height + conference + waa +
                          passing_completions + passing_att + passing_pct + passing_yds +
                          passing_td + passing_int + passing_ypa + rushing_car + 
                          rushing_yds + rushing_td + rushing_ypc + rushing_long +
                          fumbles_fum + fumbles_rec + fumbles_lost,
                          # avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                          # max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                          # accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                          # accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                          # decel_1_5_value + decel_2_0_value, 
                        data = train,
                        importance = "impurity")
  test <- qb_model_data |> filter(season == i)
  train_mat <- model.matrix(waa_next_year ~ 
                              snaps + year_in_league + ranking + hs_stars +
                              rating + tp_stars + weight + height + conference + waa +
                              passing_completions + passing_att + passing_pct + passing_yds +
                              passing_td + passing_int + passing_ypa + rushing_car + 
                              rushing_yds + rushing_td + rushing_ypc + rushing_long +
                              fumbles_fum + fumbles_rec + fumbles_lost,
                              # avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                              # max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                              # accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                              # accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                              # decel_1_5_value + decel_2_0_value, 
                            data = train)
  test_mat <- model.matrix(waa_next_year ~ 
                             snaps + year_in_league + ranking + hs_stars +
                             rating + tp_stars + weight + height + conference + waa +
                             passing_completions + passing_att + passing_pct + passing_yds +
                             passing_td + passing_int + passing_ypa + rushing_car + 
                             rushing_yds + rushing_td + rushing_ypc + rushing_long +
                             fumbles_fum + fumbles_rec + fumbles_lost,
                             # avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                             # max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                             # accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                             # accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                             # decel_1_5_value + decel_2_0_value, 
                           data = test)
  xg_model <- xgboost(data = train_mat,
                                    label = train |> pull(waa_next_year),
                                    nrounds = xg_tune_qb$bestTune$nrounds,
                                    params = as.list(select(xg_tune_qb$bestTune, -nrounds)),
                                    objective = "reg:squarederror",
                                    verbosity = 0)
  
  test_xg_preds <- predict(xg_model, test_mat, type = "response")
  test$xg_preds <- test_xg_preds
  test_rf_preds <- predict(rf_qb_model, test)$predictions
  test$rf_preds <- test_rf_preds
  
  qb_oos_preds <- bind_rows(qb_oos_preds, test)
  # print(paste("Hold Out Year", i, "RF LM Summary:"))
  # print(summary(lm(waa_next_year ~ rf_preds, data = test)))
  # print(paste("Hold Out Year", i, "PFF Projections LM Summary:"))
  # print(summary(lm(waa_next_year ~ projected_waa, data = test)))
}
# rf_qb_model <- ranger(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
#                         rating + tp_stars + weight + height + conference + waa +
#                         passing_completions + passing_att + passing_pct + passing_yds +
#                         passing_td + passing_int + passing_ypa + rushing_car + 
#                         rushing_yds + rushing_td + rushing_ypc + rushing_long +
#                         fumbles_fum + fumbles_rec + fumbles_lost, data = qb_model_data,
#                       importance = "impurity")
# vip(rf_qb_model, num_features = 50)
# rf_qb_preds <- predict(rf_qb_model, qb_model_data)$predictions
# qb_model_data$rf_preds <- rf_qb_preds
summary(lm(waa_next_year ~ rf_preds, weights = snaps, data = qb_oos_preds))
summary(lm(waa_next_year ~ xg_preds, weights = snaps, data = qb_oos_preds))
summary(lm(waa_next_year ~ projected_waa, weights = snaps, data = qb_oos_preds))

rb_model_data <- all_model_data |> 
  filter(position == "HB" | position == "FB") |> 
  left_join(rb_stats |> select(year, athlete_id, rushing_car:fumbles_lost),
            by = join_by(athlete_id, season == year)) |> 
  inner_join(all_athleticism |> 
               select(player_id, season, max_speed_value, total_distance_value,
                      contains("speed"), contains("accel"), contains("decel")) |> 
               select(-closing_speed_on_target_value)) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

set.seed(123)
full_rb_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                              rating + tp_stars + weight + height + conference + waa +
                              rushing_car + rushing_yds + rushing_td + rushing_ypc + 
                              rushing_car + rushing_long + fumbles_fum + fumbles_rec +
                              fumbles_lost + max_speed_value + total_distance_value +
                              receiving_rec + receiving_yds + receiving_td + 
                              receiving_ypr + receiving_long + 
                              avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                              max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                              accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                              accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                              decel_1_5_value + decel_2_0_value + position, 
                            data = rb_model_data)
xg_grid <- crossing(nrounds = seq(50, 150, 10),
                    eta = c(0.01, 0.05, 0.1), gamma = 0,
                    max_depth = c(2, 3, 4, 5), colsample_bytree = 1,
                    min_child_weight = 1, subsample = 1)
xg_tune_rb <- train(x = full_rb_mat,
                    y = rb_model_data |> pull(waa_next_year),
                    tuneGrid = xg_grid,
                    trControl = trainControl(method = "cv", number = 5),
                    method = "xgbTree",
                    verbosity = 0)

rb_model <- ranger(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                        rating + tp_stars + weight + height + conference + waa +
                        rushing_car + rushing_yds + rushing_td + rushing_ypc + 
                        rushing_car + rushing_long + fumbles_fum + fumbles_rec +
                        fumbles_lost + max_speed_value + total_distance_value +
                        receiving_rec + receiving_yds + receiving_td + 
                        receiving_ypr + receiving_long + 
                        avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                        max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                        accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                        accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                        decel_1_5_value + decel_2_0_value + position, 
                      data = rb_model_data, importance = "impurity")

rb_oos_preds <- data.frame()
for (i in 2021:2024){
  train <- rb_model_data |> filter(season != i)
  test <- rb_model_data |> filter(season == i)
  rf_rb_model <- ranger(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                          rating + tp_stars + weight + height + conference + waa +
                          rushing_car + rushing_yds + rushing_td + rushing_ypc + 
                          rushing_car + rushing_long + fumbles_fum + fumbles_rec +
                          fumbles_lost + max_speed_value + total_distance_value +
                          receiving_rec + receiving_yds + receiving_td + 
                          receiving_ypr + receiving_long + 
                          avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                          max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                          accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                          accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                          decel_1_5_value + decel_2_0_value + position, 
                        data = train, importance = "impurity")
  train_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                                rating + tp_stars + weight + height + conference + waa +
                                rushing_car + rushing_yds + rushing_td + rushing_ypc + 
                                rushing_car + rushing_long + fumbles_fum + fumbles_rec +
                                fumbles_lost + max_speed_value + total_distance_value +
                                receiving_rec + receiving_yds + receiving_td + 
                                receiving_ypr + receiving_long + 
                                avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                                max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                                accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                                accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                                decel_1_5_value + decel_2_0_value + position, 
                              data = train)
  test_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                              rating + tp_stars + weight + height + conference + waa +
                              rushing_car + rushing_yds + rushing_td + rushing_ypc + 
                              rushing_car + rushing_long + fumbles_fum + fumbles_rec +
                              fumbles_lost + max_speed_value + total_distance_value +
                              receiving_rec + receiving_yds + receiving_td + 
                              receiving_ypr + receiving_long + 
                              avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                              max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                              accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                              accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                              decel_1_5_value + decel_2_0_value + position, 
                            data = test)
  xg_model <- xgboost(data = train_mat,
                      label = train |> pull(waa_next_year),
                      nrounds = xg_tune_rb$bestTune$nrounds,
                      params = as.list(select(xg_tune_rb$bestTune, -nrounds)),
                      objective = "reg:squarederror",
                      verbosity = 0)
  test_rf_preds <- predict(rf_rb_model, test)$predictions
  test$rf_preds <- test_rf_preds
  test_xg_preds <- predict(xg_model, test_mat, type = "response")
  test$xg_preds <- test_xg_preds
  rb_oos_preds <- bind_rows(rb_oos_preds, test)
  print(paste("Hold Out Year", i, "RF LM Summary:"))
  print(summary(lm(waa_next_year ~ rf_preds, data = test)))
  print(paste("Hold Out Year", i, "PFF Projections LM Summary:"))
  print(summary(lm(waa_next_year ~ projected_waa, data = test)))
}
summary(lm(waa_next_year ~ rf_preds, data = rb_oos_preds, weights = snaps))
summary(lm(waa_next_year ~ xg_preds, data = rb_oos_preds, weights = snaps))
summary(lm(waa_next_year ~ projected_waa, data = rb_oos_preds, weights = snaps))

wr_model_data <- all_model_data |> 
  filter(position == "WR") |> 
  left_join(wr_stats |> select(year, athlete_id, rushing_car:fumbles_lost),
            by = join_by(athlete_id, season == year)) |> 
  inner_join(all_athleticism |> 
               select(player_id, season, max_speed_value, total_distance_value,
                      contains("speed"), contains("accel"), contains("decel")) |> 
               select(-closing_speed_on_target_value)) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

set.seed(123)
full_wr_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                              rating + tp_stars + weight + height + conference + waa +
                              rushing_car + rushing_yds + rushing_td + rushing_ypc + 
                              rushing_car + rushing_long + fumbles_fum + fumbles_rec +
                              fumbles_lost + max_speed_value + total_distance_value +
                              receiving_rec + receiving_yds + receiving_td + 
                              receiving_ypr + receiving_long + 
                              avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                              max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                              accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                              accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                              decel_1_5_value + decel_2_0_value, 
                            data = wr_model_data)
xg_grid <- crossing(nrounds = seq(50, 150, 10),
                    eta = c(0.01, 0.05, 0.1), gamma = 0,
                    max_depth = c(2, 3, 4, 5), colsample_bytree = 1,
                    min_child_weight = 1, subsample = 1)
xg_tune_wr <- train(x = full_wr_mat,
                    y = wr_model_data |> pull(waa_next_year),
                    tuneGrid = xg_grid,
                    trControl = trainControl(method = "cv", number = 5),
                    method = "xgbTree",
                    verbosity = 0)
wr_model <- xgboost(data = full_wr_mat,
                    label = wr_model_data |> pull(waa_next_year),
                    nrounds = xg_tune_wr$bestTune$nrounds,
                    params = as.list(select(xg_tune_wr$bestTune, -nrounds)),
                    objective = "reg:squarederror",
                    verbosity = 0)

wr_oos_preds <- data.frame()
for (i in 2021:2024){
  train <- wr_model_data |> filter(season != i)
  test <- wr_model_data |> filter(season == i)
  train_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                                rating + tp_stars + weight + height + conference + waa +
                                rushing_car + rushing_yds + rushing_td + rushing_ypc + 
                                rushing_car + rushing_long + fumbles_fum + fumbles_rec +
                                fumbles_lost + max_speed_value + total_distance_value +
                                receiving_rec + receiving_yds + receiving_td + 
                                receiving_ypr + receiving_long + 
                                avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                                max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                                accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                                accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                                decel_1_5_value + decel_2_0_value, 
                              data = train)
  test_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                              rating + tp_stars + weight + height + conference + waa +
                              rushing_car + rushing_yds + rushing_td + rushing_ypc + 
                              rushing_car + rushing_long + fumbles_fum + fumbles_rec +
                              fumbles_lost + max_speed_value + total_distance_value +
                              receiving_rec + receiving_yds + receiving_td + 
                              receiving_ypr + receiving_long + 
                              avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                              max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                              accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                              accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                              decel_1_5_value + decel_2_0_value, 
                            data = test)
  rf_wr_model <- ranger(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                          rating + tp_stars + weight + height + conference + waa +
                          rushing_car + rushing_yds + rushing_td + rushing_ypc + 
                          rushing_car + rushing_long + fumbles_fum + fumbles_rec +
                          fumbles_lost + max_speed_value + total_distance_value +
                          receiving_rec + receiving_yds + receiving_td + 
                          receiving_ypr + receiving_long + 
                          avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                          max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                          accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                          accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                          decel_1_5_value + decel_2_0_value, 
                        data = train, importance = "impurity")
  xg_model <- xgboost(data = train_mat,
                      label = train |> pull(waa_next_year),
                      nrounds = xg_tune_wr$bestTune$nrounds,
                      params = as.list(select(xg_tune_wr$bestTune, -nrounds)),
                      objective = "reg:squarederror",
                      verbosity = 0)
  test_preds <- predict(rf_wr_model, test)$predictions
  test$rf_preds <- test_preds
  test_xg_preds <- predict(xg_model, test_mat, type = "response")
  test$xg_preds <- test_xg_preds
  wr_oos_preds <- bind_rows(wr_oos_preds, test)
  # print(paste("Hold Out Year", i, "RF LM Summary:"))
  # print(summary(lm(waa_next_year ~ rf_preds, data = test)))
  # print(paste("Hold Out Year", i, "PFF Projections LM Summary:"))
  # print(summary(lm(waa_next_year ~ projected_waa, data = test)))
}
summary(lm(waa_next_year ~ rf_preds, data = wr_oos_preds, weights = snaps))
summary(lm(waa_next_year ~ xg_preds, data = wr_oos_preds, weights = snaps))
summary(lm(waa_next_year ~ projected_waa, data = wr_oos_preds, weights = snaps))
# vip(rf_wr_model, num_features = 50)

te_model_data <- all_model_data |> 
  filter(position == "TE") |> 
  left_join(te_stats |> select(year, athlete_id, rushing_car:fumbles_lost),
            by = join_by(athlete_id, season == year)) |> 
  inner_join(all_athleticism |> 
               select(player_id, season, max_speed_value, total_distance_value,
                      contains("speed"), contains("accel"), contains("decel"),
                      contains("get_off")) |> 
               select(-closing_speed_on_target_value)) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

full_te_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                              rating + tp_stars + weight + height + conference + waa +
                              rushing_car + rushing_yds + rushing_td + rushing_ypc + 
                              rushing_car + rushing_long + fumbles_fum + fumbles_rec +
                              fumbles_lost + max_speed_value + total_distance_value +
                              receiving_rec + receiving_yds + receiving_td + 
                              receiving_ypr + receiving_long + 
                              avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                              max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                              accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                              accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                              decel_1_5_value + decel_2_0_value + get_off_first_0_5_value +
                              get_off_first_1_0_value + get_off_first_1_5_value + 
                              get_off_first_2_0_value, 
                              data = te_model_data)
xg_grid <- crossing(nrounds = seq(50, 150, 10),
                    eta = c(0.01, 0.05, 0.1), gamma = 0,
                    max_depth = c(2, 3, 4, 5), colsample_bytree = 1,
                    min_child_weight = 1, subsample = 1)
xg_tune_te <- train(x = full_te_mat,
                    y = te_model_data |> pull(waa_next_year),
                    tuneGrid = xg_grid,
                    trControl = trainControl(method = "cv", number = 5),
                    method = "xgbTree",
                    verbosity = 0)

te_model <- xgboost(data = full_te_mat,
                    label = te_model_data |> pull(waa_next_year),
                    nrounds = xg_tune_te$bestTune$nrounds,
                    params = as.list(select(xg_tune_te$bestTune, -nrounds)),
                    objective = "reg:squarederror",
                    verbosity = 0)

te_oos_preds <- data.frame()
set.seed(123)
for (i in 2021:2024){
  train <- te_model_data |> filter(season != i)
  test <- te_model_data |> filter(season == i)
  train_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                                rating + tp_stars + weight + height + conference + waa +
                                rushing_car + rushing_yds + rushing_td + rushing_ypc + 
                                rushing_car + rushing_long + fumbles_fum + fumbles_rec +
                                fumbles_lost + max_speed_value + total_distance_value +
                                receiving_rec + receiving_yds + receiving_td + 
                                receiving_ypr + receiving_long + 
                                avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                                max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                                accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                                accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                                decel_1_5_value + decel_2_0_value + get_off_first_0_5_value +
                                get_off_first_1_0_value + get_off_first_1_5_value + 
                                get_off_first_2_0_value, 
                              data = train)
  test_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                              rating + tp_stars + weight + height + conference + waa +
                              rushing_car + rushing_yds + rushing_td + rushing_ypc + 
                              rushing_car + rushing_long + fumbles_fum + fumbles_rec +
                              fumbles_lost + max_speed_value + total_distance_value +
                              receiving_rec + receiving_yds + receiving_td + 
                              receiving_ypr + receiving_long + 
                              avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                              max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                              accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                              accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                              decel_1_5_value + decel_2_0_value + get_off_first_0_5_value +
                              get_off_first_1_0_value + get_off_first_1_5_value + 
                              get_off_first_2_0_value, 
                            data = test)
  rf_te_model <- ranger(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                          rating + tp_stars + weight + height + conference + waa +
                          rushing_car + rushing_yds + rushing_td + rushing_ypc + 
                          rushing_car + rushing_long + fumbles_fum + fumbles_rec +
                          fumbles_lost + max_speed_value + total_distance_value +
                          receiving_rec + receiving_yds + receiving_td + 
                          receiving_ypr + receiving_long + 
                          avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                          max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                          accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                          accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                          decel_1_5_value + decel_2_0_value + get_off_first_0_5_value +
                          get_off_first_1_0_value + get_off_first_1_5_value + 
                          get_off_first_2_0_value, 
                        data = train, importance = "impurity")
  xg_model <- xgboost(data = train_mat,
                      label = train |> pull(waa_next_year),
                      nrounds = xg_tune_te$bestTune$nrounds,
                      params = as.list(select(xg_tune_te$bestTune, -nrounds)),
                      objective = "reg:squarederror",
                      verbosity = 0)
  test_preds <- predict(rf_te_model, test)$predictions
  test$rf_preds <- test_preds
  test_xg_preds <- predict(xg_model, test_mat, type = "response")
  test$xg_preds <- test_xg_preds
  te_oos_preds <- bind_rows(te_oos_preds, test)
  # print(paste("Hold Out Year", i, "RF LM Summary:"))
  # print(summary(lm(waa_next_year ~ rf_preds, data = test)))
  # print(paste("Hold Out Year", i, "PFF Projections LM Summary:"))
  # print(summary(lm(waa_next_year ~ projected_waa, data = test)))
}
summary(lm(waa_next_year ~ rf_preds, data = te_oos_preds, weights = snaps))
summary(lm(waa_next_year ~ xg_preds, data = te_oos_preds, weights = snaps))
summary(lm(waa_next_year ~ projected_waa, data = te_oos_preds, weights = snaps))
# vip(rf_te_model, num_features = 50)

ol_model_data <- all_model_data |> 
  filter(position %in% c("C", "T", "G")) |> 
  inner_join(all_athleticism |> 
               select(player_id, season, max_speed_value, total_distance_value,
                      contains("speed"), contains("accel"), contains("decel"),
                      contains("get_off")) |> 
               select(-closing_speed_on_target_value)) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

full_ol_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                              rating + tp_stars + weight + height + conference + waa +
                              max_speed_value + total_distance_value +
                              avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                              max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                              accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                              accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                              decel_1_5_value + decel_2_0_value + get_off_first_0_5_value +
                              get_off_first_1_0_value + get_off_first_1_5_value + 
                              get_off_first_2_0_value,
                            data = ol_model_data)

xg_grid <- crossing(nrounds = seq(50, 150, 10),
                    eta = c(0.01, 0.05, 0.1), gamma = 0,
                    max_depth = c(2, 3, 4, 5), colsample_bytree = 1,
                    min_child_weight = 1, subsample = 1)
xg_tune_ol <- train(x = full_ol_mat,
                    y = ol_model_data |> pull(waa_next_year),
                    tuneGrid = xg_grid,
                    trControl = trainControl(method = "cv", number = 5),
                    method = "xgbTree",
                    verbosity = 0)

ol_model <- xgboost(data = full_ol_mat,
                    label = ol_model_data |> pull(waa_next_year),
                    nrounds = xg_tune_ol$bestTune$nrounds,
                    params = as.list(select(xg_tune_ol$bestTune, -nrounds)),
                    objective = "reg:squarederror",
                    verbosity = 0)

ol_oos_preds <- data.frame()
set.seed(123)
for (i in 2021:2024){
  train <- ol_model_data |> filter(season != i)
  test <- ol_model_data |> filter(season == i)
  train_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                                rating + tp_stars + weight + height + conference + waa +
                                max_speed_value + total_distance_value +
                                avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                                max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                                accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                                accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                                decel_1_5_value + decel_2_0_value + get_off_first_0_5_value +
                                get_off_first_1_0_value + get_off_first_1_5_value + 
                                get_off_first_2_0_value,
                              data = train)
  test_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                              rating + tp_stars + weight + height + conference + waa +
                              max_speed_value + total_distance_value +
                              avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                              max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                              accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                              accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                              decel_1_5_value + decel_2_0_value + get_off_first_0_5_value +
                              get_off_first_1_0_value + get_off_first_1_5_value + 
                              get_off_first_2_0_value,
                            data = test)
  rf_ol_model <- ranger(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                          rating + tp_stars + weight + height + conference + waa +
                          max_speed_value + total_distance_value +
                          avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                          max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                          accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                          accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                          decel_1_5_value + decel_2_0_value + get_off_first_0_5_value +
                          get_off_first_1_0_value + get_off_first_1_5_value + 
                          get_off_first_2_0_value, 
                        data = train, importance = "impurity")
  xg_model <- xgboost(data = train_mat,
                      label = train |> pull(waa_next_year),
                      nrounds = xg_tune_ol$bestTune$nrounds,
                      params = as.list(select(xg_tune_ol$bestTune, -nrounds)),
                      objective = "reg:squarederror",
                      verbosity = 0)
  test_rf_preds <- predict(rf_ol_model, test)$predictions
  test$rf_preds <- test_rf_preds
  test_xg_preds <- predict(xg_model, test_mat, type = "response")
  test$xg_preds <- test_xg_preds
  ol_oos_preds <- bind_rows(ol_oos_preds, test)
  # print(paste("Hold Out Year", i, "RF LM Summary:"))
  # print(summary(lm(waa_next_year ~ rf_preds, data = test)))
  # print(paste("Hold Out Year", i, "PFF Projections LM Summary:"))
  # print(summary(lm(waa_next_year ~ projected_waa, data = test)))
}
summary(lm(waa_next_year ~ rf_preds, data = ol_oos_preds, weights = snaps))
summary(lm(waa_next_year ~ xg_preds, data = ol_oos_preds, weights = snaps))
summary(lm(waa_next_year ~ projected_waa, data = ol_oos_preds, weights = snaps))
# vip(rf_ol_model, num_features = 50)

cb_model_data <- all_model_data |> 
  filter(position == "CB") |> 
  left_join(defensive_stats |> 
               select(year, athlete_id, defensive_solo:defensive_td),
            by = join_by(athlete_id, season == year)) |> 
  inner_join(all_athleticism |> 
               select(player_id, season, max_speed_value, total_distance_value,
                      contains("speed"), contains("accel"), contains("decel"),
                      contains("get_off"))) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

full_cb_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                              rating + tp_stars + weight + height + conference + waa +
                              max_speed_value + total_distance_value +
                              avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                              max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                              accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                              accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                              decel_1_5_value + decel_2_0_value + closing_speed_on_target_value,
                              # defensive_solo + defensive_tot + defensive_tfl + 
                              # defensive_sacks + defensive_qb_hur + interceptions_int +
                              # interceptions_yds + interceptions_td + defensive_pd +
                              # defensive_td,
                            data = cb_model_data)

xg_tune_cb <- train(x = full_cb_mat,
                    y = cb_model_data |> pull(waa_next_year),
                    tuneGrid = xg_grid,
                    trControl = trainControl(method = "cv", number = 5),
                    method = "xgbTree",
                    verbosity = 0)

cb_model <- xgboost(data = full_cb_mat,
                    label = cb_model_data |> pull(waa_next_year),
                    nrounds = xg_tune_cb$bestTune$nrounds,
                    params = as.list(select(xg_tune_cb$bestTune, -nrounds)),
                    objective = "reg:squarederror",
                    verbosity = 0)

cb_oos_preds <- data.frame()
set.seed(123)
for (i in 2021:2024){
  train <- cb_model_data |> filter(season != i)
  test <- cb_model_data |> filter(season == i)
  train_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                              rating + tp_stars + weight + height + conference + waa +
                              max_speed_value + total_distance_value +
                              avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                              max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                              accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                              accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                              decel_1_5_value + decel_2_0_value + closing_speed_on_target_value,
                              # defensive_solo + defensive_tot + 
                              # defensive_tfl + defensive_sacks + defensive_qb_hur + 
                              # interceptions_int +
                              # interceptions_yds + interceptions_td + defensive_pd +
                              # defensive_td,
                           data = train)
  test_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                             rating + tp_stars + weight + height + conference + waa +
                             max_speed_value + total_distance_value +
                             avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                             max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                             accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                             accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                             decel_1_5_value + decel_2_0_value + closing_speed_on_target_value,
                             # defensive_solo + defensive_tot + 
                             # defensive_tfl + defensive_sacks + defensive_qb_hur + 
                             # interceptions_int +
                             # interceptions_yds + interceptions_td + defensive_pd +
                             # defensive_td,
                            data = test)
  rf_cb_model <- ranger(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                          rating + tp_stars + weight + height + conference + waa +
                          max_speed_value + total_distance_value +
                          avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                          max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                          accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                          accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                          decel_1_5_value + decel_2_0_value + closing_speed_on_target_value,
                          # defensive_solo + defensive_tot + 
                          # defensive_tfl + defensive_sacks + defensive_qb_hur + 
                          # interceptions_int +
                          # interceptions_yds + interceptions_td + defensive_pd +
                          # defensive_td, 
                        data = train, importance = "impurity")
  xg_model <- xgboost(data = train_mat,
                      label = train |> pull(waa_next_year),
                      nrounds = xg_tune_cb$bestTune$nrounds,
                      params = as.list(select(xg_tune_cb$bestTune, -nrounds)),
                      objective = "reg:squarederror",
                      verbosity = 0)
  test_rf_preds <- predict(rf_cb_model, test)$predictions
  test$rf_preds <- test_rf_preds
  test_xg_preds <- predict(xg_model, test_mat, type = "response")
  test$xg_preds <- test_xg_preds
  cb_oos_preds <- bind_rows(cb_oos_preds, test)
  # print(paste("Hold Out Year", i, "RF LM Summary:"))
  # print(summary(lm(waa_next_year ~ rf_preds, data = test)))
  # print(paste("Hold Out Year", i, "PFF Projections LM Summary:"))
  # print(summary(lm(waa_next_year ~ projected_waa, data = test)))
}
# beating PFF
summary(lm(waa_next_year ~ rf_preds, data = cb_oos_preds, weights = snaps))
summary(lm(waa_next_year ~ xg_preds, data = cb_oos_preds, weights = snaps))
summary(lm(waa_next_year ~ projected_waa, data = cb_oos_preds, weights = snaps))
# vip(rf_ol_model, num_features = 50)


s_model_data <- all_model_data |> 
  filter(position == "S") |> 
  left_join(defensive_stats |> 
              select(year, athlete_id, defensive_solo:defensive_td),
            by = join_by(athlete_id, season == year)) |> 
  inner_join(all_athleticism |> 
               select(player_id, season, max_speed_value, total_distance_value,
                      contains("speed"), contains("accel"), contains("decel"),
                      contains("get_off"))) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

full_s_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                             rating + tp_stars + weight + height + conference + waa +
                             max_speed_value + total_distance_value +
                             avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                             max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                             accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                             accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                             decel_1_5_value + decel_2_0_value + closing_speed_on_target_value +
                             defensive_solo + defensive_tot +
                             defensive_tfl + defensive_sacks + defensive_qb_hur +
                             interceptions_int +
                             interceptions_yds + interceptions_td + defensive_pd +
                             defensive_td,
                           data = s_model_data)

xg_tune_s <- train(x = full_s_mat,
                    y = s_model_data |> pull(waa_next_year),
                    tuneGrid = xg_grid,
                    trControl = trainControl(method = "cv", number = 5),
                    method = "xgbTree",
                    verbosity = 0)

s_model <- xgboost(data = full_s_mat,
                    label = s_model_data |> pull(waa_next_year),
                    nrounds = xg_tune_s$bestTune$nrounds,
                    params = as.list(select(xg_tune_s$bestTune, -nrounds)),
                    objective = "reg:squarederror",
                    verbosity = 0)

s_oos_preds <- data.frame()
set.seed(123)
for (i in 2021:2024){
  train <- s_model_data |> filter(season != i)
  test <- s_model_data |> filter(season == i)
  train_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                              rating + tp_stars + weight + height + conference + waa +
                              max_speed_value + total_distance_value +
                              avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                              max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                              accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                              accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                              decel_1_5_value + decel_2_0_value + closing_speed_on_target_value +
                              defensive_solo + defensive_tot +
                              defensive_tfl + defensive_sacks + defensive_qb_hur +
                              interceptions_int +
                              interceptions_yds + interceptions_td + defensive_pd +
                              defensive_td,
                            data = train)
  test_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                             rating + tp_stars + weight + height + conference + waa +
                             max_speed_value + total_distance_value +
                             avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                             max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                             accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                             accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                             decel_1_5_value + decel_2_0_value + closing_speed_on_target_value +
                             defensive_solo + defensive_tot +
                             defensive_tfl + defensive_sacks + defensive_qb_hur +
                             interceptions_int +
                             interceptions_yds + interceptions_td + defensive_pd +
                             defensive_td,
                           data = test)
  rf_s_model <- ranger(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                         rating + tp_stars + weight + height + conference + waa +
                         max_speed_value + total_distance_value +
                         avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                         max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                         accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                         accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                         decel_1_5_value + decel_2_0_value + closing_speed_on_target_value +
                         defensive_solo + defensive_tot +
                         defensive_tfl + defensive_sacks + defensive_qb_hur +
                         interceptions_int +
                         interceptions_yds + interceptions_td + defensive_pd +
                         defensive_td,
                       data = train, importance = "impurity")
  xg_model <- xgboost(data = train_mat,
                      label = train |> pull(waa_next_year),
                      nrounds = xg_tune_s$bestTune$nrounds,
                      params = as.list(select(xg_tune_s$bestTune, -nrounds)),
                      objective = "reg:squarederror",
                      verbosity = 0)
  test_rf_preds <- predict(rf_s_model, test)$predictions
  test$rf_preds <- test_rf_preds
  test_xg_preds <- predict(xg_model, test_mat, type = "response")
  test$xg_preds <- test_xg_preds
  s_oos_preds <- bind_rows(s_oos_preds, test)
  # print(paste("Hold Out Year", i, "RF LM Summary:"))
  # print(summary(lm(waa_next_year ~ rf_preds, data = test)))
  # print(paste("Hold Out Year", i, "PFF Projections LM Summary:"))
  # print(summary(lm(waa_next_year ~ projected_waa, data = test)))
}
# BEATING PFF AGAIN 
summary(lm(waa_next_year ~ rf_preds, data = s_oos_preds, weights = snaps))
summary(lm(waa_next_year ~ xg_preds, data = s_oos_preds, weights = snaps))
summary(lm(waa_next_year ~ projected_waa, data = s_oos_preds, weights = snaps))

lb_model_data <- all_model_data |> 
  filter(position == "LB") |> 
  left_join(defensive_stats |> 
              select(year, athlete_id, defensive_solo:defensive_td),
            by = join_by(athlete_id, season == year)) |> 
  inner_join(all_athleticism |> 
               select(player_id, season, max_speed_value, total_distance_value,
                      contains("speed"), contains("accel"), contains("decel"),
                      contains("get_off"))) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

full_lb_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                              rating + tp_stars + weight + height + conference + waa +
                              max_speed_value + total_distance_value +
                              avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                              max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                              accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                              accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                              decel_1_5_value + decel_2_0_value + closing_speed_on_target_value,
                              # defensive_solo + defensive_tot +
                              # defensive_tfl + defensive_sacks + defensive_qb_hur +
                              # interceptions_int +
                              # interceptions_yds + interceptions_td + defensive_pd +
                              # defensive_td,
                           data = lb_model_data)

xg_tune_lb <- train(x = full_lb_mat,
                   y = lb_model_data |> pull(waa_next_year),
                   tuneGrid = xg_grid,
                   trControl = trainControl(method = "cv", number = 5),
                   method = "xgbTree",
                   verbosity = 0)

lb_model <- ranger(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                        rating + tp_stars + weight + height + conference + waa +
                        max_speed_value + total_distance_value +
                        avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                        max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                        accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                        accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                        decel_1_5_value + decel_2_0_value + closing_speed_on_target_value,
                      # defensive_solo + defensive_tot +
                      # defensive_tfl + defensive_sacks + defensive_qb_hur +
                      # interceptions_int +
                      # interceptions_yds + interceptions_td + defensive_pd +
                      # defensive_td, 
                      data = lb_model_data, importance = "impurity")

lb_oos_preds <- data.frame()
set.seed(123)
for (i in 2021:2024){
  train <- lb_model_data |> filter(season != i)
  test <- lb_model_data |> filter(season == i)
  train_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                              rating + tp_stars + weight + height + conference + waa +
                              max_speed_value + total_distance_value +
                              avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                              max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                              accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                              accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                              decel_1_5_value + decel_2_0_value + closing_speed_on_target_value,
                              # defensive_solo + defensive_tot +
                              # defensive_tfl + defensive_sacks + defensive_qb_hur +
                              # interceptions_int +
                              # interceptions_yds + interceptions_td + defensive_pd +
                              # defensive_td,
                            data = train)
  test_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                             rating + tp_stars + weight + height + conference + waa +
                             max_speed_value + total_distance_value +
                             avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                             max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                             accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                             accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                             decel_1_5_value + decel_2_0_value + closing_speed_on_target_value,
                             # defensive_solo + defensive_tot +
                             # defensive_tfl + defensive_sacks + defensive_qb_hur +
                             # interceptions_int +
                             # interceptions_yds + interceptions_td + defensive_pd +
                             # defensive_td,
                           data = test)
  rf_lb_model <- ranger(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                          rating + tp_stars + weight + height + conference + waa +
                          max_speed_value + total_distance_value +
                          avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                          max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                          accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                          accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                          decel_1_5_value + decel_2_0_value + closing_speed_on_target_value,
                          # defensive_solo + defensive_tot +
                          # defensive_tfl + defensive_sacks + defensive_qb_hur +
                          # interceptions_int +
                          # interceptions_yds + interceptions_td + defensive_pd +
                          # defensive_td, 
                       data = train, importance = "impurity")
  xg_model <- xgboost(data = train_mat,
                      label = train |> pull(waa_next_year),
                      nrounds = xg_tune_lb$bestTune$nrounds,
                      params = as.list(select(xg_tune_lb$bestTune, -nrounds)),
                      objective = "reg:squarederror",
                      verbosity = 0)
  test_rf_preds <- predict(rf_lb_model, test)$predictions
  test$rf_preds <- test_rf_preds
  test_xg_preds <- predict(xg_model, test_mat, type = "response")
  test$xg_preds <- test_xg_preds
  lb_oos_preds <- bind_rows(lb_oos_preds, test)
  # print(paste("Hold Out Year", i, "RF LM Summary:"))
  # print(summary(lm(waa_next_year ~ rf_preds, data = test)))
  # print(paste("Hold Out Year", i, "PFF Projections LM Summary:"))
  # print(summary(lm(waa_next_year ~ projected_waa, data = test)))
}
# BEATING PFF AGAIN 
summary(lm(waa_next_year ~ rf_preds, data = lb_oos_preds, weights = snaps))
summary(lm(waa_next_year ~ xg_preds, data = lb_oos_preds, weights = snaps))
summary(lm(waa_next_year ~ projected_waa, data = lb_oos_preds, weights = snaps))

edge_model_data <- all_model_data |> 
  filter(position == "ED") |> 
  left_join(defensive_stats |> 
              select(year, athlete_id, defensive_solo:defensive_td),
            by = join_by(athlete_id, season == year)) |> 
  inner_join(all_athleticism |> 
               select(player_id, season, max_speed_value, total_distance_value,
                      contains("speed"), contains("accel"), contains("decel"),
                      contains("get_off"))) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

full_edge_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                                rating + tp_stars + weight + height + conference + waa +
                                max_speed_value + total_distance_value +
                                avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                                max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                                accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                                accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                                decel_1_5_value + decel_2_0_value + closing_speed_on_target_value +
                                get_off_first_0_5_value + get_off_first_1_0_value +
                                get_off_first_1_5_value + get_off_first_2_0_value,
                              # defensive_solo + defensive_tot +
                              # defensive_tfl + defensive_sacks + defensive_qb_hur +
                              # interceptions_int +
                              # interceptions_yds + interceptions_td + defensive_pd +
                              # defensive_td,
                            data = edge_model_data)

xg_tune_edge <- train(x = full_edge_mat,
                    y = edge_model_data |> pull(waa_next_year),
                    tuneGrid = xg_grid,
                    trControl = trainControl(method = "cv", number = 5),
                    method = "xgbTree",
                    verbosity = 0)

edge_model <- xgboost(data = full_edge_mat,
                    label = edge_model_data |> pull(waa_next_year),
                    nrounds = xg_tune_edge$bestTune$nrounds,
                    params = as.list(select(xg_tune_edge$bestTune, -nrounds)),
                    objective = "reg:squarederror",
                    verbosity = 0)

edge_oos_preds <- data.frame()
set.seed(123)
for (i in 2021:2024){
  train <- edge_model_data |> filter(season != i)
  test <- edge_model_data |> filter(season == i)
  train_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                              rating + tp_stars + weight + height + conference + waa +
                              max_speed_value + total_distance_value +
                              avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                              max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                              accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                              accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                              decel_1_5_value + decel_2_0_value + closing_speed_on_target_value +
                              get_off_first_0_5_value + get_off_first_1_0_value +
                              get_off_first_1_5_value + get_off_first_2_0_value,
                            # defensive_solo + defensive_tot +
                            # defensive_tfl + defensive_sacks + defensive_qb_hur +
                            # interceptions_int +
                            # interceptions_yds + interceptions_td + defensive_pd +
                            # defensive_td,
                            data = train)
  test_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                             rating + tp_stars + weight + height + conference + waa +
                             max_speed_value + total_distance_value +
                             avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                             max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                             accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                             accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                             decel_1_5_value + decel_2_0_value + closing_speed_on_target_value +
                             get_off_first_0_5_value + get_off_first_1_0_value +
                             get_off_first_1_5_value + get_off_first_2_0_value,
                             # defensive_solo + defensive_tot +
                             # defensive_tfl + defensive_sacks + defensive_qb_hur +
                             # interceptions_int +
                             # interceptions_yds + interceptions_td + defensive_pd +
                             # defensive_td,
                           data = test)
  rf_edge_model <- ranger(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                          rating + tp_stars + weight + height + conference + waa +
                          max_speed_value + total_distance_value +
                          avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                          max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                          accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                          accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                          decel_1_5_value + decel_2_0_value + closing_speed_on_target_value +
                          get_off_first_0_5_value + get_off_first_1_0_value +
                          get_off_first_1_5_value + get_off_first_2_0_value,
                        # defensive_solo + defensive_tot +
                        # defensive_tfl + defensive_sacks + defensive_qb_hur +
                        # interceptions_int +
                        # interceptions_yds + interceptions_td + defensive_pd +
                        # defensive_td,
                        data = train, importance = "impurity")
  xg_model <- xgboost(data = train_mat,
                      label = train |> pull(waa_next_year),
                      nrounds = xg_tune_edge$bestTune$nrounds,
                      params = as.list(select(xg_tune_edge$bestTune, -nrounds)),
                      objective = "reg:squarederror",
                      verbosity = 0)
  test_rf_preds <- predict(rf_edge_model, test)$predictions
  test$rf_preds <- test_rf_preds
  test_xg_preds <- predict(xg_model, test_mat, type = "response")
  test$xg_preds <- test_xg_preds
  edge_oos_preds <- bind_rows(edge_oos_preds, test)
  # print(paste("Hold Out Year", i, "RF LM Summary:"))
  # print(summary(lm(waa_next_year ~ rf_preds, data = test)))
  # print(paste("Hold Out Year", i, "PFF Projections LM Summary:"))
  # print(summary(lm(waa_next_year ~ projected_waa, data = test)))
}

summary(lm(waa_next_year ~ rf_preds, data = edge_oos_preds, weights = snaps))
summary(lm(waa_next_year ~ xg_preds, data = edge_oos_preds, weights = snaps))
summary(lm(waa_next_year ~ projected_waa, data = edge_oos_preds, weights = snaps))

dl_model_data <- all_model_data |> 
  filter(position == "DI") |> 
  left_join(defensive_stats |> 
              select(year, athlete_id, defensive_solo:defensive_td),
            by = join_by(athlete_id, season == year)) |> 
  inner_join(all_athleticism |> 
               select(player_id, season, max_speed_value, total_distance_value,
                      contains("speed"), contains("accel"), contains("decel"),
                      contains("get_off"))) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

full_dl_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                                rating + tp_stars + weight + height + conference + waa +
                                max_speed_value + total_distance_value +
                                avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                                max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                                accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                                accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                                decel_1_5_value + decel_2_0_value + closing_speed_on_target_value +
                                get_off_first_0_5_value + get_off_first_1_0_value +
                                get_off_first_1_5_value + get_off_first_2_0_value + 
                              defensive_solo + defensive_tot +
                              defensive_tfl + defensive_sacks + defensive_qb_hur +
                              interceptions_int +
                              interceptions_yds + interceptions_td + defensive_pd +
                              defensive_td,
                              data = dl_model_data)

xg_tune_dl <- train(x = full_dl_mat,
                      y = dl_model_data |> pull(waa_next_year),
                      tuneGrid = xg_grid,
                      trControl = trainControl(method = "cv", number = 5),
                      method = "xgbTree",
                      verbosity = 0)

dl_model <- ranger(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                        rating + tp_stars + weight + height + conference + waa +
                        max_speed_value + total_distance_value +
                        avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                        max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                        accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                        accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                        decel_1_5_value + decel_2_0_value + closing_speed_on_target_value +
                        get_off_first_0_5_value + get_off_first_1_0_value +
                        get_off_first_1_5_value + get_off_first_2_0_value + 
                        defensive_solo + defensive_tot +
                        defensive_tfl + defensive_sacks + defensive_qb_hur +
                        interceptions_int +
                        interceptions_yds + interceptions_td + defensive_pd +
                        defensive_td,
                      data = dl_model_data, importance = "impurity")

dl_oos_preds <- data.frame()
set.seed(123)
for (i in 2021:2024){
  train <- dl_model_data |> filter(season != i)
  test <- dl_model_data |> filter(season == i)
  train_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                              rating + tp_stars + weight + height + conference + waa +
                              max_speed_value + total_distance_value +
                              avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                              max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                              accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                              accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                              decel_1_5_value + decel_2_0_value + closing_speed_on_target_value +
                              get_off_first_0_5_value + get_off_first_1_0_value +
                              get_off_first_1_5_value + get_off_first_2_0_value + 
                              defensive_solo + defensive_tot +
                              defensive_tfl + defensive_sacks + defensive_qb_hur +
                              interceptions_int +
                              interceptions_yds + interceptions_td + defensive_pd +
                              defensive_td,
                            data = train)
  test_mat <- model.matrix(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                             rating + tp_stars + weight + height + conference + waa +
                             max_speed_value + total_distance_value +
                             avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                             max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                             accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                             accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                             decel_1_5_value + decel_2_0_value + closing_speed_on_target_value +
                             get_off_first_0_5_value + get_off_first_1_0_value +
                             get_off_first_1_5_value + get_off_first_2_0_value + 
                             defensive_solo + defensive_tot +
                             defensive_tfl + defensive_sacks + defensive_qb_hur +
                             interceptions_int +
                             interceptions_yds + interceptions_td + defensive_pd +
                             defensive_td,
                           data = test)
  rf_dl_model <- ranger(waa_next_year ~ snaps + year_in_league + ranking + hs_stars +
                          rating + tp_stars + weight + height + conference + waa +
                          max_speed_value + total_distance_value +
                          avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                          max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                          accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                          accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                          decel_1_5_value + decel_2_0_value + closing_speed_on_target_value +
                          get_off_first_0_5_value + get_off_first_1_0_value +
                          get_off_first_1_5_value + get_off_first_2_0_value + 
                          defensive_solo + defensive_tot +
                          defensive_tfl + defensive_sacks + defensive_qb_hur +
                          interceptions_int +
                          interceptions_yds + interceptions_td + defensive_pd +
                          defensive_td,
                        data = train, importance = "impurity")
  xg_model <- xgboost(data = train_mat,
                      label = train |> pull(waa_next_year),
                      nrounds = xg_tune_dl$bestTune$nrounds,
                      params = as.list(select(xg_tune_dl$bestTune, -nrounds)),
                      objective = "reg:squarederror",
                      verbosity = 0)
  test_rf_preds <- predict(rf_dl_model, test)$predictions
  test$rf_preds <- test_rf_preds
  test_xg_preds <- predict(xg_model, test_mat, type = "response")
  test$xg_preds <- test_xg_preds
  dl_oos_preds <- bind_rows(dl_oos_preds, test)
  # print(paste("Hold Out Year", i, "RF LM Summary:"))
  # print(summary(lm(waa_next_year ~ rf_preds, data = test)))
  # print(paste("Hold Out Year", i, "PFF Projections LM Summary:"))
  # print(summary(lm(waa_next_year ~ projected_waa, data = test)))
}
# BEATING PFF AGAIN
summary(lm(waa_next_year ~ rf_preds, data = dl_oos_preds, weights = snaps))
summary(lm(waa_next_year ~ xg_preds, data = dl_oos_preds, weights = snaps))
summary(lm(waa_next_year ~ projected_waa, data = dl_oos_preds, weights = snaps))

all_oos_preds <- bind_rows(
    qb_oos_preds |> select(athlete_id:team, projected_waa, waa_next_year, xg_preds),
    wr_oos_preds |> select(athlete_id:team, projected_waa, waa_next_year, xg_preds),
    te_oos_preds |> select(athlete_id:team, projected_waa, waa_next_year, xg_preds),
    ol_oos_preds |> select(athlete_id:team, projected_waa, waa_next_year, xg_preds),
    cb_oos_preds |> select(athlete_id:team, projected_waa, waa_next_year, xg_preds),
    s_oos_preds |> select(athlete_id:team, projected_waa, waa_next_year, xg_preds),
    edge_oos_preds |> select(athlete_id:team, projected_waa, waa_next_year, xg_preds),
    rb_oos_preds |> select(athlete_id:team, projected_waa, waa_next_year, rf_preds),
    lb_oos_preds |> select(athlete_id:team, projected_waa, waa_next_year, rf_preds),
    dl_oos_preds |> select(athlete_id:team, projected_waa, waa_next_year, rf_preds)
  ) |> 
  mutate(preds = ifelse(is.na(xg_preds), rf_preds, xg_preds))

summary(lm(waa_next_year ~ projected_waa, data = all_oos_preds, weights = snaps))
summary(lm(waa_next_year ~ preds, data = all_oos_preds, weights = snaps))


preds_by_team <- all_oos_preds |> 
  group_by(team, season) |> 
  summarise(pred_war = sum(preds)) |> 
  ungroup()

record_and_preds <- all_record_and_war |> 
  inner_join(preds_by_team |> mutate(year = season + 1), 
             by = join_by(ultimate_name == team, year))

summary(lm(wins ~ proj_war, data = record_and_preds))
summary(lm(wins ~ pred_war, data = record_and_preds))


# vi(rf_qb_model) |> 
#   slice_head(n = 10) |> 
#   arrange(desc(Importance)) |> 
#   ggplot(aes(reorder(Variable, Importance), Importance)) +
#   geom_col(fill = "darkblue") +
#   coord_flip() +
#   scale_x_discrete(labels = c(
#     waa = "WAA",
#     passing_yds = "Passing Yards",
#     snaps = "Total Snaps",
#     passing_td = "Passing Touchdowns",
#     passing_completions = "Completions",
#     rushing_ypc = "Yards per carry",
#     passing_pct = "Completion %",
#     passing_att = "Passing Attempts",
#     passing_ypa = "Yards per pass attempt",
#     rushing_yds = "Rush Yards"
#   )) +
#   labs(x = "Variable", title = "Feature Importance Plot", subtitle = "QB WAA Projection Model") +
#   ggthemes::theme_clean()
# 
# vi(rf_rb_model) |> 
#   slice_head(n = 10) |> 
#   arrange(desc(Importance)) |> 
#   ggplot(aes(reorder(Variable, Importance), Importance)) +
#   geom_col(fill = "darkred") +
#   coord_flip() +
#   scale_x_discrete(labels = c(
#     waa = "WAA",
#     passing_yds = "Passing Yards",
#     snaps = "Total Snaps",
#     passing_td = "Passing Touchdowns",
#     passing_completions = "Completions",
#     rushing_ypc = "Yards per carry",
#     passing_pct = "Completion %",
#     passing_att = "Passing Attempts",
#     passing_ypa = "Yards per pass attempt",
#     rushing_yds = "Rush Yards",
#     ranking = "HS Recruit Ranking",
#     receiving_yds = "Receiving Yards",
#     total_distance_value = "Total Distance Covered/Game",
#     receiving_rec = "Receptions",
#     rushing_car = "Carries",
#     accel_1_5_value = "Acceleration (0-1.5 secs)",
#     max_speed_10_20_yards_value = "Max Speed (10-20 yards)"
#   )) +
#   labs(x = "Variable", title = "Feature Importance Plot", subtitle = "RB WAA Projection Model") +
#   ggthemes::theme_clean()
# 
# vi(rf_wr_model) |> 
#   slice_head(n = 10) |> 
#   arrange(desc(Importance)) |> 
#   ggplot(aes(reorder(Variable, Importance), Importance)) +
#   geom_col(fill = "purple4") +
#   coord_flip() +
#   scale_x_discrete(labels = c(
#     waa = "WAA",
#     passing_yds = "Passing Yards",
#     snaps = "Total Snaps",
#     passing_td = "Passing Touchdowns",
#     passing_completions = "Completions",
#     rushing_ypc = "Yards per carry",
#     passing_pct = "Completion %",
#     passing_att = "Passing Attempts",
#     passing_ypa = "Yards per pass attempt",
#     rushing_yds = "Rush Yards",
#     ranking = "HS Recruit Ranking",
#     receiving_yds = "Receiving Yards",
#     total_distance_value = "Total Distance Covered/Game",
#     receiving_rec = "Receptions",
#     rushing_car = "Carries",
#     accel_1_5_value = "Acceleration (0-1.5 secs)",
#     max_speed_10_20_yards_value = "Max Speed (10-20 yards)",
#     decel_0_5_value = "Deceleration (0-0.5 secs)",
#     accel_0_5_value = "Acceleration (0-0.5 secs)",
#     decel_2_0_value = "Deceleration (0-2 secs)",
#     decel_1_0_value = "Deceleration (0-1 secs)",
#     max_speed_0_10_yards_value = "Max Speed (0-10 yards)"
#   )) +
#   labs(x = "Variable", title = "Feature Importance Plot", subtitle = "WR WAA Projection Model") +
#   ggthemes::theme_clean()
# 
# vi(rf_te_model) |> 
#   slice_head(n = 10) |> 
#   arrange(desc(Importance)) |> 
#   ggplot(aes(reorder(Variable, Importance), Importance)) +
#   geom_col(fill = "darkorange") +
#   coord_flip() +
#   scale_x_discrete(labels = c(
#     waa = "WAA",
#     passing_yds = "Passing Yards",
#     snaps = "Total Snaps",
#     passing_td = "Passing Touchdowns",
#     passing_completions = "Completions",
#     rushing_ypc = "Yards per carry",
#     passing_pct = "Completion %",
#     passing_att = "Passing Attempts",
#     passing_ypa = "Yards per pass attempt",
#     rushing_yds = "Rush Yards",
#     ranking = "HS Recruit Ranking",
#     receiving_yds = "Receiving Yards",
#     total_distance_value = "Total Distance Covered/Game",
#     receiving_rec = "Receptions",
#     rushing_car = "Carries",
#     accel_1_5_value = "Acceleration (0-1.5 secs)",
#     max_speed_10_20_yards_value = "Max Speed (10-20 yards)",
#     decel_0_5_value = "Deceleration (0-0.5 secs)",
#     accel_0_5_value = "Acceleration (0-0.5 secs)",
#     decel_2_0_value = "Deceleration (0-2 secs)",
#     decel_1_0_value = "Deceleration (0-1 secs)",
#     max_speed_0_10_yards_value = "Max Speed (0-10 yards)",
#     receiving_td = "Receiving Touchdowns",
#     max_speed_value = "Overall Max Speed",
#     receiving_long = "Longest Reception",
#     get_off_first_1_5_value = "Get-Off Speed (0-1.5 seconds)"
#   )) +
#   labs(x = "Variable", title = "Feature Importance Plot", subtitle = "TE WAA Projection Model") +
#   ggthemes::theme_clean()
