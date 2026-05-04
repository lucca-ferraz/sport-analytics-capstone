portal_26 <- cfbd_recruiting_transfer_portal(year = 2026)

current_data <- war25 |> 
  inner_join(projected_war_26 |> select(projected_waa, player_id)) |> 
  filter(!(position %in% c("K", "LS", "P"))) |> 
  inner_join(team_codes |> select(team_code, fastr_name)) |> 
  left_join(all_recruiting |> 
              select(athlete_id, name, ranking, hs_stars = stars, rating), 
            by = join_by(athlete_id, player == name)) |> 
  left_join(portal_26 |> 
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
         team, ranking:conference, waa, 
         projected_waa)

qb_current_data <- current_data |> 
  filter(position == "QB") |> 
  left_join(qb_stats |> select(year, athlete_id, passing_completions:fumbles_lost),
            by = join_by(athlete_id, season == year)) |> 
  inner_join(all_athleticism |> 
               select(player_id, season, max_speed_value, total_distance_value,
                      contains("speed"), contains("accel"), contains("decel")) |> 
               select(-closing_speed_on_target_value)) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

qb_current_mat <- model.matrix( ~ 
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
                            data = qb_current_data)

qb_preds <- predict(qb_model, qb_current_mat, type = "response")
qb_current_data$predicted_waa <- qb_preds
qb_current_data |> arrange(desc(predicted_waa)) |> View()


wr_current_data <- current_data |> 
  filter(position == "WR") |> 
  left_join(wr_stats |> select(year, athlete_id, rushing_car:fumbles_lost),
            by = join_by(athlete_id, season == year)) |> 
  inner_join(all_athleticism |> 
               select(player_id, season, max_speed_value, total_distance_value,
                      contains("speed"), contains("accel"), contains("decel")) |> 
               select(-closing_speed_on_target_value)) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

wr_current_mat <- model.matrix( ~ snaps + year_in_league + ranking + hs_stars +
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
                            data = wr_current_data)

wr_preds <- predict(wr_model, wr_current_mat, type = "response")
wr_current_data$predicted_waa <- wr_preds
wr_current_data |> arrange(desc(predicted_waa)) |> View()


te_current_data <- current_data |> 
  filter(position == "TE") |> 
  left_join(te_stats |> select(year, athlete_id, rushing_car:fumbles_lost),
            by = join_by(athlete_id, season == year)) |> 
  inner_join(all_athleticism |> 
               select(player_id, season, max_speed_value, total_distance_value,
                      contains("speed"), contains("accel"), contains("decel"),
                      contains("get_off")) |> 
               select(-closing_speed_on_target_value)) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))
te_current_mat <- model.matrix( ~ snaps + year_in_league + ranking + hs_stars +
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
                               data = te_current_data)

te_preds <- predict(te_model, te_current_mat, type = "response")
te_current_data$predicted_waa <- te_preds
te_current_data |> arrange(desc(predicted_waa)) |> View()

ol_current_data <- current_data |> 
  filter(position %in% c("C", "T", "G")) |> 
  inner_join(all_athleticism |> 
               select(player_id, season, max_speed_value, total_distance_value,
                      contains("speed"), contains("accel"), contains("decel"),
                      contains("get_off")) |> 
               select(-closing_speed_on_target_value)) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

ol_current_mat <- model.matrix( ~ snaps + year_in_league + ranking + hs_stars +
                              rating + tp_stars + weight + height + conference + waa +
                              max_speed_value + total_distance_value +
                              avg_speed_0_10_yards_value + avg_speed_10_20_yards_value +
                              max_speed_0_10_yards_value + max_speed_10_20_yards_value +
                              accel_0_5_value + accel_1_0_value + accel_1_5_value + 
                              accel_2_0_value + decel_0_5_value + decel_1_0_value + 
                              decel_1_5_value + decel_2_0_value + get_off_first_0_5_value +
                              get_off_first_1_0_value + get_off_first_1_5_value + 
                              get_off_first_2_0_value,
                            data = ol_current_data)
ol_preds <- predict(ol_model, ol_current_mat, type = "response")
ol_current_data$predicted_waa <- ol_preds
ol_current_data |> arrange(desc(predicted_waa)) |> View()

cb_current_data <- current_data |> 
  filter(position == "CB") |> 
  left_join(defensive_stats |> 
              select(year, athlete_id, defensive_solo:defensive_td),
            by = join_by(athlete_id, season == year)) |> 
  inner_join(all_athleticism |> 
               select(player_id, season, max_speed_value, total_distance_value,
                      contains("speed"), contains("accel"), contains("decel"),
                      contains("get_off"))) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

cb_current_mat <- model.matrix( ~ snaps + year_in_league + ranking + hs_stars +
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
                            data = cb_current_data)
cb_preds <- predict(cb_model, cb_current_mat, type = "response")
cb_current_data$predicted_waa <- cb_preds
cb_current_data |> arrange(desc(predicted_waa)) |> View()

s_current_data <- current_data |> 
  filter(position == "S") |> 
  left_join(defensive_stats |> 
              select(year, athlete_id, defensive_solo:defensive_td),
            by = join_by(athlete_id, season == year)) |> 
  inner_join(all_athleticism |> 
               select(player_id, season, max_speed_value, total_distance_value,
                      contains("speed"), contains("accel"), contains("decel"),
                      contains("get_off"))) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

s_current_mat <- model.matrix( ~ snaps + year_in_league + ranking + hs_stars +
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
                           data = s_current_data)
s_preds <- predict(s_model, s_current_mat, type = "response")
s_current_data$predicted_waa <- s_preds
s_current_data |> arrange(desc(predicted_waa)) |> View()

edge_current_data <- current_data |> 
  filter(position == "ED") |> 
  left_join(defensive_stats |> 
              select(year, athlete_id, defensive_solo:defensive_td),
            by = join_by(athlete_id, season == year)) |> 
  inner_join(all_athleticism |> 
               select(player_id, season, max_speed_value, total_distance_value,
                      contains("speed"), contains("accel"), contains("decel"),
                      contains("get_off"))) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

edge_current_mat <- model.matrix( ~ snaps + year_in_league + ranking + hs_stars +
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
                              data = edge_current_data)
edge_preds <- predict(edge_model, edge_current_mat, type = "response")
edge_current_data$predicted_waa <- edge_preds
edge_current_data |> arrange(desc(predicted_waa)) |> View()

rb_current_data <- current_data |> 
  filter(position == "HB" | position == "FB") |> 
  left_join(rb_stats |> select(year, athlete_id, rushing_car:fumbles_lost),
            by = join_by(athlete_id, season == year)) |> 
  inner_join(all_athleticism |> 
               select(player_id, season, max_speed_value, total_distance_value,
                      contains("speed"), contains("accel"), contains("decel")) |> 
               select(-closing_speed_on_target_value)) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

rb_preds <- predict(rb_model, rb_current_data)$predictions
rb_current_data$predicted_waa <- rb_preds
rb_current_data |> arrange(desc(predicted_waa)) |> View()

lb_current_data <- current_data |> 
  filter(position == "LB") |> 
  left_join(defensive_stats |> 
              select(year, athlete_id, defensive_solo:defensive_td),
            by = join_by(athlete_id, season == year)) |> 
  inner_join(all_athleticism |> 
               select(player_id, season, max_speed_value, total_distance_value,
                      contains("speed"), contains("accel"), contains("decel"),
                      contains("get_off"))) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

lb_preds <- predict(lb_model, lb_current_data)$predictions
lb_current_data$predicted_waa <- lb_preds
lb_current_data |> arrange(desc(predicted_waa)) |> View()

dl_current_data <- current_data |> 
  filter(position == "DI") |> 
  left_join(defensive_stats |> 
              select(year, athlete_id, defensive_solo:defensive_td),
            by = join_by(athlete_id, season == year)) |> 
  inner_join(all_athleticism |> 
               select(player_id, season, max_speed_value, total_distance_value,
                      contains("speed"), contains("accel"), contains("decel"),
                      contains("get_off"))) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

dl_preds <- predict(dl_model, dl_current_data)$predictions
dl_current_data$predicted_waa <- dl_preds
dl_current_data |> arrange(desc(predicted_waa)) |> View()

all_current_preds <- bind_rows(
  qb_current_data, rb_current_data, wr_current_data, te_current_data, ol_current_data,
  cb_current_data, s_current_data, lb_current_data, edge_current_data, dl_current_data
)
all_current_preds |> arrange(desc(predicted_waa)) |> View()

write_csv(all_current_preds, "26_predictions.csv")

library(cfbplotR)
library(gt)
library(gtExtras)
all_current_preds |> 
  arrange(desc(predicted_waa)) |> 
  select(athlete_id, player, position, predicted_waa) |> 
  slice_head(n = 5) |> 
  gt() |> 
  tab_header(title = md("**Highest Predicted WAA**"),
             subtitle = "2026 CFB Season") |> 
  gt_theme_538() |> 
  fmt_number(columns = predicted_waa) |> 
  data_color(columns = predicted_waa, 
             palette = "RdYlGn", domain = c(-0.06, 0.8)) |> 
  cols_label(player = "Player", 
             predicted_waa = "Predicted WAA",
             athlete_id = "") |> 
  cols_align(align = "center", columns = everything()) |> 
  gt_fmt_cfb_headshot(athlete_id)

all_current_preds |> 
  arrange(desc(predicted_waa)) |> 
  filter(position != "QB") |> 
  select(athlete_id, player, position, predicted_waa) |> 
  slice_head(n = 5) |> 
  gt() |> 
  tab_header(title = md("**Highest Predicted WAA**"),
             subtitle = "Non-Quarterbacks, 2026 CFB Season") |> 
  gt_theme_538() |> 
  fmt_number(columns = predicted_waa) |> 
  data_color(columns = predicted_waa, 
             palette = "RdYlGn", domain = c(-0.06, 0.8)) |> 
  cols_label(player = "Player", 
             predicted_waa = "Predicted WAA",
             athlete_id = "") |> 
  cols_align(align = "center", columns = everything()) |> 
  gt_fmt_cfb_headshot(athlete_id)

pred_waa_by_team <- all_current_preds |> 
  inner_join(team_codes, by = join_by(team == ultimate_name)) |> 
  left_join(portal_26 |> 
              mutate(player = paste(first_name, last_name)) |> 
              select(player, origin, destination),
            by = join_by(player, fastr_name == origin)) |> 
  mutate(team_26 = ifelse(is.na(destination), fastr_name, destination))

team_info <- cfbfastR::cfbd_team_info(year = 2025)

pred_waa_by_team |> 
  group_by(team_26) |> 
  summarise(total_waa = sum(predicted_waa)) |> 
  arrange(desc(total_waa)) |> 
  slice_head(n = 10) |> 
  rowid_to_column(var = "index") |> 
  rename(rank = index) |> 
  mutate(logo = team_26) |> 
  inner_join(team_info |> select(team_26 = school, mascot)) |> 
  gt() |> 
  tab_header(title = md("**Highest Predicted WAA**"),
             subtitle = "2026 CFB Rosters") |> 
  gt_theme_538() |> 
  fmt_number(columns = total_waa) |> 
  data_color(columns = total_waa, 
             palette = "RdYlGn", domain = c(-0.6, 2.1)) |> 
  cols_label(team_26 = "Team", 
             total_waa = "Predicted WAA") |> 
  cols_align(align = "center", columns = everything()) |> 
  gt_merge_stack_team_color("team_26", "mascot", "team_26") |> 
  gt_fmt_cfb_logo(columns = "logo")
  #gt_fmt_cfb_headshot(athlete_id)
