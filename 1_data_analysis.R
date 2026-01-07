# NFLPA Data Competition ----
## Univariate Analyses ----

### load packages ----
library(tidyverse)
library(nflverse)

### Create Dataset for games last decade ----
games_2015_2024 <- load_schedules(seasons = 2015:2024) |> 
  mutate(
    is_thursday = weekday == "Thursday",
    short_rest_home = home_rest < 6,
    short_rest_away = away_rest < 6
  )


team_games <- games_2015_2024 %>%
  select(
    game_id, season, week, weekday,
    home_team, away_team,
    home_rest, away_rest,
    home_score, away_score
  ) %>%
  pivot_longer(
    cols = c(home_team, away_team),
    names_to = "home_away",
    values_to = "team",
    values_drop_na = TRUE
  ) %>%
  mutate(
    rest_days = if_else(home_away == "home_team", home_rest, away_rest),
    points_for = if_else(home_away == "home_team", home_score, away_score),
    points_against = if_else(home_away == "home_team", away_score, home_score),
    short_rest = rest_days < 6,
    is_thursday = weekday == "Thursday",
    win = points_for > points_against
  )


injuries_2015_2024 <- load_injuries(seasons = 2015:2024)

injuries_joined <- injuries_2015_2024 |> 
  left_join(
    team_games,
    by = c("team", "season", "week")
  )

### Visualizations ----
injuries_joined %>%
  group_by(short_rest) %>%
  summarise(
    injuries = n(),
    players = n_distinct(full_name)
  )


injuries_joined %>%
  filter(report_status %in% c("Out", "Doubtful")) %>%
  group_by(short_rest) %>%
  summarise(n = n())


injuries_joined %>%
  group_by(is_thursday) %>%
  summarise(injuries = n())


injuries_per_game <- injuries_joined %>%
  group_by(game_id, team, short_rest) %>%
  summarise(
    injuries = n(),
    .groups = "drop"
  )


injury_summary <- injuries_per_game %>%
  group_by(short_rest) %>%
  summarise(
    avg_injuries = mean(injuries),
    se = sd(injuries) / sqrt(n()),
    games = n(),
    .groups = "drop"
  )


# -----------------------------------

team_games <- team_games %>%
  arrange(team, season, week) %>%
  group_by(team, season) %>%
  mutate(
    prev_short_rest = lag(short_rest)
  ) %>%
  ungroup()


injuries_lagged <- injuries_joined %>%
  left_join(
    team_games,
    by = c("team", "season", "week")
  )


injuries_weekly <- injuries_lagged %>%
  group_by(team, season, week, prev_short_rest) %>%
  summarise(
    injuries = n(),
    .groups = "drop"
  )

injury_lag_summary <- injuries_weekly %>%
  group_by(prev_short_rest) %>%
  summarise(
    avg_injuries = mean(injuries),
    se = sd(injuries) / sqrt(n()),
    weeks = n(),
    .groups = "drop"
  )

injuries_lagged %>%
  filter(report_status %in% c("Out", "IR")) %>%
  group_by(team, season, week, prev_short_rest) %>%
  summarise(severe_injuries = n(), .groups = "drop") %>%
  group_by(prev_short_rest) %>%
  summarise(
    avg_severe = mean(severe_injuries),
    se = sd(severe_injuries) / sqrt(n()),
    weeks = n()
  )

fatigue <- c("Hamstring", "Groin", "Calf", "Quad", "Achilles")

injuries_lagged %>%
  filter(practice_primary_injury %in% fatigue) %>%
  group_by(team, season, week, prev_short_rest) %>%
  summarise(injuries = n(), .groups = "drop") %>%
  group_by(prev_short_rest) %>%
  summarise(avg = mean(injuries))


injuries_lagged %>%
  filter(report_status == "Out") %>%
  group_by(team, season, week, prev_short_rest) %>%
  summarise(out_players = n_distinct(full_name, .groups = "drop") %>%
  group_by(prev_short_rest) %>%
  summarise(avg_out = mean(out_players))










