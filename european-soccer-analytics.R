# European Soccer EDA
# I'm using a Kaggle dataset that has match results, player ratings,
# team tactics, and Bet365 odds for 11 European leagues from 2008 to 2016.
# The goal is to find non-obvious patterns in the data.

# install.packages(c("RSQLite","DBI","dplyr","tidyr","ggplot2","lubridate","scales"))
suppressPackageStartupMessages({
  library(DBI); library(RSQLite)
  library(dplyr); library(tidyr); library(ggplot2)
  library(lubridate); library(scales)
})

# Update this path to wherever you saved the Kaggle database file
DB_PATH <- "C:/Users/Sheraz-21/Downloads/IDS 400/Dataset/database.sqlite"
OUT     <- "out"
dir.create(OUT, showWarnings = FALSE)

# The database has a lot of columns I don't need (XML event data, extra bookmakers)
# so I'm only pulling the columns that are actually useful
con     <- dbConnect(SQLite(), DB_PATH)
country <- dbReadTable(con, "Country")
league  <- dbReadTable(con, "League")
team    <- dbReadTable(con, "Team")
player  <- dbReadTable(con, "Player")

match <- dbGetQuery(con, "
  SELECT id, country_id, league_id, season, stage, date,
         home_team_api_id, away_team_api_id,
         home_team_goal, away_team_goal,
         B365H, B365D, B365A
  FROM Match")

patt <- dbGetQuery(con, "
  SELECT player_api_id, date, overall_rating, potential, preferred_foot,
         finishing, dribbling, short_passing, long_passing, ball_control,
         acceleration, sprint_speed, stamina, strength, vision,
         marking, standing_tackle, gk_reflexes
  FROM Player_Attributes")

tatt <- dbGetQuery(con, "
  SELECT team_api_id, date, buildUpPlaySpeed, buildUpPlayPassing,
         chanceCreationPassing, chanceCreationCrossing, chanceCreationShooting,
         defencePressure, defenceAggression, defenceTeamWidth,
         buildUpPlaySpeedClass, defenceDefenderLineClass
  FROM Team_Attributes")
dbDisconnect(con)

cat("Loaded:\n")
cat("  Match:       ", nrow(match), "rows\n")
cat("  Player:      ", nrow(player), "rows\n")
cat("  Player_Attr: ", nrow(patt),  "rows\n")
cat("  Team_Attr:   ", nrow(tatt),  "rows\n")
cat("  Team:        ", nrow(team),  "rows\n\n")

# =============================================================
# STEP 1: CLEANING
# =============================================================

# First I want to know how much data is missing in the columns I care about
key_cols <- c("home_team_goal","away_team_goal","B365H","B365D","B365A","date","season")
miss <- sapply(match[key_cols], function(x) sum(is.na(x) | x == ""))
cat("Missing values in key Match columns:\n"); print(miss)

# The dates come in as strings so I'm converting them to proper Date objects
match$date      <- as.Date(substr(match$date, 1, 10))
patt$date       <- as.Date(substr(patt$date, 1, 10))
tatt$date       <- as.Date(substr(tatt$date, 1, 10))
player$birthday <- as.Date(substr(player$birthday, 1, 10))

# Drop any matches where the score wasn't recorded
before <- nrow(match)
match  <- match %>% filter(!is.na(home_team_goal), !is.na(away_team_goal))
cat("Dropped", before - nrow(match), "matches with missing scores\n")

# I need complete Bet365 odds for the calibration analysis later
# so I'm flagging which matches have all three odds available
match$has_odds <- !is.na(match$B365H) & !is.na(match$B365D) & !is.na(match$B365A)
cat("Matches with full Bet365 odds:", sum(match$has_odds), "of", nrow(match), "\n")

# Players have multiple attribute snapshots over time
# I only want the most recent one per player
patt_latest <- patt %>%
  group_by(player_api_id) %>%
  arrange(desc(date)) %>%
  slice(1) %>% ungroup()
cat("Player_Attributes:", nrow(patt), "->", nrow(patt_latest),
    "(latest per player)\n")

# Clean up any extra whitespace in name fields
team$team_long_name <- trimws(team$team_long_name)
league$name         <- trimws(league$name)
country$name        <- trimws(country$name)

# Quick sanity check on goals to catch any obvious outliers
cat("Goals summary (home & away):\n")
print(summary(c(match$home_team_goal, match$away_team_goal)))
cat("Max goals by one team in a match:",
    max(c(match$home_team_goal, match$away_team_goal)), "\n\n")

# =============================================================
# STEP 2: FEATURE ENGINEERING
# =============================================================

# Building all the columns I need for analysis
# The de-vigging step removes the bookmaker margin from the odds
# so the implied probabilities sum to 1 and are fair to compare
match <- match %>%
  mutate(
    season_start = as.integer(substr(season, 1, 4)),
    total_goals  = home_team_goal + away_team_goal,
    goal_diff    = home_team_goal - away_team_goal,
    home_win     = home_team_goal >  away_team_goal,
    draw         = home_team_goal == away_team_goal,
    away_win     = home_team_goal <  away_team_goal,
    result       = case_when(
      home_win ~ "Home",
      draw     ~ "Draw",
      TRUE     ~ "Away"),
    p_home_raw   = ifelse(has_odds, 1/B365H, NA),
    p_draw_raw   = ifelse(has_odds, 1/B365D, NA),
    p_away_raw   = ifelse(has_odds, 1/B365A, NA),
    overround    = p_home_raw + p_draw_raw + p_away_raw,
    p_home       = p_home_raw / overround,
    p_draw       = p_draw_raw / overround,
    p_away       = p_away_raw / overround
  )

# Join in the league and country names so visuals are readable
match <- match %>%
  left_join(league  %>% select(league_id  = id, league_name  = name), by = "league_id") %>%
  left_join(country %>% select(country_id = id, country_name = name), by = "country_id")

# Calculate player age from birthday vs the date of their attribute snapshot
patt_latest <- patt_latest %>%
  left_join(player %>% select(player_api_id, birthday, height, weight),
            by = "player_api_id") %>%
  mutate(age = as.integer(floor(as.numeric(date - birthday) / 365.25)))

# Reshape the match table so each team gets its own row per match
# This makes it much easier to calculate per-team stats like points and goals
home_long <- match %>% transmute(
  league_name, country_name, season, season_start,
  team_api_id = home_team_api_id,
  goals_for   = home_team_goal,
  goals_ag    = away_team_goal,
  pts = ifelse(home_win, 3, ifelse(draw, 1, 0)),
  venue = "home")
away_long <- match %>% transmute(
  league_name, country_name, season, season_start,
  team_api_id = away_team_api_id,
  goals_for   = away_team_goal,
  goals_ag    = home_team_goal,
  pts = ifelse(away_win, 3, ifelse(draw, 1, 0)),
  venue = "away")
team_match <- bind_rows(home_long, away_long)

cat("Feature engineering done.\n\n")

# =============================================================
# STEP 3: EDA
# =============================================================

theme_set(theme_minimal(base_size = 12) +
            theme(plot.title = element_text(face = "bold")))

# How many goals does each league average per match?
goals_by_league <- match %>%
  group_by(league_name) %>%
  summarise(matches = n(),
            avg_total_goals = mean(total_goals),
            .groups = "drop") %>%
  arrange(desc(avg_total_goals))
cat("\nAvg goals per match by league:\n"); print(goals_by_league)

p1 <- ggplot(goals_by_league,
             aes(x = reorder(league_name, avg_total_goals), y = avg_total_goals)) +
  geom_col(fill = "#2c7fb8") +
  geom_text(aes(label = sprintf("%.2f", avg_total_goals)),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  labs(title = "Average Goals per Match by League (2008-2016)",
       x = NULL, y = "Avg goals per match") +
  expand_limits(y = max(goals_by_league$avg_total_goals) * 1.10)
print(p1); ggsave(file.path(OUT, "p1_goals_by_league.png"), p1, width = 8, height = 5, dpi = 150)

# What share of matches end in home win, draw, or away win by league?
p2 <- ggplot(match %>% count(league_name, result) %>%
               group_by(league_name) %>% mutate(pct = n/sum(n)),
             aes(x = league_name, y = pct, fill = result)) +
  geom_col(position = "stack") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c(Home = "#1a9850", Draw = "#999999", Away = "#d73027")) +
  coord_flip() +
  labs(title = "Match Outcome Distribution by League",
       x = NULL, y = "Share of matches", fill = NULL)
print(p2); ggsave(file.path(OUT, "p2_outcome_by_league.png"), p2, width = 8, height = 5, dpi = 150)

# Has the average number of goals per match changed over time?
goals_over_time <- match %>%
  group_by(season_start) %>%
  summarise(avg_goals = mean(total_goals), .groups = "drop")
p3 <- ggplot(goals_over_time, aes(x = season_start, y = avg_goals)) +
  geom_line(color = "#2c7fb8", linewidth = 1) +
  geom_point(color = "#2c7fb8", size = 2) +
  labs(title = "Average Goals per Match by Season",
       x = "Season starting year", y = "Avg goals/match")
print(p3); ggsave(file.path(OUT, "p3_goals_over_time.png"), p3, width = 8, height = 4, dpi = 150)

cat("\nPlots saved to", OUT, "\n\n")

# =============================================================
# STEP 4: INSIGHTS
# =============================================================

# Insight 1: Is home advantage getting smaller over time?
cat("\n--- Insight 1: Home advantage erosion ---\n")
home_adv <- match %>%
  group_by(season_start) %>%
  summarise(matches = n(),
            home_win_pct = mean(home_win),
            draw_pct     = mean(draw),
            away_win_pct = mean(away_win),
            .groups = "drop")
print(home_adv)

# Fit a linear trend to see if the decline is statistically meaningful
fit1 <- lm(home_win_pct ~ season_start, data = home_adv)
cat(sprintf("\nLinear trend in home-win pct: slope = %.4f per year (p = %.3f)\n",
            coef(fit1)[2], summary(fit1)$coefficients[2,4]))

p_ins1 <- ggplot(home_adv, aes(x = season_start)) +
  geom_line(aes(y = home_win_pct, color = "Home win"), linewidth = 1.1) +
  geom_line(aes(y = away_win_pct, color = "Away win"), linewidth = 1.1) +
  geom_line(aes(y = draw_pct,     color = "Draw"),     linewidth = 1.1) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(values = c("Home win" = "#1a9850",
                                "Away win" = "#d73027",
                                "Draw"     = "#999999")) +
  labs(title = "Home advantage erosion: outcome share by season",
       subtitle = "Across 11 European leagues, 2008-2016",
       x = "Season starting year", y = "Share of matches", color = NULL)
print(p_ins1); ggsave(file.path(OUT, "ins1_home_advantage.png"), p_ins1,
                      width = 8, height = 5, dpi = 150)

# Insight 2: Does Bet365 accurately price match outcomes?
# I'm grouping matches by how confident the bookmaker was in the favorite
# then checking whether favorites actually win at that rate
cat("\n--- Insight 2: Bet365 calibration ---\n")
upsets <- match %>%
  filter(has_odds) %>%
  mutate(
    fav = case_when(
      pmax(p_home, p_draw, p_away) == p_home ~ "Home",
      pmax(p_home, p_draw, p_away) == p_away ~ "Away",
      TRUE ~ "Draw"),
    fav_prob = pmax(p_home, p_draw, p_away),
    fav_bucket = cut(fav_prob,
                     breaks = c(0.35, 0.45, 0.55, 0.65, 0.75, 1.0),
                     labels = c("35-45%","45-55%","55-65%","65-75%","75%+"),
                     include.lowest = TRUE),
    fav_won = (fav == "Home" & home_win) |
      (fav == "Away" & away_win) |
      (fav == "Draw" & draw)
  )

upset_rate <- upsets %>%
  filter(!is.na(fav_bucket)) %>%
  group_by(fav_bucket) %>%
  summarise(matches      = n(),
            fav_win_rate = mean(fav_won),
            upset_rate   = 1 - mean(fav_won),
            avg_fav_prob = mean(fav_prob),
            .groups = "drop")
cat("\nFavorite win rate vs implied probability bucket:\n")
print(upset_rate)

p_ins2 <- ggplot(upset_rate, aes(x = avg_fav_prob, y = fav_win_rate)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(aes(size = matches), color = "#2c7fb8") +
  geom_text(aes(label = fav_bucket), nudge_y = 0.03, size = 3.2) +
  scale_x_continuous(labels = percent_format(), limits = c(0.35, 1)) +
  scale_y_continuous(labels = percent_format(), limits = c(0.35, 1)) +
  scale_size_continuous(range = c(3, 10)) +
  labs(title = "Bet365 favorite calibration",
       subtitle = "Dashed line = perfect calibration; points above = market underestimates favorites",
       x = "Average implied probability of favorite",
       y = "Actual favorite win rate", size = "Matches")
print(p_ins2); ggsave(file.path(OUT, "ins2_calibration.png"), p_ins2,
                      width = 8, height = 5, dpi = 150)

strong_fav <- upsets %>% filter(fav_prob >= 0.65)
cat(sprintf("\nFavorites with >=65%% implied prob (n=%d) win %.1f%% of the time.\n",
            nrow(strong_fav), 100 * mean(strong_fav$fav_won)))

# Insight 3: Which tactical traits actually predict league performance?
# I'm comparing build-up speed, passing style, and defensive pressure
# against points per match to see which one matters most
cat("\n--- Insight 3: Tactical traits and performance ---\n")

# Use the most recent tactical snapshot per team
tatt_latest <- tatt %>%
  group_by(team_api_id) %>%
  arrange(desc(date)) %>% slice(1) %>% ungroup()

# Aggregate each team's season performance into a single row
team_season <- team_match %>%
  group_by(league_name, season, season_start, team_api_id) %>%
  summarise(matches = n(), pts = sum(pts),
            gf = sum(goals_for), ga = sum(goals_ag),
            .groups = "drop") %>%
  mutate(ppm = pts / matches, gd = gf - ga)

# Join tactical ratings to season performance
team_style <- team_season %>%
  inner_join(tatt_latest %>% select(team_api_id, buildUpPlaySpeed,
                                    buildUpPlayPassing, defencePressure),
             by = "team_api_id") %>%
  mutate(speed_bucket = cut(buildUpPlaySpeed,
                            breaks = c(0, 40, 60, 100),
                            labels = c("Slow (<=40)","Balanced (41-60)","Fast (>60)")),
         press_bucket = cut(defencePressure,
                            breaks = c(0, 40, 50, 60, 100),
                            labels = c("Low (<=40)","Med-Low (41-50)",
                                       "Med-High (51-60)","High (>60)")),
         pass_bucket  = cut(buildUpPlayPassing,
                            breaks = c(0, 40, 60, 100),
                            labels = c("Short (<=40)","Mixed (41-60)","Long (>60)")))

speed_summary <- team_style %>% filter(!is.na(speed_bucket)) %>%
  group_by(speed_bucket) %>% summarise(n = n(), avg_ppm = mean(ppm), .groups = "drop")
press_summary <- team_style %>% filter(!is.na(press_bucket)) %>%
  group_by(press_bucket) %>% summarise(n = n(), avg_ppm = mean(ppm), .groups = "drop")
pass_summary  <- team_style %>% filter(!is.na(pass_bucket)) %>%
  group_by(pass_bucket)  %>% summarise(n = n(), avg_ppm = mean(ppm), .groups = "drop")

cat("\nBuild-up speed (no effect):\n");  print(speed_summary)
cat("\nDefensive pressure:\n");           print(press_summary)
cat("\nPassing style:\n");               print(pass_summary)

cor_speed <- cor(team_style$buildUpPlaySpeed,   team_style$ppm, use = "complete.obs")
cor_pass  <- cor(team_style$buildUpPlayPassing, team_style$ppm, use = "complete.obs")
cor_press <- cor(team_style$defencePressure,    team_style$ppm, use = "complete.obs")
cat(sprintf("\nCorrelations with PPM:  speed = %+.3f   passing = %+.3f   pressure = %+.3f\n",
            cor_speed, cor_pass, cor_press))

p_ins3a <- ggplot(team_style %>% filter(!is.na(speed_bucket)),
                  aes(x = speed_bucket, y = ppm, fill = speed_bucket)) +
  geom_boxplot(alpha = 0.85, outlier.size = 1) +
  scale_fill_manual(values = c("Slow (<=40)"="#1a9850",
                               "Balanced (41-60)"="#999999",
                               "Fast (>60)"="#d73027")) +
  labs(title = "Build-up speed is not the differentiator",
       subtitle = "Slow, balanced, and fast teams all average roughly the same PPM",
       x = "Build-up play speed", y = "Points per match", fill = NULL) +
  theme(legend.position = "none")
print(p_ins3a); ggsave(file.path(OUT, "ins3a_speed_no_effect.png"), p_ins3a,
                       width = 8, height = 5, dpi = 150)

p_ins3b <- ggplot(team_style %>% filter(!is.na(press_bucket)),
                  aes(x = press_bucket, y = ppm, fill = press_bucket)) +
  geom_boxplot(alpha = 0.85, outlier.size = 1) +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(title = "Higher defensive pressure leads to more points (corr +0.32)",
       subtitle = "Aggressive defending wins seasons",
       x = "Defensive pressure", y = "Points per match", fill = NULL) +
  theme(legend.position = "none")
print(p_ins3b); ggsave(file.path(OUT, "ins3b_pressure.png"), p_ins3b,
                       width = 8, height = 5, dpi = 150)

p_ins3c <- ggplot(team_style %>% filter(!is.na(pass_bucket)),
                  aes(x = pass_bucket, y = ppm, fill = pass_bucket)) +
  geom_boxplot(alpha = 0.85, outlier.size = 1) +
  scale_fill_manual(values = c("Short (<=40)"="#1a9850",
                               "Mixed (41-60)"="#999999",
                               "Long (>60)"="#d73027")) +
  labs(title = "Short passing teams outperform long ball teams (corr -0.21)",
       subtitle = "Possession style beats direct play",
       x = "Passing style", y = "Points per match", fill = NULL) +
  theme(legend.position = "none")
print(p_ins3c); ggsave(file.path(OUT, "ins3c_passing.png"), p_ins3c,
                       width = 8, height = 5, dpi = 150)

# Save all key numbers to a text file for reference
sink(file.path(OUT, "summary.txt"))
cat("EUROPEAN SOCCER EDA - KEY NUMBERS\n")
cat("=================================\n\n")
cat("Matches:", nrow(match), "| Seasons: 2008-2016 | Leagues: 11\n\n")
cat("--- Insight 1: Home advantage erosion ---\n")
print(home_adv)
cat(sprintf("\nHome-win trend: %.4f per year (p = %.3f)\n",
            coef(fit1)[2], summary(fit1)$coefficients[2,4]))
cat("\n--- Insight 2: Favorite calibration ---\n")
print(upset_rate)
cat(sprintf("\nStrong favorites (>=65%% prob) win %.1f%% of the time.\n",
            100 * mean(strong_fav$fav_won)))
cat("\n--- Insight 3: Tactical traits ---\n")
cat("Speed (no effect):\n");    print(speed_summary)
cat("Pressure (matters):\n");   print(press_summary)
cat("Passing (matters):\n");    print(pass_summary)
cat(sprintf("\nCorrelations with PPM:  speed=%+.3f  passing=%+.3f  pressure=%+.3f\n",
            cor_speed, cor_pass, cor_press))
sink()

cat("\nDone. All outputs saved to:", OUT, "\n")

# Export clean data to CSV for Power BI
write.csv(match,           file.path(OUT, "match_clean.csv"),      row.names = FALSE)
write.csv(goals_by_league, file.path(OUT, "goals_by_league.csv"),  row.names = FALSE)
write.csv(home_adv,        file.path(OUT, "home_advantage.csv"),   row.names = FALSE)
write.csv(upset_rate,      file.path(OUT, "upset_rate.csv"),       row.names = FALSE)
write.csv(team_style,      file.path(OUT, "team_style.csv"),       row.names = FALSE)
cat("CSVs exported to", OUT, "\n")