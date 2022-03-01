library(tidyverse)

games <- read_rds("~/Documents/QSS30/Project/all_games.rds")

is_first_half <- function(game_id) {
  first_half_game_ids <- c(2011020001:2011020615, 2012020001:2012020360,
                           2013020001:2013020615, 2014020001:2014020615,
                           2015020001:2015020615, 2016020001:2016020615,
                           2017020001:2017020635, 2018020001:2018020635)
  return(game_id %in% first_half_game_ids)
}

games_fixed <- games %>%
  rename(home_team = teams.home.team.name) %>%
  rename(away_team = teams.away.team.name) %>%
  mutate(season = as.numeric(substr(game_id, 1, 4))) %>%
  mutate(game_num = as.numeric(substr(game_id, 7, 10))) %>%
  mutate(first_half = is_first_half(game_id)) %>%
  mutate(post_change = ifelse(season>2014, 1, 0))

home_regulation_goals <- games_fixed %>%
  filter(first_half==1) %>%
  filter(periods.periodType == "REGULAR") %>%
  group_by(home_team, season) %>%
  summarize(goals_for=sum(periods.home$goals),
            goals_against=sum(periods.away$goals),
            num_games = n()/3) %>%
  rename(team=home_team)

away_regulation_goals <- games_fixed %>%
  filter(first_half==1) %>%
  filter(periods.periodType == "REGULAR") %>%
  group_by(away_team, season) %>%
  summarize(goals_for=sum(periods.away$goals),
            goals_against=sum(periods.home$goals),
            num_games = n()/3) %>%
  rename(team=away_team)

regulation_goals <- merge(home_regulation_goals,
                          away_regulation_goals,
                          by=c("team", "season")) %>%
  mutate(goals_for=goals_for.x+goals_for.y) %>%
  mutate(goals_against=goals_against.x+goals_against.y) %>%
  mutate(num_games=num_games.x+num_games.y) %>%
  select(team, season, goals_for, goals_against, num_games) %>%
  mutate(goal_differential_per_game = (goals_for - goals_against)/num_games)


ots <- games_fixed %>%
  filter(periods.periodType == "OVERTIME") %>%
  filter(first_half==1) %>%
  mutate(home_win = ifelse(teams.home.goals>teams.away.goals, 1, 0)) %>%
  mutate(SO = ifelse(hasShootout, 1, 0)) %>%
  select(home_team, away_team, home_win, SO, post_change, season)

ots1 <- merge(ots, regulation_goals,
             by.x = c("season", "home_team"),
             by.y = c("season", "team")) %>%
  rename(home_diff = goal_differential_per_game)


final_df <- merge(ots1, regulation_goals,
             by.x = c("season", "away_team"),
             by.y = c("season", "team")) %>%
  rename(away_diff = goal_differential_per_game) %>%
  mutate(power_difference = home_diff - away_diff) %>%
  select(home_team, away_team, home_win, SO, post_change, power_difference)


logit_model <- glm(home_win ~ post_change + power_difference + post_change*power_difference,
                   family=binomial(), data=final_df)

summary(logit_model)

stargazer::stargazer(logit_model, type="text")
