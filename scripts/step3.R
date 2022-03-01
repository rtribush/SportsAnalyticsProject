# Author: Darren Colby and Rylan Tribush
# Date: 3/1/2022
# Purpose: To estimate the effect of the NHL's rule change on the probability of
# the home team winning

# Imports -----------------------------------------------------------------

library(tidyverse)
library(stargazer)

# Load the data -----------------------------------------------------------

games <- read_rds("data/all_games.rds")

# Find games that occurred in the first half of each season ---------------

# Determines if a game was played in the first half of a season
# @param game_id a game ID
# @return a boolean
is_first_half <- function(game_id) {

    first_half_game_ids <- c(2011020001:2011020615, 2012020001:2012020360,
                             2013020001:2013020615, 2014020001:2014020615,
                             2015020001:2015020615, 2016020001:2016020615,
                             2017020001:2017020635, 2018020001:2018020635)

    return(game_id %in% first_half_game_ids)
}

games <- games %>%

    # Create a variable that tells whether a game was in the first half
    mutate(is_first_half = is_first_half(game_id),

           # Also code which season a game occurred in
           season = str_sub(periods.startTime, 1, 7),
           season = as.numeric(str_replace_all(season, "-", "")),
           season = case_when(
               season <= 201206 ~ 95,
               season %in% 201301:201306 ~ 96,
               season %in% 201310:201406 ~ 97,
               season %in% 201410:201506 ~ 98,
               season %in% 201510:201606 ~ 99,
               season %in% 201610:201706 ~ 100,
               season %in% 201710:201806 ~ 101,
               season >= 201810 ~ 102),
           post_change = as_factor(ifelse(season > 98, 1, 0))) %>%
    ungroup()

# Calculate goal differential ---------------------------------------------

# Goal summary for the home teams
home_regulation_goals <- games %>%

    # Subset to games in the first half of each season not in overtime
    filter(is_first_half == TRUE & currentPeriod < 4) %>%

    # Calculate goals for and against each home team
    group_by(teams.home.team.name, season) %>%
    summarise(goals_for = sum(teams.home.goals),
              goals_against = sum(teams.away.goals),
              num_games = n() / 3) %>%
        ungroup() %>%

    # Use shorter name
    rename(team = teams.home.team.name)

# Goal summary for the away teams
away_regulation_goals <- games %>%

    # Subset to games in the first half of each season not in overtime
    filter(is_first_half == TRUE & currentPeriod < 4) %>%

    # Calculate goals for and against each home team
    group_by(teams.away.team.name, season) %>%
    summarise(goals_for = sum(teams.away.goals),
              goals_against = sum(teams.home.goals),
              num_games = n() / 3) %>%
    ungroup() %>%

    # Use shorter name
    rename(team = teams.away.team.name)

# Calculate goal differential per game per team
# Merge home and away goal data
regulation_goals <- left_join(home_regulation_goals, away_regulation_goals,
                              by = c("team", "season")) %>%

    # Goals for each team
    mutate(goals_for = goals_for.x + goals_for.y,

           # Goals against each team
           goals_against = goals_against.x + goals_against.y,

           # Number of games
           num_games = num_games.x + num_games.y,

           # Mean differential
           goal_diff_per_game = (goals_for - goals_against) / num_games) %>%
    select(team, season, goals_for, goals_against, num_games,
           goal_diff_per_game)

# Add goal statistics to overtime data ------------------------------------

ots <- games %>%

    # Filter to overtime games in the first half
    filter(currentPeriod > 3 & is_first_half == FALSE) %>%

    # Code which team won and whether there was a shootout
    mutate(home_win = as_factor(ifelse(teams.home.goals > teams.away.goals, 1, 0)),
           SO = as_factor(ifelse(hasShootout, 1, 0))) %>%
    ungroup() %>%

    # Make better names
    rename(home_team = teams.home.team.name,
           away_team = teams.away.team.name) %>%
    select(home_team, away_team, home_win, SO, post_change, season) %>%

    # Join with the data for regulation play
    left_join(regulation_goals, by = c("season" = "season",
                                       "home_team" = "team")) %>%

    # Make a better variable name
    rename(home_diff = goal_diff_per_game) %>%

    # Merge with regulation goals again to get away differential
    left_join(regulation_goals, by = c("season" = "season",
                                       "away_team" = "team")) %>%
    # Add better name
    rename(away_diff = goal_diff_per_game) %>%

    # Calculate power differential
    mutate(power_diff = home_diff - away_diff) %>%
    ungroup() %>%
    select(home_team, away_team, home_win, SO, post_change, power_diff)

# Estimate a logistic regression ------------------------------------------

# home wins as dependent variable
m1 <- glm(home_win ~ post_change + power_diff + post_change*power_diff +
              home_team, family = binomial(link = "logit"), data = ots)

# table 1 -----------------------------------------------------------------

stargazer(m1,
          add.lines=list(c('Home team fixed effects', 'Yes','No')),
          keep = c("post_change1", "power_diff", "post_change1:power_diff"),
          dep.var.labels = c("Home team win"),
          covariate.labels = c("Rule change", "Power differential", "
                               Rule change x Power differential"),
          type = "text")
