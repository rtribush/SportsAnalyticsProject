# Author: Darren Colby
# Date 2/16/2022
# Purpose: To clean and analyze data on shootoust before and after the NHL's new
# rule went into effect in the 2015-16 season

# Imports -----------------------------------------------------------------

library(tidyverse)

# Load the data -----------------------------------------------------------

games <- read_rds("data/all_games.rds")

# Light transformation ----------------------------------------------------

games <- games %>%

    # Create a variable for each season
    mutate(season = str_sub(periods.startTime, 1, 7),
           season = as.numeric(str_replace_all(season, "-", "")),
           season = case_when(
               season <= 201206 ~ 95,
               season %in% 201301:201306 ~ 96,
               season %in% 201310:201406 ~ 97,
               season %in% 201410:201506 ~ 98,
               season %in% 201510:201606 ~ 99,
               season %in% 201610:201706 ~ 100,
               season %in% 201710:201806 ~ 101,
               season >= 201810 ~ 102
           ),

           # Add dummy variable for whether a game goes into overtime
           overtime = ifelse(currentPeriod > 3, TRUE, FALSE),

           # Make shootout dummy variable
           shootout = hasShootout,

           # Convert game_id to a character string
           game_id = as.character(game_id)) %>%

    # Make game_id the first column
    relocate(game_id, season) %>%

    # Rename columns
    rename(home_team = teams.home.team.name, away_team = teams.away.team.name) %>%

    # Subset to relevant variables
    select(game_id, season, home_team, away_team, overtime, shootout)

# Figure 1 ----------------------------------------------------------------

games %>%

    # Calculate proportion of overtimes per season
    group_by(season) %>%
    summarise(proportion = mean(as.numeric(overtime)),
              shootout = "Overtime") %>%

    # Calculate proportion of shootouts per season and bind with overtimes
    bind_rows(games %>%
                  group_by(season) %>%
                  summarise(proportion = mean(as.numeric(shootout)),
                            shootout = "Shootout")) %>%

    # Make plot grouped by overtime and shootouts
    ggplot(aes(x = season,
               y = proportion,
               fill = shootout)) +

    # Basically a geom_area, but shows seasons as discrete instead of continuous
    geom_col(position = "identity",
             width = 1) +

    # Line and label for when the new rule was adopted
    geom_vline(xintercept = 98.5) +
    geom_text(x = 100.2,
              y = 0.56,
              label = "3-on-3 rule\nadopted") +

    # Add arrow from text
    geom_segment(aes(x = 99.7,
                     y = 0.55,
                     xend = 98.55,
                     yend = 0.5),
                 arrow = arrow()) +

    # Label overtime and shootouts
    geom_text(x = 100.5,
              y = 0.19,
              label = "Overtime",
              color = "white") +
    geom_text(x = 100.5,
              y = 0.05,
              label = "Shootout",
              color = "white") +

    # Fix scales
    scale_x_continuous(breaks = seq(95, 102, 1),
                       expand = c(-0.001, 0)) +
    scale_y_continuous(breaks = seq(0, 0.6, 0.1),
                       expand = expansion(mult = c(0, 0.1))) +

    # Add nice colors
    scale_fill_manual(values = c("#6fc27c", "#0f4d19")) +

    # Give informative labels
    labs(title = "NHL game outcomes",
         x = "Season",
         y = "Proportion of all games",
         caption = "*96th season shorter due to collective bargaining dispute",
         fill = NULL) +
    theme_minimal() +

    # No legend because the plot is labeled directly
    theme(legend.position = "none")

# Save the plot
ggsave("figures/figure1.jpg", width = 5.5, height = 4.25)
