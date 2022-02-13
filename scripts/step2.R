# Author: Darren Colby
# Date 2/13/2022
# Purpose: To clean and analyze data on shootoust before and after the NHL's new
# rule went into effect in the 2015-16 season

# Imports -----------------------------------------------------------------

library(tidyverse)

# Load the data -----------------------------------------------------------

games <- read_rds("data/all_games.rds")

# Add variables for season and overtime -----------------------------------

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
               season %in% 201710:201806 ~ 101
           ),

           # Add boolean for whether a game goes into overtime
           overtime = ifelse(currentPeriod > 3, TRUE, FALSE))

# Figure 1 ----------------------------------------------------------------

games %>%
    group_by(season) %>%
    summarise(proportion = mean(overtime),
              shootout = "Overtime") %>%
    bind_rows(games %>%
                  group_by(season) %>%
                  summarise(proportion = mean(hasShootout),
                            shootout = "Shootout")) %>%
    ggplot(aes(x = season,
               y = proportion,
               fill = factor(shootout))) +
    geom_area(position = "identity") +
    geom_vline(xintercept = 99) +
    geom_text(x = 100,
              y = 0.49,
              label = "NHL implements \n3-on-3 overtime rule") +
    scale_x_continuous(breaks = seq(95, 101, 1),
                       expand = c(0, 0)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.7))) +
    scale_fill_manual(values = c("#0f4d19", "#6fc27c")) +
    labs(title = "NHL games that ended in overtime vs shootouts",
         x = "Season",
         y = "Proportion of all games",
         caption = "*The 96th season was shorter due to a collective bargaining agreement dispute",
         fill = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom")

# Save the plot
ggsave("figures/figure1.jpg", width = 5, height = 4.25)
