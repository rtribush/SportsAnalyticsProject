# Author: Darren Colby
# Date 2/18/2022
# Purpose: To clean and analyze data on shootoust before and after the NHL's new
# rule went into effect in the 2015-16 season

# Imports -----------------------------------------------------------------

library(tidyverse)
library(ggthemes)

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
    ungroup() %>%

    # Make game_id the first column
    relocate(game_id, season) %>%

    # Rename columns
    rename(home_team = teams.home.team.name, away_team = teams.away.team.name) %>%

    # Subset to relevant variables
    select(game_id, season, home_team, away_team, overtime, shootout)


# Make dataframe of only overtime games -----------------------------------

# Only games from one year before and after the rule change with overtime
games_ot_1112 <- games %>%

    # Filter to games that went to overtime a season before and after change
    filter(overtime == TRUE & season %in% 98:99) %>%

    # Deduplicate since each game has an observation for each period
    distinct(game_id, season, overtime, shootout) %>%

    # Select relevant columns
    select(season, shootout)

# Aggregate to periods before and after the rule change
games_ot_grouped <- games %>%

    # games that went into overtime
    filter(overtime == TRUE) %>%

    # Deduplicate
    distinct(game_id, season, overtime, shootout) %>%

    # Dummy variable for if a season was before or after the rule chnage
    mutate(treatment = ifelse(season < 99, 0, 1)) %>%
    ungroup() %>%

    # Subset to relevant columns
    select(treatment, shootout)

# All games that went into overtime
games_ot_all <- games %>%

    # Only games that went into overtime
    filter(overtime == TRUE) %>%

    # Deduplicate since each game has an observation for each period
    distinct(game_id, season, overtime, shootout) %>%

    # Select the relevant columns
    select(season, shootout)

# Figure 1 ----------------------------------------------------------------

games %>%

    # Calculate proportion of overtimes per season
    group_by(season) %>%
    summarise(proportion = mean(as.numeric(overtime)),
              shootout = "Overtime") %>%
    ungroup() %>%

    # Calculate proportion of shootouts per season and bind with overtimes
    bind_rows(games %>%
                  group_by(season) %>%
                  summarise(proportion = mean(as.numeric(shootout)),
                            shootout = "Shootout")) %>%
    ungroup() %>%

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

# Conduct Chi-squared tests -----------------------------------------------

# Test for 2011-2012 season
chisquared_1112 <- chisq.test(games_ot_1112$season, games_ot_1112$shootout)

# Test for the four years before and after the rule change
chisquared_grouped <- chisq.test(games_ot_grouped$treatment,
                                 games_ot_grouped$shootout)

# Test all seasons disaggregated
chisquared_all <- chisq.test(games_ot_all$season, games_ot_all$shootout)

# Plotting function -------------------------------------------------------


# Plots the observed vs expected number of shootouts per year
# @param seasons a vector of seasons to add to the x-axis
# @param test a chi-squared test
# @param title a title for the plot
# @return a ggplot object
plot_chisquare <- function(test, title, seasons) {

    # Retrieve the expected and observed number of games in each year
    df1 <- bind_cols(seasons, test$observed[ ,2],
                     test$expected[ ,2]) %>%

        # Use better column names
        rename(season = 1, observed = 2, expected = 3) %>%

        # Convert season to a factor so it is plotted as such
        mutate(season = as_factor(season),

               # Subtract the expected from observed number of shootouts
               difference = observed - expected) %>%
        ungroup()

    plot <- ggplot(df1,
                   aes(x = season,
                       y = difference)) +

        # Bar plot
        geom_col(width = 0.98,
                 fill = "red") +
        labs(title = title,
             x = "Season",
             y = "Difference") +

        # Adds labels to the bars. If bars are negative, labels are below bars,
        # oterwise they are above the bars
        geom_text(aes(label = round(difference, 2)),
                  position = position_dodge(width = 0.9),
                  vjust = ifelse(df1$difference > 1, -1, 2)) +

        # Add a line to make it easier to see where zero id
        geom_hline(yintercept = 0) +
        theme_tufte()

    return(plot)

}


# Figure 2 ----------------------------------------------------------------

plot_chisquare(chisquared_1112,
               "Difference in expected and observed shootouts (seaons 98-99)",
               c(98, 99)) +
    ylim(-30, 30) +
    annotate("text",
             x = 1,
             y = -15,
             label = paste("p =",
                           as.character(signif(chisquared_1112$p.value, 2))))

# Save it
ggsave("figures/figure2.jpg", width = 5.5, height = 3)

# Figure 3 ----------------------------------------------------------------

plot_chisquare(chisquared_grouped,
               "Difference in expected and observed shootouts four years before and after the NHL rule change",
               c("Before the\nrule change", "After the\nrule change")) +
    ylim(-170, 170) +
    annotate("text",
             x = 1,
             y = -100,
             label = paste("p =",
                           as.character(signif(chisquared_grouped$p.value, 2))))

# Save it
ggsave("figures/figure3.jpg", width = 5.5, height = 3)

# Figure 4 ----------------------------------------------------------------

plot_chisquare(chisquared_all, "Difference in expected and observed shootouts (seasons 95-102)",
               95:102) +
    ylim(-50, 50) +
    annotate("text",
             x = 2,
             y = -25,
             label = paste("p =",
                           as.character(signif(chisquared_all$p.value, 2))))

# Save it
ggsave("figures/figure4.jpg", width = 7, height = 5)
