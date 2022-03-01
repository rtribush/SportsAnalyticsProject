# Authors: Darren Colby and Rylan Tribush
# Date: 3/1/2022
# Purpose: To import and clean NHL game data for analysis

# Imports -----------------------------------------------------------------

library(tidyverse)
library(jsonlite)

# Function to read one game -----------------------------------------------

# Downloads a single game from the NHL's API
# @param game_id the ID of the game to download information for
# @return a dataframe with information from the game
download_game_data <- function(game_id) {

    # The static part of the URL concatenated with the game ID
    url_part1 <- paste("https://statsapi.web.nhl.com/api/v1/game/",
                       as.character(game_id))

    # The first part of the URL concatenated with the full URL
    full_url <- paste(url_part1, "/linescore")

    # Get the JSON file of game data from the NHL
    raw_json <- fromJSON(full_url)

    # Convert it to a tibble
    game_df <- as_tibble(as.data.frame(raw_json)[ ,-1])

    # Select columns of interest that are available in for all games
    game_df <- game_df %>%
        select(teams.home.team.name, teams.away.team.name, periods.startTime,
               currentPeriod, hasShootout, teams.home.goals, teams.away.goals,
               periods.periodType)

    # Add a unique ID for each game
    game_df <- game_df %>%
        mutate(game_id = game_id)

    return(game_df)
}

# Download all the data ---------------------------------------------------

# IDs for all games three years before and after the rule change
game_ids <- c(2011020001:2011021230, 2012020001:2012020720,
              2013020001:2013021230, 2014020001:2014021230,
              2015020001:2015021230, 2016020001:2016021230,
              2017020001:2017021271, 2018020001:2018021271)

# Download all games and put them into a single dataframe
all_games <- bind_rows(map(game_ids, function(id) download_game_data(id)))

# Export the data ---------------------------------------------------------

write_rds(all_games, "data/all_games.rds")
