library(tidyverse)
library(jsonlite)

download_standing_data <- function(year) {
  year_url <- paste("https://statsapi.web.nhl.com/api/v1/standings/byLeague?date=", as.character(year), "-12-31")
  raw_json <- fromJSON(year_url)
  standings_df <- as_tibble(as.data.frame(raw_json$records$teamRecords))
  standings_df <- standings_df %>%
    mutate(season = year)
  return(standings_df)
}

the_years <- 2011:2018

all_standings <- bind_rows(map(the_years, function(id) download_standing_data(id)))

all_standings$team <- all_standings$team$name

write_csv(all_standings, "~/Documents/QSS30/Project/standings.csv")


