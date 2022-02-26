games <- read_rds("~/Documents/QSS30/Project/all_games.rds")

standings <- read_csv("~/Documents/QSS30/Project/standings.csv")

standings$goalDiff <- standings$goalsScored - standings$goalsAgainst

stands <- standings %>%
  select(team, goalDiff, season)

ots <- games %>%
  rename(home_team = teams.home.team.name) %>%
  rename(away_team = teams.away.team.name) %>%
  filter(periods.periodType == "OVERTIME") %>%
  mutate(home_win = ifelse(teams.home.goals>teams.away.goals, 1, 0)) %>%
  mutate(SO = ifelse(hasShootout, 1, 0)) %>%
  mutate(season = as.numeric(substr(game_id, 1, 4))) %>%
  select(home_team, away_team, home_win, SO, season)

merged <- merge(ots, stands, by.x=c("home_team", "season"), by.y=c("team", "season")) %>%
  rename(homedff = goalDiff)

finaldf <- merge(merged, stands, by.x=c("away_team", "season"), by.y=c("team", "season")) %>%
  rename(awaydff = goalDiff)

finaldf$home_advantage <- finaldf$homedff - finaldf$awaydff
finaldf$post_change <- ifelse(finaldf$season>2014, 1, 0)

logit_model <- glm(home_win ~ home_advantage + post_change + 
                     home_advantage*post_change,
                   family=binomial(), data=finaldf)

summary(logit_model)

