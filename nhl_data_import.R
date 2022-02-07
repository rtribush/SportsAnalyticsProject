# NHL overtime data cleaning and processing
# by Rylan Tribush

library(nhlapi)

# Warning: may take a while to run
games_2014 <- nhl_games_linescore(2014020001:2014021230)
games_2015 <- nhl_games_linescore(2015020001:2015021230)
games_2016 <- nhl_games_linescore(2016020001:2016021230)
games_2017 <- nhl_games_linescore(2017020001:2017021230)

# put key 2014 data into dataframe
df_2014 <- data.frame(gameID = 1:1230, home = NA, away = NA, OT=rep(0, 1230), SO=rep(0, 1230))
for (id in df_2014$gameID) {
  df_2014$home[id] <- games_2014[[id]]$teams$home$team$name
  df_2014$away[id] <- games_2014[[id]]$teams$away$team$name
  if (games_2014[[id]]$currentPeriod > 3) {
    df_2014$OT[id] <- 1
  }
  if (games_2014[[id]]$hasShootout) {
    df_2014$SO[id] <- 1
  }
}

# repeat for 2015
df_2015 <- data.frame(gameID = 1:1230, home = NA, away = NA, OT=rep(0, 1230), SO=rep(0, 1230))
for (id in df_2015$gameID) {
  df_2015$home[id] <- games_2015[[id]]$teams$home$team$name
  df_2015$away[id] <- games_2015[[id]]$teams$away$team$name
  if (games_2015[[id]]$currentPeriod > 3) {
    df_2015$OT[id] <- 1
  }
  if (games_2015[[id]]$hasShootout) {
    df_2015$SO[id] <- 1
  }
}

# repeat for 2016
df_2016 <- data.frame(gameID = 1:1230, home = NA, away = NA, OT=rep(0, 1230), SO=rep(0, 1230))
for (id in df_2016$gameID) {
  df_2016$home[id] <- games_2016[[id]]$teams$home$team$name
  df_2016$away[id] <- games_2016[[id]]$teams$away$team$name
  if (games_2016[[id]]$currentPeriod > 3) {
    df_2016$OT[id] <- 1
  }
  if (games_2016[[id]]$hasShootout) {
    df_2016$SO[id] <- 1
  }
}

# repeat for 2017
df_2017 <- data.frame(gameID = 1:1230, home = NA, away = NA, OT=rep(0, 1230), SO=rep(0, 1230))
for (id in df_2017$gameID) {
  df_2017$home[id] <- games_2017[[id]]$teams$home$team$name
  df_2017$away[id] <- games_2017[[id]]$teams$away$team$name
  if (games_2017[[id]]$currentPeriod > 3) {
    df_2017$OT[id] <- 1
  }
  if (games_2017[[id]]$hasShootout) {
    df_2017$SO[id] <- 1
  }
}

# may have to change for your computer
write.csv(df_2014, "~/Documents/QSS30/Project/data_2014.csv", row.names = FALSE)
write.csv(df_2015, "~/Documents/QSS30/Project/data_2015.csv", row.names = FALSE)
write.csv(df_2016, "~/Documents/QSS30/Project/data_2016.csv", row.names = FALSE)
write.csv(df_2017, "~/Documents/QSS30/Project/data_2017.csv", row.names = FALSE)
