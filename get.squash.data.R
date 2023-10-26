## scrape squash results data from www.psaworldtour.com
##
## list of tournaments from 
## https://www.psaworldtour.com/tournaments/
##
## need scores from tournament sites like 
## https://www.psaworldtour.com/tournament/u-s-open-2023/
## 
## build in some delay time, and scrape recent and 
## scrape most important events first in case we can rate limited
library(XML)
library(xml2)
library(rvest)


## get list of all completed tournaments and save into a rds file
tournaments.url = 'https://www.psaworldtour.com/tournaments'
html = scan(tournaments.url, what='', sep='\n')
filename = paste0('rawdata/', 'tournaments', '.rds')
saveRDS(html, file=filename)


## get links to tournaments
raw.tournaments <- readRDS(filename)
# number of completed events in current 23/24 season
n_completed <- length(raw.tournaments[grepl('data-status=\"completed\"', raw.tournaments)]) - 1 
tournament.links <- raw.tournaments[grepl('href=\"https://www.psaworldtour.com/tournament/', raw.tournaments)]

# only 2 tournaments for this assignment
for (i in 1:3) {
  cat(i, "\n")
  url <- gsub(".*\"(.+)\".*", "\\1", tournament.links[i])
  html = scan(url, what='', sep='\n')
  filename = gsub("https://www.psaworldtour.com/tournament/(.+)/", "\\1", url)
  saveRDS(html, file=paste0('rawdata/tournaments/rds/', filename, '.rds'))
  writeLines(html, paste0('rawdata/tournaments/html/', filename, '.html'))
  
  ## put in a short pause so the site doesn't hate us
  Sys.sleep(runif(1, min=.5, max=5)) 
}


## load files and extract results into a data frame

# constants for round names
round_names <- c("Final", "Semifinal", "Quarterfinal", "Round of 16", "Round of 32", "Round of 64")

# a function to extract relevant data from a file into a data frame
extractResults <- function(filename, df) {
  
  print(paste("extracting data for", filename))
  
  # load from rds file
  file <- readRDS(paste0("rawdata/tournaments/rds/", filename, ".rds"))
  
  # tournament name
  name <- file[grepl('h1 style=\"font-size: 3em;\"', file)]
  name <- gsub(".*>(.+)<.*", "\\1", name)
  
  # dates
  dates <- file[grepl("\\s+\\d{2}\\s[A-Z][a-z]{2}\\s-\\s\\d{2}\\s[A-Z][a-z]{2}\\s+</p>", file)]
  dates <- gsub("</p>", "\\1", dates)
  dates <- trimws(dates)
  
  # location
  location <- file[grepl("<p>.+</p>", file)]
  location <- gsub("\\s*<p>", "\\1", location)
  location <- gsub("</p>", "\\1", location)

  # prize money tier
  prize_money_tier <- file[grepl("\\s+\\$", file)]
  prize_money_tier <- gsub("</p>", "\\1", prize_money_tier)
  prize_money_tier <- trimws(prize_money_tier)
  
  # men/women
  # both men and women if
  # e.g. "$6,000 (M)                                         /                                         $9,000 (W)"
  men <- TRUE
  women <- TRUE
  if (!grepl("\\/", prize_money_tier)) {
    # only one gender e.g. "$6,000 (M)"
    # check second last char
    if (substr(prize_money_tier, nchar(prize_money_tier)-1, nchar(prize_money_tier)-1) == "M") {
      women <- FALSE
    } else {
      men <- FALSE
    }
  }
  
  
  # match results (round, date, players, player seed, overall game scores, points)
  # first table is list of players with seeding (first two if both gender, one for each gender), 
  # then each of the following table contains matches for each round 
  tables <- readHTMLTable(file)
  table_i <- 0
  
  # get players' seeding in tournament from first (two) table(s)
  # and players' country from "alt" attr in flag img from html file using rvest
  seed_country_m <- data.frame(player = character(0), seed = character(0))
  seed_country_w <- data.frame(player = character(0), seed = character(0))
  n_players_m <- 0
  n_players_w <- 0
  
  # load from html file
  html <- xml2::read_html(paste0("rawdata/tournaments/html/", filename, ".html"))
  
  if (men) {
    # get seeding
    table_i <- table_i + 1
    entries <- tables[[table_i]]$V3
    n_players_m <- length(entries)
    for (i in 1:n_players_m) {
      # e.g. "Ali Farag\n                                                    (1)" 
      if (grepl("\\(", entries[i])) {
        player <- gsub("\n.*", "", entries[i])
        seed <- gsub("\\)", "", gsub(".*\\(", "", entries[i]))
      } else {
        player <- entries[i]
        seed <- "unseeded"
      }
      seed_country_m <- rbind(seed_country_m, list(player=player, seed=seed))
    }
    
    # get countries
    countries <- html %>%
      html_nodes('.men .flag') %>%
      html_attr(name = "alt")
    seed_country_m$country = countries
    
  }
  if (women) {
    # get seeding
    table_i <- table_i + 1
    entries <- tables[[table_i]]$V3
    n_players_w <- length(entries)
    for (i in 1:n_players_w) {
      if (grepl("\\(", entries[i])) {
        player <- gsub("\n.*", "", entries[i])
        seed <- gsub("\\)", "", gsub(".*\\(", "", entries[i]))
      } else {
        player <- entries[i]
        seed <- "unseeded"
      }
      seed_country_w <- rbind(seed_country_w, list(player=player, seed=seed))
    }
    
    # get countries
    countries <- html %>%
      html_nodes('.women .flag') %>%
      html_attr(name = "alt")
    seed_country_w$country = countries
  }
  
  # get matches results round by round
  if (men) {
    matches_m <- data.frame(round = character(0), gender = character(0), 
                            player1 = character(0), player2 = character(0), 
                            p1_country = character(0), p2_country = character(0), 
                            p1_seed = character(0), p2_seed = character(0), 
                            p1_sets_won = numeric(0), p2_sets_won = numeric(0), 
                            p1_set1_score = numeric(0), p1_set2_score = numeric(0), 
                            p1_set3_score = numeric(0), p1_set4_score = numeric(0), 
                            p1_set5_score = numeric(0), p2_set1_score = numeric(0), 
                            p2_set2_score = numeric(0), p2_set3_score = numeric(0), 
                            p2_set4_score = numeric(0), p2_set5_score = numeric(0), 
                            date_of_match = character(0))
    
    # get the number of rounds based on number of players
    n_rounds <- ceiling(log2(n_players_m))
    
    # iterate through rest of the tables, 
    # where each table represent each round in the order of Final, Semifinal, Quarterfinal, ...
    for (i in 1:n_rounds) {
      round <- tables[[table_i + i]]
      n_matches <- nrow(round)/3
      for (j in 0:(n_matches-1)) {
        date_of_match <- round[3*j + 1,]$V1
        
        p1 <- round[3*j + 2,]
        player1 <- p1$V1
        player1 <- gsub("\\n.*", "", player1)
        p1_sets_won <- p1$V3
        p1_set1_score <- p1$V4
        p1_set2_score <- p1$V5
        p1_set3_score <- p1$V6
        p1_set4_score <- p1$V7
        p1_set5_score <- p1$V8
        p1_seed <- seed_country_m$seed[seed_country_m$player == player1]
        p1_country <- seed_country_m$country[seed_country_m$player == player1]
        
        p2 <- round[3*j + 3,]
        player2 <- p2$V1
        player2 <- gsub("\\n.*", "", player2)
        p2_sets_won <- p2$V3
        p2_set1_score <- p2$V4
        p2_set2_score <- p2$V5
        p2_set3_score <- p2$V6
        p2_set4_score <- p2$V7
        p2_set5_score <- p2$V8
        p2_seed <- seed_country_m$seed[seed_country_m$player == player2]
        p2_country <- seed_country_m$country[seed_country_m$player == player2]
        
        # add to data frame
        matches_m <- rbind(matches_m, 
                           list(round = round_names[i], gender = "m",
                                player1 = player1, player2 = player2, 
                                p1_country = p1_country, p2_country = p2_country, 
                                p1_seed = p1_seed, p2_seed = p2_seed, 
                                p1_sets_won = p1_sets_won, p2_sets_won = p2_sets_won, 
                                p1_set1_score = p1_set1_score, p1_set2_score = p1_set2_score, 
                                p1_set3_score = p1_set3_score, p1_set4_score = p1_set4_score, 
                                p1_set5_score = p1_set5_score, p2_set1_score = p2_set1_score, 
                                p2_set2_score = p2_set2_score, p2_set3_score = p2_set3_score, 
                                p2_set4_score = p2_set4_score, p2_set5_score = p2_set5_score, 
                                date_of_match = date_of_match))
      }
    }
    table_i <- table_i + n_rounds
    
    # add tournament name, dates, location, prize_money_tier
    matches_m$tournament = name
    matches_m$dates = dates
    matches_m$location = location
    matches_m$prize_money_tier = gsub(" \\(M\\).*", "", prize_money_tier)
    
    # add to main data frame
    df <- rbind(df, matches_m)
  }
  
  if (women) {
    matches_w <- data.frame(round = character(0), gender = character(0), 
                            player1 = character(0), player2 = character(0), 
                            p1_country = character(0), p2_country = character(0), 
                            p1_seed = character(0), p2_seed = character(0), 
                            p1_sets_won = numeric(0), p2_sets_won = numeric(0), 
                            p1_set1_score = numeric(0), p1_set2_score = numeric(0), 
                            p1_set3_score = numeric(0), p1_set4_score = numeric(0), 
                            p1_set5_score = numeric(0), p2_set1_score = numeric(0), 
                            p2_set2_score = numeric(0), p2_set3_score = numeric(0), 
                            p2_set4_score = numeric(0), p2_set5_score = numeric(0), 
                            date_of_match = character(0))
    n_rounds <- ceiling(log2(n_players_w))
    for (i in 1:n_rounds) {
      round <- tables[[table_i + i]]
      n_matches <- nrow(round)/3
      for (j in 0:(n_matches-1)) {
        j
        date_of_match <- round[3*j + 1,]$V1
        
        p1 <- round[3*j + 2,]
        player1 <- p1$V1
        player1 <- gsub("\\n.*", "", player1)
        p1_sets_won <- p1$V3
        p1_set1_score <- p1$V4
        p1_set2_score <- p1$V5
        p1_set3_score <- p1$V6
        p1_set4_score <- p1$V7
        p1_set5_score <- p1$V8
        p1_seed <- seed_country_w$seed[seed_country_w$player == player1]
        p1_country <- seed_country_w$country[seed_country_w$player == player1]
        
        p2 <- round[3*j + 3,]
        player2 <- p2$V1
        player2 <- gsub("\\n.*", "", player2)
        p2_sets_won <- p2$V3
        p2_set1_score <- p2$V4
        p2_set2_score <- p2$V5
        p2_set3_score <- p2$V6
        p2_set4_score <- p2$V7
        p2_set5_score <- p2$V8
        p2_seed <- seed_country_w$seed[seed_country_w$player == player2]
        p2_country <- seed_country_w$country[seed_country_w$player == player2]
        
        # add to data frame
        matches_w <- rbind(matches_w, 
                           list(round = round_names[i], gender = "w",
                                player1 = player1, player2 = player2, 
                                p1_seed = p1_seed, p2_seed = p2_seed, 
                                p1_country = p1_country, p2_country = p2_country, 
                                p1_sets_won = p1_sets_won, p2_sets_won = p2_sets_won, 
                                p1_set1_score = p1_set1_score, p1_set2_score = p1_set2_score, 
                                p1_set3_score = p1_set3_score, p1_set4_score = p1_set4_score, 
                                p1_set5_score = p1_set5_score, p2_set1_score = p2_set1_score, 
                                p2_set2_score = p2_set2_score, p2_set3_score = p2_set3_score, 
                                p2_set4_score = p2_set4_score, p2_set5_score = p2_set5_score, 
                                date_of_match = date_of_match))
      }
    }
    table_i <- table_i + n_rounds
    
    # add tournament name, dates, location, prize_money_tier
    matches_w$tournament = name
    matches_w$dates = dates
    matches_w$location = location
    matches_w$prize_money_tier = trimws(gsub(" \\(W\\)", "", gsub(".*\\/", "", prize_money_tier)))
    
    # add to main data frame
    df <- rbind(df, matches_w)
  }
  return(df)
}

df <- data.frame(round = character(0), gender = character(0), 
                 player1 = character(0), player2 = character(0), 
                 p1_seed = character(0), p2_seed = character(0), 
                 p1_country = character(0), p2_country = character(0), 
                 p1_sets_won = numeric(0), p2_sets_won = numeric(0), 
                 p1_set1_score = numeric(0), p1_set2_score = numeric(0), 
                 p1_set3_score = numeric(0), p1_set4_score = numeric(0), 
                 p1_set5_score = numeric(0), p2_set1_score = numeric(0), 
                 p2_set2_score = numeric(0), p2_set3_score = numeric(0), 
                 p2_set4_score = numeric(0), p2_set5_score = numeric(0), 
                 date_of_match = character(0), tournament = character(0),
                 dates = character(0), location = character(0), 
                 prize_money_tier = character(0))

filenames <- list.files('rawdata/tournaments/rds/')
for (i in 1:length(filenames)) {
  df <- extractResults(gsub(".rds", "", filenames[i]), df)
}

write.csv(df, file="data/squash.csv")


