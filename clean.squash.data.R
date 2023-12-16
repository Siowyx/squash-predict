library(dplyr)

df <- read.csv("data/squash.csv")
df <- subset(df, select = -X)


# add a column p1_win (p1 win =1, p2 win = 0) indicating which player won
df$p1_win = NA 
df <- df %>% 
  mutate(p1_win = ifelse(p1_sets_won > p2_sets_won, 1, 0))
 

# clean date_of_match
df <- df %>%
  mutate(date_of_match = gsub(" \\|.*", "", date_of_match)) %>%
  mutate(date_of_match = as.Date(date_of_match, "%d %B %Y"))


# remove rows with invalid player e.g. "-", "04 Jun 2021 | 12:15"
df <- df %>%
  filter(!grepl("\\d", player2)) %>%
  filter(!grepl("-", player2)) %>%
  filter(!grepl("-", player1))


# constants for round names
round_names <- c("Final", "Semifinal", "Quarterfinal", "Round of 16", "Round of 32", "Round of 64")

# new season starts from august 
aug_to_dec <- c("Aug", "Sep", "Oct", "Nov", "Dec")

# For date_of_match == NA, assigns date for each round counting backwards from the last day, one round per day
# Some tournaments might have two matches per day or one round played over two days but I think it shouldn't matter much
tournaments_NA_dates <- unique(df[is.na(df$date_of_match),]$tournament)
for (t in tournaments_NA_dates) {
  temp <- df %>% filter(tournament == t)
  
  temp_w <- temp %>% filter(gender == "w")
  if (sum(is.na(temp_w$date_of_match)) > 0) {
    end_date <- gsub(".*- ", "", temp_w$dates[1])
    if (gsub("\\d* ", "", end_date) %in% aug_to_dec) { 
      yr <- gsub("-.*", "", temp_w$season[1])
    } else {
      yr <- gsub(".*-", "", temp_w$season[1])
    }
    end_date <- as.Date(paste(end_date, yr), "%d %B %Y")
    for (i in 1:length(round_names)) {
      df <- df %>% 
        mutate(date_of_match = 
                 ifelse(tournament == t & gender == "w" & is.na(date_of_match) & round == round_names[i],
                        end_date - i + 1,
                        date_of_match))
    }
  }
  
  temp_m <- temp %>% filter(gender == "m")
  if (sum(is.na(temp_m$date_of_match)) > 0) {
    end_date <- gsub(".*- ", "", temp_m$dates[1])
    if (gsub("\\d* ", "", end_date) %in% aug_to_dec) { 
      yr <- gsub("-.*", "", temp_m$season[1])
    } else {
      yr <- gsub(".*-", "", temp_m$season[1])
    }
    end_date <- as.Date(paste(end_date, yr), "%d %B %Y")
    for (i in 1:length(round_names)) {
      df <- df %>% 
        mutate(date_of_match = 
                 ifelse(tournament == t & gender == "m" & is.na(date_of_match) & round == round_names[i],
                        end_date - i + 1,
                        date_of_match))
    }
  }
}
df$date_of_match <- as.Date.numeric(df$date_of_match)


# drop NA scores
df <- df %>% 
  filter(!is.na(p1_set1_score))


# fill in seed if possible from player name e.g. "Maxwell Orr    (WC)"
df <- df %>% 
  mutate(p1_seed = ifelse(is.na(p1_seed) & grepl("\\(", player1), trimws(gsub(".*\\((.*)\\).*", "\\1", player1)), p1_seed),
         player1 = ifelse(is.na(p1_seed) & grepl("\\(", player1), trimws(gsub("\\(.*", "", player1)), player1)) %>% 
  mutate(p2_seed = ifelse(is.na(p2_seed) & grepl("\\(", player2), trimws(gsub(".*\\((.*)\\).*", "\\1", player2)), p2_seed),
         player2 = ifelse(is.na(p2_seed) & grepl("\\(", player2), trimws(gsub("\\(.*", "", player2)), player2)) %>%
  mutate(player1 = trimws(gsub("\\(.*", "", player1)),
         player2 = trimws(gsub("\\(.*", "", player2)))


# fill in countries if possible based on other rows
players_NA_country <- unique(c(df[is.na(df$p1_country),]$player1, df[is.na(df$p2_country),]$player2))
for (i in 1:length(players_NA_country)) {
  country <- NA
  if (players_NA_country[i] %in% df$player1) {
    countries <- unique(df[df$player1 == players_NA_country[i],]$p1_country)
    country <- countries[1]
    df <- df %>%
      mutate(p1_country = ifelse(
        player1 == players_NA_country[i] & unlist(lapply(p1_country, function(x){x == "" || is.na(x)})),
        country, 
        p1_country
        ))
  }
  
  if (players_NA_country[i] %in% df$player2) {
    if (is.na(country)) {
      countries <- unique(df[df$player2 == players_NA_country[i],]$p2_country)
      country <- countries[1]
    }
    df <- df %>%
      mutate(p2_country = ifelse(
        player2 == players_NA_country[i] & unlist(lapply(p2_country, function(x){x == "" || is.na(x)})),
        country, 
        p2_country
      ))
  }
}


# add a continuous prize money column
df$prize_money <- as.numeric(gsub("\\$", "", gsub(",", "", df$prize_money_tier)))


# miscellaneous
# AS Insurance Services Ltd NM Academy Open 2021 is men's event and round is wrong
df <- df %>%
  mutate(gender = ifelse(tournament == "AS Insurance Services Ltd NM Academy Open 2021", 'm', gender),
         round = ifelse(tournament == "AS Insurance Services Ltd NM Academy Open 2021",
                        case_when(
                          round == "Semifinal"    ~ "Final",
                          round == "Quarterfinal" ~ "Semifinal",
                          round == "Round of 16"  ~ "Quarterfinal",
                          .default = round
                        ), 
                        round)
         )

# CIB PSA World Tour Finals is a special end-of-season event with round-robin format
# This messes with the format assumed when pulling data (keep or drop?)
# CIB PSA World Tour Finals 2022-2023
for (i in 66724:66734) {
  df[i,]$round = "Round Robin"
  if (i >= 66727) {
    df[i,]$gender = "m"
  }
}
df[66735,]$round = "Final"  # only women's final pulled from website

# CIB PSA World Tour Finals 2021/2022
for (i in 59602:59613) {
  df[i,]$round = "Round Robin"
  if (i >= 59606) {
    df[i,]$gender = "m"
  }
}
df[59614,]$round = "Final"  # only women's final pulled from website

# CIB PSA World Tour Finals 2020-2021
for (i in 56588:56599) {
  df[i,]$round = "Round Robin"
  if (i >= 56592) {
    df[i,]$gender = "m"
  }
}
df[56600,]$round = "Final"  # only women's final pulled from website

# CIB PSA World Tour Finals 2019-2020
for (i in 56572:56583) {
  df[i,]$round = "Round Robin"
  if (i >= 56576) {
    df[i,]$gender = "m"
  }
}
df[56584,]$round = "Final"  # only women's final pulled from website

# CIB PSA World Tour Finals 2018-2019
for (i in 49432:49443) {
  df[i,]$round = "Round Robin"
  if (i >= 49436) {
    df[i,]$gender = "m"
  }
}
df[49444,]$round = "Final"  # only women's final pulled from website

# `Abdulla Mohd Al Tamimi` as `Abdulla Altamimi`
df <- df %>%
  mutate(player1 = ifelse(player1 == "Abdulla Mohd Al Tamimi", "Abdulla Altamimi", player1),
         player2 = ifelse(player2 == "Abdulla Mohd Al Tamimi", "Abdulla Altamimi", player2))

# Final of womens Edinburgh Sports Club Open 2016 appeared twice and one wrongly labeled as mens
df <- df %>%
  filter(!(gender == 'm' & tournament == "Edinburgh Sports Club Open 2016"))

# save cleaned data
write.csv(df, file="data/cleaned_squash.csv")



