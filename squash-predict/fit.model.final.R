# fit logistic model for predicting game win probability

library(dplyr)
library(tidyverse)
library(lme4)


fit_logistic_model_for_game_win_prob <- function(df, seasons) {
  df2 <- df %>%
    filter(season %in% seasons)
  
  all_players <- unique(c(df2$player1, df2$player2))
  n_matches_per_player <- rep(0, length(all_players))
  for (i in 1:length(all_players)) {
    n_matches_per_player[i] <- nrow(df2[df2$player1 == all_players[i],]) + nrow(df2[df2$player2 == all_players[i],])
    
    # players with less than a certain number of matches coded as 'Others'
    c <- length(seasons)*5
    if (n_matches_per_player[i] <= min(c, 30)) {
      df2 <- df2 %>%
        mutate(player1 = ifelse(player1 == all_players[i], "Others", player1),
               player2 = ifelse(player2 == all_players[i], "Others", player2))
    }
  }
  
  # drop rows where both player1 and player2 are "Others"
  df2 <- df2[!(df2$player1 %in% "Others" & df2$player2 %in% "Others"),]
  
  # create a new data frame with player coefficient matrix, p1_set_win
  # player1 = 1, player2 = -1, else 0
  
  # men
  d_m <- df2[df2$gender == 'm',]
  all_players_m <- sort(unique(c(d_m$player1, d_m$player2)))
  
  # only keep the player1, player2 columns and create new columns for p1_set1_win etc
  d_m <- d_m %>%
    select(player1, player2, p1_set1_score, p1_set2_score, p1_set3_score, p1_set4_score, p1_set5_score,
           p2_set1_score, p2_set2_score, p2_set3_score, p2_set4_score, p2_set5_score) %>%
    mutate(p1_set1_win = ifelse(p1_set1_score > p2_set1_score, 1, 0),
           p1_set2_win = ifelse(p1_set2_score > p2_set2_score, 1, 0),
           p1_set3_win = ifelse(is.na(p1_set3_score) | is.na(p2_set3_score),
                                NA,
                                ifelse(p1_set3_score > p2_set3_score, 1, 0)),
           p1_set4_win = ifelse(is.na(p1_set4_score) | is.na(p2_set4_score),
                                NA,
                                ifelse(p1_set4_score > p2_set4_score, 1, 0)),
           p1_set5_win = ifelse(is.na(p1_set5_score) | is.na(p2_set5_score),
                                NA,
                                ifelse(p1_set5_score > p2_set5_score, 1, 0))) %>%
    select(-c("p1_set1_score", "p1_set2_score", "p1_set3_score", "p1_set4_score", "p1_set5_score",
              "p2_set1_score", "p2_set2_score", "p2_set3_score", "p2_set4_score", "p2_set5_score"))
  
  # turn player1, player2 into players coefficient matrix
  for (player in all_players_m) {
    d_m[,`player`] <- 0
  }
  d_m <- d_m[, c(all_players_m, "p1_set1_win", "p1_set2_win", "p1_set3_win", "p1_set4_win", "p1_set5_win", 
                 "player1", "player2")]
  for (i in 1:nrow(d_m)) {
    d_m[i, which(all_players_m == d_m[i,]$player1)] <- 1
    d_m[i, which(all_players_m == d_m[i,]$player2)] <- -1
  }
  
  # pivot longer to have one row per set
  d_m <- d_m %>% 
    pivot_longer(cols=c("p1_set1_win", "p1_set2_win", "p1_set3_win", "p1_set4_win", "p1_set5_win"), 
                 values_to = "p1_set_win", values_drop_na = TRUE) %>%
    select(-c("player1", "player2", "name"))
  
  
  # women
  d_w <- df2[df2$gender == 'w',]
  all_players_w <- sort(unique(c(d_w$player1, d_w$player2)))
  
  # only keep the player1, player2 columns and create new columns for p1_set1_win etc
  d_w <- d_w %>%
    select(player1, player2, p1_set1_score, p1_set2_score, p1_set3_score, p1_set4_score, p1_set5_score,
           p2_set1_score, p2_set2_score, p2_set3_score, p2_set4_score, p2_set5_score) %>%
    mutate(p1_set1_win = ifelse(p1_set1_score > p2_set1_score, 1, 0),
           p1_set2_win = ifelse(p1_set2_score > p2_set2_score, 1, 0),
           p1_set3_win = ifelse(is.na(p1_set3_score) | is.na(p2_set3_score),
                                NA,
                                ifelse(p1_set3_score > p2_set3_score, 1, 0)),
           p1_set4_win = ifelse(is.na(p1_set4_score) | is.na(p2_set4_score),
                                NA,
                                ifelse(p1_set4_score > p2_set4_score, 1, 0)),
           p1_set5_win = ifelse(is.na(p1_set5_score) | is.na(p2_set5_score),
                                NA,
                                ifelse(p1_set5_score > p2_set5_score, 1, 0))) %>%
    select(-c("p1_set1_score", "p1_set2_score", "p1_set3_score", "p1_set4_score", "p1_set5_score",
              "p2_set1_score", "p2_set2_score", "p2_set3_score", "p2_set4_score", "p2_set5_score"))
  
  # turn player1, player2 into players coefficient matrix
  for (player in all_players_w) {
    d_w[,`player`] <- 0
  }
  d_w <- d_w[, c(all_players_w, "p1_set1_win", "p1_set2_win", "p1_set3_win", "p1_set4_win", "p1_set5_win", 
                 "player1", "player2")]
  for (i in 1:nrow(d_w)) {
    d_w[i, which(all_players_w == d_w[i,]$player1)] <- 1
    d_w[i, which(all_players_w == d_w[i,]$player2)] <- -1
  }
  
  # pivot longer to have one row per set
  d_w <- d_w %>% 
    pivot_longer(cols=c("p1_set1_win", "p1_set2_win", "p1_set3_win", "p1_set4_win", "p1_set5_win"), 
                 values_to = "p1_set_win", values_drop_na = TRUE) %>%
    select(-c("player1", "player2", "name"))
  
  
  # Fit logistic regression model
  model_m <- glm(p1_set_win ~ . - Others, family = binomial, data = d_m)
  model_w <- glm(p1_set_win ~ . - Others, family = binomial, data = d_w)
  
  return(list(model_m, model_w))
}

predict_game_win_prob <- function(model, p1, p2) {
  all_players <- gsub("`", "", names(coefficients(model)))
  all_players[1] <- "Others"
  
  new_data <- data.frame(matrix(ncol = length(all_players), nrow = 1))
  colnames(new_data) <- all_players
  
  coef <- rep(0, length(all_players))
  coef[which(all_players == p1)] <- 1
  coef[which(all_players == p2)] <- -1
  new_data[1,] <- coef
  
  win_prob <- predict(model, newdata = new_data, type = "response")
  
  return(win_prob[[1]])
}

markov_transition_matrix <- function(win_prob) {
  lose_prob <- 1-win_prob
  
  scores <- factor(c("0-0", "1-0", "0-1", "2-0", "1-1", "0-2", "3-0", "2-1", "1-2", "0-3", 
                     "3-1", "2-2", "1-3", "3-2", "2-3"))
  m <- matrix(nrow = 15, ncol = 15)
  rownames(m) <- scores
  colnames(m) <- scores
  
  for (score in scores) {
    s <- unlist(strsplit(score, "-"))
    if (as.numeric(s[1]) == 3 | as.numeric(s[2]) == 3) next
    win_s <- paste0(as.character(as.numeric(s[1]) + 1), "-", s[2])
    lose_s <- paste0(s[1], "-", as.character(as.numeric(s[2]) + 1))
    m[score, win_s] <- win_prob
    m[score, lose_s] <- lose_prob
  }
  
  m["3-0", "3-0"] <- 1
  m["0-3", "0-3"] <- 1
  m["3-1", "3-1"] <- 1
  m["1-3", "1-3"] <- 1
  m["3-2", "3-2"] <- 1
  m["2-3", "2-3"] <- 1
  
  m[is.na(m)] <- 0
  
  return(m)
}

predict_score_prob <- function(model, player1, player2, p1_set_won, p2_set_won) {
  win_prob <- predict_game_win_prob(model, player1, player2)
  m <- markov_transition_matrix(win_prob)
  
  score_prob <- rep(0, 15)
  score_prob[which(rownames(m) == paste0(p1_set_won, "-", p2_set_won))] = 1
  for (i in (p1_set_won+p2_set_won):5) {
    score_prob <- score_prob %*% m
  }
  return(score_prob)
}



