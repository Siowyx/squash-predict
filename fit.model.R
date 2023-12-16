library(dplyr)
library(lme4)


fit_logistic_model_for_win_prob <- function(df, seasons) {
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
  
  # create a new data frame with player coefficient matrix and p1_win
  # player1 = 1, player2 = -1, else 0
  
  # men
  df2_m <- df2[df2$gender == 'm',]
  all_players_m <- sort(unique(c(df2_m$player1, df2_m$player2)))
  player_coef_matrix <- matrix(ncol = length(all_players_m), nrow = nrow(df2_m))
  
  for (i in 1:nrow(df2_m)) {
    coef <- rep(0, length(all_players_m))
    coef[which(all_players_m == df2_m[i,]$player1)] <- 1
    coef[which(all_players_m == df2_m[i,]$player2)] <- -1
    player_coef_matrix[i,] <- coef
  }
  
  d_m <- data.frame(player_coef_matrix, p1_win = df2_m$p1_win)
  colnames(d_m) <- c(all_players_m, 'p1_win')
  
  # women
  df2_w <- df2[df2$gender == 'w',]
  all_players_w <- sort(unique(c(df2_w$player1, df2_w$player2)))
  player_coef_matrix <- matrix(ncol = length(all_players_w), nrow = nrow(df2_w))
  
  for (i in 1:nrow(df2_w)) {
    coef <- rep(0, length(all_players_w))
    coef[which(all_players_w == df2_w[i,]$player1)] <- 1
    coef[which(all_players_w == df2_w[i,]$player2)] <- -1
    player_coef_matrix[i,] <- coef
  }
  
  d_w <- data.frame(player_coef_matrix, p1_win = df2_w$p1_win)
  colnames(d_w) <- c(all_players_w, 'p1_win')
  
  # # check number of matches per player
  # colSums(sapply(d_m, function(x){x != 0}))
  # colSums(sapply(d_w, function(x){x != 0}))
  
  
  # Fit logistic regression model
  model_m <- glm(p1_win ~ ., family = binomial, data = d_m)
  model_w <- glm(p1_win ~ ., family = binomial, data = d_w)

  return(list(model_m, model_w))
}


# do logistic regression for the past 5 seasons
df <- read.csv("data/cleaned_squash.csv")
df <- subset(df, select = -X)

seasons <- c("2023-2024", "2022-2023", "2021-2022", "2020-2021", "2019-2020")

models <- fit_logistic_model_for_win_prob(df, seasons)
model_m <- models[[1]]
model_w <- models[[2]]
summary(model_m)
summary(model_w)

# check the coefficients
sort(coefficients(model_m), decreasing = TRUE)
sort(coefficients(model_w), decreasing = TRUE)


# Predict probabilities
predict_win_prob <- function(model, p1, p2) {
  all_players <- gsub("`", "", names(coefficients(model))[-1])
  
  new_data <- data.frame(matrix(ncol = length(all_players), nrow = 1))
  colnames(new_data) <- all_players
  
  coef <- rep(0, length(all_players))
  coef[which(all_players == p1)] <- 1
  coef[which(all_players == p2)] <- -1
  new_data[1,] <- coef
  
  win_prob <- predict(model, newdata = new_data, type = "response")
  
  return(win_prob[[1]])
}

models <- readRDS("models/model2019-2024.rds")
model_m <- models[[1]]
model_w <- models[[2]]

predict_win_prob(model_m, "Ali Farag", "Mostafa Asal")
predict_win_prob(model_m, "Mostafa Asal", "Ali Farag")
predict_win_prob(model_m, "Ali Farag", "Ong Sai Hung")

predict_win_prob(model_w, "Nouran Gohar", "Amanda Sobhy")
predict_win_prob(model_w, "Amanda Sobhy", "Nouran Gohar")
predict_win_prob(model_w, "Amanda Sobhy", "Aira Azman")
predict_win_prob(model_w, "Aira Azman", "Amanda Sobhy")



# the model actually perform really well, the coefficients is similar to the current rankings
# next to try predict games score?, predict win probability after being 1-0 up etc




