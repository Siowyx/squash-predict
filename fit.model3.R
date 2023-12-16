# fit logistic model for predicting game score or predict win probability after being 1-0 up etc

library(dplyr)
library(lme4)


# pivot longer then wider 
# pivot longer 2 rows per game
# game, game no, tewm1score, tem2 score, 



fit_logistic_model_for_dynamic_win_prob <- function(df, seasons) {
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
  
  
  # create a new data frame with player coefficient matrix, p1_sets_won, p2_sets_won, and p1_win
  # p1_sets_won, p2_sets_won indicate the current game score e.g. 1-1, 2-1
  # player1 = 1, player2 = -1, else 0
  
  # men
  df2_m <- df2[df2$gender == 'm',]
  all_players_m <- sort(unique(c(df2_m$player1, df2_m$player2)))

  d_m <- data.frame(matrix(nrow = 0, ncol = length(all_players_m) + 3))
  for (i in 1:nrow(df2_m)) {
    print(paste0(i, "/", nrow(df2_m)))
    
    coef <- rep(0, length(all_players_m))
    coef[which(all_players_m == df2_m[i,]$player1)] <- 1
    coef[which(all_players_m == df2_m[i,]$player2)] <- -1
    
    p1_sets_won <- 0
    p2_sets_won <- 0
    for (j in 1:5) {
      p1_score <- df2_m[i, paste0("p1_set", as.character(j), "_score")]
      p2_score <- df2_m[i, paste0("p2_set", as.character(j), "_score")]
      if (!is.na(p1_score) & !is.na(p2_score)) {
        if (p1_score > p2_score) {
          p1_sets_won <- p1_sets_won + 1
        } else {
          p2_sets_won <- p2_sets_won + 1
        }
        d_m <- rbind(d_m, c(coef, p1_sets_won, p2_sets_won, df2_m[i,]$p1_win))
      }
    }
  }
  colnames(d_m) <- c(all_players_m, 'p1_sets_won', 'p2_sets_won', 'p1_win')    #heerrree
  
  # women
  df2_w <- df2[df2$gender == 'w',]
  all_players_w <- sort(unique(c(df2_w$player1, df2_w$player2)))

  d_w <- data.frame(matrix(nrow = 0, ncol = length(all_players_w) + 3))
  for (i in 1:nrow(df2_w)) {
    print(paste0(i, "/", nrow(df2_m)))
    
    coef <- rep(0, length(all_players_w))
    coef[which(all_players_w == df2_w[i,]$player1)] <- 1
    coef[which(all_players_w == df2_w[i,]$player2)] <- -1
    
    p1_sets_won <- 0
    p2_sets_won <- 0
    for (j in 1:5) {
      p1_score <- df2_w[i, paste0("p1_set", as.character(j), "_score")]
      p2_score <- df2_w[i, paste0("p2_set", as.character(j), "_score")]
      if (!is.na(p1_score) & !is.na(p2_score)) {
        if (p1_score > p2_score) {
          p1_sets_won <- p1_sets_won + 1
        } else {
          p2_sets_won <- p2_sets_won + 1
        }
        d_w <- rbind(d_w, c(coef, p1_sets_won, p2_sets_won, df2_w[i,]$p1_win))
      }
    }
  }
  colnames(d_w) <- c(all_players_w, 'p1_sets_won', 'p2_sets_won', 'p1_win')

  
  # Fit logistic regression model
  model_m <- glm(p1_win ~ ., family = binomial, data = d_m)
  model_w <- glm(p1_win ~ ., family = binomial, data = d_w)
  
  return(list(model_m, model_w))
}




df <- read.csv("data/cleaned_squash.csv")
df <- subset(df, select = -X)

seasons <- c("2023-2024", "2022-2023", "2021-2022")
# seasons <- c("2023-2024", "2022-2023", "2021-2022", "2020-2021", "2019-2020")

# # takes a long time to run
# models <- fit_logistic_model_for_dynamic_win_prob(df, seasons)
# model_m <- models[[1]]
# model_w <- models[[2]]
# summary(model_m)
# summary(model_w)

# # save model
# saveRDS(models, "models/model_dynamic2021-2024.rds")


# seasons <- c("2023-2024", "2022-2023", "2021-2022")
# models <- fit_logistic_model_for_dynamic_win_prob(df, seasons)
# saveRDS(models, "models/model_dynamic2021-2024.rds")
# 
# seasons <- c("2023-2024", "2022-2023")
# models <- fit_logistic_model_for_dynamic_win_prob(df, seasons)
# saveRDS(models, "models/model_dynamic2022-2024.rds")
# 
# seasons <- c("2023-2024")
# models <- fit_logistic_model_for_dynamic_win_prob(df, seasons)
# saveRDS(models, "models/model_dynamic2023-2024.rds")





# check the coefficients
sort(coefficients(model_m), decreasing = TRUE)
sort(coefficients(model_w), decreasing = TRUE)


# Predict probabilities
predict_dynamic_win_prob <- function(model, p1, p2, p1_sets_won, p2_sets_won) {
  all_players <- gsub("`", "", names(coefficients(model))[-1])
  all_players <- all_players[all_players != "p1_sets_won" & all_players != "p2_sets_won"]
  
  new_data <- data.frame(matrix(ncol = length(all_players) + 2, nrow = 1))
  colnames(new_data) <- c(all_players, 'p1_sets_won', 'p2_sets_won')
  
  coef <- rep(0, length(all_players))
  coef[which(all_players == p1)] <- 1
  coef[which(all_players == p2)] <- -1
  new_data[1,] <- c(coef, p1_sets_won, p2_sets_won)
  
  win_prob <- predict(model, newdata = new_data, type = "response")
  
  return(win_prob[[1]])
}

models_dynamic <- readRDS("models/model_dynamic2019-2024.rds")
model_dynamic_m <- models_dynamic[[1]]
model_dynamic_w <- models_dynamic[[2]]

predict_dynamic_win_prob(model_dynamic_m, "Ali Farag", "Mostafa Asal", 0, 0)
predict_dynamic_win_prob(model_dynamic_m, "Ali Farag", "Mostafa Asal", 1, 0)
predict_dynamic_win_prob(model_dynamic_m, "Ali Farag", "Mostafa Asal", 3, 0)
predict_dynamic_win_prob(model_dynamic_m, "Mostafa Asal", "Ali Farag", 1, 0)
predict_dynamic_win_prob(model_dynamic_m, "Ali Farag", "Mostafa Asal", 0, 1)
predict_dynamic_win_prob(model_dynamic_m, "Ali Farag", "Mostafa Asal", 0, 2)
predict_dynamic_win_prob(model_dynamic_m, "Ali Farag", "Mostafa Asal", 0, 3)
predict_dynamic_win_prob(model_dynamic_m, "Ali Farag", "Ong Sai Hung", 0, 3)
predict_dynamic_win_prob(model_dynamic_m, "Ong Sai Hung", "Ali Farag", 3, 0)

predict_dynamic_win_prob(model_dynamic_w, "Nouran Gohar", "Amanda Sobhy", 0, 0)


