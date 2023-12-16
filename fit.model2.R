# fit and save models for later use

library(dplyr)
library(lme4)

# use function in fit.model.R to fit models with 2 consecutive seasons since 2000
df <- read.csv("data/cleaned_squash.csv")
df <- subset(df, select = -X)

for (i in 2000:2022) {
  seasons <- c(paste0(as.character(i), "-", as.character(i+1)), paste0(as.character(i+1), "-", as.character(i+2)))
  models <- fit_logistic_model_for_win_prob(df, seasons)
  if (!file.exists("models/")) {
    # create new directories
    dir.create("models/")
  }
  saveRDS(models, paste0("models/model", as.character(i), "-", as.character(i+2), ".rds"))
}

models <- readRDS("models/model2000-2002.rds")
model_m <- models[[1]]
model_w <- models[[2]]
summary(model_m)
summary(model_w)




seasons <- c()
for (i in 2000:2023) {
  seasons <- c(seasons, paste0(as.character(i), "-", as.character(i+1)))
}
# fit logistic model for all seasons since 2000
models <- fit_logistic_model_for_win_prob(df, seasons)
model_m <- models[[1]]
model_w <- models[[2]]
summary(model_m)
summary(model_w)

sort(coefficients(model_m), decreasing = TRUE)
sort(coefficients(model_w), decreasing = TRUE)

saveRDS(models, "models/model2000-2024.rds")

# don't think the model with all seasons since 2000 is meaningful




# fit model for th most recent n=1,...,10 seasons
for (i in 1:10) {
  seasons <- c()
  for (j in 2024:(2024-i+1)) {
    seasons <- c(seasons, paste0(as.character(j-1), "-", as.character(j)))
  }
  models <- fit_logistic_model_for_win_prob(df, seasons)
  saveRDS(models, paste0("models/model", as.character(2024-i), "-2024.rds"))
}





