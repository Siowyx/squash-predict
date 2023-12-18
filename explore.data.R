library(dplyr)
library(ggplot2)
library(pubtheme)


# check number of matches played per player
df <- read.csv("data/cleaned_squash.csv")
df <- subset(df, select = -X)
seasons <- ("2023-2024")
df2 <- df %>%
  filter(season %in% seasons)

all_players <- unique(c(df2$player1, df2$player2))
n_matches_per_player <- rep(0, length(all_players))
for (i in 1:length(all_players)) {
  n_matches_per_player[i] <- nrow(df2[df2$player1 == all_players[i],]) + nrow(df2[df2$player2 == all_players[i],])
}

d <- data.frame(all_players, n_matches_per_player)
dg <- d %>%
  group_by(n_matches_per_player) %>%
  summarise(player_count = n())

g = ggplot(dg, aes(x=n_matches_per_player, y=player_count))+
  geom_col(width = 0.8)+ 
  labs(title    = "Player Count per Number of Matches Played in 2023-24 Season",
       x = 'Number of Matches Played in 2023-24 Season', 
       y = 'Player Count')
g %>%
  pub(type='bar')


# players with less than 5 matches coded as "Others"
for (i in 1:length(all_players)) {
  if (n_matches_per_player[i] <= 5) {
    df2 <- df2 %>%
      mutate(player1 = ifelse(player1 == all_players[i], "Others", player1),
             player2 = ifelse(player2 == all_players[i], "Others", player2))
  }
}
# drop rows where both player1 and player2 are "Others"
df2 <- df2[!(df2$player1 %in% "Others" & df2$player2 %in% "Others"),]


# check win rate 
d_m <- df2 %>%
  filter(gender == "m")

d_m1 <- d_m %>%
  group_by(player1) %>%
  summarise(matches_won = sum(p1_win), n <- n())
colnames(d_m1) <- c("player", "matches_won", "n")

d_m2 <- d_m %>%
  group_by(player2) %>%
  summarise(matches_won = n() - sum(p1_win), n <- n())
colnames(d_m2) <- c("player", "matches_won", "n")

d_m1 <- merge(d_m1, d_m2, by = "player", all = TRUE)
d_m1 <- d_m1 %>%
  replace_na(list(matches_won.x = 0, n.x = 0, matches_won.y = 0, n.y = 0)) %>%
  mutate(win_rate = (matches_won.x + matches_won.y)/(n.x + n.y)) %>%
  arrange(desc(win_rate))

g2 = ggplot(d_m1[1:10,], aes(x=win_rate, y=factor(player, levels = rev(player))))+
  geom_col(width = 0.8)+ 
  labs(title    = "Male Players with the Highest Win Rate in 2023-24 Season",
       x = 'Win Rate', 
       y = NULL)
g2 %>%
  pub(type='bar')




















