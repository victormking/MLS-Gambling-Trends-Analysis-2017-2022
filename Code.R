library(dplyr)
library(ggplot2)
library(readr)

# Read the data and check the column names
mls <- read_csv("621 - Gambling/Exam 2/Q2/MLS_2017-22.csv")
colnames(mls)

# Choose the appropriate column name(s) for `Road`
# Assuming the columns are now named 'Road...3' and 'Road...4'

mls <- mls %>%
  rename(RoadScore = `Road...3`) %>%
  mutate(
    Home_Win = as.integer(HomeScore > RoadScore),
    Road_Win = as.integer(HomeScore < RoadScore),
    Draw = as.integer(HomeScore == RoadScore),
    Home_Fav = as.integer(HomeOdds < RoadOdds),
    Away_Fav = as.integer(RoadOdds < HomeOdds),
    bet_on_home = 1,
    amount_bet = 1,  # Assuming a fixed bet amount of 1 unit
    potential_return = ifelse(Home_Win == 1, HomeOdds * amount_bet, 0),
    net_return = potential_return - amount_bet,
    bet_on_draw = 1,
    amount_bet_Draw = 1,  # Assuming a fixed bet amount of 1 unit
    potential_return_Draw = ifelse(Draw == 1, DrawOdds * amount_bet_Draw, 0),
    net_return_Draw = potential_return_Draw - amount_bet_Draw,
    bet_on_away = 1,
    amount_bet_Away = 1,  # Assuming a fixed bet amount of 1 unit
    potential_return_Away = ifelse(Road_Win == 1, RoadOdds * amount_bet_Away, 0),
    net_return_Away = potential_return_Away - amount_bet_Away
  )

# Calculate total returns for each strategy
total_home_returns <- sum(mls$net_return, na.rm = TRUE)
total_draw_returns <- sum(mls$net_return_Draw, na.rm = TRUE)
total_away_returns <- sum(mls$net_return_Away, na.rm = TRUE)

# Create and visualize strategy returns
strategy_returns <- data.frame(
  Strategy = c("Home_Win", "Draw", "Road_Win"),
  Returns = c(total_home_returns, total_draw_returns, total_away_returns)
)

ggplot(strategy_returns, aes(x = Strategy, y = Returns, fill = Strategy)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ggtitle("Returns from Different Betting Strategies")

