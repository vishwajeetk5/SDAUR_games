# Load required libraries
library(dplyr)
library(ggplot2)

# Data Wrangling
games <- data.frame(Video_Games_Sales_as_at_22_Dec_2016)
head(games)
str(games)
nrow(games)
games2 <- filter(games, Year_of_Release > 2011)
nrow(games2)
games3 <- clean_names(games2)
colnames(games2)
# any duplicates
anyDuplicated(games3)
# Remove rows with unknown year of release.
games4 <- filter(games3, year_of_release != "NA")

# Problem Statement 1: Publishers and Developers with Highest-Rated Games
publisher_avg_scores <- aggregate(critic_score + user_score ~ publisher, games4, mean)
sorted_publisher_avg_scores <- publisher_avg_scores[order(publisher_avg_scores$critic_score, decreasing = TRUE), ]
cat("Publishers with the highest average critic scores:\n")
print(sorted_publisher_avg_scores[1:5, ])

developer_avg_scores <- aggregate(critic_score + user_score ~ developer, games4, mean)
sorted_developer_avg_scores <- developer_avg_scores[order(developer_avg_scores$critic_score, decreasing = TRUE), ]
cat("\nDevelopers with the highest average critic scores:\n")
print(sorted_developer_avg_scores[1:5, ])

# Problem Statement 2: Game Genre for Ubisoft Investment
genre_sales <- games4 %>%
  group_by(genre) %>%
  summarise(avg_global_sales = mean(global_sales, na.rm = TRUE))
most_profitable_genre <- genre_sales[which.max(genre_sales$avg_global_sales),]
most_profitable_genre

# Problem Statement 3: Correlation between User and Critic Scores
user_scores <- games4$user_score
critic_scores <- games4$critic_score
correlation_coefficient <- cor(user_scores, critic_scores)
cat("Correlation coefficient between user and critic scores: ", correlation_coefficient, "\n")
ggplot(games4, aes(x = user_score, y = critic_score)) +
  geom_point() +
  labs(title = "Correlation Between User and Critic Scores",
       x = "User Scores",
       y = "Critic Scores") +
  theme_minimal()

# Problem Statement 4: Probability of Game Receiving 'M' Rating
total_M_games <- sum(games4$rating == 'M')
total_games <- nrow(games4)
probability_M_given_developer <- total_M_games / total_games

set.seed(123) # Set seed for reproducibility
sample_size <- 500 # Define sample size
sample_data <- games4[sample(nrow(games4), sample_size), ]
sample_M_games <- sum(sample_data$rating == 'M')
sample_probability_M_given_developer <- sample_M_games / sample_size

cat("Probability of a game receiving an 'M' rating given the developer's previous game ratings in the entire population: ", probability_M_given_developer, "\n")
cat("Probability of a game receiving an 'M' rating given the developer's previous game ratings in the sample: ", sample_probability_M_given_developer, "\n")

# Problem Statement 5: Evolution of Global Game Sales Across Platforms
games4$year_of_release <- as.numeric(games4$year_of_release)
platform_sales <- games4 %>%
  group_by(platform, year_of_release) %>%
  summarise(total_global_sales = sum(global_sales))

ggplot(platform_sales, aes(x = year_of_release, y = total_global_sales, color = platform)) +
  geom_line() +
  labs(title = "Evolution of Global Game Sales Across Platforms",
       x = "Year of Release",
       y = "Total Global Sales") +
  theme_minimal()

# Problem Statement 6: Number of Games Released Each Year by Genre
games_per_year_genre <- games4 %>%
  group_by(year_of_release, genre) %>%
  summarise(num_games = n())

# Plotting the number of games released each year by genre
ggplot(games_per_year_genre, aes(x = year_of_release, y = num_games, color = genre)) +
  geom_line() +
  labs(title = "Number of Games Released Each Year by Genre",
       x = "Year of Release",
       y = "Number of Games") +
  theme_minimal()

# Problem Statement 7: Top Publishers and Developers by Global Sales
top_publishers_global_sales <- games4 %>%
  group_by(publisher) %>%
  summarise(total_global_sales = sum(global_sales)) %>%
  top_n(5, total_global_sales)

top_developers_global_sales <- games4 %>%
  group_by(developer) %>%
  summarise(total_global_sales = sum(global_sales)) %>%
  top_n(5, total_global_sales)

cat("Top Publishers by Global Sales:\n")
print(top_publishers_global_sales)

cat("\nTop Developers by Global Sales:\n")
print(top_developers_global_sales)

# Problem Statement 8: Distribution of Game Ratings
ratings_distribution <- games4 %>%
  group_by(rating) %>%
  summarise(num_games = n())

# Plotting the distribution of game ratings
ggplot(ratings_distribution, aes(x = rating, y = num_games)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Game Ratings",
       x = "Rating",
       y = "Number of Games") +
  theme_minimal()

# Problem Statement 9: Average Sales of Games Across Different Platforms
avg_sales_per_platform <- games4 %>%
  group_by(platform) %>%
  summarise(avg_global_sales = mean(global_sales, na.rm = TRUE))

cat("Average Global Sales of Games Across Different Platforms:\n")
print(avg_sales_per_platform)

# Problem Statement 10: Comparison of Sales Figures Between North America, Europe, and Japan
sales_comparison <- games4 %>%
  summarise(avg_na_sales = mean(NA_Sales, na.rm = TRUE),
            avg_eu_sales = mean(EU_Sales, na.rm = TRUE),
            avg_jp_sales = mean(JP_Sales, na.rm = TRUE))

cat("Average Sales Figures (in millions) Across Different Regions:\n")
print(sales_comparison)

# Problem Statement 11: Genre-wise User and Critic Scores
genre_scores <- games4 %>%
  group_by(genre) %>%
  summarise(avg_user_score = mean(User_Score, na.rm = TRUE),
            avg_critic_score = mean(Critic_Score, na.rm = TRUE))

cat("Genre-wise Average User and Critic Scores:\n")
print(genre_scores)

# Problem Statement 12: Publisher-wise Sales and Scores
publisher_sales_scores <- games4 %>%
  group_by(publisher) %>%
  summarise(total_global_sales = sum(Global_Sales, na.rm = TRUE),
            avg_user_score = mean(User_Score, na.rm = TRUE),
            avg_critic_score = mean(Critic_Score, na.rm = TRUE))

cat("Publisher-wise Total Global Sales and Average Scores:\n")
print(publisher_sales_scores)
