library(tidyverse)
library(ggplot2)

data <- read.csv("Project/phs_2020_1.csv")


library(ggplot2)

plot1 <- ggplot(data, aes(x = stat_amount)) +
  geom_histogram(binwidth = 500, fill = "blue", color = "black") +
  labs(title = "Histogram of stat_amount", x = "Stat Amount", y = "Frequency")
print(plot1)

plot2 <- ggplot(data, aes(y = stat_amount)) +
  geom_boxplot(fill = "tomato", color = "black") +
  labs(title = "Boxplot of stat_amount", y = "Stat Amount")
print(plot2)

plot3 <- ggplot(data, aes(x = factor(player_name))) +
  geom_bar() +
  labs(title = "Matches Played by Player", x = "Player Name", y = "Number of Matches") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(plot3)


damage_done <- data[data$stat_name == "All Damage Done", ]
damage_blocked <- data[data$stat_name == "Damage Blocked", ]

# Assuming there is a common key to merge the two, like player_name and match_id
merged_data <- merge(damage_done, damage_blocked, by=c("player_name", "esports_match_id"))


plot4 <- ggplot(merged_data, aes(x=stat_amount.x, y=stat_amount.y)) +
  geom_point() +
  labs(title="All Damage Done vs Damage Blocked", x="All Damage Done", y="Damage Blocked")
print(plot4)

plot5 <- ggplot(data, aes(x=map_type, y=stat_amount)) +
  geom_boxplot() +
  labs(title="Stat Amount by Map Type", x="Map Type", y="Stat Amount")
print(plot5)

plot6 <- ggplot(data, aes(x=stat_amount, fill=map_type)) +
  geom_density(alpha=0.7) +
  facet_wrap(~map_type) +
  labs(title="Density of stat_amount by Map Type", x="Stat Amount", y="Density")
print(plot6)

library(dplyr)

average_stats <- data %>%
  group_by(hero_name) %>%
  summarise(average_stat = mean(stat_amount, na.rm=TRUE))

plot7 <- ggplot(average_stats, aes(x=reorder(hero_name, average_stat), y=average_stat, fill=hero_name)) +
  geom_bar(stat="identity") +
  labs(title="Average Stat Amount by Hero Name", x="Hero Name", y="Average Stat Amount") +
  theme(axis.text.x = element_text(angle=90, hjust=1))
print(plot7)

data$start_time <- as.POSIXct(data$start_time, format="%m/%d/%Y %H:%M")

plot8 <- ggplot(data, aes(x=start_time, y=stat_amount, group=esports_match_id, color=esports_match_id)) +
  geom_line() +
  labs(title="Stat Amount over Time", x="Time", y="Stat Amount")
print(plot8)

plot9 <- ggplot(data, aes(x=damage_done, y = damage_blocked, color=hero_name)) +
  geom_point() +
  labs(title="Scatter Plot of stat_amount vs stat_amount2", x="Damage Done", y="Damage Blocked")
print(plot9)

# Reshape the data into a wide format if not already done
wide_data <- dcast(data, esports_match_id + player_name ~ stat_name, value.var="stat_amount")

# Compute the correlation matrix
correlation_matrix <- cor(wide_data[, -c(1,2)], use="complete.obs")

# Convert to a long format for ggplot2
melted_correlation_matrix <- melt(correlation_matrix, na.rm = TRUE)

plot10 <- ggplot(melted_correlation_matrix, aes(stat_amount, hero_name, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 9, hjust = 1)) +
  labs(title = "Correlation Heatmap of Statistics", x = "", y = "")
print(plot10)