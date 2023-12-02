library(readr)
library(dplyr)
library(ggplot2)

# Load the filtered dataset without "All Heroes"
file_path <- 'Project/phs_2020_1_no_all_heroes.csv'
data <- read_csv(file_path)

# Mapping of hero names to their roles
hero_roles <- c(
  'D.Va' = 'Tank', 'Doomfist' = 'Tank', 'Junker Queen' = 'Tank', 'Mauga' = 'Tank',
  'Orisa' = 'Tank', 'Ramattra' = 'Tank', 'Reinhardt' = 'Tank', 'Roadhog' = 'Tank',
  'Sigma' = 'Tank', 'Winston' = 'Tank', 'Wrecking Ball' = 'Tank', 'Zarya' = 'Tank',
  'Ashe' = 'Damage', 'Bastion' = 'Damage', 'McCree' = 'Damage', 'Echo' = 'Damage',
  'Genji' = 'Damage', 'Hanzo' = 'Damage', 'Junkrat' = 'Damage', 'Mei' = 'Damage',
  'Pharah' = 'Damage', 'Reaper' = 'Damage', 'Sojourn' = 'Damage',
  'Soldier: 76' = 'Damage', 'Sombra' = 'Damage', 'Symmetra' = 'Damage',
  'Torbjörn' = 'Damage', 'Tracer' = 'Damage', 'Widowmaker' = 'Damage',
  'Ana' = 'Support', 'Baptiste' = 'Support', 'Brigitte' = 'Support',
  'Illari' = 'Support', 'Kiriko' = 'Support', 'Lifeweaver' = 'Support',
  'Lúcio' = 'Support', 'Mercy' = 'Support', 'Moira' = 'Support', 'Zenyatta' = 'Support'
)

# Apply the role mapping to the eliminations data
data <- data %>%
  mutate(role = hero_roles[hero_name])

# Filter the dataset for rows where 'stat_name' is 'Eliminations'
eliminations_data <- filter(data, stat_name == 'Eliminations')

# Calculate the average eliminations per game for each player
average_eliminations <- eliminations_data %>%
  group_by(player_name, esports_match_id) %>%
  summarise(avg_elim = mean(stat_amount), .groups = 'drop') %>%
  group_by(player_name) %>%
  summarise(average_eliminations = mean(avg_elim), .groups = 'drop')

# Determine the most common role for each player
player_roles <- eliminations_data %>%
  group_by(player_name) %>%
  summarise(common_role = Mode(role), .groups = 'drop')

# Merge the player roles with the average eliminations
player_impact <- merge(average_eliminations, player_roles, by = "player_name")

# Create the visualization with the adjusted edge colors
ggplot(player_impact, aes(x = reorder(player_name, -average_eliminations), y = average_eliminations, fill = common_role)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c('Tank' = 'blue', 'Damage' = 'red', 'Support' = 'green')) +
  coord_flip() +
  theme_minimal() +
  labs(x = "Player Name", y = "Average Eliminations Per Game", fill = "Player Role", title = "Average Eliminations Per Game by Player, Sorted by Average Eliminations") +
  theme(legend.position = "bottom")

# Save the plot with a taller layout to prevent overlapping names
ggsave("player_impact.png", width = 20, height = 20, units = "in")

# Function to calculate the mode
Mode <- function(x) {
  ux <- unique(x)
  if (length(ux) == 1) return(ux) # If there's only one unique value, return it
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}




# Visualization 2: Average eliminations by hero
avg_elim_by_hero <- eliminations_data %>%
  group_by(hero_name) %>%
  summarise(average_eliminations = mean(stat_amount, na.rm = TRUE)) %>%
  arrange(desc(average_eliminations))

ggplot(avg_elim_by_hero, aes(x = reorder(hero_name, average_eliminations), y = average_eliminations)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Average Eliminations by Hero", x = "Hero", y = "Average Eliminations")


# Visualization 4: Average eliminations by role
avg_elim_by_role <- eliminations_data %>%
  group_by(role) %>%
  summarise(average_eliminations = mean(stat_amount, na.rm = TRUE)) %>%
  arrange(desc(average_eliminations))

ggplot(avg_elim_by_role, aes(x = role, y = average_eliminations, fill = role)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Eliminations by Role", x = "Role", y = "Average Eliminations") +
  scale_fill_manual(values = c('Tank' = 'blue', 'Damage' = 'red', 'Support' = 'green')) +
  theme_minimal()


  # Assuming data, library imports, and hero_roles vector are already set up from the previous step.

# Visualization 5: Elimination count distribution by role
ggplot(eliminations_data, aes(x = stat_amount, fill = role)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  labs(title = "Elimination Count Distribution by Role", x = "Number of Eliminations", y = "Frequency") +
  scale_fill_manual(values = c('Tank' = 'blue', 'Damage' = 'red', 'Support' = 'green')) +
  theme_minimal()

# Visualization 6: Top players by average eliminations within each role
top_players_by_role <- eliminations_data %>%
  group_by(player_name, role) %>%
  summarise(average_eliminations = mean(stat_amount, na.rm = TRUE)) %>%
  slice_max(order_by = average_eliminations, n = 5) %>%
  ungroup() %>%
  arrange(role, desc(average_eliminations))

ggplot(top_players_by_role, aes(x = reorder(player_name, average_eliminations), y = average_eliminations, fill = role)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Players by Average Eliminations within Each Role", x = "Player", y = "Average Eliminations") +
  scale_fill_manual(values = c('Tank' = 'blue', 'Damage' = 'red', 'Support' = 'green')) +
  theme_minimal()

# Visualization 7: Boxplot of eliminations by hero
ggplot(eliminations_data, aes(x = hero_name, y = stat_amount, fill = role)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Boxplot of Eliminations by Hero", x = "Hero", y = "Number of Eliminations") +
  theme_minimal()

# Visualization 8: Boxplot of eliminations by player
ggplot(eliminations_data, aes(x = player_name, y = stat_amount, fill = role)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Boxplot of Eliminations by Player", x = "Player", y = "Number of Eliminations") +
  theme_minimal()

# Visualization 9: Boxplot of eliminations by role
ggplot(eliminations_data, aes(x = role, y = stat_amount, fill = role)) +
  geom_boxplot() +
  labs(title = "Boxplot of Eliminations by Role", x = "Role", y = "Number of Eliminations") +
  scale_fill_manual(values = c('Tank' = 'blue', 'Damage' = 'red', 'Support' = 'green')) +
  theme_minimal()

# Visualization 10: Eliminations over time (assuming there is a time variable like 'match_date')
eliminations_over_time <- eliminations_data %>%
  mutate(match_date = as.Date(start_time)) %>%  # Convert to Date type if it's not already
  group_by(match_date, role) %>%
  summarise(total_eliminations = sum(stat_amount, na.rm = TRUE)) %>%
  ungroup()

ggplot(eliminations_over_time, aes(x = match_date, y = total_eliminations, color = role)) +
  geom_line() +
  labs(title = "Eliminations Over Time by Role", x = "Date", y = "Total Eliminations") +
  scale_color_manual(values = c('Tank' = 'blue', 'Damage' = 'red', 'Support' = 'green')) +
  theme_minimal()

# Visualization 11: Hero usage frequency
hero_usage_frequency <- eliminations_data %>%
  group_by(hero_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(hero_usage_frequency, aes(x = reorder(hero_name, count), y = count, fill = role)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Hero Usage Frequency", x = "Hero", y = "Count") +
  theme_minimal()

# Note: For Visualizations 10 and 11, make sure you have a 'start_time' or equivalent
