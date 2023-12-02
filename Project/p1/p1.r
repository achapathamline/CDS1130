library(dplyr)
library(ggplot2)

# Load your dataset (assuming it's in CSV format)
df <- read.csv('Project/phs_2020_1.csv')

# Map the heroes to their roles
hero_roles <- data.frame(
  hero_name = c("D.Va", "Orisa", "Reinhardt", "Roadhog", "Sigma", "Winston", "Wrecking Ball", "Zarya",
                "Doomfist", "Ashe", "Bastion", "McCree", "Echo", "Genji", "Hanzo", "Junkrat", "Mei", "Pharah", "Reaper", "Soldier: 76", "Sombra", "Symmetra", "Torbjörn", "Tracer", "Widowmaker",
                "Ana", "Baptiste", "Brigitte", "Lúcio", "Mercy", "Moira", "Zenyatta"),
  role = c(rep("Tank", 8), rep("Damage", 17), rep("Support", 7))
)

# Convert 'stat_amount' to numeric in case it's read as a factor
df$stat_amount <- as.numeric(as.character(df$stat_amount))

# Add a column for number of games played by each player if not already present
# Assuming 'game_id' or similar column exists that identifies each game uniquely
df <- df %>%
  group_by(player_name) %>%
  mutate(GamesPlayed = n_distinct(esports_match_id))

# Analysis for Leading Players by Total Damage Done
players_damage_done <- df %>%
  filter(stat_name == 'All Damage Done') %>%
  group_by(player_name) %>%
  summarize(TotalDamage = sum(stat_amount), GamesPlayed = first(GamesPlayed)) %>%
  mutate(AvgDamagePerGame = TotalDamage / GamesPlayed) %>%
  arrange(desc(TotalDamage))

# Find the most played hero for each player
player_most_played_hero <- df %>%
  filter(stat_name == 'Time Played') %>%
  group_by(player_name, hero_name) %>%
  summarize(TotalTimePlayed = sum(stat_amount), .groups = 'drop') %>%
  arrange(desc(TotalTimePlayed)) %>%
  slice(1) # Takes the top row for each player after arranging in descending order

# Join the most played hero with the hero roles to infer the player's role
player_roles <- player_most_played_hero %>%
  select(player_name, hero_name) %>%
  left_join(hero_roles, by = "hero_name")

# Join the role information with players' damage stats
players_damage_done <- players_damage_done %>%
  left_join(player_roles, by = "player_name")

# Join the role information with players' eliminations stats
players_eliminations <- players_eliminations %>%
  left_join(player_roles, by = "player_name")


# Plot for Average Damage per Game
plot1 <- ggplot(players_damage_done, aes(x=reorder(player_name, AvgDamagePerGame), y=AvgDamagePerGame)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title="Average Damage Done per Game by Players", x="Player Name", y="Average Damage per Game")

# Analysis for Leading Players by Total Eliminations
players_eliminations <- df %>%
  filter(stat_name == 'Eliminations') %>%
  group_by(player_name) %>%
  summarize(TotalEliminations = sum(stat_amount), GamesPlayed = first(GamesPlayed)) %>%
  mutate(AvgEliminationsPerGame = TotalEliminations / GamesPlayed) %>%
  arrange(desc(TotalEliminations))

# Plot for Average Eliminations per Game
plot2 <- ggplot(players_eliminations, aes(x=reorder(player_name, AvgEliminationsPerGame), y=AvgEliminationsPerGame)) +
  geom_bar(stat="identity", fill="blue") +
  coord_flip() +
  labs(title="Average Eliminations per Game by Players", x="Player Name", y="Average Eliminations per Game")


# Analysis for Most Impactful Heroes by Average Damage Done
heroes_damage_done <- df %>%
  filter(stat_name == 'All Damage Done') %>%
  group_by(hero_name) %>%
  summarize(AvgDamage = mean(stat_amount)) %>%
  arrange(desc(AvgDamage))

# Plot for Most Impactful Heroes by Average Damage Done
plot3 <- ggplot(heroes_damage_done, aes(x=reorder(hero_name, AvgDamage), y=AvgDamage)) +
  geom_bar(stat="identity", fill="green") +
  coord_flip() +
  labs(title="Average Damage Done by Heroes", x="Hero Name", y="Average Damage")

# Analysis for Most Impactful Heroes by Average Eliminations
heroes_eliminations <- df %>%
  filter(stat_name == 'Eliminations') %>%
  group_by(hero_name) %>%
  summarize(AvgEliminations = mean(stat_amount)) %>%
  arrange(desc(AvgEliminations))

# Plot for Most Impactful Heroes by Average Eliminations
plot4 <- ggplot(heroes_eliminations, aes(x=reorder(hero_name, AvgEliminations), y=AvgEliminations)) +
  geom_bar(stat="identity", fill="red") +
  coord_flip() +
  labs(title="Average Eliminations by Heroes", x="Hero Name", y="Average Eliminations")

# Execute the code to view the tables and plots
print(players_damage_done)
print(players_eliminations)
print(heroes_damage_done)
print(heroes_eliminations)

# Don't forget to replace 'path_to_your_csv.csv' with the actual path to your dataset.
print(plot1)
print(plot2)
print(plot3)
print(plot4)
