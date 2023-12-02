library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the filtered dataset without "All Heroes"
file_path <- 'Project/phs_2020_1_no_all_heroes.csv'  # Make sure to use your actual file path
data <- read_csv(file_path)

# Calculate the count of hero picks per map for each match
hero_picks_count <- data %>%
  group_by(esports_match_id, map_name, hero_name) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ungroup()

# Aggregate counts across all matches
hero_picks_aggregated <- hero_picks_count %>%
  group_by(map_name, hero_name) %>%
  summarise(total_count = sum(count), .groups = 'drop')

# Reshape data for heatmap (wide format)
hero_picks_wide <- hero_picks_aggregated %>%
  pivot_wider(names_from = map_name, values_from = total_count, values_fill = list(total_count = 0))

# Convert the data to a long format for ggplot2
hero_picks_long <- hero_picks_wide %>%
  gather(key = "map_name", value = "pick_count", -hero_name)

# Create the heatmap
ggplot(hero_picks_long, aes(x = map_name, y = hero_name, fill = pick_count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Hero Picks on Different Maps", x = "Map", y = "Hero") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Output the plot to a file
ggsave("hero_picks_heatmap.png", width = 12, height = 8, units = "in")
