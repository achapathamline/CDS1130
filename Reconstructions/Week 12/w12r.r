# Load necessary libraries
library(ggplot2)
library(sf)

# Read the shapefile
states <- read_sf("Reconstructions/Week 12/cb_2018_us_state_500k/cb_2018_us_state_500k.shp")

annotated_data <- data.frame(
  state = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  votes = c(0.3365, 0.25, 0.3654, 0.2933, 1.0, 0.3365, 0.3077, 0.25, 0.25, 0.6250, 0.4375, 0.2644, 0.2644, 0.4952, 0.3654, 0.2933, 0.2933, 0.3221, 0.3221, 0.2644, 0.3510, 0.3654, 0.4375, 0.3510, 0.2933, 0.3510, 0.25, 0.2788, 0.2933, 0.2644, 0.4087, 0.2788, 0.6250, 0.4231, 0.25, 0.4663, 0.3077, 0.3077, 0.4952, 0.2644, 0.3365, 0.25, 0.3654, 0.7548, 0.2933, 0.25, 0.3942, 0.3798, 0.2788, 0.3510, 0.25),
  winner = c("Republican", "Republican", "Democrat", "Republican", "Democrat", "Democrat", "Democrat", "Democrat", "Democrat", "Republican", "Democrat", "Democrat", "Republican", "Democrat", "Republican", "Republican", "Republican", "Republican", "Republican", "Democrat", "Democrat", "Democrat", "Democrat", "Democrat", "Republican", "Republican", "Republican", "Republican", "Democrat", "Democrat", "Democrat", "Democrat", "Democrat", "Republican", "Republican", "Republican", "Republican", "Democrat", "Democrat", "Democrat", "Republican", "Republican", "Republican", "Republican", "Republican", "Democrat", "Democrat", "Democrat", "Republican", "Democrat", "Republican")
)

# Merge your data with the shapefile data
# Make sure the state identifiers match. It often requires using either full state names or specific codes.
# Let's assume your shapefile uses full state names in a column named 'STUSPS'
# You may need to adjust this depending on your shapefile's format
map_data <- merge(states, annotated_data, by.x = "STUSPS", by.y = "state")

# Create the plot
p <- ggplot(map_data) +
  geom_sf(aes(fill = winner, alpha = votes), color = "white") +
  scale_fill_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
  theme_void() +
  labs(title = "US Election Results by State", subtitle = "Color by Winning Party, Transparency by Vote Share") +
  guides(alpha = "none") +
  coord_sf(aspect = "equal")

print(p)
