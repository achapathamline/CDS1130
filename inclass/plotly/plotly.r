library(tidyverse)
library(openintro)
library(plotly)
library(sf)
library(forcats)
library(stringr)

mn_counties_shape <- read_sf("inclass/maps/shp_bdry_counties_in_minnesota/mn_county_boundaries.shp")

# Create a 2d scatterplot
scatterplot_2d <- ggplot(data = county, aes(x = poverty, y = homeownership, col = state)) +
  geom_point(data = subset(county, state %in% c("Minnesota", "Louisiana")))

# print(ggplotly(scatterplot_2d))

# Create a bar chart
barchart <- ggplot(data = county, aes(x = pop2017, y = name)) +
  geom_col(data = subset(county, state %in% c("Minnesota")))

# print(ggplotly(barchart))

# Create a line plot

county_long <- county %>%
  pivot_longer(
    cols = starts_with("pop"),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(year = as.numeric(str_extract(year, "[0-9]+")))

lineplot <- ggplot(county_long, aes(x = year, y = population, group = name, color = name)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Population Over Time by County",
       x = "Year",
       y = "Population")

# Convert ggplot object to a plotly object
print(ggplotly(lineplot))



# Create a map

mn_county_shape <- read_sf("inclass/maps/shp_bdry_counties_in_minnesota/mn_county_boundaries.shp")

map_data <- read.csv("inclass/plotly/co-est2022-alldata.csv")

fixed_map_data <- subset(map_data, STNAME %in% c("Minnesota")) %>%
  filter(COUNTY != "0")

census_and_shape <- full_join(mn_county_shape, fixed_map_data, by = c("CTY_FIPS" = "COUNTY"))

mapplot <- ggplot() +
  geom_sf(data = census_and_shape, aes(fill = POPESTIMATE2022))

# print(ggplotly(mapplot))


# Create a 3d scatter plot

# Sample data
set.seed(123) # For reproducibility
x <- rnorm(50)
y <- rnorm(50)
z <- rnorm(50)

p <- plot_ly(x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'markers')


p <- p %>% 
    layout(title = "3D Scatter Plot",
           scene = list(
               xaxis = list(title = 'X Axis'),
               yaxis = list(title = 'Y Axis'),
               zaxis = list(title = 'Z Axis')
           ))

# print(p)