# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(zoo)

# Load the data
co2_data <- read_csv("co2_mm_mlo.csv")

co2_data$RollingMean <- rollmean(co2_data$average, k = 12, fill = NA, align = "center")

# Plot the data
plot <- ggplot(data = co2_data, aes(x = `decimal date`, y = average)) +
  geom_line(color = "red", size = 1.5, alpha = 0.5) +
  geom_line(aes(y = RollingMean), color = "black", size = 1.5) +
  labs(
    title = "Atmospheric CO2 at Mauna Loa Observatory",
    x = "Year",
    y = "CO2 mole fraction (ppm)"
  )

print(plot)