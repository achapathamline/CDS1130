
library(ggplot2)
library(dplyr)
library(tidyr)
# Load the data
df <- read.csv("Week 10 Reconstruction/mlb.csv")

# Remove rows where 'Time' column is NA (missing)
data <- subset(df, !is.na(Time))

library(ggplot2)
library(dplyr)
library(tidyr)

# Assuming 'data' is a dataframe that has been read into R already

lastsegment <- "#ff0000"
newavgline <- "#0000ff"

# Convert 'Time' from 'HH:MM' to minutes
convert_time_to_minutes <- function(time_str) {
  if (is.na(time_str)) {
    return(NA)  # Return NA if the time is NA
  }
  time_parts <- as.numeric(strsplit(time_str, ':')[[1]])
  hours <- time_parts[1]
  minutes <- time_parts[2]
  return(hours * 60 + minutes)
}

# Apply the conversion to the 'Time' column
data$Time_in_minutes <- sapply(data$Time, convert_time_to_minutes)

# Filter the data to include only the year 1955 onwards
data_filtered <- data %>%
  filter(Year >= 1955)

# Sort the data by Year to ensure the line connects points in chronological order
data_sorted <- data_filtered %>%
  arrange(Year)

# Plotting the graph
gg <- ggplot(data_sorted, aes(x = Year, y = Time_in_minutes)) +
  geom_line(aes(group = 1)) +  # Connect points with lines
  geom_point() +  # Add points
  theme_minimal(base_size = 14) +  # Use a minimal theme with base font size set

  # Add title and labels
  ggtitle('MLB Game Time Over Years (1955 Onwards)') +
  xlab('Year') +
  ylab('Game Time in Minutes') +
  theme(plot.title = element_text(hjust = 0.5)) + # Center the plot title
  geom_hline(yintercept = tail(data_sorted$Time_in_minutes, 1), linetype="dashed", color = newavgline) +
  theme(legend.position = "bottom") # Position legend at the bottom

# Color the last segment
if (nrow(data_sorted) > 1) {
  last_segment <- tail(data_sorted, 2)
  gg <- gg + geom_line(data = last_segment, aes(x = Year, y = Time_in_minutes), color = lastsegment)
}

# Draw a horizontal line for the last data point's Time value
gg <- gg + geom_hline(yintercept = tail(data_sorted$Time_in_minutes, 1), linetype="dashed", color = newavgline, size = 0.5)

# Adding the legend manually
last_point <- tail(data_sorted, 1)
gg <- gg + geom_point(data = last_point, aes(x = Year, y = Time_in_minutes), color = lastsegment, size = 3) +
  geom_text(data = last_point, aes(x = Year, y = Time_in_minutes, label = "", vjust = -2))

# Draw the grid
gg <- gg + theme(panel.grid.major = element_line(colour = "grey80"),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank())

# Display the plot
print(gg)
