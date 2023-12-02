# Load packages
library(tidyverse)
library(openintro)
library(ggplot2)
library(dplyr)
library(sf)

# Read data
library(readr)
survey_data <- read_csv("inclass/maps/AllSchools_Food.csv")
mn_school_districts_shape <- read_sf("maps/shp_bdry_school_district_boundaries/school_district_boundaries.shp")
mn_county_shape <- read_sf("maps/shp_bdry_counties_in_minnesota/mn_county_boundaries.shp")
mn_tribal_land_shape <- read_sf("maps/shp_bdry_tribal_government/Tribal_Government_in_Minnesota.shp")
mn_county_annotation_shape <- read_sf("maps/shp_bdry_counties_in_minnesota/county_label_annotation.shp")

# Exercise 2

# Filter data
filtered_data <- survey_data %>%
  filter(Report == "Free/Reduced Price Lunch; Special Education") %>%
  filter(Question == "Do you currently get free or reduced-price lunch at school?")

# Summarize data
total_yes <- filtered_data %>%
  group_by(SchoolID) %>%
  filter(Response == "Yes") %>%
  summarise(yes = sum(TotalCount))

total_no <- filtered_data %>%
  group_by(SchoolID) %>%
  filter(Response == "No") %>%
  summarise(no = sum(TotalCount))

total_not_sure <- filtered_data %>%
  group_by(SchoolID) %>%
  filter(Response == "Not Sure") %>%
  summarise(not_sure = sum(TotalCount))

# Combine data
combined <- bind_cols(total_yes, total_no, total_not_sure)
combined <- combined[-c(3, 5)] %>%
  mutate(total = yes + no + not_sure,
         prop_yes = yes / total)

# Join datasets
combined_2 <- full_join(mn_school_districts_shape, combined, by = c("FORMID" = "SchoolID...1"))


# Plot
plot <- ggplot() +
  geom_sf(data = combined_2, aes(fill = -prop_yes)) +
  geom_sf(data = mn_county_shape, fill = NA, color = "yellow") +
  geom_sf(data = mn_tribal_land_shape, fill = NA, color = "green") +
  labs(title = "Proportion of Students by District that have Free or Reduced Lunch",
       fill = "Proportion")

print(plot)
