# Load the dataset
file_path <- 'Project/phs_2020_1.csv'
data <- read_csv(file_path)

# Filter out entries with hero_name "All Heroes"
data_without_all_heroes <- filter(data, hero_name != "All Heroes")

# Save the filtered dataset to a new CSV file
new_file_path <- 'Project/phs_2020_1_no_all_heroes.csv'
write_csv(data_without_all_heroes, new_file_path)

# Output the path to the new file
print(paste("Filtered dataset saved to:", new_file_path))
