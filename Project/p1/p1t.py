import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Load the dataset
file_path = 'Project/phs_2020_1.csv'
data = pd.read_csv(file_path)

# Filter the dataset for rows where 'stat_name' is 'Eliminations'
eliminations_data = data[data['stat_name'] == 'Eliminations']

# Calculate the average eliminations per game for each player
average_eliminations = (
    eliminations_data.groupby(['player_name', 'esports_match_id'])['stat_amount']
    .mean()  # Calculate the average eliminations per match for each player
    .groupby(level=0)
    .mean()  # Calculate the average across all matches for each player
    .reset_index(name='average_eliminations')  # Reset index to convert groupby object back to dataframe
)

# Mapping of hero names to their roles based on the user's input
hero_roles = {
    'D.Va': 'Tank', 'Doomfist': 'Tank', 'Junker Queen': 'Tank', 'Mauga': 'Tank',
    'Orisa': 'Tank', 'Ramattra': 'Tank', 'Reinhardt': 'Tank', 'Roadhog': 'Tank',
    'Sigma': 'Tank', 'Winston': 'Tank', 'Wrecking Ball': 'Tank', 'Zarya': 'Tank',
    'Ashe': 'Damage', 'Bastion': 'Damage', 'Cassidy': 'Damage', 'Echo': 'Damage',
    'Genji': 'Damage', 'Hanzo': 'Damage', 'Junkrat': 'Damage', 'Mei': 'Damage',
    'Pharah': 'Damage', 'Reaper': 'Damage', 'Sojourn': 'Damage',
    'Soldier: 76': 'Damage', 'Sombra': 'Damage', 'Symmetra': 'Damage',
    'Torbjörn': 'Damage', 'Tracer': 'Damage', 'Widowmaker': 'Damage',
    'Ana': 'Support', 'Baptiste': 'Support', 'Brigitte': 'Support',
    'Illari': 'Support', 'Kiriko': 'Support', 'Lifeweaver': 'Support',
    'Lúcio': 'Support', 'Mercy': 'Support', 'Moira': 'Support', 'Zenyatta': 'Support'
}

# Apply the role mapping to the eliminations data
eliminations_data['role'] = eliminations_data['hero_name'].map(hero_roles)

# Determine the most common role for each player
player_roles = (
    eliminations_data.groupby('player_name')['role']
    .apply(lambda x: x.mode()[0])  # The mode will give us the most common role for each player
    .reset_index(name='common_role')
)

# Merge the player roles with the average eliminations
player_impact = average_eliminations.merge(player_roles, on='player_name', how='left')

# Define a custom color palette where Tank is blue, Damage is red, and Support is green
custom_palette = {'Tank': 'blue', 'Damage': 'red', 'Support': 'green'}

# Define darker shades for the edge colors
edge_colors = {'Tank': 'navy', 'Damage': 'darkred', 'Support': 'darkgreen'}

# Set the aesthetic style of the plots
sns.set_style("whitegrid")

# Create the visualization with the adjusted edge colors
plt.figure(figsize=(20, 12))

# Create a new color palette with edge colors for contrast
palette_with_contrast = {role: (fill_color, edge_colors[role]) for role, fill_color in custom_palette.items()}

# Sort the player impact dataframe by average eliminations only (ignoring the role)
player_impact_sorted = player_impact.sort_values('average_eliminations', ascending=False)

# Create the bar plot with the sorted data
plt.figure(figsize=(20, 12))
barplot = sns.barplot(
    x='average_eliminations', 
    y='player_name', 
    hue='common_role', 
    data=player_impact_sorted,
    palette=custom_palette,
    linewidth=1.5,  # Set line width for the bars
    edgecolor="white"  # Set edge color to white for visibility
)

# Add a legend and titles
plt.legend(title='Player Role', loc='lower right', frameon=True, edgecolor='black')
plt.xlabel('Average Eliminations Per Game')
plt.ylabel('Player Name')
plt.title('Average Eliminations Per Game by Player, Sorted by Average Eliminations')

# Show the plot with the sorted bars
plt.tight_layout()
plt.show()


