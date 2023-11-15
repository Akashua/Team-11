  library(spatstat.geom)
  library(ggplot2)
  library(dplyr)

#Fig 1
# Select columns by their indices to avoid duplicates.
# Assuming the first 'x' and 'y' are the correct coordinates.
north_theft_locations_df <- north_theft_locations_df[, c(1, 2, 19:ncol(north_theft_locations_df))]
south_theft_locations_df <- south_theft_locations_df[, c(1, 2, 19:ncol(south_theft_locations_df))]

# Now let's try plotting again, making sure there are no duplicated column names
ggplot() +
  geom_polygon(data = north_poly_df, aes(x = long, y = lat, group = group), fill = "gray") +
  geom_point(data = north_theft_locations_df, aes(x = x, y = y), color = "blue") +
  coord_fixed(ratio = 1) +
  labs(title = "North")

ggplot() +
  geom_polygon(data = south_poly_df, aes(x = long, y = lat, group = group), fill = "gray") +
  geom_point(data = south_theft_locations_df, aes(x = x, y = y), color = "blue") +
  coord_fixed(ratio = 1) +
  labs(title = "South")

#Fig 2
library(ggplot2)
library(spatstat.geom)
library(sp)

# Calculate distances for the North region using the indices of the nearest neighbors
north_distances <- sqrt((covariates$x[nearest_indices_north] - north_theft_locations_df$x)^2 +
                          (covariates$y[nearest_indices_north] - north_theft_locations_df$y)^2)

# Calculate distances for the South region using the indices of the nearest neighbors
south_distances <- sqrt((covariates$x[nearest_indices_south] - south_theft_locations_df$x)^2 +
                          (covariates$y[nearest_indices_south] - south_theft_locations_df$y)^2)

# Combine both regions' distances into a single vector
all_distances <- c(north_distances, south_distances)

# Create a scatter plot of theft (recovery) locations
recovery_plot <- ggplot(north_theft_locations_df, aes(x = x, y = y)) +
  geom_point(color = "blue") +
  theme_minimal() +
  labs(title = "Recovered",
       x = "Easting (km)",
       y = "Northing (km)")

# Create a histogram of distances
distance_histogram <- ggplot(data.frame(distances = all_distances), aes(x = distances)) +
  geom_histogram(binwidth = 0.1, fill = "gray", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Distance",
       x = "Distance (km)",
       y = "Frequency")

# Combine the two plots into one figure
# Here, we will use the 'patchwork' library for combining plots
library(patchwork)
combined_plot <- recovery_plot | distance_histogram

# Print the combined plot
print(combined_plot)
