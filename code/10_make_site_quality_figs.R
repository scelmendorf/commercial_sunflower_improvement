library(terra)
library(sf)
library(ggplot2)
library(maps)

# Step 1: Read the GeoTIFF
# going to come in as ESPG 3857
r <- rast("C:/Users/Sarah/Downloads/croppedSiteQualityDifference_12_Aug_2025.tif")

# Step 2: Get state polygons and convert to sf
states_sf <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
st_crs(states_sf) <- 4326  # Assign CRS (WGS84)

# Step 3: Filter for specific states
target_states <- c("colorado", "nebraska", "south dakota",
                   "north dakota", "kansas", "texas", "minnesota")
states_subset <- states_sf[states_sf$ID %in% target_states, ]

# Step 4: Reproject to match raster CRS
states_subset_proj <- st_transform(states_subset, crs(r))

# Step 5: Crop and mask raster to target states
# Convert sf to SpatVector
states_vect <- vect(states_subset_proj)
r_trimmed <- mask(crop(r, states_vect), states_vect)

# Step 6: Convert to data frame for plotting
r_df <- as.data.frame(r_trimmed, xy = TRUE, na.rm = TRUE)
colnames(r_df)[3] <- "value"
r_df$value[r_df$value ==0] <-NA

# Step 7: Plot
ggplot() +
  geom_tile(data = r_df, aes(x = x, y = y, fill = value)) +
  geom_sf(data = states_subset_proj, fill = NA,
          color = "black", linewidth = 0.3) +
  #scale_fill_viridis_c(na.value = NA) +
  scale_fill_gradient2(
    low = "saddlebrown",     # For low values (negative)
    mid = "grey80",          # For 0
    high = "darkblue",      # For high values (positive)
    midpoint = 0,
    na.value = NA
  ) +
  coord_sf(crs = crs(r)) +
  labs(title = "Difference in sunflower growing conditions (2040 vs 2000)",
       x = "Longitude", y = "Latitude", fill = "Change in \nsite quality") +
  theme_minimal()
