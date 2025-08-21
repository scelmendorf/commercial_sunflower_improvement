library(terra)
library(sf)
library(ggplot2)
library(maps)

# Step 1: Search for and mosaic multiple rasters
# Define the directory to search
raster_dir <- "C:/Users/Sarah/Downloads/GEE_export_14_Aug_2025-20250818T202054Z-1-001/GEE_export_temp-GEE_export_14_Aug_2025"

# Find all files matching the pattern 'SiteQualityDif'
all_raster_files <- list.files(raster_dir, pattern = "SiteQualityDif.*\\.tif$", 
                          full.names = TRUE, recursive = FALSE)

for (tx_var in c("noTX", "TX")) {
  for (fix_rand in c ('fixef', 'ranef')) {
    for (yr_var in c("noYr", "Yr")) {
      # Start with the full list for each combination
      raster_files <- all_raster_files
      raster_files <- raster_files[grepl(fix_rand, raster_files)]
      if (tx_var == 'noTX'){
        raster_files <- raster_files[grepl(tx_var, raster_files)]
      }
      if (yr_var == "noYr"){
        raster_files <- raster_files[grepl(yr_var, raster_files)]
      }
      # Check if any files were found
      if (length(raster_files) == 0) {
        cat(paste0("No raster files matching combination: ", fix_rand, ", ", tx_var, ", ", yr_var, "\n"))
        next
      } else {
        # Read all rasters
        raster_list <- lapply(raster_files, rast)
        # Mosaic all rasters together
        if (length(raster_list) == 1) {
          r <- raster_list[[1]]
        } else {
          r <- do.call(mosaic, raster_list)
        }
        # Step 2: Get state polygons and convert to sf
        states_sf <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
        st_crs(states_sf) <- 4326  # Assign CRS (WGS84)
        # Step 3: Filter for specific states
        target_states <- c("colorado", "nebraska", "south dakota",
                           "north dakota", "kansas", #"texas",
                           "minnesota")
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
        r_df$value[r_df$value == 0] <- NA
        
        # Step 7: Plot
        p <- ggplot() +
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
          theme_bw()
        
        ggsave(file.path('figures', paste0('SiteQualityDif_2040_vs_2000_', fix_rand, '_', tx_var, '_', yr_var, 
                                           ".png")), 
               plot = p, width = 10, height = 6, dpi = 600)
      }
    }
  }
}