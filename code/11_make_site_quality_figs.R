library(terra)
library(sf)
library(ggplot2)
library(maps)
library(cowplot)

# Define the directory to search
raster_dir <- file.path("figure_inputs", "gee")

# Find all files
all_raster_files <- list.files(raster_dir,
  full.names = TRUE, recursive = FALSE,
)

# Function to create plots for a given time period and parameter combinations
create_plots <- function(time_period, fix_rand_options, tx_options, yr_options) {
  plot_list <- list()
  plot_index <- 1

  for (fix_rand in fix_rand_options) {
    for (tx_var in tx_options) {
      for (yr_var in yr_options) {
        # Find files matching the combination and time period
        raster_files <- all_raster_files
        raster_files <- raster_files[grepl(time_period, raster_files)]
        raster_files <- raster_files[grepl(fix_rand, raster_files)]
        raster_files <- raster_files[grepl(tx_var, raster_files)]

        if (yr_var == "noYr") {
          raster_files <- raster_files[grepl(yr_var, raster_files)]
        } else {
          raster_files <- raster_files[!grepl("noYr", raster_files)]
        }

        # Check if any files were found
        if (length(raster_files) == 0) {
          cat(paste0("No raster files matching combination: ", time_period, ", ", fix_rand, ", ", tx_var, ", ", yr_var, "\n"))
          next
        }

        cat(paste0("Processing ", time_period, ": ", fix_rand, "_", tx_var, "_", yr_var, " (", length(raster_files), " files)\n"))

        # Read all rasters and mosaic
        raster_list <- lapply(raster_files, rast)
        r <- if (length(raster_list) == 1) raster_list[[1]] else do.call(mosaic, raster_list)

        # Step 2: Get state polygons and convert to sf
        states_sf <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
        st_crs(states_sf) <- 4326 # Assign CRS (WGS84)

        # Step 3: Filter for specific states
        target_states <- c(
          "colorado", "nebraska", "south dakota",
          "north dakota", "kansas", "texas", "oklahoma",
          "minnesota"
        )
        states_subset <- states_sf[states_sf$ID %in% target_states, ]

        # Step 4: Reproject to match raster CRS
        states_subset_proj <- st_transform(states_subset, crs(r))

        # Step 5: Crop and mask raster to target states
        # Convert sf to SpatVector
        states_vect <- vect(states_subset_proj)
        r_trimmed <- mask(crop(r, states_vect), states_vect)

        # Step 6: Convert to data frame for plotting
        r_df <- as.data.frame(r_trimmed, xy = TRUE, na.rm = TRUE)

        janitor::get_dupes(r_df, x, y)

        value_col <- names(r_df)[3] # Get the actual column name

        # Calculate statistics
        data_range <- range(r_df[[value_col]], na.rm = TRUE)
        data_median <- median(r_df[[value_col]], na.rm = TRUE)
        stats_text <- paste0(
          "Range: ", round(data_range[1], 0), " to ", round(data_range[2], 0),
          "  |  Median: ", round(data_median, 0)
        )

        # Set color scale based on time period
        if (time_period %in% c("Current", "Future")) {
          # Relative site quality values
          fill_limits <- c(-2000, 2000)
          fill_breaks <- seq(-2000, 2000, by = 500)
          fill_colors <- scale_fill_gradient2(
            low = "darkred", mid = "yellow", high = "darkgreen",
            midpoint = 0, na.value = NA,
            limits = fill_limits, breaks = fill_breaks, labels = fill_breaks
          )
        } else {
          # Difference values
          fill_limits <- c(-1000, 1000)
          fill_breaks <- seq(-1000, 1000, by = 500)
          fill_colors <- scale_fill_gradient2(
            low = "saddlebrown", mid = "grey80", high = "darkblue",
            midpoint = 0, na.value = NA,
            limits = fill_limits, breaks = fill_breaks, labels = fill_breaks
          )
        }

        title_string <- paste0(time_period, ": ", tx_var, "_", yr_var)
        title_string <- ifelse(fix_rand == "fixef", paste0(title_string, " \n(County as Fixed Effect)"), paste0(title_string, " \n(County as Random Effect)"))

        # Create plot
        p <- ggplot() +
          geom_tile(data = r_df, aes(x = x, y = y, fill = !!sym(value_col))) +
          geom_sf(data = states_subset_proj, fill = NA, color = "black", linewidth = 0.3) +
          fill_colors +
          coord_sf(crs = crs(r)) +
          labs(
            title = title_string,
            subtitle = stats_text,
            x = "Longitude", y = "Latitude",
            fill = ifelse(time_period %in% c("Current", "Future"), "Site Quality", "Change in \nSite Quality")
          ) +
          theme_bw() +
          theme(plot.subtitle = element_text(size = 9))

        # Save individual plot
        ggsave(
          file.path("figures", paste0(
            "SiteQuality_", time_period, "_", fix_rand, "_", tx_var, "_", yr_var, ".tiff"
          )),
          plot = p, width = 10, height = 6, dpi = 500
        )

        plot_list[[plot_index]] <- p
        plot_index <- plot_index + 1
      }
    }
  }

  return(plot_list)
}

# Function to create difference rasters using raster math
create_difference_rasters <- function(fix_rand_options, tx_options, yr_options) {
  plot_list <- list()
  plot_index <- 1

  for (fix_rand in fix_rand_options) {
    for (tx_var in tx_options) {
      for (yr_var in yr_options) {
        # Find current files
        current_files <- all_raster_files
        current_files <- current_files[grepl("Current", current_files)]
        current_files <- current_files[grepl(fix_rand, current_files)]
        current_files <- current_files[grepl(tx_var, current_files)]

        # Find future files
        future_files <- all_raster_files
        future_files <- future_files[grepl("Future", future_files)]
        future_files <- future_files[grepl(fix_rand, future_files)]
        future_files <- future_files[grepl(tx_var, future_files)]

        if (yr_var == "noYr") {
          current_files <- current_files[grepl(yr_var, current_files)]
          future_files <- future_files[grepl(yr_var, future_files)]
        } else {
          current_files <- current_files[!grepl("noYr", current_files)]
          future_files <- future_files[!grepl("noYr", future_files)]
        }

        # Check if both current and future files exist
        if (length(current_files) == 0 || length(future_files) == 0) {
          cat(paste0("Missing files for difference calculation: ", fix_rand, "_", tx_var, "_", yr_var, "\n"))
          next
        }

        cat(paste0("Creating difference raster: ", fix_rand, "_", tx_var, "_", yr_var, "\n"))

        # Read and mosaic current rasters
        current_raster_list <- lapply(current_files, rast)
        current_r <- if (length(current_raster_list) == 1) current_raster_list[[1]] else do.call(mosaic, current_raster_list)

        # Read and mosaic future rasters
        future_raster_list <- lapply(future_files, rast)
        future_r <- if (length(future_raster_list) == 1) future_raster_list[[1]] else do.call(mosaic, future_raster_list)

        # Calculate difference: Future - Current
        diff_r <- future_r - current_r

        # Step 2: Get state polygons and convert to sf
        states_sf <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
        st_crs(states_sf) <- 4326 # Assign CRS (WGS84)

        # Step 3: Filter for specific states
        target_states <- c(
          "colorado", "nebraska", "south dakota",
          "north dakota", "kansas", "texas", "oklahoma",
          "minnesota"
        )
        states_subset <- states_sf[states_sf$ID %in% target_states, ]

        # Step 4: Reproject to match raster CRS
        states_subset_proj <- st_transform(states_subset, crs(diff_r))

        # Step 5: Crop and mask raster to target states
        # Convert sf to SpatVector
        states_vect <- vect(states_subset_proj)
        diff_r_trimmed <- mask(crop(diff_r, states_vect), states_vect)

        # Convert to data frame for plotting
        diff_df <- as.data.frame(diff_r_trimmed, xy = TRUE, na.rm = TRUE)
        value_col <- names(diff_df)[3]

        # Calculate statistics
        data_range <- range(diff_df[[value_col]], na.rm = TRUE)
        data_median <- median(diff_df[[value_col]], na.rm = TRUE)
        stats_text <- paste0(
          "Range: ", round(data_range[1], 0), " to ", round(data_range[2], 0),
          "  |  Median: ", round(data_median, 0)
        )

        title_string <- paste0(tx_var, "_", yr_var)
        title_string <- ifelse(fix_rand == "fixef", paste0(title_string, " \n(County as Fixed Effect)"), paste0(title_string, " \n(County as Random Effect)"))


        # Create plot
        p <- ggplot() +
          geom_tile(data = diff_df, aes(x = x, y = y, fill = !!sym(value_col))) +
          geom_sf(data = states_subset_proj, fill = NA, color = "black", linewidth = 0.3) +
          scale_fill_gradient2(
            low = "saddlebrown", mid = "grey80", high = "darkblue",
            midpoint = 0, na.value = NA,
            limits = c(-1000, 1000),
            breaks = seq(-1000, 1000, by = 500),
            labels = seq(-1000, 1000, by = 500)
          ) +
          coord_sf(crs = crs(diff_r)) +
          labs(
            title = title_string,
            subtitle = stats_text,
            x = "Longitude",
            y = "Latitude",
            # fill = "Change in \nSite Quality \n(2040 vs 2000)\lbs"
            # fill = bquote("Change in" ~ "\nSite Quality" ~ "\n(2040 vs 2000)\n" * "(lbs" ~ acre^-1 * ")")
            # fill = expression("Change in\nSite Quality\n(lbs per acre)")
            fill = "Change in\nSite Quality\n(lbs per acre)"
          ) +
          theme_bw() +
          theme(plot.subtitle = element_text(size = 9))

        # Save individual plot
        ggsave(
          file.path("figures", paste0(
            "SiteQuality_Difference_", fix_rand, "_", tx_var, "_", yr_var, ".tiff"
          )),
          plot = p, width = 10, height = 6, dpi = 500
        )

        plot_list[[plot_index]] <- p
        plot_index <- plot_index + 1
      }
    }
  }

  return(plot_list)
}

# Define parameter combinations
fix_rand_options <- c("fixef", "ranef")
tx_options <- c("noTX", "includeTX")
yr_options <- c("noYr", "Yr")

# Create plots for each time period
#current_plots <- create_plots("Current", fix_rand_options, tx_options, yr_options)
#future_plots <- create_plots("Future", fix_rand_options, tx_options, yr_options)
difference_plots <- create_difference_rasters(fix_rand_options, tx_options, yr_options)

# Create multi-panel figures for each time period
create_multipanel <- function(plot_list, filename_suffix, time_period) {
  if (length(plot_list) > 0) {
    legend <- get_legend(plot_list[[1]] + theme(legend.box.margin = margin(0, 0, 0, 12)))
    multi_panel_plot <- plot_grid(
      plotlist = lapply(plot_list, function(x) {
        x +
          theme(legend.position = "none")
      }), nrow = 2,
      labels = "AUTO",
      label_x = 0.2, # move labels slightly right from left edge
      label_y = 0.98, # move labels slightly down from top edge
      hjust = 0, # anchor label to left
      vjust = 1 # anchor label to top
    )
    final_plot <- ggdraw(plot_grid(multi_panel_plot, legend, rel_widths = c(3, 0.4)) +
      theme(plot.background = element_rect(fill = "white", color = NA)))

    ggsave(
      file.path("figures", paste0("SiteQuality_", filename_suffix, "_all_combinations.tiff")),
      plot = final_plot, width = 16, height = 8, dpi = 400, bg = "white"
    )
  }
}

# Create multi-panel figures
#create_multipanel(current_plots, "Current", "Current")
#create_multipanel(future_plots, "Future", "Future")
create_multipanel(difference_plots, "Difference", "Difference")

# Difference values
fill_limits <- c(-400, 400)
fill_breaks <- seq(-400, 400, by = 200)
# change scale for main fig to accentuate difs
main_fig <- difference_plots[[4]] +
  scale_fill_gradient2(
    low = "saddlebrown", mid = "grey80", high = "darkblue",
    midpoint = 0, na.value = NA,
    limits = fill_limits, breaks = fill_breaks, labels = fill_breaks
  ) +
  ggtitle(NULL)

ggsave(main_fig,
  file = file.path("figures", "SiteQuality_Difference_fixef_includeTX_Yr_main_figure.tiff"),
  width = 10, height = 6, dpi = 500
)
