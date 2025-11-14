# Clean workspace with pacman unload all
if(!require(pacman)) install.packages("pacman")
pacman::p_unload(all)

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(maps)
library(mapdata)
library(grid)
library(cowplot)

# Get USA map data
usa <- map_data("state")

# Define a mapping between state abbreviations and full state names
state_mapping <- data.frame(
  State = c("CO", "KS", "ND", "NE", "SD", "TX", "MN", "OK"),
  state_name = c(
    "colorado", "kansas", "north dakota", "nebraska", "south dakota", "texas",
    "minnesota", "oklahoma"
  )
)

centroids <- data.frame(
  State = state.abb,
  lat.centroid = state.center$y,
  long.centroid = state.center$x
)

# https://github.com/wilkelab/cowplot/issues/202
# sub function to extract legend as cowplot get_legend broken
get_legend2 <- function(plot, legend = NULL) {
  if (is.ggplot(plot)) {
    gt <- ggplotGrob(plot)
  } else {
    if (is.grob(plot)) {
      gt <- plot
    } else {
      stop("Plot object is neither a ggplot nor a grob.")
    }
  }
  pattern <- "guide-box"
  if (!is.null(legend)) {
    pattern <- paste0(pattern, "-", legend)
  }
  indices <- grep(pattern, gt$layout$name)
  not_empty <- !vapply(
    gt$grobs[indices],
    inherits,
    what = "zeroGrob",
    FUN.VALUE = logical(1)
  )
  indices <- indices[not_empty]
  if (length(indices) > 0) {
    return(gt$grobs[[indices[1]]])
  }
  return(NULL)
}

# Create a function to generate the choropleth map for each modality
create_choropleth <- function(mod_data, legend_title = "Change in yield per year \n (lbs/acre)") {
  # Extract the modality name
  mod_name <- as.character(mod_data$mod[1])

  # Filter USA map for just the states in our dataset
  states_to_plot <- usa[usa$region %in% mod_data$state_name, ]

  # Get neighboring states for context (all states within our boundaries)
  context_states <- usa[usa$long >= map_bounds$lon_min &
    usa$long <= map_bounds$lon_max &
    usa$lat >= map_bounds$lat_min &
    usa$lat <= map_bounds$lat_max, ]

  # Merge our data with the map data
  map_data <- merge(states_to_plot, mod_data, by.x = "region", by.y = "state_name")

  # Sort the data to ensure proper polygon drawing
  map_data <- map_data[order(map_data$order), ]

  # Create the plot
  p <- ggplot() +
    # Add neighboring states as light grey background
    geom_polygon(
      data = context_states, aes(x = long, y = lat, group = group),
      fill = "grey85", color = "grey60", size = 0.1
    ) +
    # Add our states with data
    geom_polygon(
      data = map_data,
      aes(x = long, y = lat, group = group, fill = Slope),
      color = ifelse(map_data$significant, "black", "grey60"),
      size = ifelse(map_data$significant, 1.2, 0.5)
    ) +
    # Add slope values as text labels
    geom_text(
      data = map_data %>%
        group_by(region, State) %>%
        summarise(
          long.centroid = mean(long.centroid),
          lat.centroid = mean(lat.centroid),
          Slope = mean(Slope),
          .groups = "drop"
        ) %>%
        distinct(),
      aes(
        x = long.centroid, y = lat.centroid,
        label = sprintf("%.1f", round(Slope, 1))
      ),
      size = 4
    ) + 
    # Set color scale for slope values
    scale_fill_gradient2(
      low = "brown",
      mid = "white",
      high = "green",
      midpoint = 0,
      limits = c(-abs_max, abs_max),
      name = legend_title
    ) +
    # Set map boundaries
    coord_fixed(1.3,
      xlim = c(map_bounds$lon_min, map_bounds$lon_max),
      ylim = c(map_bounds$lat_min, map_bounds$lat_max)
    ) +
    # Customize theme
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.position = "bottom"
    ) +
    # Add title
    ggtitle(paste0(toupper(substr(mod_name, 1, 1)), substr(mod_name, 2, nchar(mod_name))))

  return(p)
}

for (response_var in c(
  "oil_pct", "yield_lb_acre", "flower_50pct_days_past_planting",
  "oil_yield_lb_acre", "harvest_moisture_pct",
  "height_cm", "value_generic", "test_weight_lbs_bushel"
)) {
  cat(response_var)

  csvs <- list.files("figure_inputs", pattern = "csv")
  csvs <- grep(pattern = response_var, x = csvs, value = TRUE) # get just reponse var
  csvs <- grep(csvs,
    pattern = "table.csv", value = TRUE,
    invert = TRUE
  ) # remove the non-p versions  of results
  if (response_var == "yield_lb_acre") {
    csvs <- grep(csvs,
      pattern = "oil", value = TRUE,
      invert = TRUE
    ) # remove oil yield from plain yield
  }

  f1 <- read.csv(file.path("figure_inputs", csvs[1]), header = TRUE)
  f2 <- read.csv(file.path("figure_inputs", csvs[2]), header = TRUE)

  if (response_var == "yield_lb_acre") {
    nass <- readRDS(file.path("figure_inputs", "nass_yield_trends.rds"))
    f3 <- nass$plot_data %>%
      rename(State = state_alpha, Slope = slope, Slope_SE = se) %>%
      dplyr::select(State, Slope, Slope_SE, pval) %>%
      distinct() %>%
      mutate(
        mod = "NASS",
        response_var = "yield_lb_acre"
      ) %>%
      distinct()
    f2 <- f2 %>% bind_rows(., f3)
    both_sets <- dplyr::bind_rows(f1, f2) %>%
      mutate(significance = ifelse(pval < 0.05, "significant", "not significant")) %>%
      filter(!State %in% c("CA", "OK")) %>% # leave to plains for now and states with a few decades of data
      mutate(State = factor(State, levels = c("TX", "OK", "KS", "CO", "NE", "SD", "MN", "ND"))) %>%
      mutate(mod = factor(mod, levels = c("NASS", "trial_mean", "breeding", "agronomy", "check")))
  } else {
    both_sets <- dplyr::bind_rows(f1, f2) %>%
      mutate(significance = ifelse(pval < 0.05, "significant", "not significant")) %>%
      mutate(State = factor(State, levels = c("TX", "KS", "CO", "NE", "ND", "SD"))) %>%
      mutate(mod = factor(mod, levels = c("trial_mean", "breeding", "agronomy", "check")))
  }

  data <- both_sets
  # Join full state names to our data
  data <- data %>%
    left_join(state_mapping, by = "State") %>%
    left_join(centroids, by = "State")

  # Define significance
  data$significant <- data$pval < 0.05
  
  data <- data %>%
    filter(!(State == "TX" & mod != "NASS")) %>%
    # only 4 years of data in thie CO trial for this
    # so include in suppmat regressions but not chloropleth
    filter(!(State == "CO" & response_var == "flower_50pct_days_past_planting"))
  #}

  # Get range of slope values for consistent color scale
  slope_range <- range(data$Slope)
  abs_max <- max(abs(slope_range))

  # Set the geographic boundaries of our map (with small buffer)
  # These bounds focus on the central-plains region where our states are located
  map_bounds <- list(
    lon_min = -115, # Western boundary
    lon_max = -90, # Eastern boundary
    lat_min = 25, # Southern boundary
    lat_max = 50 # Northern boundary
  )

  # Create plots for each modality
  if (response_var == "yield_lb_acre") {
    nass_plot <- create_choropleth(data[data$mod == "NASS", ]) + theme(legend.position = "none") +
      ggtitle("Statewide Trends \n(USDA)")
  }

  agronomy_plot <- create_choropleth(data[data$mod == "agronomy", ]) + theme(legend.position = "none") +
    ggtitle("Agronomy + Climate \n component of \n Trial Trends")
  breeding_plot <- create_choropleth(data[data$mod == "breeding", ]) + theme(legend.position = "none") +
    ggtitle("Genetic Improvement \n component of \n Trial Trends")
  trial_mean_plot <- create_choropleth(data[data$mod == "trial_mean", ]) + theme(legend.position = "none") +
    ggtitle("Trial Trends \n (overall)")
  check_plot <- create_choropleth(data[data$mod == "check", ]) + theme(legend.position = "none")

  # make one plot with legend
  if (response_var == "yield_lb_acre") {
    tst <- create_choropleth(data[data$mod == "trial_mean", ],
      legend_title = "Change in yield per year \n (lbs/acre)"
    )
  } else if (response_var == "harvest_moisture_pct") {
    tst <- create_choropleth(data[data$mod == "trial_mean", ],
      legend_title = "Change in harvest moisture per year \n (%)"
    )
  } else if (response_var == "height_cm") {
    tst <- create_choropleth(data[data$mod == "trial_mean", ],
      legend_title = "Change in plant height per year \n (cm)"
    )
  } else if (response_var == "mature_doy") {
    tst <- create_choropleth(data[data$mod == "trial_mean", ],
      legend_title = "Change in maturity date per year \n (day of year)"
    )
  } else if (response_var == "mature_days_past_planting") {
    tst <- create_choropleth(data[data$mod == "trial_mean", ],
      legend_title = "Change in relative maturity timing per year \n (days past planting)"
    )
  } else if (response_var == "oil_pct") {
    tst <- create_choropleth(data[data$mod == "trial_mean", ],
      legend_title = "Change in oil content per year \n (%)"
    )
  } else if (response_var == "oil_yield_lb_acre") {
    tst <- create_choropleth(data[data$mod == "trial_mean", ],
      legend_title = "Change in oil yield per year \n (lbs/acre)"
    )
  } else if (response_var == "test_weight_lbs_bushel") {
    tst <- create_choropleth(data[data$mod == "trial_mean", ],
      legend_title = "Change in test weight per year \n (lb/bushel)"
    )
  } else if (response_var == "value_generic") {
    tst <- create_choropleth(data[data$mod == "trial_mean", ],
      legend_title = "Change value per year \n (unitless)"
    )
  } else if (response_var == "flower_50pct_days_past_planting") {
    tst <- create_choropleth(data[data$mod == "trial_mean", ],
      legend_title = "Change in relative flowering date per year \n (days past planting)"
    )
  }

  legend <- get_legend2(tst)
  rm(tst)

  if (response_var == "yield_lb_acre") {
    p1 <- cowplot::plot_grid(nass_plot, trial_mean_plot, ncol = 2)
    p1a <- cowplot::plot_grid(NULL, p1, legend, ncol = 1, rel_heights = c(0.1, 1, 0.1))
    p3 <- cowplot::plot_grid(breeding_plot, agronomy_plot, ncol = 1)
    final_plot <- cowplot::plot_grid(
      p1a, p3,
      align = "none", # axis = "b",
      nrow = 1, rel_widths = c(2, 0.8)
    )
  } else {
    p1 <- cowplot::plot_grid(NULL, trial_mean_plot, ncol = 2)
    p1a <- cowplot::plot_grid(NULL, p1, legend, ncol = 1, rel_heights = c(0.1, 1, 0.1))
    p3 <- cowplot::plot_grid(breeding_plot, agronomy_plot, ncol = 1)
    final_plot <- cowplot::plot_grid(
      p1a, p3,
      align = "none", # axis = "b",
      nrow = 1, rel_widths = c(2, 0.8)
    )
  }

ggsave(final_plot,
      filename = file.path("figures", paste0(response_var, "_slope_map_no_texas.png")),
      height = 8, width = 10, bg = "white"
    )
}


