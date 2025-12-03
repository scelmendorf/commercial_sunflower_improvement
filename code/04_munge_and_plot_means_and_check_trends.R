# Clean workspace with pacman unload all
if(!require(pacman)) install.packages("pacman")
pacman::p_unload(all)

library(tidyverse)
library(cowplot)

# function to rebuild the trends plots (means and checks)
rebuild_trends_plot <- function(
    trend_data,
    trend_check_data,
    custom_order = NULL) {
  library(ggplot2)
  library(dplyr)
  library(patchwork)
  trend_data <- readRDS(trend_data)
  trend_check_data <- readRDS(trend_check_data)
  # Extract data from the lists
  meta_reg <- trend_data$meta_reg
  meta_reg_check <- trend_check_data$data
  response_var <- trend_data$response_var
  state_labels <- trend_data$state_labels
  state_labels_check <- trend_check_data$state_labels_check
  label_positions <- trend_data$label_positions
  label_positions_check <- trend_check_data$label_positions_check
  pval_text <- trend_data$pval_text
  pval_text_check <- trend_check_data$pval_text
  nocheck_data <- trend_data$data

  # Determine which states are in both datasets
  states_all <- unique(c(meta_reg$State, meta_reg_check$State))

  # Use custom order if provided, otherwise default south to north
  if (is.null(custom_order)) {
    south_to_north_order <- c("TX", "KS", "CO", "NE", "SD", "ND")
    # Filter to only states present in the data
    ordered_states <- south_to_north_order[south_to_north_order %in% states_all]
  } else {
    ordered_states <- custom_order[custom_order %in% states_all]
  }

  # For any state in the data but not in the order, append to the end
  missing_states_meta_reg <- setdiff(ordered_states, unique(nocheck_data$State))

  meta_reg <- meta_reg %>%
    bind_rows(data.frame(State = missing_states_meta_reg))

  missing_states_meta_reg_check <- setdiff(
    ordered_states,
    unique(meta_reg_check$State)
  )

  meta_reg_check <- meta_reg_check %>%
    bind_rows(data.frame(State = missing_states_meta_reg_check))

  # Plot 1: Trial means
  # Get trial data for plotting
  # nocheck_data <- if (!is.null(trend_data$nocheck_data)) trend_data$nocheck_data else data.frame()

  trend_plot <- ggplot(meta_reg, aes(x = Year)) +
    geom_point(
      data = nocheck_data,
      aes(y = .data[[response_var]]),
      alpha = 0.05
    ) +
    geom_point(aes(y = Estimate), alpha = 0.6) +
    geom_errorbar(
      aes(ymin = Estimate - SE, ymax = Estimate + SE),
      width = 0.2,
      alpha = 0.4
    ) +
    geom_line(aes(y = Predicted, color = colors), size = 1) +
    geom_ribbon(aes(ymin = CI_low, ymax = CI_up, fill = colors), alpha = 0.2) +
    scale_color_identity() +
    scale_fill_identity() +
    facet_wrap(
      ~ factor(State, levels = rev(ordered_states)),
      ncol = 1,
      scales = "free_y"
    ) +
    theme_minimal(base_size = 14) +
    geom_label(
      data = state_labels,
      aes(x = label_positions$x, y = label_positions$y, label = label),
      hjust = 0,
      vjust = 1,
      size = 3.5,
      fill = "white",
      label.size = NA,
      alpha = 0.7
    ) +
    labs(
      title = paste(
        "Meta-Regression of",
        response_var,
        "Trial Means Over Time"
      ),
      subtitle = paste(
        "Per-state predictions with 95% CI and slope",
        pval_text,
        sep = "\n"
      ),
      x = "Year",
      y = paste0(response_var, " trial mean")
    )

  # Plot 2: Check varieties
  trend_plot_check <- ggplot(meta_reg_check, aes(x = Year)) +
    geom_point(aes(y = Estimate, shape = Unif_Name), alpha = 0.6) +
    geom_line(aes(y = Predicted, color = colors), size = 1) +
    geom_ribbon(aes(ymin = CI_low, ymax = CI_up, fill = colors), alpha = 0.2) +
    scale_color_identity() +
    scale_fill_identity() +
    facet_wrap(
      ~ factor(State, levels = rev(ordered_states)),
      ncol = 1,
      scales = "free_y"
    ) +
    theme_minimal(base_size = 14) +
    geom_label(
      data = state_labels_check,
      aes(
        x = label_positions_check$x,
        y = label_positions_check$y,
        label = label
      ),
      hjust = 0,
      vjust = 1,
      size = 3.5,
      fill = "white",
      label.size = NA,
      alpha = 0.7
    ) +
    labs(
      title = paste(
        "Meta-Regression of",
        response_var,
        "Check Varieties Over Time"
      ),
      subtitle = paste(
        "Per-state predictions with 95% CI and slope",
        pval_text_check,
        sep = "\n"
      ),
      x = "Year",
      y = paste0(response_var, " (checks only)")
    )

  # Return all plots and the ordered states used
  return(list(
    trial_plot = trend_plot,
    check_plot = trend_plot_check,
    ordered_states = ordered_states
  ))
}

rebuild_trends_plot_planting <- function(trend_data, nass_data, custom_order = NULL) {
  library(ggplot2)
  library(dplyr)
  library(patchwork)
  trend_data <- readRDS(trend_data)
  # Extract data from the lists
  meta_reg <- trend_data$meta_reg
  response_var <- trend_data$response_var
  state_labels <- trend_data$state_labels
  label_positions <- trend_data$label_positions
  pval_text <- trend_data$pval_text
  no_check_subset <- trend_data$data
  check_data <- nass_data$plot_data

  # Determine which states are in both datasets
  states_all <- unique(c(meta_reg$State, check_data$state_alpha))

  # Use custom order if provided, otherwise default south to north
  if (is.null(custom_order)) {
    south_to_north_order <- c("TX", "KS", "CO", "NE", "SD", "ND")
    # Filter to only states present in the data
    ordered_states <- south_to_north_order[south_to_north_order %in% states_all]
  } else {
    ordered_states <- custom_order[custom_order %in% states_all]
  }

  # For any state in the data but not in the order, append to the end
  missing_states_meta_reg <- setdiff(
    ordered_states,
    unique(meta_reg$State)
  )

  meta_reg <- meta_reg %>%
    bind_rows(data.frame(State = missing_states_meta_reg))

  # If planting_doy or harvest_doy, convert to date for y axis
  if (response_var %in% c("planting_doy", "harvest_doy")) {
    no_check_subset$y_date <- as.Date(no_check_subset[[response_var]] - 1, origin = "2020-01-01")
    meta_reg$Predicted_date <- as.Date(meta_reg$Predicted - 1, origin = "2020-01-01")
    meta_reg$CI_low_date <- as.Date(meta_reg$CI_low - 1, origin = "2020-01-01")
    meta_reg$CI_up_date <- as.Date(meta_reg$CI_up - 1, origin = "2020-01-01")
    state_labels$y_date <- as.Date(label_positions$y - 1, origin = "2020-01-01")
    ylab_str <- ifelse(response_var == "planting_doy", "Planting date", "Harvest date")
    trend_plot <- ggplot(meta_reg, aes(x = Year)) +
      geom_point(
        data = no_check_subset,
        aes(y = y_date),
        alpha = 0.05
      ) +
      geom_line(aes(y = Predicted_date, color = colors), size = 1) +
      geom_ribbon(aes(ymin = CI_low_date, ymax = CI_up_date, fill = colors), alpha = 0.2) +
      scale_color_identity() +
      scale_fill_identity() +
  facet_wrap(~ factor(State, levels = rev(c("TX", "KS", "CO", "NE", "SD", "ND"))), ncol = 1) +
      theme_minimal(base_size = 14) +
      geom_label(
        data = state_labels,
        aes(x = label_positions$x, y = y_date, label = label),
        hjust = 0,
        vjust = 1,
        size = 3.5,
        fill = "white",
        label.size = NA,
        alpha = 0.7
      ) +
      scale_y_date(date_labels = "%b-%d") +
      labs(
        title = paste("Regression of", response_var, "Over Time"),
        subtitle = paste(
          "Per-state predictions with 95% CI and slope",
          pval_text,
          sep = "\n"
        ),
        x = "Year",
        y = ylab_str
      )
  } else {
    trend_plot <- ggplot(meta_reg, aes(x = Year)) +
      geom_point(
        data = no_check_subset,
        aes(y = .data[[response_var]]),
        alpha = 0.05
      ) +
      geom_line(aes(y = Predicted, color = colors), size = 1) +
      geom_ribbon(aes(ymin = CI_low, ymax = CI_up, fill = colors), alpha = 0.2) +
      scale_color_identity() +
      scale_fill_identity() +
  facet_wrap(~ factor(State, levels = rev(c("TX", "KS", "CO", "NE", "SD", "ND"))), ncol = 1) +
      theme_minimal(base_size = 14) +
      geom_label(
        data = state_labels,
        aes(x = label_positions$x, y = label_positions$y, label = label),
        hjust = 0,
        vjust = 1,
        size = 3.5,
        fill = "white",
        label.size = NA,
        alpha = 0.7
      ) +
      labs(
        title = paste("Regression of", response_var, "Over Time"),
        subtitle = paste(
          "Per-state predictions with 95% CI and slope",
          pval_text,
          sep = "\n"
        ),
        x = "Year",
        y = paste0(response_var)
      )
  }

  # Return the plot and the ordered states used
  return(list(
    trial_plot = trend_plot,
    ordered_states = ordered_states
  ))
}


# function to rebuild the trends plots (means and checks)
rebuild_gain_plot <- function(
    rds_file_path,
    new_theme = NULL,
    new_title = NULL,
    new_subtitle = NULL,
    new_x_label = NULL,
    new_y_label = NULL,
    new_facet_layout = NULL,
    ordered_states = c("TX", "KS", "CO", "NE", "SD", "ND")) {
  # Load the saved plot data
  plot_data <- readRDS(rds_file_path)

  # Extract the components
  orig_plot <- plot_data$plot
  data <- plot_data$data
  state_labels <- plot_data$state_labels
  label_positions <- plot_data$label_positions
  pval_text <- plot_data$pval_text
  response_var <- plot_data$response_var

  # Determine if it's an agronomy or breeding plot from the filename
  is_agronomy <- grepl("agronomy", rds_file_path)

  # Create the appropriate title based on plot type
  if (is.null(new_title)) {
    if (is_agronomy) {
      title <- paste(
        "Meta-Regression of",
        response_var,
        "BLUEs \nAgronomy Over Time"
      )
      x_axis <- "Year"
    } else {
      title <- paste(
        "Meta-Regression of",
        response_var,
        "BLUEs \nBreeding Over Time"
      )
      x_axis <- "Year Variety Introduced"
    }
  } else {
    title <- new_title
  }

  # Create the appropriate subtitle
  if (is.null(new_subtitle)) {
    subtitle <- paste(
      "Per-state predictions with 95% CI and slope",
      pval_text,
      sep = "\n"
    )
  } else {
    subtitle <- new_subtitle
  }

  # Set the x-axis label
  if (is.null(new_x_label)) {
    x_label <- if (is_agronomy) "Year" else "Year Variety Introduced"
  } else {
    x_label <- new_x_label
  }

  # Set the y-axis label
  if (is.null(new_y_label)) {
    y_label <- paste0(response_var, " BLUE")
  } else {
    y_label <- new_y_label
  }

  # Set the facet layout
  if (is.null(new_facet_layout)) {
    n_cols <- 1
  } else {
    n_cols <- new_facet_layout
  }

  # Create dummy data for missing states to ensure all panels are shown
  states_in_data <- unique(data$State)
  missing_states <- setdiff(ordered_states, states_in_data)

  # Create dummy rows for missing states
  if (length(missing_states) > 0) {
    dummy_data <- data.frame(
      State = missing_states,
      stringsAsFactors = FALSE
    )

    if (is_agronomy) {
      dummy_data$Year <- NA
      dummy_data$Estimate <- NA
      dummy_data$SE <- NA
      dummy_data$Predicted <- NA
      dummy_data$CI_low <- NA
      dummy_data$CI_up <- NA
      dummy_data$Slope <- NA
      dummy_data$pval <- NA
      dummy_data$Slope_SE <- NA
      dummy_data$linetype <- NA
      dummy_data$colors <- NA
    } else {
      dummy_data$min_yr <- NA
      dummy_data$Estimate <- NA
      dummy_data$SE <- NA
      dummy_data$Predicted <- NA
      dummy_data$CI_low <- NA
      dummy_data$CI_up <- NA
      dummy_data$Slope <- NA
      dummy_data$pval <- NA
      dummy_data$Slope_SE <- NA
      dummy_data$linetype <- NA
      dummy_data$colors <- NA
    }

    # Combine the real data with the dummy data
    data <- bind_rows(data, dummy_data)
  }

  # Create the new plot using ggplot from scratch to avoid layer conflicts
  new_plot <- ggplot(data, aes(x = if (is_agronomy) Year else min_yr)) +
    geom_point(aes(y = Estimate), alpha = 0.6) +
    geom_errorbar(
      aes(ymin = Estimate - SE, ymax = Estimate + SE),
      width = 0.2,
      alpha = 0.4
    ) +
    geom_line(aes(y = Predicted, color = colors), size = 1) +
    geom_ribbon(aes(ymin = CI_low, ymax = CI_up, fill = colors), alpha = 0.2) +
    scale_color_identity() +
    scale_fill_identity() +
    # Use ordered_states to ensure all states are shown, even if there's no data
    facet_wrap(~ factor(State, levels = rev(ordered_states)), ncol = n_cols) +
    (if (is.null(new_theme)) theme_minimal(base_size = 14) else new_theme) +
    # Only add labels for states that have data
    geom_label(
      data = state_labels,
      aes(x = label_positions$x, y = label_positions$y, label = label),
      hjust = 0,
      vjust = 1,
      size = 3.5,
      fill = "white",
      label.size = NA,
      alpha = 0.7
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label
    )

  return(new_plot)
}

# run over all variables -------------------------------------------------

for (response_var in c(
  "oil_pct",
  "yield_lb_acre",
  "flower_50pct_days_past_planting",
  "oil_yield_lb_acre",
  "harvest_moisture_pct",
  "height_cm",
  "value_generic",
  "test_weight_lbs_bushel"
)) {
  cat(response_var)
  all_files <- list.files(
    "figure_inputs",
    pattern = response_var,
    full.names = TRUE
  )
  all_files <- all_files[grepl("rds", all_files)]
  if (response_var == "yield_lb_acre") {
    all_files <- all_files[!grepl("oil", all_files)]
  }

  trend_filename <- all_files[grepl("trend_plot.rds$", all_files)]
  check_filename <- all_files[grepl("trend_plot_check.rds$", all_files)]
  env_filename <- all_files[grepl("agronomy_gain_p.rds$", all_files)]
  gen_filename <- all_files[grepl("genetic_gain_p.rds$", all_files)]

  trends <- rebuild_trends_plot(trend_filename, check_filename)

  gen <- rebuild_gain_plot(gen_filename)
  env <- rebuild_gain_plot(env_filename)

  if (response_var == "yield_lb_acre") {
    nass_inputs <- readRDS(file.path(
      "figure_inputs", "nass_yield_trends.rds"
    ))
    
    # back of the envelope on yield improvements in individual states
    # modND <- lm (Value ~ as.numeric(year), data = nass_inputs$plot_data %>%
    # filter(state_alpha == 'ND'))
    # predict (modND, newdata = data.frame(year = 1980))
    # predict (modND, newdata = data.frame(year = 2020))
    # predict (modND, newdata = data.frame(year = 1995))
    # 
    # modSD <- lm (Value ~ as.numeric(year), data = nass_inputs$plot_data %>%
    #                filter(state_alpha == 'SD'))
    # predict (modSD, newdata = data.frame(year = 1980))
    # predict (modSD, newdata = data.frame(year = 2020))
    
    # modCO <- lm (Value ~ as.numeric(year), data = nass_inputs$plot_data %>%
    #                filter(state_alpha == 'CO'))
    # predict (modND, newdata = data.frame(year = 1995))-
    # predict (modCO, newdata = data.frame(year = 1995))
    # predict (modND, newdata = data.frame(year = 2020))-
    # predict (modCO, newdata = data.frame(year = 2020))
    # difference of 146

    nass_plot <- ggplot(nass_inputs$plot_data, aes(
      x = as.numeric(year), y = Value,
      group = state_alpha
    )) +
      geom_point() +
      geom_smooth(method = "lm", aes(color = color), se = TRUE) +
      geom_label(
        data = nass_inputs$state_labels,
        aes(x = rep(nass_inputs$label_positions$x, nrow (nass_inputs$state_labels)),
            y = rep(nass_inputs$label_positions$y,nrow (nass_inputs$state_labels)), label = label),
        hjust = 0, vjust = 1, size = 3.5,
        fill = "white", label.size = NA, alpha = 0.7
      ) +
      facet_wrap(~ factor(state_alpha, c(rev(nass_inputs$ordered_states))), ncol = 1) +
      scale_color_identity() +
      ggtitle("Yield Trends by State \n Source: USDA - NASS") +
      labs(
        subtitle = paste(
          "Per-state predictions with 95% CI and slope",
          nass_inputs$pval_text,
          sep = "\n"
        ),
        x = "Year",
        y = "lb/acre"
      ) +
      theme_minimal(base_size = 14)

    combined_plot <- (nass_plot + theme(plot.subtitle = element_text(size = 10))) +
      (trends$trial_plot +
        ggtitle("Trial Means") +
        theme(plot.subtitle = element_text(size = 10))) +
      (gen +
        ggtitle("Genetic") +
        theme(plot.subtitle = element_text(size = 10))) +
      (env +
        ggtitle("Environment") +
        theme(plot.subtitle = element_text(size = 10))) +
      (trends$check_plot +
        ggtitle("Checks") +
        theme(plot.subtitle = element_text(size = 10))) +
      plot_layout(widths = c(1, 1, 1, 1, 1))

    ggsave(
      file.path("figures", paste0(response_var, "_all_trends.jpg")),
      combined_plot,
      width = 25,
      height = 8
    )
  } else {
    combined_plot <- (trends$trial_plot +
      ggtitle("Trial Means") +
      theme(plot.subtitle = element_text(size = 10))) +
      (gen +
        ggtitle("Genetic") +
        theme(plot.subtitle = element_text(size = 10))) +
      (env +
        ggtitle("Environment") +
        theme(plot.subtitle = element_text(size = 10))) +
      (trends$check_plot +
        ggtitle("Checks") +
        theme(plot.subtitle = element_text(size = 10))) +
      plot_layout(widths = c(1, 1, 1, 1))

    ggsave(
      file.path("figures", paste0(response_var, "_all_trends.jpg")),
      combined_plot,
      width = 20,
      height = 8
    )
  }
}

# Planting/harvest dates -------------------------------------------------

response_var <- "planting_doy"
cat(response_var)
all_files <- list.files(
  "figure_inputs",
  pattern = response_var,
  full.names = TRUE
)
all_files <- all_files[grepl("rds", all_files)]
if (response_var == "yield_lb_acre") {
  all_files <- all_files[!grepl("oil", all_files)]
}

trend_filename <- all_files[grepl("trend_plot.rds$", all_files)]

nass_inputs <- readRDS(file.path(
  "figure_inputs", "nass_planting_trends.rds"
))
planting_plot <- rebuild_trends_plot_planting(
  trend_filename,
  nass_inputs
)

# Convert min_yday to a date for plotting
nass_inputs$plot_data$val_date <- as.Date(nass_inputs$plot_data$min_yday - 1, origin = "2020-01-01")
nass_inputs$plot_data <-nass_inputs$plot_data %>%
  bind_rows(data.frame(state_alpha =
                         setdiff(planting_plot$trial_plot$data$State, nass_inputs$plot_data$state_alpha),
            short_desc = "SUNFLOWER - PROGRESS, MEASURED IN PCT PLANTED"))

nass_plot_planting <- ggplot(nass_inputs$plot_data %>%
  filter(short_desc == "SUNFLOWER - PROGRESS, MEASURED IN PCT PLANTED"), aes(
  x = as.numeric(year), y = val_date,
  group = state_alpha
)) +
  geom_point() +
  geom_smooth(method = "lm", aes(color = color), se = TRUE) +
  geom_label(
    data = left_join(
      nass_inputs$state_labels %>% filter(short_desc == "SUNFLOWER - PROGRESS, MEASURED IN PCT PLANTED"),
      nass_inputs$label_positions %>% filter(short_desc == "SUNFLOWER - PROGRESS, MEASURED IN PCT PLANTED"),
      by = "state_alpha"
    ),
    aes(
      x = x, 
      y = as.Date(y - 1, origin = "2020-01-01"),
      label = label
    ),
    hjust = 0, vjust = 1, size = 3.5,
    fill = "white", label.size = NA, alpha = 0.7
  ) +
  facet_wrap(~ factor(state_alpha, c(rev(c("TX", "KS", "CO", "NE", "SD", "ND")))), ncol = 1) +
  scale_color_identity() +
  scale_y_date(date_labels = "%b-%d") +
  ggtitle("Planting Date Trends by State \n Source: USDA - NASS") +
  labs(
    subtitle = paste(
      "Per-state predictions with 95% CI and slope",
      nass_inputs$pval_text_plant,
      sep = "\n"
    ),
    x = "Year",
    y = "Planting date"
  ) +
  theme_minimal(base_size = 14)


response_var <- "harvest_doy"
cat(response_var)
all_files <- list.files(
  "figure_inputs",
  pattern = response_var,
  full.names = TRUE
)

trend_filename <- all_files[grepl("trend_plot.rds$", all_files)]


nass_inputs <- readRDS(file.path(
  "figure_inputs", "nass_harvest_trends.rds"
))


# Convert min_yday to a date for plotting
nass_inputs$plot_data <-nass_inputs$plot_data %>%
  #add blank row for NE
  bind_rows(data.frame(state_alpha =
                         setdiff(planting_plot$trial_plot$data$State, nass_inputs$plot_data$state_alpha),
                       short_desc = "SUNFLOWER - PROGRESS, MEASURED IN PCT HARVESTED"))

nass_inputs$plot_data$val_date<- as.Date(nass_inputs$plot_data$min_yday - 1, origin = "2020-01-01")
nass_plot_harvest <- ggplot(nass_inputs$plot_data %>%
  filter(short_desc == "SUNFLOWER - PROGRESS, MEASURED IN PCT HARVESTED"), aes(
  x = as.numeric(year), y = val_date,
  group = state_alpha
)) +
  geom_point() +
  geom_smooth(method = "lm", aes(color = color), se = TRUE) +
  geom_label(
    data = left_join(
      nass_inputs$state_labels %>% filter(short_desc == "SUNFLOWER - PROGRESS, MEASURED IN PCT HARVESTED"),
      nass_inputs$label_positions %>% filter(short_desc == "SUNFLOWER - PROGRESS, MEASURED IN PCT HARVESTED"),
      by = "state_alpha"
    ),
    aes(
      x = x,
      y = as.Date(y - 1, origin = "2020-01-01"),
      label = label
    ),
    hjust = 0, vjust = 1, size = 3.5,
    fill = "white", label.size = NA, alpha = 0.7
  ) +
  facet_wrap(~ factor(state_alpha, c(rev(c("TX", "KS", "CO", "NE", "SD", "ND")))), ncol = 1) +
  scale_color_identity() +
  scale_y_date(date_labels = "%b-%d") +
  ggtitle("Harvest Date Trends by State \n Source: USDA - NASS") +
  labs(
    subtitle = paste(
      "Per-state predictions with 95% CI and slope",
      nass_inputs$pval_text_harv,
      sep = "\n"
    ),
    x = "Year",
    y = "Harvest date"
  ) +
  theme_minimal(base_size = 14)


harvest_plot <- rebuild_trends_plot_planting(
  trend_filename,
  nass_inputs
)


# Set y-axis limits for planting and harvest dates
planting_min <- as.Date("2020-05-15")
planting_max <- as.Date("2020-07-15")
harvest_min <- as.Date("2020-07-15")
harvest_max <- as.Date("2020-12-31")
# Create breaks for 1st of each month for planting and harvest axes
planting_breaks <- seq(planting_min, planting_max, by = "month")
planting_labels <- ifelse(format(planting_breaks, "%b") == "Jan", "", format(planting_breaks, "%b-%d"))
harvest_breaks <- seq(harvest_min, harvest_max, by = "month")
harvest_labels <- ifelse(format(harvest_breaks, "%b") == "Jan", "", format(harvest_breaks, "%b-%d"))

combined_plot <- plot_grid(
  nass_plot_planting +
    scale_y_date(limits = c(planting_min, planting_max), breaks = planting_breaks, labels = planting_labels) +
    theme(plot.subtitle = element_text(size = 10)),
  planting_plot$trial_plot +
    scale_y_date(limits = c(planting_min, planting_max), breaks = planting_breaks, labels = planting_labels) +
    ggtitle("Trial Planting") +
    theme(plot.subtitle = element_text(size = 10)),
  nass_plot_harvest +
    scale_y_date(limits = c(harvest_min, harvest_max), breaks = harvest_breaks, labels = harvest_labels) +
    theme(plot.subtitle = element_text(size = 10)),
  harvest_plot$trial_plot +
    scale_y_date(limits = c(harvest_min, harvest_max), breaks = harvest_breaks, labels = harvest_labels) +
    ggtitle("Trial Harvest") +
    theme(plot.subtitle = element_text(size = 10)),
  ncol = 4
)

ggsave(
  file.path("figures", paste0("planting_harvest", "_all_trends.jpg")),
  combined_plot,
  width = 15,
  height = 9
)
