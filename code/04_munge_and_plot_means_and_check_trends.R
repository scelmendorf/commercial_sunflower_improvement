library (tidyverse)
library (cowplot)

# function to rebuild the trends plots (means and checks)
rebuild_trends_plot <- function(
  trend_data,
  trend_check_data,
  custom_order = NULL
) {
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
  #check_data <- trend_check_data$data

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
  #nocheck_data <- if (!is.null(trend_data$nocheck_data)) trend_data$nocheck_data else data.frame()

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
    #geom_point(data = check_data, aes(y = .data[[response_var]]), alpha = 0.05) +
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

  #combined_plot <-cowplot::plot_grid(trend_plot, trend_plot_check)
  # Combine plots side by side

  # Return all three plots and the ordered states used
  return(list(
    trial_plot = trend_plot,
    check_plot = trend_plot_check,
    #combined_plot = combined_plot#,
    ordered_states = ordered_states
  ))
}

rebuild_trends_plot_planting <- function(trend_data, custom_order = NULL) {
  library(ggplot2)
  library(dplyr)
  library(patchwork)
  trend_data <- readRDS(trend_data)
  #trend_check_data <-readRDS(trend_check_data)
  # Extract data from the lists
  meta_reg <- trend_data$meta_reg
  #meta_reg_check <- trend_check_data$data
  response_var <- trend_data$response_var
  state_labels <- trend_data$state_labels
  #state_labels_check <- trend_check_data$state_labels_check
  label_positions <- trend_data$label_positions
  #label_positions_check <- trend_check_data$label_positions_check
  pval_text <- trend_data$pval_text
  #pval_text_check <- trend_check_data$pval_text
  no_check_subset <- trend_data$data
  #check_data <- trend_check_data$data

  # Determine which states are in both datasets
  states_all <- unique(c(meta_reg$State))

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
    unique(no_check_subset$State)
  )

  meta_reg <- meta_reg %>%
    bind_rows(data.frame(State = missing_states_meta_reg))

  #missing_states_meta_reg_check <- setdiff(ordered_states, unique(meta_reg_check$State))

  #meta_reg_check<-meta_reg_check %>%
  # bind_rows(data.frame(State = missing_states_meta_reg_check))

  # Plot 1: Trial means
  # Get trial data for plotting
  #nocheck_data <- if (!is.null(trend_data$nocheck_data)) trend_data$nocheck_data else data.frame()

  trend_plot <- ggplot(meta_reg, aes(x = Year)) +
    geom_point(
      data = no_check_subset,
      aes(y = .data[[response_var]]),
      alpha = 0.05
    ) +
    #geom_point(aes(y = Estimate), alpha = 0.6) +
    #geom_errorbar(aes(ymin = Estimate - SE, ymax = Estimate + SE), width = 0.2, alpha = 0.4) +
    geom_line(aes(y = Predicted, color = colors), size = 1) +
    geom_ribbon(aes(ymin = CI_low, ymax = CI_up, fill = colors), alpha = 0.2) +
    scale_color_identity() +
    scale_fill_identity() +
    facet_wrap(~ factor(State, levels = rev(ordered_states)), ncol = 1) +
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

  # Return all three plots and the ordered states used
  return(list(
    trial_plot = trend_plot,
    #combined_plot = combined_plot#,
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
  ordered_states = c("TX", "KS", "CO", "NE", "SD", "ND")
) {
  # Load the saved plot data
  plot_data <- readRDS(rds_file_path)

  # Extract the components
  orig_plot <- plot_data$plot
  data <- plot_data$data
  state_labels <- plot_data$state_labels
  label_positions <- plot_data$label_positions
  #ordered_states <- plot_data$ordered_states
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
    data <- rbind(data, dummy_data)
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
  all_files <- all_files[grepl('rds', all_files)]
  if (response_var == "yield_lb_acre") {
    all_files <- all_files[!grepl('oil', all_files)]
  }

  trend_filename <- all_files[grepl('trend_plot.rds$', all_files)]
  check_filename <- all_files[grepl('trend_plot_check.rds$', all_files)]
  env_filename <- all_files[grepl('agronomy_gain_p.rds$', all_files)]
  gen_filename <- all_files[grepl('genetic_gain_p.rds$', all_files)]
  #
  #
  #
  # trend_data <-file.path(
  #   "figure_inputs", "oil_yield_lb_acre_trend_plot.rds")
  # trend_check_data <- file.path(
  #   "figure_inputs", "oil_yield_lb_acre_trend_plot_check.rds")

  trends <- rebuild_trends_plot(trend_filename, check_filename)

  # genetic<-file.path(
  #   "figure_inputs", "oil_yield_lb_acre_genetic_gain_p.rds")
  # environment<-file.path(
  #   "figure_inputs", "oil_yield_lb_acre_agronomy_gain_p.rds")
  gen <- rebuild_gain_plot(gen_filename)
  env <- rebuild_gain_plot(env_filename)

  if (response_var == "yield_lb_acre") {
  
  nass_inputs <-readRDS(file.path(
    "figure_inputs", "nass_yield_trends.rds")
  )

  nass_plot <- ggplot(nass_inputs$plot_data, aes(x = as.numeric(year), y = Value,
                                      group = state_alpha)) +
  geom_point() +
  geom_smooth(method = "lm", aes(color = color), se = TRUE) +
    geom_label(data = nass_inputs$state_labels,
               aes(x = label_positions$x, y = label_positions$y, label = label),
               hjust = 0, vjust = 1, size = 3.5,
               fill = "white", label.size = NA, alpha = 0.7) +
  facet_wrap(~factor(state_alpha, c(rev(ordered_states))), ncol = 1) +
  scale_color_identity() +
  ggtitle("Yield Trends by State \n Source: USDA - NASS") +
  labs(
  subtitle = paste(
        "Per-state predictions with 95% CI and slope",
        nass$inputs$pval_text,
        sep = "\n"
      ),
      x = "Year",
    y = "lb/acre")+
  theme_minimal(base_size = 14)
    
    combined_plot <- (nass_plot +theme(plot.subtitle = element_text(size = 10)))+
      (trends$trial_plot +
    ggtitle("Trial Means") +
    theme(plot.subtitle = element_text(size = 10))) +
    (gen +
      ggtitle('Genetic') +
      theme(plot.subtitle = element_text(size = 10))) +
    (env +
      ggtitle('Environment') +
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
    
  }else{
  combined_plot <- (trends$trial_plot +
    ggtitle("Trial Means") +
    theme(plot.subtitle = element_text(size = 10))) +
    (gen +
      ggtitle('Genetic') +
      theme(plot.subtitle = element_text(size = 10))) +
    (env +
      ggtitle('Environment') +
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
all_files <- all_files[grepl('rds', all_files)]
if (response_var == "yield_lb_acre") {
  all_files <- all_files[!grepl('oil', all_files)]
}

trend_filename <- all_files[grepl('trend_plot.rds$', all_files)]


planting_plot <- rebuild_trends_plot_planting(trend_filename)


response_var <- "harvest_doy"
cat(response_var)
all_files <- list.files(
  "figure_inputs",
  pattern = response_var,
  full.names = TRUE
)
all_files <- all_files[grepl('rds', all_files)]
if (response_var == "yield_lb_acre") {
  all_files <- all_files[!grepl('oil', all_files)]
}

trend_filename <- all_files[grepl('trend_plot.rds$', all_files)]


harvest_plot <- rebuild_trends_plot_planting(trend_filename)

combined_plot <- plot_grid(
  planting_plot$trial_plot +
    ggtitle("Planting Date") +
    theme(plot.subtitle = element_text(size = 10)),
  harvest_plot$trial_plot +
    ggtitle("Harvest Date") +
    theme(plot.subtitle = element_text(size = 10)),
  ncol = 2
)

ggsave(
  file.path("figures", paste0('planting_harvest', "_all_trends.jpg")),
  combined_plot,
  width = 10,
  height = 8
)
