# Load required libraries
library(tidyverse)
library(lme4)
library(modelbased)
library(ggpubr)
library(marginaleffects)

# Load pre-computed climate model results
envt_quality_fixef_noTX <- readRDS(file.path('figure_inputs', 'AIC_metric', 'sunflower_climate_fixef_noTX_final.rds'))
envt_quality_fixef_noTX_noYr <- readRDS(file.path('figure_inputs', 'AIC_metric', 'sunflower_climate_fixef_noTX_noYr_final.rds'))
envt_quality_fixef_includeTX <- readRDS(file.path('figure_inputs', 'AIC_metric', 'sunflower_climate_fixef_withTX_final.rds'))
envt_quality_fixef_includeTX_noYr <- readRDS(file.path('figure_inputs', 'AIC_metric', 'sunflower_climate_fixef_withTX_noYr_final.rds'))

envt_quality_ranef_noTX <- readRDS(file.path('figure_inputs', 'AIC_metric', 'sunflower_climate_ranef_noTX_final.rds'))
envt_quality_ranef_noTX_noYr <- readRDS(file.path('figure_inputs', 'AIC_metric', 'sunflower_climate_ranef_noTX_noYr_final.rds'))
envt_quality_ranef_includeTX <- readRDS(file.path('figure_inputs', 'AIC_metric', 'sunflower_climate_ranef_withTX_final.rds'))
envt_quality_ranef_includeTX_noYr <- readRDS(file.path('figure_inputs', 'AIC_metric', 'sunflower_climate_ranef_withTX_noYr_final.rds'))

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Build and run climwin model based on mod_sel structure
#' 
#' @param envt_quality List containing mod_sel and param_thetas data
#' @param model_type Character string, either "ranef" (mixed model) or "fixef" (fixed effects)
#' @param include_year Logical, whether to include year_centered in model
#' @return Fitted model object (lmer or lm)
run_climwin_model <- function(envt_quality, model_type = "ranef", include_year = TRUE) {
  
  # Validate inputs
  if (!is.list(envt_quality) || is.null(envt_quality$mod_sel) || is.null(envt_quality$param_thetas)) {
    stop("envt_quality must be a list containing mod_sel and param_thetas components")
  }
  
  if (!model_type %in% c("ranef", "fixef")) {
    stop("model_type must be either 'ranef' or 'fixef'")
  }
  
  # Get mod_sel data
  
  mod_sel <- envt_quality$mod_sel
  n_terms <- nrow(mod_sel)
  
  # Build signal terms based on mod_sel structure
  signal_terms <- character()
  for (i in 1:n_terms) {
    signal_name <- paste0("signal", i)
    
    # Check if quadratic term is needed
    is_quadratic <- !is.na(mod_sel$func[i]) && mod_sel$func[i] == "quad"
    
    if (is_quadratic) {
      signal_terms <- c(signal_terms, signal_name, paste0("I(", signal_name, "^2)"))
    } else {
      signal_terms <- c(signal_terms, signal_name)
    }
  }
  
  # Build formula
  formula_parts <- signal_terms
  
  if(include_year) {
    formula_parts <- c("year_centered", formula_parts)
  }
  
  # Build and fit model based on type
  if (model_type == "ranef") {
    formula_str <- paste("theta ~", paste(formula_parts, collapse = " + "), "+ (1|county_state)")
    # Use REML = TRUE for final model estimation
    mymod <- lmer(as.formula(formula_str), data = envt_quality$param_thetas, REML = TRUE)
  } else {
    formula_parts <- c(formula_parts, "county_state")
    formula_str <- paste("theta ~", paste(formula_parts, collapse = " + "))
    mymod <- lm(as.formula(formula_str), data = envt_quality$param_thetas)
  }
  
  return(mymod)
}

#' Convert weeks to year days for climate window analysis
#' 
#' @param wk Numeric vector of week values
#' @return List with min_yday and max_yday components
weeks_to_ydays <- function(wk) {
  max_ydays <- (-1 * wk * 7 + 2)
  min_ydays <- (-7 * wk - 4)
  
  return(list(
    min_yday = ceiling(min_ydays),
    max_yday = floor(max_ydays)
  ))
}

#' Get climate scaling information and axis labels
#' 
#' @param climate_var Character string indicating climate variable type
#' @param envt_quality List containing scaling_factors data
#' @return List with sd_val, mean_val, and xaxis_title components
# get_climate_scale_info("tmax_mod", envt_quality)
get_climate_scale_info <- function(climate_var, envt_quality) {
  
  # Validate inputs
  if (is.null(climate_var) || !is.character(climate_var)) {
    stop("climate_var must be a character string")
  }
  
  if (!is.list(envt_quality) || is.null(envt_quality$scaling_factors)) {
    stop("envt_quality must contain scaling_factors component")
  }
  sd_val <- case_when(
    grepl("tmax_mod", climate_var) ~ envt_quality$scaling_factors$tmaxdegc$sd,
    grepl("vpd_mod", climate_var) ~ envt_quality$scaling_factors$vpd_tmax_kpa$sd,
    grepl("photo_mod", climate_var) ~ envt_quality$scaling_factors$daylh$sd,
    grepl("GDD_mod", climate_var) ~ envt_quality$scaling_factors$GDD_modave$sd,
    grepl("temp_mod", climate_var) ~ envt_quality$scaling_factors$meanT$sd,
    grepl("precip_mod", climate_var) ~ envt_quality$scaling_factors$prcpmmday$sd,
    grepl("solrad_mod", climate_var) ~ envt_quality$scaling_factors$sradWm2$sd,
    grepl("nightT_mod", climate_var) ~ envt_quality$scaling_factors$tmindegc$sd,
    TRUE ~ NaN
  )
  
  mean_val <- case_when(
    grepl("tmax_mod", climate_var) ~ envt_quality$scaling_factors$tmaxdegc$mean,
    grepl("vpd_mod", climate_var) ~ envt_quality$scaling_factors$vpd_tmax_kpa$mean,
    grepl("photo_mod", climate_var) ~ envt_quality$scaling_factors$daylh$mean,
    grepl("GDD_mod", climate_var) ~ envt_quality$scaling_factors$GDD_modave$mean,
    grepl("temp_mod", climate_var) ~ envt_quality$scaling_factors$meanT$mean,
    grepl("precip_mod", climate_var) ~ envt_quality$scaling_factors$prcpmmday$mean,
    grepl("solrad_mod", climate_var) ~ envt_quality$scaling_factors$sradWm2$mean,
    grepl("nightT_mod", climate_var) ~ envt_quality$scaling_factors$tmindegc$mean,
    TRUE ~ NaN
  )
  
  xaxis_title <- case_when(
    grepl("tmax_mod", climate_var) ~ 'max temperature (°C)',
    grepl("vpd_mod", climate_var) ~ 'vapor pressure deficit (kPa)',
    grepl("photo_mod", climate_var) ~ 'photoperiod (hours)',
    grepl("GDD_mod", climate_var) ~ 'mean growing degree day (°C)',
    grepl("solrad_mod", climate_var) ~ 'mean solar radiation (W m^-2 day^-1)',
    grepl("precip_mod", climate_var) ~ 'mean precipitation (mm d^-1)',
    grepl("temp_mod", climate_var) ~ 'mean temperature (°C)',
    grepl("nightT_mod", climate_var) ~ 'min temperature (°C)',
    TRUE ~ NA
  )
  
  return(list(sd_val = sd_val, mean_val = mean_val, xaxis_title = xaxis_title))
}

#' Extract and process model results with visualizations
#' 
#' @param mymod Fitted model object (lmer or lm)
#' @param envt_quality List containing mod_sel and scaling data
#' @return List with model, results dataframe, and signal_plots

#process_model_results(mymod = model_result, envt_quality = config$data)
process_model_results <- function(mymod, envt_quality) {
  # Extract model coefficients based on model type
  if (class(mymod)[1] == "lmerMod") {
    results_df <- broom.mixed::tidy(mymod, effects = "fixed") %>%
      dplyr::select(term, estimate, std.error) %>%
      dplyr::rename(Parameter = term, Scaled_Coefficient = estimate, Std_Err = std.error)
  } else {
    results_df <- broom.mixed::tidy(mymod) %>%
      dplyr::select(term, estimate, std.error) %>%
      dplyr::rename(Parameter = term, Scaled_Coefficient = estimate, Std_Err = std.error)
  }
  
  # Create marginal effects plots for each climate signal
  signal_plots <- list()
  
  # Extract signal terms from model results
  signal_terms <- results_df$Parameter[grep("^signal", results_df$Parameter)]
  unique_signals <- unique(gsub("\\^2\\)$", "", gsub("I\\(signal", "signal", signal_terms)))
  
  climwin_results <- envt_quality$mod_sel %>%
    mutate(Parameter = paste0('signal', row_number()))
  
  # Loop through each signal and create a plot
  for (signal in unique_signals) {
    # Get the back-transformation values from the results that will be created
    signal_info <- climwin_results %>%
      filter(Parameter == signal)
    
    # Extract the original scale info for this signal
    climate_var <- signal_info$mod[1]
    
    # Get scaling factors and axis labels
    scale_info <- get_climate_scale_info(climate_var, envt_quality)
    sd_val <- scale_info$sd_val
    mean_val <- scale_info$mean_val
    stat_val <- signal_info$stat[1]
    
    xaxis_title <- scale_info$xaxis_title
    if (stat_val!="mean"){
      xaxis_title <- paste0(xaxis_title, " [", stat_val, "]")
    }
    
    sig <- ifelse(signal_info$pval < 0.05, "sign", "non")
    
    # Create marginal effects plot with back-transformed x-axis
    plt <- estimate_means(mymod, c(signal)) %>%
      mutate(
        original_scale = get(signal) * sd_val + mean_val,
        sig = sig
      ) %>%
      ggplot(aes(x = original_scale, y = Mean)) +
      geom_line(aes(linetype = sig)) +
      scale_linetype_manual(values = c("sign" = "solid", "non" = "dashed")) +
      guides(linetype = "none") +  # Remove linetype legend
      geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
      labs(
        x = xaxis_title,
        y = "Effect on Environmental Quality"
      ) +
      theme_bw() +
      theme(axis.title.y = element_blank())
    
    # Add original data points if available
    if (signal %in% names(envt_quality$param_thetas)) {
      plt <- plt + 
        geom_point(
          data = envt_quality$param_thetas %>% 
            mutate(original_scale = !!sym(signal) * sd_val + mean_val),
          aes(x = original_scale, y = theta), 
          color = "black", size = 0.5, alpha = 0.3
        )
    }
    
    # Create timeline inset plot
    inset_plot <- ggplot() +
      geom_segment(aes(x = -4, xend = 0, y = 0, yend = 0), 
                   color = '#156082', linewidth = 2) +
      geom_segment(aes(x = 0, xend = 20, y = 0, yend = 0), 
                   color = "#47D45A", linewidth = 2) +
      # Highlight the specific window for this signal
      geom_segment(aes(x = (-1 * signal_info$WindowOpen) - 0.5, 
                       xend = -1 * signal_info$WindowClose + 0.5, 
                       y = 0, yend = 0), 
                   color = "red", linewidth = 2) +
      scale_y_continuous(limits = c(-0.5, 0.5), expand = c(0, 0)) +
      theme_void()
    
    # Position the inset plot
    x_range <- range(plt$data$original_scale, na.rm = TRUE)
    y_range <- range(envt_quality$param_thetas$theta, na.rm = TRUE)
    
    inset_width <- diff(x_range) * 0.4
    inset_height <- diff(y_range) * 0.1
    
    plt <- plt +
      annotation_custom(
        grob = ggplotGrob(inset_plot),
        xmin = x_range[2] - inset_width,
        xmax = x_range[2],
        ymin = y_range[2] - inset_height,
        ymax = y_range[2]
      )
    
    signal_plots[[signal]] <- plt
  }
  
  # Create climwin results mapping
  climwin_results <- envt_quality$mod_sel %>%
    mutate(Parameter = paste0('signal', row_number()))

  climwin_results_quad <- envt_quality$mod_sel %>%
    mutate(Parameter = paste0('I(signal', row_number(), '^2)')) %>%
    bind_rows(climwin_results, .)

  # Map results to climate variables and scaling factors
  results_df <- results_df %>%
    left_join(climwin_results_quad, by = "Parameter") %>%
    mutate(
      std_val = get_climate_scale_info(mod, envt_quality)$sd_val,
      mean_val = get_climate_scale_info(mod, envt_quality)$mean_val
    ) %>%
    rowwise() %>%
    mutate(
      start = weeks_to_ydays(WindowOpen)$min_yday,
      end = weeks_to_ydays(WindowClose)$max_yday
    ) %>%
    ungroup() %>%
    select(
      Parameter, climate, WindowOpen, WindowClose, pval, func, stat,
      Scaled_Coefficient, Std_Err, start, end, std_val, mean_val
    )
  
  return(list(model = mymod, results = results_df, signal_plots = signal_plots))
}

# ============================================================================
# MODEL EXECUTION AND RESULTS PROCESSING
# ============================================================================

# Define model configurations
model_configs <- list(
  ranef_noTX = list(
    data = envt_quality_ranef_noTX,
    model_type = "ranef",
    include_year = TRUE
  ),
  fixef_noTX = list(
    data = envt_quality_fixef_noTX,
    model_type = "fixef", 
    include_year = TRUE
  ),
  ranef_includeTX = list(
    data = envt_quality_ranef_includeTX,
    model_type = "ranef",
    include_year = TRUE
  ),
  fixef_includeTX = list(
    data = envt_quality_fixef_includeTX,
    model_type = "fixef",
    include_year = TRUE
  ),
  ranef_noTX_noYr = list(
    data = envt_quality_ranef_noTX_noYr,
    model_type = "ranef",
    include_year = FALSE
  ),
  fixef_noTX_noYr = list(
    data = envt_quality_fixef_noTX_noYr,
    model_type = "fixef",
    include_year = FALSE
  ),
  ranef_includeTX_noYr = list(
    data = envt_quality_ranef_includeTX_noYr,
    model_type = "ranef",
    include_year = FALSE
  ),
  fixef_includeTX_noYr = list(
    data = envt_quality_fixef_includeTX_noYr,
    model_type = "fixef",
    include_year = FALSE
  )
)

# Configure warning settings for model execution
options(warn = 2)

# Run all models and collect results
all_results <- list()
all_plots <- list()

for (model_name in names(model_configs)) {
  config <- model_configs[[model_name]]
  
  cat("Running model:", model_name, "\n")
  
  # Run model and process results
  model_result <- run_climwin_model(
    config$data, 
    config$model_type, 
    config$include_year
  ) %>%
    process_model_results(., config$data)
  
  # Create combined plot with shared y-axis label
  all_plots[[model_name]] <- ggpubr::ggarrange(plotlist = model_result$signal_plots) %>%
    annotate_figure(
      .,
      left = grid::textGrob("relative environmental quality", rot = 90, gp = grid::gpar(cex = 1.3))
    )
  
  
  # Add model name to results
  model_result$results$model_name <- model_name
  
  # Store results
  all_results[[model_name]] <- model_result$results
}

# Combine all results into single dataframe
final_results_df <- bind_rows(all_results) %>%
  select(model_name, everything())

#' Extract signal name from parameter string
#' 
#' @param str Character string containing parameter name
#' @return Character string with extracted signal name
extract_signal_name <- function(str) {
  # Handle "I(signalX^2)" format
  if (grepl("^I\\(signal\\d+\\^2\\)$", str)) {
    return(gsub("I\\(([^\\^]+)\\^2\\)", "\\1", str))
  }
  # Handle "signalX" format
  else if (grepl("^signal\\d+$", str)) {
    return(str)
  }
  # Return original if no pattern matches
  else {
    return(str)
  }
}

# Display summary statistics
cat("Final results contain", nrow(final_results_df), "rows across", 
    length(unique(final_results_df$model_name)), "models\n")

# Generate and save plots for all models
for (model_name in names(all_plots)) {
  plot_filename <- paste0("figures/", model_name, "_clim_effects_AIC.png")
  ggsave(plot_filename, all_plots[[model_name]], width = 10.2, height = 8, bg = "white")
  cat("Saved plot for", model_name, "to", plot_filename, "\n")
}

# ============================================================================
# GEE PARAMETER GENERATION FUNCTIONS
# ============================================================================

# Generate GEE parameters from results
# Convert climwin results to GEE-compatible parameter format
convert_climwin_to_gee_params <- function(df, model_name) {
  
  # Filter for the specific model
  model_data <- df %>% filter(model_name == !!model_name)
  
  # Initialize parameter list
  params <- list()
  
  # Extract intercept and year coefficient
  intercept_row <- model_data %>% filter(Parameter == "(Intercept)")
  year_row <- model_data %>% filter(Parameter == "year_centered")
  
  # params$intercept <- ifelse(nrow(intercept_row) > 0, intercept_row$Scaled_Coefficient[1], 0)
  params$intercept <- intercept_row$Scaled_Coefficient
  
  # for ones with no yr
  params$yearcoef <- ifelse(nrow(year_row) > 0, year_row$Scaled_Coefficient[1], 0)
  
  # Initialize scaling factors from the dataframe
  # Extract scaling factors from std_val and mean_val columns
  scaling_data <- model_data %>% 
    filter(!is.na(climate) & !is.na(std_val) & !is.na(mean_val)) %>%
    select(climate, std_val, mean_val) %>%
    distinct()
  
  # Initialize default scaling factors
  params$tmeanMean <- 0
  params$tmeanStdDev <- 0
  params$vpdMean <- 0
  params$vpdStdDev <- 0
  params$gddMean <- 0
  params$gddStdDev <- 0
  params$tmaxMean <- 0
  params$tmaxStdDev <- 0
  params$tminMean <- 0
  params$tminStdDev <- 0
  params$prMean <- 0
  params$prStdDev <- 0
  params$rsdsWMean <- 0
  params$rsdsStdDev <- 0
  params$daylengthMean <- 0
  params$daylengthStdDev <- 0
  
  # Update scaling factors from dataframe
  for (i in 1:nrow(scaling_data)) {
    climate <- scaling_data$climate[i]
    mean_val <- scaling_data$mean_val[i]
    std_val <- scaling_data$std_val[i]
    
    if (climate == "tmax") {
      params$tmaxMean <- mean_val
      params$tmaxStdDev <- std_val
    } else if (climate == "meanT") {
      params$tmeanMean <- mean_val
      params$tmeanStdDev <- std_val
    } else if (climate == "GDD_modave") {
      params$gddMean <- mean_val
      params$gddStdDev <- std_val
    } else if (climate == "vpd") {
      params$vpdMean <- mean_val
      params$vpdStdDev <- std_val
    } else if (climate == "nightT") {
      params$tminMean <- mean_val
      params$tminStdDev <- std_val
    } else if (climate == "precip") {
      params$prMean <- mean_val
      params$prStdDev <- std_val
    } else if (climate == "solrad") {
      params$rsdsWMean <- mean_val
      params$rsdsStdDev <- std_val
    } else if (climate == "photo") {
      params$daylengthMean <- mean_val
      params$daylengthStdDev <- std_val
    }
  }
  
  # Initialize all coefficients to 0 with the correct number of parameters for each variable
  # Temperature max - up to 3 combinations
  params$tmaxLinear_1 <- 0
  params$tmaxQuadratic_1 <- 0
  params$tmaxForm_1 <- NA
  params$tmaxLinear_2 <- 0
  params$tmaxQuadratic_2 <- 0
  params$tmaxForm_2 <- NA
  params$tmaxLinear_3 <- 0
  params$tmaxQuadratic_3 <- 0
  params$tmaxForm_3 <- NA
  
  # Mean temperature - up to 2 combinations
  params$tmeanLinear_1 <- 0
  params$tmeanQuadratic_1 <- 0
  params$tmeanLinear_2 <- 0
  params$tmeanQuadratic_2 <- 0

  
  # GDD - up to 2 combinations
  params$gddLinear_1 <- 0
  params$gddQuadratic_1 <- 0
  params$gddLinear_2 <- 0
  params$gddQuadratic_2 <- 0

  
  # VPD - up to 4 combinations
  params$vpdLinear_1 <- 0
  params$vpdQuadratic_1 <- 0
  params$vpdLinear_2 <- 0
  params$vpdQuadratic_2 <- 0
  params$vpdLinear_3 <- 0
  params$vpdQuadratic_3 <- 0
  params$vpdLinear_4 <- 0
  params$vpdQuadratic_4 <- 0
  
  # Minimum temperature - up to 5 combinations
  params$tminLinear_1 <- 0
  params$tminQuadratic_1 <- 0
  params$tminForm_1 <- NA
  params$tminLinear_2 <- 0
  params$tminQuadratic_2 <- 0
  params$tminForm_2 <- NA
  params$tminLinear_3 <- 0
  params$tminQuadratic_3 <- 0
  params$tminForm_3 <- NA
  params$tminLinear_4 <- 0
  params$tminQuadratic_4 <- 0
  params$tminForm_4 <- NA
  params$tminLinear_5 <- 0
  params$tminQuadratic_5 <- 0
  params$tminForm_5 <- NA

  # Solar radiation - up to 2 combinations
  params$rsdsLinear_1 <- 0
  params$rsdsQuadratic_1 <- 0
  params$rsdsLinear_2 <- 0
  params$rsdsQuadratic_2 <- 0
  
  # Precipitation - 1 combination
  params$pptLinear <- 0
  
  # Photoperiod - 1 combination
  params$daylengthLinear <- 0
  params$daylengthQuadratic <- 0
  
  # Initialize time windows to default (not used)
  # Mean temperature windows - up to 2
  # default is 25 for params not used
  params$tmean1_start <- 150
  params$tmean1_end <- 150
  params$tmean2_start <- 150
  params$tmean2_end <- 150
  
  
  # Max temperature windows - up to 3
  params$tmax1_start <- 150
  params$tmax1_end <- 150
  params$tmax2_start <- 150
  params$tmax2_end <- 150
  params$tmax3_start <- 150
  params$tmax3_end <- 150
  
  # VPD windows - up to 4
  params$vpd1_start <- 150
  params$vpd1_end <- 150
  params$vpd2_start <- 150
  params$vpd2_end <- 150
  params$vpd3_start <- 150
  params$vpd3_end <- 150
  params$vpd4_start <- 150
  params$vpd4_end <- 150
  
  # Min temperature windows - up to 5
  params$tmin1_start <- 150
  params$tmin1_end <- 150
  params$tmin2_start <- 150
  params$tmin2_end <- 150
  params$tmin3_start <- 150
  params$tmin3_end <- 150
  params$tmin4_start <- 150
  params$tmin4_end <- 150
  params$tmin5_start <- 150
  params$tmin5_end <- 150
  
  
  # GDD windows - up to 2
  params$gdd1_start <- 150
  params$gdd1_end <- 150
  params$gdd2_start <- 150
  params$gdd2_end <- 150

  # Solar radiation windows - up to 2
  params$rsds1_start <- 150
  params$rsds1_end <- 150
  params$rsds2_start <- 150
  params$rsds2_end <- 150

  # Precipitation window - 1
  params$pr_start <- 150
  params$pr_end <- 150
  
  # Extract climate variable parameters
  signal_counter <- list(tmax = 1, meanT = 1, vpd = 1, nightT = 1, GDD_modave = 1, solrad = 1)
  
  for (i in 1:nrow(model_data)) {
  #run the first 9 to debug 10
  # for (i in 1:9){
    row <- model_data[i, ]
    param_name <- row$Parameter
    climate <- row$climate
    coef_value <- row$Scaled_Coefficient
    func_type <- row$func
    start_day <- row$start
    end_day <- row$end
    stat <-row$stat
    
    # Extract signal name from parameter name (e.g., "signal4" from "I(signal4^2)")
    signal_base <- gsub("I\\(([^\\^]+)\\^2\\)", "\\1", param_name)
    signal_base <- ifelse(grepl("^signal\\d+$", signal_base), signal_base, param_name)
    
    # only run for climate variables, skip for year, intercept
    if (!is.na(climate) && !is.na(coef_value)) {
      
      # Handle tmax parameters - up to 3 combinations
      if (climate == "tmax") {
        signal_num <- signal_counter$tmax
        
        if (grepl("^signal\\d+$", param_name)) {
          # Linear term
          if (signal_num == 1) {
            params$tmaxLinear_1 <- coef_value
            params$tmax1_start <- start_day
            params$tmax1_end <- end_day
            params$tmaxForm_1 <- stat
          } else if (signal_num == 2) {
            params$tmaxLinear_2 <- coef_value
            params$tmax2_start <- start_day
            params$tmax2_end <- end_day
            params$tmaxForm_2 <- stat
          } else if (signal_num == 3) {
            params$tmaxLinear_3 <- coef_value
            params$tmax3_start <- start_day
            params$tmax3_end <- end_day
            params$tmaxForm_3 <- stat
          }
          
          #if only a linear term no quadratic, increment counter
          if (sum(grepl(param_name, model_data$Parameter))==1){
            signal_counter$tmax <- signal_counter$tmax + 1
          }
        } else if (grepl("^I\\(signal\\d+\\^2\\)$", param_name)) {
          # Quadratic term
          if (signal_num == 1) {
            params$tmaxQuadratic_1 <- coef_value
          } else if (signal_num == 2) {
            params$tmaxQuadratic_2 <- coef_value
          } else if (signal_num == 3) {
            params$tmaxQuadratic_3 <- coef_value
          }
          signal_counter$tmax <- signal_counter$tmax + 1
        }
        # signal_counter$tmax <- signal_counter$tmax + 1
      }
      
      # Handle meanT parameters - up to 2 combinations
      else if (climate == "meanT") {
        signal_num <- signal_counter$meanT
        
        if (grepl("^signal\\d+$", param_name)) {
          # Linear term
          if (signal_num == 1) {
            params$tmeanLinear_1 <- coef_value
            params$tmean1_start <- start_day
            params$tmean1_end <- end_day
            params$tmean_end <- end_day
          } else if (signal_num == 2) {
            params$tmeanLinear_2 <- coef_value
            params$tmean2_start <- start_day
            params$tmean2_end <- end_day
          }
          #if only a linear term no quadratic, increment counter
          if (sum(grepl(param_name, model_data$Parameter))==1){
            signal_counter$meanT <- signal_counter$meanT + 1
          }
        } else if (grepl("^I\\(signal\\d+\\^2\\)$", param_name)) {
          # Quadratic term
          if (signal_num == 1) {
            params$tmeanQuadratic_1 <- coef_value
          } else if (signal_num == 2) {
            params$tmeanQuadratic_2 <- coef_value
          }
          signal_counter$meanT <- signal_counter$meanT + 1
        }
        #signal_counter$meanT <- signal_counter$meanT + 1
      }
      
      # Handle GDD_modave parameters - up to 2 combinations
      else if (climate == "GDD_modave") {
        signal_num <- signal_counter$GDD_modave
        
        if (grepl("^signal\\d+$", param_name)) {
          # Linear term
          if (signal_num == 1) {
            params$gddLinear_1 <- coef_value
            params$gdd1_start <- start_day
            params$gdd1_end <- end_day
            params$gdd_end <- end_day
          } else if (signal_num == 2) {
            params$gddLinear_2 <- coef_value
            params$gdd2_start <- start_day
            params$gdd2_end <- end_day
          }
          #if only a linear term no quadratic, increment counter
          if (sum(grepl(param_name, model_data$Parameter))==1){
            signal_counter$GDD_modave <- signal_counter$GDD_modave + 1
          }
          
        } else if (grepl("^I\\(signal\\d+\\^2\\)$", param_name)) {
          # Quadratic term
          if (signal_num == 1) {
            params$gddQuadratic_1 <- coef_value
          } else if (signal_num == 2) {
            params$gddQuadratic_2 <- coef_value
          }
          signal_counter$GDD_modave <- signal_counter$GDD_modave + 1
        }
        #signal_counter$GDD_modave <- signal_counter$GDD_modave + 1
      }
      
      # Handle VPD parameters - up to 4 combinations
      else if (climate == "vpd") {
        signal_num <- signal_counter$vpd
        
        if (grepl("^signal\\d+$", param_name)) {
          # Linear term
          if (signal_num == 1) {
            params$vpdLinear_1 <- coef_value
            params$vpd1_start <- start_day
            params$vpd1_end <- end_day
          } else if (signal_num == 2) {
            params$vpdLinear_2 <- coef_value
            params$vpd2_start <- start_day
            params$vpd2_end <- end_day
          } else if (signal_num == 3) {
            params$vpdLinear_3 <- coef_value
            params$vpd3_start <- start_day
            params$vpd3_end <- end_day
          } else if (signal_num == 4) {
            params$vpdLinear_4 <- coef_value
            params$vpd4_start <- start_day
            params$vpd4_end <- end_day
          }
          #if only a linear term no quadratic, increment counter
          if (sum(grepl(param_name, model_data$Parameter))==1){
            signal_counter$vpd <- signal_counter$vpd + 1
          }
        } else if (grepl("^I\\(signal\\d+\\^2\\)$", param_name)) {
          # Quadratic term
          if (signal_num == 1) {
            params$vpdQuadratic_1 <- coef_value
          } else if (signal_num == 2) {
            params$vpdQuadratic_2 <- coef_value
          } else if (signal_num == 3) {
            params$vpdQuadratic_3 <- coef_value
          } else if (signal_num == 4) {
            params$vpdQuadratic_4 <- coef_value
          }
           signal_counter$vpd <- signal_counter$vpd + 1
        }
        #signal_counter$vpd <- signal_counter$vpd + 1
      }
      
      # Handle nightT (minimum temperature) parameters - up to 5 combinations
      else if (climate == "nightT") {
        signal_num <- signal_counter$nightT
        
        if (grepl("^signal\\d+$", param_name)) {
          # Linear term
          if (signal_num == 1) {
            params$tminLinear_1 <- coef_value
            params$tmin1_start <- start_day
            params$tmin1_end <- end_day
            params$tmin_end <- end_day
            params$tminForm_1 <- stat
          } else if (signal_num == 2) {
            params$tminLinear_2 <- coef_value
            params$tmin2_start <- start_day
            params$tmin2_end <- end_day
            params$tminForm_2 <- stat
          } else if (signal_num == 3) {
            params$tminLinear_3 <- coef_value
            params$tmin3_start <- start_day
            params$tmin3_end <- end_day
            params$tminForm_3 <- stat
          } else if (signal_num == 4) {
            params$tminLinear_4 <- coef_value
            params$tmin4_start <- start_day
            params$tmin4_end <- end_day
            params$tminForm_4 <- stat
          } else if (signal_num == 5) {
            params$tminLinear_5 <- coef_value
            params$tmin5_start <- start_day
            params$tmin5_end <- end_day
            params$tminForm_5 <- stat
          }
          #if only a linear term no quadratic, increment counter
          if (sum(grepl(param_name, model_data$Parameter))==1){
            signal_counter$nightT <- signal_counter$nightT + 1
          }
        } else if (grepl("^I\\(signal\\d+\\^2\\)$", param_name)) {
          # Quadratic term
          if (signal_num == 1) {
            params$tminQuadratic_1 <- coef_value
          } else if (signal_num == 2) {
            params$tminQuadratic_2 <- coef_value
          } else if (signal_num == 3) {
            params$tminQuadratic_3 <- coef_value
          } else if (signal_num == 4) {
            params$tminQuadratic_4 <- coef_value
          } else if (signal_num == 5) {
            params$tminQuadratic_5 <- coef_value
          }
           signal_counter$nightT <- signal_counter$nightT + 1
        }
        #signal_counter$nightT <- signal_counter$nightT + 1
      }
      
      # Handle precipitation parameters (1 combination)
      else if (climate == "precip") {
        if (grepl("^signal\\d+$", param_name)) {
          params$pptLinear <- coef_value
          params$pr_start <- start_day
          params$pr_end <- end_day
        }
      }
      
      # Handle solar radiation parameters - up to 2 combinations
      else if (climate == "solrad") {
        signal_num <- signal_counter$solrad
        
        if (grepl("^signal\\d+$", param_name)) {
          # Linear term
          if (signal_num == 1) {
            params$rsdsLinear_1 <- coef_value
            params$rsds1_start <- start_day
            params$rsds1_end <- end_day
          } else if (signal_num == 2) {
            params$rsdsLinear_2 <- coef_value
            params$rsds2_start <- start_day
            params$rsds2_end <- end_day
          }
          #if only a linear term no quadratic, increment counter
          if (sum(grepl(param_name, model_data$Parameter))==1){
            signal_counter$solrad <- signal_counter$solrad + 1
          }
        } else if (grepl("^I\\(signal\\d+\\^2\\)$", param_name)) {
          # Quadratic term
          if (signal_num == 1) {
            params$rsdsQuadratic_1 <- coef_value
          } else if (signal_num == 2) {
            params$rsdsQuadratic_2 <- coef_value
          }
          signal_counter$solrad <- signal_counter$solrad + 1
        }
        #signal_counter$solrad <- signal_counter$solrad + 1
      }
      
      # Handle photo (daylength) parameters (1 combination)
      else if (climate == "photo") {
        if (grepl("^signal\\d+$", param_name)) {
          params$daylengthLinear <- coef_value
          params$dayl_start <- start_day
          params$dayl_end <- end_day
        } else if (grepl("^I\\(signal\\d+\\^2\\)$", param_name)) {
          params$daylengthQuadratic <- coef_value
        }
      }
    }
  }
  
  return(params)
}

#working now SCE is up to here 10/23/2025
#tst<-convert_climwin_to_gee_params(final_results_df, 'ranef_includeTX_noYr')

# Function to format parameters as GEE JavaScript syntax (UPDATED)
format_gee_params <- function(params, model_name) {
  
  # Round numeric values to 6 decimal places
  round_values <- function(x) {
    if (is.numeric(x)) {
      return(round(x, 6))
    } else {
      return(x)
    }
  }
  
  params <- lapply(params, round_values)
  
  # Create the JavaScript object string
  js_string <- paste0("'", model_name, "': {\n")
  
  # Scaling factors section
  js_string <- paste0(js_string, "    // Scaling factors\n")
  js_string <- paste0(js_string, "    tmeanMean: ", params$tmeanMean, ", tmeanStdDev: ", params$tmeanStdDev, ",\n")
  js_string <- paste0(js_string, "    vpdMean: ", params$vpdMean, ", vpdStdDev: ", params$vpdStdDev, ",\n")
  js_string <- paste0(js_string, "    gddMean: ", params$gddMean, ", gddStdDev: ", params$gddStdDev, ",\n")
  js_string <- paste0(js_string, "    tmaxMean: ", params$tmaxMean, ", tmaxStdDev: ", params$tmaxStdDev, ",\n")
  js_string <- paste0(js_string, "    tminMean: ", params$tminMean, ", tminStdDev: ", params$tminStdDev, ",\n")
  js_string <- paste0(js_string, "    prMean: ", params$prMean, ", prStdDev: ", params$prStdDev, ",\n")
  js_string <- paste0(js_string, "    rsdsWMean: ", params$rsdsWMean, ", rsdsStdDev: ", params$rsdsStdDev, ",\n")
  js_string <- paste0(js_string, "    daylengthMean: ", params$daylengthMean, ", daylengthStdDev: ", params$daylengthStdDev, ",\n\n")
  #js_string <- paste0(js_string, "    daylengthMean: ", params$daylengthMean, ", daylengthStdDev: ", params$daylengthStdDev, ",\n\n")
  
  # Regression coefficients section (UPDATED for all parameter combinations)
  js_string <- paste0(js_string, "    // Regression coefficients\n")
  js_string <- paste0(js_string, "    intercept: ", params$intercept, ", yearcoef: ", params$yearcoef, ",\n")
  
  # Max temperature parameters - up to 3
  js_string <- paste0(js_string, "    tmaxLinear_1: ", params$tmaxLinear_1, ", tmaxQuadratic_1: ", params$tmaxQuadratic_1, ", tmaxForm_1: ", params$tmaxForm_1,",\n")
  js_string <- paste0(js_string, "    tmaxLinear_2: ", params$tmaxLinear_2, ", tmaxQuadratic_2: ", params$tmaxQuadratic_2, ", tmaxForm_2: ", params$tmaxForm_2,",\n")
  js_string <- paste0(js_string, "    tmaxLinear_3: ", params$tmaxLinear_3, ", tmaxQuadratic_3: ", params$tmaxQuadratic_3, ", tmaxForm_3: ", params$tmaxForm_3,",\n")
  

  # Mean temperature parameters - up to 2
  js_string <- paste0(js_string, "    tmeanLinear_1: ", params$tmeanLinear_1, ", tmeanQuadratic_1: ", params$tmeanQuadratic_1, ", tmeanForm_1: mean,","\n")
  js_string <- paste0(js_string, "    tmeanLinear_2: ", params$tmeanLinear_2, ", tmeanQuadratic_2: ", params$tmeanQuadratic_2, ", tmeanForm_2: mean,","\n")
  
  # GDD parameters - up to 2
  js_string <- paste0(js_string, "    gddLinear_1: ", params$gddLinear_1, ", gddQuadratic_1: ", params$gddQuadratic_1, ", gddForm_1: mean,","\n")
  js_string <- paste0(js_string, "    gddLinear_2: ", params$gddLinear_2, ", gddQuadratic_2: ", params$gddQuadratic_2, ", gddForm_2: mean,","\n")
  # VPD parameters - up to 4
  js_string <- paste0(js_string, "    vpdLinear_1: ", params$vpdLinear_1, ", vpdQuadratic_1: ", params$vpdQuadratic_1, ", vpdForm_1: mean,","\n")
  js_string <- paste0(js_string, "    vpdLinear_2: ", params$vpdLinear_2, ", vpdQuadratic_2: ", params$vpdQuadratic_2, ", vpdForm_2: mean,","\n")
  js_string <- paste0(js_string, "    vpdLinear_3: ", params$vpdLinear_3, ", vpdQuadratic_3: ", params$vpdQuadratic_3, ", vpdForm_3: mean,","\n")
  js_string <- paste0(js_string, "    vpdLinear_4: ", params$vpdLinear_4, ", vpdQuadratic_4: ", params$vpdQuadratic_4, ", vpdForm_4: mean,","\n")
  
  # Minimum temperature parameters - up to 5
  js_string <- paste0(js_string, "    tminLinear_1: ", params$tminLinear_1, ", tminQuadratic_1: ", params$tminQuadratic_1, ", tminForm_1: ", params$tminForm_1,",\n")
  js_string <- paste0(js_string, "    tminLinear_2: ", params$tminLinear_2, ", tminQuadratic_2: ", params$tminQuadratic_2, ", tminForm_2: ", params$tminForm_2,",\n")
  js_string <- paste0(js_string, "    tminLinear_3: ", params$tminLinear_3, ", tminQuadratic_3: ", params$tminQuadratic_3, ", tminForm_3: ", params$tminForm_3,",\n")
  js_string <- paste0(js_string, "    tminLinear_4: ", params$tminLinear_4, ", tminQuadratic_4: ", params$tminQuadratic_4, ", tminForm_4: ", params$tminForm_4,",\n")
  js_string <- paste0(js_string, "    tminLinear_5: ", params$tminLinear_5, ", tminQuadratic_5: ", params$tminQuadratic_5, ", tminForm_5: ", params$tminForm_5,",\n")
  
  # Solar radiation parameters - up to 2
  js_string <- paste0(js_string, "    rsdsLinear_1: ", params$rsdsLinear_1, ", rsdsQuadratic_1: ", params$rsdsQuadratic_1, ", rdsForm_1: mean,","\n")
  js_string <- paste0(js_string, "    rsdsLinear_2: ", params$rsdsLinear_2, ", rsdsQuadratic_2: ", params$rsdsQuadratic_2, ", rdsForm_2: mean,","\n")
  
  # Precipitation parameter - 1
  js_string <- paste0(js_string, "    pptLinear: ", params$pptLinear, ", pptForm_1: mean,","\n")
  
  # Photoperiod parameters - 1
  js_string <- paste0(js_string, "    daylengthLinear: ", params$daylengthLinear, ", daylengthQuadratic: ", params$daylengthQuadratic, ", daylengthForm_1: mean,", "\n\n")
  
  # Time windows section (UPDATED for all parameter combinations)
  js_string <- paste0(js_string, "    // Time windows (relative to planting DOY)\n")
  
  # Mean temperature windows - up to 2
  js_string <- paste0(js_string, "    tmean1_start: ", params$tmean1_start, ", tmean1_end: ", params$tmean1_end, ",\n")
  js_string <- paste0(js_string, "    tmean2_start: ", params$tmean2_start, ", tmean2_end: ", params$tmean2_end, ",\n")
  
  # Max temperature windows - up to 3
  js_string <- paste0(js_string, "    tmax1_start: ", params$tmax1_start, ", tmax1_end: ", params$tmax1_end, ",\n")
  js_string <- paste0(js_string, "    tmax2_start: ", params$tmax2_start, ", tmax2_end: ", params$tmax2_end, ",\n")
  js_string <- paste0(js_string, "    tmax3_start: ", params$tmax3_start, ", tmax3_end: ", params$tmax3_end, ",\n")
  
  # VPD windows - up to 4
  js_string <- paste0(js_string, "    vpd1_start: ", params$vpd1_start, ", vpd1_end: ", params$vpd1_end, ",\n")
  js_string <- paste0(js_string, "    vpd2_start: ", params$vpd2_start, ", vpd2_end: ", params$vpd2_end, ",\n")
  js_string <- paste0(js_string, "    vpd3_start: ", params$vpd3_start, ", vpd3_end: ", params$vpd3_end, ",\n")
  js_string <- paste0(js_string, "    vpd4_start: ", params$vpd4_start, ", vpd4_end: ", params$vpd4_end, ",\n")
  
  # GDD windows - up to 2
  js_string <- paste0(js_string, "    gdd1_start: ", params$gdd1_start, ", gdd1_end: ", params$gdd1_end, ",\n")
  js_string <- paste0(js_string, "    gdd2_start: ", params$gdd2_start, ", gdd2_end: ", params$gdd2_end, ",\n")
  
  # Precipitation window - 1
  js_string <- paste0(js_string, "    pr_start: ", params$pr_start, ", pr_end: ", params$pr_end, ",\n")
  
  # Min temperature windows - up to 5
  js_string <- paste0(js_string, "    tmin1_start: ", params$tmin1_start, ", tmin1_end: ", params$tmin1_end, ",\n")
  js_string <- paste0(js_string, "    tmin2_start: ", params$tmin2_start, ", tmin2_end: ", params$tmin2_end, ",\n")
  js_string <- paste0(js_string, "    tmin3_start: ", params$tmin3_start, ", tmin3_end: ", params$tmin3_end, ",\n")
  js_string <- paste0(js_string, "    tmin4_start: ", params$tmin4_start, ", tmin4_end: ", params$tmin4_end, ",\n")
  js_string <- paste0(js_string, "    tmin5_start: ", params$tmin5_start, ", tmin5_end: ", params$tmin5_end, ",\n")
  
  # Solar radiation windows - up to 2
  js_string <- paste0(js_string, "    rsds1_start: ", params$rsds1_start, ", rsds1_end: ", params$rsds1_end, ",\n")
  js_string <- paste0(js_string, "    rsds2_start: ", params$rsds2_start, ", rsds2_end: ", params$rsds2_end, ",\n")
  
  # Photoperiod window - 1
  js_string <- paste0(js_string, "    dayl_start: ", ifelse(is.null(params$dayl_start) || is.na(params$dayl_start), 10, params$dayl_start), 
                      ", dayl_end: ", ifelse(is.null(params$dayl_end) || is.na(params$dayl_end), 10, params$dayl_end), "\n")
  
  js_string <- paste0(js_string, "}")
  
  return(js_string)
}

# Function to process all models in final_results_df
generate_all_gee_params <- function(final_results_df) {
  
  # Get unique model names
  model_names <- unique(final_results_df$model_name)
  
  # Generate parameters for each model
  all_params <- list()
  all_js_strings <- character()
  
  for (model_name in model_names) {
    # Convert to parameters
    params <- convert_climwin_to_gee_params(final_results_df, model_name)
    all_params[[model_name]] <- params
    
    # Format as JavaScript
    js_string <- format_gee_params(params, model_name)
    all_js_strings <- c(all_js_strings, js_string)
  }
  
  # Combine all JavaScript strings
  complete_js <- paste(all_js_strings, collapse = ",\n\n")
  complete_js <- paste0("var paramSets = {\n", complete_js, "\n};")
  
  return(list(
    parameters = all_params,
    javascript = complete_js
  ))
}


# Generate GEE parameter output for all models
gee_output <- generate_all_gee_params(final_results_df)

# GEE hates NA so use mean
gee_output$javascript <- gsub(': NA', ': mean', gee_output$javascript)
gee_output$javascript <- gsub(': mean', ': "mean"', gee_output$javascript)
gee_output$javascript <- gsub(': min', ': "min"', gee_output$javascript)
gee_output$javascript <- gsub(': max', ': "max"', gee_output$javascript)


# Save JavaScript output to file
writeLines(gee_output$javascript, file.path("figure_inputs", "gee_parameter_sets_AIC.js"))



# sce update so min/max/mean quoted and NAs are null and the daylength syntax is fixed, make "mean" the default instead of NA

# Display example parameter extraction for verification
fixef_includeTX_params <- convert_climwin_to_gee_params(final_results_df, "fixef_includeTX")
cat("Scaling factors extracted for fixef_includeTX:\n")
cat("tmaxMean:", fixef_includeTX_params$tmaxMean, "tmaxStdDev:", fixef_includeTX_params$tmaxStdDev, "\n")
cat("tmeanMean:", fixef_includeTX_params$tmeanMean, "tmeanStdDev:", fixef_includeTX_params$tmeanStdDev, "\n")
cat("gddMean:", fixef_includeTX_params$gddMean, "gddStdDev:", fixef_includeTX_params$gddStdDev, "\n")
cat("vpdMean:", fixef_includeTX_params$vpdMean, "vpdStdDev:", fixef_includeTX_params$vpdStdDev, "\n")
