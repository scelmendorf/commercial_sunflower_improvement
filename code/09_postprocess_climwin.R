library (tidyverse)
library (lme4)
library (modelbased)
library (ggpubr)
library(marginaleffects)


#bash_args <-c(
# "--daymet_path", "~/ORCC/climate_files/daymet_timeseries_cleaned.csv" ,
# "--stanfit_path", "~/ORCC/big_bayesian_files/fitted_model_FWHs_commercial_yield_all_yield_newtest.rds",
# "--tx_filter", "exclude",
# "--analysis_type", "fixef",
# "--pval_metric", "C",
# "--nrepeats", "10",
# "--delta_aic_threshold", "-5.0",
# "--outfile_base", "sunflower_climate_fixef_noTX",
# "--sunflower_data_path", "scripts/misc_sce_scripsts/testing_pubversion/sunflower_data_yield_clim_subset.csv")
# 

#args <- parser$parse_args(bash_args)
envt_quality_fixef_noTX <- readRDS('figure_inputs/sunflower_climate_fixef_noTX_final.rds')
envt_quality_fixef_noTX_noYr <- readRDS('figure_inputs/sunflower_climate_fixef_noTX_noYr_final.rds')
envt_quality_fixef_includeTX <- readRDS('figure_inputs/sunflower_climate_fixef_withTX_final.rds')
envt_quality_fixef_includeTX_noYr <- readRDS('figure_inputs/sunflower_climate_fixef_withTX_noYr_final.rds')

envt_quality_ranef_noTX <- readRDS('figure_inputs/sunflower_climate_ranef_noTX_final.rds')
envt_quality_ranef_noTX_noYr <- readRDS('figure_inputs/sunflower_climate_ranef_noTX_noYr_final.rds')
envt_quality_ranef_includeTX <- readRDS('figure_inputs/sunflower_climate_ranef_withTX_final.rds')
envt_quality_ranef_includeTX_noYr <- readRDS('figure_inputs/sunflower_climate_ranef_withTX_noYr_final.rds')

# see if we can remake meanT
#0.8753394
# envt_quality_ranef_noTX$param_thetas$signal1[envt_quality_ranef_noTX$param_thetas$trial_id == 'Eureka_2006']
# #152
# envt_quality_ranef_noTX$param_thetas$planting_doy[envt_quality_ranef_noTX$param_thetas$trial_id == 'Eureka_2006']
# 
# tst<- day_sub %>% filter(trial_id=='Eureka_2006') %>%
#   ungroup() %>%
#   mutate(week = round((yday-152+1)/7)) %>%
#   filter(week %in% c(-1:19)) %>%
#   summarize(tmax = mean (tmaxdegc))

# for a planting_doy of 152
# a -1, 19 range would be
# days 141-287


#0.8753394 is the actual tmax (scaled)


# figure out the number of terms and their form from the $mod_sel argument and then write a 
# model. each signal corresponds to one row (in order) in the envt_quality_xxx$mod_sel object
# if ranef, run as lmer with random effect of (1|county_state)
# if fixef, run as lm with fixed effect of county_state
# if no_Yr, include no other terms other than the signals
# if not no_Yar, include year_centered in the model
# example below would be for a random effects model, with y terms in mod_sel
# terms 1-3, 5-6 are quadratic, terms 1 and 7 are linear.
# c
# envt_quality_ranef_noTX$mod_sel
# 
# # ranefs
# 
# # rerun the final model
# mymod <- lmer(theta ~ year_centered+
#                 #tmax
#                 signal1 + I(signal1^2) +
#                 signal2 + I(signal2^2) +  
#                 signal3 + I(signal3^2) +  
#                 signal4 +
#                 signal5 + I(signal5^2) + 
#                 signal6 + I(signal6^2) +
#                 signal7 +
#                 (1|county_state),
#               data = envt_quality$param_thetas,
#               REML = FALSE)
# 
# fixed_effects <- fixef(mymod)
# 
# results_df <- data.frame(
#   Parameter = names(fixed_effects),
#   Scaled_Coefficient = fixed_effects
# )
# 
# climwin_results <- envt_quality$mod_sel %>%
#   mutate(Parameter = paste0('signal', row.names(.))
#   )
# 
# climwin_results_quad <- envt_quality$mod_sel %>%
#   mutate(Parameter = paste0('I(signal', row.names(.),'^2)')
#   ) %>%
#   bind_rows(., climwin_results)
# 
# 
# results_df <- results_df %>%
#   #map to terms
#   left_join(., climwin_results_quad) %>%
#   # signal1 tmas
#   mutate(
#     Std_Dev =
#       case_when(
#         grepl("signal1|signal6", Parameter) ~
#           envt_quality$scaling_factors$tmaxdegc$sd,
#         grepl("signal2", Parameter) ~
#           envt_quality$scaling_factors$meanT$sd,
#         grepl("signal3", Parameter) ~
#           envt_quality$scaling_factors$GDD_modave$sd,
#         grepl("signal4|signal5", Parameter) ~
#           envt_quality$scaling_factors$vpd_tmax_kpa$sd,
#         grepl("signal7", Parameter) ~
#           envt_quality$scaling_factors$prcpmmday$sd,
#         TRUE ~ NA
#       ),
#     # signal1 tmas
#     mean_val =
#       case_when(
#         grepl("signal1|signal6", Parameter) ~
#           envt_quality$scaling_factors$tmaxdegc$mean,
#         grepl("signal2", Parameter) ~
#           envt_quality$scaling_factors$meanT$mean,
#         grepl("signal3", Parameter) ~
#           envt_quality$scaling_factors$GDD_modave$mean,
#         grepl("signal4|signal5", Parameter) ~
#           envt_quality$scaling_factors$vpd_tmax_kpa$mean,
#         grepl("signal7", Parameter) ~
#           envt_quality$scaling_factors$prcpmmday$mean,
#         TRUE ~ NA
#       ))

#config$data, 
#config$model_type, 
#config$include_year

#envt_quality = config$data
# Function to build and run model based on mod_sel structure
run_climwin_model <- function(envt_quality, model_type = "ranef", include_year = TRUE) {
  
  # Get mod_sel data
  
  #GGally::ggpairs(envt_quality$param_thetas %>%select(starts_with('signal')))
  
  mod_sel <- envt_quality$mod_sel
  n_terms <- nrow(mod_sel)
  
  # Build signal terms
  signal_terms <- character()
  for(i in 1:n_terms) {
    signal_name <- paste0("signal", i)
    
    # Check if quadratic (assuming func column indicates this, or use other logic)
    # You may need to adjust this condition based on your mod_sel structure
    is_quadratic <- !is.na(mod_sel$func[i]) && mod_sel$func[i] == "quad"
    
    if(is_quadratic) {
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
  
  if(model_type == "ranef") {
    formula_str <- paste("theta ~", paste(formula_parts, collapse = " + "), "+ (1|county_state)")
    # Use REML = TRUE for final model estimation; set to FALSE for model comparison (AIC, etc.)
    mymod <- lmer(as.formula(formula_str), data = envt_quality$param_thetas, REML = TRUE)
  } else {
    formula_parts <- c(formula_parts, "county_state")
    formula_str <- paste("theta ~", paste(formula_parts, collapse = " + "))
    mymod <- lm(as.formula(formula_str), data = envt_quality$param_thetas)
  }
  return(mymod)
}

# with planting doy 152, the and a weeks of -1
# # the start is -7-4 == 11 days earlier
# # and the end is 19*7+2 == 136 days later
# 141 287

#empirically backed out how climwin calcs weeks
weeks_to_ydays <- function(wk) {
  max_ydays = (-1*wk*7+2)
  min_ydays <- (-7*wk-4)
  #max_ydays <- (weeks-1) + 0.5) * 7
  
  return(list(
    #center_yday = center_ydays,
    min_yday = ceiling(min_ydays),
    max_yday = floor(max_ydays)
  ))
}

# model_result <- run_climwin_model(
#   config$data, 
#   config$model_type, 
#   config$include_year
# ) %>%
#   process_model_results(., config$data)

#mymod <-model_result
#envt_quality = config$data
# Function to extract and process results
process_model_results <- function(mymod, envt_quality) {
  if(class(mymod)[1] == "lmerMod") {
    results_df <- broom.mixed::tidy(mymod, effects = "fixed") %>%
      dplyr::select(term, estimate, std.error) %>%
      dplyr::rename(Parameter = term, Scaled_Coefficient = estimate, Std_Err = std.error)
  } else {
    results_df <- broom.mixed::tidy(mymod) %>%
      dplyr::select(term, estimate, std.error) %>%
      dplyr::rename(Parameter = term, Scaled_Coefficient = estimate, Std_Err = std.error)
  }
  
  #this code is for plotting the estimated means
  #return one plot for each signal_term.
  # back-transform the 
  # Create a list to hold plots for each signal
  signal_plots <- list()
  
  # Extract number of signals from the model terms
  #model_terms <- names(fixef(mymod))
  #signal_terms <- model_terms[grep("^signal", model_terms)]
  signal_terms <- results_df$Parameter[grep("^signal", results_df$Parameter)]
  unique_signals <- unique(gsub("\\^2\\)$", "", gsub("I\\(signal", "signal", signal_terms)))
  
  climwin_results <- envt_quality$mod_sel %>%
    mutate(Parameter = paste0('signal', row.names(.))
    )
  
  
  #     climwin_results_quad <- envt_quality$mod_sel %>%
  #       mutate(Parameter = paste0('I(signal', row.names(.),'^2)')
  #       ) %>%
  #       bind_rows(., climwin_results)
  
  # Loop through each signal and create a plot
  for(signal in unique_signals) {
    # Get the back-transformation values from the results that will be created
    signal_info <- climwin_results %>%
      filter(Parameter == signal) #%>%
    #left_join(climwin_results_quad, by = c("climate", "WindowOpen", "WindowClose", "mod", "func"))
    
    # Extract the original scale info for this signal
    climate_var <- signal_info$mod[1]
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
    
    sig = ifelse(signal_info$pval<0.05, "sign", "non")
    
    # Create a plot with back-transformed x-axis
    plt <- estimate_means(mymod, c(signal)) %>%
      mutate(original_scale = get(signal) * sd_val + mean_val,
             sig = sig) %>%
      ggplot(aes(x = original_scale, y = Mean)) +
      geom_line(aes(linetype = sig)) +
      scale_linetype_manual(values = c("sign" = "solid", "non" = "dashed")) +
      #remove linetype legend
      guides(linetype = "none") +
      geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
      labs(
        x = xaxis_title,
        y = "Effect on Environmental Quality"
      ) +
      theme_bw()+
      theme(axis.title.y = element_blank())
    
    # Add points with original data if available
    if(signal %in% names(envt_quality$param_thetas)) {
      plt <- plt + 
        geom_point(
          data = envt_quality$param_thetas %>% 
            mutate(original_scale = !!sym(signal) * sd_val + mean_val),
          aes(x = original_scale, y = theta), 
          color = "black", size = 0.5, alpha = 0.3
        )
    }
    
    inset_plot <- ggplot() +
      
      # Light grey line for full range (-28 to 140)
      # geom_segment(aes(x = -28, xend = 140, y = 0, yend = 0), 
      #              color = "lightgrey", size = 4) +
      geom_segment(aes(x = -4, xend = 0, y = 0, yend = 0), 
                   color = '#156082', linewidth =2) +
      geom_segment(aes(x = 0, xend = 20, y = 0, yend = 0), 
                   color = "#47D45A", linewidth =2) +
      # Dark grey line for the specific range
      geom_segment(aes(x = (-1*signal_info$WindowOpen)-.5, 
                       xend = -1*signal_info$WindowClose+.5, 
                       y = 0, yend = 0), 
                   color = "red", linewidth =2) +
      #geom_vline(aes(xintercept = 0), color = "black", size = 1) +
      scale_y_continuous(limits = c(-0.5, 0.5), expand = c(0, 0)) +
      theme_void()
    
    x_range <- range(plt$data$original_scale, na.rm = TRUE)
    y_range <- range(envt_quality$param_thetas$theta, na.rm = TRUE)
    #Sys.sleep(1)
    # Position the inset in the upper right corner
    inset_width <- diff(x_range) * 0.4
    inset_height <- diff(y_range) * 0.1
    
    plt <-plt +
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
    mutate(Parameter = paste0('I(signal', row_number(),'^2)')) %>%
    bind_rows(climwin_results, .)
  
  #results_df
  results_df<- results_df %>%
    left_join(climwin_results_quad, by = "Parameter") %>%
    mutate(
      std_val = case_when(
        mod == "tmax_mod" ~ envt_quality$scaling_factors$tmaxdegc$sd,
        mod == "vpd_mod" ~ envt_quality$scaling_factors$vpd_tmax_kpa$sd,
        mod == "photo_mod" ~ envt_quality$scaling_factors$daylh$sd,
        mod =="GDD_mod" ~ envt_quality$scaling_factors$GDD_modave$sd,
        mod =="temp_mod" ~ envt_quality$scaling_factors$meanT$sd,
        mod =="precip_mod" ~ envt_quality$scaling_factors$prcpmmday$sd,
        mod =="solrad_mod" ~ envt_quality$scaling_factors$sradWm2$sd,
        mod =="nightT_mod" ~ envt_quality$scaling_factors$tmindegc$sd,
        TRUE ~ NaN
      ),
      mean_val = case_when(
        mod =="tmax_mod"~ envt_quality$scaling_factors$tmaxdegc$mean,
        mod =="vpd_mod" ~ envt_quality$scaling_factors$vpd_tmax_kpa$mean,
        mod =="photo_mod" ~ envt_quality$scaling_factors$daylh$mean,
        mod =="GDD_mod" ~ envt_quality$scaling_factors$GDD_modave$mean,
        mod =="temp_mod" ~ envt_quality$scaling_factors$meanT$mean,
        mod =="precip_mod"~ envt_quality$scaling_factors$prcpmmday$mean,
        mod =="solrad_mod"~envt_quality$scaling_factors$sradWm2$mean,
        mod =="nightT_mod" ~ envt_quality$scaling_factors$tmindegc$mean,
        TRUE ~ NaN
      )
    )%>%
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

# this is pretty close I just need to unscale the theta
# and add the right names
# possibly want to show them as partials against the raw
# sce start here to make the marginal effects plots


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

options(warn = 2)
# Run all models and collect results
all_results <- list()
all_plots <-list()

for(model_name in names(model_configs)) {
  config <- model_configs[[model_name]]
  
  cat("Running model:", model_name, "\n")
  
  # Run model
  model_result <- run_climwin_model(
    config$data, 
    config$model_type, 
    config$include_year
  ) %>%
    process_model_results(., config$data)
  
  # Add a larger left margin to all plots to accommodate the shared y-axis title
  # model_result$signal_plots <- lapply(
  #   model_result$signal_plots,
  #   function(p) p + theme(plot.margin = margin(5.5, 5.5, 5.5, 30, "pt"))
  # )
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

extract_signal_name <- function(str) {
  # Handle "I(signalX^2)" format
  if(grepl("^I\\(signal\\d+\\^2\\)$", str)) {
    return(gsub("I\\(([^\\^]+)\\^2\\)", "\\1", str))
  }
  # Handle "signalX" format
  else if(grepl("^signal\\d+$", str)) {
    return(str)
  }
  # Return original if no pattern matches
  else {
    return(str)
  }
}

# count how many there are

tst <- final_results_df %>%
  rowwise() %>%
  mutate(signal = extract_signal_name(Parameter)) %>%
  ungroup() %>% filter(!is.na(climate)) %>% select(model_name, signal, climate) %>%
  distinct() %>% group_by(model_name, climate) %>% dplyr::tally()


# Display summary
cat("Final results contain", nrow(final_results_df), "rows across", 
    length(unique(final_results_df$model_name)), "models\n")

# plot all
for (model_name in names(all_plots)) {
  plot_filename <- paste0("figures/", model_name, "_clim_effects.png")
  ggsave(plot_filename, all_plots[[model_name]], width = 10, height = 8, bg = "white")
  cat("Saved plot for", model_name, "to", plot_filename, "\n")
}

# paste params into all_climvars_all_mods in GEE
View(final_results_df %>% filter(grepl('fixef_includeTX', model_name)))

# get Claude AI to convert these into param files to paste into GEE code
# Function to convert climwin results dataframe to GEE parameter syntax
convert_climwin_to_gee_params <- function(df, model_name) {
  
  # Filter for the specific model
  model_data <- df %>% filter(model_name == !!model_name)
  
  # Initialize parameter list
  params <- list()
  
  # Extract intercept and year coefficient
  intercept_row <- model_data %>% filter(Parameter == "(Intercept)")
  year_row <- model_data %>% filter(Parameter == "year_centered")
  
  params$intercept <- ifelse(nrow(intercept_row) > 0, intercept_row$Scaled_Coefficient[1], 0)
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
  params$tmaxLinear_2 <- 0
  params$tmaxQuadratic_2 <- 0
  params$tmaxLinear_3 <- 0
  params$tmaxQuadratic_3 <- 0
  
  # Mean temperature - up to 2 combinations
  params$tmeanLinear_1 <- 0
  params$tmeanQuadratic_1 <- 0
  params$tmeanLinear_2 <- 0
  params$tmeanQuadratic_2 <- 0
  params$tmeanLinear <- 0  # For backward compatibility
  params$tmeanQuadratic <- 0  # For backward compatibility
  
  # GDD - up to 2 combinations
  params$gddLinear_1 <- 0
  params$gddQuadratic_1 <- 0
  params$gddLinear_2 <- 0
  params$gddQuadratic_2 <- 0
  params$gddLinear <- 0  # For backward compatibility
  params$gddQuadratic <- 0  # For backward compatibility
  
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
  params$tminLinear_2 <- 0
  params$tminQuadratic_2 <- 0
  params$tminLinear_3 <- 0
  params$tminQuadratic_3 <- 0
  params$tminLinear_4 <- 0
  params$tminQuadratic_4 <- 0
  params$tminLinear_5 <- 0
  params$tminQuadratic_5 <- 0
  params$tminLinear <- 0  # For backward compatibility
  params$tminQuadratic <- 0  # For backward compatibility
  
  # Solar radiation - up to 2 combinations
  params$rsdsLinear_1 <- 0
  params$rsdsQuadratic_1 <- 0
  params$rsdsLinear_2 <- 0
  params$rsdsQuadratic_2 <- 0
  params$rsdsLinear <- 0  # For backward compatibility
  params$rsdsQuadratic <- 0  # For backward compatibility
  
  # Precipitation - 1 combination
  params$pptLinear <- 0
  
  # Photoperiod - 1 combination
  params$daylengthLinear <- 0
  params$daylengthQuadratic <- 0
  
  # Initialize time windows to default (not used)
  # Mean temperature windows - up to 2
  params$tmean1_start <- 10
  params$tmean1_end <- 10
  params$tmean2_start <- 10
  params$tmean2_end <- 10
  
  
  # Max temperature windows - up to 3
  params$tmax1_start <- 10
  params$tmax1_end <- 10
  params$tmax2_start <- 10
  params$tmax2_end <- 10
  params$tmax3_start <- 10
  params$tmax3_end <- 10
  
  # VPD windows - up to 4
  params$vpd1_start <- 10
  params$vpd1_end <- 10
  params$vpd2_start <- 10
  params$vpd2_end <- 10
  params$vpd3_start <- 10
  params$vpd3_end <- 10
  params$vpd4_start <- 10
  params$vpd4_end <- 10
  
  # Min temperature windows - up to 5
  params$tmin1_start <- 10
  params$tmin1_end <- 10
  params$tmin2_start <- 10
  params$tmin2_end <- 10
  params$tmin3_start <- 10
  params$tmin3_end <- 10
  params$tmin4_start <- 10
  params$tmin4_end <- 10
  params$tmin5_start <- 10
  params$tmin5_end <- 10
  
  
  # GDD windows - up to 2
  params$gdd1_start <- 10
  params$gdd1_end <- 10
  params$gdd2_start <- 10
  params$gdd2_end <- 10
  params$gdd_start <- 10  # For backward compatibility
  params$gdd_end <- 10  # For backward compatibility
  
  # Solar radiation windows - up to 2
  params$rsds1_start <- 10
  params$rsds1_end <- 10
  params$rsds2_start <- 10
  params$rsds2_end <- 10
  params$rsds_start <- 10  # For backward compatibility
  params$rsds_end <- 10  # For backward compatibility
  
  # Precipitation window - 1
  params$pr_start <- 10
  params$pr_end <- 10
  
  # Extract climate variable parameters
  signal_counter <- list(tmax = 1, meanT = 1, vpd = 1, nightT = 1, GDD_modave = 1, solrad = 1)
  
  for (i in 1:nrow(model_data)) {
    row <- model_data[i, ]
    param_name <- row$Parameter
    climate <- row$climate
    coef_value <- row$Scaled_Coefficient
    func_type <- row$func
    start_day <- row$start
    end_day <- row$end
    
    # Extract signal name from parameter name (e.g., "signal4" from "I(signal4^2)")
    signal_base <- gsub("I\\(([^\\^]+)\\^2\\)", "\\1", param_name)
    signal_base <- ifelse(grepl("^signal\\d+$", signal_base), signal_base, param_name)
    
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
          } else if (signal_num == 2) {
            params$tmaxLinear_2 <- coef_value
            params$tmax2_start <- start_day
            params$tmax2_end <- end_day
          } else if (signal_num == 3) {
            params$tmaxLinear_3 <- coef_value
            params$tmax3_start <- start_day
            params$tmax3_end <- end_day
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
      }
      
      # Handle meanT parameters - up to 2 combinations
      else if (climate == "meanT") {
        signal_num <- signal_counter$meanT
        
        if (grepl("^signal\\d+$", param_name)) {
          # Linear term
          if (signal_num == 1) {
            params$tmeanLinear_1 <- coef_value
            params$tmeanLinear <- coef_value  # Backward compatibility
            params$tmean1_start <- start_day
            params$tmean1_end <- end_day
            params$tmean_start <- start_day  # Backward compatibility
            params$tmean_end <- end_day
          } else if (signal_num == 2) {
            params$tmeanLinear_2 <- coef_value
            params$tmean2_start <- start_day
            params$tmean2_end <- end_day
          }
        } else if (grepl("^I\\(signal\\d+\\^2\\)$", param_name)) {
          # Quadratic term
          if (signal_num == 1) {
            params$tmeanQuadratic_1 <- coef_value
            params$tmeanQuadratic <- coef_value  # Backward compatibility
          } else if (signal_num == 2) {
            params$tmeanQuadratic_2 <- coef_value
          }
          signal_counter$meanT <- signal_counter$meanT + 1
        }
      }
      
      # Handle GDD_modave parameters - up to 2 combinations
      else if (climate == "GDD_modave") {
        signal_num <- signal_counter$GDD_modave
        
        if (grepl("^signal\\d+$", param_name)) {
          # Linear term
          if (signal_num == 1) {
            params$gddLinear_1 <- coef_value
            params$gddLinear <- coef_value  # Backward compatibility
            params$gdd1_start <- start_day
            params$gdd1_end <- end_day
            params$gdd_start <- start_day  # Backward compatibility
            params$gdd_end <- end_day
          } else if (signal_num == 2) {
            params$gddLinear_2 <- coef_value
            params$gdd2_start <- start_day
            params$gdd2_end <- end_day
          }
        } else if (grepl("^I\\(signal\\d+\\^2\\)$", param_name)) {
          # Quadratic term
          if (signal_num == 1) {
            params$gddQuadratic_1 <- coef_value
            params$gddQuadratic <- coef_value  # Backward compatibility
          } else if (signal_num == 2) {
            params$gddQuadratic_2 <- coef_value
          }
          signal_counter$GDD_modave <- signal_counter$GDD_modave + 1
        }
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
      }
      
      # Handle nightT (minimum temperature) parameters - up to 5 combinations
      else if (climate == "nightT") {
        signal_num <- signal_counter$nightT
        
        if (grepl("^signal\\d+$", param_name)) {
          # Linear term
          if (signal_num == 1) {
            params$tminLinear_1 <- coef_value
            params$tminLinear <- coef_value  # Backward compatibility
            params$tmin1_start <- start_day
            params$tmin1_end <- end_day
            params$tmin_start <- start_day  # Backward compatibility
            params$tmin_end <- end_day
          } else if (signal_num == 2) {
            params$tminLinear_2 <- coef_value
            params$tmin2_start <- start_day
            params$tmin2_end <- end_day
          } else if (signal_num == 3) {
            params$tminLinear_3 <- coef_value
            params$tmin3_start <- start_day
            params$tmin3_end <- end_day
          } else if (signal_num == 4) {
            params$tminLinear_4 <- coef_value
            params$tmin4_start <- start_day
            params$tmin4_end <- end_day
          } else if (signal_num == 5) {
            params$tminLinear_5 <- coef_value
            params$tmin5_start <- start_day
            params$tmin5_end <- end_day
          }
        } else if (grepl("^I\\(signal\\d+\\^2\\)$", param_name)) {
          # Quadratic term
          if (signal_num == 1) {
            params$tminQuadratic_1 <- coef_value
            params$tminQuadratic <- coef_value  # Backward compatibility
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
            params$rsdsLinear <- coef_value  # Backward compatibility
            params$rsds1_start <- start_day
            params$rsds1_end <- end_day
            params$rsds_start <- start_day  # Backward compatibility
            params$rsds_end <- end_day
          } else if (signal_num == 2) {
            params$rsdsLinear_2 <- coef_value
            params$rsds2_start <- start_day
            params$rsds2_end <- end_day
          }
        } else if (grepl("^I\\(signal\\d+\\^2\\)$", param_name)) {
          # Quadratic term
          if (signal_num == 1) {
            params$rsdsQuadratic_1 <- coef_value
            params$rsdsQuadratic <- coef_value  # Backward compatibility
          } else if (signal_num == 2) {
            params$rsdsQuadratic_2 <- coef_value
          }
          signal_counter$solrad <- signal_counter$solrad + 1
        }
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
  
  # Regression coefficients section (UPDATED for all parameter combinations)
  js_string <- paste0(js_string, "    // Regression coefficients\n")
  js_string <- paste0(js_string, "    intercept: ", params$intercept, ", yearcoef: ", params$yearcoef, ",\n")
  
  # Max temperature parameters - up to 3
  js_string <- paste0(js_string, "    tmaxLinear_1: ", params$tmaxLinear_1, ", tmaxQuadratic_1: ", params$tmaxQuadratic_1, ",\n")
  js_string <- paste0(js_string, "    tmaxLinear_2: ", params$tmaxLinear_2, ", tmaxQuadratic_2: ", params$tmaxQuadratic_2, ",\n")
  js_string <- paste0(js_string, "    tmaxLinear_3: ", params$tmaxLinear_3, ", tmaxQuadratic_3: ", params$tmaxQuadratic_3, ",\n")
  
  # Mean temperature parameters - up to 2
  js_string <- paste0(js_string, "    tmeanLinear_1: ", params$tmeanLinear_1, ", tmeanQuadratic_1: ", params$tmeanQuadratic_1, ",\n")
  js_string <- paste0(js_string, "    tmeanLinear_2: ", params$tmeanLinear_2, ", tmeanQuadratic_2: ", params$tmeanQuadratic_2, ",\n")
  
  # GDD parameters - up to 2
  js_string <- paste0(js_string, "    gddLinear_1: ", params$gddLinear_1, ", gddQuadratic_1: ", params$gddQuadratic_1, ",\n")
  js_string <- paste0(js_string, "    gddLinear_2: ", params$gddLinear_2, ", gddQuadratic_2: ", params$gddQuadratic_2, ",\n")
  
  # VPD parameters - up to 4
  js_string <- paste0(js_string, "    vpdLinear_1: ", params$vpdLinear_1, ", vpdQuadratic_1: ", params$vpdQuadratic_1, ",\n")
  js_string <- paste0(js_string, "    vpdLinear_2: ", params$vpdLinear_2, ", vpdQuadratic_2: ", params$vpdQuadratic_2, ",\n")
  js_string <- paste0(js_string, "    vpdLinear_3: ", params$vpdLinear_3, ", vpdQuadratic_3: ", params$vpdQuadratic_3, ",\n")
  js_string <- paste0(js_string, "    vpdLinear_4: ", params$vpdLinear_4, ", vpdQuadratic_4: ", params$vpdQuadratic_4, ",\n")
  
  # Minimum temperature parameters - up to 5
  js_string <- paste0(js_string, "    tminLinear_1: ", params$tminLinear_1, ", tminQuadratic_1: ", params$tminQuadratic_1, ",\n")
  js_string <- paste0(js_string, "    tminLinear_2: ", params$tminLinear_2, ", tminQuadratic_2: ", params$tminQuadratic_2, ",\n")
  js_string <- paste0(js_string, "    tminLinear_3: ", params$tminLinear_3, ", tminQuadratic_3: ", params$tminQuadratic_3, ",\n")
  js_string <- paste0(js_string, "    tminLinear_4: ", params$tminLinear_4, ", tminQuadratic_4: ", params$tminQuadratic_4, ",\n")
  js_string <- paste0(js_string, "    tminLinear_5: ", params$tminLinear_5, ", tminQuadratic_5: ", params$tminQuadratic_5, ",\n")
  
  # Solar radiation parameters - up to 2
  js_string <- paste0(js_string, "    rsdsLinear_1: ", params$rsdsLinear_1, ", rsdsQuadratic_1: ", params$rsdsQuadratic_1, ",\n")
  js_string <- paste0(js_string, "    rsdsLinear_2: ", params$rsdsLinear_2, ", rsdsQuadratic_2: ", params$rsdsQuadratic_2, ",\n")
  
  # Precipitation parameter - 1
  js_string <- paste0(js_string, "    pptLinear: ", params$pptLinear, ",\n")
  
  # Photoperiod parameters - 1
  js_string <- paste0(js_string, "    daylengthLinear: ", params$daylengthLinear, ", daylengthQuadratic: ", params$daylengthQuadratic, ",\n\n")
  
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


# Example usage:
# Generate GEE parameters using scaling factors from the dataframe
gee_output <- generate_all_gee_params(final_results_df)

# Print the JavaScript code
cat(gee_output$javascript)

# Save to file
writeLines(gee_output$javascript, file.path("figure_inputs", "gee_parameter_sets.js"))

# You can also check the extracted parameters for a specific model
fixef_includeTX_params <- convert_climwin_to_gee_params(final_results_df, "fixef_includeTX")
print("Scaling factors extracted for fixef_includeTX:")
print(paste("tmaxMean:", fixef_includeTX_params$tmaxMean, "tmaxStdDev:", fixef_includeTX_params$tmaxStdDev))
print(paste("tmeanMean:", fixef_includeTX_params$tmeanMean, "tmeanStdDev:", fixef_includeTX_params$tmeanStdDev))
print(paste("gddMean:", fixef_includeTX_params$gddMean, "gddStdDev:", fixef_includeTX_params$gddStdDev))
print(paste("vpdMean:", fixef_includeTX_params$vpdMean, "vpdStdDev:", fixef_includeTX_params$vpdStdDev))
