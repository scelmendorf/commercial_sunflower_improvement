#!/usr/bin/env Rscript

##############################################
# This file takes the environmental scores (thetas)
# from the FW_bayesian models and relates them to 
# real climate variables.

# By EIC
# Updated by sce to stepwise select and run on all yield
# updated 28 July 2025
# Modified for command line usage
##############################################


# EXAMPLE USAGE WITH SCREEN (for long-running analyses):
# 
# Start a screen session:
# screen -S climate_analysis
# 
# Run the analysis (basic example):
#
# Rscript code/07_site_envt_quality.R \
#   --daymet_path "~/ORCC/climate_files/daymet_timeseries_cleaned.csv" \
#   --stanfit_path "~/ORCC/big_bayesian_files/fitted_model_FWHs_commercial_yield_all_yield_newtest.rds" \
#   --tx_filter exclude \
#   --analysis_type ranef \
#   --pval_metric AIC \
#   --nrepeats 10 \
#   --delta_aic_threshold -5.0 \
#   --outfile_base "figure_inputs/sunflower_climate_ranef_noTX" \
#   --sunflower_data_path "data_derived/sunflower_data_yield_clim_subset.csv"
# 
# Detach from screen: Ctrl+A then D
# Reattach to screen: screen -r climate_analysis
# List screen sessions: screen -ls
# Kill screen session: screen -S climate_analysis -X quit
#
# ARGUMENTS:
# --tx_filter: include/exclude Texas data (default: exclude)
# --analysis_type: ranef/fixef analysis type (default: fixef) 
# --daymet_path: path to daymet climate data CSV (REQUIRED)
# --stanfit_path: path to fitted stanfit RDS file (REQUIRED)
# --pval_metric: AIC/C for p-value calculation (default: AIC)
# --nrepeats: number of randomization repeats (default: 100)
# --delta_aic_threshold: stopping threshold for delta AIC (default: -5.0)
# --outfile_base: base name for output files (default: climate_analysis_output)
# --sunflower_data_path: path to sunflower data CSV (has default path)
#
##############################################


# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(rstan)
  library(climwin)
  library(lme4)
  library(argparse)
})

# Set up argument parser
parser <- ArgumentParser(description='Run climate window analysis on sunflower yield data')

parser$add_argument('--tx_filter', 
                    choices=c('include', 'exclude'), 
                    default='exclude',
                    help='Whether to include or exclude Texas data (default: exclude)')

parser$add_argument('--analysis_type', 
                    choices=c('ranef', 'fixef'), 
                    default='fixef',
                    help='Type of analysis to run: ranef or fixef (default: fixef)')

parser$add_argument('--daymet_path', 
                    required=TRUE,
                    help='Path to daymet timeseries CSV file')

parser$add_argument('--stanfit_path', 
                    required=TRUE,
                    help='Path to fitted stanfit model RDS file')

parser$add_argument('--pval_metric', 
                    choices=c('AIC', 'C'), 
                    default='AIC',
                    help='P-value metric to use (default: AIC)')

parser$add_argument('--nrepeats', 
                    type='integer', 
                    default=100,
                    help='Number of randomization repeats (default: 100)')

parser$add_argument('--delta_aic_threshold', 
                    type='double', 
                    default=-5.0,
                    help='Delta AIC threshold for stopping (default: -5.0)')

parser$add_argument('--outfile_base', 
                    default='climate_analysis_output',
                    help='Base name for output files (default: climate_analysis_output)')

parser$add_argument('--sunflower_data_path', 
                    default='scripts/misc_sce_scripsts/testing_pubversion/sunflower_data_yield_clim_subset.csv',
                    help='Path to sunflower data CSV file')

# At the very top, before argparse
cat("=== RAW COMMAND LINE ARGUMENTS ===\n")
raw_args <- commandArgs(trailingOnly = TRUE)
cat("Number of arguments:", length(raw_args), "\n")
for (i in seq_along(raw_args)) {
  cat(sprintf("  [%d]: '%s'\n", i, raw_args[i]))
}

# Parse arguments
args <- parser$parse_args()

# After parsing
args <- parser$parse_args()
cat("\n=== PARSED ARGUMENTS ===\n")
print(args)
cat("\n")

cat("Starting climate analysis with parameters:\n")
cat("  TX filter:", args$tx_filter, "\n")
cat("  Analysis type:", args$analysis_type, "\n")
cat("  Daymet path:", args$daymet_path, "\n")
cat("  Stanfit path:", args$stanfit_path, "\n")
cat("  P-value metric:", args$pval_metric, "\n")
cat("  N repeats:", args$nrepeats, "\n")
cat("  Delta AIC threshold:", args$delta_aic_threshold, "\n")
cat("  Output file base:", args$outfile_base, "\n\n")

# Read data
cat("Reading sunflower data...\n")
mydata <- read.csv(args$sunflower_data_path) %>%
  rename(genotype = Unif_Name) 

cat("Reading climate data...\n")
daymet <- read.csv(args$daymet_path)
daymet <- daymet %>%
  rename(Location = location, lat = latitude, lon = longitude, Year = year)

state_mapping <- mydata %>%
  select(Location, State, Year) %>%
  distinct()

cat("Reading fitted model...\n")
stanfit_yield <- readRDS(args$stanfit_path)

param_summary <- summary(stanfit_yield)$summary

# Create a data frame of parameters and their confidence intervals
param_data <- as.data.frame(param_summary) %>%
  rownames_to_column(var = "Parameter") %>%
  mutate(
    Lower = `2.5%`,  # Lower bound of the 95% CI
    Upper = `97.5%`, # Upper bound of the 95% CI
    Mean = mean
  ) %>%
  select(Parameter, Mean, Lower, Upper)

mydata_yield_params = mydata %>%
  mutate(environment = as.numeric(as.factor(as.character(mydata$trial_id))),
         germplasm = as.numeric(as.factor(as.character(mydata$genotype)))) %>%
  mutate(environment = paste0('theta[', environment, ']'),
         germplasm_1 = paste0('alpha[',  germplasm, ']'),
         germplasm_2 = paste0('lambda[',  germplasm, ']')) %>%
  #add lambdas --> sensitivities to the environment
  left_join(., param_data %>%
              filter(grepl('lambda', Parameter)) %>%
              filter(!grepl('sigma', Parameter)),
            by = c("germplasm_2" = "Parameter")) %>%
  rename(lambda = Mean,
         lambda_lower = Lower,
         lambda_upper = Upper) %>%
  #add alphas --> mean phenology
  left_join(., param_data %>%
              filter(grepl('alpha', Parameter)) %>%
              filter(!grepl('sigma', Parameter)),
            by = c("germplasm_1" = "Parameter"))%>%
  rename(alpha = Mean,
         alpha_lower = Lower,
         alpha_upper = Upper) %>%
  #add thetas --> mean envt
  left_join(., param_data %>%
              filter(grepl('theta', Parameter)) %>%
              filter(!grepl('sigma', Parameter)),
            by = c("environment" = "Parameter")) %>%
  rename(theta = Mean,
         theta_lower = Lower,
         theta_upper = Upper) %>%
  mutate(nu = param_data %>%
           filter(grepl('nu', Parameter)) %>%
           pull(Mean)) %>%
  mutate(ypred = alpha + theta + lambda*theta)

# overall fit of the model - pretty good
cat("Model fit R-squared:", summary(lm(ypred ~ yield_lb_acre, data = mydata_yield_params))$adj.r.squared, "\n")

# Clim win exploration -----------------------------------------------------

# extract just the site environmental data (biological data)
# here removing the genotype parameters
mydata_yield_params_thetas <- mydata_yield_params %>%
  select(trial_id, Location, county_state, named_location, lat, lon, Year, planting_doy, environment,
         theta, theta_lower, theta_upper) %>% 
  distinct()

# need to have only one planting date per trial
# so check if there are multiple planting dates per trial
mydata_yield_params_thetas %>% 
  group_by(trial_id) %>%
  summarise(n = n()) %>% 
  filter(n > 1) %>%
  left_join(., mydata_yield_params_thetas)

#eic code to deal with planting dates close but not identical
mydata_yield_params_thetas <- mydata_yield_params_thetas %>%
  group_by(Location, Year) %>%
  mutate(planting_doy1 = mean(planting_doy)) %>%
  ungroup() %>%
  distinct(Location, lat, lon, Year, planting_doy1, environment,
           theta, theta_lower, theta_upper, .keep_all = T) %>%
  mutate(planting_date = format(as.Date(planting_doy1-1, origin=paste0(Year, "-01-01")), "%d/%m/%Y"), 
         trial_id_loc = paste(Location, Year, sep = "_"), .after = planting_doy) %>%
  filter(!is.na(planting_date),
         Year > 1979)

mydata_yield_params_thetas <- mydata_yield_params_thetas %>%
  left_join(., (state_mapping %>%
                  select(Location, Year, State) %>% distinct())
  )  %>%
  group_by(county_state) %>%
  mutate(year_centered = Year - mean(Year, na.rm = TRUE)) %>%
  ungroup()

# Apply TX filter
if (args$tx_filter == "exclude") {
  cat("Filtering out Texas data...\n")
  mydata_yield_params_thetas <- mydata_yield_params_thetas %>%
    filter(State != 'TX')
} else {
  cat("Including all states (including Texas)...\n")
}

# Makes climate data subset
tbase = 6.7
tmax = 26

# calc es from equations here:
# https://www.nature.com/articles/s41597-025-04544-5
# note this overestimates actual flux so needs a scalar correction
# applied to it. However, that scalar will end up in the regression coefficients
# if applied equally to all (all same climate class, so can ignore here)

#similar equtions can be found here
#https://dataverse.harvard.edu/api/access/datafile/6104029
# see tutorial and equations for nasa power Allen et al. (1998):

#Allen, R.G., L.S. Pereira, D. Raes, M. Smith. 1998. Crop evapotranspiration - Guidelines for
#computing crop water requirements. FAO Irrigation and drainage, 56. FAO - Food and Agriculture
#Organization of the United Nations. Rome, Italy. ISBN 92-5-104219-5. http://www.fao.org/3/
#  x0490e/x0490e00.htm#Contents

# Function to calculate saturation vapor pressure
calc_es <- function(temp_c) {
  # Calculate saturation vapor pressure using Tetens equation
  # For T >= 0°C: over liquid water
  # For T < 0°C: over ice
  # Result is in kPa
  
  es <- ifelse(temp_c >= 0,
               # Over liquid water (T >= 0°C)
               0.611 * exp((17.27 * temp_c) / (temp_c + 237.3)),
               # Over ice (T < 0°C) 
               0.611 * exp((21.875 * temp_c) / (temp_c + 265.5)))
  return(es)
}

daymet_mydata <- daymet %>%
  semi_join(., mydata_yield_params_thetas, by = c("Location", "lat", "lon", "Year")) %>%
  mutate(date = format(ymd(date), "%d/%m/%Y"),
         trial_id_loc = paste(Location, Year, sep = "_"), .after = Location) %>%
  mutate(GDD_modave = pmax(0, (ifelse(tmaxdegc > tmax, tmax, tmaxdegc) + 
                                 ifelse(tmindegc < tbase, tbase, tmindegc)) / 2 - tbase),
         meanT = (tmaxdegc + tmindegc)/2,
         daylh = dayls/3600,
         es_tmax = calc_es(tmaxdegc), # calc VPD off max daily temp,
         vp_kpa = vpPa / 1000,
         vpd_tmax_kpa = es_tmax - vp_kpa
  ) 

# Define function to fit climwin with random effects ------------------------------------------

run_climate_window_analysis_ranef <- function(day_sub, param_thetas, nrepeats = 100, 
                                              pvalmetric = "AIC", delta_aic_threshold = -5,
                                              outfile_base = 'tst_ranef_screen') {
  
  # Define climate variables with their configurations
  climate_vars <- list(
    temp_mod = list(
      xvar = list(meanT = day_sub$meanT),
      range = c(-2, -20),
      stat = "mean",
      func = c("lin", "quad")
    ),
    GDD_mod = list(
      xvar = list(GDD_modave = day_sub$GDD_modave),
      range = c(-2, -20),
      stat = "mean",
      func = c("lin", "quad")
    ),
    nightT_mod = list(
      xvar = list(nightT = day_sub$tmindegc),
      range = c(-2, -20),
      stat = c("mean", "min"),
      func = c("lin", "quad")
    ),
    tmax_mod = list(
      xvar = list(tmax = day_sub$tmaxdegc),
      range = c(-2, -20),
      stat = c("mean", "max"),
      func = c("lin", "quad")
    ),
    photo_mod = list(
      xvar = list(photo = day_sub$daylh),
      range = c(-2, -20),
      stat = "mean",
      func = c("lin", "quad")
    ),
    precip_mod = list(
      xvar = list(precip = day_sub$prcpmmday),
      range = c(4, -20),
      stat = "mean",
      func = c("lin", "quad")
    ),
    solrad_mod = list(
      xvar = list(solrad = day_sub$sradWm2),
      range = c(-2, -20),
      stat = "mean",
      func = c("lin", "quad")
    ),
    vpd_mod = list(
      xvar = list(vpd = day_sub$vpd_tmax_kpa),
      range = c(-2, -20),
      stat = "mean",
      func = c("lin", "quad")
    )
  )
  
  # Climate variable mapping for output
  climate_mapping <- c(
    temp_mod = "meanT",
    GDD_mod = "GDD_modave",
    GDD_base_mod = "GDD_base",
    nightT_mod = "nightT",
    tmax_mod = "tmax",
    photo_mod = "photo",
    solrad_mod = "solrad",
    precip_mod = "prcpmmday",
    vpd_mod = "vpd_tmax_kpa"
  )
  
  # Function to run sliding window analysis for a single climate variable
  run_single_climate_analysis <- function(var_name, var_config, baseline_model) {
    
    # Run sliding window
    sliding_result <- slidingwin(
      xvar = var_config$xvar,
      cdate = day_sub$date,
      bdate = param_thetas$planting_date,
      baseline = baseline_model,
      cinterval = "week",
      range = var_config$range,
      type = "relative",
      stat = var_config$stat,
      func = var_config$func,
      spatial = list(param_thetas$trial_id_loc, day_sub$trial_id_loc)
    )
    
    # Run randomization
    random_result <- randwin(
      repeats = nrepeats,
      xvar = var_config$xvar,
      cdate = day_sub$date,
      bdate = param_thetas$planting_date,
      baseline = baseline_model,
      cinterval = "week",
      range = var_config$range,
      type = "relative",
      stat = var_config$stat,
      func = var_config$func,
      spatial = list(param_thetas$trial_id_loc, day_sub$trial_id_loc)
    )
    
    # Calculate p-values
    n_models <- length(sliding_result)-1 # how many flavors did you try (last is combos)
    
    pvals <- sapply(1:n_models, function(i) {
      pvalue(
        dataset = sliding_result[[i]]$Dataset,
        datasetrand = random_result[[i]],
        metric = pvalmetric,
        sample.size = nrow(param_thetas)
      )
    })
    
    # Create p-value data frame
    pval_df <- data.frame(
      stat = sliding_result$combos$stat,
      func = sliding_result$combos$func,
      pval = pvals
    )
    
    return(list(
      sliding = sliding_result,
      random = random_result,
      pvals = pval_df
    ))
  }
  
  # Initialize storage
  mod_sel <- list()
  i <- 1
  
  # Initial baseline model
  
  formula_str <- paste("theta ~ 1 + (1|county_state) + year_centered")
  
  baseline_formula <- eval(substitute(lmer(formula, data = data_env, REML = FALSE),
                                      list(formula = as.formula(formula_str),
                                           data_env = param_thetas)))
  
  # Main analysis loop
  repeat {
    cat('Running iteration', i, '\n')
    
    # Run analysis for all climate variables
    all_results <- map(names(climate_vars), function(var_name) {
      run_single_climate_analysis(var_name, climate_vars[[var_name]], baseline_formula)
    })
    names(all_results) <- names(climate_vars)
    cat("finished all results")
    
    # Combine all p-values
    all_pvals <- map_dfr(names(all_results), function(var_name) {
      all_results[[var_name]]$pvals |>
        mutate(
          mod = var_name,
          pval = as.character(pval)
        )
    }) |>
      mutate(pval = as.numeric(gsub('<', '', pval))) |>
      arrange(mod)
    
    # Combine all model combinations (vectorized operations)
    combos <- map_dfr(names(all_results), function(var_name) {
      all_results[[var_name]]$sliding$combos |>
        mutate(mod = var_name)
    }) |>
      full_join(all_pvals, by = c("mod", "stat", "func")) |>
      filter(pval <= 0.05) |>
      arrange(DeltaAICc)
    
    
    # Check for significant models
    if (nrow(combos) == 0) {
      cat('No significant models found. Stopping.\n')
      break
    }
    
    # Select best model
    best_mod_index <- 1
    best_var_name <- combos$mod[best_mod_index]
    best_stat <- combos$stat[best_mod_index]
    best_func <- combos$func[best_mod_index]
    cat("find")
    # Find the specific model variant
    sliding_result <- all_results[[best_var_name]]$sliding
    best_var_index <- which(
      sliding_result$combos$stat == as.character(best_stat) &
        sliding_result$combos$func == as.character(best_func)
    )
    cat("extract")
    # Extract climate signal
    best_climate <- sliding_result[[best_var_index]]$BestModelData$climate
    param_thetas[[paste0('signal', i)]] <- best_climate
    
    # Store model selection result
    mod_sel[[i]] <- combos[best_mod_index, ]
    
    # Check stopping criterion
    if (combos$DeltaAICc[best_mod_index] > delta_aic_threshold) {
      cat('Delta AIC threshold reached. Stopping.\n')
      break
    }
    
    # Update baseline model for next iteration
    tmp_mod <- bind_rows(mod_sel)
    n_included <- nrow(tmp_mod)
    polyform <- tmp_mod$func
    
    cat("quad")
    # Build formula string
    signal_terms <- paste0("signal", 1:n_included, collapse = " + ")
    if (any(polyform == 'quad')) {
      signal_terms_quad <- paste0("I(signal", 1:n_included, "^2)")
      signal_terms_quad <- signal_terms_quad[which(polyform == 'quad')]
      signal_terms_quad <- paste0(signal_terms_quad, collapse = " + ")
      signal_terms <- paste(signal_terms, "+", signal_terms_quad)
    }
    cat("forstr")
    formula_str <- paste("theta ~ 1 + (1|county_state) + year_centered +", signal_terms)
    
    baseline_formula <- eval(substitute(lmer(formula, data = data_env, REML = FALSE),
                                        list(formula = as.formula(formula_str),
                                             data_env = param_thetas)))
    
    saveRDS(list(
      mod_sel = bind_rows(mod_sel),
      param_thetas = param_thetas
    ), file = paste0(outfile_base, '_', i, '.rds'))
    
    cat('Updated formula:', formula_str, '\n')
    i <- i + 1
  }
  
  # Return results
  return(list(
    mod_sel = bind_rows(mod_sel),
    param_thetas = param_thetas
  ))
}

# Define function to fit climwin with fixed effects ------------------------------------------
run_climate_window_analysis_fixef <- function(day_sub, param_thetas, nrepeats = 5, 
                                              pvalmetric = "AIC", delta_aic_threshold = -5,
                                              outfile_base = 'tst_fixef_screen') {
  
  # Define climate variables with their configurations
  climate_vars <- list(
    temp_mod = list(
      xvar = list(meanT = day_sub$meanT),
      range = c(-2, -20),
      stat = "mean",
      func = c("lin", "quad")
    ),
    GDD_mod = list(
      xvar = list(GDD_modave = day_sub$GDD_modave),
      range = c(-2, -20),
      stat = "mean",
      func = c("lin", "quad")
    ),
    nightT_mod = list(
      xvar = list(nightT = day_sub$tmindegc),
      range = c(-2, -20),
      stat = c("mean", "min"),
      func = c("lin", "quad")
    ),
    tmax_mod = list(
      xvar = list(tmax = day_sub$tmaxdegc),
      range = c(-2, -20),
      stat = c("mean", "max"),
      func = c("lin", "quad")
    ),
    photo_mod = list(
      xvar = list(photo = day_sub$daylh),
      range = c(-2, -20),
      stat = "mean",
      func = c("lin", "quad")
    ),
    precip_mod = list(
      xvar = list(precip = day_sub$prcpmmday),
      range = c(4, -20),
      stat = "mean",
      func = c("lin", "quad")
    ),
    solrad_mod = list(
      xvar = list(solrad = day_sub$sradWm2),
      range = c(-2, -20),
      stat = "mean",
      func = c("lin", "quad")
    ),
    vpd_mod = list(
      xvar = list(vpd = day_sub$vpd_tmax_kpa),
      range = c(-2, -20),
      stat = "mean",
      func = c("lin", "quad")
    )
  )
  
  
  # Climate variable mapping for output
  climate_mapping <- c(
    temp_mod = "meanT",
    GDD_mod = "GDD_modave",
    nightT_mod = "nightT",
    tmax_mod = "tmax",
    photo_mod = "photo",
    solrad_mod = "solrad",
    precip_mod = "prcpmmday",
    vpd_mod = "vpd_tmax_kpa"
  )
  
  #run_single_climate_analysis(var_name, climate_vars[[var_name]], baseline_formula)
  # Function to run sliding window analysis for a single climate variable
  run_single_climate_analysis <- function(var_name, var_config, baseline_model) {
    
    # Run sliding window
    sliding_result <- slidingwin(
      xvar = var_config$xvar,
      cdate = day_sub$date,
      bdate = param_thetas$planting_date,
      baseline = baseline_model,
      cinterval = "week",
      range = var_config$range,
      type = "relative",
      stat = var_config$stat,
      func = var_config$func,
      spatial = list(param_thetas$trial_id_loc, day_sub$trial_id_loc)
    )
    
    # Run randomization
    random_result <- randwin(
      repeats = nrepeats,
      xvar = var_config$xvar,
      cdate = day_sub$date,
      bdate = param_thetas$planting_date,
      baseline = baseline_model,
      cinterval = "week",
      range = var_config$range,
      type = "relative",
      stat = var_config$stat,
      func = var_config$func,
      spatial = list(param_thetas$trial_id_loc, day_sub$trial_id_loc)
    )
    
    # Calculate p-values
    n_models <- length(sliding_result)-1 # how many flavors did you try (last is combos)
    pvals <- sapply(1:n_models, function(i) {
      pvalue(
        dataset = sliding_result[[i]]$Dataset,
        datasetrand = random_result[[i]],
        metric = pvalmetric,
        sample.size = nrow(param_thetas)
      )
    })
    
    # Create p-value data frame
    pval_df <- data.frame(
      stat = sliding_result$combos$stat,
      func = sliding_result$combos$func,
      pval = pvals
    )
    
    return(list(
      sliding = sliding_result,
      random = random_result,
      pvals = pval_df
    ))
  }
  
  # Initialize storage
  mod_sel <- list()
  i <- 1
  
  # Initial baseline model
  baseline_formula <- lm(theta ~ county_state + year_centered, data = param_thetas)
  
  # Main analysis loop
  repeat {
    cat('Running iteration', i, '\n')
    
    # Run analysis for all climate variables
    all_results <- map(names(climate_vars), function(var_name) {
      run_single_climate_analysis(var_name, climate_vars[[var_name]], baseline_formula)
    })
    names(all_results) <- names(climate_vars)
    cat("finished all results")
    
    
    if (length(all_results) == 0) {
      cat('All climate variable analyses failed. Stopping.\n')
      break
    }
    
    cat("Finished all results for iteration", i, "\n")
    
    # Combine all p-values
    all_pvals <- map_dfr(names(all_results), function(var_name) {
      tryCatch({
        all_results[[var_name]]$pvals |>
          mutate(
            mod = var_name,
            pval = as.character(pval)
          )
      }, error = function(e) {
        cat("Error processing p-values for", var_name, ":", e$message, "\n")
        return(data.frame())
      })
    })
    
    if (nrow(all_pvals) == 0) {
      cat('No p-values could be calculated. Stopping.\n')
      break
    }
    
    all_pvals <- all_pvals |>
      mutate(pval = as.numeric(gsub('<', '', pval))) |>
      arrange(mod)
    
    # Combine all model combinations
    combos <- map_dfr(names(all_results), function(var_name) {
      tryCatch({
        all_results[[var_name]]$sliding$combos |>
          mutate(mod = var_name)
      }, error = function(e) {
        cat("Error processing combos for", var_name, ":", e$message, "\n")
        return(data.frame())
      })
    })
    
    if (nrow(combos) == 0) {
      cat('No model combinations found. Stopping.\n')
      break
    }
    
    combos <- combos |>
      full_join(all_pvals, by = c("mod", "stat", "func")) |>
      filter(pval <= 0.05) |>
      arrange(DeltaAICc)
    
    # Check for significant models
    if (nrow(combos) == 0) {
      cat('No significant models found. Stopping.\n')
      break
    }
    
    # Select best model
    best_mod_index <- 1
    best_var_name <- combos$mod[best_mod_index]
    best_stat <- combos$stat[best_mod_index]
    best_func <- combos$func[best_mod_index]
    
    cat("Finding best model variant for", best_var_name, "\n")
    
    # Find the specific model variant
    sliding_result <- all_results[[best_var_name]]$sliding
    best_var_index <- which(
      sliding_result$combos$stat == as.character(best_stat) &
        sliding_result$combos$func == as.character(best_func)
    )
    
    if (length(best_var_index) == 0) {
      cat('Could not find matching model variant. Stopping.\n')
      break
    }
    
    cat("Extracting climate signal\n")
    # Extract climate signal
    best_climate <- sliding_result[[best_var_index]]$BestModelData$climate
    param_thetas[[paste0('signal', i)]] <- best_climate
    
    # Store model selection result
    mod_sel[[i]] <- combos[best_mod_index, ]
    
    # Check stopping criterion
    if (combos$DeltaAICc[best_mod_index] > delta_aic_threshold) {
      cat('Delta AIC threshold reached. Stopping.\n')
      break
    }
    
    # Update baseline model for next iteration
    tmp_mod <- bind_rows(mod_sel)
    n_included <- nrow(tmp_mod)
    polyform <- tmp_mod$func
    
    cat("Building new formula\n")
    # Build formula string
    signal_terms <- paste0("signal", 1:n_included, collapse = " + ")
    
    # Handle quadratic terms
    if (any(polyform == 'quad')) {
      quad_indices <- which(polyform == 'quad')
      signal_terms_quad <- paste0("I(signal", quad_indices, "^2)", collapse = " + ")
      signal_terms <- paste(signal_terms, "+", signal_terms_quad)
    }
    
    # Create formula string (note: consistent naming)
    formula_str <- paste("theta ~ county_state + year_centered +", signal_terms)
    Sys.sleep(3)
    baseline_formula <- eval(substitute(lm(formula, data = data_env), 
                                        list(formula = as.formula(formula_str), 
                                             data_env = param_thetas)))
    
    cat('Updated formula:', formula_str, '\n')
    
    # Save intermediate results
    tryCatch({
      saveRDS(list(
        mod_sel = bind_rows(mod_sel),
        param_thetas = param_thetas,
        formula_used = formula_str,
        iteration = i
      ), file = paste0(outfile_base, '_', i, '.rds'))
    }, error = function(e) {
      cat("Warning: Could not save intermediate results:", e$message, "\n")
    })
    
    i <- i + 1
    
    # Safety check to prevent infinite loops
    if (i > 20) {
      cat("Maximum iterations reached. Stopping.\n")
      break
    }
  }
  
  # Return results
  return(list(
    mod_sel = if(length(mod_sel) > 0) bind_rows(mod_sel) else data.frame(),
    param_thetas = param_thetas
  ))
}

# Prepare scaled climate data
cat("Preparing climate data...\n")

cat("Preparing climate data...\n")

# Store scaling factors for back-transformation
scaling_factors <- list()
climate_vars_to_scale <- c("prcpmmday", "sradWm2", "tmaxdegc", "tmindegc", 
                           "GDD_modave", "meanT", "daylh", "vpd_tmax_kpa")

for (var in climate_vars_to_scale) {
  if (var %in% names(daymet_mydata)) {
    original_values <- daymet_mydata[[var]]
    scaling_factors[[var]] <- list(
      center = attr(scale(original_values), "scaled:center"),
      scale = attr(scale(original_values), "scaled:scale"),
      mean = mean(original_values, na.rm = TRUE),
      sd = sd(original_values, na.rm = TRUE)
    )
  }
}

daymet_scaled <- daymet_mydata %>%
  mutate(across(all_of(climate_vars_to_scale), scale))

# Run the analysis based on selected type
cat("Running", args$analysis_type, "analysis...\n")

if (args$analysis_type == "ranef") {
  results <- run_climate_window_analysis_ranef(
    day_sub = daymet_scaled,
    param_thetas = mydata_yield_params_thetas,
    nrepeats = args$nrepeats,
    pvalmetric = args$pval_metric,
    delta_aic_threshold = args$delta_aic_threshold,
    outfile_base = args$outfile_base
  )
} else {
  results <- run_climate_window_analysis_fixef(
    day_sub = daymet_scaled,
    param_thetas = mydata_yield_params_thetas,
    nrepeats = args$nrepeats,
    pvalmetric = args$pval_metric,
    delta_aic_threshold = args$delta_aic_threshold,
    outfile_base = args$outfile_base
  )
}


# Add scaling factors and analysis metadata to results
results$scaling_factors <- scaling_factors
results$analysis_metadata <- list(
  tx_filter = args$tx_filter,
  analysis_type = args$analysis_type,
  pval_metric = args$pval_metric,
  nrepeats = args$nrepeats,
  delta_aic_threshold = args$delta_aic_threshold,
  daymet_path = args$daymet_path,
  stanfit_path = args$stanfit_path,
  run_date = Sys.time(),
  r_version = R.version.string,
  n_environments = nrow(mydata_yield_params_thetas)
)


# Save final results
final_output_file <- paste0(args$outfile_base, "_final.rds")
cat("Saving final results to:", final_output_file, "\n")
saveRDS(results, final_output_file)

cat("Analysis complete!\n")
cat("Final results saved to:", final_output_file, "\n")

# Print summary of results
if (nrow(results$mod_sel) > 0) {
  cat("\nSummary of selected models:\n")
  print(results$mod_sel)
} else {
  cat("\nNo models were selected.\n")
}