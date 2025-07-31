#!/usr/bin/env Rscript

# Code to run Bayesian FW analysis on sunflower data
# SCE 4 Dec 2024
# updated 2 April 2025
# updated 19 April 2025 to run with the full yield set ignoring flowering
# updated 24 July 2025 to clean up standard inputs and remove unused code
# this file is intended to be run from the command line

# note the basic code for the models are from
# TURBET DELOF, Michel; GOLDRINGER, Isabelle; DAWSON, Julie; RIVIERE,
# Pierre; VAN FRANCK, Gaëlle; DAVID, Olivier, 2024,
# "Supplementary Data and models : Bayesian joint-regression analysis
# of unbalanced series of on-farm trials", https://doi.org/10.57745/SUTZ9U,
# Recherche Data Gouv, V1,
# https://entrepot.recherche.data.gouv.fr/dataset.xhtml?persistentId=doi:10.57745/SUTZ9U


# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Check if help is requested
if ("--help" %in% args || length(args) == 0) {
  cat("Usage: Rscript script_name.R --input=<input_file> --output=<output_file> --niter=<niter> --nwarm=<nwarm> --nthin=<nthin> --nchain=<nchain\n")
  cat("Arguments:\n")
  cat("  --input    Path to input data file\n")
  cat("  --output   Path to save results\n")
  cat("  --niter   Number of iterations \n")
  cat("  --nwarm   Number of warmups \n")
  cat("  --nchain   Number of chains \n")
  cat("  --nthin   Thinning \n")
  quit(status = 0)
}


# input_file = ""data_derived/sunflower_data_yield_clim_subset.csv"
# output = ~/ORCC/big_bayesian_files/fitted_model_FWHs_gdd_commercial.rds

# to run
# Rscript code/06_fw_bayesian_rscript_commercial_yield_all_yield_records.R --input=data_derived/sunflower_data_yield_clim_subset.csv --output=~/ORCC/big_bayesian_files/fitted_model_FWHs_commercial_yield_all_yield.rds --niter=50 --nwarm=10 --nchain=2 --nthin=1

# Function to parse named arguments
parse_args <- function(args) {
  args_list <- list()
  for (arg in args) {
    if (grepl("^--", arg)) {
      arg_split <- strsplit(sub("^--", "", arg), "=")[[1]]
      if (length(arg_split) == 2) {
        args_list[[arg_split[1]]] <- arg_split[2]
      }
    }
  }
  return(args_list)
}

# Parse the arguments
parsed_args <- parse_args(args)

# Check if all required arguments are provided
required_args <- c("input", "output", "nthin", "niter", "nwarm", "nchain")
missing_args <- required_args[!required_args %in% names(parsed_args)]

if (length(missing_args) > 0) {
  cat("Error: Missing required arguments:", paste(missing_args, collapse = ", "), "\n")
  cat("Use --help for usage information\n")
  quit(status = 1)
}

# Extract argument values
input_file <- parsed_args$input
output_file <- parsed_args$output
numThin <- as.numeric(parsed_args$nthin)
numIter <- as.numeric(parsed_args$niter)
numWarm <- as.numeric(parsed_args$nwarm)
numChain <- as.numeric(parsed_args$nchain)


# Print the arguments (for demonstration)
cat("Input file:", input_file, "\n")
cat("Output file:", output_file, "\n")
cat("NumThin:", numThin, "\n")
cat("NumIter:", numIter, "\n")
cat("NumChain", numChain, "\n")

if (file.exists(output_file)){
  stop ("output file exists and will be overwritten")
}


# setup -------------------------------------------------------------------
# libraries
library(rstan)
library (tidyverse)


# models take a very long time to run so only rerun if necessary
rerun_models <-TRUE

# rstan options
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


# define models -----------------------------------------------------------


# Define hierarchical Finlay Wilkinson model with student T error structure
### Model FWHs####

model_FWHs <- '
 data {
  int n_obs; //number of observations
  real y[n_obs]; // data to be explained
  int germplasm[n_obs]; //for each data the corresponding number of germplam (from 1 to n_germ)
  int n_germ; // number of germplams
  int environment[n_obs]; // for each data the corresponding number of environment (from 1 to n_env)
  int n_env; //number of environments
  
  real prior_scale_mu;
  real prior_location_mu;
  real prior_scale_sigma_alpha;
  real prior_scale_sigma_lambda;
  real prior_scale_sigma_theta;
  real prior_scale_sigma;
 }
  
 parameters {
  real unscaled_mu;
  real<lower = 0> unscaled_sigma_alpha;
  real<lower = 0> unscaled_sigma_theta;
  real<lower = 0> unscaled_sigma_lambda;
  real<lower = 0> unscaled_sigma;
  vector[n_germ] unscaled_alpha;
  vector[n_germ] unscaled_lambda;
  vector[n_env] unscaled_theta;
  real<lower = 2> nu;
 }

 transformed parameters {
  real mu;
  real sigma_alpha;
  real sigma_lambda;
  real sigma_theta;
  real sigma;
  vector[n_germ] alpha;
  vector[n_germ] lambda;
  vector[n_env] theta;
  mu = prior_location_mu + prior_scale_mu * unscaled_mu;
  sigma_alpha = prior_scale_sigma_alpha * unscaled_sigma_alpha;
  sigma_theta = prior_scale_sigma_theta * unscaled_sigma_theta;
  sigma_lambda = prior_scale_sigma_lambda * unscaled_sigma_lambda;
  sigma = prior_scale_sigma * unscaled_sigma;
  alpha = mu + sigma_alpha * unscaled_alpha; //to optimise stan algorithms we use non-centred parametrisation cf stan manual
  theta = sigma_theta * unscaled_theta; //to optimise stan algorithms we use non-centred parametrisation cf stan manual
  lambda = sigma_lambda * unscaled_lambda; //to optimise stan algorithms we use non-centred parametrisation cf stan manual
  }
 
 model {
  nu ~ gamma(2,0.1);
  unscaled_mu ~ normal(0.0, 1.0);
  unscaled_alpha ~ normal(0.0, 1.0);
  unscaled_sigma_alpha ~ normal(0.0, 1.0);
  unscaled_lambda ~ normal(0.0, 1.0);
  unscaled_sigma_lambda ~ normal(0.0, 1.0);
  unscaled_theta ~ normal(0.0, 1.0);
  unscaled_sigma_theta ~ normal(0.0, 1.0);
  unscaled_sigma ~ normal(0.0, 1.0);
  for(i in 1:n_obs) {
   y[i] ~ student_t(nu,alpha[germplasm[i]]+theta[environment[i]]+
                 lambda[germplasm[i]]*theta[environment[i]],sigma);
  }
 }

 generated quantities {
  vector[n_obs] log_lik;
  
  for(i in 1:n_obs) {
  log_lik[i] = student_t_lpdf(y[i]|nu,alpha[germplasm[i]]+theta[environment[i]]+
                 lambda[germplasm[i]]*theta[environment[i]],sigma);
  }
 }
'


# compile models ----------------------------------------------------------

# if we wnat to use additional models need to add them to compiled list here
compil_FWHs <- stan_model(model_code = model_FWHs,
                          model_name = "Model FWHs")


# read and munge sunflower data -------------------------------------------
# rename to genotype to match g*e subscripting
mydata <- read.csv(input_file) %>%
  rename(genotype = Unif_Name) 


# run FW model -----------------------------------------------------------
if (rerun_models){
# Based on the Delof, took prior_sd to be ~2x the empirical
# sd of the data, and prior_mean to be a rounded number close to the mean of
# the data; experimentation shows not too sensitive to this


prior_sd <-800 # for yield


# 20066.2 sectons for 10000 interations in screen (~6hrs)
Sys.time()
fitted_model_FWHs_yield <- sampling(object = compil_FWHs,
                                     data = list(y = mydata$yield_lb_acre,
                                                 n_obs = nrow(mydata),
                                                 germplasm = as.numeric(as.factor(as.character(mydata$genotype))),
                                                 n_germ = length(unique(mydata$genotype)),
                                                 environment = as.numeric(as.factor(as.character(mydata$trial_id))),
                                                 n_env = length(unique(mydata$trial_id)),
                                                 prior_scale_mu = prior_sd,
                                                 prior_location_mu = 2000, # for yield
                                                 prior_scale_sigma_alpha = prior_sd,
                                                 prior_scale_sigma_lambda = 0.75,
                                                 prior_scale_sigma_theta = prior_sd,
                                                 prior_scale_sigma = prior_sd), 
                                     pars = c("log_lik","nu","sigma_alpha","sigma_theta","sigma_lambda","sigma",
                                              "alpha","lambda","theta"),
                                    chains = numChain,iter = numIter,warmup = numWarm,thin = numThin,
                                    cores =4)
saveRDS(fitted_model_FWHs_yield, output_file)
Sys.time()
}

