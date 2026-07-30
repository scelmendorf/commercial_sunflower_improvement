# Commercial Sunflower Improvement Analysis

**ALWAYS follow these instructions first** and only fallback to additional search and context gathering if the information here is incomplete or found to be in error.

This repository contains R scripts for analyzing commercial sunflower improvement data, including genetic and environmental trends over time. The analysis follows a numbered sequential workflow (00-10) with command-line tools for Bayesian modeling and climate analysis.

## Working Effectively

### Bootstrap and Setup Environment
- Install R and system dependencies:
  ```bash
  sudo apt-get update && sudo apt-get install -y r-base r-base-dev
  sudo apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev
  ```
- Install core R packages (takes 3-5 minutes):
  ```bash
  sudo apt-get install -y r-cran-tidyverse r-cran-lme4 r-cran-ggplot2 r-cran-metafor r-cran-broom r-cran-cowplot r-cran-rstan r-cran-argparse
  ```
- **NETWORK LIMITATION**: Direct CRAN package installation may fail due to network restrictions. Always use `apt-get` for R packages when possible.

### Validate Environment
- Run basic validation test:
  ```bash
  Rscript -e "library(tidyverse); library(lme4); cat('Environment ready\n')"
  ```
- Expected time: 2 seconds for package loading

### Required Directory Structure
- Create required directories:
  ```bash
  mkdir -p data_derived figure_inputs tables
  ```

## Sequential Analysis Workflow

### Step 1: Data Preparation
```bash
Rscript code/00_prep_data.R
```
- **Requires**: Raw sunflower trial data in `../sunflower_digitization_qc/data_derived/sunflower_data_simple.csv`
- **Output**: Filtered dataset for climate analysis
- **Time**: 30-60 seconds

### Step 2: Trend Analysis
```bash
Rscript code/01_all_trends_over_time.R
Rscript code/02_mean_trends_over_time.R
```
- **Time**: 2-5 minutes each. NEVER CANCEL - these scripts process multiple states
- **Output**: Genetic vs environmental trend analysis results

### Step 3: External Data Integration
```bash
Rscript code/03_summarize_nass_progress_report_yield.R
Rscript code/03.5_calculate_statewide_plantingdates_NASS.R
```
- **Requires**: NASS API key (obtain from https://quickstats.nass.usda.gov/api/)
- **Time**: 1-3 minutes depending on NASS API response

### Step 4: Visualization
```bash
Rscript code/04_munge_and_plot_means_and_check_trends.R
Rscript code/05_plot_trends_statemap_heatmaps.R
```
- **Output**: Publication-ready figures in `figures/` directory
- **Time**: 2-4 minutes

### Step 5: Bayesian Analysis (Command-line Tool)
```bash
Rscript code/06_fw_bayesian_rscript_commercial_yield_all_yield_records.R \
  --input=data_derived/sunflower_data_yield_clim_subset.csv \
  --output=~/output/fitted_model_FWHs_commercial_yield.rds \
  --niter=50 --nwarm=10 --nchain=2 --nthin=1
```
- **NEVER CANCEL**: Bayesian MCMC analysis takes 15-45 minutes depending on parameters
- **Timeout**: Set to 60+ minutes for production runs
- **Parameters**:
  - `--input`: Path to input data file
  - `--output`: Path to save fitted model
  - `--niter`: Number of iterations (production: 2000+)
  - `--nwarm`: Number of warmup iterations (production: 1000+)
  - `--nchain`: Number of chains (recommend: 4)
  - `--nthin`: Thinning interval

### Step 6: Climate Analysis (Command-line Tool)
```bash
Rscript code/07_site_envt_quality.R \
  --daymet_path=~/climate_files/daymet_timeseries_cleaned.csv \
  --stanfit_path=~/output/fitted_model_FWHs_commercial_yield.rds \
  --tx_filter=exclude \
  --analysis_type=fixef \
  --pval_metric=AIC \
  --nrepeats=100 \
  --delta_aic_threshold=-5.0 \
  --outfile_base=sunflower_climate_analysis
```
- **NEVER CANCEL**: Climate window analysis takes 20-60 minutes with default parameters
- **Timeout**: Set to 90+ minutes for production runs
- **Parameters**:
  - `--tx_filter`: include|exclude (whether to include Texas data)
  - `--analysis_type`: ranef|fixef (random or fixed effects analysis)
  - `--pval_metric`: AIC|C (p-value metric)
  - `--nrepeats`: Number of randomization repeats (default: 100)

### Step 7: Post-processing and Final Outputs
```bash
Rscript code/09_postprocess_climwin.R
Rscript code/10_make_site_quality_figs.R
```
- **Time**: 3-8 minutes
- **Output**: Final site quality maps and processed results

## Validation and Testing

### Manual Validation Requirements
After making changes to analysis scripts, ALWAYS run:

1. **Basic Environment Test**:
   ```bash
   Rscript -e "library(tidyverse); library(lme4); library(metafor); cat('All packages loaded\n')"
   ```

2. **Data Processing Test**:
   ```bash
   # Test data manipulation capabilities
   Rscript -e "
   library(tidyverse)
   test_data <- data.frame(year=2010:2020, yield=rnorm(11, 2000, 200))
   result <- test_data %>% filter(year > 2015) %>% summarise(mean_yield = mean(yield))
   cat('Data processing test passed\n')
   "
   ```

3. **Help Command Tests**:
   ```bash
   # Test command-line interfaces
   Rscript code/06_fw_bayesian_rscript_commercial_yield_all_yield_records.R --help
   Rscript code/07_site_envt_quality.R --help
   ```

### Google Earth Engine Integration
- JavaScript file: `code/gee_site_quality_with_test.js`
- **Cannot be run locally** - requires Google Earth Engine account and web interface
- Contains parameter sets for different model configurations
- Used for remote sensing data analysis

## Common Issues and Solutions

### Package Installation Failures
- **Problem**: "unable to install packages" or network timeouts
- **Solution**: Use `sudo apt-get install r-cran-PACKAGENAME` instead of `install.packages()`
- **Missing packages**: Some packages like `pacman`, `climwin` may need manual installation or alternative approaches

### Missing Data Files
- **Problem**: Scripts expect data in specific paths (e.g., `../sunflower_digitization_qc/`)
- **Solution**: Verify data file locations and update paths in scripts as needed
- **Create missing directories**: `mkdir -p data_derived figure_inputs tables`

### Long-Running Commands
- **CRITICAL**: NEVER cancel Bayesian or climate analysis scripts
- **Bayesian analysis**: 15-45 minutes normal, up to 2 hours for full production runs
- **Climate analysis**: 20-60 minutes normal, up to 90 minutes for complex analyses
- **Always set timeouts to 60+ minutes for these operations**

## Key File Locations

### Input Data
- Raw sunflower data: `../sunflower_digitization_qc/data_derived/sunflower_data_simple.csv`
- Climate data: `~/climate_files/daymet_timeseries_cleaned.csv`
- Processed data: `data_derived/`

### Outputs
- Figures: `figures/` (final publication figures)
- Intermediate results: `figure_inputs/`
- Tables: `tables/`
- Models: `~/output/` or specified output paths

### Configuration
- Google Earth Engine parameters: `code/gee_site_quality_with_test.js`
- Analysis parameters: Built into individual R scripts

## Expected Execution Times

- **Environment setup**: 3-5 minutes
- **Basic R scripts (00-05, 09-10)**: 30 seconds to 8 minutes each
- **Bayesian analysis (06)**: 15-45 minutes (NEVER CANCEL)
- **Climate analysis (07)**: 20-60 minutes (NEVER CANCEL)
- **Complete workflow**: 45-120 minutes for full analysis

## Development Best Practices

- Always test changes with small datasets first
- Run validation tests after modifying analysis scripts
- Check output directories for expected files after each step
- Monitor R memory usage for large datasets
- Use appropriate timeout values for long-running operations
- Follow the numbered sequence (00-10) unless specifically modifying workflow