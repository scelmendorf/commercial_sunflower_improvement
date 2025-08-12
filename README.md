# commercial_sunflower_improvement

## Project Structure and Analysis Workflow

This repository contains scripts for analyzing commercial sunflower improvement data, including genetic and environmental trends over time. The analysis follows a structured workflow as outlined below:

```
commercial_sunflower_improvement/
├── code/
│   ├── 00_prep_data.R                          # Data preparation and filtering
│   │   └── Filters data to dryland oilseed hybrids
│   │   └── Combines trials at same location/year
│   │   └── Removes small trials and rare hybrids
│   │
│   ├── 01_all_trends_over_time.R               # Genetic vs environmental trend analysis
│   │   └── Dissects genetic vs environmental trends in all variables
│   │   └── Supports Piepho and McKay models for comparison
│   │   └── Runs statistical analysis across multiple states
│   │
│   ├── 02_mean_trends_over_time.R              # Mean trends calculation
│   │   └── Calculates trends over homogenized variables
│   │   └── Performs meta-analysis across states
│   │   └── Generates trend statistics and models
│   │
│   ├── 03_summarize_nass_progress_report_yield.R  # NASS yield data analysis
│   │   └── Downloads and analyzes NASS yield progress reports
│   │   └── Creates summary statistics from national data
│   │   └── Generates plots of yield trends
│   ├── 03.5_calculate_statewide_plantingdates_NASS.R  # Planting date analysis
│   │   └── Downloads NASS planting date data
│   │   └── Calculates state-wide planting patterns using interpolation
│   ├── 04_munge_and_plot_means_and_check_trends.R  # Trend visualization
│   │   └── Creates publication-ready trend plots
│   │   └── Combines means and check variety trends
│   │   └── Rebuilds and formats final visualizations
│   │
│   ├── 05_plot_trends_statemap_heatmaps.R      # Geographic visualization
│   │   └── Creates state-level heatmap visualizations
│   │   └── Maps trends across geographical regions
│   │   └── Generates choropleth maps for different traits
│   │
│   ├── 06_fw_bayesian_rscript_commercial_yield_all_yield_records.R  # Bayesian analysis
│   │   └── Runs Bayesian Finlay-Wilkinson analysis
│   │   └── Command-line executable for yield analysis
│   │   └── Generates environmental quality scores for downstream analyses
│   └── 10_make_site_quality_figs.R             # Site quality mapping
│       └── Creates site quality difference maps
│       └── Visualizes environmental quality across regions
│       └── Generates publication figures
│
├── data_derived/                               # Processed datasets
├── figure_inputs/                              # Intermediate analysis results
├── figures/                                    # Final publication figures
├── tables/                                     # Summary tables and results
└── api_key/                                    # NASS API authentication
    └── sce_nass_key.txt                        # Add yours here
```

### Analysis Workflow

1. **Data Preparation** (`00_prep_data.R`): Clean and filter raw sunflower trial data
2. **Trend Analysis** (`01_all_trends_over_time.R`, `02_mean_trends_over_time.R`): Analyze genetic and environmental trends
3. **External Data** (`03_summarize_nass_progress_report_yield.R`, `08_calculate_statewide_plantingdates_NASS.R`): Incorporate NASS national data
4. **Bayesian Modeling** (`06_fw_bayesian_rscript_commercial_yield_all_yield_records.R`): Advanced statistical modeling
5. **Visualization** (`04_munge_and_plot_means_and_check_trends.R`, `05_plot_trends_statemap_heatmaps.R`, `10_make_site_quality_figs.R`): Create publication-ready figures

### Requirements

- R with packages: tidyverse, lme4, metafor, rnassqs, terra, sf, ggplot2
- NASS API key (obtain from https://quickstats.nass.usda.gov/api/)
- Sunflower trial data (commercial varieties)