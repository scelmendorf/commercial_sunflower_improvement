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
│   │
│   ├── 04_calculate_statewide_plantingdates_NASS.R  # Planting date analysis
│   │   └── Downloads NASS planting date data
│   │   └── Calculates state-wide planting patterns using interpolation
│   │   └── Analyzes temporal trends in planting dates
│   │
│   ├── 05_munge_and_plot_means_and_check_trends.R  # Trend visualization
│   │   └── Creates publication-ready trend plots
│   │   └── Combines means and check variety trends
│   │   └── Rebuilds and formats final visualizations
│   │
│   ├── 06_plot_trends_statemap_heatmaps.R      # Geographic visualization
│   │   └── Creates state-level heatmap visualizations
│   │   └── Maps trends across geographical regions
│   │   └── Generates choropleth maps for different traits
│   │
│   ├── 07_fw_bayesian_rscript_commercial_yield_all_yield_records.R  # Bayesian analysis
│   │   └── Runs Bayesian Finlay-Wilkinson analysis for per trial yield
│   │   └── Command-line executable for yield analysis
│   │   └── Generates environmental quality scores for each trial for downstream analyses
│   │
│   ├── 08_site_envt_quality.R                  # Site environmental quality analysis
│   │   └── Runs climate window analysis to determine environmental correlates of overall trial performance
│   │   └── Uses AIC values for to determine critical variables and climate window periods
│   │   └── Year fixed effect term included
│   │
│   ├── 08.5_site_envt_quality_noyear.R         # Site quality without year term
│   │   └── Runs climate window analysis to determine environmental correlates of overall trial performance
│   │   └── Uses AIC values for to determine critical variables and climate window periods
│   │   └── Year fixed effect omitted
│   │
│   ├── 09_postprocess_climwin_AIC_100.R        # Climate window analysis
│   │   └── Post-processes climwin model outputs
│   │   └── Builds plots and summary tables
│   │   └── Outputs summary parameters into .js format for GEE projections
│   │
│   ├── 10.1_project_site_quality_quarter_states.js  # GEE projection
│   │   └── Google Earth Engine script for spatial projection
│   │   └── Processs site quality in quarter-state chunks to solve GEE memory issues
│   │
│   ├── 10.2_project_site_quality_eighth_states.js   # GEE projection 
│   │   └── Google Earth Engine script for spatial projection
│   │   └── Processs site quality in eighth-state chunks to solve GEE memory issues
│   │   └── Only used when timeouts preclude using quarter states option
│   │
│   ├── 11_make_site_quality_figs.R             # Site quality visualization
│   │   └── Creates site quality difference maps
│   │   └── Visualizes environmental quality across regions
│   │   └── Generates publication figures for site quality
│   │
│   └── 12_rename_figs_ordered_for_publication.R  # Figure organization
│       └── Renames figures for publication submission
│       └── Orders figures according to manuscript order and journal sequential numbering requirements
│       └── Ensures consistent figure naming conventions
│
├── data_derived/                               # Processed datasets
├── figure_inputs/                              # Intermediate analysis results
├── figures/                                    # Final publication figures
├── tables/                                     # Summary tables and results
```

### Analysis Workflow

1. **Data Preparation** (`00_prep_data.R`): Clean and filter raw sunflower trial data
2. **Trend Analysis** (`01_all_trends_over_time.R`, `02_mean_trends_over_time.R`): Analyze genetic and environmental trends
3. **External Data** (`03_summarize_nass_progress_report_yield.R`, `04_calculate_statewide_plantingdates_NASS.R`): Incorporate NASS national data
4. **Visualization - Trends** (`05_munge_and_plot_means_and_check_trends.R`, `06_plot_trends_statemap_heatmaps.R`): Create trend visualizations
5. **Bayesian Modeling** (`07_fw_bayesian_rscript_commercial_yield_all_yield_records.R`): Bayesian estimates of site qualtiy
6. **Environmental Quality** (`08_site_envt_quality.R`, `08.5_site_envt_quality_noyear.R`): Analysis of environmental correlates of site quality
7. **Climate Window Analysis** (`09_postprocess_climwin_AIC_100.R`): Summarize critical climate variables and periods
8. **Spatial Projection** (`10.1_project_site_quality_quarter_states.js`, `10.2_project_site_quality_eighth_states.js`): Google Earth Engine mapping
9. **Visualization - Quality** (`11_make_site_quality_figs.R`): Create site quality maps
10. **Publication Prep** (`12_rename_figs_ordered_for_publication.R`): Organize figures for manuscript

### Requirements

- R with packages: tidyverse, lme4, metafor, rnassqs, terra, sf, ggplot2, climwin
- NASS API key (obtain from https://quickstats.nass.usda.gov/api/)
- Google Earth Engine account (for spatial projection scripts)
- Sunflower trial data (commercial varieties)