##############################################.
#### Make a subset of data 
#### for F-W models
####
#### EIC
#### 3/21/2025
#### updated 7/24/2025 by SCE for all traits
##############################################.
pacman::p_unload(negate=TRUE)

# Summary of filters in this script:
# 1. Filter to dryland, oilseed hybrids
# 2. Combine trials that are at the same named_location and year
# 3. *Average duplicate entries for the same hybrid in the same trial

# Controls ---------------------------------------------------------------
remove_small_trials <- TRUE # will remove trials that have 3 or fewer hybrids
threshold_hybrids_per_trial <- 3 # trials with fewer than this number of hybrids will be removed

remove_rare_hybrids <- TRUE # will remove hybrids that have 3 or fewer entries
threshold_entries_per_hybrid <- 3 # hybrids with fewer than this number of entries will be removed


# Packages ---------------------------------------------------------------

library(tidyverse)

# Load data ------------------------------------------------------------

all_data <- read.csv("../sunflower_digitization_qc/data_derived/sunflower_data_simple.csv")

# Make filtered dataset for climate analysis - requires planting date ----------

filtered_data <- all_data %>%
  # we will combine across co-occurring trials here with close planting dates
  # for more power and average reps
  select(-Trial_ID) %>%
  filter(Irrigated == "dryland", Oil_Confection == "oil") %>% # dryland, oilseed hybrids only
  filter(!is.na(planting_date) & 
           !is.na(yield_lb_acre)) %>% # entries must include planting date, yield
  filter(Unif_Name != "undetermined_undetermined") %>% # remove undetermined hybrids
  mutate(county_state = paste(garden_county, State, sep = '_'), # add county_state column
         ungrouped_trial_id = paste(Location, Year, sep = '_'), .before = "Location") # unique trial identifier


## Co-occuring trials ------------------------------------------------
# trials that should be 
# lumped together, with a new trial_id that will group them together
lump_trials <- filtered_data %>%
  distinct(named_location, Year, ungrouped_trial_id) %>%
  group_by(named_location, Year) %>%
  summarise(n = n()) %>% # how many trials are at the same named_location & Year
  filter(n > 1) %>% # filter to locations & years with more than 1 trial
  left_join(filtered_data %>% 
              group_by(ungrouped_trial_id, named_location, Year, planting_doy) %>% 
              group_keys(), by = c("named_location", "Year")) %>% # add in planting date info
  group_by(named_location, Year) %>%
  filter(max(planting_doy) - min(planting_doy) < 3) %>% # keep trials that are within 2 days
  mutate(lump_trial_id = paste(named_location, Year, sep = '_')) %>% # create new trial_id for each group
  select(-n)


# join the lump_trial_id to the data and create a new trial_id column
filtered_data <- filtered_data %>%
  left_join(., lump_trials, by = c('named_location', 'Year', 'ungrouped_trial_id', 'planting_doy'), 
            relationship = "many-to-one") %>% 
  mutate(trial_id = if_else(is.na(lump_trial_id), ungrouped_trial_id, lump_trial_id),
         .before = "Location") %>% # create new trial_id, with the correct grouping
  select(-lump_trial_id, -ungrouped_trial_id) %>% # remove unneeded columns
  ungroup()

duplicate_entries <- filtered_data %>% 
  group_by(Unif_Name, trial_id) %>% 
  tally(name = 'n_entries') %>% 
  arrange(-n_entries) %>%
  filter(n_entries > 1)

need_summarizing <- duplicate_entries %>%
  left_join(., filtered_data) 

useful_cols <- c("trial_id", 'Unif_Name')
numeric_cols <- names(select_if(filtered_data, is.numeric))
non_numeric_cols <- setdiff(names(filtered_data), numeric_cols)
useful_cols_num <- c(useful_cols, numeric_cols)

# summarise numeric columns
summary_output_numeric <- need_summarizing[names(need_summarizing) %in% useful_cols_num] %>%
  group_by(Unif_Name, trial_id) %>%
  summarise(across(everything(), function(x) mean(x, na.rm = TRUE))) %>%
  ungroup()

# do distinct on non-numeric columns, to make sure the meta-data was the same
# should be same number of rows as summary_output_numeric
need_summarizing_nonnumeric <- need_summarizing[names(need_summarizing) %in% non_numeric_cols] %>%
  distinct(Unif_Name, trial_id, .keep_all = T)

summarized_trials <- full_join(summary_output_numeric, need_summarizing_nonnumeric)

# Remove the unsummarized rows from filtered_data
filtered_data <- anti_join(filtered_data, need_summarizing)

# Bind summarized data to full data
filtered_data <- bind_rows(filtered_data, summarized_trials)

# remove unneeded variables
rm(
  need_summarizing, need_summarizing_nonnumeric,
  useful_cols, useful_cols_num, summary_output_numeric,
  summarized_trials
)

## Small trials ---------------------------------------------------------
# remove trials with fewer than 3 hybrids
if(remove_small_trials == TRUE){
  small_trials <- filtered_data %>%
    group_by(trial_id, named_location, Year) %>%
    tally(name = 'n_hybrids') %>% 
    filter(n_hybrids <= threshold_hybrids_per_trial) %>%
    select(-n_hybrids)
  filtered_data <- filtered_data %>% anti_join(., small_trials, by = c('trial_id', 'named_location', 'Year'))
}


## Rare hybrids ---------------------------------------------------------
# remove hybrids that have fewer than 3 entries
if(remove_rare_hybrids == TRUE){
  rare_hybrids <- filtered_data %>% 
    group_by(Unif_Name) %>% 
    tally(name = 'n_entries') %>% 
    arrange(n_entries) %>% 
    filter(n_entries <= threshold_entries_per_hybrid)
  filtered_data <- filtered_data %>% anti_join(., rare_hybrids)
}



# Write out filtered dataset ------------------------------------------------
if (!dir.exists("data_derived")) {
  dir.create("data_derived")
}

write.csv(filtered_data %>%
            select(Unif_Name, trial_id,named_location, Location, State, county_state, lat, lon, Year, planting_doy, yield_lb_acre),
                   "data_derived/sunflower_data_yield_clim_subset.csv",
          row.names = FALSE)

