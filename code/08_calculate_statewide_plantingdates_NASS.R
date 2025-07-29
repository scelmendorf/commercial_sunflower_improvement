# code to check out planting dates by date from NASS
# SCE 20 Nov 2024
# updated 19 March 2024 for some summary stats

# setup
library(rnassqs)
library(tidyverse)
library(zoo)


# if using this set your own paths after requesting your
# own key and add it to the .gitignore
# get an nass key from here https://quickstats.nass.usda.gov/api/
if (file.exists("api_key/sce_nass_key.txt")) {
  api_key <- readLines("api_key/sce_nass_key.txt")
} else {
  cat("must first generate a NASS key")
}


nassqs_auth(key = api_key)

# general useful stuff poking around NASS
# figure out param names
# nassqs_param_values("statisticcat_desc")
# check out params

# Define parameters for sunflower crop progress
params <- list(
  commodity_desc = "SUNFLOWER", # Specifies sunflowers
  statisticcat_desc = "PROGRESS",
  agg_level_desc = "STATE" #       # You can change to "NATIONAL" for national-level data
)

# Check that our record request is under the 50,000 limit
nassqs_record_count(params)

response <- nassqs_GET(params)

sunflower <- nassqs_parse(response, as = "data.frame")

# plot data on planting dates
ggplot(
  sunflower %>% filter(short_desc == "SUNFLOWER - PROGRESS, MEASURED IN PCT PLANTED"),
  aes(x = lubridate::yday(week_ending), y = Value, color = as.numeric(year))
) +
  geom_point() +
  geom_smooth() +
  geom_hline(aes(yintercept = 50)) +
  geom_vline(aes(xintercept = 150)) +
  facet_wrap(~location_desc, ncol = 2) +
  ggtitle("planting_date")

# to get day of year of 50% planting
# linearly interpolate between values per week and then make
# summary stats
# linear interpolate between dates

doy_interp <- sunflower %>%
  filter(short_desc == "SUNFLOWER - PROGRESS, MEASURED IN PCT PLANTED") %>%
  # Convert week_ending to Date format
  mutate(date = as.Date(week_ending)) %>%
  # Group by location for processing
  group_by(location_desc, year) %>%
  # Complete the date series for each location
  complete(date = seq(min(date), max(date), by = "day")) %>%
  # Ensure location_desc is maintained for all added rows
  fill(location_desc, .direction = "downup") %>%
  # Linearly interpolate the value column
  mutate(
    Value = na.approx(Value, na.rm = FALSE),
    # Maintain other key columns (adjust as needed for your actual data)
    year = year(date),
    # For any remaining columns you want to maintain, add them here
    # Either copying from the nearest non-NA value
    short_desc = first(short_desc)
  ) %>%
  # Keep only the columns you need
  select(location_desc, date, Value, year, short_desc)


doy_50_interp <- doy_interp %>%
  mutate(yday = lubridate::yday(date)) %>%
  filter(Value > 50) %>%
  group_by(location_desc, year) %>%
  summarize(min_yday = min(yday))

all_yrs_interp <- doy_50_interp %>%
  pivot_wider(., names_from = location_desc, values_from = min_yday) %>%
  na.omit(.) %>%
  # pivot longer
  pivot_longer(., cols = -year, names_to = "location_desc", values_to = "min_yday") %>%
  mutate(
    location_desc = factor(location_desc,
      levels = c(
        "MINNESOTA", "NORTH DAKOTA", "SOUTH DAKOTA",
        "KANSAS", "COLORADO", "TEXAS"
      ),
      labels = c("MN", "ND", "SD", "KS", "CO", "TX")
    ),
    date = as.Date(min_yday, origin = "2023-12-31")
  )

# calc median doys
all_yrs_interp %>%
  group_by(location_desc) %>%
  summarize(median_planting_doy = median(min_yday))

ggplot(all_yrs_interp, aes(x = location_desc, y = date, fill = location_desc)) +
  geom_boxplot() +
  ylab("Day of 50% planting") +
  xlab("State") +
  guides(fill = "none") +
  ggtitle("Statewide planting dates: 2014 - 2024") +
  scale_y_date(date_labels = "%b %d") +
  theme_bw() +
  theme(panel.grid = element_blank())
ggsave("temp_plots/opt_manuscript_figures/nass_planting_dates.png", width = 5, height = 4, dpi = 300)


# or plot without the interpolation
doy_50 <- sunflower %>%
  filter(short_desc == "SUNFLOWER - PROGRESS, MEASURED IN PCT PLANTED") %>%
  filter(Value > 50) %>%
  group_by(location_desc, year) %>%
  summarize(min_yday = min(yday(week_ending)))

all_yrs <- doy_50 %>%
  pivot_wider(., names_from = location_desc, values_from = min_yday) %>%
  na.omit(.) %>%
  # pivot longer
  pivot_longer(., cols = -year, names_to = "location_desc", values_to = "min_yday") %>%
  mutate(
    location_desc = factor(location_desc,
      levels = c(
        "MINNESOTA", "NORTH DAKOTA", "SOUTH DAKOTA",
        "KANSAS", "COLORADO", "TEXAS"
      ),
      labels = c("MN", "ND", "SD", "KS", "CO", "TX")
    ),
    date = as.Date(min_yday, origin = "2023-12-31")
  )

ggplot(all_yrs, aes(x = location_desc, y = date, fill = location_desc)) +
  geom_boxplot() +
  ylab("day of year of 50% planting") +
  xlab("State") +
  guides(fill = "none") +
  theme_classic() +
  ggtitle("Statewide planting dates - 2014 - 2024")

# median planting dates - used for GEE maps
all_yrs_interp %>%
  group_by(location_desc) %>%
  summarize(
    median_planting_doy = median(min_yday),
    median_date = median(date)
  )

# guesstimate NE data from commercial data since none available from NASS
NE_planting_doy <- read.csv(file.path("data_derived", "sunflower_data_yield_clim_subset.csv")) %>%
  filter(State == "NE") %>%
  select(trial_id, planting_doy, Year) %>%
  distinct() %>%
  summarize(median_planting_doy = median(planting_doy))
