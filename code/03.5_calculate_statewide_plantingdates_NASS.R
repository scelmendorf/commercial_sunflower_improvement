# code to check out planting dates by date from NASS
# SCE 20 Nov 2024
# updated 19 March 2024 for some summary stats

# setup
pacman::p_unload(all)
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

# not really any data for bloom
ggplot(
  sunflower %>% filter(short_desc == "SUNFLOWER - PROGRESS, MEASURED IN PCT BLOOMING"),
  aes(x = lubridate::yday(week_ending), y = Value, color = as.numeric(year))
) +
  geom_point() +
  geom_smooth() +
  geom_hline(aes(yintercept = 50)) +
  geom_vline(aes(xintercept = 150)) +
  facet_wrap(~location_desc, ncol = 2) +
  ggtitle("pct_bloom")

ggplot(
  sunflower %>% filter(short_desc == "SUNFLOWER - PROGRESS, MEASURED IN PCT HARVESTED"),
  aes(x = lubridate::yday(week_ending), y = Value, color = as.numeric(year))
) +
  geom_point() +
  geom_smooth() +
  geom_hline(aes(yintercept = 50)) +
  geom_vline(aes(xintercept = 150)) +
  facet_wrap(~location_desc, ncol = 2) +
  ggtitle("pct_harvest")



# to get day of year of 50% planting
# linearly interpolate between values per week and then make
# summary stats
# linear interpolate between dates

doy_interp <- sunflower %>%
  filter(short_desc %in% c("SUNFLOWER - PROGRESS, MEASURED IN PCT PLANTED",
                            "SUNFLOWER - PROGRESS, MEASURED IN PCT HARVESTED")) %>%
  # Convert week_ending to Date format
  mutate(date = as.Date(week_ending)) %>%
  # Group by location for processing
  group_by(location_desc, year, short_desc) %>%
  # Complete the date series for each location
  complete(date = seq(min(date), max(date), by = "day")) %>%
  # Ensure location_desc is maintained for all added rows
  fill(location_desc, .direction = "downup") %>%
  # Linearly interpolate the value column
  mutate(
    Value = na.approx(Value, na.rm = FALSE),
    # Maintain other key columns (adjust as needed for your actual data)
    year = year(date)
  ) %>%
  # Keep only the columns you need
  select(location_desc, date, Value, year, short_desc)

# find interpolated date where >50% of event occurred
doy_50_interp <- doy_interp %>%
  mutate(yday = lubridate::yday(date)) %>%
  filter(Value > 50) %>%
  group_by(location_desc, year, short_desc) %>%
  summarize(min_yday = min(yday), .groups = "drop")

# remove a few obvious errors for harvest
doy_50_interp = doy_50_interp %>%
  filter(!(short_desc == "SUNFLOWER - PROGRESS, MEASURED IN PCT HARVESTED" & min_yday<100))



all_yrs_interp <- doy_50_interp %>%
  filter(short_desc == 'SUNFLOWER - PROGRESS, MEASURED IN PCT PLANTED') %>%
  pivot_wider(., names_from = location_desc, values_from = min_yday) %>%
  na.omit(.) %>%
  select(-short_desc) %>%
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

## make plots-------------------------------------------------------------------
# Step 2: Compute slope, p-value, se and color for each state

doy_50_interp<-doy_50_interp %>%
  left_join(., sunflower %>%
              select(location_desc, state_alpha) %>%
              distinct(), by = "location_desc")

state_trends <- doy_50_interp %>%
  group_by(state_alpha, short_desc) %>%
  do({
    fit <- lm(min_yday ~ as.numeric(year), data = .)
    tidy_fit <- broom::tidy(fit)
    slope <- tidy_fit$estimate[2]
    se <-tidy_fit$std.error[2]
    pval <- tidy_fit$p.value[2]
    color <- case_when(
      pval < 0.05 & slope > 0 ~ "green",
      pval < 0.05 & slope < 0 ~ "red",
      TRUE ~ "black"
    )
    tibble(slope = slope, pval = pval, se = se, color = color)
  })

# Step 3: Join colors back to main dataset
plot_data <- doy_50_interp %>%
  left_join(state_trends, by = c("state_alpha", "short_desc"))

# Define the state order from south to north
south_to_north_order <- c(
  "TX", "KS", "CO",
  "NE", "SD", #"MN",
  "ND"
)


# Keep only those states in your data
states_in_data <- unique(doy_50_interp$state_alpha)
ordered_states <- south_to_north_order[south_to_north_order %in% states_in_data]

plot_data <- plot_data %>%
  filter(state_alpha %in% ordered_states)

# One row per state for annotation
state_labels <- state_trends %>%
  group_by(state_alpha, short_desc) %>%
  summarise(
    Slope = unique(slope),
    pval = unique(pval)
  ) %>%
  mutate(
    label = paste0("Slope = ", round(Slope, 2), 
                   "\nP = ", round(pval, 3))
  ) %>%
  filter(state_alpha %in% ordered_states)


# Get label positions per state
# only nec if we free_x the plots
label_positions <- plot_data %>%
  group_by(short_desc) %>%
  summarise(
    x = min(as.numeric(year), na.rm = TRUE) + 1,
    y = max(min_yday, na.rm = TRUE)
  ) 

# figure out date range
plot_data %>%
  group_by(state_alpha, short_desc) %>%
  summarize(
    min_year = min(as.numeric(year), na.rm = TRUE),
    max_year = max(as.numeric(year), na.rm = TRUE),
    nyr = length(unique(year)))

# summarize difference in trends
trends_mod_plant <- lm(min_yday ~ state_alpha*year, data = plot_data %>%
                   mutate(year = as.numeric(year))%>%
                     filter(short_desc == "SUNFLOWER - PROGRESS, MEASURED IN PCT PLANTED"))


pval_text_plant <- paste0("State * Year interaction P = ", 
                    round(anova(trends_mod_plant)$`Pr(>F)`[3],3)) # p-value for interaction term
# Step 4: Plot with per-panel color
planting_plot <- ggplot(plot_data %>%
                          filter(short_desc== "SUNFLOWER - PROGRESS, MEASURED IN PCT PLANTED"), aes(x = as.numeric(year), y = min_yday,
                                    group = state_alpha)) +
  geom_point() +
  geom_smooth(method = "lm", aes(color = color), se = TRUE) +
  geom_label(data = state_labels %>%
               dplyr::filter(short_desc == "SUNFLOWER - PROGRESS, MEASURED IN PCT PLANTED"),
             aes(x = label_positions$x[label_positions$short_desc == "SUNFLOWER - PROGRESS, MEASURED IN PCT PLANTED"],
                 y = label_positions$y[label_positions$short_desc == "SUNFLOWER - PROGRESS, MEASURED IN PCT PLANTED"], label = label),
             hjust = 0, vjust = 1, size = 3.5,
             fill = "white", label.size = NA, alpha = 0.7) +
  facet_wrap(~factor(state_alpha, c(rev(ordered_states))), ncol = 1) +
  scale_color_identity() +
  ggtitle("Planting Date Trends by State \n Source: USDA - NASS") +
  labs(
    subtitle = paste(
      "Per-state predictions with 95% CI and slope",
      pval_text_plant,
      sep = "\n"
    ),
    x = "Year",
    y = "planting doy") +
  theme_minimal(base_size = 14)


# summarize difference in trends
trends_mod_harv <- lm(min_yday ~ state_alpha*year, data = plot_data %>%
                         mutate(year = as.numeric(year))%>%
                         filter(short_desc == "SUNFLOWER - PROGRESS, MEASURED IN PCT HARVESTED"))


pval_text_harv <- paste0("State * Year interaction P = ", 
                          round(anova(trends_mod_harv)$`Pr(>F)`[3],3)) # p-value for interaction term

harvest_plot <- ggplot(plot_data %>%
                          filter(short_desc== "SUNFLOWER - PROGRESS, MEASURED IN PCT HARVESTED"), aes(x = as.numeric(year), y = min_yday,
                                                                                                    group = state_alpha)) +
  geom_point() +
  geom_smooth(method = "lm", aes(color = color), se = TRUE) +
  geom_label(data = state_labels %>%
               dplyr::filter(short_desc == "SUNFLOWER - PROGRESS, MEASURED IN PCT HARVESTED"),
             aes(x = label_positions$x[label_positions$short_desc == "SUNFLOWER - PROGRESS, MEASURED IN PCT HARVESTED"],
                 y = label_positions$y[label_positions$short_desc == "SUNFLOWER - PROGRESS, MEASURED IN PCT HARVESTED"], label = label),
             hjust = 0, vjust = 1, size = 3.5,
             fill = "white", label.size = NA, alpha = 0.7) +
  facet_wrap(~factor(state_alpha, c(rev(ordered_states))), ncol = 1) +
  scale_color_identity() +
  ggtitle("Harvest Date Trends by State \n Source: USDA - NASS") +
  labs(
    subtitle = paste(
      "Per-state predictions with 95% CI and slope",
      pval_text_harv,
      sep = "\n"
    ),
    x = "Year",
    y = "harvest doy")+
  theme_minimal(base_size = 14)

planting_data <-list(
  plot_data = plot_data,
  #data = mydat,
  pval_text_plant = pval_text_plant,
  ordered_states = ordered_states,
  state_labels = state_labels,
  label_positions = label_positions)

harvest_data <-list(
  plot_data = plot_data,
  #data = mydat,
  pval_text_harv = pval_text_harv,
  ordered_states = ordered_states,
  state_labels = state_labels,
  label_positions = label_positions)

write_rds(
  planting_data,
  file.path("figure_inputs", "nass_planting_trends.rds")
)

write_rds(
  harvest_data,
  file.path("figure_inputs", "nass_harvest_trends.rds")
)

