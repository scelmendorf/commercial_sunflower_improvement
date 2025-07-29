# code to check out planting dates by date from NASS
# SCE 20 Nov 2024
# updated 19 March 2024 for some summary stats

# setup
library(rnassqs)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(broom)


# if using this set your own paths after requesting your
# own key and add it to the .gitignore
# get an nass key from here https://quickstats.nass.usda.gov/api/
api_key <- readLines("api_key/sce_nass_key.txt")
nassqs_auth(key = api_key)

# grab everything you can that might be useful
# turns out mostly yield is complete the others not so much
# left in code to grab more if we want to get it later

myresult <-list()
for (param in c("PRODUCTION", "YIELD", 'AREA HARVESTED', "AREA PLANTED")) {
  params <- list(
    commodity_desc = "SUNFLOWER", # Specifies sunflowers
    statisticcat_desc = param,
    agg_level_desc = "STATE"
    # year = "2024"           # can subset by year if file size too large
  )

# perform query

nassqs_record_count(params)

response <- nassqs_GET(params)

myresult[[param]] <- nassqs_parse(response, as = "data.frame")
}

all_res <-myresult %>%
  data.table::rbindlist() %>%
  filter(reference_period_desc == "YEAR")

# make nice plot with trends colored by state
# focusing on states with yield trials or adjacent

# Step 1: Filter data
filtered_data <- all_res %>%
  filter(reference_period_desc == "YEAR",
         class_desc == "OIL TYPE",
         unit_desc == "LB / ACRE",
         state_name != "OTHER STATES")

# Step 2: Compute slope, p-value, se and color for each state
state_trends <- filtered_data %>%
  group_by(state_alpha) %>%
  do({
    fit <- lm(Value ~ as.numeric(year), data = .)
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
plot_data <- filtered_data %>%
  left_join(state_trends, by = "state_alpha")

# Define the state order from south to north
south_to_north_order <- c(
  "TX", "KS", "CO",
  "NE", "SD", #"MN",
  "ND"
)

# Keep only those states in your data
states_in_data <- unique(filtered_data$state_alpha)
ordered_states <- south_to_north_order[south_to_north_order %in% states_in_data]

plot_data <- plot_data %>%
  filter(state_alpha %in% ordered_states)

# One row per state for annotation
state_labels <- state_trends %>%
  group_by(state_alpha) %>%
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
  summarise(
    x = min(as.numeric(year), na.rm = TRUE) + 1,
    y = max(Value, na.rm = TRUE)
  ) 

# figure out date range
# OK and CA are only since 2009
# ok only has 7 yrs, so skip
plot_data %>%
  group_by(state_alpha) %>%
  summarize(
    min_year = min(as.numeric(year), na.rm = TRUE),
    max_year = max(as.numeric(year), na.rm = TRUE),
    nyr = length(unique(year)))
  

# Step 4: Plot with per-panel color
yield_plot <- ggplot(plot_data, aes(x = as.numeric(year), y = Value,
                                      group = state_alpha)) +
  geom_point() +
  geom_smooth(method = "lm", aes(color = color), se = TRUE) +
    geom_label(data = state_labels,
               aes(x = label_positions$x, y = label_positions$y, label = label),
               hjust = 0, vjust = 1, size = 3.5,
               fill = "white", label.size = NA, alpha = 0.7) +
  facet_wrap(~factor(state_alpha, c(rev(ordered_states))), ncol = 1) +
  scale_color_identity() +
  ggtitle("Oilseed Sunflower Yield Trends by State") +
  labs( subtitle =
    "Source: USDA - NASS",
    x = "Year",
    y = "lb/acre")+
  theme_minimal(base_size = 14)

# ggsave("temp_plots/crop_science/nass_sunflower_yield_trends_by_state.png",
#        plot = yield_plot,
#        width = 10, height = 8, dpi = 300)

# write_rds(
#   yield_plot,
#   "figure_inputs/nass_sunflower_yield_trends_by_state.rds"
# )

#sce start HERE, need to make the plot_data right
mydat = state_trends %>%
  rename(
         State = state_alpha,
         Slope = slope,
         Slope_SE = se) %>%
  select(-color)


yield_data <-list(
  meta_reg = NULL,
  plot_data = plot_data,
  data = mydat,
  ordered_states = ordered_states,
  state_labels = state_labels,
  label_positions = label_positions)

write_rds(
  yield_data,
  file.path("figure_inputs", "nass_yield_trends.rds")
)


