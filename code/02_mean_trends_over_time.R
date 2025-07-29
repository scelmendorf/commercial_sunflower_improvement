# calc trends over all homogenized variables
# SCE 6 March 2025
# updated 29 July 2026
# TODO

# Packages ---------------------------------------------------------------
#pacman::p_unload(all)
library(tidyverse)
library (ggplot2)
library(broom.mixed)
library(effects)
library(cowplot)
# run for all states
library(lme4)
library(dplyr)
library(purrr)
library(ggpmisc)
library(metafor)


# if making tinytables html
require(webshot2)
require(tinytable)

test <- FALSE


# Load data ------------------------------------------------------------

# read data already filtered for just primarily oil trials

all_data <- read.csv("../sunflower_digitization_qc/data_derived/sunflower_data_simple.csv")

# if you want to subset to just a few states to run
# can use this
if (test) {
  all_data <- all_data %>%
    filter(State %in% c("SD", "CO"))
}


# Make filtered dataset ---------------------------------------------------
# original data is already filtered to remove irrigated/primarily NO trials

filtered_data <- all_data %>%
  filter(!grepl('irrigated|NO|late|recrop|additional|short', Location)) %>%
  filter(Irrigated == "dryland", Oil_Confection == "oil") %>% # dryland, oilseed hybrids only
  # for all trends, we can leave in the undetermineds
  #filter(Unif_Name != "undetermined_undetermined") %>% # remove undetermined hybrids
  mutate(county_state = paste(garden_county, State, sep = '_')) %>%
  # very few trials from these states
  filter(!State %in% c('MN', 'WY')) %>%
  mutate(yearfact = as.factor(Year)) %>%
  mutate(
    flow_mat = mature_doy - flower_50_doy,
    plant_harv = harvest_doy - planting_doy,
    oil_yield_lb_acre = yield_lb_acre * oil_pct / 100,
    # calculate premium/discount structure
    value_generic = yield_lb_acre*(1+0.02 * (oil_pct -40))
  )


# determine checks as very common varieties, sometimed labels as checks
# plus some occasional ones# checks <- alldata %>%
#   group_by(Unif_Name) %>%
#   summarise(ct = dplyr::n()) %>%
#   arrange(desc(ct))


# how I found checks in the original data including the original typed name
# unique(all_data$Hybrid[grepl('check', all_data$Hybrid, ignore.case = T)])
# full_set <- read.csv("data_derived/sunflower_data.csv")
# unique(full_set$Unif_Name[grepl('check', full_set$Brand.Hybrid, ignore.case = T)])

# these seem to be the common checks so remove them and/or were labeled as checks
# in the actual data (some might already be removed as confectionary)
check_vars <- c(
  "USDA_894", "Cargill_SF 270", "Cargill_SF 187", "Pioneer_63M91",
  "USDA_924", "USDA_undetermined", "undetermined_undetermined",
  "USDA_cmsHA89/RHA801", "USDA_cmsHA300/RHA801", "USDA_cmsHA406/RHA373",
  "USDA_cmsHA89/HA300//RHA274", "USDA_cmsHA300/RHA274",
  "USDA_cms HA412/RHA377"
)

nocheck_data <- filtered_data %>%
  filter(!Unif_Name %in% check_vars)

check_data <- filtered_data %>%
  filter(Unif_Name %in% c(
    "USDA_894", "Cargill_SF 270", "Cargill_SF 187", "Pioneer_63M91",
    "USDA_924"
  ))

yr_introduced <- nocheck_data %>%
  group_by(Unif_Name) %>%
  summarise(min_yr = min(Year))

# Initialize list to store anova_trend results
anova_trend_results <- list()



# run models to get year and genotype effects per state------------------------
states <- unique(filtered_data$State)

for (response_var in c("planting_doy", "harvest_doy", "oil_pct","yield_lb_acre", "flower_50pct_days_past_planting" ,"oil_yield_lb_acre","harvest_moisture_pct",              
"height_cm",  "value_generic","test_weight_lbs_bushel")) {
  # response_var <- "planting_doy"
  cat(response_var)
  cat("\n")

  # still need to deal with
  # mature_days_past_planting_censored,
  trial_means <- nocheck_data %>%
    # unite(county, State, garden_county, remove = FALSE) %>%
    group_by(Year, named_location, State, garden_county) %>%
    # summarize at the rest of the variables
    summarise(
      across(
        all_of((response_var)),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          se = ~ sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
        )
      ),
      .groups = "drop"
    ) %>%
    mutate(yearfact = as.factor(Year))


  # iterate over variables of interest for temporal trends


  trial_means <- trial_means %>%
    filter(!is.na(garden_county)) %>%
    filter(!is.na(.data[[paste0(response_var, "_mean")]])) %>%
    mutate(
      study_id = row_number(),
      # rename so we don't have to have custom names below
      yi_var = .data[[paste0(response_var, "_mean")]],
      vi_var = (.data[[paste0(response_var, "_se")]])^2
    )
  if (response_var %in% c('planting_doy', 'harvest_doy')){
    m1 <-lme4::lmer(yi_var ~ Year * State + (1 | garden_county) + (1 | yearfact),
                    data = trial_means)
    
    m0 <-lme4::lmer(yi_var ~ Year + State + (1 | garden_county) + (1 | yearfact),
                    data = trial_means)
    
    anova_trend <- anova(m1, m0)
    anova_trend_results[[response_var]] <- data.frame(
      response_variable = response_var,
      model_comparison = "State*Year vs State+Year",
      AIC_full = anova_trend$AIC[2],
      AIC_reduced = anova_trend$AIC[1],
      LRT_statistic = anova_trend$Chisq[2],
      p_value = anova_trend$`Pr(>Chisq)`[2],
      significant = anova_trend$`Pr(>Chisq)` < 0.05
    )
  } else{
    # test for difference in trends among states
    m1 <- rma.mv(
      yi = yi_var,
      V = vi_var,
      mods = ~ State * Year,
      random = list(~ 1 | study_id, ~ 1 | garden_county, ~ 1 | yearfact),
      data = trial_means, method = "ML"
    )
    m0 <- rma.mv(
      yi = yi_var,
      V = vi_var,
      mods = ~ State + Year,
      random = list(~ 1 | study_id, ~ 1 | garden_county, ~ 1 | yearfact),
      data = trial_means, method = "ML"
    )
    anova_trend <- anova(m1, m0)
    
    anova_trend_results[[response_var]] <- data.frame(
      response_variable = response_var,
      model_comparison = "State*Year vs State+Year",
      AIC_full = anova_trend$fit.stats.f[1],
      AIC_reduced = anova_trend$fit.stats.r[1],
      LRT_statistic = anova_trend$LRT,
      p_value = anova_trend$pval,
      significant = anova_trend$pval < 0.05
    )
  }
  
  # Step 2: Run meta-regression per state
  # no change over time
  if (response_var %in% c('planting_doy', 'harvest_doy')){
    meta_reg <- trial_means %>%
      group_split(State) %>%
      map_dfr(function(df) {
        state_name <- unique(df$State)
        
        # Fit random-effects meta-regression with Year as moderator
        fit <- tryCatch(
          {
            lmerTest::lmer(paste0("yi_var", "", "~ Year + (1 | garden_county) + (1|yearfact)"),
                           data = df,
                           control = lmerControl(
                             optimizer = "bobyqa",
                             optCtrl = list(maxfun = 1000000)
                           )
            )
          },
          error = function(e) {
            message("Failed for state ", state_name, ": ", e$message)
            return(NULL)
          }
        )
        
        if (is.null(fit)) {
          return(NULL)
        }
        
        # Extract relevant stats from summary
        sum_fit <- summary(fit)$coefficients
        
        library(merTools)
        
        preds <- predictInterval(
          merMod = fit,
          newdata =
            data.frame(
              Year = df$Year
            ),
          # re.form=NA,
          level = 0.95,
          n.sims = 1000, # number of bootstraps (increase for stability)
          stat = "mean", # can also use "median"
          type = "linear.prediction",
          include.resid.var = FALSE,
          which = "fixed", # if you want fixed-effects only
          returnSims = FALSE
        )
        
        # Determine line type and color
        linetype <- ifelse(anova(fit)[1, 6] < 0.05, "solid", "dotted")
        
        color <- case_when(
          (as.numeric(anova(fit)[1, 6]) < 0.05 & sum_fit["Year", "Estimate"]) > 0 ~ "green",
          (as.numeric(anova(fit)[1, 6]) < 0.05 & sum_fit["Year", "Estimate"]) < 0 ~ "red",
          TRUE ~ "black"
        )
        
        # Return combined data for plotting
        tibble(
          State = state_name,
          Year = df$Year,
          Estimate = df[[response_var]],
          Predicted = preds$fit,
          CI_low = preds$upr,
          CI_up = preds$lwr,
          Slope = sum_fit["Year", "Estimate"],
          pval = anova(fit)[1, 6],
          Slope_SE = sum_fit["Year", "Std. Error"],
          linetype = ifelse(pval < 0.05, "solid", "dotted"),
          colors = color
        )
      })
  } else { 
    meta_reg <- trial_means %>%
      group_split(State) %>%
      map_dfr(function(df) {
        state_name <- unique(df$State)
        
        # Fit random-effects meta-regression with Year as moderator
        fit <- tryCatch(
          {
            rma.mv(
              yi = yi_var, V = vi_var, mods = ~Year,
              data = df, random = list(~ 1 | study_id, ~ 1 | garden_county, ~ 1 | yearfact),
              method = "REML"
            )
          },
          error = function(e) {
            message("Failed for state ", state_name, ": ", e$message)
            return(NULL)
          }
        )
        
        if (is.null(fit)) {
          return(NULL)
        }
        
        # Extract relevant stats from summary
        sum_fit <- summary(fit)
        
        
        # Predict values at each year in the state's data
        preds <- predict(fit, newmods = df$Year)
        
        # Determine line type and color
        linetype <- ifelse(as.numeric(sum_fit$pval[2]) < 0.05, "solid", "dotted")
        
        color <- case_when(
          as.numeric(sum_fit$pval[2]) < 0.05 & as.numeric(sum_fit$beta[2]) > 0 ~ "green",
          as.numeric(sum_fit$pval[2]) < 0.05 & as.numeric(sum_fit$beta[2]) < 0 ~ "red",
          TRUE ~ "black"
        )
        
        # Return combined data for plotting
        tibble(
          State = state_name,
          Year = df$Year,
          Estimate = df$yi_var,
          SE = sqrt(df$vi_var),
          Predicted = preds$pred,
          CI_low = preds$ci.lb,
          CI_up = preds$ci.ub,
          Slope = sum_fit$beta[2],
          pval = sum_fit$pval[2],
          Slope_SE = sum_fit$se[2],
          linetype = linetype,
          colors = color
        )
      })
  }

  # One row per state for annotation
  state_labels <- meta_reg %>%
    group_by(State) %>%
    summarise(
      Slope = unique(Slope),
      pval = unique(pval)
    ) %>%
    mutate(
      label = paste0(
        "Slope = ", round(Slope, 2),
        "\nP = ", round(pval, 3)
      )
    )


  # Get label positions per state
  # only nec if we free_x the plots
  if (response_var %in% c("planting_doy", "harvest_doy")) {
  label_positions <- meta_reg %>%
    # group_by(State) %>%
    summarise(
      x = min(Year, na.rm = TRUE) + 1,
      y = max(CI_up, na.rm = TRUE)
    )
  } else {
    label_positions <- meta_reg %>%
      # group_by(State) %>%
      summarise(
        x = min(Year, na.rm = TRUE) + 1,
        y = max(Estimate, na.rm = TRUE)
      )
  }

  # Format nicely
  if (response_var %in% c("planting_doy", "harvest_doy")) {
    pval_text <- paste0("State * Year interaction P = ", round(anova_trend$`Pr(>Chisq)`[2], 3))
  } else {
    # For other response variables, use the p-value from the anova table
    pval_text <- paste0("State * Year interaction P = ", round(anova_trend$pval, 3))
  }

  # Define the state order from south to north
  south_to_north_order <- c(
    "TX", "KS", "CO",
    "NE", "SD", "ND"
  )

  # Keep only those states in your data
  states_in_data <- unique(meta_reg$State)
  ordered_states <- south_to_north_order[south_to_north_order %in% states_in_data]
 
  if (response_var %in% c('planting_doy', 'harvest_doy')){
    no_check_subset <- nocheck_data %>%
      dplyr::select(Year, State, garden_county, !!response_var) %>%
      distinct()
    trend_plot <- ggplot(meta_reg, aes(x = Year)) +
      geom_point(data = no_check_subset, aes(y = .data[[response_var]]), alpha = 0.05) +
      #geom_point(aes(y = Estimate), alpha = 0.6) +
      #geom_errorbar(aes(ymin = Estimate - SE, ymax = Estimate + SE), width = 0.2, alpha = 0.4) +
      geom_line(aes(y = Predicted, color = colors), size = 1) +
      geom_ribbon(aes(ymin = CI_low, ymax = CI_up, fill = colors), alpha = 0.2) +
      scale_color_identity() +
      scale_fill_identity() +
      facet_wrap(~ factor(State, c(rev(ordered_states))), ncol = 1) +
      theme_minimal(base_size = 14) +
      geom_label(
        data = state_labels,
        aes(x = label_positions$x, y = label_positions$y, label = label),
        hjust = 0, vjust = 1, size = 3.5,
        fill = "white", label.size = NA, alpha = 0.7
      ) +
      labs(
        title = paste("Regression of", response_var, "Over Time"),
        subtitle = paste(
          "Per-state predictions with 95% CI and slope",
          pval_text,
          sep = "\n"
        ),
        x = "Year",
        y = paste0(response_var)
      )
    
    trend_data <-list(
      meta_reg = meta_reg,
      plot = trend_plot,
      data = no_check_subset,
      ordered_states = ordered_states,
      state_labels = state_labels,
      label_positions = label_positions,
      response_var = response_var,
      pval_text = pval_text)
    write_rds(trend_data, file = file.path(
      "figure_inputs",
      paste0(response_var, "_trend_plot.rds")
    ))
    
  } else{
    trend_plot <- ggplot(meta_reg, aes(x = Year)) +
      geom_point(data = nocheck_data, aes(y = .data[[response_var]]), alpha = 0.05) +
      geom_point(aes(y = Estimate), alpha = 0.6) +
      geom_errorbar(aes(ymin = Estimate - SE, ymax = Estimate + SE), width = 0.2, alpha = 0.4) +
      geom_line(aes(y = Predicted, color = colors), size = 1) +
      geom_ribbon(aes(ymin = CI_low, ymax = CI_up, fill = colors), alpha = 0.2) +
      scale_color_identity() +
      scale_fill_identity() +
      facet_wrap(~ factor(State, c(rev(ordered_states))), ncol = 1) +
      theme_minimal(base_size = 14) +
      geom_label(
        data = state_labels,
        aes(x = label_positions$x, y = label_positions$y, label = label),
        hjust = 0, vjust = 1, size = 3.5,
        fill = "white", label.size = NA, alpha = 0.7
      ) +
      labs(
        title = paste("Meta-Regression of", response_var, "Trial Means Over Time"),
        subtitle = paste(
          "Per-state predictions with 95% CI and slope",
          pval_text,
          sep = "\n"
        ),
        x = "Year",
        y = paste0(response_var, " trial mean")
      )
    
    trend_data <-list(
      meta_reg = meta_reg,
      plot = trend_plot,
      data = nocheck_data,
      ordered_states = ordered_states,
      state_labels = state_labels,
      label_positions = label_positions,
      response_var = response_var,
      pval_text = pval_text)
    write_rds(trend_data, file = file.path(
      "figure_inputs",
      paste0(response_var, "_trend_plot.rds")
    ))
  }

  ####

  # check_data
  if (response_var %in% c('planting_doy', 'harvest_doy')){
    next
  }else{
    trial_vals <- check_data %>%
      mutate(yearfact = as.factor(Year)) %>%
      filter(!is.na(garden_county)) %>%
      filter(!is.na(.data[[response_var]]))
    
    m1 <- lmerTest::lmer(paste0(response_var, "", "~ State*Year + Unif_Name + (1 | State/garden_county) + (1|yearfact)"),
                         data = trial_vals,
                         control = lmerControl(
                           optimizer = "bobyqa",
                           optCtrl = list(maxfun = 1000000)
                         )
    )
    
    anova_trend <- anova(m1)
    
    
    # Step 2: Run Regression per state
    # no change over time
    meta_reg_check <- trial_vals %>%
      group_split(State) %>%
      map_dfr(function(df) {
        state_name <- unique(df$State)
        
        # Fit random-effects meta-regression with Year as moderator
        fit <- tryCatch(
          {
            lmerTest::lmer(paste0(response_var, "", "~ Year + Unif_Name + (1 | garden_county) + (1|yearfact)"),
                           data = df,
                           control = lmerControl(
                             optimizer = "bobyqa",
                             optCtrl = list(maxfun = 1000000)
                           )
            )
          },
          error = function(e) {
            message("Failed for state ", state_name, ": ", e$message)
            return(NULL)
          }
        )
        
        if (is.null(fit)) {
          return(NULL)
        }
        
        # Extract relevant stats from summary
        sum_fit <- summary(fit)$coefficients
        
        library(merTools)
        
        preds <- predictInterval(
          merMod = fit,
          newdata =
            data.frame(
              Year = df$Year,
              # pick most common check at that state
              Unif_Name = names(sort(table(df$Unif_Name)))[length(sort(table(df$Unif_Name)))]
            ),
          # re.form=NA,
          level = 0.95,
          n.sims = 1000, # number of bootstraps (increase for stability)
          stat = "mean", # can also use "median"
          type = "linear.prediction",
          include.resid.var = FALSE,
          which = "fixed", # if you want fixed-effects only
          returnSims = FALSE
        )
        
        # Determine line type and color
        linetype <- ifelse(anova(fit)[1, 6] < 0.05, "solid", "dotted")
        
        color <- case_when(
          (as.numeric(anova(fit)[1, 6]) < 0.05 & sum_fit["Year", "Estimate"]) > 0 ~ "green",
          (as.numeric(anova(fit)[1, 6]) < 0.05 & sum_fit["Year", "Estimate"]) < 0 ~ "red",
          TRUE ~ "black"
        )
        
        # Return combined data for plotting
        tibble(
          State = state_name,
          Year = df$Year,
          Unif_Name = df$Unif_Name,
          Estimate = df[[response_var]],
          Predicted = preds$fit,
          CI_low = preds$upr,
          CI_up = preds$lwr,
          Slope = sum_fit["Year", "Estimate"],
          pval = anova(fit)[1, 6],
          Slope_SE = sum_fit["Year", "Std. Error"],
          linetype = ifelse(pval < 0.05, "solid", "dotted"),
          colors = color
        )
      })
    
    
    # One row per state for annotation
    state_labels_check <- meta_reg_check %>%
      group_by(State) %>%
      summarise(
        Slope = unique(Slope),
        pval = unique(pval)
      ) %>%
      mutate(
        label = paste0(
          "Slope = ", round(Slope, 2),
          "\nP = ", round(pval, 3)
        )
      )
    
    
    # Get label positions per state
    # only nec if we free_x the plots
    label_positions_check <- meta_reg_check %>%
      # group_by(State) %>%
      summarise(
        x = min(Year, na.rm = TRUE) + 1,
        y = max(Estimate, na.rm = TRUE)
      ) 
    # Format nicely
    pval_text <- paste0("State * Year interaction P = ", round(anova_trend[4, 6], 3))
    
    
    # Define the state order from south to north
    south_to_north_order <- c(
      "TX", "KS", "CO",
      "NE", "SD", "ND"
    )
    
    # Keep only those states in your data
    states_in_data_check <- unique(meta_reg_check$State)
    ordered_states_check <- south_to_north_order[south_to_north_order %in% states_in_data_check]
    
    trend_plot_check <- ggplot(meta_reg_check, aes(x = Year)) +
      geom_point(aes(y = Estimate, shape = Unif_Name), alpha = 0.6) +
      geom_line(aes(y = Predicted, color = colors), size = 1) +
      geom_ribbon(aes(ymin = CI_low, ymax = CI_up, fill = colors), alpha = 0.2) +
      scale_color_identity() +
      scale_fill_identity() +
      facet_wrap(~ factor(State, c(rev(ordered_states_check))), ncol = 1) +
      theme_minimal(base_size = 14) +
      geom_label(
        data = state_labels_check,
        aes(x = label_positions_check$x, y = label_positions_check$y, label = label),
        hjust = 0, vjust = 1, size = 3.5,
        fill = "white", label.size = NA, alpha = 0.7
      ) +
      labs(
        title = paste("Meta-Regression of", response_var, "Check Varieties Over Time"),
        subtitle = paste(
          "Per-state predictions with 95% CI and slope",
          pval_text,
          sep = "\n"
        ),
        x = "Year",
        y = paste0(response_var, " (checks only)")
      )
    
    ### end repeat for checks
    
    # ggsave(trend_plot_check,
    #   filename = file.path(
    #     "temp_plots", "crop_science",
    #     paste0(response_var, "_trend_plot_check.jpg")
    #   ), width = 5, height = 10,
    #   scale = 1.4
    # )
    Sys.sleep(2)
    
    trend_check_data <-list(
      plot = trend_plot_check,
      data = meta_reg_check,
      ordered_states_check = ordered_states_check,
      state_labels_check = state_labels_check,
      label_positions_check = label_positions_check,
      response_var = response_var,
      pval_text = pval_text)
    
    
    readr::write_rds(trend_check_data, file = file.path(
      "figure_inputs",
      paste0(response_var, "_trend_plot_check.rds")
    ))
    
    
    # ggsave(trend_plot,
    #   filename = file.path(
    #     "temp_plots", "crop_science",
    #     paste0(response_var, "_trend_plot.jpg")
    #   ), width = 5, height = 10,
    #   scale = 1.4
    # )
    
    
    write.csv(
      meta_reg_check %>%
        dplyr::select(State, Slope, Slope_SE, pval) %>%
        distinct() %>%
        mutate(mod = "check") %>%
        bind_rows(
          meta_reg %>%
            dplyr::select(State, Slope, Slope_SE, pval) %>%
            distinct() %>%
            mutate(
              mod = "trial_mean"
            )
        ) %>%
        mutate(response_var = paste(response_var)),
      file = file.path(
        "figure_inputs",
        paste0(response_var, "_check_trends.csv")
      ),
      row.names = FALSE
    )
  }
}

# Convert list to data frame and save

anova_trend_results[["planting_doy"]] <-anova_trend_results[["planting_doy"]][,2]
anova_trend_table <- bind_rows(anova_trend_results)

# Save the anova_trend results table
write.csv(anova_trend_table, 
          file = file.path("figure_inputs", "anova_mean_trends_results.csv"), 
          row.names = FALSE)

# Display the table
print(anova_trend_table)



# Get all CSV file names
all_tabs <- list.files(
  file.path("figure_inputs"),
  pattern = "\\.csv$"
)

# just get the anova trends
all_tabs <-all_tabs[!grepl("anova_mean_trends_results|breed_results", all_tabs)]

# Initialize an empty data frame
all_trends <- data.frame()

# Loop through files and combine into one data frame
for (fname in all_tabs) {
  temp <- read.csv(file.path("figure_inputs", fname)) %>%
    mutate(fname = fname)
  
  all_trends <- bind_rows(all_trends, temp)
}

# Clean trait column to create trait2
all_trends <- all_trends %>%
  mutate(trait = gsub('_check_trends\\.csv|_trends\\.csv', '', fname))


wide_data <- all_trends %>%
  mutate(pval = round(pval,3)) %>%
  pivot_wider(
    id_cols = c(State, trait),
    names_from = mod,
    values_from = c(Slope, Slope_SE, pval),
    names_glue = "{.value}_{mod}"
  ) 


tinytable::save_tt(
  tinytable::tt(wide_data, # %>%
                # mutate(p.value = round(p.value, 3)),
                digits = 1
  ), "tables/trends_table.html",
  overwrite = TRUE
)


library (jpeg)
library (grid)
library (gridExtra)
for (response_var in c(
  "oil_pct", "yield_lb_acre", "flower_50pct_days_past_planting",
  "oil_yield_lb_acre", "harvest_moisture_pct",
  "value_per_acre", "value_generic",
  "height_cm", "test_weight_lbs_bushel"
)){
  plots <- list.files(
    file.path("temp_plots", "crop_science"),
    pattern = paste0(response_var, ".*\\.jpg$")
  )
  
  
  f1<-readJPEG(file.path("temp_plots", "crop_science", plots[1]))
  f2<-readJPEG(file.path("temp_plots", "crop_science", plots[2]))
  
  # Convert to raster grobs
  g1 <- rasterGrob(f1, interpolate = TRUE)
  g2 <- rasterGrob(f2, interpolate = TRUE)
  
  # Arrange side by side
  jpeg(file.path("temp_plots", "crop_science", "combined_plots",
                 paste0(response_var,".jpg")), width = 800, height = 800, quality = 95)
  grid.arrange(g1, g2, ncol = 2)
  dev.off()
}
#   
#   library(patchwork)
#   ylim1 <-ggplot_build(f1)$layout$panel_scales_y[[1]]$range$range
#   ylim2 <-ggplot_build(f2)$layout$panel_scales_y[[1]]$range$range
#   mylims <-c(min(c(ylim1, ylim2)), max(c(ylim1, ylim2)))
#   
#   combined_plot <- f1+ylim(mylims) | f2+ylim(mylims) 
# }