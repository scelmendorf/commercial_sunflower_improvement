# Dissect g vs e trends in all variables
# SCE last updated 25 July 2025

# reset to test = F if you want to just run a couple of states to debug
# run twice if want to compare Piepho and McKay models
pacman::p_unload(negate=TRUE)
# Packages ---------------------------------------------------------------
library(tidyverse)
library(broom.mixed)
#library(effects)
#require(webshot2)
#require(tinytable)
#library (cowplot)
library(lme4)
library(dplyr)
library(purrr)
#library (ggpmisc)
library (metafor)

test<-FALSE
use_piepho<-FALSE

threshold_entries_per_hybrid<-3
threshold_years_per_county<-3


# Load data ------------------------------------------------------------

# read data already filtered for just primarily oil trials
# etc
all_data <- read.csv("../sunflower_digitization_qc/data_derived/sunflower_data_simple.csv")


if (test) {
  all_data <-all_data %>%
    filter(State %in% c('SD', 'NE'))
}


# Make filtered dataset ---------------------------------------------------
# remove experiments like primarily NO trials, trials that had funky
# planting dates et

filtered_data <- all_data %>%
  filter(!grepl('irrigated|NO|late|recrop|additional|short', Location)) %>%
  filter(Irrigated == "dryland", Oil_Confection == "oil") %>% # dryland, oilseed hybrids only
  # for all trends, we can leave in the undetermineds
  filter(Unif_Name != "undetermined_undetermined") %>% # remove undetermined hybrids
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

# report stats (here w stats)

length(unique(filtered_data$named_location))
range(filtered_data$Year)
length(unique(filtered_data$Unif_Name))


# Determine check varieties -----------------------------------------------

# checks may be varieties that are actually labeled as checks or just included
# very very frqeuently.
# determine checks as very common varieties, sometimed labels as checks
# plus some occasional ones# 
# checks <- alldata %>%
#   group_by(Unif_Name) %>%
#   summarise(ct = dplyr::n()) %>%
#   arrange(desc(ct))


# how I found checks in the original data including the original typed name
# unique(all_data$Hybrid[grepl('check', all_data$Hybrid, ignore.case = T)])
# full_set <- read.csv("data_derived/sunflower_data.csv")
# unique(full_set$Unif_Name[grepl('check', full_set$Brand.Hybrid, ignore.case = T)])

# these seem to be the common checks so remove them and/or were labeled as checks
# in the actual data (some might already be removed as confectionary)
check_vars <- c("USDA_894", "Cargill_SF 270", "Cargill_SF 187", "Pioneer_63M91",
                "USDA_924","USDA_undetermined","undetermined_undetermined",
                "USDA_cmsHA89/RHA801","USDA_cmsHA300/RHA801", "USDA_cmsHA406/RHA373",
                "USDA_cmsHA89/HA300//RHA274", "USDA_cmsHA300/RHA274" , 
                "USDA_cms HA412/RHA377")

nocheck_data <- filtered_data %>%
  filter(!Unif_Name %in% check_vars) 

check_data <-filtered_data %>%
  filter(Unif_Name %in% c("USDA_894", "Cargill_SF 270", "Cargill_SF 187", "Pioneer_63M91",
                          "USDA_924"))

yr_introduced <- nocheck_data %>%
  group_by(Unif_Name) %>%
  summarise(min_yr = min(Year))



# run models to get year and genotype effects per state------------------------
states <-unique (filtered_data$State)

for (response_var in c("oil_pct","yield_lb_acre","flower_50pct_days_past_planting" ,
                       "oil_yield_lb_acre","harvest_moisture_pct",              
                       "height_cm", 
                       "value_generic",
                       "test_weight_lbs_bushel")){
  cat(response_var)
  cat('\n')
  
  # get BLUEs from models with two different
  # random effects structures as there's debate about which is preferable
  # in the literature.
  # Either way the genotype effect and year effect is fixed
  # bc modeling as random creates correlations per the Mackay citation
  # simulations
  cat ('running BLUEs')
  cat('\n')
  blue_results <- map_dfr(states, function(st) {
    message("Processing state: ", st)
    
    # Step 1: Subset to state
    state_data <- filtered_data %>% filter(State == st)
    
    # Step 2: Filter Unif_Names with >=3 years of data
    unif_names_valid <- state_data %>%
      filter(!is.na(.data[[response_var]])) %>%
      group_by(Unif_Name) %>%
      summarise(n_years = n_distinct(yearfact), .groups = "drop") %>%
      filter(n_years >= threshold_entries_per_hybrid) %>%
      pull(Unif_Name)
    
    if (length(unif_names_valid) < 10) return(NULL)
    
    # Step a: Filter counties with >=3 years of data
    county_valid <- state_data %>%
      group_by(garden_county) %>%
      summarise(n_years = n_distinct(yearfact), .groups = "drop") %>%
      filter(n_years >= threshold_years_per_county) %>%
      pull(garden_county)
    
    state_data <- state_data %>%
      filter(Unif_Name %in% unif_names_valid) %>%
      filter(garden_county %in% county_valid) %>%
      filter(!is.na(.data[[response_var]]))
    
    # Check for singletons
    singletons <- state_data %>%
      select(Unif_Name, Year) %>% distinct() %>%
      group_by(Unif_Name) %>%
      dplyr::tally() %>%
      filter(n == 1)
    
    state_data <- state_data %>%
      anti_join(., singletons, by = join_by(Unif_Name))
    
    state_data_nocheck <- nocheck_data %>%
      filter(State == st) %>%
      filter(garden_county %in% county_valid) %>%
      filter(!is.na(.data[[response_var]]))
    
    # SCALING: Calculate scaling parameters for the response variable
    response_mean <- mean(state_data[[response_var]], na.rm = TRUE)
    response_sd <- sd(state_data[[response_var]], na.rm = TRUE)
    
    # Scale the response variable
    state_data[[paste0(response_var, "_scaled")]] <- scale(state_data[[response_var]])[,1]
    
    # Also scale for nocheck data using same parameters
    state_data_nocheck[[paste0(response_var, "_scaled")]] <- (state_data_nocheck[[response_var]] - response_mean) / response_sd
    
    #sce just added this
    if (length(unique(state_data$yearfact))<10){
      return(NULL)
    }
    
    fixed <- NULL
    fixed_piepho_1 <- NULL
    fixed_piepho_2 <- NULL
    
    # Step 3: Fit model (mackay) - using scaled response variable
    if (!use_piepho) {
      model <- tryCatch({
        formula <- reformulate(
          c("1", "Unif_Name", "yearfact", 
            "(1 | yearfact:Unif_Name)", 
            "(1 | yearfact:garden_county)"),
          response = paste0(response_var, "_scaled")
        )
        
        warnings_caught <- character()
        
        model_result <- withCallingHandlers(
          lmer(formula,
               data = state_data,
               control = lmerControl(optimizer = "bobyqa")),
          warning = function(w) {
            msg <- conditionMessage(w)
            warnings_caught <<- c(warnings_caught, msg)
            invokeRestart("muffleWarning")
          }
        )
        
        convergence_warnings <- grep("failed to converge|Hessian|eigenvalue|convergence|rank deficient", 
                                     warnings_caught, ignore.case = TRUE, value = TRUE)
        
        if (length(convergence_warnings) > 0) {
          message("Model discarded for state ", st, " due to convergence issues:")
          message(paste("-", convergence_warnings, collapse = "\n"))
          return(NULL)
        }
        
        if (length(warnings_caught) > 0) {
          message("Warnings for state ", st, ":")
          message(paste("-", warnings_caught, collapse = "\n"))
        }
        
        model_result
        
      }, error = function(e) {
        message("Mackay Model failed for state ", st, ": ", e$message)
        return(NULL)
      })
      
      if (!is.null(model)) {
        fixed <- broom.mixed::tidy(model, effects = "fixed") %>%
          filter(grepl("^Unif_Name|^yearfact", term)) %>%
          mutate(State = st, model_struct = 'mackay') %>%
          # Back-transform estimate and std.error
          mutate(
            estimate = estimate * response_sd,
            std.error = std.error * response_sd
          )
      }
    }
    
    # Alternate model structure (piepho_1) - using scaled response variable
    if (use_piepho) {
      model_piepho_1 <- tryCatch({
        formula <- reformulate(
          c("1", "Unif_Name", "yearfact",
            "(1|garden_county)",
            "(1| Unif_Name:garden_county)",
            "(1 | yearfact:Unif_Name)",
            "(1 | yearfact:garden_county)"),
          response = paste0(response_var, "_scaled")
        )
        
        warnings_caught <- character()
        
        model_result <- withCallingHandlers(
          lmer(formula,
               data = state_data,
               control = lmerControl(optimizer = "bobyqa")),
          warning = function(w) {
            msg <- conditionMessage(w)
            warnings_caught <<- c(warnings_caught, msg)
            invokeRestart("muffleWarning")
          }
        )
        
        convergence_warnings <- grep("failed to converge|Hessian|eigenvalue|convergence|rank deficient", 
                                     warnings_caught, ignore.case = TRUE, value = TRUE)
        
        if (length(convergence_warnings) > 0) {
          message("model_piepho_1 discarded for state ", st, " due to convergence issues:")
          message(paste("-", convergence_warnings, collapse = "\n"))
          return(NULL)
        }
        
        if (length(warnings_caught) > 0) {
          message("Warnings for model_piepho_1 for state ", st, ":")
          message(paste("-", warnings_caught, collapse = "\n"))
        }
        
        model_result
        
      }, error = function(e) {
        message("model_piepho_1 failed for state ", st, ": ", e$message)
        return(NULL)
      })
      
      if (!is.null(model_piepho_1)) {
        fixed_piepho_1 <- broom.mixed::tidy(model_piepho_1, effects = "fixed") %>%
          filter(grepl("^Unif_Name|^yearfact", term)) %>%
          mutate(State = st, model_struct = 'piepho_1') %>%
          # Back-transform estimate and std.error
          mutate(
            estimate = estimate * response_sd,
            std.error = std.error * response_sd
          )
      }
    }
    
    # Simpler model on non-check data (piepho_2) - using scaled response variable
    if (nrow(state_data_nocheck) >= 10) {
      model_piepho_2 <- tryCatch({
        formula <- reformulate(
          c("1", "yearfact", 
            "(1|garden_county)",
            "(1 | yearfact:Unif_Name)", 
            "(1 | yearfact:garden_county)"),
          response = paste0(response_var, "_scaled")
        )
        
        lmer(formula,
             data = state_data_nocheck,
             control = lmerControl(optimizer = "bobyqa"))
        
      }, error = function(e) {
        message("model_piepho_2 failed for state ", st, ": ", e$message)
        return(NULL)
      })
      
      if (!is.null(model_piepho_2)) {
        fixed_piepho_2 <- broom.mixed::tidy(model_piepho_2, effects = "fixed") %>%
          filter(grepl("^Unif_Name|^yearfact", term)) %>%
          mutate(State = st, model_struct = 'piepho_2') %>%
          # Back-transform estimate and std.error using same scaling parameters
          mutate(
            estimate = estimate * response_sd,
            std.error = std.error * response_sd
          )
      }
    }
    
    # Return all fixed effect summaries
    bind_rows(fixed, fixed_piepho_1, fixed_piepho_2)
  })
  
  
  # 2nd level models on blues ####################################################
  
  #wrangle
  if(nrow(blue_results !=0)){
    blue_results <- blue_results %>%
      mutate(
        effect_type = case_when(
          grepl("^Unif_Name", term) ~ "Unif_Name",
          grepl("^yearfact", term) ~ "yearfact",
          TRUE ~ "other"
        ),
        level = sub("^Unif_Name", "", term),
        level = sub("^yearfact", "", level)
      ) 
    
    blue_results <- blue_results %>%
      left_join(., (yr_introduced %>%
                      rename(level = Unif_Name)))
    
    
    cat ('running metaregression agronomy')
    cat('\n')
    
    # run meta-analysis on trends in agronomy/climate over time
    # Step 1: Prepare data
    meta_input_agronomy <- blue_results %>%
      filter(effect_type == "yearfact" & model_struct == 'mackay') %>%
      mutate(
        Year = as.numeric(level),
      ) %>%
      filter(!is.na(std.error))  # Remove any incomplete cases
    
    if (use_piepho){
      meta_input_agronomy <- blue_results %>%
        filter(effect_type == "yearfact" & model_struct == 'piepho_1') %>%
        mutate(
          Year = as.numeric(level),
        ) %>%
        filter(!is.na(std.error))
    }
    
    if (length(unique(meta_input_agronomy$State))>1){
      # Fit meta-regression with Year * State interaction
      mod_state_year_interaction_agronomy <- rma(
        yi = estimate,
        sei = std.error,
        mods = ~ Year * State,
        method = "ML",
        data = meta_input_agronomy
      )
      
      mod_state_year_additive_agronomy <- rma(
        yi = estimate,
        sei = std.error,
        mods = ~ Year + State,
        method = "ML",
        data = meta_input_agronomy
      )
      
      
      # not sig different
      anova_agronomy <- anova(mod_state_year_interaction_agronomy, mod_state_year_additive_agronomy,
                              refit = TRUE)
      
    } else{
      anova_agronomy <-NULL
    }
    
    # Step 2: Run meta-regression per state
    # no change over time
    meta_reg_agronomy_results <- meta_input_agronomy %>%
      group_split(State) %>%
      map_dfr(function(df) {
        state_name <- unique(df$State)
        
        # Fit random-effects meta-regression with Year as moderator
        fit <- tryCatch({
          rma(yi = estimate, sei = std.error, mods = ~ Year, data = df, method = "REML")
        }, error = function(e) {
          message("Failed for state ", state_name, ": ", e$message)
          return(NULL)
        })
        
        if (is.null(fit)) return(NULL)
        
        # Extract relevant stats from summary
        sum_fit <- summary(fit)
        
        
        # Predict values at each year in the state's data
        preds <- predict(fit, newmods = df$Year)
        
        # Determine line type and color
        linetype <- ifelse(as.numeric(sum_fit$pval[2]) < 0.05, "solid", "dotted")
        
        color <- case_when(
          as.numeric(sum_fit$pval[2]) < 0.05 & as.numeric(sum_fit$beta[2]) > 0 ~ "green",
          as.numeric(sum_fit$pval[2])< 0.05 & as.numeric(sum_fit$beta[2]) < 0 ~ "red",
          TRUE ~ "black"
        )
        
        # Return combined data for plotting
        tibble(
          State = state_name,
          Year = df$Year,
          Estimate = df$estimate,
          SE = df$std.error,
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
    
    
    # One row per state for annotation
    state_labels <- meta_reg_agronomy_results %>%
      group_by(State) %>%
      summarise(
        Slope = unique(Slope),
        pval = unique(pval)
      ) %>%
      mutate(
        label = paste0("Slope = ", round(Slope, 2), 
                       "\nP = ", signif(pval, 3))
      )
    
    
    # Get label positions per state
    # only nec if we free_x the plots
    label_positions <- meta_reg_agronomy_results %>%
      #group_by(State) %>%
      summarise(
        x = min(Year, na.rm = TRUE) + 1,
        y = max(Estimate, na.rm = TRUE)
      ) #%>%
    #left_join(state_labels, by = "State")
    
    # Format nicely
    if(!is.null(anova_agronomy)){
      pval_text <- paste0("State * Year interaction P = ", signif(anova_agronomy$pval, 3))
    } else {
      pval_text <-NULL
    }
    
    # Define the state order from south to north
    south_to_north_order <- c(
      "TX", "KS", "CO",
      "NE", "SD", "ND"
    )
    
    # Keep only those states in your data
    states_in_data <- unique(meta_reg_agronomy_results$State)
    ordered_states <- south_to_north_order[south_to_north_order %in% states_in_data]
    
    agronomy_plot <- ggplot(meta_reg_agronomy_results, aes(x = Year)) +
      geom_point(aes(y = Estimate), alpha = 0.6) +
      geom_errorbar(aes(ymin = Estimate - SE, ymax = Estimate + SE), width = 0.2, alpha = 0.4) +
      geom_line(aes(y = Predicted, color = colors),
                #linetype = linetype),
                size = 1) +
      geom_ribbon(aes(ymin = CI_low, ymax = CI_up, fill = colors), alpha = 0.2) +
      scale_color_identity()+
      scale_fill_identity()+
      facet_wrap(~factor(State, c(rev(ordered_states))), ncol = 1) +
      theme_minimal(base_size = 14) +
      geom_label(data = state_labels,
                 aes(x = label_positions$x, y = label_positions$y, label = label),
                 hjust = 0, vjust = 1, size = 3.5,
                 fill = "white", label.size = NA, alpha = 0.7) +
      labs(
        title = paste("Meta-Regression of", response_var, "BLUEs \nAgronomy Over Time"),
        subtitle = paste(
          "Per-state predictions with 95% CI and slope",
          pval_text,
          sep = "\n"
        ),
        x = "Year",
        y = paste0(response_var, " BLUE")
      )
    
    
    # Create a self-contained list with everything needed to rebuild the agronomy plot
    agronomy_plot_data <- list(
      plot = agronomy_plot,
      data = meta_reg_agronomy_results,
      state_labels = state_labels,
      label_positions = label_positions,
      ordered_states = ordered_states,
      pval_text = pval_text,
      response_var = response_var
    )
    
    ###
    #repeat for genetic gain
    # run meta-analysis on trends in agronomy/climate over time
    
    cat ('running metaregression breeding')
    cat('\n')
    
    
    # Step 1: Prepare data
    meta_input_breeding <- blue_results %>%
      filter(effect_type == "Unif_Name"& model_struct == 'mackay') %>%
      filter(!is.na(std.error)) %>%  # Remove any incomplete cases 
      filter(!is.na(min_yr)) # these are checks
    
    if (use_piepho){
      meta_input_breeding  <- blue_results %>%
        filter(effect_type == "Unif_Name"& model_struct == 'piepho_1') %>%
        filter(!is.na(std.error)) %>%  # Remove any incomplete cases 
        filter(!is.na(min_yr)) # these are checks
    }
    
    if (length(unique(meta_input_breeding$State))>1){
      # Fit meta-regression with Year * State interaction
      mod_state_year_interaction_breeding <- rma(
        yi = estimate,
        sei = std.error,
        mods = ~ min_yr * State,
        method = "ML",
        data = meta_input_breeding 
      )
      
      mod_state_year_additive_breeding <- rma(
        yi = estimate,
        sei = std.error,
        mods = ~ min_yr + State,
        method = "ML",
        data = meta_input_breeding 
      )
      
      # not sig different
      anova_breeding <- anova(mod_state_year_interaction_breeding, mod_state_year_additive_breeding,
                              refit = TRUE)
    } else{
      anova_breeding<-NULL
    }
    
    
    
    # Step 2: Run meta-regression per state
    # no change over time
    meta_reg_breeding_results <- meta_input_breeding %>%
      group_split(State) %>%
      map_dfr(function(df) {
        state_name <- unique(df$State)
        
        # Fit random-effects meta-regression with Year as moderator
        fit <- tryCatch({
          rma(yi = estimate, sei = std.error, mods = ~ min_yr, data = df, method = "REML")
        }, error = function(e) {
          message("Failed for state ", state_name, ": ", e$message)
          return(NULL)
        })
        
        if (is.null(fit)) return(NULL)
        
        # Extract relevant stats from summary
        sum_fit <- summary(fit)
        
        # Determine line type and color
        linetype <- ifelse(as.numeric(sum_fit$pval[2]) < 0.05, "solid", "dotted")
        
        color <- case_when(
          as.numeric(sum_fit$pval[2]) < 0.05 & as.numeric(sum_fit$beta[2]) > 0 ~ "green",
          as.numeric(sum_fit$pval[2])< 0.05 & as.numeric(sum_fit$beta[2]) < 0 ~ "red",
          TRUE ~ "black"
        )
        
        
        # Predict values at each year in the state's data
        preds <- predict(fit, newmods = df$min_yr)
        
        # Return combined data for plotting
        tibble(
          State = state_name,
          min_yr = df$min_yr,
          Estimate = df$estimate,
          SE = df$std.error,
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
    
    
    # One row per state for annotation
    state_labels <- meta_reg_breeding_results %>%
      group_by(State) %>%
      summarise(
        Slope = unique(Slope),
        pval = unique(pval)
      ) %>%
      mutate(
        label = paste0("Slope = ", round(Slope, 2), 
                       "\nP = ", signif(pval, 3))
      )
    
    
    # Get label positions per state
    # only nec if we free_x the plots
    label_positions <- meta_reg_breeding_results %>%
      #group_by(State) %>%
      summarise(
        x = min(min_yr, na.rm = TRUE) + 1,
        y = max(Estimate, na.rm = TRUE)
      ) #%>%
    #left_join(state_labels, by = "State")
    
    # Format nicely
    #pval_text <- paste0("State * Year of Introduction interaction P = ", signif(anova_breeding$pval, 3))
    
    # Format nicely
    if(!is.null(anova_breeding)){
      pval_text <- paste0("State * Year of Introduction interaction P = ", signif(anova_breeding$pval, 3))
    } else {
      pval_text <-NULL
    }
    
    breeding_plot <- ggplot(meta_reg_breeding_results, aes(x = min_yr)) +
      geom_point(aes(y = Estimate), alpha = 0.6) +
      geom_errorbar(aes(ymin = Estimate - SE, ymax = Estimate + SE), width = 0.2, alpha = 0.4) +
      #geom_line(aes(y = Predicted), color = "blue", size = 1) +
      geom_line(aes(y = Predicted, color = colors),
                #linetype = linetype),
                size = 1) +
      #geom_ribbon(aes(ymin = CI_low, ymax = CI_up), alpha = 0.2, fill = "blue") +
      
      geom_ribbon(aes(ymin = CI_low, ymax = CI_up, fill = colors), alpha = 0.2) +
      #scale_linetype_identity()+
      scale_color_identity()+
      scale_fill_identity()+
      facet_wrap(~factor(State, c(rev(ordered_states))), ncol = 1) +
      theme_minimal(base_size = 14) +
      geom_label(data = state_labels,
                 aes(x = label_positions$x, y = label_positions$y, label = label),
                 hjust = 0, vjust = 1, size = 3.5,
                 fill = "white", label.size = NA, alpha = 0.7) +
      labs(
        title = paste("Meta-Regression of", response_var, "BLUEs \nBreeding Over Time"),
        subtitle = paste(
          "Per-state predictions with 95% CI and slope",
          pval_text,
          sep = "\n"
        ),
        x = "Year Variety Introduced",
        y = paste0(response_var, " BLUE")
      )
    
    
    lims <-max(abs(range (meta_reg_breeding_results$Predicted)), abs(range (meta_reg_agronomy_results$Predicted)))
    lims <-1.05*lims
    
    # Create a self-contained list with everything needed to rebuild the breeding plot
    breeding_plot_data <- list(
      plot = breeding_plot,
      data = meta_reg_breeding_results,
      state_labels = state_labels,
      label_positions = label_positions,
      ordered_states = ordered_states,
      pval_text = pval_text,
      response_var = response_var
    )
    
    
    if (!use_piepho){
      # ggsave(agronomy_plot, filename = file.path('temp_plots', 'crop_science',
      #                                            paste0(response_var, '_agronomy_gain.jpg')), width=5, height=10,
      #        scale = 1.4)
      #   # otherwise 2nd doesn't save
      # Sys.sleep(2)  
      # ggsave(breeding_plot, filename = file.path('temp_plots', 'crop_science',
      #                                            paste0(response_var, '_genetic_gain.jpg')), width=5, height=10,
      # scale = 1.4)
      
      
      write_rds(agronomy_plot_data, file = file.path('figure_inputs',
                                                     paste0(response_var, '_agronomy_gain.rds')))
      
      
      
      
      write_rds(breeding_plot_data, file = file.path('figure_inputs',
                                                     paste0(response_var, '_genetic_gain.rds')))
      
      write.csv(meta_reg_agronomy_results %>%
                  select(State, Slope, Slope_SE, pval) %>%
                  distinct()%>%
                  mutate(mod = 'agronomy') %>%
                  bind_rows(
                    meta_reg_breeding_results %>%
                      select(State, Slope, Slope_SE, pval) %>%
                      distinct()%>%
                      mutate(
                        mod = 'breeding') 
                  )%>%
                  mutate(response_var = paste(response_var)),
                file = file.path('figure_inputs',
                                 paste0(response_var, '_ag_breed_results_table.csv')),
                row.names = FALSE)
    }
    
    if (use_piepho){
      # ggsave(agronomy_plot, filename = file.path('figure_inputs',
      #                                            paste0(response_var, '_agronomy_gain_p.jpg')), width=5, height=10,
      #        scale = 1.4)
      # write_rds(agronomy_plot, file = file.path('temp_plots', 'crop_science',
      #                                            paste0(response_var, '_agronomy_gain_p.rds')))
      
      # Create a self-contained list with everything needed to rebuild the agronomy plot
      # agronomy_plot_data_p <- list(
      #   plot = agronomy_plot,
      #   data = meta_reg_agronomy_results,
      #   state_labels = state_labels,
      #   label_positions = label_positions,
      #   ordered_states = ordered_states,
      #   pval_text = pval_text,
      #   response_var = response_var
      # )
      
      write_rds(agronomy_plot_data, file = file.path('figure_inputs',
                                                     paste0(response_var, '_agronomy_gain_p.rds')))
      
      
      Sys.sleep(2) 
      # ggsave(breeding_plot, filename = file.path('temp_plots', 'crop_science',
      #                                            paste0(response_var, '_genetic_gain_p.jpg')), width=5, height=10,
      #        scale = 1.4)
      
      
      # Create a self-contained list with everything needed to rebuild the breeding plot
      # breeding_plot_data_p <- list(
      #   plot = breeding_plot,
      #   data = meta_reg_breeding_results,
      #   state_labels = state_labels,
      #   label_positions = label_positions,
      #   ordered_states = ordered_states,
      #   pval_text = pval_text,
      #   response_var = response_var
      # )
      
      
      write_rds(breeding_plot_data, file = file.path('figure_inputs',
                                                     paste0(response_var, '_genetic_gain_p.rds')))
      
      write.csv(meta_reg_agronomy_results %>%
                  select(State, Slope, Slope_SE, pval) %>%
                  distinct()%>%
                  mutate(mod = 'agronomy') %>%
                  bind_rows(
                    meta_reg_breeding_results %>%
                      select(State, Slope, Slope_SE, pval) %>%
                      distinct()%>%
                      mutate(
                        mod = 'breeding') 
                  )%>%
                  mutate(response_var = paste(response_var)),
                file = file.path('figure_inputs',
                                 paste0(response_var, '_ag_breed_results_table_p.csv')),
                row.names = FALSE)
    }
  }
}


# # Load the saved plot data
# plot_data <- readRDS(file.path('figure_inputs', 'yield_lb_acre_agronomy_gain.rds'))
# plot_data_p <- readRDS(file.path('figure_inputs', 'yield_lb_acre_agronomy_gain_p.rds')
#                        
#                        # Extract the components
#                        plot <- plot_data$plot
#                        data <- plot_data$data
#                        state_labels <- plot_data$state_labels
#                        label_positions <- plot_data$label_positions
#                        ordered_states <- plot_data$ordered_states
#                        pval_text <- plot_data$pval_text
#                        response_var <- plot_data$response_var
#                        
#                        # Now you can modify the plot as needed
#                        modified_plot <- plot_data$plot +
#                          theme_bw() +  # Change the theme
#                          labs(title = "Modified Agronomy Gain Plot") 
#                        
#                        
#                        plot_data_p <- readRDS("temp_plots/crop_science/yield_lb_acre_agronomy_gain_p.rds")
#                        
#                        p1 <- rebuild_gain_plot("temp_plots/crop_science/yield_lb_acre_agronomy_gain_p.rds", 
#                                                new_theme = NULL,
#                                                new_title = NULL,
#                                                new_subtitle = NULL,
#                                                new_x_label = NULL,
#                                                new_y_label = NULL,
#                                                new_facet_layout = NULL) 
#                        p2 <- rebuild_gain_plot("temp_plots/crop_science/yield_lb_acre_agronomy_gain.rds", 
#                                                new_theme = NULL,
#                                                new_title = NULL,
#                                                new_subtitle = NULL,
#                                                new_x_label = NULL,
#                                                new_y_label = NULL,
#                                                new_facet_layout = NULL) 
#                        ordered_states <-c("TX", "KS", "CO", "NE", "SD", "ND")
#                        combined_plot <- p1 +
#                          facet_wrap(~factor(State, c(rev(ordered_states))), ncol = 1) + p2
#                        patchwork
#                        
#                        
#                        
#                        modified_plot_p <-plot_data_p$plot+
#                          facet_wrap(~factor(State, c(rev(plot_data_p$ordered_states))), ncol = 1) +
#                          theme_minimal(base_size = 14) +
#                          geom_label(data = plot_data_p$state_labels,
#                                     aes(x = plot_data_p$label_positions$x, y = plot_data_p$label_positions$y,
#                                         label = label),
#                                     hjust = 0, vjust = 1, size = 3.5,
#                                     fill = "white", label.size = NA, alpha = 0.7) +
#                          labs(
#                            title = paste("Meta-Regression of", response_var, "BLUEs \nBreeding Over Time"),
#                            subtitle = paste(
#                              "Per-state predictions with 95% CI and slope",
#                              plot_data_p$pval_text,
#                              sep = "\n"
#                            ),
#                            x = "Year Variety Introduced",
#                            y = paste0(plot_data_p$response_var, " BLUE")
#                          )
#                        label_positions = label_positions_p
#                        
#                        # ordered_states <-south_to_north_order
#                        # #combine results 
#                        # f1 <-readRDS(file.path('temp_plots', 'crop_science',
#                        #                        paste0(response_var, '_breeding_gain_p.rds')))
#                        f2 <-readRDS(file.path('temp_plots', 'crop_science',
#                                               paste0(response_var, '_breeding_gain.rds')))
#                        f2a <-readRDS(file.path('temp_plots', 'crop_science',
#                                                paste0(response_var, '_breeding_gain_p.rds')))
#                        f3 <-readRDS(file.path('temp_plots', 'crop_science',
#                                               paste0(response_var, '_agronomy_gain.rds')))
#                        f3a <-readRDS(file.path('temp_plots', 'crop_science',
#                                                paste0(response_var, '_agronomy_gain_p.rds')))
#                        ylim1 <-ggplot_build(f2)$layout$panel_scales_y[[1]]$range$range
#                        ylim2 <-ggplot_build(f3)$layout$panel_scales_y[[1]]$range$range
#                        mylims <-c(min(c(ylim1, ylim2)), max(c(ylim1, ylim2)))
#                        
#                        # read all the csvs and combine into a single file
#                        
#                        spreadsheets <-list.files(
#                          'temp_plots/crop_science', pattern = 'csv$'
#                        )
#                        # 
#                        # library(patchwork)
#                        # combined_plot <- f2+ylim(mylims) | f2a+ylim(mylims) | f3+ylim(mylims) | f3a+ylim(mylims)
#                        # print(combined_plot)
