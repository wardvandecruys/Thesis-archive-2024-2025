# Chapter 4.2
# creation of all graphs/heatmaps for the first sample policy (random draw with p_lapse just under threshold)
# creation of a function that finds the optimal hyper-box for a given policy (Chapter 4.2.2)
# expanding this framework to more examples (Chapter 4.3.3)

load("lapses_data")

# Define columns for imputation
imputation_cols <- c("ps_lag1", "ps_lag2", "avg_ps", "change_10", "change_21",
                     "mean_01", "mean_12", "vol_012", "mean_012")

# Create an empty list to store the median values for later use
imputation_medians <- list()

# Impute NAs on lapses_data and save the medians
message("\nImputing NAs on the full 'lapses_data' and saving medians...")
for (col in imputation_cols) {
  if (col %in% colnames(lapses_data)) {
    median_val <- median(lapses_data[[col]], na.rm = TRUE)
    lapses_data[[col]][is.na(lapses_data[[col]])] <- median_val
    imputation_medians[[col]] <- median_val # Save the calculated median
    message(paste0("  - Imputed '", col, "' with median: ", round(median_val, 4)))
  }
}

# Predict probabilities for the 'Yes' class
load("model_xgb")
predictors <- setdiff(names(lapses_data), c("policy_id", "data_year", "surrenders"))
lapses_data$p_lapse <- probabilities <- predict(model_xgb, newdata = lapses_data[, predictors], type = "prob")$Yes

save(lapses_data, file = "lapses_data")


# ------------------------------------------------------------------------------

# --- Define Dummy Data  ---
# original categorical variable definitions
cat_vars <- c("prem_freq", "living_place", "risk_class")
cat_vars_levels <- list(
  prem_freq = c("Semi-annual", 'Quarterly', 'Monthly', 'Annual', 'Other'),
  living_place = c("EastCoast", "Other", "WestCoast"),
  risk_class = c('SubStd-smoker', 'Prefered-smoker', 'Prefered-nonSmoker',
                 'SubStd-nonSmoker', 'Standard-nonSmoker', 'Standard-smoker'))

# Individual observation
row_example = 150943
x_interest = lapses_data[row_example,]

print(x_interest$p_lapse)
x_interest_info <- lapses_data[lapses_data$policy_id == x_interest$policy_id, ] # check: minstens derde rij/jaar vd polis om zever te vermijden


# Numerical feature multipliers/increments
annual_prem_multipliers <- seq(1.5, 0.5, by = -0.05)
ps_rate_increments <- seq(-0.1, 0.1, by = 0.01)

# XGBoost model and predictors
predictors <- setdiff(names(lapses_data), c("policy_id", "data_year", "surrenders"))
print(predictors)
print(model_xgb)

# Initial prediction for x_interest
str(x_interest)
# Ensure x_interest has all predictors for its own prediction
# Convert categorical columns in x_interest to factors for prediction
for (col in cat_vars) {
  if (col %in% colnames(x_interest) && is.character(x_interest[[col]])) {
    x_interest[[col]] <- factor(x_interest[[col]], levels = cat_vars_levels[[col]])
  }
}

save(x_interest, file = "x_interest")
#load(x_interest)


# --- Function for combination dataframe generation (input: dummy data) ---
generate_scenario_combinations <- function(
    x_interest,
    cat_vars_levels,
    annual_prem_multipliers,
    ps_rate_increments,
    model_xgb,
    predictors,
    selected_scenario_vars
) {
  
  current_df <- x_interest
  
  all_cat_vars <- names(cat_vars_levels)
  selected_cat_vars <- intersect(selected_scenario_vars, all_cat_vars)
  
  if (length(selected_cat_vars) > 0) {
    filtered_cat_levels <- cat_vars_levels[selected_cat_vars]
    cat_combinations_df <- expand.grid(filtered_cat_levels, stringsAsFactors = FALSE)
    x_interest_base_cols <- x_interest %>%
      dplyr::select(-dplyr::any_of(all_cat_vars))
    x_interest_duplicated_base <- x_interest_base_cols[rep(1, nrow(cat_combinations_df)), ]
    current_df <- bind_cols(x_interest_duplicated_base, cat_combinations_df)
  } else {
    current_df <- x_interest
  }
  
  original_ps_rate_x_interest <- x_interest$ps_rate
  original_duration_if_x_interest <- x_interest$duration_if
  original_avg_ps_x_interest <- x_interest$avg_ps
  
  if ("annual_prem" %in% selected_scenario_vars) {
    if (!"annual_prem" %in% colnames(current_df)) {
      current_df$annual_prem <- x_interest$annual_prem
    }
    current_df <- current_df %>%
      mutate(temp_row_id = row_number()) %>%
      crossing(annual_prem_multiplier = annual_prem_multipliers) %>%
      mutate(annual_prem = annual_prem * annual_prem_multiplier) %>%
      dplyr::select(-annual_prem_multiplier, -temp_row_id)
  }
  
  if ("ps_rate" %in% selected_scenario_vars) {
    cols_to_ensure_for_ps_rate <- c("ps_rate", "ps_lag1", "ps_lag2", "avg_ps", "duration_if")
    for (col in cols_to_ensure_for_ps_rate) {
      if (!col %in% colnames(current_df)) {
        current_df[[col]] <- x_interest[[col]]
      }
    }
    current_df <- current_df %>%
      mutate(temp_row_id = row_number()) %>%
      crossing(ps_rate_increment = ps_rate_increments) %>%
      mutate(ps_rate = ps_rate + ps_rate_increment) %>%
      filter(ps_rate >= 0) %>%
      dplyr::select(-ps_rate_increment, -temp_row_id)
    
    current_df <- current_df %>%
      mutate(
        change_10 = ps_lag1 - ps_rate,
        mean_01 = rowMeans(cbind(ps_rate, ps_lag1), na.rm = FALSE),
        vol_012 = apply(cbind(ps_rate, ps_lag1, ps_lag2), 1, sd, na.rm = FALSE),
        mean_012 = rowMeans(cbind(ps_rate, ps_lag1, ps_lag2), na.rm = FALSE),
        avg_ps = ((((original_avg_ps_x_interest + 1)^(original_duration_if_x_interest) / (1 + original_ps_rate_x_interest)) * (1 + ps_rate))^(1 / original_duration_if_x_interest)) - 1
      )
  } else {
    derived_cols <- c("change_10", "mean_01", "vol_012", "mean_012")
    for (col in derived_cols) {
      if (col %in% predictors && !col %in% colnames(current_df)) {
        current_df[[col]] <- x_interest[[col]]
      }
    }
  }
  
  cols_to_add_from_x_interest <- setdiff(colnames(x_interest), colnames(current_df))
  if (length(cols_to_add_from_x_interest) > 0) {
    for (col_name in cols_to_add_from_x_interest) {
      current_df[[col_name]] <- x_interest[[col_name]]
    }
  }
  
  for (col in all_cat_vars) {
    if (col %in% colnames(current_df) && is.character(current_df[[col]])) {
      current_df[[col]] <- factor(current_df[[col]], levels = cat_vars_levels[[col]])
    }
  }
  
  if (!all(predictors %in% colnames(current_df))) {
    stop("Not all predictors are present in the generated dataframe. Check 'predictors' and 'selected_scenario_vars'. Missing: ",
         paste(setdiff(predictors, colnames(current_df)), collapse = ", "))
  }
  
  if (is.null(model_xgb) || is.null(predictors) || length(predictors) == 0) {
    warning("model_xgb or predictors not provided/empty. Skipping prediction.")
    current_df$p_lapse <- NA # Set to NA if no prediction can be made
  } else {
    # Predictions
    set.seed(123) # Moved set.seed outside tryCatch for consistent dummy model behavior
    tryCatch({
      # Ensure newdata is always a data.frame, even if predictors has length 1
      current_df$p_lapse <- predict(model_xgb, newdata = current_df %>% dplyr::select(all_of(predictors)), type = "prob")$Yes
    }, error = function(e) {
      warning(paste("Prediction failed:", e$message))
      current_df$p_lapse <- NA # Assign NA on error
    })
  }
  
  if ("p_lapse" %in% colnames(current_df) && !all(is.na(current_df$p_lapse))) {
    x_interest_pred <- x_interest$p_lapse
    current_df$pred_diff <- current_df$p_lapse - x_interest_pred
  } else {
    current_df$pred_diff <- NA
  }
  
  if ("ps_rate" %in% colnames(current_df)) {
    current_df$ps_rate <- round(current_df$ps_rate, 4)
  }
  if ("annual_prem" %in% colnames(current_df)) {
    current_df$annual_prem <- round(current_df$annual_prem, 2)
  }
  
  for (var in selected_cat_vars) {
    if (var %in% colnames(current_df)) {
      current_df[[var]] <- factor(current_df[[var]], levels = cat_vars_levels[[var]])
    }
  }
  
  return(current_df)
}

# --- Example Usage --- 

# Example 1a: Vary only 'prem_freq'
combinations_with_pred_1 <- generate_scenario_combinations(
  x_interest = x_interest,
  cat_vars_levels = cat_vars_levels,
  annual_prem_multipliers = annual_prem_multipliers,
  ps_rate_increments = ps_rate_increments,
  model_xgb = model_xgb,
  predictors = predictors,
  selected_scenario_vars = c("prem_freq")
)
print("--- Combinations with Pred (Example 1: prem_freq) ---")
print(head(combinations_with_pred_1))
print(paste("Number of rows:", nrow(combinations_with_pred_1))) # logisch: 5 mogelijke levels
# ter controle: merk op dat eentje steeds x_interest is
print(unique(combinations_with_pred_1$prem_freq))
print(unique(combinations_with_pred_1$annual_prem)) # if not selected: orig value
print(unique(combinations_with_pred_1$ps_rate)) # Should be original ps_rate if not selected

# Example 1: Vary only 'prem_freq' and 'annual_prem'
combinations_with_pred_1 <- generate_scenario_combinations(
  x_interest = x_interest,
  cat_vars_levels = cat_vars_levels,
  annual_prem_multipliers = annual_prem_multipliers,
  ps_rate_increments = ps_rate_increments,
  model_xgb = model_xgb,
  predictors = predictors,
  selected_scenario_vars = c("prem_freq", "annual_prem")
)
print("--- Combinations with Pred (Example 1: prem_freq, annual_prem) ---")
print(head(combinations_with_pred_1))
print(paste("Number of rows:", nrow(combinations_with_pred_1))) # 5*21
print(unique(combinations_with_pred_1$prem_freq))
print(unique(combinations_with_pred_1$annual_prem))
print(unique(combinations_with_pred_1$ps_rate)) # Should be original ps_rate if not selected

# Example 1a: Vary only 'living_place'
combinations_with_pred_1 <- generate_scenario_combinations(
  x_interest = x_interest,
  cat_vars_levels = cat_vars_levels,
  annual_prem_multipliers = annual_prem_multipliers,
  ps_rate_increments = ps_rate_increments,
  model_xgb = model_xgb,
  predictors = predictors,
  selected_scenario_vars = c("living_place")
)
print("--- Combinations with Pred (Example 1: living_place) ---")
print(head(combinations_with_pred_1))
print(paste("Number of rows:", nrow(combinations_with_pred_1)))
print(unique(combinations_with_pred_1$prem_freq))
print(unique(combinations_with_pred_1$annual_prem))
print(unique(combinations_with_pred_1$ps_rate)) # Should be original ps_rate if not selected

# Example 1b: Vary only 'annual_prem'
combinations_with_pred_1 <- generate_scenario_combinations(
  x_interest = x_interest,
  cat_vars_levels = cat_vars_levels,
  annual_prem_multipliers = annual_prem_multipliers,
  ps_rate_increments = ps_rate_increments,
  model_xgb = model_xgb,
  predictors = predictors,
  selected_scenario_vars = c("annual_prem")
)
print("--- Combinations with Pred (Example 1: annual_prem) ---")
print(head(combinations_with_pred_1))
print(paste("Number of rows:", nrow(combinations_with_pred_1)))
print(unique(combinations_with_pred_1$prem_freq))
print(unique(combinations_with_pred_1$annual_prem))
print(unique(combinations_with_pred_1$ps_rate)) # Should be original ps_rate if not selected

# Example 2: Vary 'ps_rate' and 'risk_class'
combinations_with_pred_2 <- generate_scenario_combinations(
  x_interest = x_interest,
  cat_vars_levels = cat_vars_levels,
  annual_prem_multipliers = annual_prem_multipliers,
  ps_rate_increments = ps_rate_increments,
  model_xgb = model_xgb,
  predictors = predictors,
  selected_scenario_vars = c("ps_rate", "risk_class")
)
print("--- Combinations with Pred (Example 2: ps_rate, risk_class) ---")
print(head(combinations_with_pred_2))
print(paste("Number of rows:", nrow(combinations_with_pred_2)))
print(unique(combinations_with_pred_2$ps_rate))
print(unique(combinations_with_pred_2$risk_class))
print(unique(combinations_with_pred_2$annual_prem)) # Should be original annual_prem if not selected

# Example 3: Vary all scenario variables
combinations_with_pred_3 <- generate_scenario_combinations(
  x_interest = x_interest,
  cat_vars_levels = cat_vars_levels,
  annual_prem_multipliers = annual_prem_multipliers,
  ps_rate_increments = ps_rate_increments,
  model_xgb = model_xgb,
  predictors = predictors,
  selected_scenario_vars = c("prem_freq", "living_place", "annual_prem", "risk_class", "ps_rate")
)
print("--- Combinations with Pred (Example 3: All scenario vars) ---")
print(head(combinations_with_pred_3))
print(paste("Number of rows:", nrow(combinations_with_pred_3)))

save(combinations_with_pred_3, file = "combinations_with_pred_3")
#load("combinations_with_pred_3")

# Example 4: No scenario variables selected (should return x_interest with prediction) 
combinations_with_pred_4 <- generate_scenario_combinations(
  x_interest = x_interest,
  cat_vars_levels = cat_vars_levels,
  annual_prem_multipliers = annual_prem_multipliers,
  ps_rate_increments = ps_rate_increments,
  model_xgb = model_xgb,
  predictors = predictors,
  selected_scenario_vars = c()
)

print("--- Combinations with Pred (Example 4: No scenario vars) ---")
print(combinations_with_pred_4) # only x_interest!!
print(paste("Number of rows:", nrow(combinations_with_pred_4)))


# ------------------------------------------------------------------------------
# VISUALIZE CHANGES - heatmaps etc (lower dimensional hyperboxes construction)
plot_scenario_impact <- function(data, selected_scenario_vars, fill_var = "pred_diff", x_interest = NULL, cat_vars_levels = NULL) {
  
  # Basic validation
  if (is.null(data) || nrow(data) == 0) {
    stop("Input 'data' is empty or NULL.")
  }
  if (!fill_var %in% colnames(data)) {
    stop(paste0("'", fill_var, "' not found in the input data."))
  }
  if (!all(selected_scenario_vars %in% colnames(data))) {
    stop("Not all 'selected_scenario_vars' are present in the input data.")
  }
  
  num_vars <- length(selected_scenario_vars)
  plot_title_base <- paste("Impact on", fill_var)
  p <- NULL # Initialize plot object
  
  # Prepare x_interest for plotting if provided
  x_interest_point_data <- NULL
  if (!is.null(x_interest)) {
    # Ensure x_interest has the fill_var for plotting
    if (!fill_var %in% colnames(x_interest)) {
      # If fill_var is pred_diff, and x_interest doesn't have it, calculate it
      # This part remains as per your original logic: if x_interest is the baseline, pred_diff is 0.
      if (fill_var == "pred_diff" && "p_lapse" %in% colnames(x_interest)) {
        x_interest$pred_diff <- 0 # If it's a baseline, pred_diff should be 0
      } else {
        warning(paste0("x_interest does not contain '", fill_var, "'. Original point will not be plotted."))
        x_interest <- NULL # Disable plotting x_interest point
      }
    }
    if (!is.null(x_interest)) {
      x_interest_point_data <- x_interest # Use x_interest directly for point data
    }
  }
  
  
  if (num_vars == 0) {
    message("No scenario variables selected. Returning a single point plot (if data has one row).")
    if (nrow(data) == 1) {
      p <- ggplot(data, aes(x = 1, y = .data[[fill_var]])) +
        geom_point(size = 5, color = "#1D8DB0") +
        labs(title = paste(plot_title_base, "(Single Point)"),
             x = "", y = fill_var) +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5))
      
      # ADDITION FOR 0D PLOT
      if (fill_var == "p_lapse") {
        p <- p + geom_hline(yintercept = 0.5, color = "red", linetype = "dashed", linewidth = 1)
      }
    } else {
      message("Data has more than one row but no scenario variables selected. Cannot visualize meaningfully.")
      return(NULL)
    }
    
  } else if (num_vars == 1) {
    x_var <- selected_scenario_vars[1]
    plot_title <- paste(plot_title_base, "by", x_var)
    
    # Ensure x_var is factor if it's a categorical variable
    if (x_var %in% names(cat_vars_levels) && is.character(data[[x_var]])) {
      data[[x_var]] <- factor(data[[x_var]], levels = cat_vars_levels[[x_var]])
    } else if (is.numeric(data[[x_var]])) {
      # For numerical variables, ensure they are ordered for line plot
      data <- data %>% arrange(.data[[x_var]])
    } else { # For any other type, just factorize for geom_col fallback
      data[[x_var]] <- factor(data[[x_var]])
    }
    
    
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[fill_var]])) +
      labs(title = plot_title, x = x_var, y = fill_var) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    if (is.numeric(data[[x_var]])) { # This condition should check original type, not factored type
      # Re-check original type for geom_line decision
      if (is.numeric(data[[x_var]])) { # If it was numeric, it's already sorted
        p <- p + geom_line(color = "#1D8DB0", linewidth = 1) + geom_point(color = "#1D8DB0", size = 2)
      } else { # Should be factor by now if not numeric
        p <- p + geom_col(fill = "#1D8DB0")
      }
    } else { # Assume categorical or non-numeric factored if not numeric
      p <- p + geom_col(fill = "#1D8DB0") # Bar plot for categorical
    }
    
    # ADDITION FOR 1D PLOT
    if (fill_var == "p_lapse") {
      p <- p + geom_hline(yintercept = 0.5, color = "red", linetype = "dashed", linewidth = 1)
    }
    
    # Add x_interest point for 1D
    if (!is.null(x_interest_point_data)) {
      # Ensure x_interest_point_data[[x_var]] is factor with same levels if categorical
      # or just use the numeric value if it's numeric and was not factored in main data
      if (x_var %in% names(cat_vars_levels) && is.character(x_interest_point_data[[x_var]])) {
        x_interest_point_data[[x_var]] <- factor(x_interest_point_data[[x_var]], levels = levels(data[[x_var]]))
      } else if (is.numeric(data[[x_var]])) { # If main data's x_var was numeric and NOT factored
        # Do nothing, use numeric value directly for geom_point
      } else { # If main data's x_var was factored (e.g., character to factor)
        x_interest_point_data[[x_var]] <- factor(x_interest_point_data[[x_var]], levels = levels(data[[x_var]]))
      }
      
      p <- p +
        geom_point(
          data = x_interest_point_data,
          aes(x = .data[[x_var]], y = .data[[fill_var]]),
          color = "black", shape = 19, size = 4,
          inherit.aes = FALSE
        )
    }
    
  } else if (num_vars == 2) {
    x_var <- selected_scenario_vars[1]
    y_var <- selected_scenario_vars[2]
    plot_title <- paste(plot_title_base, "by", x_var, "and", y_var)
    
    # factor conversion
    for (var in c(x_var, y_var)) {
      if (var %in% names(cat_vars_levels) && is.character(data[[var]])) {
        data[[var]] <- factor(data[[var]], levels = cat_vars_levels[[var]])
      } else if (is.numeric(data[[var]])) {
        # Combine all unique values from data and x_interest for robust factor levels
        all_unique_values <- unique(c(data[[var]], if (!is.null(x_interest_point_data)) x_interest_point_data[[var]] else NULL))
        # Sort them numerically
        sorted_levels <- sort(unique(all_unique_values))
        data[[var]] <- factor(data[[var]], levels = sorted_levels, ordered = TRUE)
      } else { # Default to simple factor for other types if not explicitly handled
        data[[var]] <- factor(data[[var]])
      }
    }
    
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[fill_var]])) +
      geom_tile(color = "white", linewidth = 0.3) +
      labs(title = plot_title, x = x_var, y = y_var) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      )
    
    # Choose color scale based on fill_var
    if (fill_var == "pred_diff") {
      p <- p + scale_fill_gradient2(low = "#1D8DB0", mid = "white", high = "#DD8A2E", midpoint = 0, name = fill_var)
    } else { # p_lapse
      p <- p + scale_fill_gradient(low = "#1D8DB0", high = "#DD8A2E", name = fill_var)
    }
    
    # Add x_interest point for 2D
    if (!is.null(x_interest_point_data)) {
      # Ensure x_interest values for x_var and y_var are factors and match levels
      # Use the levels already established from the main data for consistency
      x_interest_point_data[[x_var]] <- factor(x_interest_point_data[[x_var]], levels = levels(data[[x_var]]))
      x_interest_point_data[[y_var]] <- factor(x_interest_point_data[[y_var]], levels = levels(data[[y_var]]))
      
      p <- p +
        geom_point(
          data = x_interest_point_data,
          aes(x = .data[[x_var]], y = .data[[y_var]]), # Use .data for consistency
          color = "black", shape = 19, size = 4,
          inherit.aes = FALSE
        )
    }
    
  } else if (num_vars == 3) {
    x_var <- selected_scenario_vars[1]
    y_var <- selected_scenario_vars[2]
    facet_var <- selected_scenario_vars[3]
    plot_title <- paste(plot_title_base, "by", x_var, ",", y_var, "and", facet_var)
    
    # factor conversion
    for (var in c(x_var, y_var, facet_var)) {
      if (var %in% names(cat_vars_levels) && is.character(data[[var]])) {
        data[[var]] <- factor(data[[var]], levels = cat_vars_levels[[var]])
      } else if (is.numeric(data[[var]])) {
        # Combine all unique values from data and x_interest for robust factor levels
        all_unique_values <- unique(c(data[[var]], if (!is.null(x_interest_point_data)) x_interest_point_data[[var]] else NULL))
        # Sort them numerically
        sorted_levels <- sort(unique(all_unique_values))
        data[[var]] <- factor(data[[var]], levels = sorted_levels, ordered = TRUE)
      } else { # Default to simple factor for other types if not explicitly handled
        data[[var]] <- factor(data[[var]])
      }
    }
    
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[fill_var]])) +
      geom_tile(color = "white", linewidth = 0.3) +
      facet_wrap(as.formula(paste("~", facet_var))) + # Facet by the third variable
      labs(title = plot_title, x = x_var, y = y_var) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      )
    
    # Choose color scale based on fill_var
    if (fill_var == "pred_diff") {
      p <- p + scale_fill_gradient2(low = "#1D8DB0", mid = "white", high = "#DD8A2E", midpoint = 0, name = fill_var)
    } else { # p_lapse
      p <- p + scale_fill_gradient(low = "#1D8DB0", high = "#DD8A2E", name = fill_var)
    }
    
    # Add x_interest point for 3D (faceted)
    if (!is.null(x_interest_point_data)) {
      # Create a specific dataframe for the point to ensure it's plotted on the correct facet
      x_interest_plot_data <- data.frame(
        x_val = x_interest_point_data[[x_var]],
        y_val = x_interest_point_data[[y_var]],
        facet_val = x_interest_point_data[[facet_var]],
        fill_val = x_interest_point_data[[fill_var]]
      )
      # Ensure factor levels match the main data's levels
      x_interest_plot_data$x_val <- factor(x_interest_plot_data$x_val, levels = levels(data[[x_var]]))
      x_interest_plot_data$y_val <- factor(x_interest_plot_data$y_val, levels = levels(data[[y_var]]))
      x_interest_plot_data$facet_val <- factor(x_interest_plot_data$facet_val, levels = levels(data[[facet_var]]))
      
      p <- p +
        geom_point(
          data = x_interest_plot_data,
          aes(x = x_val, y = y_val),
          color = "black", shape = 19, size = 4,
          inherit.aes = FALSE
        )
    }
    
  } else {
    message("Visualization for 4 or more scenario variables is not directly supported as a single plot type.")
    message("Consider filtering your 'data' to fewer dimensions or creating multiple plots.")
    return(NULL) # Return NULL or an informative message
  }
  
  return(p)
}


# --- Example Usage of plot_scenario_impact ---

# first turn annual prem & psrate into factor
load("x_interest")
str(x_interest)
x_interest$ps_rate <- round(x_interest$ps_rate, 4)
x_interest$annual_prem <- round(x_interest$annual_prem, 2)


# Example 1: 1D Plot (e.g., impact of annual_prem)
selected_vars_1D <- c("annual_prem")
data_1D <- generate_scenario_combinations(
  x_interest = x_interest,
  cat_vars_levels = cat_vars_levels,
  annual_prem_multipliers = annual_prem_multipliers,
  ps_rate_increments = ps_rate_increments,
  model_xgb = model_xgb,
  predictors = predictors,
  selected_scenario_vars = selected_vars_1D
)
print("Plotting 1D (annual_prem):")
plot_1D <- plot_scenario_impact(data_1D, selected_vars_1D, fill_var = "p_lapse", x_interest = x_interest, cat_vars_levels = cat_vars_levels)
print(plot_1D)

plot_1D_pred_diff <- plot_scenario_impact(data_1D, selected_vars_1D, fill_var = "pred_diff", x_interest = x_interest, cat_vars_levels = cat_vars_levels)
print(plot_1D_pred_diff)

# Example 1a: 1D Plot (e.g., impact of living_place)
selected_vars_1D <- c("living_place")
data_1D <- generate_scenario_combinations(
  x_interest = x_interest,
  cat_vars_levels = cat_vars_levels,
  annual_prem_multipliers = annual_prem_multipliers,
  ps_rate_increments = ps_rate_increments,
  model_xgb = model_xgb,
  predictors = predictors,
  selected_scenario_vars = selected_vars_1D
)
print("Plotting 1D (living_place):")
plot_1D <- plot_scenario_impact(data_1D, selected_vars_1D, fill_var = "p_lapse", x_interest = x_interest, cat_vars_levels = cat_vars_levels)
print(plot_1D)

plot_1D_pred_diff <- plot_scenario_impact(data_1D, selected_vars_1D, fill_var = "pred_diff", x_interest = x_interest, cat_vars_levels = cat_vars_levels)
print(plot_1D_pred_diff)

# Example 1b: 1D Plot (e.g., impact of ps_rate)
selected_vars_1D <- c("ps_rate")
data_1D <- generate_scenario_combinations(
  x_interest = x_interest,
  cat_vars_levels = cat_vars_levels,
  annual_prem_multipliers = annual_prem_multipliers,
  ps_rate_increments = ps_rate_increments,
  model_xgb = model_xgb,
  predictors = predictors,
  selected_scenario_vars = selected_vars_1D
)
print("Plotting 1D (ps_rate):")
plot_1D <- plot_scenario_impact(data_1D, selected_vars_1D, fill_var = "p_lapse", x_interest = x_interest, cat_vars_levels = cat_vars_levels)
print(plot_1D)

plot_1D_pred_diff <- plot_scenario_impact(data_1D, selected_vars_1D, fill_var = "pred_diff", x_interest = x_interest, cat_vars_levels = cat_vars_levels)
print(plot_1D_pred_diff)


# Example 2: 2D Plot (e.g., impact of annual_prem and ps_rate)
selected_vars_2D <- c("annual_prem", "ps_rate")
data_2D <- generate_scenario_combinations(
  x_interest = x_interest,
  cat_vars_levels = cat_vars_levels,
  annual_prem_multipliers = annual_prem_multipliers,
  ps_rate_increments = ps_rate_increments,
  model_xgb = model_xgb,
  predictors = predictors,
  selected_scenario_vars = selected_vars_2D
)
print("Plotting 2D (annual_prem vs ps_rate):")
plot_2D <- plot_scenario_impact(data_2D, selected_vars_2D, fill_var = "pred_diff", x_interest = x_interest, cat_vars_levels = cat_vars_levels)
print(plot_2D)

plot_2D_p_lapse <- plot_scenario_impact(data_2D, selected_vars_2D, fill_var = "p_lapse", x_interest = x_interest, cat_vars_levels = cat_vars_levels)
print(plot_2D_p_lapse)


# Example 3: 3D Plot (e.g., annual_prem, ps_rate, and risk_class as facet)
selected_vars_3D <- c("annual_prem", "ps_rate", "risk_class")
data_3D <- generate_scenario_combinations(
  x_interest = x_interest,
  cat_vars_levels = cat_vars_levels,
  annual_prem_multipliers = annual_prem_multipliers,
  ps_rate_increments = ps_rate_increments,
  model_xgb = model_xgb,
  predictors = predictors,
  selected_scenario_vars = selected_vars_3D
)
print("Plotting 3D (annual_prem vs ps_rate, faceted by risk_class):")
plot_3D <- plot_scenario_impact(data_3D, selected_vars_3D, fill_var = "pred_diff", x_interest = x_interest, cat_vars_levels = cat_vars_levels)
print(plot_3D)

ggsave(
  filename = "heatmap_3d.png",
  plot = plot_3D,
  width = 10, # Adjust width as needed
  height = 5, # Adjust height as needed
  units = "in",
  dpi = 300,
  bg = "white"
)

plot_3D_p_lapse <- plot_scenario_impact(data_3D, selected_vars_3D, fill_var = "p_lapse", x_interest = x_interest, cat_vars_levels = cat_vars_levels)
print(plot_3D_p_lapse)

# Example 4: 4D (or more) - Not supported directly
selected_vars_4D <- c("annual_prem", "ps_rate", "risk_class", "prem_freq")
data_4D <- generate_scenario_combinations(
  x_interest = x_interest,
  cat_vars_levels = cat_vars_levels,
  annual_prem_multipliers = annual_prem_multipliers,
  ps_rate_increments = ps_rate_increments,
  model_xgb = model_xgb,
  predictors = predictors,
  selected_scenario_vars = selected_vars_4D
)
print("Attempting to plot 4D (should show message):")
plot_4D <- plot_scenario_impact(data_4D, selected_vars_4D, fill_var = "pred_diff", x_interest = x_interest, cat_vars_levels = cat_vars_levels)
print(plot_4D) # This will print NULL and the message from the function


# ------------------------------------------------------------------------------
# --- Loop for 1D plots  ---

# Define all scenario variables that can be varied individually
all_scenario_vars <- c("prem_freq", "living_place", "annual_prem", "risk_class", "ps_rate")
one_d_plots <- list()

library(cowplot)

# Loop through each scenario variable to generate a 1D plot
for (s_var in all_scenario_vars) {
  message(paste("Generating 1D plot for:", s_var))
  
  # Generate data for the current 1D scenario
  current_data_1D <- generate_scenario_combinations(
    x_interest = x_interest,
    cat_vars_levels = cat_vars_levels,
    annual_prem_multipliers = annual_prem_multipliers,
    ps_rate_increments = ps_rate_increments,
    model_xgb = model_xgb,
    predictors = predictors,
    selected_scenario_vars = c(s_var) # Only select the current variable
  )
  
  # Generate the 1D plot for p_lapse
  current_plot <- plot_scenario_impact(
    data = current_data_1D,
    selected_scenario_vars = c(s_var),
    fill_var = "p_lapse",
    x_interest = x_interest,
    cat_vars_levels = cat_vars_levels
  )
  
  # Add plot to the list if successfully created
  if (!is.null(current_plot)) {
    one_d_plots[[s_var]] <- current_plot
  }
}

# Arrange all 1D plots in a grid
if (length(one_d_plots) > 0) {
  # Remove any NULL plots if some failed to generate
  one_d_plots_valid <- one_d_plots[!sapply(one_d_plots, is.null)]
  
  if (length(one_d_plots_valid) > 0) {
    
    # Define plots for the top row (three equal-width plots)
    top_row_plots <- one_d_plots_valid[c("prem_freq", "living_place", "annual_prem")]
    
    # Define plots for the bottom row (two plots, one double-width)
    bottom_row_plots <- one_d_plots_valid[c("ps_rate", "risk_class")]
    
    # Create the plot for the top row
    top_row_grid <- plot_grid(
      plotlist = top_row_plots,
      ncol = 3,
      labels = c("A", "B", "C"),
      rel_widths = c(1, 1, 1)
    )
    
    # Create the plot for the bottom row, with "risk_class" being double-width
    # Note: rel_widths = c(1, 2) makes the second plot (risk_class) twice as wide as the first.
    bottom_row_grid <- plot_grid(
      plotlist = bottom_row_plots,
      ncol = 2,
      labels = c("D", "E"),
      rel_widths = c(1, 2)
    )
    
    # Combine the two rows into a final grid
    combined_1d_plot_grid <- plot_grid(
      top_row_grid,
      bottom_row_grid,
      ncol = 1,
      rel_heights = c(1, 1) # This ensures both rows have the same height
    )
    # Save the combined plot to a file
    # Adjust width and height based on the number of plots and desired output size
    ggsave(
      filename = "all_1d_scenario_plots.png",
      plot = combined_1d_plot_grid,
      width = 12, # Example width
      height = 8,  # Adjusted height for two rows
      units = "in",
      dpi = 300,
      bg = "white"
    )
    message("Saved all 1D scenario plots to 'all_1d_scenario_plots.png'")
  } else {
    message("No valid 1D plots were generated to combine.")
  }
} else {
  message("No 1D plots were generated.")
}



# --- Loop for 2D Plots ---

# Generate all unique combinations of 2 variables
var_pairs_2D <- combn(all_scenario_vars, 2, simplify = FALSE)

# List to store all 2D plots
two_d_plots <- list()
message(paste("Generating", length(var_pairs_2D), "2D plots for all combinations..."))

# Loop through each pair of scenario variables to generate a 2D plot
for (i in seq_along(var_pairs_2D)) {
  pair <- var_pairs_2D[[i]]
  s_var1 <- pair[1]
  s_var2 <- pair[2]
  
  message(paste0("Generating 2D plot for: ", s_var1, " vs ", s_var2))
  
  # Generate data for the current 2D scenario
  current_data_2D <- generate_scenario_combinations(
    x_interest = x_interest,
    cat_vars_levels = cat_vars_levels,
    annual_prem_multipliers = annual_prem_multipliers,
    ps_rate_increments = ps_rate_increments,
    model_xgb = model_xgb,
    predictors = predictors,
    selected_scenario_vars = c(s_var1, s_var2) # Select both variables for the combination
  )
  
  # # Generate the 2D plot for pred_diff
  # current_plot_pred_diff <- plot_scenario_impact(
  #   data = current_data_2D,
  #   selected_scenario_vars = c(s_var1, s_var2),
  #   fill_var = "pred_diff",
  #   x_interest = x_interest,
  #   cat_vars_levels = cat_vars_levels
  # )
  # 
  # Generate the 2D plot for p_lapse
  current_plot_p_lapse <- plot_scenario_impact(
    data = current_data_2D,
    selected_scenario_vars = c(s_var1, s_var2),
    fill_var = "p_lapse",
    x_interest = x_interest,
    cat_vars_levels = cat_vars_levels
  )
  
  # Add plots to the list if successfully created
  # if (!is.null(current_plot_pred_diff)) {
  #   plot_name_pred_diff <- paste0(s_var1, "_vs_", s_var2, "_pred_diff")
  #   two_d_plots[[plot_name_pred_diff]] <- current_plot_pred_diff
  # }
  if (!is.null(current_plot_p_lapse)) {
    plot_name_p_lapse <- paste0(s_var1, "_vs_", s_var2, "_p_lapse")
    two_d_plots[[plot_name_p_lapse]] <- current_plot_p_lapse
  }
}

# Now, arrange and save the 2D plots
if (length(two_d_plots) > 0) {
  # Remove any NULL plots if some failed to generate
  two_d_plots_valid <- two_d_plots[!sapply(two_d_plots, is.null)]
  
  if (length(two_d_plots_valid) > 0) {
    # You have 5 variables, so combn(5, 2) = 10 unique pairs.
    # If you generate two plots per pair (pred_diff and p_lapse), that's 20 plots.
    
    # Split the list into chunks for the grids
    # First 6 plots (indices 1 to 6)
    grid_plot1_indices <- 1:min(6, length(two_d_plots_valid))
    grid_plot1_list <- two_d_plots_valid[grid_plot1_indices]
    
    # Remaining plots (indices 7 onwards)
    grid_plot2_indices <- (min(6, length(two_d_plots_valid)) + 1):length(two_d_plots_valid)
    grid_plot2_list <- two_d_plots_valid[grid_plot2_indices]
    
    # Combine and save Grid 1 (first 6 plots as 3x2)
    if (length(grid_plot1_list) > 0) {
      # Determine ncol and nrow dynamically based on the number of plots for grid 1
      # For 6 plots, 3x2 (ncol=2, nrow=3) or 2x3 (ncol=3, nrow=2) is good.
      # Let's target 2 columns for a 3x2 layout as requested.
      num_cols_grid1 <- 2
      combined_2d_plot_grid1 <- plot_grid(
        plotlist = grid_plot1_list,
        ncol = num_cols_grid1,
        labels = "AUTO", # Add labels A, B, C...
        rel_widths = rep(1, num_cols_grid1) # Equal widths for columns
      )
      
      # Save the combined plot 1
      ggsave(
        filename = "2d_scenario_plots_grid1.png",
        plot = combined_2d_plot_grid1,
        width = 16, # Adjust width as needed for 2 columns of heatmaps
        height = 8 * ceiling(length(grid_plot1_list) / num_cols_grid1), # Dynamic height
        units = "in",
        dpi = 300,
        bg = "white"
      )
      message("Saved 2D scenario plots Grid 1 to '2d_scenario_plots_grid1.png'")
    } else {
      message("No valid plots for 2D scenario plots Grid 1.")
    }
    
    # Combine and save Grid 2 (last remaining plots as 2x2)
    if (length(grid_plot2_list) > 0) {
      # For remaining plots, target 2 columns for a 2x2 layout
      num_cols_grid2 <- 2
      combined_2d_plot_grid2 <- plot_grid(
        plotlist = grid_plot2_list,
        ncol = num_cols_grid2,
        labels = "AUTO", # Continue labels from previous grid or restart
        rel_widths = rep(1, num_cols_grid2)
      )
      
      # Save the combined plot 2
      ggsave(
        filename = "2d_scenario_plots_grid2.png",
        plot = combined_2d_plot_grid2,
        width = 16, # Adjust width as needed
        height = 8 * ceiling(length(grid_plot2_list) / num_cols_grid2), # Dynamic height
        units = "in",
        dpi = 300,
        bg = "white"
      )
      message("Saved 2D scenario plots Grid 2 to '2d_scenario_plots_grid2.png'")
    } else {
      message("No valid plots for 2D scenario plots Grid 2.")
    }
    
  } else {
    message("No valid 2D plots were generated to combine.")
  }
} else {
  message("No 2D plots were generated.")
}



# --- Loop for 3D Plots (Interactive) ---
# Ensure you have necessary libraries loaded
library(ggplot2) # For plot_scenario_impact (though not directly used for plotly output)
library(dplyr)   # For data manipulation
library(cowplot) # For plot_grid (not directly used for plotly output)
library(plotly)  # For interactive 3D plots
library(viridis) # For color scales in plotly (optional, plotly has defaults)

# Generate all unique combinations of 3 variables
var_triples_3D <- combn(all_scenario_vars, 3, simplify = FALSE)

message(paste("Generating", length(var_triples_3D), "3D interactive plots for all combinations..."))

# Loop through each triple of scenario variables to generate a 3D plot
for (i in seq_along(var_triples_3D)) {
  triple <- var_triples_3D[[i]]
  s_var1 <- triple[1]
  s_var2 <- triple[2]
  s_var3 <- triple[3]
  
  plot_title <- paste("Impact on p_lapse by", s_var1, ",", s_var2, "and", s_var3)
  message(paste0("Generating 3D plot for: ", s_var1, " vs ", s_var2, " vs ", s_var3))
  
  # Generate data for the current 3D scenario
  current_data_3D <- generate_scenario_combinations(
    x_interest = x_interest,
    cat_vars_levels = cat_vars_levels,
    annual_prem_multipliers = annual_prem_multipliers,
    ps_rate_increments = ps_rate_increments,
    model_xgb = model_xgb,
    predictors = predictors,
    selected_scenario_vars = c(s_var1, s_var2, s_var3)
  )
  
  # Check if data is valid before plotting
  if (is.null(current_data_3D) || nrow(current_data_3D) == 0) {
    warning(paste0("No data generated for 3D plot of ", paste(triple, collapse = ", "), ". Skipping plot."))
    next
  }
  
  axis_data <- list()
  axis_ticks <- list()
  
  for (var_name in c(s_var1, s_var2, s_var3)) {
    if (is.factor(current_data_3D[[var_name]])) {
      original_levels <- levels(current_data_3D[[var_name]])
      axis_data[[var_name]] <- as.numeric(current_data_3D[[var_name]])
      axis_ticks[[var_name]] <- list(
        tickvals = seq_along(original_levels),
        ticktext = original_levels
      )
    } else if (is.numeric(current_data_3D[[var_name]])) {
      axis_data[[var_name]] <- current_data_3D[[var_name]]
      axis_ticks[[var_name]] <- list()
    } else {
      warning(paste0("Variable '", var_name, "' is neither numeric nor factor. Attempting to convert to factor."))
      current_data_3D[[var_name]] <- as.factor(current_data_3D[[var_name]])
      original_levels <- levels(current_data_3D[[var_name]])
      axis_data[[var_name]] <- as.numeric(current_data_3D[[var_name]])
      axis_ticks[[var_name]] <- list(
        tickvals = seq_along(original_levels),
        ticktext = original_levels
      )
    }
  }
  
  # Prepare x_interest for the highlight point similarly
  x_interest_clean <- as.data.frame(x_interest)
  x_interest_axis_data <- list()
  for (var_name in c(s_var1, s_var2, s_var3)) {
    if (is.factor(x_interest_clean[[var_name]])) {
      if (!is.null(levels(current_data_3D[[var_name]]))) {
        x_interest_clean[[var_name]] <- factor(x_interest_clean[[var_name]], levels = levels(current_data_3D[[var_name]]))
      }
      x_interest_axis_data[[var_name]] <- as.numeric(x_interest_clean[[var_name]])
    } else {
      x_interest_axis_data[[var_name]] <- x_interest_clean[[var_name]]
    }
  }
  
  # Create the 3D scatter plot using plotly
  fig_3d_plot <- plot_ly(
    data = current_data_3D,
    x = axis_data[[s_var1]],
    y = axis_data[[s_var2]],
    z = axis_data[[s_var3]],
    color = ~p_lapse,
    colorscale = 'Viridis',
    type = 'scatter3d',
    mode = 'markers',
    marker = list(size = 5, opacity = 0.8),
    text = ~paste0(
      s_var1, ": ", .data[[s_var1]], "<br>",
      s_var2, ": ", .data[[s_var2]], "<br>",
      s_var3, ": ", .data[[s_var3]], "<br>",
      "p_lapse: ", round(p_lapse, 4)
    ),
    hoverinfo = 'text',
    # FIX: Use empty name and showlegend=FALSE along with a legendgroup
    name = "", # Empty string for name to suppress text
    showlegend = FALSE, # Hide this trace from the legend
    legendgroup = "main_scenario_data" # Assign it to a group
  ) %>%
    layout(
      title = plot_title,
      scene = list(
        xaxis = c(list(title = s_var1), axis_ticks[[s_var1]]),
        yaxis = c(list(title = s_var2), axis_ticks[[s_var2]]),
        zaxis = c(list(title = s_var3), axis_ticks[[s_var3]])
      )
    )
  
  # Add x_interest point if provided
  if (!is.null(x_interest) && nrow(x_interest) > 0 &&
      all(c(s_var1, s_var2, s_var3, "p_lapse") %in% colnames(x_interest))) {
    
    fig_3d_plot <- fig_3d_plot %>%
      add_trace(
        data = x_interest_clean,
        x = x_interest_axis_data[[s_var1]],
        y = x_interest_axis_data[[s_var2]],
        z = x_interest_axis_data[[s_var3]],
        mode = 'markers',
        marker = list(size = 8, color = 'red', symbol = 'circle', line = list(color = 'black', width = 2)),
        name = "Point of Interest",
        showlegend = TRUE, # This must remain TRUE to show the Point of Interest
        legendgroup = "x_interest_data", # Assign it to a different group
        inherit = FALSE,
        hoverinfo = 'text',
        text = ~paste0(
          "Original Point<br>",
          s_var1, ": ", x_interest_clean[[s_var1]], "<br>",
          s_var2, ": ", x_interest_clean[[s_var2]], "<br>",
          s_var3, ": ", x_interest_clean[[s_var3]], "<br>",
          "p_lapse: ", round(x_interest_clean$p_lapse, 4)
        )
      )
  }
  
  print(fig_3d_plot)
  # htmlwidgets::saveWidget(fig_3d_plot, file = paste0("3d_scenario_", s_var1, "_", s_var2, "_", s_var3, "_p_lapse.html"))
}

# red dot: x_interest



# --- Other loop for 3D Plots (Filled Cube/Volume (instead of dots) Heatmap) ---
message(paste("Generating", length(var_triples_3D), "3D interactive volume plots for all combinations..."))

# Loop through each triple of scenario variables to generate a 3D volume plot
for (i in seq_along(var_triples_3D)) {
  triple <- var_triples_3D[[i]]
  s_var1 <- triple[1] # X-axis
  s_var2 <- triple[2] # Y-axis
  s_var3 <- triple[3] # Z-axis
  
  plot_title <- paste("3D Volume Heatmap: p_lapse by", s_var1, ",", s_var2, "and", s_var3)
  message(paste0("Generating 3D volume plot for: ", s_var1, " vs ", s_var2, " vs ", s_var3))
  
  # Generate data for the current 3D scenario (this is the potentially sparse data)
  current_data_3D_sparse <- generate_scenario_combinations(
    x_interest = x_interest,
    cat_vars_levels = cat_vars_levels,
    annual_prem_multipliers = annual_prem_multipliers,
    ps_rate_increments = ps_rate_increments,
    model_xgb = model_xgb,
    predictors = predictors,
    selected_scenario_vars = c(s_var1, s_var2, s_var3)
  )
  
  # Check if data is valid before proceeding
  if (is.null(current_data_3D_sparse) || nrow(current_data_3D_sparse) == 0) {
    warning(paste0("No data generated for 3D volume plot of ", paste(triple, collapse = ", "), ". Skipping plot."))
    next
  }
  
  
  # 1. Determine all possible levels/values for each variable
  #    Prioritize cat_vars_levels for categorical variables
  #    For numeric variables, collect all unique values from sparse data and x_interest, then sort them.
  
  # Initialize lists to hold the comprehensive levels for each variable
  all_variable_levels <- list()
  
  for (var_name_iter in c(s_var1, s_var2, s_var3)) {
    if (var_name_iter %in% names(cat_vars_levels)) {
      # Use pre-defined categorical levels
      all_variable_levels[[var_name_iter]] <- cat_vars_levels[[var_name_iter]]
    } else {
      # For other types (likely numeric):
      # Collect all unique values from the sparse data and x_interest
      combined_values <- unique(c(current_data_3D_sparse[[var_name_iter]],
                                  if (!is.null(x_interest)) x_interest[[var_name_iter]] else NULL))
      # Sort them if they are numeric, otherwise just use unique values
      if (is.numeric(combined_values)) {
        all_variable_levels[[var_name_iter]] <- sort(combined_values)
      } else {
        all_variable_levels[[var_name_iter]] <- unique(combined_values) # For non-numeric, non-predefined
      }
    }
  }
  
  # 2. Create the full grid using expand.grid
  full_grid <- expand.grid(
    V1 = all_variable_levels[[s_var1]],
    V2 = all_variable_levels[[s_var2]],
    V3 = all_variable_levels[[s_var3]],
    stringsAsFactors = FALSE # Ensure characters are not auto-converted to factors here
  )
  colnames(full_grid) <- c(s_var1, s_var2, s_var3)
  
  # 3. Left Join the sparse data onto the full grid
  #    Combinations not in current_data_3D_sparse will get NA for p_lapse.
  volume_data_full <- left_join(full_grid, current_data_3D_sparse, by = c(s_var1, s_var2, s_var3))
  
  # Ensure p_lapse values are available after the join
  if (!"p_lapse" %in% colnames(volume_data_full) || all(is.na(volume_data_full$p_lapse))) {
    warning(paste0("All 'p_lapse' values are missing or NA for combination ", paste(triple, collapse = ", "), ". Skipping plot."))
    next
  }
  
  # --- Prepare data for Plotly axes and volume values (using the full grid) ---
  volume_data <- as.data.frame(volume_data_full) # Use the fully populated data frame
  
  axis_levels <- list() # Store actual levels for ticktext
  axis_indices <- list() # Store numeric indices for x, y, z aesthetics
  
  # Process volume_data to convert to factors/ordered factors with correct levels
  for (var_name in c(s_var1, s_var2, s_var3)) {
    # Use the levels determined from the all_variable_levels for consistency
    final_levels_for_factor <- all_variable_levels[[var_name]]
    
    volume_data[[var_name]] <- factor(volume_data[[var_name]],
                                      levels = final_levels_for_factor,
                                      ordered = is.numeric(final_levels_for_factor)) # Order if levels are numeric
    
    axis_levels[[var_name]] <- levels(volume_data[[var_name]]) # Get the levels after factoring
    axis_indices[[var_name]] <- as.numeric(volume_data[[var_name]]) # Convert to 1-based index
  }
  
  # Get min/max p_lapse for the color scale, ignoring NAs
  plot_p_lapse_min <- min(volume_data$p_lapse, na.rm = TRUE)
  plot_p_lapse_max <- max(volume_data$p_lapse, na.rm = TRUE)
  
  # Create the 3D volume plot
  fig_3d_volume <- plot_ly(
    data = volume_data,
    x = axis_indices[[s_var1]],
    y = axis_indices[[s_var2]],
    z = axis_indices[[s_var3]],
    value = ~p_lapse, # Use p_lapse for the volume intensity
    type = 'volume',
    opacity = 0.6,
    colorscale = 'Viridis',
    colorbar = list(title = "p_lapse"),
    isomin = plot_p_lapse_min, # Set min/max for color scale correctly
    isomax = plot_p_lapse_max,
    name = "", # Hide main trace from legend (no 'trace 0')
    showlegend = FALSE,
    legendgroup = "main_volume_data"
  ) %>%
    layout(
      title = plot_title,
      scene = list(
        xaxis = list(
          title = s_var1,
          tickvals = seq_along(axis_levels[[s_var1]]),
          ticktext = axis_levels[[s_var1]]
        ),
        yaxis = list(
          title = s_var2,
          tickvals = seq_along(axis_levels[[s_var2]]),
          ticktext = axis_levels[[s_var2]]
        ),
        zaxis = list(
          title = s_var3,
          tickvals = seq_along(axis_levels[[s_var3]]),
          ticktext = axis_levels[[s_var3]]
        )
      )
    )
  
  # Add x_interest point if provided (as a scatter point on top of the volume)
  if (!is.null(x_interest) && nrow(x_interest) > 0 &&
      all(c(s_var1, s_var2, s_var3, "p_lapse") %in% colnames(x_interest))) {
    
    x_interest_clean <- as.data.frame(x_interest)
    
    # Convert x_interest variables to factors using the levels from the main volume_data for consistent indexing
    x_interest_indices <- list()
    for (var_name in c(s_var1, s_var2, s_var3)) {
      # Use the *established* levels from the `all_variable_levels` for consistent factoring
      if (!is.null(all_variable_levels[[var_name]])) {
        x_interest_clean[[var_name]] <- factor(x_interest_clean[[var_name]],
                                               levels = all_variable_levels[[var_name]],
                                               ordered = is.numeric(all_variable_levels[[var_name]]))
      } else {
        # Fallback if levels somehow weren't established (shouldn't happen with the new grid logic)
        x_interest_clean[[var_name]] <- factor(x_interest_clean[[var_name]])
      }
      x_interest_indices[[var_name]] <- as.numeric(x_interest_clean[[var_name]])
    }
    
    fig_3d_volume <- fig_3d_volume %>%
      add_trace(
        data = x_interest_clean,
        x = x_interest_indices[[s_var1]],
        y = x_interest_indices[[s_var2]],
        z = x_interest_indices[[s_var3]],
        type = 'scatter3d', # Add as a scatter trace on top
        mode = 'markers',
        marker = list(size = 8, color = 'red', symbol = 'circle', line = list(color = 'black', width = 2)),
        name = "Point of Interest",
        showlegend = TRUE,
        legendgroup = "x_interest_data", # Assign a different group
        inherit = FALSE,
        hoverinfo = 'text',
        text = ~paste0(
          "Original Point<br>",
          s_var1, ": ", x_interest_clean[[s_var1]], "<br>",
          s_var2, ": ", x_interest_clean[[s_var2]], "<br>",
          s_var3, ": ", x_interest_clean[[s_var3]], "<br>",
          "p_lapse: ", round(x_interest_clean$p_lapse, 4)
        )
      )
  }
  
  print(fig_3d_volume) # This will open the interactive plot
}

message("Finished generating 3D interactive volume plots.")





# ==============================================================================================
# Before: pure visualization of differences in p_lapse
# now: more towards hyperboxes: differentiating p_lapse >< 0.5

# --- plot_hyperbox function ---
plot_hyperbox <- function(data, selected_scenario_vars, threshold = 0.5, x_interest = NULL, cat_vars_levels = NULL) {
  
  # Basic validation
  if (is.null(data) || nrow(data) == 0) {
    stop("Input 'data' is empty or NULL.")
  }
  if (!"p_lapse" %in% colnames(data)) {
    stop("'p_lapse' column not found in the input data. This function requires p_lapse predictions.")
  }
  if (!all(selected_scenario_vars %in% colnames(data))) {
    stop("Not all 'selected_scenario_vars' are present in the input data.")
  }
  if (is.null(x_interest) || !"p_lapse" %in% colnames(x_interest)) {
    stop("x_interest must be provided and contain a 'p_lapse' column.")
  }
  
  num_vars <- length(selected_scenario_vars)
  plot_title_base <- "Impact on p_lapse"
  p <- NULL
  description <- "Hyperbox description not available."
  
  data <- data %>%
    mutate(p_lapse_category = ifelse(p_lapse < threshold, "Below Threshold", "Above Threshold"))
  
  x_interest_category <- ifelse(x_interest$p_lapse < threshold, "Below Threshold", "Above Threshold")
  
  color_scale_values <- c("Below Threshold" = "green", "Above Threshold" = "red")
  
  if (num_vars == 0) {
    message("No scenario variables selected. Returning a single point plot (if data has one row).")
    if (nrow(data) == 1) {
      p <- ggplot(data, aes(x = 1, y = p_lapse, color = p_lapse_category)) +
        geom_point(size = 5) +
        scale_color_manual(values = color_scale_values, guide = "none") +
        labs(title = plot_title_base,
             x = "", y = "p_lapse") +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5))
      
      description <- paste0("The x_interest point (p_lapse = ", round(x_interest$p_lapse, 4), ") is in the '", x_interest_category, "' zone.")
    } else {
      message("Data has more than one row but no scenario variables selected. Cannot visualize meaningfully.")
      return(list(plot = NULL, description = description))
    }
    
  } else if (num_vars == 1) {
    x_var <- selected_scenario_vars[1]
    plot_title <- paste(plot_title_base, "by", x_var)
    
    if (x_var %in% names(cat_vars_levels) && is.character(data[[x_var]])) {
      data[[x_var]] <- factor(data[[x_var]], levels = cat_vars_levels[[x_var]])
      x_interest[[x_var]] <- factor(x_interest[[x_var]], levels = levels(data[[x_var]]))
    } else if (is.numeric(data[[x_var]])) {
      data <- data %>% arrange(.data[[x_var]])
    }
    
    p <- ggplot(data, aes(x = .data[[x_var]], y = p_lapse)) +
      labs(title = plot_title, x = x_var, y = "p_lapse") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    if (is.numeric(data[[x_var]])) {
      p <- p +
        geom_line(color = "black", linewidth = 1) +
        geom_point(aes(color = p_lapse_category), size = 3) +
        scale_color_manual(values = color_scale_values, name = "p_lapse Category", guide = "none")
    } else {
      p <- p +
        geom_col(aes(fill = p_lapse_category)) +
        scale_fill_manual(values = color_scale_values, name = "p_lapse Category", guide = "none")
    }
    
    p <- p +
      geom_point(
        data = x_interest,
        aes(x = .data[[x_var]], y = p_lapse),
        color = "black", shape = 19, size = 4,
        inherit.aes = FALSE
      ) +
      geom_hline(yintercept = threshold, color = "red", linetype = "dashed", linewidth = 1)
    
    if (is.numeric(data[[x_var]])) {
      x_interest_val <- x_interest[[x_var]]
      same_category_data <- data %>% filter(p_lapse_category == x_interest_category)
      relevant_segments <- same_category_data %>%
        arrange(.data[[x_var]]) %>%
        mutate(segment_id = cumsum(c(1, diff(.data[[x_var]]) > (mean(diff(.data[[x_var]]), na.rm = TRUE) * 1.1))))
      x_interest_segment_id <- relevant_segments %>%
        filter(.data[[x_var]] == x_interest_val) %>%
        pull(segment_id) %>%
        unique()
      if (length(x_interest_segment_id) > 0) {
        segment_data <- relevant_segments %>% filter(segment_id == x_interest_segment_id[1])
        min_val <- min(segment_data[[x_var]])
        max_val <- max(segment_data[[x_var]])
        description <- paste0("The x_interest point (", x_var, " = ", round(x_interest_val, 2), ", p_lapse = ", round(x_interest$p_lapse, 4), ") is in the '", x_interest_category, "' zone. This zone extends from ", x_var, " = ", round(min_val, 2), " to ", round(max_val, 2), ".")
      } else {
        description <- paste0("The x_interest point (", x_var, " = ", round(x_interest_val, 2), ", p_lapse = ", round(x_interest$p_lapse, 4), ") is in the '", x_interest_category, "' zone. No continuous segment found for description.")
      }
    } else {
      same_category_levels <- data %>%
        filter(p_lapse_category == x_interest_category) %>%
        pull(.data[[x_var]]) %>%
        unique() %>%
        as.character()
      description <- paste0("The x_interest point (", x_var, " = ", x_interest[[x_var]], ", p_lapse = ", round(x_interest$p_lapse, 4), ") is in the '", x_interest_category, "' zone. Other categories in this zone include: ", paste(sort(same_category_levels), collapse = ", "), ".")
    }
    
  } else if (num_vars == 2) {
    x_var <- selected_scenario_vars[1]
    y_var <- selected_scenario_vars[2]
    plot_title <- paste(plot_title_base, "by", x_var, "and", y_var)
    
    if (x_var %in% names(cat_vars_levels)) {
      data[[x_var]] <- factor(data[[x_var]], levels = cat_vars_levels[[x_var]])
      x_interest[[x_var]] <- factor(x_interest[[x_var]], levels = levels(data[[x_var]]))
    } else { data[[x_var]] <- as.numeric(data[[x_var]]) }
    if (y_var %in% names(cat_vars_levels)) {
      data[[y_var]] <- factor(data[[y_var]], levels = cat_vars_levels[[y_var]])
      x_interest[[y_var]] <- factor(x_interest[[y_var]], levels = levels(data[[y_var]]))
    } else { data[[y_var]] <- as.numeric(data[[y_var]]) }
    
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]], fill = p_lapse_category)) +
      geom_tile(color = "white", linewidth = 0.3) +
      scale_fill_manual(values = color_scale_values, name = "p_lapse Category", guide = "none") +
      labs(title = plot_title, x = x_var, y = y_var) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(0.5, "cm"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      )
    
    if (is.numeric(data[[x_var]])) { p <- p + scale_x_continuous(expand = c(0,0)) } else { p <- p + scale_x_discrete(expand = c(0,0)) }
    if (is.numeric(data[[y_var]])) { p <- p + scale_y_continuous(expand = c(0,0)) } else { p <- p + scale_y_discrete(expand = c(0,0)) }
    
    p <- p +
      geom_point(
        data = x_interest,
        aes(x = .data[[x_var]], y = .data[[y_var]]),
        color = "black", shape = 19, size = 4,
        inherit.aes = FALSE
      )
    
    x_val_interest <- if (is.numeric(x_interest[[x_var]])) round(x_interest[[x_var]], 2) else x_interest[[x_var]]
    y_val_interest <- if (is.numeric(x_interest[[y_var]])) round(x_interest[[y_var]], 2) else x_interest[[y_var]]
    
    description <- paste0("The x_interest point (", x_var, " = ", x_val_interest, ", ", y_var, " = ", y_val_interest, ", p_lapse = ", round(x_interest$p_lapse, 4), ") is in the '", x_interest_category, "' zone. This plot shows a 2D hyperbox where each cell is colored based on its p_lapse category.")
    
  } else if (num_vars == 3) {
    x_var <- selected_scenario_vars[1]
    y_var <- selected_scenario_vars[2]
    facet_var <- selected_scenario_vars[3]
    plot_title <- paste(plot_title_base, "by", x_var, ",", y_var, "and", facet_var)
    
    if (x_var %in% names(cat_vars_levels)) {
      data[[x_var]] <- factor(data[[x_var]], levels = cat_vars_levels[[x_var]])
      x_interest[[x_var]] <- factor(x_interest[[x_var]], levels = levels(data[[x_var]]))
    } else { data[[x_var]] <- as.numeric(data[[x_var]]) }
    if (y_var %in% names(cat_vars_levels)) {
      data[[y_var]] <- factor(data[[y_var]], levels = cat_vars_levels[[y_var]])
      x_interest[[y_var]] <- factor(x_interest[[y_var]], levels = levels(data[[y_var]]))
    } else { data[[y_var]] <- as.numeric(data[[y_var]]) }
    if (facet_var %in% names(cat_vars_levels)) {
      data[[facet_var]] <- factor(data[[facet_var]], levels = cat_vars_levels[[facet_var]])
      x_interest[[facet_var]] <- factor(x_interest[[facet_var]], levels = levels(data[[facet_var]]))
    } else { data[[facet_var]] <- as.numeric(data[[facet_var]]) }
    
    
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]], fill = p_lapse_category)) +
      geom_tile(color = "white", linewidth = 0.3) +
      facet_wrap(as.formula(paste("~", facet_var)), scales = "free_x") +
      scale_fill_manual(values = color_scale_values, name = "p_lapse Category", guide = "none") +
      labs(title = plot_title, x = x_var, y = y_var) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(0.5, "cm"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      )
    
    if (is.numeric(data[[x_var]])) { p <- p + scale_x_continuous(expand = c(0,0)) } else { p <- p + scale_x_discrete(expand = c(0,0)) }
    if (is.numeric(data[[y_var]])) { p <- p + scale_y_continuous(expand = c(0,0)) } else { p <- p + scale_y_discrete(expand = c(0,0)) }
    
    p <- p +
      geom_point(
        data = x_interest,
        aes(x = .data[[x_var]], y = .data[[y_var]]),
        color = "black", shape = 19, size = 4,
        inherit.aes = FALSE
      )
    
    x_val_interest <- if (is.numeric(x_interest[[x_var]])) round(x_interest[[x_var]], 2) else x_interest[[x_var]]
    y_val_interest <- if (is.numeric(x_interest[[y_var]])) round(x_interest[[y_var]], 2) else x_interest[[y_var]]
    facet_val_interest <- if (is.numeric(x_interest[[facet_var]])) round(x_interest[[facet_var]], 2) else x_interest[[facet_var]]
    
    description <- paste0("The x_interest point (", x_var, " = ", x_val_interest, ", ", y_var, " = ", y_val_interest, ", ", facet_var, " = ", facet_val_interest, ", p_lapse = ", round(x_interest$p_lapse, 4), ") is in the '", x_interest_category, "' zone. This plot shows a 3D hyperbox (faceted 2D plots) where each cell is colored based on its p_lapse category.")
    
  } else {
    message("Visualization for 4 or more scenario variables is not directly supported as a single plot type.")
    message("Consider filtering your 'data' to fewer dimensions or creating multiple plots.")
    return(list(plot = NULL, description = description))
  }
  
  return(list(plot = p, description = description))
}


# ------------------------------------------------------------------------------
# --- Example Usage of plot_hyperbox ---

# Example 1: 1D Hyperbox Plot (e.g., impact of annual_prem)
selected_vars_1D_hyper <- c("annual_prem")
data_1D_hyper <- generate_scenario_combinations(
  x_interest = x_interest,
  cat_vars_levels = cat_vars_levels,
  annual_prem_multipliers = annual_prem_multipliers,
  ps_rate_increments = ps_rate_increments,
  model_xgb = model_xgb,
  predictors = predictors,
  selected_scenario_vars = selected_vars_1D_hyper
)
print("Plotting 1D Hyperbox (annual_prem):")
hyperbox_1D_result <- plot_hyperbox(data_1D_hyper, selected_vars_1D_hyper, x_interest = x_interest, cat_vars_levels = cat_vars_levels)
print(hyperbox_1D_result$plot)
print(hyperbox_1D_result$description)


# Example 1a: 1D Hyperbox Plot (e.g., impact of prem_freq - categorical)
selected_vars_1D_cat_hyper <- c("prem_freq")
data_1D_cat_hyper <- generate_scenario_combinations(
  x_interest = x_interest,
  cat_vars_levels = cat_vars_levels,
  annual_prem_multipliers = annual_prem_multipliers,
  ps_rate_increments = ps_rate_increments,
  model_xgb = model_xgb,
  predictors = predictors,
  selected_scenario_vars = selected_vars_1D_cat_hyper
)
print("Plotting 1D Hyperbox (prem_freq):")
hyperbox_1D_cat_result <- plot_hyperbox(data_1D_cat_hyper, selected_vars_1D_cat_hyper, x_interest = x_interest, cat_vars_levels = cat_vars_levels)
print(hyperbox_1D_cat_result$plot)
print(hyperbox_1D_cat_result$description)


# Example 2: 2D Hyperbox Plot
selected_vars_2D_hyper <- c("living_place", "risk_class")
data_2D_hyper <- generate_scenario_combinations(
  x_interest = x_interest,
  cat_vars_levels = cat_vars_levels,
  annual_prem_multipliers = annual_prem_multipliers,
  ps_rate_increments = ps_rate_increments,
  model_xgb = model_xgb,
  predictors = predictors,
  selected_scenario_vars = selected_vars_2D_hyper
)
print("Plotting 2D Hyperbox :")
hyperbox_2D_result <- plot_hyperbox(data_2D_hyper, selected_vars_2D_hyper, x_interest = x_interest, cat_vars_levels = cat_vars_levels)
print(hyperbox_2D_result$plot)
print(hyperbox_2D_result$description)

ggsave(
  filename = "hyperbox_2d.png",
  plot = hyperbox_2D_result$plot,
  width = 6, # Adjust width as needed
  height = 6, # Adjust height as needed
  units = "in",
  dpi = 300,
  bg = "white"
)


# Example 3: 3D Hyperbox Plot (e.g., annual_prem, ps_rate, and risk_class as facet)
selected_vars_3D_hyper <- c("annual_prem", "ps_rate", "risk_class")
data_3D_hyper <- generate_scenario_combinations(
  x_interest = x_interest,
  cat_vars_levels = cat_vars_levels,
  annual_prem_multipliers = annual_prem_multipliers,
  ps_rate_increments = ps_rate_increments,
  model_xgb = model_xgb,
  predictors = predictors,
  selected_scenario_vars = selected_vars_3D_hyper
)
print("Plotting 3D Hyperbox (annual_prem vs ps_rate, faceted by risk_class):")
hyperbox_3D_result <- plot_hyperbox(data_3D_hyper, selected_vars_3D_hyper, x_interest = x_interest, cat_vars_levels = cat_vars_levels)
print(hyperbox_3D_result$plot)
print(hyperbox_3D_result$description)

ggsave(
  filename = "hyperbox_3d_.png",
  plot = hyperbox_3D_result$plot,
  width = 10, # Adjust width as needed
  height = 8, # Adjust height as needed
  units = "in",
  dpi = 300,
  bg = "white"
)



# Example 4: 4D (or more) Hyperbox - Not supported directly
selected_vars_4D_hyper <- c("annual_prem", "ps_rate", "risk_class", "prem_freq")
data_4D_hyper <- generate_scenario_combinations(
  x_interest = x_interest,
  cat_vars_levels = cat_vars_levels,
  annual_prem_multipliers = annual_prem_multipliers,
  ps_rate_increments = ps_rate_increments,
  model_xgb = model_xgb,
  predictors = predictors,
  selected_scenario_vars = selected_vars_4D_hyper
)
print("Attempting to plot 4D Hyperbox (should show message and return NULL):")
hyperbox_4D_result <- plot_hyperbox(data_4D_hyper, selected_vars_4D_hyper, x_interest = x_interest, cat_vars_levels = cat_vars_levels)
print(hyperbox_4D_result$plot)
print(hyperbox_4D_result$description)


# ------------------------------------------------------------------------------

# --- Loop for 1D Hyperbox Plots ---
# Define a threshold value outside the function
threshold_value <- 0.5

# Define all scenario variables that can be varied individually
all_scenario_vars <- c("prem_freq", "living_place", "annual_prem", "risk_class", "ps_rate")
one_d_hyperbox_plots <- list()
library(cowplot)
library(ggplot2)

for (s_var in all_scenario_vars) {
  message(paste("Generating 1D hyperbox plot for:", s_var))
  
  current_data_1D <- generate_scenario_combinations(
    x_interest = x_interest,
    cat_vars_levels = cat_vars_levels,
    annual_prem_multipliers = annual_prem_multipliers,
    ps_rate_increments = ps_rate_increments,
    model_xgb = model_xgb,
    predictors = predictors,
    selected_scenario_vars = c(s_var)
  )
  
  # Pass the threshold to the function
  hyperbox_result <- plot_hyperbox(
    data = current_data_1D,
    selected_scenario_vars = c(s_var),
    x_interest = x_interest,
    cat_vars_levels = cat_vars_levels,
    threshold = threshold_value
  )
  
  if (!is.null(hyperbox_result$plot)) {
    one_d_hyperbox_plots[[s_var]] <- hyperbox_result$plot
  }
}

if (length(one_d_hyperbox_plots) > 0) {
  one_d_plots_valid <- one_d_hyperbox_plots[!sapply(one_d_hyperbox_plots, is.null)]
  
  if (length(one_d_plots_valid) > 0) {
    top_row_plots <- one_d_plots_valid[c("prem_freq", "living_place", "annual_prem")]
    bottom_row_plots <- one_d_plots_valid[c("ps_rate", "risk_class")]
    
    top_row_grid <- plot_grid(
      plotlist = top_row_plots,
      ncol = 3,
      labels = c("A", "B", "C"),
      rel_widths = c(1, 1, 1)
    )
    
    # Create the legend separately from a simple plot
    legend_plot <- ggplot(data.frame(p_lapse_category = c("Below Threshold", "Above Threshold"), x = 1, y = 1),
                          aes(x = x, y = y, color = p_lapse_category)) +
      geom_point(size = 5) +
      scale_color_manual(values = c("Below Threshold" = "green", "Above Threshold" = "red"),
                         name = "p_lapse Category") +
      theme_void() +
      guides(color = guide_legend(override.aes = list(size = 5)))
    
    legend <- get_legend(legend_plot)
    
    bottom_row_grid <- plot_grid(
      plotlist = bottom_row_plots,
      ncol = 2,
      labels = c("D", "E"),
      rel_widths = c(1, 2)
    )
    
    combined_rows <- plot_grid(
      top_row_grid,
      bottom_row_grid,
      ncol = 1,
      rel_heights = c(1, 1)
    )
    
    final_plot_with_legend <- plot_grid(
      combined_rows, legend,
      ncol = 2,
      rel_widths = c(3, 0.4)
    )
    
    final_title <- ggdraw() + 
      draw_label(
        paste("1D plots for p_lapse (Threshold =", threshold_value, ")"),
        fontface = 'bold',
        x = 0,
        hjust = 0,
        size = 16
      ) +
      theme(
        plot.margin = margin(0, 0, 0, 7)
      )
    
    final_plot <- plot_grid(
      final_title, final_plot_with_legend,
      ncol = 1,
      rel_heights = c(0.1, 1)
    )
    
    ggsave(
      filename = "all_1d_hyperbox_plots.png",
      plot = final_plot,
      width = 15,
      height = 8,
      units = "in",
      dpi = 300,
      bg = "white"
    )
    message("Saved all 1D hyperbox plots to 'all_1d_hyperbox_plots.png'")
  } else {
    message("No valid 1D plots were generated to combine.")
  }
} else {
  message("No 1D plots were generated.")
}




# --- Loop for 2D Plots ---

# Define a threshold value for the hyperbox plots
threshold_value <- 0.5

# Generate all unique combinations of 2 variables
# Assuming 'all_scenario_vars' is already defined from previous examples
all_scenario_vars <- c("prem_freq", "living_place", "annual_prem", "risk_class", "ps_rate")
var_pairs_2D <- combn(all_scenario_vars, 2, simplify = FALSE)

# List to store all 2D plots
two_d_hyperbox_plots <- list()
message(paste("Generating", length(var_pairs_2D), "2D plots for all combinations..."))

# Loop through each pair of scenario variables to generate a 2D plot
for (i in seq_along(var_pairs_2D)) {
  pair <- var_pairs_2D[[i]]
  s_var1 <- pair[1]
  s_var2 <- pair[2]
  
  message(paste0("Generating 2D hyperbox plot for: ", s_var1, " vs ", s_var2))
  
  # Generate data for the current 2D scenario
  current_data_2D <- generate_scenario_combinations(
    x_interest = x_interest,
    cat_vars_levels = cat_vars_levels,
    annual_prem_multipliers = annual_prem_multipliers,
    ps_rate_increments = ps_rate_increments,
    model_xgb = model_xgb,
    predictors = predictors,
    selected_scenario_vars = c(s_var1, s_var2)
  )
  
  # Generate the 2D hyperbox plot
  hyperbox_result <- plot_hyperbox(
    data = current_data_2D,
    selected_scenario_vars = c(s_var1, s_var2),
    x_interest = x_interest,
    cat_vars_levels = cat_vars_levels,
    threshold = threshold_value
  )
  
  # Add the plot component to the list if successfully created
  if (!is.null(hyperbox_result$plot)) {
    plot_name <- paste0(s_var1, "_vs_", s_var2)
    two_d_hyperbox_plots[[plot_name]] <- hyperbox_result$plot
  }
}

# Now, arrange and save the 2D plots
if (length(two_d_hyperbox_plots) > 0) {
  two_d_plots_valid <- two_d_hyperbox_plots[!sapply(two_d_hyperbox_plots, is.null)]
  
  if (length(two_d_plots_valid) > 0) {
    # We have 10 plots total, so we'll create a 3x2 grid and a 2x2 grid.
    
    # Split the list into chunks for the grids
    grid_plot1_list <- two_d_plots_valid[1:6]
    grid_plot2_list <- two_d_plots_valid[7:10]
    
    # --- Create a single legend and title for all plots ---
    legend_plot <- ggplot(data.frame(p_lapse_category = c("Below Threshold", "Above Threshold"), x = 1, y = 1),
                          aes(x = x, y = y, fill = p_lapse_category)) +
      geom_tile() + # Use geom_tile to match the heatmap style
      scale_fill_manual(values = c("Below Threshold" = "green", "Above Threshold" = "red"),
                        name = "p_lapse Category") +
      theme_void()
    
    legend <- get_legend(legend_plot)
    
    main_title <- ggdraw() + 
      draw_label(
        paste("2D plots of p_lapse (Threshold =", threshold_value, ")"),
        fontface = 'bold',
        x = 0, hjust = 0, size = 16
      ) +
      theme(plot.margin = margin(0, 0, 0, 7))
    
    # --- Combine and save Grid 1 (first 6 plots as 3x2) ---
    if (length(grid_plot1_list) > 0) {
      combined_2d_plot_grid1 <- plot_grid(
        plotlist = grid_plot1_list,
        ncol = 2,
        labels = "AUTO",
        label_size = 12
      )
      
      final_plot_grid1 <- plot_grid(
        main_title,
        plot_grid(combined_2d_plot_grid1, legend, ncol = 2, rel_widths = c(3, 0.4)),
        ncol = 1,
        rel_heights = c(0.1, 1)
      )
      
      ggsave(
        filename = "2d_hyperbox_plots_grid1.png",
        plot = final_plot_grid1,
        width = 16,
        height = 12,
        units = "in",
        dpi = 300,
        bg = "white"
      )
      message("Saved 2D hyperbox plots Grid 1 to '2d_hyperbox_plots_grid1.png'")
    } else {
      message("No valid plots for 2D hyperbox plots Grid 1.")
    }
    
    # --- Combine and save Grid 2 (last 4 plots as 2x2) ---
    if (length(grid_plot2_list) > 0) {
      combined_2d_plot_grid2 <- plot_grid(
        plotlist = grid_plot2_list,
        ncol = 2,
        labels = "AUTO",
        label_size = 12
      )
      
      final_plot_grid2 <- plot_grid(
        main_title,
        plot_grid(combined_2d_plot_grid2, legend, ncol = 2, rel_widths = c(3, 0.4)),
        ncol = 1,
        rel_heights = c(0.1, 1)
      )
      
      ggsave(
        filename = "2d_hyperbox_plots_grid2.png",
        plot = final_plot_grid2,
        width = 16,
        height = 12,
        units = "in",
        dpi = 300,
        bg = "white"
      )
      message("Saved 2D hyperbox plots Grid 2 to '2d_hyperbox_plots_grid2.png'")
    } else {
      message("No valid plots for 2D hyperbox plots Grid 2.")
    }
  } else {
    message("No valid 2D plots were generated to combine.")
  }
} else {
  message("No 2D plots were generated.")
}



# ------------------------------------------------------------------------------
# P-DIMENSIONAL OPTIMAL HYPERBOX CALCULATION

# --- Data Preparation  ---
# 1. Rename the dataset to 'lapsedata'
load("combinations_with_pred_3")
lapsedata <- combinations_with_pred_3
lapsedata$pred_lapse <- ifelse(lapsedata$p_lapse >= 0.5, "Yes", "No")
lapsedata$pred_lapse <- as.factor(lapsedata$pred_lapse) # Explicitly convert to factor

cat("--- Data Preparation ---\n")
cat("Added 'pred_lapse' column to 'lapsedata' based on 'p_lapse' threshold (0.5).\n")
cat("Distribution of 'pred_lapse' in 'lapsedata':\n")
print(table(lapsedata$pred_lapse)) # this should correctly show factor levels

# 2. Define the categorical variables of interest
scenario_vars <- c("prem_freq", "living_place", "annual_prem", "risk_class", "ps_rate")

# 3. Ensure the specified columns are treated as factors in 'lapsedata'
cat("\nConverting specified variables to factors in 'lapsedata' if they aren't already:\n")
for (var in scenario_vars) {
  if (!is.factor(lapsedata[[var]])) {
    lapsedata[[var]] <- as.factor(lapsedata[[var]])
    cat(paste0("  - Converted '", var, "' to factor.\n"))
  } else {
    cat(paste0("  - '", var, "' is already a factor.\n"))
  }
}

# 4. Print the levels for these categorical variables
cat("\nLevels of categorical variables in 'lapsedata':\n")
for (var in scenario_vars) {
  if (length(levels(lapsedata[[var]])) > 10) {
    cat(paste0("- ", var, ": (", length(levels(lapsedata[[var]])), " levels, showing first 10) ", paste(head(levels(lapsedata[[var]]), 10), collapse = ", "), ", ...\n"))
  } else {
    cat(paste0("- ", var, ": ", paste(levels(lapsedata[[var]]), collapse = ", "), "\n"))
  }
}


# Convert x_interest scenario_vars to factors to match lapsedata's levels
load("x_interest")
str(x_interest)

x_interest$ps_rate <- round(x_interest$ps_rate, 4)
x_interest$annual_prem <- round(x_interest$annual_prem, 2)

# This is crucial for comparisons and index lookups
for (var in scenario_vars) {
  # Convert numeric to character for annual_prem and ps_rate before factor conversion
  if (is.numeric(x_interest[[var]])) {
    x_interest[[var]] <- as.character(x_interest[[var]])
  }
  x_interest[[var]] <- factor(x_interest[[var]], levels = levels(lapsedata[[var]]))
  # Check if the mandatory level from x_interest actually exists in lapsedata levels
  if (!as.character(x_interest[[var]]) %in% levels(lapsedata[[var]])) {
    stop(paste0("Error: Mandatory level '", as.character(x_interest[[var]]),
                "' for variable '", var, "' not found in 'lapsedata' levels. Please check your data."))
  }
}

# Derive pred_lapse for x_interest
x_interest$pred_lapse <- factor(ifelse(x_interest$p_lapse >= 0.5, "Yes", "No"), levels = c("No", "Yes"))

cat("\nStructure of generated 'lapsedata':\n")
print(str(lapsedata))
cat("\nx_interest prediction for lapse: ", as.character(x_interest$pred_lapse), "\n")


# ------------------------------------------------------------------------------
library(dplyr)
library(purrr) # For map, if needed, but expand.grid is base R
library(rlang) # For !!rlang::sym()

# optimal box function
find_optimal_lapse_subset_pure <- function(lapsesdata, x_interest) {
  
  cat("Starting analysis to find the optimal PURE subset...\n")
  
  scenario_vars <- c("prem_freq", "living_place", "annual_prem", "risk_class", "ps_rate")
  
  x_interest_pred_lapse <- as.character(x_interest$pred_lapse)
  cat(paste0("Target 'pred_lapse' for x_interest (and for 100% purity): ", x_interest_pred_lapse, "\n"))
  
  max_pure_coverage <- -1 # Will store the highest proportion of total_rows_lapsedata covered by a pure subset
  best_subset_levels <- NULL
  best_subset_df_size <- 0 # Will store the number of rows in lapsedata that form this pure subset
  
  total_rows_lapsedata <- nrow(lapsesdata)
  cat(paste0("Total rows in 'lapsesdata': ", total_rows_lapsedata, "\n\n"))
  
  # --- 1. Generate all possible level selections for each variable ---
  cat("Generating possible level selections for each scenario variable dynamically from 'lapsedata'...\n")
  possible_selections_list <- list()
  
  for (var_name in scenario_vars) {
    all_levels <- levels(lapsesdata[[var_name]])
    mandatory_level <- as.character(x_interest[[var_name]])
    
    if (!(mandatory_level %in% all_levels)) {
      stop(paste0("Error: Mandatory level '", mandatory_level, "' for variable '", var_name,
                  "' is not present in 'lapsedata' levels. Please check your 'lapsedata' and 'x_interest' inputs."))
    }
    
    if (var_name %in% c("annual_prem", "ps_rate")) {
      cat(paste0("  - Processing '", var_name, "' (continuous range constraint, mandatory: ", mandatory_level, ")\n"))
      mandatory_idx <- which(all_levels == mandatory_level)
      L <- length(all_levels)
      
      current_var_selections <- list()
      count <- 1
      for (start_pos in 1:mandatory_idx) {
        for (end_pos in mandatory_idx:L) {
          selected_range_levels <- all_levels[start_pos:end_pos]
          current_var_selections[[count]] <- selected_range_levels
          count <- count + 1
        }
      }
      possible_selections_list[[var_name]] <- current_var_selections
      cat(paste0("    Number of continuous ranges for '", var_name, "': ", length(current_var_selections), "\n"))
      
    } else {
      cat(paste0("  - Processing '", var_name, "' (mandatory: ", mandatory_level, ")\n"))
      other_levels <- setdiff(all_levels, mandatory_level)
      
      current_var_selections <- list()
      num_other_levels <- length(other_levels)
      
      for (i in 0:(2^num_other_levels - 1)) {
        binary_representation <- as.logical(intToBits(i)[1:num_other_levels])
        selected_from_others <- other_levels[binary_representation]
        current_var_selections[[i + 1]] <- c(mandatory_level, selected_from_others)
      }
      possible_selections_list[[var_name]] <- current_var_selections
      cat(paste0("    Number of combinations for '", var_name, "': ", length(current_var_selections), "\n"))
    }
    
    if (length(possible_selections_list[[var_name]]) == 0) {
      stop(paste0("Internal Error: Variable '", var_name, "' generated no possible level selections. This indicates a logic error or unexpected data state."))
    }
  }
  
  # --- 2. Create an index grid for all possible subset combinations ---
  cat("\nCreating index grid for all possible subset combinations...\n")
  grid_inputs <- purrr::map(possible_selections_list, ~1:length(.x))
  names(grid_inputs) <- scenario_vars
  
  cat("  DEBUG: Content of grid_inputs before expand.grid:\n")
  print(grid_inputs)
  
  index_grid <- do.call(expand.grid, grid_inputs)
  
  total_subsets_to_check <- nrow(index_grid)
  cat(paste0("Total number of subsets to evaluate: ", total_subsets_to_check, "\n"))
  cat("This process might take some time given the number of subsets (",
      format(total_subsets_to_check, big.mark = ","), ").\n\n")
  
  
  # --- 3. Loop through each combination of selected levels and evaluate ---
  progress_interval <- max(1, floor(total_subsets_to_check / 100))
  
  for (i in 1:total_subsets_to_check) {
    if (i %% progress_interval == 0) {
      cat(paste0("Processing subset ", format(i, big.mark = ","), " of ",
                 format(total_subsets_to_check, big.mark = ","), " (",
                 round(i/total_subsets_to_check*100), "%)\n"))
    }
    
    current_indices <- index_grid[i, ]
    current_subset_levels <- list()
    expected_rows_product <- 1
    
    # Reconstruct the current subset's levels
    for (var_name in scenario_vars) {
      debug_index_value <- as.numeric(current_indices[[var_name]])
      
      if (is.null(possible_selections_list[[var_name]]) ||
          debug_index_value <= 0 ||
          debug_index_value > length(possible_selections_list[[var_name]]) ||
          is.null(possible_selections_list[[var_name]][[debug_index_value]])) {
        
        stop(paste0("CRITICAL ERROR DIAGNOSIS (Iteration ", i, "): Invalid or NULL selection for variable '", var_name, "'.\n",
                    "  Index to be used: ", debug_index_value, "\n",
                    "  Type of index: ", typeof(debug_index_value), "\n",
                    "  Is 'possible_selections_list[[var_name]]' NULL? ", is.null(possible_selections_list[[var_name]]), "\n",
                    "  Length of 'possible_selections_list[[var_name]]': ", length(possible_selections_list[[var_name]]), "\n",
                    "  Is element at this index NULL? ", is.null(possible_selections_list[[var_name]][[debug_index_value]]), "\n",
                    "  If debug_index_value is not integer, this might be the cause."))
      }
      
      selected_levels <- possible_selections_list[[var_name]][[debug_index_value]]
      current_subset_levels[[var_name]] <- selected_levels
      expected_rows_product <- expected_rows_product * length(selected_levels)
    }
    
    filtered_df <- lapsesdata
    for (var_name in scenario_vars) {
      filtered_df <- filtered_df %>%
        dplyr::filter(!!rlang::sym(var_name) %in% current_subset_levels[[var_name]])
    }
    
    num_rows_in_subset <- nrow(filtered_df)
    
    # Check if all rows in the filtered_df have the target prediction
    is_pure_subset <- all(filtered_df$pred_lapse == x_interest_pred_lapse)
    
    if (!is_pure_subset) {
      # If not pure, this subset is not considered for max_pure_coverage
      next
    }
    
    # If it is pure, proceed to check its size (coverage)
    if (num_rows_in_subset != expected_rows_product) {
      warning(paste0("Subset consistency check failed for iteration ", i, ".\n",
                     "Expected rows (product of levels): ", expected_rows_product,
                     ", Actual filtered rows: ", num_rows_in_subset, ".\n",
                     "This might indicate an inconsistency in your 'lapsedata' structure (not a full factorial for scenario_vars)."))
    }

    # Coverage is now the size of this pure subset compared to the total dataset size
    current_pure_coverage <- num_rows_in_subset / total_rows_lapsedata
    
    if (current_pure_coverage > max_pure_coverage) {
      max_pure_coverage <- current_pure_coverage
      best_subset_levels <- current_subset_levels
      best_subset_df_size <- num_rows_in_subset # Store the actual size of the pure subset
    }
  }
  
  cat("\nAnalysis complete!\n")
  if (!is.null(best_subset_levels)) {
    cat("\n--- Optimal PURE Subset Found ---\n")
    cat(paste0("Highest PURE Coverage (proportion of total lapsedata rows): ", sprintf("%.4f", max_pure_coverage * 100), "%\n"))
    cat(paste0("Number of combinations (rows in lapsedata) covered by this pure subset: ", format(best_subset_df_size, big.mark = ","), "\n"))
    cat("Levels included in the optimal pure subset:\n")
    for (var_name in scenario_vars) {
      cat(paste0("- ", var_name, ": ", paste(best_subset_levels[[var_name]], collapse = ", "), "\n"))
    }
  } else {
    cat("No valid PURE subsets found that contain the x_interest prediction. This means no hyperbox was 100% pure.\n")
  }
  
  invisible(list(
    best_subset_levels = best_subset_levels,
    max_pure_coverage = max_pure_coverage
  ))
}

# --- Example Usage  ---
optimal_pure_result <- find_optimal_lapse_subset_pure(lapsedata, x_interest)
save(optimal_pure_result, file = "optimal_pure_result")
print(optimal_pure_result)


# ------------------------------------------------------------------------------
# Chapter 4.3: Other examples

load("lapses_data")
load("model_xgb")

cat_vars <- c("prem_freq", "living_place", "risk_class")
cat_vars_levels <- list(
  prem_freq = c("Semi-annual", 'Quarterly', 'Monthly', 'Annual', 'Other'),
  living_place = c("EastCoast", "Other", "WestCoast"),
  risk_class = c('SubStd-smoker', 'Prefered-smoker', 'Prefered-nonSmoker',
                 'SubStd-nonSmoker', 'Standard-nonSmoker', 'Standard-smoker'))

# Numerical feature multipliers/increments
annual_prem_multipliers <- seq(1.5, 0.5, by = -0.05)
ps_rate_increments <- seq(-0.1, 0.1, by = 0.01)

# XGBoost model and predictors
predictors <- setdiff(names(lapses_data), c("policy_id", "data_year", "surrenders"))

scenario_vars <- c("prem_freq", "living_place", "annual_prem", "risk_class", "ps_rate")



# Chapter 4.3.1: just above threshold policy
x_mid = lapses_data[93833,]
print(x_mid$p_lapse)

for (col in cat_vars) {
  if (col %in% colnames(x_mid) && is.character(x_mid[[col]])) {
    x_mid[[col]] <- factor(x_mid[[col]], levels = cat_vars_levels[[col]])
  }
}

# create dataframe
combidf_mid <- generate_scenario_combinations(
  x_mid = x_mid,
  cat_vars_levels = cat_vars_levels,
  annual_prem_multipliers = annual_prem_multipliers,
  ps_rate_increments = ps_rate_increments,
  model_xgb = model_xgb,
  predictors = predictors,
  selected_scenario_vars = c("prem_freq", "living_place", "annual_prem", "risk_class", "ps_rate")
)

print(nrow(combidf_mid))

combidf_mid$pred_lapse <- ifelse(combidf_mid$p_lapse >= 0.5, "Yes", "No")
combidf_mid$pred_lapse <- as.factor(combidf_mid$pred_lapse) # Explicitly convert to factor
print(table(combidf_mid$pred_lapse)) # Now this should correctly show factor levels

# Ensure the specified columns are treated as factors in 'combidf_mid'
cat("\nConverting specified variables to factors in 'combidf_mid' if they aren't already:\n")
for (var in scenario_vars) {
  if (!is.factor(combidf_mid[[var]])) {
    combidf_mid[[var]] <- as.factor(combidf_mid[[var]])
    cat(paste0("  - Converted '", var, "' to factor.\n"))
  } else {
    cat(paste0("  - '", var, "' is already a factor.\n"))
  }
}

# Print the levels for these categorical variables
cat("\nLevels of categorical variables in 'combidf_mid':\n")
for (var in scenario_vars) {
  if (length(levels(combidf_mid[[var]])) > 10) {
    cat(paste0("- ", var, ": (", length(levels(combidf_mid[[var]])), " levels, showing first 10) ", paste(head(levels(combidf_mid[[var]]), 10), collapse = ", "), ", ...\n"))
  } else {
    cat(paste0("- ", var, ": ", paste(levels(combidf_mid[[var]]), collapse = ", "), "\n"))
  }
}

x_mid$ps_rate <- round(x_mid$ps_rate, 4)
x_mid$annual_prem <- round(x_mid$annual_prem, 2)

for (var in scenario_vars) {
  # Convert numeric to character for annual_prem and ps_rate before factor conversion
  if (is.numeric(x_mid[[var]])) {
    x_mid[[var]] <- as.character(x_mid[[var]])
  }
  x_mid[[var]] <- factor(x_mid[[var]], levels = levels(combidf_mid[[var]]))
  # Check if the mandatory level from x_mid actually exists in combidf_mid levels
  if (!as.character(x_mid[[var]]) %in% levels(combidf_mid[[var]])) {
    stop(paste0("Error: Mandatory level '", as.character(x_mid[[var]]),
                "' for variable '", var, "' not found in 'combidf_mid' levels. Please check your data."))
  }
}

# Derive pred_lapse for x_mid
x_mid$pred_lapse <- factor(ifelse(x_mid$p_lapse >= 0.5, "Yes", "No"), levels = c("No", "Yes"))

cat("\nStructure of generated 'combidf_mid':\n")
print(str(combidf_mid))
cat("\nx_mid prediction for lapse: ", as.character(x_mid$pred_lapse), "\n")

# hyperbox function
box_mid <- find_optimal_lapse_subset_pure(combidf_mid, x_mid)
save(box_mid, file = "box_mid")
print(box_mid)



# Chapter 4.3.2: high lapse prediction policy
x_high = lapses_data[25195,]
print(x_high$p_lapse)

for (col in cat_vars) {
  if (col %in% colnames(x_high) && is.character(x_high[[col]])) {
    x_high[[col]] <- factor(x_high[[col]], levels = cat_vars_levels[[col]])
  }
}

# create dataframe
combidf_high <- generate_scenario_combinations(
  x_interest = x_high,
  cat_vars_levels = cat_vars_levels,
  annual_prem_multipliers = annual_prem_multipliers,
  ps_rate_increments = ps_rate_increments,
  model_xgb = model_xgb,
  predictors = predictors,
  selected_scenario_vars = c("prem_freq", "living_place", "annual_prem", "risk_class", "ps_rate")
)

print(nrow(combidf_high))

combidf_high$pred_lapse <- ifelse(combidf_high$p_lapse >= 0.5, "Yes", "No")
combidf_high$pred_lapse <- as.factor(combidf_high$pred_lapse) # Explicitly convert to factor
print(table(combidf_high$pred_lapse)) # Now this should correctly show factor levels

# Ensure the specified columns are treated as factors in 'combidf_high'
cat("\nConverting specified variables to factors in 'combidf_high' if they aren't already:\n")
for (var in scenario_vars) {
  if (!is.factor(combidf_high[[var]])) {
    combidf_high[[var]] <- as.factor(combidf_high[[var]])
    cat(paste0("  - Converted '", var, "' to factor.\n"))
  } else {
    cat(paste0("  - '", var, "' is already a factor.\n"))
  }
}

# Print the levels for these categorical variables
cat("\nLevels of categorical variables in 'combidf_high':\n")
for (var in scenario_vars) {
  if (length(levels(combidf_high[[var]])) > 10) {
    cat(paste0("- ", var, ": (", length(levels(combidf_high[[var]])), " levels, showing first 10) ", paste(head(levels(combidf_high[[var]]), 10), collapse = ", "), ", ...\n"))
  } else {
    cat(paste0("- ", var, ": ", paste(levels(combidf_high[[var]]), collapse = ", "), "\n"))
  }
}

x_high$ps_rate <- round(x_high$ps_rate, 4)
x_high$annual_prem <- round(x_high$annual_prem, 2)

for (var in scenario_vars) {
  # Convert numeric to character for annual_prem and ps_rate before factor conversion
  if (is.numeric(x_high[[var]])) {
    x_high[[var]] <- as.character(x_high[[var]])
  }
  x_high[[var]] <- factor(x_high[[var]], levels = levels(combidf_high[[var]]))
  # Check if the mandatory level from x_high actually exists in combidf_high levels
  if (!as.character(x_high[[var]]) %in% levels(combidf_high[[var]])) {
    stop(paste0("Error: Mandatory level '", as.character(x_high[[var]]),
                "' for variable '", var, "' not found in 'combidf_high' levels. Please check your data."))
  }
}

# Derive pred_lapse for x_high
x_high$pred_lapse <- factor(ifelse(x_high$p_lapse >= 0.5, "Yes", "No"), levels = c("No", "Yes"))

cat("\nStructure of generated 'combidf_high':\n")
print(str(combidf_high))
cat("\nx_high prediction for lapse: ", as.character(x_high$pred_lapse), "\n")

# hyperbox function
box_high <- find_optimal_lapse_subset_pure(combidf_high, x_high)
save(box_high, file = "box_high")
print(box_high)


# Chapter 4.3.3: low lapse probability policy
x_low = lapses_data[175025,]
print(x_low$p_lapse)

for (col in cat_vars) {
  if (col %in% colnames(x_low) && is.character(x_low[[col]])) {
    x_low[[col]] <- factor(x_low[[col]], levels = cat_vars_levels[[col]])
  }
}

# create dataframe #function < 4b.hb
combidf_low <- generate_scenario_combinations(
  x_interest = x_low,
  cat_vars_levels = cat_vars_levels,
  annual_prem_multipliers = annual_prem_multipliers,
  ps_rate_increments = ps_rate_increments,
  model_xgb = model_xgb,
  predictors = predictors,
  selected_scenario_vars = c("prem_freq", "living_place", "annual_prem", "risk_class", "ps_rate")
)

print(nrow(combidf_low))

combidf_low$pred_lapse <- ifelse(combidf_low$p_lapse >= 0.5, "Yes", "No")
combidf_low$pred_lapse <- as.factor(combidf_low$pred_lapse) # Explicitly convert to factor
print(table(combidf_low$pred_lapse)) # Now this should correctly show factor levels

# Ensure the specified columns are treated as factors in 'combidf_low'
cat("\nConverting specified variables to factors in 'combidf_low' if they aren't already:\n")
for (var in scenario_vars) {
  if (!is.factor(combidf_low[[var]])) {
    combidf_low[[var]] <- as.factor(combidf_low[[var]])
    cat(paste0("  - Converted '", var, "' to factor.\n"))
  } else {
    cat(paste0("  - '", var, "' is already a factor.\n"))
  }
}

# Print the levels for these categorical variables
cat("\nLevels of categorical variables in 'combidf_low':\n")
for (var in scenario_vars) {
  if (length(levels(combidf_low[[var]])) > 10) {
    cat(paste0("- ", var, ": (", length(levels(combidf_low[[var]])), " levels, showing first 10) ", paste(head(levels(combidf_low[[var]]), 10), collapse = ", "), ", ...\n"))
  } else {
    cat(paste0("- ", var, ": ", paste(levels(combidf_low[[var]]), collapse = ", "), "\n"))
  }
}

x_low$ps_rate <- round(x_low$ps_rate, 4)
x_low$annual_prem <- round(x_low$annual_prem, 2)

for (var in scenario_vars) {
  # Convert numeric to character for annual_prem and ps_rate before factor conversion
  if (is.numeric(x_low[[var]])) {
    x_low[[var]] <- as.character(x_low[[var]])
  }
  x_low[[var]] <- factor(x_low[[var]], levels = levels(combidf_low[[var]]))
  # Check if the mandatory level from x_low actually exists in combidf_low levels
  if (!as.character(x_low[[var]]) %in% levels(combidf_low[[var]])) {
    stop(paste0("Error: Mandatory level '", as.character(x_low[[var]]),
                "' for variable '", var, "' not found in 'combidf_low' levels. Please check your data."))
  }
}

# Derive pred_lapse for x_low
x_low$pred_lapse <- factor(ifelse(x_low$p_lapse >= 0.5, "Yes", "No"), levels = c("No", "Yes"))

cat("\nStructure of generated 'combidf_low':\n")
print(str(combidf_low))
cat("\nx_low prediction for lapse: ", as.character(x_low$pred_lapse), "\n")

# hyperbox function: laad uit deel 6.2
box_low <- find_optimal_lapse_subset_pure(combidf_low, x_low)
save(box_low, file = "box_low")
print(box_low)
