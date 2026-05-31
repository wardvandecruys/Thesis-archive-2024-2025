################################################################################
# C3 — Modeling Pipeline for Lapse Classification
#
# Overview
# - Fits four models with the same resampling setup:
#   GLMNet (logistic), XGBoost, Neural Network (nnet), Random Forest (ranger).
# - Repeated 10-fold CV (3 repeats) with random search (tuneLength = 100).
# - Optimizes F1 (positive class = "Yes"); probabilities enabled.
# - Parallelized via doParallel using (cores - 1).
#
# Data & Preprocessing
# - Input: `lapses_data_beta` (expects `surrenders` as factor with levels "No"/"Yes";
#   `ageph` and `fund` cast to factors).
# - Split: 70% train / 30% test (stratified).
# - Imputation: numeric predictors in a defined list imputed by the TRAIN median
#   (applied to both train and test) to prevent leakage.
# - Optional small-set scenario: activate the `vars_to_exclude` block to drop a
#   predefined set of variables, then retrain/evaluate.
#
# Evaluation
# - On the held-out test set: Accuracy, Precision, Recall, F1, ROC-AUC, PR-AUC.
# - Plots: combined ROC and Precision-Recall curves.
#
# Interpretation (XGBoost)
# - Normalized feature importance (sums to 100%).
# - PDP + up to 100 ICE curves per predictor.
# - Patchwork grid of PDP+ICE across all predictors for a concise overview.
#
# Outputs
# - Saved models: GLMNet, XGBoost, Neural Net, Random Forest (RDS files).
# - Figures: variable importance PNG, per-feature PDP+ICE PNGs, and a patchwork grid.
#
# Reproducibility
# - `set.seed(123)`; deterministic preprocessing; consistent CV settings.
################################################################################




library(caret)
library(pROC)      # For ROC curve and AUC
library(PRROC)     # For Precision-Recall curve and PR-AUC
library(e1071)     # Dependency for confusionMatrix
library(randomForest) # For Random Forest model
library(xgboost)   # For XGBoost model
library(nnet)      # For Neural Network model
library(ggplot2)   # For plotting
library(pdp)       # For Partial Dependence Plots and ICE plots
library(doParallel)
library(MLmetrics)
library(dplyr)
library(patchwork)
library(scales)

setwd("D:/R Run")
load('lapses_data_beta')

# ==============================================================================
# 1. Data Preparation
# ==============================================================================

set.seed(123)

#--------------------VARIABLE EXCLUSION FOR SMALL SET SCENARIO------------------------
# vars_to_exclude <- c("policy_id", "data_year", "ps_lag1", "ps_lag2",
#                      "avg_ps", "change_10", "change_21",
#                      "mean_01", "mean_12", "vol_012", "mean_012", "fund")
#
# # Create extra_lapse by excluding them
# lapses_data <- lapses_data[, !(names(lapses_data) %in% vars_to_exclude)]

# Ensure 'surrenders' is a factor
lapses_data$surrenders <- as.factor(lapses_data$surrenders)

# Rename factor levels to valid R variable names ("No", "Yes")
# Assuming original levels were "FALSE" and "TRUE"
levels(lapses_data$surrenders)[levels(lapses_data$surrenders) == "FALSE"] <- "No"
levels(lapses_data$surrenders)[levels(lapses_data$surrenders) == "TRUE"] <- "Yes"

# Convert character columns to factors
lapses_data$ageph <- as.factor(lapses_data$ageph)
lapses_data$fund <- as.factor(lapses_data$fund)

# Create stratified split
trainIndex <- createDataPartition(lapses_data$surrenders, p = .7, list = FALSE, times = 1)
training_data <- lapses_data[trainIndex, ]
testing_data  <- lapses_data[-trainIndex, ]


# This is crucial to prevent data leakage from the test set.
imputation_cols <- c("ps_lag1", "ps_lag2", "avg_ps", "change_10", "change_21", "mean_01", "mean_12", "vol_012", "mean_012")

for (col in imputation_cols) {
  if (col %in% colnames(training_data)) {
    median_val <- median(training_data[[col]], na.rm = TRUE)
    training_data[[col]][is.na(training_data[[col]])] <- median_val
    testing_data[[col]][is.na(testing_data[[col]])] <- median_val # Use training median for test data
  }
}

# Define predictors and formula
predictors <- setdiff(names(training_data), c("policy_id", "data_year", "surrenders"))
formula_all_predictors <- as.formula(paste("surrenders ~ .", collapse = " "))

# model_xgb <- readRDS("model_RDS_xgb_downsamp_bigdata.rds")


# ==============================================================================
# 2. Control: Repeated Cross-Validation, Random Search and Down Sampling
# ==============================================================================

# Detect number of cores to use parallel computing
num_cores <- parallel::detectCores() - 1

# Register parallel backend
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)
clusterEvalQ(cl, library(MLmetrics))

# Confirm registration
getDoParWorkers() # Should return the number of cores used

# Define prSummary to return Precision, Recall, F1
prSummary <- function(data, lev = NULL, model = NULL) {
  precision <- tryCatch(MLmetrics::Precision(y_true = data$obs, y_pred = data$pred, positive = "Yes"), error = function(e) NA)
  recall <- tryCatch(MLmetrics::Recall(y_true = data$obs, y_pred = data$pred, positive = "Yes"), error = function(e) NA)

  if (is.na(precision) || is.na(recall) || (precision + recall) == 0) {
    f1 <- 0
  } else {
    f1 <- 2 * precision * recall / (precision + recall)
  }

  out <- c(Precision = precision, Recall = recall, F1 = f1)
  return(out)
}


fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = prSummary,
  savePredictions = "final",
  search = "random",
  allowParallel = TRUE
)

# ==============================================================================
# 3. Model Training
# ==============================================================================

# 3.1. GLMNet Model
message("Training GLMNet Model...")
model_glm <- train(
  formula_all_predictors,
  data = training_data[, c(predictors, "surrenders")],
  method = "glmnet",
  family = "binomial", # For logistic regression
  trControl = fitControl,
  preProcess = c("center", "scale"),
  metric = "F1", # Optimize for AUC
  tuneLength = 100
)
print(model_glm)

saveRDS(model_glm, "model_RDS_glm_bigdata.rds")

# 3.2. XGBoost Model
message("\nTraining XGBoost Model...")
model_xgb <- train(
  formula_all_predictors,
  data = training_data[, c(predictors, "surrenders")],
  method = "xgbTree",
  trControl = fitControl,
  metric = "F1",
  tuneLength = 100 # Number of random combinations to try
)
print(model_xgb)

saveRDS(model_xgb, "model_RDS_xgb_small.rds")

# 3.3. Neural Network Model
message("\nTraining Neural Network Model...")
model_nnet <- train(
  formula_all_predictors,
  data = training_data[, c(predictors, "surrenders")],
  method = "nnet",
  trControl = fitControl,
  metric = "F1",
  preProcess = c("center", "scale"),
  tuneLength = 100, # Number of random combinations to try
  maxit = 200,
  trace = FALSE # Suppress verbose output during training
)
print(model_nnet)

saveRDS(model_nnet, "model_RDS_nnet_bigdata.rds")

# 3.4. Random Forest Model
message("\nTraining Random Forest Model...")
model_rf <- train(
  formula_all_predictors,
  data = training_data[, c(predictors, "surrenders")],
  method = "ranger",
  trControl = fitControl,
  metric = "F1",
  maximize = TRUE,
  num.trees = 1000,
  importance = "impurity",
  tuneLength = 100 # Number of random mtry values to try
)
print(model_rf)

saveRDS(model_rf, "model_RDS_rf_bigdata.rds")

stopCluster(cl)
registerDoSEQ()

# ==============================================================================
# 4. Performance Evaluation and Comparison
# ==============================================================================

# Function to get predictions and calculate common metrics
get_model_performance <- function(model, test_data, predictors, true_labels, model_name) {
  # Predict classes
  predictions <- predict(model, newdata = test_data[, predictors], type = "raw")
  # Predict probabilities for the positive class ("Yes")
  probabilities <- predict(model, newdata = test_data[, predictors], type = "prob")$Yes

  # Ensure factor levels are consistent for confusionMatrix
  predictions <- factor(predictions, levels = levels(true_labels))

  # Confusion Matrix
  cm <- confusionMatrix(predictions, true_labels, positive = "Yes")

  # Extract metrics
  accuracy <- cm$overall["Accuracy"]
  precision <- cm$byClass["Pos Pred Value"]
  recall <- cm$byClass["Sensitivity"]
  f1_score <- cm$byClass["F1"]

  # ROC AUC
  roc_obj <- roc(response = true_labels, predictor = probabilities)
  auc_roc <- auc(roc_obj)

  # PR AUC (convert true_labels to 0/1 for PRROC)
  true_labels_numeric <- ifelse(true_labels == "Yes", 1, 0)
  pr_curve_obj <- pr.curve(scores.class0 = probabilities, weights.class0 = true_labels_numeric, curve = TRUE)
  auc_pr <- pr_curve_obj$auc.integral

  # Return results in a list
  list(
    Model = model_name,
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1_Score = f1_score,
    AUC_ROC = auc_roc,
    AUC_PR = auc_pr,
    ROC_Object = roc_obj,
    PR_Curve_Data = pr_curve_obj$curve # Store curve data for plotting
  )
}

# Store results for all models
all_model_results <- list()
roc_curve_objects <- list()
pr_curve_data_list <- list()

#Get performance for each model
perf_glm <- get_model_performance(model_glm, testing_data, predictors, testing_data$surrenders, "GLMNet")
all_model_results[[perf_glm$Model]] <- perf_glm
roc_curve_objects[[perf_glm$Model]] <- perf_glm$ROC_Object
pr_curve_data_list[[perf_glm$Model]] <- perf_glm$PR_Curve_Data

perf_rf <- get_model_performance(model_rf, testing_data, predictors, testing_data$surrenders, "Random Forest")
all_model_results[[perf_rf$Model]] <- perf_rf
roc_curve_objects[[perf_rf$Model]] <- perf_rf$ROC_Object
pr_curve_data_list[[perf_rf$Model]] <- perf_rf$PR_Curve_Data

perf_xgb <- get_model_performance(model_xgb, testing_data, predictors, testing_data$surrenders, "XGBoost")
all_model_results[[perf_xgb$Model]] <- perf_xgb
roc_curve_objects[[perf_xgb$Model]] <- perf_xgb$ROC_Object
pr_curve_data_list[[perf_xgb$Model]] <- perf_xgb$PR_Curve_Data

perf_nnet <- get_model_performance(model_nnet, testing_data, predictors, testing_data$surrenders, "Neural Network")
all_model_results[[perf_nnet$Model]] <- perf_nnet
roc_curve_objects[[perf_nnet$Model]] <- perf_nnet$ROC_Object
pr_curve_data_list[[perf_nnet$Model]] <- perf_nnet$PR_Curve_Data

# Create a summary data frame for easy comparison
comparison_df <- do.call(rbind, lapply(all_model_results, function(x) {
  data.frame(
    Model = x$Model,
    Accuracy = x$Accuracy,
    Precision = x$Precision,
    Recall = x$Recall,
    F1_Score = x$F1_Score,
    AUC_ROC = x$AUC_ROC,
    AUC_PR = x$AUC_PR
  )
}))

message("\n--- Model Performance Comparison (on Test Data) ---")
print(comparison_df)

# ==============================================================================
# 5. Performance Visualization
# ==============================================================================
# Define the prediction wrapper function for caret models (returns P(Yes))
predict_prob_caret <- function(object, newdata) {
  predict(object, newdata = newdata, type = "prob")$Yes
}

# This assumes model_glm, model_rf, model_xgb, model_nnet are ALREADY IN YOUR R ENVIRONMENT
current_models_in_env <- list(
  XGBoost = model_xgb
)

# --- 6.3.xgboost Variable Importance (Normalized & Styled) ---
# Extract unscaled variable importance
importance_raw <- varImp(model_xgb, scale = FALSE)
imp_df <- as.data.frame(importance_raw$importance)
imp_df$Predictor <- rownames(imp_df)

# Normalize to sum to 100
imp_df$Importance <- (imp_df$Overall / sum(imp_df$Overall)) * 100

# Order by importance (descending) and set levels so top feature appears at the top
imp_df <- imp_df[order(imp_df$Importance, decreasing = TRUE), ]
imp_df$Predictor <- factor(imp_df$Predictor, levels = rev(imp_df$Predictor))  # REVERSED

# Define KU Leuven blue
kuleuven_blue <- "#116EAC"

# Plot
ggplot(imp_df, aes(x = Predictor, y = Importance)) +
  geom_bar(stat = "identity", fill = kuleuven_blue) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(scale = 1), expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Normalized Feature Importance – XGBoost",
    x = "Feature",
    y = "Importance (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    axis.title = element_text(size = 13),
    panel.grid.major.y = element_blank()
  )

# Sum of normalized importances (should be ~100)
total_importance <- sum(imp_df$Importance)

# Print with validation message
if (abs(total_importance - 100) < 1e-6) {
  message(sprintf("✅ Total importance sums to: %.6f %% (OK)", total_importance))
} else {
  warning(sprintf("⚠️ Total importance is: %.6f %% (Check normalization)", total_importance))
}

# Optional: Save to file
ggsave("xgboost_varimp_kuleuventesr.png", width = 10, height = 8, dpi = 300)


selected_vars_pdp_ice <- predictors
#--- PDP + ICE single image ---

library(pdp)
library(ggplot2)
library(dplyr)

ku_leuven_blue <- "#116EAC"

for (var in selected_vars_pdp_ice) {
  message(paste0("  Generating PDP+ICE for: ", var))

  if (!var %in% names(training_data)) {
    message(paste0("  Skipping ", var, ": not found in training data."))
    next
  }

  tryCatch({
    pdp_obj <- pdp::partial(
      object = model_xgb,
      pred.var = var,
      pred.fun = predict_prob_caret,
      train = training_data[, predictors, drop = FALSE],
      ice = TRUE,
      center = FALSE,
      plot = FALSE
    )

    # Determine if variable is numeric or categorical
    is_numeric <- is.numeric(training_data[[var]])

    # Extract ICE data
    ice_data <- pdp_obj[!is.null(pdp_obj$yhat.id), ]

    # Ensure categorical variables are treated as factors
    if (!is_numeric) {
      ice_data[[var]] <- as.factor(ice_data[[var]])
    }

    # Compute PDP: average of ICE lines at each unique value of var
    pdp_data <- ice_data %>%
      group_by_at(var) %>%
      summarise(yhat = mean(yhat), .groups = "drop")

    # Rename first column back to var name if needed
    names(pdp_data)[1] <- var

    # Subsample ICE curves if too many
    if (length(unique(ice_data$yhat.id)) > 100) {
      sampled_ids <- sample(unique(ice_data$yhat.id), 100)
      ice_data <- ice_data[ice_data$yhat.id %in% sampled_ids, ]
    }

    # Plotting: numeric and categorical handled differently
    if (is_numeric) {
      pdp_ice_plot <- ggplot() +
        geom_line(data = ice_data, aes_string(x = var, y = "yhat", group = "yhat.id"),
                  color = "grey80", alpha = 0.6) +
        geom_line(data = pdp_data, aes_string(x = var, y = "yhat"),
                  color = ku_leuven_blue, size = 1.1)
    } else {
      pdp_ice_plot <- ggplot() +
        geom_jitter(data = ice_data, aes_string(x = var, y = "yhat"),
                    color = "grey80", alpha = 0.6, width = 0.2, height = 0) +
        geom_point(data = pdp_data, aes_string(x = var, y = "yhat"),
                   color = ku_leuven_blue, size = 3) +
        geom_line(data = pdp_data, aes_string(x = var, y = "yhat", group = "1"),
                  color = ku_leuven_blue, size = 1.1)
    }

    # Finalize plot
    pdp_ice_plot <- pdp_ice_plot +
      labs(
        title = paste0("PDP with 100 ICE samples for ", var, " (XGBoost)"),
        x = var,
        y = "Partial Dependence (P(Surrender = Yes))"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 10)
      )

    print(pdp_ice_plot)

    # Save to file
    ggsave(
      filename = paste0("pdp_ice_xgb_GOOD_", var, ".png"),
      plot = pdp_ice_plot,
      width = 8,
      height = 6,
      dpi = 300
    )

  }, error = function(e) {
    message(paste0("  Skipping '", var, "' due to error: ", e$message))
  })
}

#--- PDP + ICE Plots in Grid ---

ku_leuven_blue <- "#116EAC"

# Container for all plots
all_plots <- list()

for (var in selected_vars_pdp_ice) {
  message(paste0("  Generating PDP+ICE for: ", var))

  if (!var %in% names(training_data)) {
    message(paste0("  Skipping ", var, ": not found in training data."))
    next
  }

  tryCatch({
    pdp_obj <- pdp::partial(
      object = model_xgb,
      pred.var = var,
      pred.fun = predict_prob_caret,
      train = training_data[, predictors, drop = FALSE],
      ice = TRUE,
      center = FALSE,
      plot = FALSE
    )

    # Determine if variable is numeric
    is_numeric <- is.numeric(training_data[[var]])

    # Extract ICE data
    ice_data <- pdp_obj[!is.null(pdp_obj$yhat.id), ]

    # Treat categorical as factor
    if (!is_numeric) {
      ice_data[[var]] <- as.factor(ice_data[[var]])
    }

    # PDP = average of ICE lines
    pdp_data <- ice_data %>%
      group_by_at(var) %>%
      summarise(yhat = mean(yhat), .groups = "drop")

    names(pdp_data)[1] <- var  # ensure correct naming

    # Subsample ICE lines to max 100
    if (length(unique(ice_data$yhat.id)) > 100) {
      sampled_ids <- sample(unique(ice_data$yhat.id), 100)
      ice_data <- ice_data[ice_data$yhat.id %in% sampled_ids, ]
    }

    # Plot PDP + ICE
    if (is_numeric) {
      pdp_ice_plot <- ggplot() +
        geom_line(data = ice_data, aes_string(x = var, y = "yhat", group = "yhat.id"),
                  color = "grey80", alpha = 0.6) +
        geom_line(data = pdp_data, aes_string(x = var, y = "yhat"),
                  color = ku_leuven_blue, size = 1.1)
    } else {
      pdp_ice_plot <- ggplot() +
        geom_jitter(data = ice_data, aes_string(x = var, y = "yhat"),
                    color = "grey80", alpha = 0.6, width = 0.2, height = 0) +
        geom_point(data = pdp_data, aes_string(x = var, y = "yhat"),
                   color = ku_leuven_blue, size = 3) +
        geom_line(data = pdp_data, aes_string(x = var, y = "yhat", group = "1"),
                  color = ku_leuven_blue, size = 1.1)
    }

    # Final styling
    pdp_ice_plot <- pdp_ice_plot +
      labs(
        title = var,
        x = var,
        y = "P(Surrender = Yes)"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9)
      )

    # Store for grid
    all_plots[[var]] <- pdp_ice_plot

  }, error = function(e) {
    message(paste0("  Skipping '", var, "' due to error: ", e$message))
  })
}

# Combine all plots into a patchwork grid
combined_plot <- wrap_plots(all_plots, ncol = 3) +
  plot_annotation(
    title = "Partial Dependence + ICE Plots for All Predictors (XGBoost)",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    )
  )

# Save the grid to a high-resolution PNG
ggsave(
  filename = "pdp_ice_grid_academic.png",
  plot = combined_plot,
  width = 16, height = 20, dpi = 300
)

