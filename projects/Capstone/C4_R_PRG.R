#----------------------------------------------------------------#
# C4_R_PRG.R       S.LEVENSON                    7.26.2025      #
#----------------------------------------------------------------#

#----------------------------#
# Load Required Packages  #
#----------------------------#
packages <- c("forecast", "xts", "stats", "purrr", "ggplot2", "dplyr", "tidyr", "RColorBrewer","openxlsx2")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}
invisible(lapply(packages, library, character.only = TRUE))

#--------------------------#
# Set Working Directory #
#--------------------------#
output_path <- "F:/ECON/Capstone/Capstone_Workfile/RStudio/"
setwd(output_path)

#------------------------------#
# Load and Prepare Data      #
#------------------------------#
data <- read.csv("IN_R.csv", header = TRUE, stringsAsFactors = FALSE)
IN_R <- ts(data, start = c(1947,1), frequency = 1)

# Extract individual series into objects
for (col in colnames(IN_R)) {
  assign(col, IN_R[, col])
}

# Restrict sample to 1947–2024 for needed series
ITV_CEP <- window(ITV_CEP, start = 1947, end = 2024)
GDP_CEPR <- window(GDP_CEPR, start = 1947, end = 2024)

#------------------------------#
# Fit ARIMA Model with Exogenous Regressor #
#------------------------------#
GDP_CEPR_arima <- Arima(log(GDP_CEPR), order = c(4, 1, 0), include.drift = TRUE, xreg = ITV_CEP)
print(GDP_CEPR_arima)

# Residual diagnostics
#par(mfrow = c(1, 2))
#acf(residuals(GDP_CEPR_arima), main = "ACF of Residuals")
#pacf(residuals(GDP_CEPR_arima), main = "PACF Residuals")

#------------------------------#
# Extract Fitted Values & Forecast Out of Sample #
#------------------------------#
GDP_CEPR_AH <- exp(fitted(GDP_CEPR_arima))  # Back-transform fitted values to original scale
GDP_CEPR_OOS <- forecast(GDP_CEPR_arima, h = 5, xreg = tail(ITV_CEP, 5))
GDP_CEPR_OOS_AF <- exp(GDP_CEPR_OOS$mean)  # Back-transform forecast mean

# Combine original data and forecast for continuity
GDP_CEPR_OOS_AF <- c(GDP_CEPR, GDP_CEPR_OOS_AF)
GDP_CEPR_OOS_AF <- ts(GDP_CEPR_OOS_AF, start = c(1947, 1), frequency = 1)

#------------------------------#
# Define In-Sample Series (2015-2019) for Model Variants #
#------------------------------#
series_names <- c("SH", "VH1", "VH2", "AH", "")  # "" corresponds to original GDP_CEPR
for (suffix in series_names) {
  input_name <- if (suffix == "") "GDP_CEPR" else paste0("GDP_CEPR_", suffix)
  output_name <- paste0("GDP_CEPR_IS_", if (suffix == "") "H" else suffix)
  series <- get(input_name)
  assign(output_name, window(series, start = 2015, end = 2019))
}

#------------------------------#
# Compute Forecast Errors & Metrics (RMSE, MAPE) In-Sample #
#------------------------------#
pred_suffixes <- c("SH", "VH1", "VH2", "AH")
actual <- as.numeric(GDP_CEPR_IS_H)
years <- as.numeric(time(GDP_CEPR_IS_H))

# Collect errors
error_list <- lapply(pred_suffixes, function(suffix) {
  pred <- as.numeric(get(paste0("GDP_CEPR_IS_", suffix)))
  data.frame(
    Year = years,
    Model = suffix,
    Squared_Deviation = (pred - actual)^2,
    Absolute_Percent_Error = abs(pred - actual) / actual * 100
  )
})
error_df <- bind_rows(error_list)

# Summary metrics
metrics <- error_df %>%
  group_by(Model) %>%
  summarise(
    RMSE = sqrt(mean(Squared_Deviation, na.rm = TRUE)),
    MAPE = mean(Absolute_Percent_Error, na.rm = TRUE),
    .groups = 'drop'
  )

print(metrics)
write.csv(metrics, file.path(output_path, "GDP_CEPR_IS_Summary_Metrics.csv"), row.names = FALSE)
write.csv(error_df, file.path(output_path, "GDP_CEPR_IS_Errors.csv"), row.names = FALSE)

#------------------------------#
# Create Combination Forecasts with Various Weights #
#------------------------------#
# Assemble prediction matrix
pred_mat <- do.call(cbind, lapply(pred_suffixes, function(s) as.numeric(get(paste0("GDP_CEPR_IS_", s)))))
colnames(pred_mat) <- pred_suffixes
metrics <- metrics[match(pred_suffixes, metrics$Model), ]
# Equal Weights
equal_weights <- rep(1 / length(pred_suffixes), length(pred_suffixes))
combo_equal <- pred_mat %*% equal_weights

# Inverse-MAPE Weights and Combination Forecast
inv_mape_weights <- 1 / metrics$MAPE
inv_mape_weights <- inv_mape_weights / sum(inv_mape_weights)
combo_inv_mape <- pred_mat %*% inv_mape_weights

# Inverse MSE Weights
mse <- colMeans((pred_mat - actual)^2)
inv_mse_weights <- 1 / mse
inv_mse_weights <- inv_mse_weights / sum(inv_mse_weights)
combo_inv_mse <- pred_mat %*% inv_mse_weights

# Optimized RMSE Weights (based on penalizing RMSE share from equal weight)
rmse_shares <- metrics$RMSE / sum(metrics$RMSE)
rmse_adjustment <- 0.25 - rmse_shares
rmse_weights <- 0.25 + rmse_adjustment
combo_opt <- pred_mat %*% rmse_weights

# Combine results
combo_df <- data.frame(
  Year = years,
  Actual = actual,
  Equal = as.numeric(combo_equal),
  Inv_MAPE = as.numeric(combo_inv_mape),
  Inv_MSE = as.numeric(combo_inv_mse),
  Adj_RMSE = as.numeric(combo_opt)
)

# Prepare model names and combinations
model_names <- pred_suffixes
combination_names <- c("Equal", "Inv_MAPE", "Inv_MSE", "Adj_RMSE")

# Ensure all weight vectors have names for clarity
equal_weights_named <- setNames(equal_weights, model_names)
inv_mape_weights_named <- setNames(inv_mape_weights, model_names)
inv_mse_weights_named <- setNames(inv_mse_weights, model_names)
rmse_weights_named <- setNames(rmse_weights, model_names)

# Create a list of named vectors
weight_list_named <- list(
  Equal = equal_weights_named,
  Inv_MAPE = inv_mape_weights_named,
  Inv_MSE = inv_mse_weights_named,
  Adj_RMSE = rmse_weights_named
)

# Combine by column binding - this produces a matrix with model names as rownames
weight_df <- do.call(cbind, weight_list_named)

print(weight_df)
#------------------------------#
# Compute Metrics for Combination Forecasts #
#------------------------------#
combo_metrics <- lapply(names(combo_df)[3:6], function(name) {
  fcst <- combo_df[[name]]
  rmse <- sqrt(mean((fcst - actual)^2, na.rm = TRUE))
  mape <- mean(abs((fcst - actual) / actual), na.rm = TRUE) * 100
  data.frame(Model = name, RMSE = rmse, MAPE = mape)
}) %>% bind_rows()

print(combo_metrics)
write.csv(combo_metrics, file.path(output_path, "combo_metrics.csv"), row.names = FALSE)

#------------------------------#
# Compare Model-Based & Combination Forecasts #
#------------------------------#
model_preds <- data.frame(
  Year = time(GDP_CEPR_IS_H),
  SH = as.numeric(GDP_CEPR_IS_SH),
  VH1 = as.numeric(GDP_CEPR_IS_VH1),
  VH2 = as.numeric(GDP_CEPR_IS_VH2),
  AH = as.numeric(GDP_CEPR_IS_AH)
) %>%
  pivot_longer(cols = SH:AH, names_to = "Model", values_to = "Forecast")

#---------------------------------------------------------------#
# Generate OOS Combination Forecasts (2025–2029)                #
#---------------------------------------------------------------#
arima_oos <- forecast(GDP_CEPR_arima, h = 5, xreg = tail(ITV_CEP, 5))
GDP_CEPR_ARIMA_OOS <- as.numeric(exp(arima_oos$mean))  # Back-transform from log scale

SF_oos  <- tail(GDP_CEPR_BASE, 5)
VF1_oos <- tail(GDP_CEPR_VF1, 5)
VF2_oos <- tail(GDP_CEPR_VF2, 5)

pred_mat_oos <- cbind(SF = SF_oos, VF1 = VF1_oos, VF2 = VF2_oos, ARIMA = GDP_CEPR_ARIMA_OOS)

combo_oos_df <- data.frame(
  Year     = 2025:2029,
  SF       = SF_oos,
  VF1      = VF1_oos,
  VF2      = VF2_oos,
  ARIMA    = GDP_CEPR_ARIMA_OOS,
  Equal    = as.numeric(pred_mat_oos %*% equal_weights),
  Inv_MAPE = as.numeric(pred_mat_oos %*% inv_mape_weights), 
  Inv_MSE  = as.numeric(pred_mat_oos %*% inv_mse_weights),
  RMSE_Opt = as.numeric(pred_mat_oos %*% rmse_weights)
)

write.csv(combo_oos_df, file.path(output_path, "combo_forecasts_OOS.csv"), row.names = FALSE)

combo_oos_long <- pivot_longer(combo_oos_df, cols = -Year, names_to = "Model", values_to = "Forecast")
combo_oos_long$Type <- ifelse(combo_oos_long$Model %in% c("SF", "VF1", "VF2", "ARIMA"), "Model", "Combination")

#------------------------------#
# Growth Rate Forecasts for OOS Period (2025–2029) #
#------------------------------#
start_year <- start(GDP_CEPR)[1]
end_year_hist <- end(GDP_CEPR)[1]
years_hist <- seq(start_year, end_year_hist)
stopifnot(length(years_hist) == length(GDP_CEPR))  # sanity check

years_forecast <- 2025:2029
all_years <- c(years_hist, years_forecast)

extend_series <- function(hist_data, forecast_data) {
  c(as.numeric(hist_data), as.numeric(forecast_data))
}

GDP_combined_df <- data.frame(
  Year     = all_years,
  GDP_CEPR = extend_series(GDP_CEPR, rep(NA, length(years_forecast))),
  SF       = extend_series(GDP_CEPR, SF_oos),
  VF1      = extend_series(GDP_CEPR, VF1_oos),
  VF2      = extend_series(GDP_CEPR, VF2_oos),
  ARIMA    = extend_series(GDP_CEPR, GDP_CEPR_ARIMA_OOS),
  Equal    = extend_series(GDP_CEPR, as.numeric(pred_mat_oos %*% equal_weights)),
  Inv_MAPE = extend_series(GDP_CEPR, as.numeric(pred_mat_oos %*% inv_mape_weights)),   # Added Inv_MAPE
  Inv_MSE  = extend_series(GDP_CEPR, as.numeric(pred_mat_oos %*% inv_mse_weights)),
  RMSE_Opt = extend_series(GDP_CEPR, as.numeric(pred_mat_oos %*% rmse_weights))        # Renamed from Adj_RMSE for consistency
)

growth_df <- data.frame(Year = GDP_combined_df$Year)
for (col in names(GDP_combined_df)[-1]) {
  x <- GDP_combined_df[[col]]
  growth_df[[col]] <- c(NA, diff(x) / head(x, -1) * 100)
}

levels_df <- GDP_combined_df
names(levels_df)[-1] <- paste0(names(levels_df)[-1], "_Level")

growth_rates_df <- growth_df
names(growth_rates_df)[-1] <- paste0(names(growth_rates_df)[-1], "_Growth")

combined_df <- left_join(levels_df, growth_rates_df, by = "Year")
write.csv(combined_df, file.path(output_path, "GDP_CEPR_Levels_and_Growth.csv"), row.names = FALSE)

#------------------------------#
# In-Sample Combination-of-Combinations
#------------------------------#

# Step 1: Individual Combo Forecasts (In-Sample & OOS)
combo_fcsts_IS <- sapply(comb_weights_list, function(w) as.numeric(pred_mat %*% w))
combo_fcsts_OOS <- sapply(comb_weights_list, function(w) as.numeric(pred_mat_oos %*% w))

# Step 2: Combo of Combos Forecast (Mean of All Combos)
combo_of_combos_IS <- rowMeans(combo_fcsts_IS)
combo_of_combos_OOS <- rowMeans(combo_fcsts_OOS)

# Step 3: In-Sample Errors
combo_rmse <- sqrt(mean((combo_of_combos_IS - actual)^2, na.rm = TRUE))
combo_mape <- mean(abs((combo_of_combos_IS - actual) / actual), na.rm = TRUE) * 100

cat("\nModel: Combo_of_Combos\n")
cat("Overall RMSE:", round(combo_rmse, 4), "MAPE:", round(combo_mape, 4), "%\n")

# Step 4: Yearly Errors
combo_sq_err <- (combo_of_combos_IS - actual)^2
combo_ape <- abs((combo_of_combos_IS - actual) / actual) * 100

combo_of_combos_yearly <- data.frame(
  Year = years,
  Model = "Combo_of_Combos",
  Squared_Error = combo_sq_err,
  Absolute_Percent_Error = combo_ape
)

print(combo_of_combos_yearly)

# Store into list for merging
yearly_errors_list[["Combo_of_Combos"]] <- combo_of_combos_yearly
all_yearly_errors <- bind_rows(yearly_errors_list)

# Step 5: Add in-sample forecast levels
combo_insample_levels <- data.frame(
  Year = years,
  Actual = actual,
  Combo_of_Combos = combo_of_combos_IS
)

# Step 6: Add out-of-sample forecast levels (if years_oos available)
if (exists("years_oos") && exists("actual_oos")) {
  combo_oos_levels <- data.frame(
    Year = years_oos,
    Actual = actual_oos,
    Combo_of_Combos = combo_of_combos_OOS
  )
  
  # Optional: OOS Yearly Errors
  combo_sq_err_oos <- (combo_of_combos_OOS - actual_oos)^2
  combo_ape_oos <- abs((combo_of_combos_OOS - actual_oos) / actual_oos) * 100
  
  combo_of_combos_yearly_oos <- data.frame(
    Year = years_oos,
    Model = "Combo_of_Combos",
    Squared_Error = combo_sq_err_oos,
    Absolute_Percent_Error = combo_ape_oos
  )
}

# Step 7: Append to combo_metrics
combo_metrics <- bind_rows(
  combo_metrics,
  data.frame(
    Model = "Combo_of_Combos",
    RMSE = combo_rmse,
    MAPE = combo_mape
  )
)

# Step 8: Add to combo_oos_df
combo_oos_df$Combo_of_Combos <- combo_of_combos_OOS


#------------------------------#
# "Combo of Combos" Forecasts #
#------------------------------#

# In-Sample Combination-of-Combinations
combo_fcsts_IS <- sapply(comb_weights_list, function(w) as.numeric(pred_mat %*% w))
combo_of_combos_IS <- rowMeans(combo_fcsts_IS)

# Out-of-Sample Combination-of-Combinations
combo_fcsts_OOS <- sapply(comb_weights_list, function(w) as.numeric(pred_mat_oos %*% w))
combo_of_combos_OOS <- rowMeans(combo_fcsts_OOS)

# In-sample errors
combo_rmse <- sqrt(mean((combo_of_combos_IS - actual)^2, na.rm = TRUE))
combo_mape <- mean(abs((combo_of_combos_IS - actual) / actual), na.rm = TRUE) * 100

cat("\nModel: Combo_of_Combos\n")
cat("Overall RMSE:", round(combo_rmse, 4), "MAPE:", round(combo_mape, 4), "%\n")

# Yearly errors
combo_sq_err <- (combo_of_combos_IS - actual)^2
combo_ape <- abs((combo_of_combos_IS - actual) / actual) * 100

combo_of_combos_yearly <- data.frame(
  Year = years,
  Model = "Combo_of_Combos",
  Squared_Error = combo_sq_err,
  Absolute_Percent_Error = combo_ape
)

# Print and store
print(combo_of_combos_yearly)
yearly_errors_list[["Combo_of_Combos"]] <- combo_of_combos_yearly

all_yearly_errors <- bind_rows(yearly_errors_list)  # Now includes Combo_of_Combos
combo_oos_df$Combo_of_Combos <- combo_of_combos_OOS

#---------------------------------------------------------------#
# Append Combo_of_Combos to combined_df                         #
#---------------------------------------------------------------#

# Extend historical GDP with Combo_of_Combos forecast (2025–2029)
combo_of_combos_extended <- extend_series(GDP_CEPR, combo_of_combos_OOS)

# Calculate growth rates
combo_of_combos_growth <- c(NA, diff(combo_of_combos_extended) / head(combo_of_combos_extended, -1) * 100)

# Append to combined_df
combined_df$Combo_of_Combos_Level  <- combo_of_combos_extended
combined_df$Combo_of_Combos_Growth <- combo_of_combos_growth

# Put the combo of combos forecast into combo_df
combo_df <- combo_df %>%
  mutate(Combo_of_Combos = combo_of_combos_IS)

#------------------------------#
# Export to Excel Workbook
#------------------------------#

excel_path <- file.path(output_path, "Forecast_Outputs.xlsx")
wb <- wb_workbook()

add_or_replace <- function(wb, sheet_name, data) {
  if (sheet_name %in% names(wb)) wb <- wb_remove_worksheet(wb, sheet = sheet_name)
  wb <- wb_add_worksheet(wb, sheet_name)
  wb <- wb_add_data(wb, sheet = sheet_name, x = data)
  wb
}

wb <- add_or_replace(wb, "InSample_Errors", error_df)
wb <- add_or_replace(wb, "InSample_Summary", metrics)
wb <- add_or_replace(wb, "Combo_Metrics", combo_metrics)
wb <- add_or_replace(wb, "Combo_Forecasts_OOS", combo_oos_df)
wb <- add_or_replace(wb, "GDP_Levels_and_Growth", combined_df)
wb <- add_or_replace(wb, "Combo_InSample_Yearly_Errors", all_yearly_errors)
wb <- add_or_replace(wb, "Combo_InSample_Forecast", combo_df)

# Only if years_oos and actual_oos exist
if (exists("combo_oos_levels")) {
  wb <- add_or_replace(wb, "Combo_OutSample_Forecast", combo_oos_levels)
}
if (exists("combo_of_combos_yearly_oos")) {
  wb <- add_or_replace(wb, "Combo_OutSample_Yearly_Errors", combo_of_combos_yearly_oos)
}

wb_save(wb, file = excel_path, overwrite = TRUE)

#------------------------------#
# Save Workspace #
#------------------------------#
save.image(file = file.path(output_path, "ECON_513.RData"))

