# --- Load necessary libraries ---
library(readr)
library(ggplot2)
library(dplyr)
library(tseries)   # For ADF test
library(forecast) # For ARIMA modeling and ACF/PACF plots

# --- Configuration ---
file_path <- 'C:/Users/USER/Downloads/UNRATE.csv' # Make sure this is your unemployment data file
date_column <- 'observation_date'
value_column <- 'UNRATE'
date_format <- '%m/%d/%Y'
forecast_steps <- 12
arima_order <- c(2, 1, 1) # Good starting point for unemployment data

# --- Load and prepare data ---
tryCatch({
  df <- read_csv(file_path)

  # Print column names to verify
  print(paste("Columns in your CSV file:", paste(colnames(df), collapse = ", ")))

  df[[date_column]] <- as.Date(df[[date_column]], format = date_format)
  df <- df %>%
    arrange(!!sym(date_column)) %>%
    rename(Date = !!sym(date_column), Value = !!sym(value_column)) %>%
    filter(!is.na(Value)) %>%
    as.data.frame()
  rownames(df) <- df$Date
  df$Date <- NULL

  print("\nData loaded successfully:")
  print(paste("Time range:", min(rownames(df)), "to", max(rownames(df))))
  print(paste("Number of observations:", nrow(df)))
  print("\nFirst few rows:")
  print(head(df))

  ts_data <- ts(df$Value, frequency = 1) # Assuming monthly data, frequency = 1

}, error = function(e) {
  print(paste("Error loading data:", e$message))
  quit(status = 1)
})

# --- Stationarity Check ---
print("\n--- Stationarity Test ---")
adf_result <- adf.test(ts_data)
print(paste("ADF Statistic:", adf_result$statistic))
print(paste("p-value:", adf_result$p.value))
print("Critical Values:")
print(adf_result$critical.values)

# --- ACF/PACF Plots ---
acf(ts_data, lag.max = 30, main = "Autocorrelation Function")
pacf(ts_data, lag.max = 30, main = "Partial Autocorrelation Function")

# --- ARIMA Modeling ---
tryCatch({
  # Fit model
  model <- arima(ts_data, order = arima_order)
  print("\n--- Model Summary ---")
  print(model) # Provides coefficients and some statistics

  # Generate forecast
  forecast_result <- forecast(model, h = forecast_steps)
  forecast_values <- forecast_result$mean
  conf_int <- forecast_result$lower[, 2] # 95% lower bound
  conf_int_upper <- forecast_result$upper[, 2] # 95% upper bound

  # Create forecast index (assuming monthly data)
  last_date <- as.Date(rownames(df)[nrow(df)])
  forecast_dates <- seq(last_date, by = "month", length.out = forecast_steps + 1)[-1]

  # Create forecast DataFrame
  forecast_df <- data.frame(
    Forecast = as.numeric(forecast_values),
    Lower_CI = as.numeric(conf_int),
    Upper_CI = as.numeric(conf_int_upper),
    Date = forecast_dates
  )

  # --- Plot Results ---
  plot_combined <- ggplot() +
    geom_line(data = tail(df, 36), aes(x = as.Date(rownames(tail(df, 36))), y = Value, color = "Historical Data"), linewidth = 1) +
    geom_line(data = forecast_df, aes(x = Date, y = Forecast, color = "Forecast"), linetype = 'dashed', linewidth = 1) +
    geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower_CI, ymax = Upper_CI), fill = 'pink', alpha = 0.3) +
    labs(
      y = 'Unemployment Rate (%)',
      x = 'Date',
      title = paste('Unemployment Rate Forecast (ARIMA(', paste(arima_order, collapse = ','), '))'),
      color = "" # Legend title
    ) +
    scale_color_manual(values = c("Historical Data" = "blue", "Forecast" = "red")) +
    theme_bw()

  print(plot_combined)

  # Print forecast
  print("\n--- Forecast Results ---")
  print(round(forecast_df, 2))

  # Residual Analysis
  residuals <- residuals(model)
  residuals_df <- data.frame(Residuals = residuals, Date = as.Date(time(residuals)))

  plot_residuals <- ggplot(residuals_df, aes(x = Date, y = Residuals)) +
    geom_line() +
    labs(title = 'Model Residuals', y = 'Residuals', x = 'Date') +
    theme_bw() +
    geom_hline(yintercept = 0, linetype = 'dashed', color = 'red')
  print(plot_residuals)

}, error = function(e) {
  print(paste("\nModeling Error:", e$message))
  print("\nTroubleshooting Tips:")
  print("1. Verify your CSV file contains 'observation_date' and 'UNRATE' columns")
  print(paste("2. Try simpler ARIMA orders like c(1,1,0)"))
  print("3. Check for missing values with summary(df)")
  print("4. Ensure dates are properly parsed with head(df)")
})