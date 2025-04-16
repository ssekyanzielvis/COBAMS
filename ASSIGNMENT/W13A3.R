# --- Load necessary libraries ---
library(readr)
library(ggplot2)
library(dplyr)
library(tseries)   # For ADF test
library(forecast) # For auto.arima and plotting ACF/PACF

# --- Configuration ---
file_path <- 'C:/Users/USER/Downloads/AirPassengers.csv'
date_column <- 'Month'
value_column <- '#Passengers'
date_format <- '%Y-%m'
seasonal_period <- 12
forecast_steps <- 12
sarima_order <- c(1, 1, 1)
seasonal_order <- c(1, 1, 1) # R's ARIMA doesn't have a direct seasonal order argument like SARIMAX
                               # We'll rely on auto.arima or specify in arima()

# --- Load and prepare data ---
tryCatch({
  df <- read_csv(file_path)
  print(paste("Columns in dataset:", paste(colnames(df), collapse = ", ")))

  # Convert date and set index
  df$Date <- as.Date(paste0(df[[date_column]], "-01"), format = "%Y-%m-%d")
  df <- df %>%
    arrange(Date) %>%
    rename(Passengers = !!sym(value_column)) %>%
    select(Date, Passengers)

  # Handle missing/infinite values (though unlikely in this dataset)
  df <- df %>%
    filter(!is.na(Passengers) & is.finite(Passengers))

  print("\nFirst 5 rows:")
  print(head(df))
  print("\nData summary:")
  print(summary(df$Passengers))

  ts_data <- ts(df$Passengers, frequency = seasonal_period)

}, error = function(e) {
  print(paste("Error loading data:", e$message))
  quit(status = 1)
})

# --- Exploratory Analysis ---
ggplot(df, aes(x = Date, y = Passengers)) +
  geom_line() +
  labs(title = 'Monthly Airline Passengers', y = 'Number of Passengers', x = 'Month') +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "grey", linetype = "dashed")) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "2 years") # Adjust breaks as needed

# --- Enhanced Stationarity Test ---
test_stationarity <- function(series, title = "Data") {
  series <- series[is.finite(series) & !is.na(series)]

  if (length(series) < 2) {
    print(paste("Cannot test stationarity - not enough data points for", title))
    return(NULL)
  }

  print(paste("\nStationarity Test -", title, ":"))
  tryCatch({
    result <- adf.test(series)
    print(paste('ADF Statistic:', result$statistic))
    print(paste('p-value:', result$p.value))
    print('Critical Values:')
    print(result$critical.values)
  }, error = function(e) {
    print(paste("Error in stationarity test:", e$message))
  })
}

# Test original data
test_stationarity(ts_data, "Original Data")

# Create and test differenced series
diff_ts_data <- diff(ts_data)
test_stationarity(diff_ts_data, "First Difference")

# --- ACF/PACF Analysis ---
acf(diff_ts_data, lag.max = 24, main = 'ACF of Differenced Data')
pacf(diff_ts_data, lag.max = 24, main = 'PACF of Differenced Data')

# --- SARIMA Modeling (using auto.arima for simplicity in R) ---
tryCatch({
  # Fit model using auto.arima (can handle seasonality)
  model <- auto.arima(ts_data,
                      seasonal = TRUE, # Let auto.arima consider seasonality
                      stepwise = FALSE, # More thorough search
                      trace = TRUE,    # Show model selection process
                      approximation = FALSE # Use exact likelihood
                      )

  print("\nModel Summary:")
  print(summary(model))

  # Generate forecast
  forecast_result <- forecast(model, h = forecast_steps)
  forecast_values <- forecast_result$mean
  conf_int <- forecast_result$lower[, 2] # 95% lower bound
  conf_int_upper <- forecast_result$upper[, 2] # 95% upper bound

  # Create forecast dates
  last_date <- as.Date(df$Date[nrow(df)])
  forecast_dates <- seq(last_date, by = "month", length.out = forecast_steps + 1)[-1]

  # --- Plot Results ---
  plot_df <- data.frame(
    Date = c(tail(df$Date, 24), forecast_dates),
    Passengers = c(tail(df$Passengers, 24), forecast_values),
    Type = c(rep("Historical Data", 24), rep("Forecast", forecast_steps))
  )

  forecast_ci_df <- data.frame(
    Date = forecast_dates,
    Lower = conf_int,
    Upper = conf_int_upper
  )

  ggplot(plot_df, aes(x = Date, y = Passengers, color = Type)) +
    geom_line(linewidth = 1) +
    geom_point(data = subset(plot_df, Type == "Historical Data"), size = 2) +
    geom_point(data = subset(plot_df, Type == "Forecast"), shape = 4, size = 2) + # Using 'x' for forecast points
    geom_ribbon(data = forecast_ci_df, aes(ymin = Lower, ymax = Upper, fill = "95% CI"), alpha = 0.3, color = NA) +
    scale_color_manual(values = c("Historical Data" = "blue", "Forecast" = "red")) +
    scale_fill_manual(name = "", values = c("95% CI" = "pink")) +
    labs(title = 'SARIMA Forecast for Airline Passengers (auto.arima)',
         x = 'Date',
         y = 'Passengers',
         color = '') +
    theme_bw() +
    theme(legend.position = "top") +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "2 years")

  # Print forecast table
  forecast_table <- data.frame(
    Date = forecast_dates,
    Forecast = round(forecast_values),
    Lower_Bound = round(conf_int),
    Upper_Bound = round(conf_int_upper)
  )
  print("\nForecasted Passengers:")
  print(forecast_table)

}, error = function(e) {
  print(paste("\nModeling Error:", e$message))
  print("\nTroubleshooting Tips:")
  print("1. Check for NaN/inf values in your data")
  print("2. Consider the ACF/PACF plots to manually choose ARIMA orders")
  print("3. Consider seasonal decomposition first using `decompose()` or `stl()`")
  print("4. Verify your data has enough observations (at least 2-3 seasonal cycles)")
})