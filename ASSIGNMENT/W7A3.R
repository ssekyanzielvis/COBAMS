# Load necessary libraries
library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)
library(Metrics) # For accuracy metrics

# --- Configuration ---
file_path <- 'C:/Users/USER/Downloads/AirPassengers.csv'   # Update with your actual file path
date_column <- 'Month'                      # Column with dates
value_column <- '#Passengers'                # Column with passenger counts
date_format <- '%Y-%m'                      # Date format in your CSV
forecast_period <- 12                       # Number of months to forecast

# --- Define clearer colors ---
training_color <- "darkblue"
actual_color <- "darkgreen"
forecast_color <- "firebrick"

# --- Load the dataset ---
tryCatch({
  df <- read_csv(file_path)
  print("Head of the DataFrame:")
  print(head(df))

  # Rename column if necessary
  if (value_column == '#Passengers' && '#Passengers' %in% names(df)) {
    names(df)[names(df) == '#Passengers'] <- "Passengers"
    value_column <- "Passengers" # Update value_column
  }

  # Convert the date column to Date objects
  df[[date_column]] <- parse_date_time(df[[date_column]], orders = date_format)
  df <- df[complete.cases(df), ]

  print("\nDataFrame with date as index (using Date column):")
  print(head(df))
  print("\nInfo of the DataFrame:")
  print(str(df))

}, error = function(e) {
  if(grepl("cannot open file", e$message)) {
    cat(paste0("Error: File not found at ", file_path, "\n"))
  } else if(grepl(paste0("Column '", date_column, "' not found"), e$message) ||
            grepl(paste0("Column '", value_column, "' not found"), e$message)) {
    cat(paste0("Error: Column not found in the CSV file. Please check the column names.\n"))
  } else {
    cat(paste0("Error: Could not parse date. Please check the 'date_format' variable. Details: ", e$message, "\n"))
  }
  quit(status = 1)
})

# --- Split data into training and testing sets ---
train_data <- df[1:(nrow(df) - forecast_period), ]
test_data <- df[(nrow(df) - forecast_period + 1):nrow(df), ]

# --- Implement Drift Method (without forecast package) ---
calculate_drift_forecast <- function(series, periods) {
  if (length(series) < 2) {
    return(rep(tail(series, 1), periods))
  }

  y <- series
  n <- length(y)
  total_change <- tail(y, 1) - y[1]
  drift <- total_change / (n - 1)

  last_value <- tail(y, 1)
  return(last_value + (1:periods) * drift)
}

# Generate forecast
drift_forecast_values <- calculate_drift_forecast(train_data[[value_column]], forecast_period)
drift_forecast_dates <- seq(
  from = max(train_data[[date_column]]) %m+% months(1),
  by = "month",
  length.out = forecast_period
)
drift_forecast_series <- data.frame(
  Date = drift_forecast_dates,
  Forecast = drift_forecast_values
)

# --- Calculate forecast accuracy metrics ---
mse_drift <- mse(test_data[[value_column]], drift_forecast_values)
rmse_drift <- rmse(test_data[[value_column]], drift_forecast_values)
mape_drift <- mape(test_data[[value_column]], drift_forecast_values)

cat("\n--- Forecast Accuracy Metrics (Drift Method) ---\n")
cat(paste0("Mean Squared Error (MSE): ", round(mse_drift, 2), "\n"))
cat(paste0("Root Mean Squared Error (RMSE): ", round(rmse_drift, 2), "\n"))
cat(paste0("Mean Absolute Percentage Error (MAPE): ", round(mape_drift, 2), "%\n"))

# --- Plot the results ---
drift_plot <- ggplot() +
  geom_line(data = train_data, aes(x = .data[[date_column]], y = .data[[value_column]], color = "Training Data"), linewidth = 1) +
  geom_point(data = train_data, aes(x = .data[[date_column]], y = .data[[value_column]], color = "Training Data"), size = 2) +
  geom_line(data = test_data, aes(x = .data[[date_column]], y = .data[[value_column]], color = "Actual Test Data"), linewidth = 1) +
  geom_point(data = test_data, aes(x = .data[[date_column]], y = .data[[value_column]], color = "Actual Test Data"), size = 2) +
  geom_line(data = drift_forecast_series, aes(x = Date, y = Forecast, color = "Drift Forecast"), linetype = "dashed", linewidth = 1.2) +
  geom_point(data = drift_forecast_series, aes(x = Date, y = Forecast, color = "Drift Forecast"), shape = 4, size = 3) +
  labs(
    title = "Airline Passengers Data with Drift Forecast",
    x = "Year-Month",
    y = "Number of Passengers",
    color = "Legend"
  ) +
  scale_color_manual(
    values = c(
      "Training Data" = training_color,
      "Actual Test Data" = actual_color,
      "Drift Forecast" = forecast_color
    )
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 10))

# Make the plot visible
print(drift_plot)

# Save the plot
ggsave("drift_forecast_plot_clear.png", plot = drift_plot, width = 12, height = 6, units = "in")

# --- Additional Analysis ---
cat("\n--- Drift Calculation Details ---\n")
cat(paste0("First value: ", train_data[[value_column]][1], "\n"))
cat(paste0("Last value: ", tail(train_data[[value_column]], 1), "\n"))
cat(paste0("Total change: ", tail(train_data[[value_column]], 1) - train_data[[value_column]][1], "\n"))
cat(paste0("Number of periods in training data: ", nrow(train_data), "\n"))
cat(paste0("Calculated drift per period: ",
            round((tail(train_data[[value_column]], 1) - train_data[[value_column]][1]) / (nrow(train_data) - 1), 2), "\n"))