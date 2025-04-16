# --- Load necessary libraries ---
library(readr)
library(dplyr)
library(ggplot2)
library(randomForest)
library(caret) # For train/test split and metrics
library(zoo) # For lag function

# --- Configuration ---
file_path <- 'C:/Users/USER/Downloads/CPIAUCSL.csv' # Replace with the actual path to your CSV file
date_column <- 'observation_date' # Replace with your date column name
price_column <- 'CPIAUCSL' # Replace with your stock price column name
n_estimators <- 100 # Number of trees in the Random Forest
test_size <- 0.2 # Proportion of data for testing
random_state <- 42 # For reproducibility
forecast_days <- 30 # Number of future days to forecast
lags <- 5 # Using the past 5 days' returns as features
threshold <- 3 # Z-score threshold for outlier removal

# --- Load the dataset ---
tryCatch({
  df <- read_csv(file_path)
  print("Head of the DataFrame:")
  print(head(df))

  # Convert date column and set as index
  df <- df %>%
    mutate(!!sym(date_column) := as.Date(!!sym(date_column))) %>%
    arrange(!!sym(date_column)) %>%
    rename(Date = !!sym(date_column), Price = !!sym(price_column))

  print("\nDataFrame with date as index:")
  print(head(df))
  print("\nInfo of the DataFrame:")
  print(str(df))

}, error = function(e) {
  if (grepl("cannot open the connection", e$message)) {
    print(paste("Error: File not found at", file_path))
  } else if (grepl(paste0("Column '", date_column, "' not found"), e$message) ||
             grepl(paste0("Column '", price_column, "' not found"), e$message)) {
    print(paste("Error: Column not found. Please check the column names."))
  } else if (grepl("invalid 'type' \\(character\\) of argument", e$message)) {
    print("Error: Could not parse date. Please check the date format.")
  } else {
    print(paste("Error loading data:", e$message))
  }
  quit(status = 1)
})

# --- Data Preparation ---
if (nrow(df) > 0) {
  # 1. Calculate Daily Returns
  df <- df %>%
    mutate(Daily_Return = (Price - lag(Price)) / lag(Price))

  # 2. Handle Missing Values (filling with the mean or dropping)
  mean_return <- mean(df$Daily_Return, na.rm = TRUE)
  df <- df %>%
    mutate(Daily_Return = ifelse(is.na(Daily_Return), mean_return, Daily_Return))

  # 3. Handle Outliers (simple Z-score method)
  z_scores <- abs((df$Daily_Return - mean(df$Daily_Return)) / sd(df$Daily_Return))
  df_no_outliers <- df %>%
    filter(z_scores < threshold)

  print("\nDataFrame after data preparation:")
  print(head(df_no_outliers))

  # --- Feature Engineering (Simple Lagged Features) ---
  for (i in 1:lags) {
    df_no_outliers <- df_no_outliers %>%
      mutate(!!paste0('Return_Lag_', i) := lag(Daily_Return, n = i))
  }
  df_no_outliers <- na.omit(df_no_outliers)

  # --- Prepare data for Machine Learning ---
  X <- df_no_outliers %>%
    select(starts_with('Return_Lag'))
  y <- df_no_outliers$Price

  # Split data into training and testing sets
  train_index <- createDataPartition(y, p = (1 - test_size), list = FALSE, times = 1)
  X_train <- X[train_index,]
  X_test <- X[-train_index,]
  y_train <- y[train_index]
  y_test <- y[-train_index]

  # Scale features
  scaler <- scale(X_train)
  X_train_scaled <- scale(X_train, center = attr(scaler, "scaled:center"), scale = attr(scaler, "scaled:scale"))
  X_test_scaled <- scale(X_test, center = attr(scaler, "scaled:center"), scale = attr(scaler, "scaled:scale"))

  # --- Fit a Machine Learning Model (Random Forest) ---
  model <- randomForest(x = X_train_scaled, y = y_train, ntree = n_estimators, importance = TRUE)

  # Evaluate the model
  y_pred <- predict(model, newdata = X_test_scaled)
  mse <- mean((y_test - y_pred)^2)
  print(paste("\nMean Squared Error on Test Set:", sprintf("%.4f", mse)))

  # --- Forecast Future Stock Prices ---
  last_known_returns <- tail(df_no_outliers$Daily_Return, lags)
  future_forecasts <- numeric(forecast_days)

  last_price <- tail(df_no_outliers$Price, 1)

  for (i in 1:forecast_days) {
    if (length(last_known_returns) < lags) {
      future_forecasts[i] <- NA
      break
    }

    future_features <- matrix(last_known_returns, nrow = 1)
    future_features_scaled <- scale(future_features, center = attr(scaler, "scaled:center"), scale = attr(scaler, "scaled:scale"))

    next_price_pred <- predict(model, newdata = future_features_scaled)[1] # Access the single prediction

    future_forecasts[i] <- next_price_pred

    # Update the last known returns for the next forecast (using predicted price return)
    next_return <- (next_price_pred / last_price) - 1
    last_known_returns <- c(tail(last_known_returns, lags - 1), next_return)
    last_price <- next_price_pred
  }

  # Create a date index for the forecast
  last_date <- tail(df_no_outliers$Date, 1)
  future_dates <- seq(last_date + 1, by = "day", length.out = forecast_days)
  forecast_df <- data.frame(Date = future_dates, Forecasted_Price = future_forecasts)

  print("\nFuture Stock Price Forecast:")
  print(forecast_df)

  # --- Plotting ---
  plot_df <- df_no_outliers %>%
    select(Date, Price) %>%
    mutate(Type = 'Actual Price') %>%
    bind_rows(forecast_df %>% rename(Price = Forecasted_Price) %>% mutate(Type = 'Forecasted Price'))

  print(ggplot(plot_df, aes(x = Date, y = Price, color = Type)) +
    geom_line() +
    labs(title = 'Stock Price Prediction using Random Forest',
         x = 'Date',
         y = 'Stock Price') +
    scale_color_manual(values = c('Actual Price' = 'blue', 'Forecasted Price' = 'red')) +
    theme_bw() +
    theme(legend.position = 'top') +
    geom_vline(xintercept = tail(df_no_outliers$Date, 1), linetype = 'dashed', color = 'grey') +
    geom_vline(xintercept = head(forecast_df$Date, 1), linetype = 'dashed', color = 'grey'))

  print(ggplot(df_no_outliers, aes(x = Daily_Return)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = 'skyblue', color = 'black') +
    geom_density(alpha = 0.2, fill = 'lightblue') +
    labs(title = 'Distribution of Daily Returns (Outliers Removed)',
         x = 'Daily Return',
         y = 'Density') +
    theme_bw())

} else {
  print("DataFrame is empty, cannot perform financial market forecasting.")
}