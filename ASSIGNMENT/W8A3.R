# --- Load necessary libraries ---
library(readr)
library(ggplot2)
library(dplyr)
library(forecast) # For time series decomposition
library(zoo)      # For rolling calculations
library(gridExtra) # For arranging multiple plots

# --- Configuration ---
file_path <- 'C:/Users/USER/Downloads/MEC.csv'
utility_type <- 'Electricity'  # Options: 'Electricity', 'Water', 'Gas', 'Passengers'
owner_type <- 'Tenant'         # Options: 'Tenant', 'Commission'
frequency <- 12                # Monthly data with yearly seasonality

# --- Load and prepare data ---
tryCatch({
  # Read data
  df <- read_csv(file_path)
  print("Initial data sample:")
  print(head(df, 3))

  # Filter for selected utility and owner
  df_filtered <- df %>%
    filter(Utility == utility_type, Owner == owner_type)

  if (nrow(df_filtered) == 0) {
    stop(paste("No data found for", utility_type, "-", owner_type))
  }

  # Create proper datetime index (using first day of each month)
  df_prepared <- df_filtered %>%
    mutate(
      Date = as.Date(paste(Year, `Month Number`, "01", sep = "-"))
    ) %>%
    arrange(Date) %>%
    select(Date, Usage) %>%
    as.data.frame()
  rownames(df_prepared) <- df_prepared$Date
  df_prepared$Date <- NULL

  # Convert Usage to numeric
  df_prepared$Usage <- as.numeric(df_prepared$Usage)

  print(paste("\nPrepared data for", utility_type, "-", owner_type, ":"))
  print(head(df_prepared))
  print(paste("\nTime range:", min(rownames(df_prepared)), "to", max(rownames(df_prepared))))
  print(paste("Number of data points:", nrow(df_prepared)))

  ts_data <- ts(df_prepared$Usage, frequency = frequency)

}, error = function(e) {
  print(paste("Data preparation error:", e$message))
  quit(status = 1)
})

# --- Time Series Decomposition ---
tryCatch({
  if (length(ts_data) > 100) {
    print("\nLarge dataset detected, trying alternative decomposition method...")
    # Calculate rolling statistics as approximation
    trend <- rollmean(ts_data, k = frequency, align = "center", fill = NA)
    detrended <- ts_data / trend
    seasonal_factors <- aggregate(detrended ~ cycle(detrended), FUN = mean)
    seasonal <- seasonal_factors[cycle(ts_data)]
    residual <- ts_data / (trend * seasonal)

    # Create decomposition plots using ggplot2
    df_plot <- data.frame(
      Date = as.Date(time(ts_data)),
      Original = as.numeric(ts_data),
      Trend = as.numeric(trend),
      Seasonal = as.numeric(seasonal),
      Residual = as.numeric(residual)
    )

    plot_original <- ggplot(df_plot, aes(x = Date, y = Original)) +
      geom_line(color = 'blue') +
      labs(y = 'Usage', title = 'Original') +
      theme_bw()

    plot_trend <- ggplot(df_plot, aes(x = Date, y = Trend)) +
      geom_line(color = 'orange') +
      labs(y = 'Trend', title = 'Trend') +
      theme_bw()

    plot_seasonal <- ggplot(df_plot, aes(x = Date, y = Seasonal)) +
      geom_line(color = 'green') +
      labs(y = 'Seasonality', title = 'Seasonal') +
      theme_bw()

    plot_residual <- ggplot(df_plot, aes(x = Date, y = Residual)) +
      geom_line(color = 'red') +
      labs(y = 'Residuals', title = 'Residuals') +
      theme_bw()

    grid.arrange(plot_original, plot_trend, plot_seasonal, plot_residual,
                 nrow = 4, top = paste('Approximate Decomposition of', utility_type, 'Consumption (', owner_type, ')'))

    print("\nNOTE: Used approximate decomposition due to memory constraints")

  } else {
    print("\nAttempting standard decomposition...")
    decomposition <- decompose(ts_data, type = "multiplicative")

    # Create decomposition plots using ggplot2
    df_plot <- data.frame(
      Date = as.Date(time(decomposition$x)),
      Original = as.numeric(decomposition$x),
      Trend = as.numeric(decomposition$trend),
      Seasonal = as.numeric(decomposition$seasonal),
      Residual = as.numeric(decomposition$random)
    )

    plot_original <- ggplot(df_plot, aes(x = Date, y = Original)) +
      geom_line(color = 'blue') +
      labs(y = 'Usage', title = 'Original') +
      theme_bw()

    plot_trend <- ggplot(df_plot, aes(x = Date, y = Trend)) +
      geom_line(color = 'orange') +
      labs(y = 'Trend', title = 'Trend') +
      theme_bw()

    plot_seasonal <- ggplot(df_plot, aes(x = Date, y = Seasonal)) +
      geom_line(color = 'green') +
      labs(y = 'Seasonality', title = 'Seasonal') +
      theme_bw()

    plot_residual <- ggplot(df_plot, aes(x = Date, y = Residual)) +
      geom_line(color = 'red') +
      labs(y = 'Residuals', title = 'Residuals') +
      theme_bw()

    grid.arrange(plot_original, plot_trend, plot_seasonal, plot_residual,
                 nrow = 4, top = paste('Decomposition of', utility_type, 'Consumption (', owner_type, ')'))
  }

  # Analysis summary
  print("\n=== Analysis Results ===")
  print("1. Trend Analysis:")
  print("   - Shows long-term patterns in the data")
  print("   - Increasing trend suggests growing consumption")
  print("   - Decreasing trend suggests reduced usage or efficiency gains")

  print(paste("\n2. Seasonality Analysis (", frequency, "-month cycle):", sep=""))
  print("   - Reveals repeating annual patterns")
  print("   - For electricity, look for summer/winter peaks")

  print("\n3. Residual Analysis:")
  print("   - Random residuals indicate good model fit")
  print("   - Patterns may suggest missing factors in the model")

}, error = function(e) {
  print(paste("\nDecomposition failed:", e$message))
  print("Possible solutions:")
  print("- Try 'additive' instead of 'multiplicative' model (if using decompose)")
  print("- Check for missing/irregular data points")
  print("- Reduce the seasonal period (frequency parameter)")
})