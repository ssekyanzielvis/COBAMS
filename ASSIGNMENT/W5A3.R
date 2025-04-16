# --- Load necessary libraries ---
library(readr)
library(dplyr)
library(ggplot2)
library(forecast) # For ts() and decompose()
library(zoo)      # For moving average
library(gridExtra) # For arranging plots

# --- Configuration ---
file_path <- 'C:/Users/USER/Downloads/MEC.csv'
utility_type <- 'Electricity'  # Options: 'Electricity', 'Water', 'Gas', 'Passengers'
owner_type <- 'Tenant'          # Options: 'Tenant', 'Commission'
frequency <- 12                 # Monthly data with yearly seasonality

# --- Define clearer colors ---
original_color <- "darkblue"
trend_color <- "darkorange"
seasonal_color <- "darkgreen"
residual_color <- "darkred"

# --- Load and prepare data ---
tryCatch({
  df <- read_csv(file_path)
  print("Initial data sample:")
  print(head(df, 3))

  df_filtered <- df %>%
    filter(Utility == utility_type, Owner == owner_type)

  if (nrow(df_filtered) == 0) {
    stop(paste("No data found for", utility_type, "-", owner_type))
  }

  df_prepared <- df_filtered %>%
    mutate(Date = as.Date(paste(Year, `Month Number`, "01", sep = "-"))) %>%
    arrange(Date) %>%
    select(Date, Usage)

  print(paste0("\nPrepared data for ", utility_type, " - ", owner_type, ":"))
  print(head(df_prepared))
  print(paste("\nTime range:", min(df_prepared$Date), "to", max(df_prepared$Date)))
  print(paste("Number of data points:", nrow(df_prepared)))

  ts_data <- ts(df_prepared$Usage, frequency = frequency)

}, error = function(e) {
  print(paste("Data preparation error:", e))
  quit(status = 1)
})

# --- Time Series Decomposition ---
tryCatch({
  if (length(ts_data) > 100) {
    print("\nLarge dataset detected, trying approximate decomposition...")
    trend <- rollmean(ts_data, k = frequency, fill = NA, align = "center")
    detrended <- ts_data / trend
    seasonal_factors <- aggregate(detrended, FUN = mean, nfrequency = frequency)
    seasonal <- rep(seasonal_factors, length.out = length(ts_data))
    residual <- ts_data / (trend * seasonal)

    plot_df <- data.frame(
      Date = df_prepared$Date,
      Original = as.numeric(ts_data),
      Trend = as.numeric(trend),
      Seasonal = as.numeric(seasonal),
      Residual = as.numeric(residual)
    )

    plot_original <- ggplot(plot_df, aes(x = Date, y = Original)) + geom_line(color = original_color, linewidth = 1) + labs(y = 'Usage', title = 'Original') + theme_bw()
    plot_trend <- ggplot(plot_df, aes(x = Date, y = Trend)) + geom_line(color = trend_color, linewidth = 1) + labs(y = 'Trend', title = 'Trend') + theme_bw()
    plot_seasonal <- ggplot(plot_df, aes(x = Date, y = Seasonal)) + geom_line(color = seasonal_color, linewidth = 1) + labs(y = 'Seasonality', title = 'Seasonal') + theme_bw()
    plot_residual <- ggplot(plot_df, aes(x = Date, y = Residual)) + geom_line(color = residual_color, linewidth = 1) + labs(y = 'Residuals', title = 'Residuals') + theme_bw()

    approx_decomposition_plot <- grid.arrange(plot_original, plot_trend, plot_seasonal, plot_residual,
                                               nrow = 4, top = paste('Approximate Decomposition of', utility_type, 'Consumption (', owner_type, ')'))
    print(approx_decomposition_plot)
    ggsave(filename = paste0("approx_decomposition_", utility_type, "_", owner_type, ".png"), plot = approx_decomposition_plot, width = 12, height = 10, units = "in")

    print("\nNOTE: Used approximate decomposition due to dataset size")

  } else {
    print("\nAttempting standard decomposition...")
    decomposition <- decompose(ts_data, type = 'multiplicative')

    plot_df <- data.frame(
      Date = df_prepared$Date[1:length(decomposition$x)],
      Original = as.numeric(decomposition$x),
      Trend = as.numeric(decomposition$trend),
      Seasonal = as.numeric(decomposition$seasonal),
      Residual = as.numeric(decomposition$random)
    )

    plot_original <- ggplot(plot_df, aes(x = Date, y = Original)) + geom_line(color = original_color, linewidth = 1) + labs(y = 'Usage', title = 'Original') + theme_bw()
    plot_trend <- ggplot(plot_df, aes(x = Date, y = Trend)) + geom_line(color = trend_color, linewidth = 1) + labs(y = 'Trend', title = 'Trend') + theme_bw()
    plot_seasonal <- ggplot(plot_df, aes(x = Date, y = Seasonal)) + geom_line(color = seasonal_color, linewidth = 1) + labs(y = 'Seasonality', title = 'Seasonal') + theme_bw()
    plot_residual <- ggplot(plot_df, aes(x = Date, y = Residual)) + geom_line(color = residual_color, linewidth = 1) + labs(y = 'Residuals', title = 'Residuals') + theme_bw()

    standard_decomposition_plot <- grid.arrange(plot_original, plot_trend, plot_seasonal, plot_residual,
                                                nrow = 4, top = paste('Decomposition of', utility_type, 'Consumption (', owner_type, ')'))
    print(standard_decomposition_plot)
    ggsave(filename = paste0("standard_decomposition_", utility_type, "_", owner_type, ".png"), plot = standard_decomposition_plot, width = 12, height = 10, units = "in")
  }

  # Analysis summary
  print("\n=== Analysis Results ===")
  print(paste0("1. Trend Analysis:"))
  print("   - Shows long-term patterns in the data")
  print("   - Increasing trend suggests growing consumption")
  print("   - Decreasing trend suggests reduced usage or efficiency gains")

  print(paste("\n2. Seasonality Analysis (", frequency, "-month cycle):", sep = ""))
  print("   - Reveals repeating annual patterns")
  print("   - For electricity, look for summer/winter peaks")

  print("\n3. Residual Analysis:")
  print("   - Random residuals indicate good model fit")
  print("   - Patterns may suggest missing factors in the model")

}, error = function(e) {
  print(paste("Decomposition failed:", e))
  print("Possible solutions:")
  print("- Try a different decomposition method (e.g., using 'stl()')")
  print("- Check for missing/irregular data points")
  print("- Reduce the seasonal period (frequency parameter)")
})