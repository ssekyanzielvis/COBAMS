# Load necessary libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(ggplot2)
library(gridExtra)

# --- Configuration ---
file_path <- 'C:/Users/USER/Downloads/MEC.csv'
utility_type <- 'Electricity'  # Options: 'Electricity', 'Water', 'Gas', 'Passengers'
owner_type <- 'Tenant'         # Options: 'Tenant', 'Commission'
frequency <- 12                # Monthly data with yearly seasonality

# --- Load and prepare data ---
tryCatch({
  # Read data
  df <- read_csv(file_path)
  cat("Initial data sample:\n")
  print(head(df, 3))
  
  # Filter for selected utility and owner
  df <- df %>% 
    filter(Utility == utility_type, Owner == owner_type)
  
  if (nrow(df) == 0) {
    stop(paste("No data found for", utility_type, "-", owner_type))
  }
  
  # Create proper datetime index (using first day of each month)
  df <- df %>%
    mutate(Date = make_date(Year, `Month Number`, 1)) %>%
    arrange(Date) %>%
    select(Date, Usage)
  
  # Convert to ts object for decomposition
  ts_data <- ts(df$Usage, frequency = frequency, start = c(year(min(df$Date)), month(min(df$Date))))
  
  cat(paste("\nPrepared data for", utility_type, "-", owner_type, ":\n"))
  print(head(df))
  cat(paste("\nTime range:", min(df$Date), "to", max(df$Date), "\n"))
  cat(paste("Number of data points:", nrow(df), "\n"))
  
}, error = function(e) {
  cat(paste("Data preparation error:", e$message, "\n"))
  quit(status = 1)
})

# --- Time Series Decomposition ---
tryCatch({
  if (length(ts_data) > 100) {
    cat("\nLarge dataset detected, trying alternative decomposition method...\n")
    
    # Calculate moving average for trend
    df$Trend <- forecast::ma(ts_data, order = frequency)
    
    # Detrend the data
    detrended <- ts_data / df$Trend
    
    # Calculate seasonal component
    seasonal <- tapply(detrended, cycle(ts_data), mean, na.rm = TRUE)
    df$Seasonal <- seasonal[cycle(ts_data)]
    
    # Calculate residuals
    df$Residual <- ts_data / (df$Trend * df$Seasonal)
    
    # Create decomposition plots
    p1 <- ggplot(df, aes(x = Date, y = Usage)) +
      geom_line(color = 'blue') +
      labs(y = 'Usage', title = 'Original') +
      theme_minimal()
    
    p2 <- ggplot(df, aes(x = Date, y = Trend)) +
      geom_line(color = 'orange') +
      labs(y = 'Trend') +
      theme_minimal()
    
    p3 <- ggplot(df, aes(x = Date, y = Seasonal)) +
      geom_line(color = 'green') +
      labs(y = 'Seasonality') +
      theme_minimal()
    
    p4 <- ggplot(df, aes(x = Date, y = Residual)) +
      geom_line(color = 'red') +
      labs(y = 'Residuals') +
      theme_minimal()
    
    grid.arrange(p1, p2, p3, p4, ncol = 1,
                 top = paste('Approximate Decomposition of', utility_type, 'Consumption (', owner_type, ')'))
    
    cat("\nNOTE: Used approximate decomposition due to memory constraints\n")
  } else {
    cat("\nAttempting standard decomposition...\n")
    
    # Perform seasonal decomposition
    decomposition <- stl(ts_data, s.window = "periodic", robust = TRUE)
    
    # Convert to dataframe for plotting
    decomp_df <- data.frame(
      Date = df$Date,
      Original = as.numeric(ts_data),
      Trend = as.numeric(decomposition$time.series[, "trend"]),
      Seasonal = as.numeric(decomposition$time.series[, "seasonal"]),
      Residual = as.numeric(decomposition$time.series[, "remainder"])
    )
    
    # Create decomposition plots
    p1 <- ggplot(decomp_df, aes(x = Date, y = Original)) +
      geom_line(color = 'blue') +
      labs(y = 'Usage', title = 'Original') +
      theme_minimal()
    
    p2 <- ggplot(decomp_df, aes(x = Date, y = Trend)) +
      geom_line(color = 'orange') +
      labs(y = 'Trend') +
      theme_minimal()
    
    p3 <- ggplot(decomp_df, aes(x = Date, y = Seasonal)) +
      geom_line(color = 'green') +
      labs(y = 'Seasonality') +
      theme_minimal()
    
    p4 <- ggplot(decomp_df, aes(x = Date, y = Residual)) +
      geom_line(color = 'red') +
      labs(y = 'Residuals') +
      theme_minimal()
    
    grid.arrange(p1, p2, p3, p4, ncol = 1,
                 top = paste('Decomposition of', utility_type, 'Consumption (', owner_type, ')'))
  }
  
  # Analysis summary
  cat("\n=== Analysis Results ===\n")
  cat("1. Trend Analysis:\n")
  cat("   - Shows long-term patterns in the data\n")
  cat("   - Increasing trend suggests growing consumption\n")
  cat("   - Decreasing trend suggests reduced usage or efficiency gains\n")
  
  cat(paste("\n2. Seasonality Analysis (", frequency, "-month cycle):\n", sep = ""))
  cat("   - Reveals repeating annual patterns\n")
  cat("   - For electricity, look for summer/winter peaks\n")
  
  cat("\n3. Residual Analysis:\n")
  cat("   - Random residuals indicate good model fit\n")
  cat("   - Patterns may suggest missing factors in the model\n")
  
}, error = function(e) {
  cat(paste("\nDecomposition failed:", e$message, "\n"))
  cat("Possible solutions:\n")
  cat("- Try different decomposition parameters\n")
  cat("- Check for missing/irregular data points\n")
  cat("- Reduce the seasonal period (frequency parameter)\n")
})