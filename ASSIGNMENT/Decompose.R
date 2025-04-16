# --- Configuration ---
file_path <- 'C:/Users/USER/Downloads/AirPassengers.csv'
date_column <- 'Month'
value_column <- 'Passengers'  # Changed from X.Passengers to match your data
date_format <- '%Y-%m'
frequency <- 12
decomposition_model <- 'multiplicative'  # Can be 'additive' or 'multiplicative'

# --- Load the dataset ---
tryCatch({
  df <- read.csv(file_path, check.names = FALSE)  # Prevents R from modifying column names
  cat("Head of the DataFrame:\n")
  print(head(df))
  
  # Show actual column names
  cat("\nActual column names:", paste(names(df), collapse=", "), "\n")
  
  # Convert the date column to Date objects
  df[[date_column]] <- as.Date(df[[date_column]], format = date_format)
  
  # Create time series object
  ts_data <- ts(df[[value_column]],
                frequency = frequency,
                start = c(as.numeric(format(df[[date_column]][1], "%Y")),
                          as.numeric(format(df[[date_column]][1], "%m"))))
  
}, error = function(e) {
  if (grepl("cannot open file", e$message)) {
    cat(paste0("Error: File not found at ", file_path, "\n"))
  } else if (grepl("undefined columns selected", e$message)) {
    cat(paste0("Error: Column '", value_column, "' not found. Available columns: ",
               paste(names(df), collapse=", "), "\n"))
  } else {
    cat(paste0("Error: Could not parse date. Details: ", e$message, "\n"))
  }
  quit(status = 1)
})

# --- Plot and decompose the time series ---
if (exists("ts_data")) {
  # Plot original series
  plot(ts_data,
       main = "Air Passengers (1949-1960)",
       xlab = "Year",
       ylab = "Number of Passengers",
       col = "darkblue",
       lwd = 2)
  grid()
  
  # --- Decomposition ---
  tryCatch({
    # First verify decomposition_model exists
    if (!exists("decomposition_model")) {
      stop("decomposition_model not defined")
    }
    
    if (decomposition_model == 'multiplicative') {
      cat("\nPerforming multiplicative decomposition...\n")
      
      # Log-transform approach
      log_ts <- log(ts_data)
      decomp <- decompose(ts(log_ts), type = "additive")
      
      # Transform components back
      trend <- exp(decomp$trend)
      seasonal <- exp(decomp$seasonal)
      random <- ts_data/(trend * seasonal)
      
      # Plot decomposition
      par(mfrow=c(4,1), mar=c(3,4,2,1))
      plot(ts_data, main="Original Series", ylab="Passengers")
      plot(trend, main="Trend Component", ylab="Passengers")
      plot(seasonal, main="Seasonal Component", ylab="Multiplier")
      plot(random, main="Random Component", ylab="Residual")
      par(mfrow=c(1,1))
      
    } else if (decomposition_model == 'additive') {
      cat("\nPerforming additive decomposition...\n")
      decomp <- decompose(ts_data, type = "additive")
      plot(decomp)
      title(main = "Additive Decomposition", outer = TRUE, line = -1)
    } else {
      stop("Invalid decomposition_model. Must be 'additive' or 'multiplicative'")
    }
    
    # Print decomposition results
    cat("\nDecomposition results (first few values):\n")
    if (decomposition_model == 'multiplicative') {
      cat("Trend:\n"); print(head(trend))
      cat("\nSeasonal:\n"); print(head(seasonal))
      cat("\nRandom:\n"); print(head(random))
    } else {
      cat("Trend:\n"); print(head(decomp$trend))
      cat("\nSeasonal:\n"); print(head(decomp$seasonal))
      cat("\nResidual:\n"); print(head(decomp$random))
    }
    
  }, error = function(e) {
    cat(paste0("\nDecomposition error: ", e$message, "\n"))
    if (exists("ts_data") && frequency(ts_data) < 2) {
      cat("Warning: Your data may not have enough periods for decomposition (need at least 2 full periods)\n")
    }
  })
} else {
  cat("Error: Time series data was not loaded properly.\n")
}
