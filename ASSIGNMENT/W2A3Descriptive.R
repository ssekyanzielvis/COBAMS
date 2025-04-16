# --- Configuration ---
file_path <- 'C:/Users/USER/Downloads/DailyDelhiClimateTest.csv'
date_column <- 'date'
value_column <- 'meantemp'
date_format <- '%Y-%m-%d'
frequency <- 12
decomposition_model <- 'additive'
output_dir <- 'C:/Users/USER/Documents/ASSIGNMENT1/' # Directory to save plots
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# --- Load the dataset ---
tryCatch({
  df <- read.csv(file_path)
  cat("Head of the DataFrame (raw):\n")
  print(head(df))
  cat("\nColumn names in raw data:", paste(names(df), collapse = ", "), "\n")

  # Convert the date column to Date objects
  df[[date_column]] <- as.Date(df[[date_column]], format = date_format)

  # Resample to monthly average
  library(dplyr)
  library(lubridate)

  df_monthly <- df %>%
    mutate(year_month = format(!!sym(date_column), "%Y-%m")) %>%
    group_by(year_month) %>%
    summarise(Monthly_Avg_Temp = mean(!!sym(value_column), na.rm = TRUE)) %>%
    mutate(date = as.Date(paste0(year_month, "-01"), format = "%Y-%m-%d"))

  cat("\nHead of df_monthly after resampling:\n")
  print(head(df_monthly))
  cat("\nStructure of df_monthly:\n")
  str(df_monthly)
  cat("\nColumn names in df_monthly:", paste(names(df_monthly), collapse = ", "), "\n")

}, error = function(e) {
  if (grepl("cannot open file", e$message)) {
    cat(paste0("Error: File not found at ", file_path, "\n"))
  } else {
    cat(paste0("Error during data loading or resampling: ", e$message, "\n"))
  }
  quit(status = 1)
})

# --- Check if df_monthly was created and has the right column ---
if (exists("df_monthly") && "Monthly_Avg_Temp" %in% names(df_monthly)) {
  cat("\ndf_monthly exists and has 'Monthly_Avg_Temp' column.\n")

  # --- Plot the data and save ---
  plot_filename <- file.path(output_dir, "w2a3.png")
  png(filename = plot_filename, width = 1200, height = 600) # Adjust width and height as needed
  plot(df_monthly[[date_column]], df_monthly$Monthly_Avg_Temp,
       type = "l",
       main = "Monthly Average Temperature",
       xlab = "Time",
       ylab = "Temperature",
       col = "blue",
       lwd = 2)
  grid()
  dev.off()
  cat(paste0("\nMonthly average temperature plot saved to: ", plot_filename, "\n"))

  # --- Decompose the time series ---
  if (nrow(df_monthly) >= 2 * frequency) {
    tryCatch({
      library(forecast)
      ts_data <- ts(df_monthly$Monthly_Avg_Temp, frequency = frequency)
      decomp <- decompose(ts_data, type = decomposition_model)

      cat("\n--- Decomposition Results (first few values) ---\n")
      cat("Trend:\n")
      print(head(decomp$trend))
      cat("\nSeasonal:\n")
      print(head(decomp$seasonal))
      cat("\nRandom:\n")
      print(head(decomp$random))

      # Plot decomposition and save
      decomp_filename <- file.path(output_dir, paste0("decomposition_", decomposition_model, ".png"))
      png(filename = decomp_filename, width = 1000, height = 800) # Adjust width and height as needed
      plot(decomp)
      title(main = paste0(toupper(substring(decomposition_model, 1, 1)), substring(decomposition_model, 2), " Decomposition of Monthly Average Temperature"), outer = TRUE, line = -1)
      dev.off()
      cat(paste0("\nDecomposition plot saved to: ", decomp_filename, "\n"))

    }, error = function(e) {
      cat(paste0("Error during decomposition: ", e$message, "\n"))
    })
  } else {
    cat(paste0("\nInsufficient data for seasonal decomposition. Requires at least ", 2 * frequency, " observations, but only ", nrow(df_monthly), " found.\n"))
  }
} else {
  cat("Error: Monthly average temperature data was not processed properly.\n")
}