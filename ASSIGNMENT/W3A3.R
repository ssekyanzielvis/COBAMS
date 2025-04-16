# --- Load necessary libraries ---
library(readr)
library(ggplot2)
library(dplyr)
library(forecast) # For ts() and decompose()

# --- Configuration ---
file_path <- 'C:/Users/USER/Downloads/CPIAUCSL.csv' # <--- IMPORTANT: UPDATE THIS PATH
date_column <- 'observation_date' # Date column name from your CPI data
price_column <- 'CPIAUCSL'        # CPI column name from your CPI data
date_format <- '%m/%d/%Y'          # Date format from your CPI data
frequency <- 12                   # Assuming monthly data
decomposition_model <- 'multiplicative' # Options: 'multiplicative' or 'additive'

# --- Load the dataset ---
tryCatch({
  df <- read_csv(file_path)
  print("Head of the DataFrame:")
  print(head(df))
  print(paste("\nColumn names in your DataFrame:", paste(colnames(df), collapse = ", ")))

  # Convert the date column to Date objects
  df <- df %>%
    mutate(!!sym(date_column) := as.Date(!!sym(date_column), format = date_format)) %>%
    arrange(!!sym(date_column)) %>%
    rename(Date = !!sym(date_column), CPI = !!sym(price_column)) %>%
    filter(!is.na(CPI))

  print("\nDataFrame with date as index (using Date column):")
  print(head(df))
  print("\nInfo of the DataFrame:")
  print(str(df))

  ts_data <- ts(df$CPI, frequency = frequency)

}, error = function(e) {
  if (grepl("cannot open the connection", e$message)) {
    print(paste("Error: File not found at", file_path))
  } else if (grepl(paste0("Column '", date_column, "' not found"), e$message) ||
             grepl(paste0("Column '", price_column, "' not found"), e$message)) {
    print(paste("Error: Column not found. Please check the column names."))
  } else if (grepl("invalid 'type' \\(character\\) of argument", e$message)) {
    print(paste("Error: Could not parse date. Please check the 'date_format' variable. Details:", e$message))
  } else {
    print(paste("Error loading data:", e$message))
  }
  quit(status = 1)
})

# --- Normalize and plot the CPI data ---
if (nrow(df) > 0) {
  if (df$CPI[1] == 0) {
    print("Error: The first value in CPI is zero. Cannot normalize.")
    quit(status = 1)
  }

  df <- df %>%
    mutate(CPI_Normalized = (CPI / CPI[1]) * 100)

  print("\nHead of DataFrame with Normalized CPI:")
  print(head(df))

  # --- Plot the CPI data ---
  plot_cpi <- ggplot(df, aes(x = Date, y = CPI)) +
    geom_line() +
    labs(title = 'CPI Over Time', x = 'Time', y = 'Index Value') +
    theme_bw() +
    theme(panel.grid.major = element_line(color = "grey", linetype = "dashed"))
  print(plot_cpi)

  # --- Plot the normalized CPI ---
  plot_normalized_cpi <- ggplot(df, aes(x = Date, y = CPI_Normalized)) +
    geom_line(linewidth = 1.2, color = 'red') +
    labs(title = 'Normalized CPI Over Time', x = 'Time', y = 'Index Value (Normalized to 100 at start)') +
    theme_bw() +
    theme(panel.grid.major = element_line(color = "grey", linetype = "dashed"))
  print(plot_normalized_cpi)

  # --- Decompose the CPI time series ---
  tryCatch({
    if (length(ts_data) >= 2 * frequency) {
      decomposition <- decompose(ts_data, type = decomposition_model)
      print("\n--- Decomposition of Normalized CPI ---")
      print("Trend (first few values):\n")
      print(head(decomposition$trend))
      print("Seasonal (first few values):\n")
      print(head(decomposition$seasonal))
      print("Residual (first few values):\n")
      print(head(decomposition$random))

      plot_df_decomp <- data.frame(
        Date = df$Date[1:length(decomposition$x)],
        Original = as.numeric(decomposition$x),
        Trend = as.numeric(decomposition$trend),
        Seasonal = as.numeric(decomposition$seasonal),
        Residual = as.numeric(decomposition$random)
      )

      plot_decomp_original <- ggplot(plot_df_decomp, aes(x = Date, y = Original)) + geom_line() + labs(title = 'Original') + theme_bw()
      plot_decomp_trend <- ggplot(plot_df_decomp, aes(x = Date, y = Trend)) + geom_line() + labs(title = 'Trend') + theme_bw()
      plot_decomp_seasonal <- ggplot(plot_df_decomp, aes(x = Date, y = Seasonal)) + geom_line() + labs(title = 'Seasonal') + theme_bw()
      plot_decomp_residual <- ggplot(plot_df_decomp, aes(x = Date, y = Residual)) + geom_line() + labs(title = 'Residual') + theme_bw()

      gridExtra::grid.arrange(plot_decomp_original, plot_decomp_trend, plot_decomp_seasonal, plot_decomp_residual,
                              nrow = 4, top = 'Decomposition of Normalized CPI')

    } else {
      print("\nError during decomposition of CPI: Not enough periods for decomposition. Ensure your data has at least twice the frequency.")
    }
  }, error = function(e) {
    print(paste("\nError during decomposition of CPI:", e$message, ". Ensure your data has enough periods for decomposition and the model is appropriate."))
  })

} else {
  print("DataFrame is empty, cannot plot.")
}