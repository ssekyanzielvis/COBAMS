# --- Configuration ---
file_path <- 'C:/Users/USER/Downloads/CPIAUCSL.csv'  # Replace with the actual path to your CSV file
date_column <- 'observation_date'
price_column <- 'CPIAUCSL'
date_format <- '%m/%d/%Y'

# --- Load the dataset ---
tryCatch({
    # Load data
    df <- read.csv(file_path)
    cat("Head of the DataFrame:\n")
    print(head(df))
    
    # Convert date column and set as index
    df[[date_column]] <- as.Date(df[[date_column]], format = date_format)
    df <- df[order(df[[date_column]]), ]
    df <- df[complete.cases(df[[price_column]]), ]
    
    cat("\nDataFrame with date as index:\n")
    print(head(df))
    cat("\nInfo of the DataFrame:\n")
    print(str(df))
    
    # --- Calculate the percentage change in CPI ---
    if (nrow(df) > 0) {
        df$CPI_Percentage_Change <- c(NA, diff(df[[price_column]]) / head(df[[price_column]], -1)) * 100
        df <- df[complete.cases(df$CPI_Percentage_Change), ]
        
        cat("\nDataFrame with CPI Percentage Change:\n")
        print(head(df))
        
        # --- Plot the CPI and percentage change ---
        library(ggplot2)
        library(gridExtra)
        
        # Plot the CPI
        p1 <- ggplot(df, aes(x = .data[[date_column]], y = .data[[price_column]])) +
            geom_line() +
            labs(y = 'CPI Value', title = 'Consumer Price Index Over Time') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5),
                  panel.grid = element_line(color = "grey90"))
        
        # Plot the Percentage Change in CPI
        p2 <- ggplot(df, aes(x = .data[[date_column]], y = CPI_Percentage_Change)) +
            geom_line(color = 'red') +
            labs(y = 'Percentage Change (%)', 
                 title = 'Percentage Change in Consumer Price Index',
                 x = 'Date') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5),
                  panel.grid = element_line(color = "grey90"))
        
        # Combine plots
        grid.arrange(p1, p2, nrow = 2)
        
    } else {
        cat("DataFrame is empty, cannot perform analysis or plot.\n")
    }
    
}, error = function(e) {
    if (grepl("cannot open file", e$message)) {
        cat(paste("Error: File not found at", file_path, "\n"))
    } else if (grepl("undefined columns selected", e$message)) {
        cat(paste("Error: Column not found in the CSV file. Please check the column names.\n"))
    } else if (grepl("character string is not in a standard unambiguous format", e$message)) {
        cat(paste("Error: Could not parse date. Please check the 'date_format' variable.\n"))
    } else {
        cat(paste("Unexpected error:", e$message, "\n"))
    }
    quit(status = 1)
})