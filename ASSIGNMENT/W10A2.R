# --- Configuration ---
file_path <- 'C:/Users/USER/Downloads/MEC.csv'
year_column <- 'Year'
month_column <- 'Month'  # Column with month names ('Aug', 'Jul', etc.)
utility_column <- 'Utility'
owner_column <- 'Owner'
value_column <- 'Usage'
units_column <- 'Units'  # Column containing units of measurement
utility_to_analyze <- 'Electricity'  # Options: 'Electricity', 'Water', 'Gas', 'Passengers'
owner_to_analyze <- 'Tenant'  # Options: 'Tenant' or 'Commission'

# --- Load and prepare data ---
tryCatch({
    # Load data
    df <- read.csv(file_path)
    cat("First 5 rows of raw data:\n")
    print(head(df, 5))
    
    # Check for required columns
    required_columns <- c(year_column, month_column, utility_column, 
                         owner_column, value_column, units_column)
    missing_columns <- setdiff(required_columns, colnames(df))
    if (length(missing_columns) > 0) {
        stop(paste("Missing required columns:", paste(missing_columns, collapse=", ")))
    }

    # Create Date column
    df$Date <- as.Date(paste(df[[year_column]], df[[month_column]], "01"), format="%Y %b %d")
    
    # Filter data
    filtered_df <- df[df[[utility_column]] == utility_to_analyze & df[[owner_column]] == owner_to_analyze, ]
    
    if (nrow(filtered_df) == 0) {
        available_utilities <- unique(df[[utility_column]])
        available_owners <- unique(df[[owner_column]])
        stop(paste(
            "No data found for", utility_to_analyze, "/", owner_to_analyze, "\n",
            "Available utilities:", paste(available_utilities, collapse=", "), "\n",
            "Available owners:", paste(available_owners, collapse=", ")
        ))
    }
    
    # Get units for labeling
    units <- ifelse(units_column %in% colnames(filtered_df), filtered_df[[units_column]][1], '')
    
    # Prepare final data frame
    result_df <- filtered_df[c("Date", value_column)]
    result_df <- result_df[order(result_df$Date), ]
    result_df <- na.omit(result_df)

    cat(paste("\nFiltered data for", utility_to_analyze, "(", owner_to_analyze, "):\n"))
    print(head(result_df))
    cat("\nSummary statistics:\n")
    print(summary(result_df[[value_column]]))

}, error = function(e) {
    if (grepl("cannot open file", e$message)) {
        cat(paste("Error: File not found at", file_path, "\n"))
    } else if (grepl("Missing required columns", e$message)) {
        cat(paste("Column Error:", e$message, "\nAvailable columns:", paste(colnames(df), collapse=", "), "\n"))
    } else if (grepl("No data found for", e$message)) {
        cat(paste("Data Error:", e$message, "\n"))
    } else {
        cat(paste("Unexpected error:", e$message, "\n"))
    }
    quit(status = 1)
})

# --- Visualization ---
if (exists("result_df") && nrow(result_df) > 0) {
    library(ggplot2)
    
    # Create plot
    p <- ggplot(result_df, aes(x = Date, y = .data[[value_column]])) +
        geom_line(linewidth = 1, linetype = "solid") +
        geom_point(size = 2) +
        labs(
            title = paste("Monthly", utility_to_analyze, "Usage\n", owner_to_analyze, "(", units, ")"),
            x = "Date",
            y = paste("Usage (", units, ")"),
            color = "Legend"
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, margin = margin(b = 20)),
            axis.title.x = element_text(margin = margin(t = 10)),
            axis.title.y = element_text(margin = margin(r = 10)),
            panel.grid.major = element_line(linetype = "dashed", color = "gray", linewidth = 0.5),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1)
        )
    
    print(p)
} else {
    cat("No data available for visualization\n")
}