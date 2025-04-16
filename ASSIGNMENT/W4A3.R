# --- Load Required Libraries ---
library(ggplot2)
library(scales)

# --- Configuration ---
file_path <- "C:/Users/USER/Downloads/AirPassengers.csv"  # Update if needed
date_column <- "Month"
value_column <- "#Passengers"
seasonal_period <- 12  # Monthly data with yearly seasonality

# --- Load the Dataset ---
df <- read.csv(file_path, stringsAsFactors = FALSE)

# Convert date column to Date format (assuming format is 'YYYY-MM')
df[[date_column]] <- as.Date(paste0(df[[date_column]], "-01"))  # Append day for proper conversion

# Remove NA values
df <- df[!is.na(df[[value_column]]), ]

# Convert time series data
ts_data <- ts(df[[value_column]], start = c(as.numeric(format(min(df[[date_column]]), "%Y")), 
                                            as.numeric(format(min(df[[date_column]]), "%m"))),
              frequency = seasonal_period)

# --- Apply Holt-Winters Model ---
hw_model <- HoltWinters(ts_data, seasonal = "multiplicative", 
                        alpha = 0.8, beta = 0.2, gamma = 0.3)

# Extract fitted values
fitted_values <- as.numeric(hw_model$fitted[, "xhat"])

# Add fitted values back to original data frame
df$Smoothed <- c(rep(NA, length(ts_data) - length(fitted_values)), fitted_values)

# --- Plot Original vs Smoothed ---
ggplot(df, aes(x = df[[date_column]])) +
  geom_line(aes(y = df[[value_column]]), color = "blue", size = 1, linetype = "solid", na.rm = TRUE) +
  geom_line(aes(y = Smoothed), color = "red", size = 1, na.rm = TRUE) +
  labs(title = "AirPassengers - Original vs Holt-Winters Smoothed",
       x = "Date", y = "Number of Passengers") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
