{
 "cells": [
  {
   "cell_type": "raw",
   "id": "033d128b",
   "metadata": {
    "vscode": {
     "languageId": "raw"
    }
   },
   "source": [
    "# Activity 2: Exploring Time Series Data in R (Using CSV File)\n",
    "\n",
    "# --- 1. Load the dataset from a CSV file ---\n",
    "# Set the working directory to where your CSV file is located\n",
    "# setwd(\"your/directory/path\") # Uncomment and replace with your actual path\n",
    "\n",
    "# Load the CSV file\n",
    "air_passengers_df <- read.csv(\"C:/Users/USER/Downloads/AirPassengrs.ecsv\")\n",
    "\n",
    "# Print the first few rows to inspect the data\n",
    "print(\"First few rows of the CSV data:\")\n",
    "head(air_passengers_df)\n",
    "\n",
    "# Assuming your CSV has columns like 'Month' and '#Passengers' (adjust if different)\n",
    "# Convert the 'Month' column to a proper date format\n",
    "air_passengers_df$Month <- as.Date(paste0(air_passengers_df$Month, \"-01\"), \"%Y-%m-%d\")\n",
    "\n",
    "# Create a time series object from the 'Passengers' column, using the 'Month' as the time index\n",
    "AirPassengers_ts <- ts(air_passengers_df$`)#Passengers`, frequency = 12, start = c(1949, 1))\n",
    "\n",
    "# --- 2. Plot the data ---\n",
    "plot(AirPassengers_ts,\n",
    "     main=\"Monthly Airline Passengers\",\n",
    "     ylab=\"Passengers\",\n",
    "     xlab=\"Time\")\n",
    "\n",
    "# --- 3. Decompose the time series into trend, seasonality, and residuals ---\n",
    "decompose_result <- decompose(AirPassengers_ts,\n",
    "                              type=\"multiplicative\")\n",
    "plot(decompose_result)\n",
    "\n",
    "# Outcome: Students will visualize the components of the time series.\n",
    "\n",
    "# --- Explanation of the R Code ---\n",
    "\n",
    "# 1. `read.csv(\"AirPassengers.csv\")`: This command reads the data from your CSV file.\n",
    "#    - Ensure that the filename \"AirPassengers.csv\" matches your file.\n",
    "#    - If the file is not in your current working directory, use the full path\n",
    "#      (e.g., `read.csv(\"/path/to/your/file/AirPassengers.csv\")`).\n",
    "\n",
    "# 2. The subsequent steps are the same as when using the built-in dataset, but they\n",
    "#    now operate on the time series object created from your CSV data.\n",
    "#    - The 'Month' column is converted to a Date object.\n",
    "#    - The 'Passengers' column is used to create a time series object (`ts`).\n",
    "#    - The `plot()` function visualizes this time series.\n",
    "#    - The `decompose()` function breaks down the time series into its components.\n",
    "#    - `plot(decompose_result)` visualizes these components.\n",
    "\n",
    "# Make sure to adjust the `read.csv()` function if your CSV file has a different\n",
    "# separator (e.g., semicolon `;` use `sep=\";\"`) or if the column names are different.\n",
    "# The code assumes you have a 'Month' column and a column containing the passenger numbers\n",
    "# (here assumed to be '#Passengers'). Adjust the column name access if needed\n",
    "# (e.g., `air_passengers_df$YourPassengerColumnName`)."
   ]
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "Python 3",
   "language": "python",
   "name": "python3"
  },
  "language_info": {
   "codemirror_mode": {
    "name": "ipython",
    "version": 3
   },
   "file_extension": ".py",
   "mimetype": "text/x-python",
   "name": "python",
   "nbconvert_exporter": "python",
   "pygments_lexer": "ipython3",
   "version": "3.12.10"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 5
}
