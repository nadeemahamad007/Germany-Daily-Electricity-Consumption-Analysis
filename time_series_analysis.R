# Load the dataset from the specified path
mydata <- read.csv("D:/MCA Life 2023-25/DATA SCIENCE Rajnish Sir/R-Programming/Project/R_Project/opsd_germany_daily.csv", header = TRUE, row.names = "Date")

# Display the first few rows of the dataset to confirm it's loaded correctly
head(mydata)

# Check the structure of the dataframe
str(mydata)

# Look at the last few rows
tail(mydata)

# Access the 'Consumption' column
mydata$Consumption

# Access the row names (dates)
row.names(mydata)

# Check the first few row names (dates)
head(row.names(mydata))

# Accessing a specific row ("2006-01-01")
mydata["2006-01-01", ]

# Accessing another specific row ("2017-08-10")
mydata["2017-08-10", ]

# Accessing multiple rows ("2006-01-01" and "2006-01-04")
mydata[c("2006-01-01", "2006-01-04"), ]

# Get summary of the dataset
summary(mydata)


# Convert the row names (Date) to Date format
x <- as.Date(row.names(mydata), format = "%Y-%m-%d")

# Display the first few rows of x to confirm conversion
head(x)

# Check the class of x
class(x)

# Display the structure of x
str(x)

# Create year, month, and day columns
year <- as.numeric(format(x, '%Y'))
month <- as.numeric(format(x, '%m'))
day <- as.numeric(format(x, '%d'))

# Display the first few rows of each new variable
head(year)
head(month)
head(day)

# Add year, month, and day columns to the existing dataframe
mydata <- cbind(mydata, year, month, day)

# Display the first few rows of the updated dataframe
head(mydata)

# Display the first 3 rows of the updated dataframe
mydata[1:3, ]

# Check the number of rows in your dataset
num_rows <- nrow(mydata)
num_rows

# Sample 8 rows from the dataset with replacement
sampled_data <- mydata[sample(nrow(mydata), 8, replace = TRUE), ]
head(sampled_data)

# Sample 8 rows from the dataset with replacement
sampled_data <- mydata[sample(nrow(mydata), 8, replace = TRUE), ]
head(sampled_data)

# Sample 8 rows from the dataset (only if there are at least 8 rows)
if (nrow(mydata) >= 8) {
  sampled_data <- mydata[sample(nrow(mydata), 8, replace = FALSE), ]
  head(sampled_data)
} else {
  warning("Not enough rows in the dataset to sample 8 rows.")
}

# Load necessary library
# install.packages("ggplot2") # Uncomment if ggplot2 is not installed
library(ggplot2)

# Check the number of rows in your dataset
num_rows <- nrow(mydata)
num_rows

# Display a random sample of 8 rows from the dataset (if possible)
if (num_rows >= 8) {
  sampled_data <- mydata[sample(nrow(mydata), 8, replace = FALSE), ]
  head(sampled_data)
} else {
  warning("Not enough rows in the dataset to sample 8 rows.")
}

# Option 1: Basic Line Plot using Base R
plot(mydata$year, mydata$Consumption, type = "l", xlab = "Year", ylab = "Consumption",
     main = "Germany Daily Electricity Consumption")

# Option 2: Enhanced Line Plot with Custom Limits and Line Type
plot(mydata$year, mydata$Consumption, type = "l", xlab = "Year", ylab = "Consumption",
     lty = 1, ylim = c(800, 1700), xlim = c(2006, 2018),
     main = "Germany Daily Electricity Consumption (2006-2018)")

# Option 3: Using Par Settings for Plot Layout
par(mfrow = c(1, 1))  # Ensure only one plot in the plotting window
plot(mydata$year, mydata$Consumption, type = "l", xlab = "Year", ylab = "Consumption",
     main = "Germany Daily Electricity Consumption")

# Option 4: Using ggplot2 for Enhanced Visualization
ggplot(mydata, aes(x = year, y = Consumption)) +
  geom_line() +
  labs(title = "Germany Daily Electricity Consumption", x = "Year", y = "Consumption") +
  theme_minimal()

# Option 4: Various ways to plot using Base R

# Plot with basic line type
plot(mydata$year, mydata$Consumption, xlab = "Year", ylab = "Consumption", type = "l", lwd = 2)

# Enhanced plot with color and line type
plot(mydata$year, mydata$Consumption, xlab = "Year", ylab = "Consumption", type = "l", lwd = 2, col = "blue")

# Plot with custom limits and enhanced options
plot(mydata$year, mydata$Consumption, xlab = "Year", ylab = "Consumption", type = "l", lwd = 2, xlim = c(2006, 2018), ylim = c(900, 2000), main = "Consumption Graph")

# Taking the log of consumption, differencing it, and plotting
plot(diff(log(mydata$Consumption)), xlab = "Year", ylab = "Differenced Log Consumption", type = "l", lwd = 2, ylim = c(-5, 5), main = "Differenced Log Consumption", col = "orange")

# Using ggplot2 for enhanced visualization
# Uncomment and run the following lines if ggplot2 is installed

# Option 1: Basic ggplot with lines
# install.packages("ggplot2")  # Uncomment if ggplot2 is not installed
library(ggplot2)

ggplot(mydata, aes(x = year, y = Consumption)) +
  geom_line() +
  labs(title = "Germany Daily Electricity Consumption", x = "Year", y = "Consumption") +
  theme_minimal()

# Option 2: ggplot with lines and points
ggplot(mydata, aes(x = year, y = Consumption, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Germany Daily Electricity Consumption", x = "Year", y = "Consumption") +
  theme_minimal()

# Option 3: Custom ggplot with line type and color
ggplot(mydata, aes(x = year, y = Consumption, group = 1)) +
  geom_line(linetype = "dashed", color = "blue") +
  labs(title = "Germany Daily Electricity Consumption", x = "Year", y = "Consumption") +
  theme_minimal()

# Assuming the dataframe 'mydata' is used and contains the relevant columns

# Load necessary library
library(ggplot2)

# Min and Max for Wind Column (assuming it is the 3rd column)
wind_min <- min(mydata[, 3], na.rm = TRUE)
wind_max <- max(mydata[, 3], na.rm = TRUE)
cat("Wind Column: Min =", wind_min, "Max =", wind_max, "\n")

# Min and Max for Consumption Column (assuming it is the 2nd column)
consumption_min <- min(mydata[, 2], na.rm = TRUE)
consumption_max <- max(mydata[, 2], na.rm = TRUE)
cat("Consumption Column: Min =", consumption_min, "Max =", consumption_max, "\n")

# Min and Max for Solar Column (assuming it is the 4th column)
solar_min <- min(mydata[, 4], na.rm = TRUE)
solar_max <- max(mydata[, 4], na.rm = TRUE)
cat("Solar Column: Min =", solar_min, "Max =", solar_max, "\n")

# Min and Max for WindSolar Column (assuming it is the 5th column)
windsolar_min <- min(mydata[, 5], na.rm = TRUE)
windsolar_max <- max(mydata[, 5], na.rm = TRUE)
cat("WindSolar Column: Min =", windsolar_min, "Max =", windsolar_max, "\n")

# Set up for multiple plots
par(mfrow = c(3, 1))  # Arrange plots in a 3x1 grid

# Example plots
plot(mydata[, 2], type = "l", col = "blue", main = "Consumption over Time", xlab = "Time", ylab = "Consumption")
plot(mydata[, 3], type = "l", col = "green", main = "Wind over Time", xlab = "Time", ylab = "Wind")
plot(mydata[, 4], type = "l", col = "red", main = "Solar over Time", xlab = "Time", ylab = "Solar")

# Reset the plotting layout to default (single plot per window)
par(mfrow = c(1, 1))

# Load the dataset from the specified path
mydata <- read.csv("D:/MCA Life 2023-25/DATA SCIENCE Rajnish Sir/R-Programming/Project/R_Project/opsd_germany_daily.csv", header = TRUE, row.names = "Date")

# Display the first few rows of the dataset to confirm it's loaded correctly
head(mydata)
# Plot Consumption
plot(mydata$Consumption, 
     main = "Consumption", 
     col = "orange", 
     xlab = "Year", 
     ylab = "Daily Totals (GWh)", 
     type = "l", 
     ylim = c(840, 1750))

# Additional plot for Solar
plot(mydata$Solar, 
     main = "Solar", 
     xlab = "Year", 
     ylab = "Daily Totals (GWh)", 
     type = "l", 
     ylim = c(0, 500), 
     col = "blue")

# Additional plot for Wind
plot(mydata$Wind, 
     main = "Wind", 
     xlab = "Year", 
     ylab = "Daily Totals (GWh)", 
     type = "l", 
     ylim = c(0, 900), 
     col = "red")

# Set up for multiple plots
par(mfrow = c(3, 1))  # Arrange plots in a 3x1 grid

# Example plots
plot(mydata$Consumption, type = "l", col = "blue", main = "Consumption over Time", xlab = "Time", ylab = "Consumption")
plot(mydata$Wind, type = "l", col = "green", main = "Wind over Time", xlab = "Time", ylab = "Wind")
plot(mydata$Solar, type = "l", col = "red", main = "Solar over Time", xlab = "Time", ylab = "Solar")

# Reset the plotting layout to default (single plot per window)
par(mfrow = c(1, 1))

# Set the plotting window to a single plot
par(mfrow = c(1, 1))

# Investigate structure of the dataset
str(mydata)

# Convert row names (Date) to Date format
x <- as.Date(row.names(mydata))

# Check the first few dates
head(x)

# Check the class of x
class(x)

# Display the structure of x
str(x)

# To convert the date column into date format (if there is a Date column)
# Assuming you are converting an existing column called 'Date' into Date format
moddate <- as.Date(x, format = "%m/%d/%Y")

# Check the structure of moddate
str(moddate)

# Combine the new moddate column with the original dataset
mydata3 <- cbind(moddate, mydata)

# Display the first few rows of the new dataframe
head(mydata3)

# Check the structure of the new dataframe
str(mydata3)

# Subset the data for a specific date range
mydata4 <- subset(mydata3, moddate >= as.Date('2017-01-01') & moddate <= as.Date('2017-12-31'))

# Display the first few rows of the subsetted data
head(mydata4)

# Plot the data from the subsetted year (2017)
plot(mydata4$moddate, mydata4$Consumption, 
     xlab = "Year", 
     ylab = "Daily Totals (GWh)", 
     type = "l", 
     col = "orange", 
     lwd = 2, 
     main = "Consumption in 2017")

# Zooming in further - Subset data for January and February 2017
mydata4 <- subset(mydata3, moddate >= as.Date('2017-01-01') & moddate <= as.Date('2017-02-28'))

# Display the first few rows of the subsetted data
head(mydata4)

# Calculate the minimum and maximum values for the x-axis (dates) and y-axis (Consumption)
xmin <- min(mydata4$moddate, na.rm = TRUE)
xmax <- max(mydata4$moddate, na.rm = TRUE)

# Calculate the minimum and maximum values for the y-axis (Consumption column)
ymin <- min(mydata4$Consumption, na.rm = TRUE)
ymax <- max(mydata4$Consumption, na.rm = TRUE)

# Check xmin, xmax, ymin, and ymax values
xmin
xmax
ymin
ymax

# Plot the Consumption data for the subsetted date range (January and February 2017)
plot(mydata4$moddate, mydata4$Consumption, 
     xlab = "Year", 
     ylab = "Daily Totals (GWh)", 
     type = "l", 
     col = "orange", 
     lwd = 2, 
     main = "Consumption (January - February 2017)", 
     xlim = c(xmin, xmax), 
     ylim = c(ymin, ymax))

# Ensure the Date column is correctly formatted
mydata3$moddate <- as.Date(mydata3$moddate, format = "%Y-%m-%d")

# Create the 'year' column from the 'moddate' column
mydata3$year <- format(mydata3$moddate, "%Y")

# Convert 'year' to numeric for plotting
mydata3$year <- as.numeric(mydata3$year)

# Set x and y limits for the plot
xmin <- min(mydata4$moddate, na.rm = TRUE)
xmax <- max(mydata4$moddate, na.rm = TRUE)
ymin <- min(mydata4$Consumption, na.rm = TRUE)
ymax <- max(mydata4$Consumption, na.rm = TRUE)

# Plot with grid lines
plot(mydata4$moddate, mydata4$Consumption, 
     xlab = "Year", 
     ylab = "Daily Totals (GWh)", 
     type = "l", 
     col = "orange", 
     lwd = 2, 
     main = "Consumption (January - February 2017)", 
     xlim = c(xmin, xmax), 
     ylim = c(ymin, ymax))

# Add grid lines
grid()

# Add solid horizontal lines at specific values
abline(h = c(1300, 1500, 1600), lty = 1, col = "black")

# Add dashed blue vertical lines at weekly intervals
abline(v = seq(xmin, xmax, by = 7), lty = 2, col = "blue")

# Add a box around the plot
box()

# Create boxplots for Consumption, Solar, and Wind data
par(mfrow = c(3, 1)) # Set up for 3 plots in one row
boxplot(mydata3$Consumption, main = "Consumption", ylab = "Consumption (GWh)")
boxplot(mydata3$Solar, main = "Solar", ylab = "Solar (GWh)")
boxplot(mydata3$Wind, main = "Wind", ylab = "Wind (GWh)")
par(mfrow = c(1, 1)) # Reset to one plot per window

# Quantiles for Consumption data
quantile(mydata3$Consumption, probs = c(0, 0.25, 0.5, 0.75, 1))

# Boxplot for Consumption data with year-wise grouping
boxplot(mydata3$Consumption ~ mydata3$year, 
        main = "Consumption by Year", 
        ylab = "Consumption (GWh)", 
        xlab = "Years", 
        ylim = c(600, 1800))

# Check the column names in the dataset to ensure they exist
print(colnames(mydata))

# Generate a frequency sequence based on numeric intervals (like yearly increments)
# Let's say you want increments of 1 (e.g., each year)
xmin <- min(mydata$Consumption, na.rm = TRUE)
xmax <- max(mydata$Consumption, na.rm = TRUE)

# Generating a sequence with numeric increments, e.g., 100 for the difference between values
freq3 <- seq(from = xmin, to = xmax, by = 100)

# Print the sequence
print(freq3)

# Let's select data which has NA values for Wind
selwind1 <- mydata[which(is.na(mydata$Wind)), 
                    c("Consumption", "Wind", "Solar")]
# Display the first 10 rows
print(selwind1[1:10, ])

# Let's select data which does not have NA values for Wind
selwind2 <- mydata[which(!is.na(mydata$Wind)), 
                    c("Consumption", "Wind", "Solar")]
# Display the first 10 rows
print(selwind2[1:10, ])

# Since there's no 'moddate' column, the check for year-based filtering will not apply.
# You can remove or modify this part if you add a date column later.

# Install tidyr if not already installed
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}

# Load the tidyr library
library(tidyr)

# Select rows where Wind is NA
selwind1 <- mydata[which(is.na(mydata$Wind)), 
                   c("Consumption", "Wind", "Solar")]

# Display the first 10 rows of 'selwind1'
print(selwind1[1:10, ])

# Select rows where Wind is NOT NA
selwind2 <- mydata[which(!is.na(mydata$Wind)), 
                   c("Consumption", "Wind", "Solar")]

# Display the first 10 rows of 'selwind2'
print(selwind2[1:10, ])

# To mimic 'selwind3', we can select data where a specific condition applies
selwind3 <- mydata[which(mydata$month == 12 & mydata$day >= 12 & mydata$day <= 16), 
                   c("Consumption", "Wind", "Solar")]

# Display the first 10 rows of 'selwind3' to verify
print(selwind3[1:10, ])

# Check the class of 'selwind3'
class(selwind3)

# Check the structure of 'selwind3'
str(selwind3)

# Use fill to forward fill missing values in the 'Wind' column of 'selwind3'
selwind3 <- selwind3 %>% fill(Wind, .direction = "down")

# Display the updated 'selwind3' after filling NA values
print(selwind3)

# Ensure necessary packages are installed and loaded
if (!requireNamespace("zoo", quietly = TRUE)) {
  install.packages("zoo")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(zoo)
library(dplyr)

# Assuming mydata3 is your main dataset and has a 'moddate' and 'Consumption' column
# Ensure the moddate is in Date format
mydata3$moddate <- as.Date(mydata3$moddate)

# Add year column if it doesn't exist
mydata3$year <- format(mydata3$moddate, "%Y")

# Calculate rolling means for different time frames
mydataTest <- mydata3 %>%
  arrange(moddate) %>%
  group_by(year) %>%
  mutate(
    test_3da = zoo::rollmean(Consumption, k = 3, fill = NA, align = "center"),
    test_7da = zoo::rollmean(Consumption, k = 7, fill = NA, align = "center"),
    test_365da = zoo::rollmean(Consumption, k = 365, fill = NA, align = "center")
  ) %>%
  ungroup()

# Check results for 2017
results_2017 <- mydataTest %>%
  filter(year == "2017") %>%
  select(moddate, Consumption, test_3da, test_7da, test_365da)

print(results_2017)

# Plot the data
par(mfrow = c(1, 1))
plot(mydataTest$moddate, mydataTest$Consumption, type = "l", col = "blue", 
     xlab = "Year", ylab = "Consumption", main = "Consumption Graph")
lines(mydataTest$moddate, mydataTest$test_7da, col = "orange")
lines(mydataTest$moddate, mydataTest$test_365da, col = "black")

# Add legend to the plot
legend("topright", legend = c("Consumption", "7-Day Avg", "365-Day Avg"), 
       col = c("blue", "orange", "black"), lty = 1)
