library(ggplot2)
library(knitr)
library(readr)
library(tibble)
library(tidyr)
library(dplyr)

#extracts the csv file from the zip 
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

working_dir <- getwd()

dest_zip_file <- file.path(working_dir, "data.zip")

dest_csv_file <- file.path(working_dir, "activity.csv")

download.file(url, dest_zip_file, method = "auto")

unzip(dest_zip_file, files = "activity.csv", exdir = working_dir)

if(file.exists(dest_csv_file)) {
  cat("CSV file extracted successfully")
} else {
  cat("Failed to extract the CSV file")
}

#reads the csv file
data <- read_csv("activity.csv")

#counts NAs in each column
na_count_steps <- sum(is.na(data$steps))
print(na_count_steps)

na_count_date <- sum(is.na(data$date))
print(na_count_date)

na_count_interval <- sum(is.na(data$interval))
print(na_count_interval)

#only the steps column has NAs

#adds a column with the days of the week
# Add a new column indicating weekday or weekend
data$day_name <- ifelse(weekdays(data$date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

#calculates the median and mean of steps on both weekdays and weekends
median_weekdays <- data %>%
  filter (day_name == "Weekday") %>%
  summarize(med_days = median(steps, na.rm = TRUE))

median_weekends <- data %>%
  filter (day_name == "Weekend") %>%
  summarize(med_ends = median(steps, na.rm = TRUE))

mean_weekdays <- data %>%
  filter (day_name == "Weekday") %>%
  summarize(mean_days = mean(steps, na.rm = TRUE))

mean_weekends <- data %>%
  filter (day_name == "Weekend") %>%
  summarize(mean_ends = mean(steps, na.rm = TRUE))

#since the median of both weekdays and weekends is the same, i.e. 0, I substitute 0 for NAs in the whole steps column
data$steps[is.na(data$steps)] <- 0

#ex.1
total_steps <- data %>%
  group_by(date) %>%
  summarize (total_per_day = sum(steps))

mean_per_day <- mean(total_steps$total_per_day)

median_per_day <- median(total_steps$total_per_day)

ggplot(total_steps, aes(x = date)) +
  geom_histogram ()

hist(total_steps$total_per_day, 
     main = "Histogram of Total Steps per Day",
     xlab = "Total Steps",
     ylab = "Frequency",
     col = "blue")

#ex.2
#aggregates the data and calculate the average number of steps per 5-minute interval
aggregated <- aggregate(steps ~ interval, data = data, mean)

plot(aggregated$interval, aggregated$steps,
     type = "l",
     main = "Average Number of Steps by 5-Minute Intervals",
     xlab = "5-Minute Interval",
     ylab = "Average Number of Steps")

# Find the 5-minute interval with the maximum average number of steps
max_interval <- data$interval[which.max(data$steps)]

# Print the result
print(paste("The 5-minute interval with the maximum average number of steps is:", max_interval))

# Find the day with the maximum average number of steps for a specific interval
max_steps_interval <- 615  # Specify the specific 5-minute interval
subset_data <- subset(data, interval == max_steps_interval)
max_day <- subset_data$date[which.max(subset_data$steps)]

# Print the result
print(paste("On", max_day, "the", max_steps_interval, "interval has the maximum average number of steps."))

#ex. 3
# Make a histogram of the total number of steps taken each day
hist(data$steps, breaks = "FD", main = "Histogram of Total Steps Per Day",
     xlab = "Total Steps", ylab = "Frequency")

# Calculate and report the mean and median total number of steps taken per day
mean_steps <- mean(data$steps)
median_steps <- median(data$steps)

print(paste("Mean steps per day:", mean_steps))
print(paste("Median steps per day:", median_steps))

# Filter out the 0 values from the 'steps' column
non_zero_steps <- data$steps[data$steps != 0]

# Make a histogram of the non-zero values
hist(non_zero_steps, breaks = "Sturges", main = "Histogram of Non-Zero Steps Per Day",
     xlab = "Total Steps", ylab = "Frequency")

# Calculate and report the mean and median total number of steps taken per day
mean_steps <- mean(non_zero_steps)
median_steps <- median(non_zero_steps)

print(paste("Mean steps per day:", mean_steps))
print(paste("Median steps per day:", median_steps))

#ex.4
data$day_name <- factor(data$day_name, levels = c("Weekday", "Weekend"))

# Calculate the average number of steps per interval and day type
avg_steps <- data %>%
  group_by(interval, day_name) %>%
  summarize(avg_steps = mean(steps)) %>%
  ungroup ()

# Create the panel plot
ggplot(avg_steps, aes(x = interval, y = avg_steps, group = day_name, color = day_name)) +
  geom_line() +
  labs(x = "5-Minute Interval", y = "Average Number of Steps",
       title = "Average Number of Steps by Interval") +
  theme_minimal() +
  facet_grid(. ~ day_name)