library(ggplot2)
library(knitr)
library(readr)
library(tibble)
library(tidyr)

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

