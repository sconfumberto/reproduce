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

#filters out rows where there are NAs
xx

#then calculates the median of steps on both weekdays and weekends
xx

#then puts the meadian for weekdays in NA rows corresponding to weekdays
xx

#and puts the meadian for weekends in NA rows corresponding to weekends
xx
