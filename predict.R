#!/usr/bin/env Rscript

# Load spreadsheet.
readings <- read.csv('space.csv', colClasses = c('factor', 'numeric', 'factor'),
                     na.strings = NULL)
readings$timestamp <- as.POSIXct(readings$timestamp, origin = '1970-01-01')
names(readings)[2] <- 'datetime'

# Bucket by hour.
readings$day <- factor(weekdays(readings$datetime))
readings$hour <- as.numeric(strftime(readings$datetime, '%H'))

# Subset to last week.
end <- max(readings$datetime)
start <- end - as.difftime(1, units = 'weeks')
readings.lastweek <- subset(readings, datetime > start)

# Come up with scores
library(dplyr)

features <- readings.lastweek %>%
    group_by(space, day, hour) %>%
    summarize(yes = sum(open == 'TRUE'),
              no = sum(open == 'FALSE'),
              broken = sum(open == 'NA'))
