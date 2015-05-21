#!/usr/bin/env Rscript

# Load spreadsheet.
readings <- read.csv('space.csv', colClasses = c('factor', 'numeric', 'factor'),
                     na.strings = NULL)
readings$timestamp <- as.POSIXct(readings$timestamp, origin = '1970-01-01')
names(readings)[2] <- 'datetime'

# Bucket by hour.
readings$day <- factor(weekdays(readings$datetime),
                       levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday',
                                  'Thursday', 'Friday', 'Saturday'))
readings$hour <- as.numeric(strftime(readings$datetime, '%H'))

# Subset to last week.
end <- max(readings$datetime)
start <- end - as.difftime(1, units = 'weeks')
readings.lastweek <- subset(readings, datetime > start)

# subset(features, space == "Hackerspace Bremen e.V." & day == 'Thursday')
predict <- function(readings, the.space, future, radius = as.difftime(0.25, units = 'hours')) {
  past <- future - as.difftime(1, units = 'weeks')
  df <- subset(readings, (space == the.space) &
                         (datetime > (past - radius)) &
                         (datetime < (past + radius)))
  open <- sum(df$open == 'TRUE')
  closed <- sum(df$open == 'FALSE')
  n <- open + closed
  if (n == 0) {
    cat('I could not estimate the probability because there were no appropriate readings last week.\n')
    NA
  } else {
    p <- open / n
    cat(paste('I give', the.space, 'a', p, 'chance of being open. This probability is based on', n, 'readings for', the.space, 'a week earlier.\n'))
    p
  }
}

# library(dplyr)
# 
# features <- readings.lastweek %>%
#     group_by(space, day, hour) %>%
#     summarize(yes = sum(open == 'TRUE'),
#               no = sum(open == 'FALSE'),
#               broken = sum(open == 'NA'))
