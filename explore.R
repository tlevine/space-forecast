#!/usr/bin/env Rscript

# Load spreadsheet
readings <- read.csv('space.csv', colClasses = c('factor', 'numeric', 'factor'),
                     na.strings = NULL)
readings$timestamp <- as.POSIXct(readings$timestamp, origin = '1970-01-01')
names(readings)[2] <- 'datetime'

# Stuff
library(dplyr)

#' Extract week-related features
space.week.features <- function(readings) readings %>%
  group_by(hackerspace, day = weekdays(datetime),
           hour = as.numeric(strftime(datetime, '%H'))) %>%
  select(hackerspace, day, hour, open) %>%
  summarize(open = mean(open), count = n())

#' Are the two times within an hour of each other?
one.hour <- function(a, b)
  abs(difftime(a, b, units = 'hours')) < 1

#' Combine historical with whether the space was open in the previous hour?
#' @return data.frame with one row per space
space.features <- function(readings, present = as.POSIXct(Sys.time())) {
  present.rounded <- min(readings$datetime[readings$datetime <= present])

  readings.present <- readings %>%
    group_by(hackerspace) %>%
    summarize(datetime = max(datetime)) %>%
    mutate(is.current = one.hour(datetime, present)) %>%
    left_join(readings)
  print(nrow(readings.present))

  readings.past <- subset(readings, datetime < present.rounded)

  if (nrow(readings.past) == 0) {
    warning('No historical data')
  }
  print(space.week.features(readings.past))
  print(space.week.features(readings.present))
}
