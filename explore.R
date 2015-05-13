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
  summarize(p.open = mean(open == 'TRUE'),
            p.closed = mean(open == 'FALSE'),
            p.na = mean(open == 'NA'),
            count = n())

#' Are the two times within an hour of each other?
one.hour <- function(a, b)
  abs(difftime(a, b, units = 'hours')) < 1

#' Combine historical with whether the space was open in the previous hour?
#' @return data.frame with one row per space
space.features <- function(readings, present = as.POSIXct(Sys.time())) {
  readings.present <- readings %>%
    filter(datetime <= present) %>%
    group_by(hackerspace) %>%
    summarize(datetime = max(datetime)) %>%
    mutate(is.current = one.hour(datetime, present)) %>%
    left_join(readings)

  readings.past <- subset(readings, datetime < present)

  if (nrow(readings.past) == 0) {
    stop('No historical data')
  }


  readings.present %>%
    inner_join(space.week.features(readings.past), by = 'hackerspace')
  #print(space.week.features(readings.present))
}

print(space.features(readings, present = as.POSIXct("2013-07-07 12:38:37 UTC")))
