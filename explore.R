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
  summarize(open = sum(open == 'TRUE'),
            closed = sum(open == 'FALSE'),
            na = sum(open == 'NA'),
            n.obs = n())

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
    left_join(readings)

  readings.past <- readings %>%
    filter(datetime < present)

  if (nrow(readings.past) == 0) {
    stop('No historical data')
  }

  features.past <- space.week.features(readings.past) %>%
                   mutate(hist.open = open,
                          hist.closed = closed,
                          hist.na = na,
                          hist.total = n.obs) %>%
                   select(hackerspace, day, hour,
                          hist.open, hist.closed, hist.na, hist.total)

  readings.present %>%
    mutate(is.current = one.hour(datetime, present)) %>%
    select(hackerspace, is.current) %>%
    inner_join(space.week.features(readings.present), by = 'hackerspace') %>%
    mutate(cur.open = ifelse(is.current, open, 0),
           cur.closed = ifelse(is.current, closed, 0),
           cur.na = ifelse(is.current, na, 1)) %>%
    select(hackerspace, day, hour,
           cur.open, cur.closed, cur.na) %>%
    inner_join(features.past) %>%
    select(hackerspace,
           cur.open, cur.closed, cur.na,
           hist.open, hist.closed, hist.na, hist.total)
}

df <- space.features(readings, present = as.POSIXct("2013-07-07 12:38:37 UTC"))
print(df)
