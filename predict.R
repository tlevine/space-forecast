#!/usr/bin/env Rscript

# Load spreadsheet
readings <- read.csv('space.csv', colClasses = c('factor', 'numeric', 'factor'),
                     na.strings = NULL)
readings$timestamp <- as.POSIXct(readings$timestamp, origin = '1970-01-01')
names(readings)[2] <- 'datetime'

# Subset to last week
end <- max(readings$datetime)
start <- end - as.difftime(1, units = 'weeks')
readings.lastweek <- subset(readings, datetime > start)
