#!/usr/bin/env Rscript

# Load spreadsheet
space <- read.csv('space.csv', colClasses = c('character', 'numeric', 'logical'))
space$timestamp <- as.POSIXct(space$timestamp, origin = '1970-01-01')
names(space)[2] <- 'datetime'
