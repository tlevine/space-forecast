#!/usr/bin/env Rscript

# Load spreadsheet
readings <- read.csv('space.csv', colClasses = c('character', 'numeric', 'logical'))
readings$timestamp <- as.POSIXct(readings$timestamp, origin = '1970-01-01')
names(readings)[2] <- 'datetime'

# Stuff
library(dplyr)
spaces <- group_by(readings, hackerspace)
summarize(spaces, open = mean(open))

space.days <- group_by(readings, hackerspace, day = weekdays(datetime))
space.days <- select(space.days, hackerspace, day, open)
