#!/usr/bin/env Rscript

# Load spreadsheet
readings <- read.csv('space.csv', colClasses = c('factor', 'numeric', 'logical'))
readings$timestamp <- as.POSIXct(readings$timestamp, origin = '1970-01-01')
names(readings)[2] <- 'datetime'

# Stuff
library(dplyr)
space.days <- group_by(readings, hackerspace, day = weekdays(datetime),
                       hour = as.numeric(strftime(datetime, '%H')))
space.days <- select(space.days, hackerspace, day, hour, open)
historical <- summarize(space.days, open = mean(open), count = n())

# Combine historical with whether the space was open in the previous hour?

# 
set.seed(284488333)

