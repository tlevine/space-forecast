#!/usr/bin/env Rscript
library(dplyr)
library(ggplot2)

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

readings.grouped <- readings %>%
                    group_by(space, date = as.Date(datetime), hour) %>%
                      summarize(yes = sum(open == 'TRUE'),
                                no = sum(open == 'FALSE'),
                                broken = sum(open == 'NA'),
                                n = n())
# readings.grouped$p <- ifelse(df$n > 0, df$yes / df$n, NA)

plot.space <- function(readings.grouped, the.space,
                       past = as.POSIXct(Sys.time()) - as.difftime(1, units = 'weeks'),
                       radius = as.difftime(12, units = 'hours')) {
    as.POSIXct(date)
    as.POSIXct(past)
  df <- subset(readings.grouped, 
    space == the.space & abs(difftime(date, past, units = 'days')) < 1)

  ggplot(df) +
    aes(x = paste0(date, ', ', hour, ':00'),
        y = yes/n) +
    geom_bar(stat = 'identity')
}
