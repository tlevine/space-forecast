#!/usr/bin/env Rscript
library(dplyr)
library(ggplot2)
library(shiny)

# Load spreadsheet.
if (!('readings' %in% ls())) {
  readings <- read.csv('space.csv', colClasses = c('factor', 'numeric', 'factor'),
                       na.strings = NULL)
  readings$timestamp <- as.POSIXct(readings$timestamp, origin = '1970-01-01')
  names(readings)[2] <- 'datetime'
}

# Bucket by hour.
readings$day <- factor(weekdays(readings$datetime),
                       levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday',
                                  'Thursday', 'Friday', 'Saturday'))
readings$hour <- as.numeric(strftime(readings$datetime, '%H'))

# Subset to last week.
end <- max(readings$datetime)
start <- end - as.difftime(1, units = 'weeks')
readings.lastweek <- subset(readings, datetime > start)

if (!('readings.grouped' %in% ls())) {
  readings.grouped <- readings %>%
                      group_by(space, date = as.POSIXct(as.Date(datetime)), hour) %>%
                        summarize(yes = sum(open == 'TRUE'),
                                  no = sum(open == 'FALSE'),
                                  broken = sum(open == 'NA'),
                                  n = n()) %>%
                        mutate(datetime = as.POSIXct(paste0(date, ' ', hour, ':00:00')),
                               p = yes/max(yes + no, 1))
}

plot.space <- function(readings.grouped, the.space,
                       past = as.POSIXct(Sys.time()) - as.difftime(1, units = 'weeks')) {
  
  df <- subset(readings.grouped, 
    space == the.space & abs(difftime(readings.grouped$date, past, units = 'days')) < 1)

  ggplot(df) +
    aes(x = datetime, y = p) +
    geom_bar(stat = 'identity')
}

app <- function(input, output) {
  output$distPlot <- renderPlot(plot.space(readings.grouped,
                                           input$space,
                                           input$datetime))
}
# shinyServer(app)
