
# ggplot histogram

library(ggplot2)
library(dplyr)
library(lubridate)
library(chron)

dta <- readr::read_csv('./data/bike_collisions.csv') %>%
  filter(as.numeric(ACCTIME) < 2400) %>%
  mutate(time = as.character(ACCTIME)) %>%
  mutate(tt = paste("2000-01-01", time)) %>%
  mutate(dt = as.POSIXct(paste("2000-01-01", time), format='%Y-%m-%d %H%M')) %>%
  select(ACCTIME, time, tt, dt)
  
ggplot(dta, aes(x=dt)) + 
  geom_bar(stat="count") + 
  scale_x_datetime(date_breaks = "2 hour", 
                   labels = date_format("%H:%M"))
# g <- ggplot(dta, aes(as.character(ACCTIME))) + geom_histogram()





time <- c("22:12", "11:04", "00:04", "23:45", "12:04", "16:33")
type <- c("Foo", "Bar", "Foo", "Foo", "Foo", "Bar")

data <- data.frame(time, type) %>%
  mutate(dt = as.POSIXct(paste("2000-01-01", time)))

ggplot(data, aes(x=dt)) + geom_bar() + 
  scale_x_datetime(date_breaks = "2 hour", 
                   labels = date_format("%H:%M"))
  

qplot(data$type, strptime(data$time, "%H:%M", tz = "UTC")) +
  scale_y_datetime(date_breaks="1 hour", date_labels="%H:%M", expand = c(0,0)) +
  xlab("Type") + ylab("Time")

qplot(data$type, dt) +
  scale_y_datetime(date_breaks="1 hour", date_labels="%H:%M", expand = c(0,0)) +
  xlab("Type") + ylab("Time")


library(ggplot2)
library(lubridate)
dd <- read.table(text= "
Date      Closed
                   2010-07-19    0.0808
                   2010-07-20    0.7547
                   2010-07-21    0.8547", stringsAsFactors=FALSE, header = TRUE)

dd$Date <- ymd(dd$Date)
ggplot(dd, aes(x=Date, y=Closed)) + 
  geom_line() + scale_x_date(date_labels = "%b-%d-%Y")




library(tidyverse)
library(scales)
# Mock data
df <- data.frame(time = paste(sprintf("%02d", sample(1:23, 50, replace = TRUE)), 
                              sprintf("%02d", sample(1:59, 50, replace = TRUE)), 
                              sprintf("%02d", sample(1:59, 50, replace = TRUE)), 
                              sep = ":"),
                 y_value = sample(1:200, 50))
head(df$time)
df %>%
  mutate(., date = as.POSIXct(paste("2000-01-01", time))) %>%
  ggplot(., aes(x = date, y = y_value)) + 
  geom_point() + 
  scale_x_datetime(date_breaks = "4 hours", 
                   date_minor_breaks = "1 hour",
                   labels = date_format("%H:%M:%S")) # format defined here
