install.packages("tidyverse")
install.packages("lubridate")
install.packages("skimr")

# import libraries

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(skimr)
getwd()
## Process the data
# load data sets

Dec_20 <- read.csv("December_20.csv")
Nov_20 <- read.csv("November_20.csv")
Jan_21 <- read.csv("January_21.csv")
Feb_21 <- read.csv("February_21.csv")
Mar_21 <- read.csv("March_21.csv")
Apr_21 <- read.csv("April_21.csv")
May_21 <- read.csv("May_21.csv")
Jun_21 <- read.csv("June_21.csv")
Jul_21 <- read.csv("July_21.csv")
Aug_21 <- read.csv("August_21.csv")
Sep_21 <- read.csv("September_21.csv")
Oct_21 <- read.csv("October_21.csv")

# Wrangling the data

colnames(Dec_20)
colnames(Nov_20)
colnames(Jan_21)
colnames(Feb_21)
colnames(Mar_21)
colnames(Apr_21)
colnames(May_21)
colnames(Jun_21)
colnames(Jul_21)
colnames(Aug_21)
colnames(Sep_21)
colnames(Oct_21)
# inspect the data 

glimpse(Nov_20)
glimpse(Dec_20)
glimpse(Jan_21)
glimpse(Feb_21)
glimpse(Mar_21)
glimpse(Apr_21)
glimpse(May_21)
glimpse(Jun_21)
glimpse(Jul_21)
glimpse(Aug_21)
glimpse(Sep_21)
glimpse(Oct_21)

# convert start_station_id and en_station_id for Nov_20from integer to character
# in order to combine the data sets

Nov_20$start_station_id <- as.character(Nov_20$start_station_id)

Nov_20$end_station_id <- as.character(Nov_20$end_station_id)

# combine the data sets into one data frame

Cyclistic <- bind_rows(Nov_20, Dec_20, Jan_21, Feb_21, Mar_21, Apr_21, 
                       May_21, Jun_21, Jul_21, Aug_21, Sep_21, Oct_21)

# remove unwanted columns

Cyclistic <- Cyclistic %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))
 
# add columns date, year, month, and day

Cyclistic$date <- as.Date(Cyclistic$started_at)
Cyclistic$year <- format(as.Date(Cyclistic$started_at),"%Y")
Cyclistic$month <- format(as.Date(Cyclistic$started_at),"%m")
Cyclistic$day <- format(as.Date(Cyclistic$started_at),"%a")

# add a calculated column ride_length

Cyclistic$ride_length <- difftime(Cyclistic$ended_at, Cyclistic$started_at,
                                   units = "mins")
# inspect the data frame

glimpse(Cyclistic)

# convert Start_station, end_station, ride_length to numeric

Cyclistic$start_station_id <- as.numeric(Cyclistic$start_station_id)
Cyclistic$end_station_id <- as.numeric(Cyclistic$end_station_id)

Cyclistic$ride_length <- as.numeric(Cyclistic$ride_length)

glimpse(Cyclistic)

Cyclistic$start_station_id <- gsub(",","",Cyclistic$start_station_id)
Cyclistic$end_station_id <- gsub(",","",Cyclistic$end_station_id)

# remove bad data from the data frame

cyclictic_new <- Cyclistic[!(Cyclistic$ride_length < 0),]

## Analyze the data
# conduct summary statictics on ride length
cyclictic_new %>%
  summarise(min_ride_lenght = min(ride_length),
            max_ride_length = max(ride_length),
            average_ride_length = mean(ride_length))
# comapre ride length by member and casual riders

aggregate(cyclictic_new$ride_length~cyclictic_new$member_casual, FUN = min)
aggregate(cyclictic_new$ride_length~cyclictic_new$member_casual, FUN = max)
aggregate(cyclictic_new$ride_length~cyclictic_new$member_casual, FUN = mean)
# average ride length between member and casual riders by day of week
cyclictic_new %>%
  group_by(member_casual, day) %>%
  summarise(average_duration_per_day = mean(ride_length), .groups = 'drop')
# order day of week
cyclictic_new$day <- ordered(cyclictic_new$day, levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))

# average ride length between member and casual riders by month

cyclictic_new %>%
  group_by(member_casual, month) %>%
  summarise(average_ride_length_by_month = mean(ride_length), .groups = 'drop')
cyclictic_new %>%
  group_by(member_casual, month) %>%
  summarise(average_ride_length_by_month = mean(ride_length))
# number of rides between member and casual riders by day

cyclictic_new %>%
  group_by(member_casual, day) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  arrange(day)
# number of rides between members by month

cyclictic_new %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  arrange(month)
# mode of bike preferred by member and casual riders

cyclictic_new %>%
  group_by(member_casual, rideable_type) %>%
  summarise(bike_preference = n(), .groups = 'drop') %>%
  arrange(rideable_type)
# average ride time for each bike type
cyclictic_new %>%
  group_by(member_casual, rideable_type) %>%
  summarise(bike_preference = mean(ride_length), .groups = 'drop') %>%
  arrange(rideable_type)
## bike type comparison by member and casual riders
# classic bike rides comparison by day of week

cyclictic_new %>%
  filter(rideable_type == "classic_bike") %>%
  group_by(member_casual, day) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  arrange(day)
# classic_bike ride time comparison by member casual

cyclictic_new %>%
  filter(rideable_type == "classic_bike") %>%
  group_by(member_casual, day) %>%
  summarise(average_ride_time = mean(ride_length), .groups = 'drop') %>%
  arrange(day)
# classic bike rides comparison by month

cyclictic_new %>%
  filter(rideable_type == "classic_bike") %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  arrange(month)
# classic bike average ride length by month

cyclictic_new %>%
  filter(rideable_type == "classic_bike") %>%
  group_by(member_casual, day) %>%
  summarise(number_of_rides = mean(ride_length), .groups = 'drop') %>%
  arrange(month)
## docked bike comparison
# docked bike ride per day for member casual

cyclictic_new %>%
  filter(rideable_type == "docked_bike", member_casual == "casual") %>%
  group_by(member_casual, day) %>%
  summarise(num_of_rides = n(), .groups = 'drop') %>%
  arrange(day)
# docked bike rides for members
cyclictic_new %>%
  filter(rideable_type == "docked_bike", member_casual == "member") %>%
  group_by(member_casual, day) %>%
  summarise(num_of_rides = n(), .groups = 'drop') %>%
  arrange(day)

## electric bike comparison
# by day of week for casual
cyclictic_new %>%
  filter(rideable_type == "electric_bike", member_casual == "casual") %>%
  group_by(member_casual, day) %>%
  summarise(num_of_rides = n(), .groups = 'drop') %>%
  arrange(day)
# by day of week for member
cyclictic_new %>%
  filter(rideable_type == "electric_bike", member_casual == "member") %>%
  group_by(member_casual, day) %>%
  summarise(num_of_rides = n(), .groups = 'drop') %>%
  arrange(day)
## summary by station
cyclictic_new %>%
  group_by(start_station_name, day) %>%
  summarise(num_of_rides = n(), .groups = 'drop') %>%
  arrange(start_station_name)

top_10_station <- cyclictic_new %>%
  group_by(start_station_name)%>%
  summarise(station_count = n(), .groups = 'drop')%>%
  arrange(desc(station_count))%>%
  head(10)

glimpse(cyclictic_new)
class(cyclictic_new$start_station_name)
cyclictic_new%>%
  summarise(not_null = n())

stations <- bind_rows(data.frame("start" = cyclictic_new$start_station_name,
                                 "rider_category" = cyclictic_new$member_casual,
                                 "end" = cyclictic_new$end_station_name))
