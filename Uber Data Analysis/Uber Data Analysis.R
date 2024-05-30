# Title: Uber Analysis
# Author: mikportfolio
# Date: 29/05/24
# Dataset: Uber data from 2014
# Objectives: To evaluate Uber performance


###############################
## Processing and Cleaning   ##
###############################

# Step 1: import library
install.packages(ggplot2)
install.packages("ggthemes")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("DT")
install.packages("scales")
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyverse) # metapackage of all tidyverse packages
library(DT)
library(scales)

# Step 2: import dataset(s), merge and inspect
Uber_201404 <- read.csv("C:\\Users\\mudrik\\Downloads\\Uber Datasets 2014\\uber-raw-data-apr14.csv")
Uber_201405 <- read.csv("C:\\Users\\mudrik\\Downloads\\Uber Datasets 2014\\uber-raw-data-may14.csv")
Uber_201406 <- read.csv("C:\\Users\\mudrik\\Downloads\\Uber Datasets 2014\\uber-raw-data-jun14.csv")
Uber_201407 <- read.csv("C:\\Users\\mudrik\\Downloads\\Uber Datasets 2014\\uber-raw-data-jul14.csv")
Uber_201408 <- read.csv("C:\\Users\\mudrik\\Downloads\\Uber Datasets 2014\\uber-raw-data-aug14.csv")
Uber_201409 <- read.csv("C:\\Users\\mudrik\\Downloads\\Uber Datasets 2014\\uber-raw-data-sep14.csv")

Uber_All_2014 <- bind_rows(Uber_201404,
                           Uber_201405,
                           Uber_201406,
                           Uber_201407,
                           Uber_201408,
                           Uber_201409)
head(Uber_All_2014)
glimpse(Uber_All_2014)

# Step 3: clean data and assign day, month, year, Day_of_Week
Uber_All_2014$Date.Time <- as.POSIXct(Uber_All_2014$Date.Time, 
                                      format="%m/%d/%Y %H:%M:%S")
Uber_All_2014$Time <- format(as.POSIXct(Uber_All_2014$Date.Time, 
                                        format = "%m/%d/%Y %H:%M:%S"), 
                             format="%H:%M:%S")
Uber_All_2014$Date.Time <- ymd_hms(Uber_All_2014$Date.Time)

Uber_All_2014 <- Uber_All_2014 %>%
  dplyr::mutate(Year = lubridate::year(Date.Time), 
                Month = lubridate::month(Date.Time), 
                Day = lubridate::day(Date.Time))

Uber_All_2014$Year <- as.factor(Uber_All_2014$Year)
Uber_All_2014$Month <- as.factor(Uber_All_2014$Month)
Uber_All_2014$Day <- as.factor(Uber_All_2014$Day)


Uber_All_2014$Day_of_Week <- factor(wday(Uber_All_2014$Date.Time, 
                                         label=TRUE))

# Step 4: add time variables 
Uber_All_2014$Second = factor(second(hms(Uber_All_2014$Time)))
Uber_All_2014$Minute = factor(minute(hms(Uber_All_2014$Time)))
Uber_All_2014$Hour = factor(hour(hms(Uber_All_2014$Time)))






###############################
## Bar Chart Visualizations  ##
###############################

# Step 5: visualize trips in Hour (of day) in a searchable table using datatable
Uber_All_2014 %>% 
  group_by(Hour) %>% 
  dplyr::summarize(Total = n()) %>% datatable()



# Step 6: Data Number of Rides by Hour in a Day
hourly_data <- Uber_All_2014 %>% 
  group_by(Hour) %>% 
  dplyr::summarize(Total = n())
ggplot(hourly_data, aes(x= Hour, 
                        y = Total)) + 
  geom_bar(stat="identity", 
           fill="steelblue", 
           color="red") + 
  #theme(legend.position = "none", 
  #      plot.title = element_text(hjust = 0.5), 
  #      plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(label = unit_format(unit = "K", scale = 1e-3, sep = " ")) +
  ylab("Total Number of Rides") +
  xlab("Hour in a day (24-Hour format)") +
  labs(title = "Trips by Hour of Day",
       subtitle = "Data obtained from April until September of 2014",
       caption = "By: mikportfolio, 29/05/24 1648PM")



# Step 7: Data Number of Rides by Hour in a Day facetted by Month
month_hour_data <- Uber_All_2014 %>% 
  group_by(Month, Hour) %>%  
  dplyr::summarize(Total = n()) %>% drop_na()
ggplot(month_hour_data, aes(Hour, Total, fill=Month)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~Month) +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(label = unit_format(unit = "K", scale = 1e-3, sep = " ")) +
  ylab("Total Number of Rides") +
  xlab("Hour in a day (24-Hour format)") +
  labs(title = "Trips by Hour of Day, faceted by Months",
       subtitle = "Data obtained from April until September of 2014",
       caption = "By: mikportfolio, 29/05/24 1800PM")



# Step 8: Data Number of Rides By Day of the Month, faceted by Months 
day_data <- Uber_All_2014 %>% 
  group_by(Month, Day) %>% 
  dplyr::summarize(Trips = n()) %>% drop_na()
View(day_data)
ggplot(day_data, aes(Day, Trips)) + 
  geom_bar(stat = "identity", 
           fill = "blue") +
  theme(legend.position = "none") + 
  facet_wrap(~Month) +
  scale_y_continuous(label = unit_format(unit = "K", scale = 1e-3, sep = " ")) +
  ylab("Total number of rides") +
  xlab("Day of the Month") +
  labs(title = "Number of Rides by Day of the Month, faceted by Months",
       subtitle = "Data obtained from April until September of 2014",
       caption = "By: mikportfolio, 29/05/24 1800PM")



# Step 9: Data Number of Rides By Day of the Week, faceted by Months 
day_month_data <- Uber_All_2014 %>% 
  group_by(Day_of_Week, Month) %>% 
  dplyr::summarize(Trips = n()) %>% drop_na()
day_month_data
ggplot(day_month_data, aes(x = Day_of_Week, 
                           y = Trips, fill = Month)) + 
  geom_bar(stat = "identity", 
           aes(fill = Month), 
           position = "dodge") + 
  facet_wrap(~Month) +
  scale_y_continuous(label = unit_format(unit = "K", scale = 1e-3, sep = " ")) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Total number of rides") +
  xlab("Day of the Week") +
  labs(title = "Number of Rides by Day of the Week, faceted by Months",
       subtitle = "Data obtained from April until September of 2014",
       caption = "By: mikportfolio, 29/05/24 1823PM")



# Step 10: Data Number of Rides By Months
month_data <- Uber_All_2014 %>% 
  group_by(Month) %>% 
  dplyr::summarize(Total = n()) %>% 
  drop_na()
month_data
ggplot(month_data, aes(x = Month, 
                       y = Total, 
                       fill = Month)) + 
  geom_bar(stat = "Identity") + 
  theme(legend.position = "none") + 
  scale_y_continuous(label = unit_format(unit = "K", scale = 1e-3, sep = " ")) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Total number of rides") +
  xlab("Day of the Week") +
  labs(title = "Number of Rides by Months",
       subtitle = "Data obtained from April until September of 2014",
       caption = "By: mikportfolio, 29/05/24 1826PM")
  


###############################
## Heat Map Visualizations   ##
###############################

# Step 11: Data Number of Rides, By Hour and Day of the Month
day_hour_data <- Uber_All_2014 %>% 
  group_by(Day, Hour) %>% 
  dplyr::summarize(Total = n()) %>% drop_na()
ggplot(day_hour_data, aes(Day, Hour, fill = Total)) + 
  geom_tile(color = "white") + 
  ylab("Hour (24-Hour Format)") +
  xlab("Day of the Month") +
  labs(title = "Number of Rides by Hour and Day of the Month",
       subtitle = "Data obtained from April until September of 2014",
       caption = "By: mikportfolio, 30/05/24 1026AM")


# Step 11: Data Number of Rides, By Month and Day of the Month
month_day_data <- Uber_All_2014 %>% 
  group_by(Month, Day) %>% 
  dplyr::summarize(Trips = n()) %>% drop_na()
ggplot(month_day_data, aes(Day, 
                           Month, 
                           fill = Trips)) + 
  geom_tile(color = "white") + 
  ylab("Month") +
  xlab("Day of the Month") +
  labs(title = "Number of Rides by Month and Day of the Month",
       subtitle = "Data obtained from April until September of 2014",
       caption = "By: mikportfolio, 30/05/24 1028AM")


# Step 11: Data Number of Rides, By Day of the Week and Month
ggplot(day_month_data, aes(Day_of_Week, 
                           Month, 
                           fill = Trips)) + 
  geom_tile(color = "white") + 
  ylab("Month") +
  xlab("Day of the Week") +
  labs(title = "Number of Rides by Month and Day of the Week",
       subtitle = "Data obtained from April until September of 2014",
       caption = "By: mikportfolio, 30/05/24 1030AM")






###############################
## Map Visualizations        ##
###############################

# Step 11: Set Map Constants for scaling purposes
min_lat <- 40 
max_lat <- 40.91
min_long <- -74.15
max_long <- -73.7004

# Step 11: Map visualization of rides in NYC
ggplot(Uber_All_2014, 
       aes(x=Lon, 
           y=Lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")



# Step 12: Map visualization of rides in NYC
ggplot(Uber_All_2014, 
       aes(x=Lon, 
           y=Lat, 
           color = Base)) +
  geom_point(size=1) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")

