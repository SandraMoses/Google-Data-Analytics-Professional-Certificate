library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data


setwd("E:/coursera/Case Study 1/extracted")

q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

#The columns names don't match so we have to fix that before joining
#the latest and most descriptive names are those of q1_2020 so we will change all others accordingly
colnames(q1_2020)

(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))

(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

#inspect the dataframes
str(q1_2020)
str(q2_2019)
str(q3_2019)
str(q4_2019)

q2_2019 <- mutate(q2_2019,
                  ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

q3_2019 <- mutate(q3_2019,
                  ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

q4_2019 <- mutate(q4_2019,
                  ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

colnames(all_trips)
# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- select(all_trips, -c("01 - Rental Details Duration In Seconds Uncapped",
                                           "05 - Member Details Member Birthday Year" ,
                                           tripduration,                                    
                                           gender, 
                                           "Member Gender",
                                           birthyear,                                       
                                           start_lat,                                       
                                           start_lng,                                       
                                           end_lat,                                         
                                           end_lng))

# Inspect the new table that has been created
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
str(all_trips)
summary(all_trips)

#few problems:

#Problem 1: In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). 
#We will need to consolidate that from four to two labels.
table(all_trips$member_casual)

all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual,
                         "Subscriber"="member",
                         "Customer"="casual"))
                         
table(all_trips$member_casual)

#Problem 2: Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
head(all_trips$started_at)

all_trips$date = as.Date(all_trips$started_at)
all_trips$day = format(as.Date(all_trips$date, "%d"))
all_trips$month = format(all_trips$date, "%m")
all_trips$year = format(all_trips$date, "%y")
all_trips$day_of_week = format(all_trips$date, "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length = as.numeric(difftime(all_trips$ended_at, all_trips$started_at))

str(all_trips)
summary(all_trips)


# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
all_trips_v2 <- all_trips[!(all_trips$ride_length<0 | all_trips$start_station_name == "HQ QR"),]

summary(all_trips_v2$ride_length)

#Now that everything is ready, we can begin solving our problem
#How do annual members and casual riders use Cyclistic bikes differently?

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- factor(all_trips_v2$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

#to prevent the e notation in r graphs
options(scipen = 999)

# Let's visualize the number of rides by rider type and weekday
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label=TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(average_duration = mean(ride_length)) %>%
  arrange(weekday) %>%
  ggplot(aes(x=weekday, y=average_duration, fill=member_casual)) +
  geom_col(position="dodge") +
  scale_y_continuous(labels = scales::comma)

#visualization for ride count per month
all_trips_v2 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x=month, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge")+
  scale_y_continuous(labels = scales::comma)

#visualization for average duration per month
all_trips_v2 %>%
  group_by(member_casual, month) %>%
  summarise(average_duration = mean(ride_length)) %>%
  ggplot(aes(x=month, y=average_duration, fill=member_casual)) +
  geom_col(position = "dodge")+
  scale_y_continuous(labels = scales::comma)

#seasonal trends
#making new column for season
all_trips_v2$season <- ifelse(all_trips_v2$month %in% c("06", "07", "08"), "Summer",
                             ifelse(all_trips_v2$month %in% c("09", "10", "11"), "Fall",
                                    ifelse(all_trips_v2$month %in% c("12", "01", "02"), "Winter",
                                          ifelse(all_trips_v2$month %in% c("03", "04", "05"), "Spring", NA
                ))))


all_trips_v2 %>%
  group_by(member_casual, season) %>%
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>%
  arrange(member_casual, season)

#ride count by season
all_trips_v2 %>%
  group_by(member_casual, season) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = season, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma)


# average duration by season
all_trips_v2 %>%
  group_by(member_casual, season) %>%
  summarise(average_duration = mean(ride_length)) %>%
  ggplot(aes(x=season, y=average_duration, fill=member_casual)) +
  geom_col(position="dodge") +
  scale_y_continuous(labels = scales::comma)

#stations Trends
all_trips_v2 %>% count(start_station_name, member_casual)
all_trips_v2 %>% count(end_station_name, member_casual)

stations_casual<- all_trips_v2 %>%
  count(start_station_name, member_casual) %>%
  filter(member_casual == 'casual') %>%
  arrange(desc(n)) %>%
  slice_head(n=10)


ggplot(data = stations_casual, aes(x = start_station_name, y = n)) + 
  geom_bar(stat = "identity") +
  ggtitle('Top 10 Start Stations - Casuals') + 
  xlab('Station Name') + ylab('Ride Count') + 
  coord_flip()

#The same for members
stations_member<- all_trips_v2 %>%
  count(start_station_name, member_casual) %>%
  filter(member_casual == 'member') %>%
  arrange(desc(n)) %>%
  slice_head(n=10)


ggplot(data = stations_member, aes(x = start_station_name, y = n)) + 
  geom_bar(stat = "identity") +
  ggtitle('Top 10 Start Stations - Members') + 
  xlab('Station Name') + ylab('Ride Count') + 
  coord_flip()

#Popular end stations
#casuals
end_stations_casual<- all_trips_v2 %>%
  count(end_station_name, member_casual) %>%
  filter(member_casual == 'casual') %>%
  arrange(desc(n)) %>%
  slice_head(n=10)


ggplot(data = end_stations_casual, aes(x = end_station_name, y = n)) + 
  geom_bar(stat = "identity") +
  ggtitle('Top 10 End Stations - Casuals') + 
  xlab('Station Name') + ylab('Ride Count') + 
  coord_flip()

#The same for end stations for members
end_stations_member<- all_trips_v2 %>%
  count(end_station_name, member_casual) %>%
  filter(member_casual == 'member') %>%
  arrange(desc(n)) %>%
  slice_head(n=10)


ggplot(data = end_stations_member, aes(x = end_station_name, y = n)) + 
  geom_bar(stat = "identity") +
  ggtitle('Top 10 End Stations - Members') + 
  xlab('Station Name') + ylab('Ride Count') + 
  coord_flip()
