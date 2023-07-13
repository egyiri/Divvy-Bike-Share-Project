#Loading the required packages. 

library(tidyverse) #For core importation and data wrangling functions
library(lubridate) #For time and date functions
library(ggplot2)   #For data visualizations
getwd()            #Spells out your working directory

#Importing the data into different data frames
q2_2019 <- read_csv(file.choose())
q3_2019 <- read_csv(file.choose())
q4_2019 <- read_csv(file.choose())
q1_2020 <- read_csv(file.choose())

#EXPLORING THE DATA...
unique(q2_2019)
unique(q3_2019$from_station_name)
unique(q4_2019$usertype)
unique(q1_2020$rideable_type)
#The unique function finds all unique values in a certain column

class(q2_2019)
class(q3_2019)
class(q4_2019)
#This class function reveals that it's a data frame. 
class(q1_2020$rideable_type)
#The class function also helps to find the data type of the selected column.

colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
View(colnames(q1_2020))
#These help identify the names of the columns in each of the data frames. 
#The names need to match so that the bind_rows function can be used. 

ncol(q2_2019)
ncol(q3_2019)
ncol(q4_2019)
ncol(q1_2020)
#The number of columns of each data frame is 12, except the q1_2020 df, which is 13. 

#CLEANING THE DATA 
#The column names for all the dfs have to be changed to suit the q1_2020 df. They seem more consistent and readable
q4_2019 <- rename(q4_2019,
                  ride_id = trip_id,
                  rideable_type = bikeid,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_name = from_station_name,
                  start_station_id = from_station_id,
                  end_station_name = to_station_name,
                  end_station_id = to_station_id,
                  member_casual = usertype)

q2_2019 <- rename(q2_2019,
                  ride_id = `01 - Rental Details Rental ID`,
                  rideable_type = `01 - Rental Details Bike ID`,
                  start_time = `01 - Rental Details Local Start Time`,
                  ended_at = `01 - Rental Details Local End Time`,
                  start_station_name = `03 - Rental Start Station Name`,
                  start_station_id = `03 - Rental Start Station ID`,
                  end_station_name = `02 - Rental End Station Name`,
                  end_station_id = `02 - Rental End Station ID`,
                  tripduration = `01 - Rental Details Duration In Seconds Uncapped`, 
                  member_casual = `User Type`)

q2_2019 <-rename(q2_2019, 
                 ride_id = trip_id,
                 started_at = start_time,
                 ended_at = end_time,
                 rideable_type = bikeid, 
                 start_station_id = from_station_id,
                 start_station_name = from_station_name,
                 end_station_id = to_station_id,
                 end_station_name = to_station_name,
                 member_casual = usertype)

colnames(q2_2019)


q3_2019 <- rename(q3_2019,
                  ride_id = trip_id,
                  rideable_type = bikeid,
                  start_time = started_at,
                  ended_at = end_time,
                  start_station_name = from_station_name,
                  start_station_id = from_station_id,
                  end_station_name = to_station_name,
                  end_station_id = to_station_id,
                  member_casual = usertype)


View(q2_2019)
View(q3_2019)
View(q4_2019)
View(q1_2020)
#The changes have been effected

#The data type of the "rideable_type" and the "ride_id" variables in the q2_2019, q3_2019, q4_2019 are all numeric
class(q2_2019$rideable_type)
class(q3_2019$rideable_type)
class(q4_2019$rideable_type)

class(q2_2019$ride_id)
class(q3_2019$ride_id)
class(q4_2019$ride_id)

class(q1_2020$ride_id)
class(q1_2020$ rideable_type)
#They have to be made characters so they can stack well on the q1_2020 data frame. 

q4_2019 <- mutate(q4_2019, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

q3_2019 <- mutate(q3_2019, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

q2_2019 <- mutate(q2_2019, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

ncol(q2_2019)
ncol(q3_2019)
ncol(q4_2019)
ncol(q1_2020)


colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)

#Now arranging all the columns in the q1_2020 data frame same order so enable easy stacking
q1_2020 <- q1_2020 %>% 
  select(ride_id, 
         started_at,
         ended_at, 
         rideable_type, 
         start_station_id, 
         start_station_name, 
         end_station_id, 
         end_station_name, 
         member_casual)

#Now, we stack!
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)


#MORE DATA CLEANING...
all_trips %>% 
  select(gender) %>% 
  filter(!complete.cases(.))

all_trips %>% 
  select(birthyear) %>% 
  filter(!complete.cases(.))
#Using the above codes, I found out that both the gender and birthyear variables had over one million null values. 
#They had to be deleted. 

all_trips <- subset(all_trips, select = -c(tripduration, gender, birthyear))
View(all_trips)
#Now let's have a better look at our new all_trips data frame
dim(all_trips)
glimpse(all_trips)
skim_without_charts(all_trips)
str(all_trips)
head(all_trips)
summary(all_trips)

unique(all_trips$member_casual)
#The member_casual variable had four unique observations instead of two. 
#Therefore, we're going to downsize them to a Boolean. 

all_trips <- all_trips %>% 
  mutate(member_casual = recode(member_casual,
                                "Subscriber" = "member",
                                "Customer" = "casual"))
table(all_trips$member_casual)

#Now let's format the 
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

View(all_trips)
str(all_trips)

class(all_trips$started_at)

#Now a new tripduration has to be calculated
all_trips$ride_duration <- 
  difftime(all_trips$ended_at, all_trips$started_at)

#The ride_duration variable has to be numeric formatted. 
all_trips$ride_duration <- as.numeric(as.character(all_trips$ride_duration))
is.numeric(all_trips$ride_duration)

#Now let's have a look 
str(all_trips$ride_duration)
class(all_trips$ride_duration)

#Also,...
#From the code below we realize that there are 130 observations from the ride_duration variable that are negative.
#There can't be any negative ride durations. 
all_trips %>% 
     select(ride_duration, start_station_name) %>% 
     filter(ride_duration < 0 | start_station_name == "HQ QR") %>% 
View()

#Now let's create a new data frame that excludes negative durations and the HQ QR variable. 
all_trips_2 <- 
  all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_duration < 0),]

#Now that the data is clean, let;s perform some basic summary statistics
summary(all_trips_2$ride_duration)
max(all_trips_2$ride_duration)
mean(all_trips_2$ride_duration)
median(all_trips_2$ride_duration)
min(all_trips_2$ride_duration)

#Finding the average ride_duration for each category of clients
all_trips_2 %>% 
  select(member_casual, ride_duration) %>% 
  group_by(member_casual) %>% 
  summarize(av_rides = mean(ride_duration)) %>% 
View()

#Finding the average ride_duration for each category of clients grouped by month too
all_trips_2 %>% 
  select(member_casual, ride_duration, month) %>% 
  group_by(member_casual, month) %>% 
  summarise(av_rides = mean(ride_duration))%>%
  arrange(month)
  View()

all_trips_2$day_of_week <- 
   ordered(all_trips_2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#Average client type ride duration by weekday 
all_trips_2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_duration)) %>%
  arrange(member_casual, weekday)

#Number of Rides by Weekday
all_trips_2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_duration)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Rides by Weekday")

#Average duration on rides by client type 
all_trips_2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_duration)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


View(all_trips_2)
write.csv(all_trips_2, "C:\\Users\\Bright\\OneDrive\\Projects\\trips_project.csv", row.names = FALSE)
