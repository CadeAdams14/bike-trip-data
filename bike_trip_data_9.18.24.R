# download and install packages
install.packages(c('readr'
                   , 'tidyverse'
                   , 'janitor'
                   , 'lubridate'
                   , 'skimr'));

install.packages("devtools")
devtools::install_github("ropensci/skimr")

library(readr)
library(tidyverse)
library(janitor)
library(lubridate)
library(skimr)
library(devtools)

# load datasets 
jan23 <- read_csv("202301-divvy-tripdata.csv") %>%
  clean_names()
feb23 <- read_csv("202302-divvy-tripdata.csv") %>%
  clean_names()
mar23 <- read_csv("202303-divvy-tripdata.csv") %>%
  clean_names()
apr23 <- read_csv("202304-divvy-tripdata.csv") %>%
  clean_names()
may23 <- read_csv("202305-divvy-tripdata.csv") %>%
  clean_names()
jun23 <- read_csv("202306-divvy-tripdata.csv") %>%
  clean_names()
jul23 <- read_csv("202307-divvy-tripdata.csv") %>%
  clean_names()
aug23 <- read_csv("202308-divvy-tripdata.csv") %>%
  clean_names()
sep23 <- read_csv("202309-divvy-tripdata.csv") %>%
  clean_names()
oct23 <- read_csv("202310-divvy-tripdata.csv") %>%
  clean_names()
nov23 <- read_csv("202311-divvy-tripdata.csv") %>%
  clean_names()
dec23 <- read_csv("202312-divvy-tripdata.csv") %>%
  clean_names()

# set the working directory to the correct file.
getwd()
setwd('/Users/cadeadams/Downloads/capstone_project_data')

# view datasets
skim_without_charts(jan23)
skim_without_charts(feb23)
skim_without_charts(mar23)
skim_without_charts(apr23)
skim_without_charts(may23)
skim_without_charts(jun23)
skim_without_charts(jul23)
skim_without_charts(aug23)
skim_without_charts(sep23)
skim_without_charts(oct23)
skim_without_charts(nov23)
skim_without_charts(dec23)

# check if the fields match
compare_df_cols_same(jan23, feb23
                     , mar23, apr23
                     , may23, jun23
                     , jul23, aug23
                     , sep23, oct23
                     , nov23, dec23)

# combine the datasets into one single 
# dataset and save it to disk
bike_data <- bind_rows(jan23, feb23
                       , mar23, apr23
                       , may23, jun23
                       , jul23, aug23
                       , sep23, oct23
                       , nov23, dec23)

save(bike_data, file = "bike_data.Rdata")

# take a quick look at the dataset
summary(bike_data)

# check for duplicates in the data
bike_data %>%
  get_dupes(ride_id)

# add columns for day, month, year, and day of week. Sunday=1
bike_data$year <- year(bike_data$started_at)
bike_data$month <- month(bike_data$started_at)
bike_data$day <- day(bike_data$started_at)
bike_data$day_of_week <- wday(bike_data$started_at)

# add ride_length column
bike_data <- bike_data %>%
  mutate(ride_length = ended_at - started_at)

# remove unnecessary columns
bike_data <- 
  select(bike_data, c(rideable_type, started_at
                      , member_casual, year
                      , month, day
                      , day_of_week, ride_length))

# save the data
write.csv(bike_data
          , '/Users/cadeadams/Downloads/capstone_project_data/bike_data_clean.csv'
          , row.names = TRUE)


# create visualization showing the mean ride length between members
# and casual riders

bike_data %>%  
  group_by(member_casual) %>%  
  summarize(Count = n()) %>% 
ggplot(mapping = aes(x = 'ride_length', y = Count, fill = member_casual)) +
  geom_bar(position='dodge', stat='summary', fun='mean') +
  labs(x = 'Members vs. Casual Riders'
       , y = 'Number of Rides'
       , title = 'Number of Rides Members vs. Casual Riders')

# create visualization showing total ride count by day of week

bike_data %>%  
  group_by(day_of_week) %>%  
  summarize(Count = n()) %>% 
  ggplot(aes(x=day_of_week, y=Count)) + 
  geom_bar(stat='identity'
           , position= "dodge"
           , fill = 'lightblue') + 
  labs(y= "Number of Rides"
    , x = "Day of Week"
    ,title ="Total Ride Count by Day of Week")

# create visualization showing total ride count for customers and members for day of week
bike_data %>%  
  group_by(member_casual, day_of_week) %>%  
  summarize(Count = n()) %>% 
  ggplot(aes(x=day_of_week, y=Count, fill=member_casual)) + 
  geom_bar(stat='identity', position= "dodge") +
  labs(x = 'Day of Week'
       , y = 'Number of Rides'
       , title = 'Ride Count for Days of the Week')

# find ride length based on both members and casual riders
bike_data %>% 
  summarise(mean = mean(ride_length))

bike_data %>%
  filter(str_detect(member_casual, 'member')) %>%
  summarise(mean = mean(ride_length))

bike_data %>%
  filter(str_detect(member_casual, 'casual')) %>%
  summarise(mean = mean(ride_length))

# save the plots
ggsave('number_of_rides.png')
ggsave('ride_count_day_of_week.png')


