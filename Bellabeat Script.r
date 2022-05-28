## Setting Up Environment
install.packages("janitor")
install.packages("lubridate")
install.packages("skimr")
install.packages("corrplot")
library(tidyverse)
library(janitor)
library(dplyr)
library(readr)
library(ggplot2)
library(skimr)
library(lubridate)
library(corrplot)

## Linking Data
activity <- read_csv("dailyActivity_merged.csv")
steps <- read_csv("dailySteps_merged.csv")
heartrate <- read_csv("heartrate_seconds_merged.csv")
calories <- read_csv("hourlyCalories_merged.csv")
intensities <- read_csv("hourlyIntensities_merged.csv")
sleep <- read_csv("sleepDay_merged.csv")
weight <- read_csv("weightLogInfo_merged.csv")

## Inspecting Data
colnames(daily_activity)
head(daily_activity)
colnames(hourly_intensities)
head(hourly_calories)
colnames(heartrate)
glimpse(activity)
print(activity$Id)

## Formatting Data
intensities$ActivityHour=as.POSIXct(intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")
# calories
calories$ActivityHour=as.POSIXct(calories$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
calories$date <- format(calories$ActivityHour, format = "%m/%d/%y")
# activity
activity$ActivityDate=as.POSIXct(activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
activity$date <- format(activity$ActivityDate, format = "%m/%d/%y")
# sleep
sleep$SleepDay=as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")

## Finding out how many participants in each set
n_distinct(activity$Id)
n_distinct(calories$Id)
n_distinct(intensities$Id)
n_distinct(sleep$Id)
n_distinct(weight$Id)
n_distinct(heartrate$Id)

# Weight & HR are too small to use

## Summaries of data sets
# activity
activity %>%  
  select(TotalSteps,TotalDistance, SedentaryMinutes, Calories) %>%
  summary()
# explore num of active minutes per category
activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()
# steps
activity %>%
  select(TotalSteps, TotalDistance) %>%
  summary()
# calories
calories %>%
  select(Calories) %>%
  summary()
# sleep
sleep %>%
  select(TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()


## Intensities over the day plot 
int_new <- intensities %>%
  group_by(time) %>%
  drop_na() %>%
  summarise(mean_total_int = mean(TotalIntensity))

ggplot(data=int_new, aes(x=time, y=mean_total_int)) + geom_histogram(stat = "identity", fill='darkred') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Average Total Intensity During the Day", x = "Time of Day", y = "Mean Intensities")

## Calories & Steps
ggplot(data=activity, aes(x=TotalSteps, y=Calories)) + 
  geom_point() + geom_smooth() + labs(title="Total Steps vs. Calories", x = "Steps")

## Calories & Distance
ggplot(data=activity, aes(x=TotalDistance, y=Calories)) + 
  geom_point() + geom_smooth() + labs(title="Total Distance vs. Calories", x = "Distance in kM")

## Sleep v. Time in Bed
ggplot(data=sleep, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + 
  geom_point()+ labs(title="Total Minutes Asleep vs. Total Minutes in Bed", x = "Minutes Asleep", y = "Minutes in Bed")

## Steps v. Sleep
merged_data <- merge(sleep, activity, by=c('Id', 'date'))

ggplot(data=merged_data, aes(x=TotalMinutesAsleep, y=TotalSteps)) + 
  geom_point()+ labs(title="Minutes Asleep vs. Steps", x = "Minutes Asleep", y = "Total Steps")

## Sedentary v. Calories
ggplot(data=activity, aes(x=SedentaryMinutes, y=Calories)) + 
  geom_point()+ labs(title="Sedentary Time v. Calories", x = "Sedentary Minutes", y = "Calories")

## Sleep v. Intensity
sleep_intensity <- merge(sleep, intensities, by=c('Id', 'date'))

ggplot(data=sleep_intensity, aes(x=TotalIntensity, y=TotalMinutesAsleep)) + 
  geom_point()+ labs(title="Minutes of Activity v. Sleep", x = "Minutes of Activity", y = "Minutes Asleep")

## Correlation chart
#main
activity_df <- activity %>%
  select(TotalSteps, TotalDistance, TrackerDistance, VeryActiveDistance, ModeratelyActiveDistance, LightActiveDistance, SedentaryActiveDistance, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes, Calories) 
corrplot(cor(activity_df), method = 'circle')

#Intensity v. Sedentary (Minutes)
activity_in <- activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes, Calories) 
corrplot(cor(activity_in), method = 'circle')

## Light activity v. Calories
ggplot(activity, aes(LightActiveDistance, Calories)) + 
  geom_point() +
  geom_smooth() +  
  labs(title = "Lightly Active Distance v Calories", x = "Lightly Active Distance", y = "Calories")

## Calories burned per day
