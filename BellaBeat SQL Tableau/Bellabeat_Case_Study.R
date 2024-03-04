##load in all necessary packages

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

## import datasets i will be using

activity <- read.csv("Fitabase_Data/dailyActivity_merged.csv")
calories <- read.csv("Fitabase_Data/dailyCalories_merged.csv")
sleep <- read.csv("Fitabase_Data/sleepDay_merged.csv")
weight <- read.csv("Fitabase_Data/weightLogInfo_merged.csv")

##look over datasets to see any issues with data

View(activity)
View(calories)
View(sleep)
View(weight)

##found issues in formatting, the date and time stamps in the sleep and weight data sets

##sleep
sleep$SleepDay=as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")
##activity
activity$ActivityDate=as.POSIXct(activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
activity$date <- format(activity$ActivityDate, format = "%m/%d/%y")
##weight
weight$Date=as.POSIXct(weight$Date, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
weight$time <- format(weight$Date, format = "%H:%M:%S")
weight$date <- format(weight$Date, format = "%m/%d/%y")
##calories
calories$date <- format(calories$ActivityDay, format = "%m/%d/%y")

##seeing number of unique particpants in each dataset
n_distinct(activity$Id)
## 33
n_distinct(calories$Id)
## 33
n_distinct(sleep$Id)
## 24
n_distinct(weight$Id)
## 8 - not significant to make any recommendations based on this data

##lets look at summary statistics on each of these datasets

activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes, Calories) %>%
  summary()

activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()

calories %>%
  select(Calories) %>%
  summary()

sleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()

weight %>%
  select(WeightKg, BMI) %>%
  summary()

## some discoveries from the summaries: most of the participant are lightly active
## average sleep time is 7 hours -- look into average sleep length benefits
## average steps per day is 7638 -- need to look into if this is a good average
## average weight is 72kg - 158lbs

## now i want to visualize some data, I will begin by merging two datasets (activity and sleep) on  the columns ID and date which was previously created after converting the data to date time formatting.

merged_data <- merge(sleep, activity, by=c('Id', 'date'))
head(merged_data)

## making some visulizations

ggplot(data=activity, aes(x=TotalSteps, y=Calories)) + 
  geom_point(color='purple') + geom_smooth() + labs(title="Total Steps vs. Calories")
## this plot shows a positive correlation between Total Steps and Calories, which shows that the more active we are, the more calories we burn.

ggplot(data=sleep, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + 
  geom_point()+ labs(title="Total Minutes Asleep vs. Total Time in Bed")
## this plot shows the relationship between total minutes asleep and total time in bed. its linear so if bellabeats users want to improve their sleep, we should consider having a notification telling you to go to bed.

##now lets see the relationship between total minutes asleep and sedentary minutes
ggplot(data=merged_data, aes(x=TotalMinutesAsleep, y=SedentaryMinutes)) + 
  geom_point(color='red') + geom_smooth() +
  labs(title="Minutes Asleep vs. Sedentary Minutes")
## this plot shows that we see a negative relationship between sedentary minutes and sleep time, which means if users want to improve their sleep we can suggest that they reduce their sedentary time and have more active time.

## to summarize: collecting this data has helped us see insights on peoples daily life that we would never know. After analyzing the data ive come to conclusions that will help with the marketing strategy
## bellabeats should target an audience of women who spend alot of time in office, or at their computers (based on sedentary time data)
## bellabeats needs to provide these women the knowledge about developing healthy habits and motivate them to change their routine to better themselves.
## we need to see an increase in daily activity to increase health benefits


