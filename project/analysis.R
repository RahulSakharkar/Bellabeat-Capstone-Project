#Loading Libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)

#Importing datasets
activity<-read_csv('dailyActivity_merged.csv')
sleep<-read_csv('sleepDay_merged.csv')
weight<-read_csv('weightLogInfo_merged.csv')

#Viewing datsets
View(activity)
View(sleep)
View(weight)

#Finding distinct observations in Id column
n_distinct(activity$Id)
n_distinct(sleep$Id)
n_distinct(weight$Id)

#plotting a graph between distance/steps and calories burnt with color representing
#the different people. Same Color points represent the same person walking on different dates
ggplot(data=activity, aes(x=TotalDistance,y=Calories,color=Id)) + geom_point() + geom_smooth(method="gam")
ggplot(data=activity, aes(x=TotalSteps,y=Calories,color=Id)) + geom_point() + geom_smooth(method="loess")+ facet_wrap(~Id)

# There is clear linear correlation between distance/steps and calories burnt. As the distance or number of step increase - the more calories are burnt.
#Bellabeat can give timely notifications to users on their step count and calories burnt.
#It an also give motivational texts or graphics to urge people to walk and complete their daily goal.


#joining tables -activity and sleep
activity_sleep= activity %>% inner_join(sleep,by=c('Id', 'Date'))
View(activity_sleep)
ggplot(data=activity_sleep,aes(x=TotalMinutesAsleep,y=SedentaryMinutes,color=Id))+ geom_point() + geom_smooth()
# the above data shows as number of hours a person sleeps reduces, his sedantary time (not-active) time increases.One Plausibile 
#reason could be that sleep replenishes our energy and reduces stress, thus making us more active.
#The data shows an average of 500 minutes(8 hrs) results in least sedantary time. While sleeping less or oversleeping leads to increased sedantary time.

#Bellabeat can give a reminder for sleep and track sleep to give more insights on the sleeping patterns of 
#a user, thus making them more aware and consious of their sleep schedule.

#it can also give a notification if their is a inactivity for an hour or more, reminding people to to light exercise or walk.

#-----------------------------------------------------------------------------------

intensities=read_csv('hourlyIntensities_merged.csv')
View(intensities)

intensities$ActivityHour=as.POSIXct(intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")

int_new <- intensities %>%
  group_by(time) %>%
  drop_na() %>%
  summarise(mean_total_int = mean(TotalIntensity))

ggplot(data=int_new, aes(x=time, y=mean_total_int,, fill=mean_total_int)) + geom_bar(stat = "identity") +theme(axis.text.x = element_text(angle = 90)) +labs(title="Average Total Intensity vs. Time")

#the graph shows that during 7 am - 6 pm people are most active.
