**Table of Content**
  
  1.0 Executive Summary. 

2.0 Products 
<br>
  3.0 Data preparation. 
<br>
  4.0 Data Cleaning. 
<br>
  5.0 Analysis. 
<br>
  6.0 Visualisation.
<br>
  7.0 Limitations. 
<br>
  8.0 Key Findings. 
<br>
  9.0 Recommendations.
<br>
  
  1.0 # Executive Summary

This is my Capstone project Google Data Analytics Professional Certificate. In this case study, my role is a junior data analyst working on the marketing analyst team at Bellabeat, a high-tech manufacturer of health-focused products for women.

Bellabeat founded in 2013 by Urška Sršen and Sando Mur is a successful small company, but they have the potential to become a larger player in the global smart device market.

1.1 # Business Task

My task is to Focus on one of Bellabeat's products and analyze smart device data to gain insight into how consumers are using their smart devices.

Present your analysis to the Bellabeat executive team along with your high-level recommendations for Bellabeat's marketing strategy.

2.0 # Products ○ Bellabeat app: The Bellabeat app provides users with health data related to their activity, sleep, stress, menstrual cycle, and mindfulness habits. This data can help users better understand their current habits and make healthy decisions. The Bellabeat app connects to their line of smart wellness products. ○ Leaf: Bellabeat's classic wellness tracker can be worn as a bracelet, necklace, or clip. The Leaf tracker connects to the Bellabeat app to track activity, sleep, and stress. ○ Time: This wellness watch combines the timeless look of a classic timepiece with smart technology to track user activity, sleep, and stress. The Time watch connects to the Bellabeat app to provide you with insights into your daily wellness. ○ Spring: This is a water bottle that tracks daily water intake using smart technology to ensure that you are appropriately hydrated throughout the day. The Spring bottle connects to the Bellabeat app to track your hydration levels. ○ Bellabeat membership: Bellabeat also offers a subscription-based membership program for users. Membership gives users 24/7 access to fully personalized guidance on nutrition, activity, sleep, health and beauty, and mindfulness based on their lifestyle and goals.

3.0 # Data preparation

```{r, Cache = TRUE}
install.packages("tidyverse")
install.packages("dplyr")
install.packages("readr")
install.packages("janitor")
install.packages("lubridate")
install.packages("skimr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("scales")
install.packages("knitr")


##Load packages
library(tidyverse)
library(dplyr)
library(readr)
library(janitor)
library(lubridate)
library(skimr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(scales)
library(knitr)

```


```{r Import Dataframe, echo=FALSE, warning=FALSE, Cache=TRUE, show_col_types = FALSE}
Daily_Activity <- read_csv("BellaBeat Dataset/dailyActivity_merged.csv") 
Daily_Calories <- read_csv("BellaBeat Dataset/dailyCalories_merged.csv")  
```


```{r Import Dataframe2, echo=FALSE, warning=FALSE, Cache=TRUE, show_col_types = FALSE}
heart_rate <- read_csv("BellaBeat Dataset/heartrate_seconds_merged.csv")
```


```{r Import Dataframe3, echo=FALSE, warning=FALSE, Cache=TRUE, show_col_types = FALSE}

Daily_Intensities <- read_csv("BellaBeat Dataset/dailyCalories_merged.csv")
Daily_Steps <- read_csv("BellaBeat Dataset/dailySteps_merged.csv")
```


```{r Import Dataframe4, echo=FALSE, warning=FALSE, Cache=TRUE, show_col_types = FALSE}
Hourly_Calories <- read_csv("BellaBeat Dataset/hourlyCalories_merged.csv")
Hourly_Intensities <- read_csv("BellaBeat Dataset/hourlyIntensities_merged.csv")
Hourly_Steps<- read_csv("BellaBeat Dataset/hourlySteps_merged.csv")
```



```{r Import Dataframe6, echo=FALSE, warning=FALSE, Cache=TRUE, show_col_types = FALSE}
Minute_Sleep <-read_csv("BellaBeat Dataset/minuteSleep_merged.csv")
```

```{r Import Dataframe7, echo=FALSE, warning=FALSE, Cache=TRUE, show_col_types = FALSE}
Minute_Steps_Wide <-read_csv("BellaBeat Dataset/minuteStepsWide_merged.csv")
```


```{r Import Dataframe8, echo=FALSE, warning=FALSE, Cache=TRUE, show_col_types = FALSE}
Sleep_Day<-read_csv("BellaBeat Dataset/sleepDay_merged.csv")
```


```{r Import Dataframe9, echo=FALSE, warning=FALSE, Cache=TRUE, show_col_types = FALSE}

WeightLoginfo<-read_csv("BellaBeat Dataset/weightLogInfo_merged.csv")

```

4.0 # DATA CLEANING

```{r Data cleaning, warning=FALSE, Cache=TRUE}
## Verify the number of users
n_distinct(Daily_Activity$Id)
```

##Verify the number of days
n_distinct(Daily_Activity$ActivityDate)

## Identify missing values 
#Daily Activity;
sum(is.na(Daily_Activity)) 

##For Daily_Calories: 
sum(is.na(Daily_Calories))

##Daily intensities: 
sum(is.na(Daily_Intensities))

##For Sleep Day :
sum(is.na(Sleep_Day))

##WeightLoginfo:
sum(is.na(WeightLoginfo))

## For WeightLoginfo dataset that has NA values in some columns 

##Replace NA with zero
WeightLoginfo <- replace_na(WeightLoginfo,list(Fat = 0))

```
```


```
```{r, Analysis}
5.0 # ANALYSIS OF THE DATA

```{r analysis, cache=TRUE, warning=FALSE}
##Format the SleepDay column in Sleep_Day Dataset

Sleep_Day_formatted <-Sleep_Day %>% 
  separate(SleepDay, into = c("Date", "Time", "P"), sep = " ") %>% 
  mutate(Date= mdy(Date)) %>% 
  mutate( Id = as.character(Id)) %>% 
  mutate(Time= parse_time(Time, format = "%H:%M:%S"))


##Format the ActivityDate column in Daily_Daily
Daily_ActivityDate_formatted <- Daily_Activity %>% 
  mutate(Date= mdy(ActivityDate)) %>% 
  select(c(-ActivityDate))

### Get the avearge in Daily_Activity Table
Daily_activity_table_average <- Daily_ActivityDate_formatted %>% 
  mutate(Total_Active_minutes = VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes) %>% 
  select(-TrackerDistance, LoggedActivitiesDistance, SedentaryMinutes) %>% 
  group_by(Id) %>% 
  reframe(Average_TotalDistance=mean(TotalDistance), Average_Totalsteps=mean(TotalSteps),
          Average_TotalCalories=mean(Calories), Total_AverageActive_minutes=mean(Total_Active_minutes), 
          Average_VeryActive_mins = mean(VeryActiveMinutes), Average_fairlyActive_mins = mean(FairlyActiveMinutes), 
          Average_lightlyActive_mins = mean(LightlyActiveMinutes))



##Average total of sleep by each user recorded
average_sleep_day_table<- Sleep_Day%>% 
  group_by(Id) %>% 
  summarise(AverageSleepRecords=mean(TotalSleepRecords), AverageMinutesAsleep=mean(TotalMinutesAsleep), AverageTimeInBed=mean(TotalTimeInBed))

#Average step from Hourly Activity Table
Hourly_Activity_Step_tab <- Hourly_Steps %>% 
  mutate(Date= mdy_hms(ActivityHour)) %>% 
  separate(Date, into = c("Date", "Time"), sep= " ")

index <-is.na(Hourly_Activity_Step_tab)
Hourly_Activity_Step_tab[is.na(Hourly_Activity_Step_tab)] <- "oo:oo"
Hourly_Activity_Step_tab[is.na(Hourly_Activity_Step_tab)] <- "oo:oo"

Hourly_Activity_tab<- Hourly_Activity_Step_tab %>% 
  mutate(Id = as.character(Id)) %>% 
  group_by(Time) %>% 
  summarise(`Average Steps` =round(mean(StepTotal), 1))


### Users who recorded their sleep with the Bellabeat Device
Num_of_days_Sleep_table<-Sleep_Day %>% 
  group_by(Id) %>% 
  summarise(num_of_days_Recorded=n()) 

Num_of_days_Sleep_tab<-Num_of_days_Sleep_table %>% 
  mutate(Id = as.character(Id))

### Users who recorded their sleep with the Bellabeat Device
Num_of_days_Sleep_table<-Sleep_Day %>% 
  group_by(Id) %>% 
  summarise(num_of_days_Recorded=n()) 

Num_of_days_Sleep_tab<-Num_of_days_Sleep_table %>% 
  mutate(Id = as.character(Id))

## Daily Steps recorded by each user
Table_Steps<- Daily_Steps %>% 
  mutate(Date= mdy(ActivityDay), Id = as.character(Id)) %>% 
  select(c(-ActivityDay)) %>% 
  group_by(Id) %>% 
  summarise(Steps=n()) 

##Daily Intensity recorded by each user 
Table_Intensities <- Daily_Intensities %>% 
  mutate(Date= mdy(ActivityDay), Id = as.character(Id)) %>% 
  select(c(-ActivityDay)) %>% 
  group_by(Id) %>% 
  summarise(Intensity=n()) 

##Daily weight recorded for each user
WeightLog_table<- WeightLoginfo %>% 
  separate(Date, into =  c("Date", "Time", "AM/PM"), sep = " ") %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(Id = as.character(Id)) %>%   
  group_by(Id) %>% 
  summarise(num_of_days_Recorded_Weight=n()) 


### Intensity by Days of the week
Daily_intensity_tab <- Hourly_Steps %>% 
  separate(ActivityHour, into =c("Date", "Time", "P"), sep = " ") %>% 
  mutate(Date= mdy(Date)) %>% 
  mutate(Days= weekdays(Date)) %>% 
  group_by(Days) %>% 
  summarise(Average_Steps = mean(StepTotal))

### Average Activity(Sleep and Step) by Days of the week
DailySleep_tab<- Sleep_Day %>% 
  separate(SleepDay, into = c("Date", "Time", "P"), sep = " ") %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(Days = weekdays(Date)) %>% 
  group_by(Days) %>% 
  summarise(Average_TotalMinutes_Inbed = mean(TotalTimeInBed),
            Average_TotalMinutes_Asleep = mean(TotalMinutesAsleep))

### Relationship between average total sleep and average total step
DailySleep_Intensity_tab<- inner_join(DailySleep_tab, Daily_intensity_tab, by = c("Days"))

```

6.0 # Visualisation

```{r, cache=TRUE, warning=FALSE}
## Heatmap of steps per hour

steps_plot<- Hourly_Activity_tab %>% 
  ggplot(aes(x = Time, y = `Average Steps`, fill = `Average Steps`)) +
  labs(title = "Average Hourly Step Count", 
       y= "Number of Steps ", x = "Time of the Day", subtitle = "Average Number of Steps Throughout the Day")+
  geom_tile(aes(height = 37, width = 0.7 )) +
  scale_fill_gradient(low = "white", high = "skyblue")+
  theme_minimal(base_size =10)+
  theme(panel.grid = element_blank(),axis.text.x = element_text( angle = 45, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size =  12, face = "bold"), axis.title.x = element_text(size = 12, face = "bold", vjust = -0.9), 
        axis.text = element_text(15), panel.background = element_rect("grey"))

steps_plot2 <- steps_plot +
  scale_x_discrete(labels = c("01:00", "02:00", "03:00", "04:00", "05:00",  "06:00", "07:00", "08:00", 
                              "09:00", "10:00",  "11:00", "12:00", "13:00", "14:00", "15:00","16:00", "17:00", 
                              "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00")) +
  geom_label(aes(label = "Average Steps"), vjust = 0, hjust = 0.5, angle = 90, size = 2.5, colour = "black",label.size = NA)

print(steps_plot2)
```


```{r, cache=TRUE, warning=FALSE}
## Daily activity
plot_intensity <- DailySleep_Intensity_tab %>% 
  ggplot(aes(x = reorder(Days, Average_Steps), y = Average_Steps, fill = Average_TotalMinutes_Asleep))+
  geom_col()+ labs(title = "Average Number of Steps vs Average Mins of Sleep for Each Day of the Week ", x= "Days of the Week", y= "Average Step Per Day", fill = "Average Total Minutes Asleep") +
  scale_fill_gradient(low = "black", high = "skyblue")

plot_intensity + theme_classic() + theme(plot.title = (element_text(face = "bold", size = 12)), axis.title.x = element_text(size = 10, face = "bold"),axis.title.y = element_text( size = 10, face = "bold"))
```


```{r, cache=TRUE, warning=FALSE}
## How often BellaBeat users record sleep, step and intensity with the devices
Sleep_day_table <- Sleep_Day_formatted %>% 
  select(Id, Date) %>% 
  mutate(Id = as.character(Id)) %>%  
  group_by(Id) %>% 
  summarise(Sleep=n())

Usag<- inner_join(Table_Steps, Table_Intensities, by = c("Id"))

## Combine  daily steps and intensities tables since values are same. 
Combine_Usag <- Usag %>% 
  select(-Steps) %>% 
  rename("Intensity & Steps" = Intensity)


Table_User <- full_join(Combine_Usag,Sleep_day_table, by =c("Id")) 

## Replace NA values with 0
Tab_users <- replace_na(Table_User,list(Sleep = 0))

##Change to Narrrow table
Table_narrow<- melt(Tab_users, id.vars = c("Id")) 

names(Table_narrow)[names(Table_narrow) == "variable"]<- "Activity"
names(Table_narrow)[names(Table_narrow) == "value"]<- "Number of Days"
```


```{r, cache=TRUE, warning=FALSE}
## To show how often BellaBeat users record sleep, step and intensity with the devices
plot_sleepvsstep <- ggplot(data =Table_narrow, aes(x = Id, y = `Number of Days`, 
                                                   fill = Activity))+
  geom_col(position = position_dodge2())+
  scale_fill_manual(values = c("Intensity & Steps" = "chocolate4", 
                               "Sleep" = "darkblue"),
                    labels = c("Intensity & Steps", "Sleep"))+
  labs(x = "BellaBeat Users", y = "Number of Days Recorded",
       title = "Number of Days Each User Recorded")+
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_blank(), plot.title = (element_text(face = "bold", size = 12)), axis.title.x = element_text(size = 10, face = "bold"), 
        axis.title.y = element_text( size = 10, face = "bold")) 

plot_sleepvsstep + theme_classic()
```


```{r, cache=TRUE, warning=FALSE}
##The relationship between with active lifestyle vs calories burn
plot_minsvscalories<- Daily_activity_table_average %>% 
  ggplot(aes(x=Total_AverageActive_minutes, y= Average_TotalCalories))+
  geom_point(aes(size = Average_VeryActive_mins, colour = Average_VeryActive_mins))+
  labs(title = "Average Active Minutes vs Average Calories Burned", 
       x="Average Active Minutes", y= "Average Calories Burned", 
       colour = "Average Rigorous Mins",
       size = " " )+theme_minimal()+
  theme_classic() + 
  theme(panel.grid = element_blank(),axis.text.x = element_text( hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size =  12, face = "bold"), axis.title.x = element_text(size = 12, face = "bold"), 
        axis.text = element_text(15))

p_1 <- plot_minsvscalories +
  geom_label(aes(label = "33% of users who were active for 200 mins or more burned 2,500 calories or more", 
                 x= 190, y= 3225), label.size = NA, fill = "yellow")

print(p_1)
```


```{r, cache=TRUE, warning=FALSE}
## Relationship between All active activities vs Sleep 
Active_sleep_calories_table<- inner_join(Daily_activity_table_average, average_sleep_day_table, by =c("Id")) %>% 
  mutate(Id = as.character(Id))
view(Active_sleep_calories_table)

plot_sleepvscalories<- Active_sleep_calories_table %>% 
  ggplot(aes(x = Total_AverageActive_minutes, y = AverageMinutesAsleep))+
  geom_point(aes(colour = Average_Totalsteps, size = 3))+
  geom_line(linewidth = 0.1)+
  labs(title = "Average Active Minutes vs Average Sleep Time ", 
       x="Average Active Minutes", y= "Average Sleep Time", colour = "Average Total Steps", size = " ")+
  theme_minimal()
```


```{r, cache=TRUE, warning=FALSE}
plot_sleepvscalories + theme_classic() +
  scale_y_continuous(breaks = seq(0,8000, by = 100))+
  scale_x_continuous(breaks = seq(0,400, by = 50)) +
  geom_label(aes(label = "54% of users who had 200 mins of active activity
                 \nhad 300 mins of sleep or more", y = 570, x = 260), fill = "grey", size = 3)
```


```{r, cache=TRUE, warning=FALSE}
plot_sleepvscalories +
  theme(panel.grid = element_blank(),axis.text.x = element_text( hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size =  12, face = "bold"), axis.title.x = element_text(size = 12, face = "bold"), 
        axis.text = element_text(15))
```

7.0 # Limitations

Sample size The dataset is limited to 33 users, and some of the subsets have even fewer. The sleep log file contains information from 24 users, and the weight log file, 8 users. Time frame The dataset is limited to 31 days. Additionally, there is no information about how long each user has had their Bellabeat device, so an analysis of change in user behavior across time is not possible. User demographics There is no information on the demographics of the users, so the data may not be accurate for a female-only audience.

8.0 # Key Findings

```{r, cache=TRUE}
calories_vs_mins <- Daily_activity_table_average %>%
  select(Average_TotalCalories, Total_AverageActive_minutes) %>% 
  count(Average_TotalCalories >= 2500, Total_AverageActive_minutes >= 200) 


mins_vs_sleep <- Active_sleep_calories_table %>% 
  select(AverageMinutesAsleep, Total_AverageActive_minutes) %>% 
  count(AverageMinutesAsleep >= 300, Total_AverageActive_minutes >= 200)

```

**Daily Steps/Intensity** Across all users, most steps occur between 12 noon - 2pm & 4pm -7pm Least recorded steps between Midnight -5am. Most recorded steps between 6-7pm

**Steps and Intensity** 24 users recorded sleep activity, only 41% recorded for 20 days or more. However 33 users recorded their steps and intensity activities, out of this, 90% recorded the activity for 30 days or more.

**Average Calories Burned vs active minutes activity** 33 users recorded their intensity activity while 8 users recorded their weight 72% of users were active for 200 mins or more 39% of users who were active for 200 mins or more burned less than 2,500 calories 33% of users who were active for 200 mins or more burned 2,500 calories or more Only 3% burned over 2,500 mins without having 200 mins or active mins

**Average Sleep time vs average active minutes activity** 54% users who had 200 mins of active activity had 300 mins of sleep or more 25% users who had 200 mins of active activity has less than 300 mins of sleep 21% of users had over 300 mins of sleep had less than 200 mins of activity

**Steps for each day of the week vs Sleep** Most users tend to sleep more on Wednesdays, Saturdays and Sundays with the most minutes of sleep being on Sundays however they tend to have the least minutes of sleep on

```{r, cache=TRUE}

## Daily 8,000 - 10,000 steps recommended by WHO 
Number_8000 <- Daily_Steps %>% 
  group_by(Id) %>% 
  summarise(count = n(),
            percent_yes = percent(sum(StepTotal >= 8,000)/count),
            percent_no = percent(sum(StepTotal <= 8,000)/count))
```

WHO recommends number of steps per day is 8,000 - 10,000 steps. **54% of the users had 8,000 steps or more in all the days recorded**
  
  ```{r, cache=TRUE}
## 420 mins of sleep daily as recommended by WHO
number_420 <- Sleep_Day %>% 
  select(Id, TotalMinutesAsleep ) %>% 
  group_by(Id) %>% 
  summarise(average = mean(TotalMinutesAsleep)) %>% 
  count(average > 419)
```

WHO recommends amount of sleep per night is 7 hours (420 mins) **50% of Bellabeat users who recorded their sleep had 420 mins of sleep recommended by WHO.**
  
  ```{r, cache=TRUE}
## CDC recommends daily intense-moderate activity of 25 mins or more
Number_25mins <- Daily_activity_table_average %>%
  select(Average_VeryActive_mins, Average_fairlyActive_mins) %>% 
  mutate(Intense_moderate = Average_VeryActive_mins + Average_fairlyActive_mins) %>%  
  summarize(count = n(),
            percent_25_yes = percent(sum(Intense_moderate >= 25)/count),
            percent_25_no = percent(sum(Intense_moderate < 25)/count))
```

CDC recommends Intense - moderate 150 mins per week, about 25 mins per day **55% of BellaBeat user had an average of 25 mins of intense-moderate activity daily.**
  
  8.0 # Recommendations 

From the data, Bellabeat users wear their devices during the day to track their health and fitness levels but are less likely to wear them at night, and very unlikely to manually record data. Bellabeat should focus on marketing these products more as comfy and easy to use in bed and also do more ads to promote the comfort and ease of use.
To enhance active lifestyle, Bellabeat should include prompts in their devices that will remind users to move when the device detects a prolonged sedentary time especially during the day. A personalised feature where users can set these reminders based on goals, lifestyle, type of jobs, health conditions etc should included and ads on this should be promoted.

To motivate its users and boost engagement, Bellabeat can celebrate its users when the reach certain milestones, targets and set goals such as active time, sleep time, steps etc
