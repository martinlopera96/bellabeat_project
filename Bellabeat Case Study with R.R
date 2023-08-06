# Install packages for exploratory data analysis.

install.packages("tidyverse")
install.packages("lubridate")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("knitr")
install.packages("kableExtra")

library(tidyverse)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)

# Read csv files for each dataset.

activities <- read.csv("C:\\Users\\Martín\\Documents\\DATA ANALYSIS\\Google Data Analytics\\Course 8\\dailyActivity_merged.csv")
intensities <- read.csv("C:\\Users\\Martín\\Documents\\DATA ANALYSIS\\Google Data Analytics\\Course 8\\hourlyIntensities_merged.csv")
calories <- read.csv("C:\\Users\\Martín\\Documents\\DATA ANALYSIS\\Google Data Analytics\\Course 8\\hourlyCalories_merged.csv")
sleep <- read.csv("C:\\Users\\Martín\\Documents\\DATA ANALYSIS\\Google Data Analytics\\Course 8\\sleepDay_merged.csv")
weight <- read.csv("C:\\Users\\Martín\\Documents\\DATA ANALYSIS\\Google Data Analytics\\Course 8\\weightLogInfo_merged.csv")
heartrate <- read.csv("C:\\Users\\Martín\\Documents\\DATA ANALYSIS\\Google Data Analytics\\Course 8\\heartrate_seconds_merged.csv")

# Datetime formats are not in an appropriate type. Let's convert them into 'POSIXct' (datetime format).

activities$ActivityDate = as.POSIXct(activities$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
intensities$ActivityHour = as.POSIXct(intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
calories$ActivityHour = as.POSIXct(calories$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep$SleepDay = as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
weight$Date = as.POSIXct(weight$Date, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
heartrate$Time = as.POSIXct(heartrate$Time, format="%Y/%m/%d %I:%M:%S %p", tz=Sys.timezone())

# intensities, calories and weight datasets all have a column with date and time in it. For data management purposes,
# I will split those columns into two: date and time.

intensities$time <- format(intensities$ActivityHour, format= "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")
intensities$ActivityHour <- NULL
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
calories$date <- format(calories$ActivityHour, format = "%m/%d/%y")
calories$ActivityHour <- NULL
weight$date <- format(weight$Date, format = "%H:%M:%S")
weight$time <- format(weight$Date, format = "%m/%d/%y")
weight$Date <- NULL
heartrate$time <- format(heartrate$Time, format = "%H:%M:%S")
heartrate$date <- format(heartrate$Time, format = "%y/%m/%d")
heartrate$Time <- NULL

# Fat column in weight dataset contains 'N/A' in almost every row. Let's remove that column.
# IsManualReport column in weight dataset contains Boolean values but it's type is 'chr' by default. Let's change that column's type to Boolean.

weight$Fat <- NULL
weight$IsManualReport <- as.logical(weight$IsManualReport)

# Check and print in console the number of blank rows that each dataset has.

activities_blank_rows <- sum(rowSums(is.na(activities)) == ncol(activities))
intensities_blank_rows <- sum(rowSums(is.na(intensities)) == ncol(intensities))
calories_blank_rows <- sum(rowSums(is.na(calories)) == ncol(calories))
sleep_blank_rows <- sum(rowSums(is.na(sleep)) == ncol(sleep))
weight_blank_rows <- sum(rowSums(is.na(weight)) == ncol(weight))
heartrate_blank_rows <- sum(rowSums(is.na(heartrate)) == ncol(heartrate))
cat("Number of blank or null rows in activites dataframe: ", activities_blank_rows)
cat("Number of blank or null rows in intensities dataframe: ", intensities_blank_rows)
cat("Number of blank or null rows in calories dataframe: ", calories_blank_rows)
cat("Number of blank or null rows in sleep dataframe: ", sleep_blank_rows)
cat("Number of blank or null rows in weight dataframe: ", weight_blank_rows)
cat("Number of blank or null rows in heartrate dataframe: ", heartrate_blank_rows)

# Check and print in console the number of blank cells that each dataset has.

activities_blank_cells <- sum(is.na(activities))
intensities_blank_cells <- sum(is.na(intensities))
calories_blank_cells <- sum(is.na(calories))
sleep_blank_cells <- sum(is.na(sleep))
weight_blank_cells <- sum(is.na(weight))
heartrate_blank_cells <- sum(is.na(heartrate))
cat("Number of blank cells in activities dataframe: ", activities_blank_cells)
cat("Number of blank cells in intensities dataframe: ", intensities_blank_cells)
cat("Number of blank cells in calories dataframe: ", calories_blank_cells)
cat("Number of blank cells in sleep dataframe: ", sleep_blank_cells)
cat("Number of blank cells in weight dataframe: ", weight_blank_cells)
cat("Number of blank cells in heartrate dataframe: ", heartrate_blank_cells)

# Now it comes the exploration phase. The length(unique()) function will tell us about number of participants in each dataset.
# Due to lack of data in the weight dataset (only 8 participants), it is concluded that this isn't enough data to make any recommendations.

unique_activities <- length(unique(activities$Id))
print(unique_activities)
unique_intensities <- length(unique(intensities$Id))
print(unique_intensities)
unique_calories <- length(unique(calories$Id))
print(unique_calories)
unique_sleep <- length(unique(sleep$Id))
print(unique_sleep)
unique_weight <- length(unique(weight$Id))
print(unique_weight)
unique_heartrate <- length(unique(heartrate$Id))
print(unique_heartrate)

# Let's explore some summary statistics.


# activities
activities %>%
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes,
         VeryActiveDistance,
         ModeratelyActiveDistance,
         LightActiveDistance,
         VeryActiveMinutes,
         FairlyActiveMinutes,
         LightlyActiveMinutes,
         Calories) %>%
  summary()
# Average SedentaryMinutes is 991 (16 hours).

# Average for: VeryActiveDistance, ModeratelyActiveDistance and LightActiveDistance are 1.50, 0.56 and 3.34 respectively.There is a high tendency
# for light active users but also for very active users. There is a low tendency for moderately active users. Active minutes summary also indicates
# us a high tendency for light users, moderate tendency for very active users and a low tendency for moderately active users.

# According to CDC, the recommended amount of steps per day is 10,000. We can see that the average TotalSteps in this dataset is 7638, which is
# considerably less than the recommended. The CDC also recommends to walk 150 minutes per day of moderate intensity.


# intensities
intensities %>%
  select(TotalIntensity,
         AverageIntensity) %>%
  summary()


# calories
calories %>%
  select(Calories) %>%
  summary()
# On the average, participants burn 97 calories per day.


# sleep
sleep %>%
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()
# On the average, users sleep 1 time for 7 hours. The average total time in bed is a little bit longer. People have tendency for
# staying 36 minutes on bed after waking up. There is some worrying data on the minimum values of TotalMinutesAsleep (58 minutes).
# It is worth doing an analysis of how many people are sleeping less than 5 hours:

count_below_5hours <- sum(sleep$TotalMinutesAsleep < 300)
print(count_below_5hours)

filtered_below_5hours <- sleep %>%
  filter(TotalMinutesAsleep < 300)

average_below_5hours <- mean(filtered_below_5hours$TotalMinutesAsleep, na.rm = TRUE)
print(average_below_5hours)
# There is a total of 50 users that are sleeping less than 5 hours and the average TotalMinutesAsleep for those 50 users is 3.2 hours.


# weight
weight %>%
  select(WeightKg,
         BMI) %>%
  summary()
# In order to make insights from the BMI we would need to have the height of the users and determine whether they have a healthy BMI or not.

weight <- weight %>%
  mutate(Height = sqrt(WeightKg / BMI))
# We import bmi-chart from https://bmicalculator.mes.fm/img/bmi-chart.png to be able to determine whether the average user has a healthy BMI or not.
# Despite not having enough data in the weight dataset, we perform the last 2 steps for academic purposes, for this is a Capstone project for a
# Google Analytics Certificate. If we could have enough participants in this dataset, the insights on how correlated are the BMI and the
# data from sleep dataset would be very interesting. Another important insight could come from evaluating the correlation between BMI and 
# activity hours and intensity.


# heartrate
heartrate %>%
  select(Value) %>%
  summary()
# A normal heart rate depends mostly of the user's age. Due to the fact that we don't have each participant's age, it is very difficult to
# give important insights from only summary statistics.


# Now that we have made a statistical analysis of our data, we can merge the relevant data in order to make some visualizations of it.
# In order to do that, I'm going to change the column's names for the purpose of having the same column name in both dataset ('Id', 'date').
# Then, we perform the inner join on columns 'Id' and 'date'.

colnames(activities)[colnames(activities) == "ActivityDate"] <- "date"
colnames(sleep)[colnames(sleep) == "SleepDay"] <- "date"

merged_data <- merge(sleep, activities, by = c('Id', 'date'))

# Examine the correlation between TotalSteps and Calories:

ggplot(data = activities) +
  geom_point(mapping = aes(x = TotalSteps, y = Calories)) +
  geom_smooth(mapping = aes(x = TotalSteps, y = Calories)) +
  labs(title = "Total Steps vs. Calories",
       x = "Total Steps",
       y = "Calories Burned") + 
  theme_grey()

# Just like one would expect: the more steps a user takes a day, the more calories they burn.

# Now let's see the correlation between the tracker distance and calories burned:

ggplot(data = activities) +
  geom_point(mapping = aes(x = TrackerDistance, y = Calories)) +
  geom_smooth(mapping = aes(x = TrackerDistance, y = Calories)) +
  labs(title = "Tracker Distance vs. Calories",
       x = "Tracker Distance",
       y = "Calories Burned") + 
  theme_grey()

# Another positive correlation: the more distance a user walks, the more calories they burn.

# Now let's examine which day users are more and less active.
# In order to do so, we need to create a new column that tells us about the weekday based on the dates we already have.

activities$date <- as.Date(activities$date)
activities$weekday <- weekdays(activities$date)

# Let's group our information by weekday and make a logical order from "Monday" to "Sunday" and use the knitr and kableExtra libraries for visualization purposes.

summary_by_weekday <- activities %>%
  group_by(weekday) %>%
  summarise(
    AverageTotalSteps = mean(TotalSteps),
    AverageTotalDistance = mean(TotalDistance),
    AverageTotalCalories = mean(Calories)
  ) %>%
  arrange(factor(weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

summary_weekday_table <- kable(summary_by_weekday, format = "html", caption = "Summary by Weekday") %>%
  kable_styling(full_width = FALSE)

print(summary_weekday_table)

# Let's plot this values into bar charts:


#AverageTotalSteps by Weekday:
ggplot(summary_by_weekday, aes(x = factor(weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
                               y = AverageTotalSteps,)) + 
  geom_bar(stat = "identity", fill = "red") + 
  geom_text(aes(label = round(AverageTotalSteps, 2)), vjust = -0.5, size = 3) +
  labs(title = "Average Total Steps by Weekday",
       x = "Weekday",
       y = "Average Total Steps")


#AverageTotalDistance by Weekday:
ggplot(summary_by_weekday, aes(x = factor(weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
                               y = AverageTotalDistance,)) + 
  geom_bar(stat = "identity", fill = "gold") + 
  geom_text(aes(label = round(AverageTotalDistance, 2)), vjust = -0.5, size = 3) +
  labs(title = "Average Total Distance by Weekday",
       x = "Weekday",
       y = "Average Total Distance")


#AverageTotalCalories by Weekday:
ggplot(summary_by_weekday, aes(x = factor(weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
                               y = AverageTotalCalories,)) + 
  geom_bar(stat = "identity", fill = "green") + 
  geom_text(aes(label = round(AverageTotalCalories, 2)), vjust = -0.5, size = 3) +
  labs(title = "Average Total Calories by Weekday",
       x = "Weekday",
       y = "Average Total Calories")
