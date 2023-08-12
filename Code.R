#### Load and import the data ####

library("tidyverse")

activity <- read.csv("activity.csv")



#### What is mean total number of steps taken per day? ####

stepsperday <- activity %>%
  group_by(date) %>%
  summarize(sumstep = sum(steps, na.rm = TRUE))

ggplot(stepsperday) +
  geom_histogram(aes(x = sumstep)) +
  labs(title = "Total number of steps taken each day",
       x = "Steps",
       y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

# the mean of the total number of steps taken per day
round(mean(stepsperday$sumstep))

# the median of the total number of steps taken per day
round(median(stepsperday$sumstep))

### What is the average daily activity pattern? ####

stepperinterval <- activity %>%
  group_by(interval) %>%
  summarize(meanstep = mean(steps, na.rm = T))



ggplot(stepperinterval) +
  geom_line(aes(x = interval,
                y = meanstep)) +
  labs(title = "Average number of steps taken, averaged across all days",
       x = " 5-minute interval",
       y = "Average across all the days") +
  theme(plot.title = element_text(hjust = 0.5))


filter(stepperinterval, meanstep == max(meanstep))


### Imputing missing values ####

# Calculate and report the total number of missing values in the dataset

sum(is.na(activity$steps))

activitywithoutNA <- map_dbl(activity$steps, mean, na.rm = T)

activity$steps

imputeSteps <- activity %>%
  group_by(interval) %>%
  mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))


imputedDailySteps <- imputeSteps %>%
  group_by(date) %>%
  summarise(total = sum(steps))


ggplot(imputedDailySteps, aes(total)) +
  geom_histogram(binwidth = 500) +
  labs(x = "Total Daily Steps",
       y = "Frequency",
       title = "Daily Steps") +
  theme(plot.title = element_text(hjust = 0.5))


## Mean number of steps per day
mean(imputedDailySteps$total, na.rm = TRUE)

## Median number of steps per day
median(imputedDailySteps$total, na.rm = TRUE)


### Imputing missing values ####
# Create weekdays/weekend variable

imputeSteps$date <- as.Date(imputeSteps$date)

wSteps <- imputeSteps %>%
  mutate(days = weekdays(date)) %>% 
  mutate(week = if_else(days == "samedi" | days == "dimanche","weekend", "weekday"))


# Turn this into Factor

wSteps <- wSteps %>%
  mutate(week = as.factor(week))


wIntSteps <- wSteps %>%
  group_by(interval,week) %>%
  summarise(mean = mean(steps, na.rm = TRUE))


ggplot(wIntSteps, aes(interval, mean)) +
  geom_line() +
  facet_grid(week ~ .) +
  labs(x = "5-minute intervals",
       y = "Avarage number of steps taken")+
  ggtitle("Weekdays and weekends activity patterns")+
  theme(plot.title = element_text(hjust = 0.5))



