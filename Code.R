#### I. Load and import the data ####

library("tidyverse")

activity <- read.csv("activity.csv")

activity$date <- as_date(activity$date)

#### II. What is mean total number of steps taken per day? ####
# a. the total number of steps taken per day
stepsperday <- activity %>%
  group_by(date) %>%
  summarize(sumstep = sum(steps))

ggplot(stepsperday) +
  geom_histogram(aes(x = sumstep)) +
  labs(title = "Total number of steps taken each day",
       x = "Steps",
       y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("I.Total_number_of_steps_taken_each_day.pdf")

# b. the mean of the total number of steps taken per day
round(mean(stepsperday$sumstep,  na.rm = T))

# c. the median of the total number of steps taken per day
round(median(stepsperday$sumstep,  na.rm = T))

### III. What is the average daily activity pattern? ####
# a. Average number of steps taken, averaged across all days
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
ggsave("III.Average_number_of_steps_taken,_averaged_across_all_days.pdf")

# b. maximum number of steps (5-minute interval)
filter(stepperinterval, meanstep == max(meanstep))


### IV. Imputing missing values ####

# Calculate and report the total number of missing values in the dataset
# a. total number of missing values in the dataset 
sum(is.na(activity$steps))

# b. strategy for filling in all of the missing values in the dataset
activitywithoutNA <- map_dbl(activity$steps, mean)
activity$steps

# c. new dataset that is equal to the original dataset but with the missing data filled in.
imputeSteps <- activity %>%
  group_by(interval) %>%
  mutate(steps = ifelse(is.na(steps), mean(steps), steps))


imputedDailySteps <- imputeSteps %>%
  group_by(date) %>%
  summarise(total = sum(steps))

# d.1. Total Daily Steps
ggplot(imputedDailySteps, aes(total)) +
  geom_histogram(binwidth = 500) +
  labs(x = "Total Daily Steps",
       y = "Frequency",
       title = "Daily Steps") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("IV.Daily_Steps.pdf")

# d.2.Mean number of steps per day
mean(imputedDailySteps$total, na.rm = TRUE)

# d.3. Median number of steps per day
median(imputedDailySteps$total, na.rm = TRUE)


### V. Are there differences in activity patterns between weekdays and weekends? ####
# a. Create weekdays/weekend variable

wSteps <- imputeSteps %>%
  mutate(days = weekdays(date)) %>% 
  mutate(week = if_else(days == "samedi" | days == "dimanche","weekend", "weekday"))

wSteps <- wSteps %>%
  mutate(week = as.factor(week))

wIntSteps <- wSteps %>%
  group_by(interval,week) %>%
  summarise(mean = mean(steps, na.rm = TRUE))

# b. Weekdays and weekends activity patterns
ggplot(wIntSteps, aes(interval, mean)) +
  geom_line() +
  facet_grid(week ~ .) +
  labs(x = "5-minute intervals",
       y = "Avarage number of steps taken")+
  ggtitle("Weekdays and weekends activity patterns")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("V.Weekdays_and_weekends_activity_patterns.pdf")

