activity_unclean <- read.csv("activity.csv")

#Load in ggplot
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)

#remove the NA values & make date data type
activity_unclean$date <- as.Date(as.character(activity_unclean$date))
activity <- na.omit(activity_unclean)
activity$day <- weekdays(activity$date)

step_day <- aggregate(steps ~ date, activity, FUN = sum)

#histogram of daily steps taken
hist(step_day$steps, 
     xlab = "average steps",
     main = "Histogram of Steps",
     col = "red")

#mean and median steps per day
mean_median_steps <- summary(step_day$steps)

#time series plot
step_int <- aggregate(steps ~ interval, activity, FUN = mean)
tim_ser <- ggplot(step_int, aes(x = interval, y = steps)) + 
  geom_line() + 
  labs(title = "Average Steps Per Interval", x = "Interval", y = "Average Steps") +
  theme(plot.title = element_text(hjust = 0.5))

#compute maximum steps and interval with max steps
max_step <- max(step_int$steps)
biggest_interval <- step_int[which.max(step_int$steps), 1]

#find number of NA in activity dataset
sum_active_na <- sum(is.na(activity_unclean))

#Replace NA with average number of steps each date
df_active_na <- activity_unclean
active_na <- is.na(df_active_na$steps)
averages <- tapply(df_active_na$steps, df_active_na$interval, mean, na.rm = TRUE)
  na.omit(averages)

df_active_na <- df_active_na %>% 
  mutate(steps = replace(steps, active_na, averages))
  
new_df_active <- tapply(df_active_na$steps, df_active_na$date,sum, na.rm = TRUE)
hist(new_df_active)

Mean_new = mean(new_df_active)
Median_new = median(new_df_active)

#find breakdown of Weekday Vs Weekend steps

df_active_na$day <- weekdays(df_active_na$date)

df_active_na$dow <- ifelse(df_active_na$day %in% c("Saturday", "Sunday"),
                            "Weekend",
                            "Weekday")

Weekend_int <- subset(df_active_na, df_active_na$dow == "Weekend")
Weekend_int <- aggregate(steps ~ interval, Weekend_int, FUN = mean)

Weekday_int<- subset(df_active_na, df_active_na$dow == "Weekday")
Weekday_int <- aggregate(steps ~ interval, Weekday_int, FUN = mean)

par(mfrow = c(2,1))
  plot(Weekday_int$interval, Weekday_int$steps, "l")
  plot(Weekend_int$interval, Weekend_int$steps, "l")