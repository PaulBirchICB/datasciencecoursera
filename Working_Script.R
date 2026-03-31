## --- Load and preprocess data ------------------------------------------

setwd("C:/Users/paulbi/OneDrive - NHS/Documents/00_Central_Resources/Learning/JHU Data Science/datasciencecoursera/Course_Project_1_RR")

activity <- read.csv("data/activity.csv")
activity$date <- as.Date(activity$date)

str(activity)

## --- Total number of steps per day -------------------------------------

total_steps_per_day <- aggregate(
  steps ~ date,
  data = activity,
  sum,
  na.rm = TRUE
)

hist(
  total_steps_per_day$steps,
  main = "Total Steps Per Day",
  xlab = "Steps",
  col = "steelblue",
  breaks = 20
)

mean_steps   <- mean(total_steps_per_day$steps)
median_steps <- median(total_steps_per_day$steps)

mean_steps
median_steps

## --- Average daily activity pattern ------------------------------------

avg_steps_interval <- aggregate(
  steps ~ interval,
  data = activity,
  FUN = mean,
  na.rm = TRUE
)

# ---- FIX 1: Convert interval numbers to HH:MM time labels ----
convert_to_time <- function(interval) {
  sprintf("%02d:%02d", interval %/% 100, interval %% 100)
}

avg_steps_interval$time <- convert_to_time(avg_steps_interval$interval)

# ---- FIX 2: Improved time-of-day plot ----
plot(
  avg_steps_interval$steps,
  type = "l",
  col = "blue",
  xaxt = "n",
  main = "Average Steps per 5-Minute Interval",
  xlab = "Time of Day",
  ylab = "Average Steps"
)

# tick marks every 2 hours (24 intervals)
time_ticks <- seq(1, nrow(avg_steps_interval), by = 24)
axis(1, at = time_ticks, labels = avg_steps_interval$time[time_ticks], las = 2)

# max interval detection unchanged
max_interval <- avg_steps_interval[which.max(avg_steps_interval$steps), ]
max_interval

## --- Imputing missing values ------------------------------------------

total_missing <- sum(is.na(activity$steps))
total_missing

activity_imputed <- activity

for (i in 1:nrow(activity_imputed)) {
  if (is.na(activity_imputed$steps[i])) {
    interval_value <- activity_imputed$interval[i]
    mean_value <- avg_steps_interval$steps[avg_steps_interval$interval == interval_value]
    activity_imputed$steps[i] <- mean_value
  }
}

## --- Total steps per day after imputation ------------------------------

total_steps_per_day_imputed <- aggregate(
  steps ~ date,
  data = activity_imputed,
  sum
)

hist(
  total_steps_per_day_imputed$steps,
  main = "Total Steps Per Day (Imputed Data)",
  xlab = "Steps",
  col = "darkgreen",
  breaks = 20
)

mean_steps_imputed   <- mean(total_steps_per_day_imputed$steps)
median_steps_imputed <- median(total_steps_per_day_imputed$steps)

mean_steps_imputed
median_steps_imputed

## --- Weekday vs Weekend activity patterns -------------------------------

activity_imputed$day_type <- ifelse(
  weekdays(activity_imputed$date) %in% c("Saturday", "Sunday"),
  "weekend",
  "weekday"
)

activity_imputed$day_type <- factor(activity_imputed$day_type, levels = c("weekday", "weekend"))

avg_steps_interval_daytype <- aggregate(
  steps ~ interval + day_type,
  data = activity_imputed,
  FUN = mean
)

# ---- FIX 3: Add HH:MM time column to the daytype dataset ----
avg_steps_interval_daytype$time <- convert_to_time(avg_steps_interval_daytype$interval)

## --- Panel plot using base R --------------------------------------------

par(mfrow = c(2, 1), mar = c(5, 5, 4, 2))

# Weekday panel
weekday_data <- subset(avg_steps_interval_daytype, day_type == "weekday")

plot(
  weekday_data$steps,
  type = "l",
  col = "blue",
  xaxt = "n",
  main = "Average Steps per Interval: Weekdays",
  xlab = "Time of Day",
  ylab = "Average Steps"
)

axis(1, at = time_ticks, labels = weekday_data$time[time_ticks], las = 2)

# Weekend panel
weekend_data <- subset(avg_steps_interval_daytype, day_type == "weekend")

plot(
  weekend_data$steps,
  type = "l",
  col = "darkorange",
  xaxt = "n",
  main = "Average Steps per Interval: Weekends",
  xlab = "Time of Day",
  ylab = "Average Steps"
)

axis(1, at = time_ticks, labels = weekend_data$time[time_ticks], las = 2)

par(mfrow = c(1,1))