library(ggplot2)
library(dplyr)
library(plyr)
library(lattice)

df <- read.csv("activity.csv")
df$day <- weekdays(as.Date(df$date))
df$DateTime<- as.POSIXct(df$date, format="%Y-%m-%d")


sum <- aggregate(df$steps ~ df$date, FUN="sum", )
colnames(sum)<- c("Date", "Steps")

hist(sum$Steps, breaks=10, xlab="Steps", main = "Total Steps per Day")


# 3 Calculate and report the mean and median of the total number of steps taken per day
mean_missing <- as.integer(mean(sum$Steps))
median_missing <- as.integer(median(sum$Steps))

# 4
ts <- tapply(df$steps, df$interval, mean, na.rm = T)
p <- plot(as.numeric(names(ts)),
     ts,
     xlab = "Interval",
     ylab = "Steps",
     main = "Time series of daily steps",
     type = "l")


five_min <- (sort(ts, decreasing = T))
#as.data.frame(five_min)
names(five_min[1])

# number of rows with missing values
nrow(df[is.na(df$steps),])


#imputing
df2 <- split(df, df$interval)

df2 <- lapply(df2, function(x) {
  x$steps[which(is.na(x$steps))] <- mean(x$steps, na.rm = TRUE)
  return(x)
})
df2 <- do.call("rbind", df2)
row.names(df2) <- NULL

df2 <- split(df2, df2$date)
df3 <- lapply(df2, function(x) {
  x$steps[which(is.na(x$steps))] <- mean(x$steps, na.rm = TRUE)
  return(x)
})
df2 <- do.call("rbind", df2)
row.names(df2) <- NULL
head(df2)
##

sum_impute <- aggregate(df2$steps ~ df2$date, FUN="sum", )
colnames(sum_impute)<- c("Date", "Steps")

hist(sum_impute$Steps, breaks=10, xlab="Steps", main = "Total Steps per Day")
mean_impute <- as.integer(mean(sum_impute$Steps))
median_impute <- as.integer(median(sum_impute$Steps))

# Are there differences in activity patterns between weekdays and weekends?

df2$Weekday_Weekend <- ifelse(df2$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

activity_by_date <- aggregate(steps~interval + Weekday_Weekend, df2, mean, na.rm = TRUE)

plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = Weekday_Weekend)) +
  geom_line() +
  labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
  facet_wrap(~Weekday_Weekend, ncol = 1, nrow=2)


print(plot)
