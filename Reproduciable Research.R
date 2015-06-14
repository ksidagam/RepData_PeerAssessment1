Activity <- read.table(file="Activity.csv",sep=",",header = TRUE)
View(Activity)
#dailysteps <- tapply(Activity$steps, Activity$date, sum)
dailysteps <- tapply(Activity$steps, Activity$date, sum)
hist<-hist(dailysteps,col='green')
bar<-barplot(dailysteps,col='green')
mean(dailysteps,na.rm=TRUE)
median(dailysteps,na.rm =TRUE)
averageActivitySteps<-aggregate(steps~interval,Activity,mean)
plot(averageActivitySteps$interval,averageActivitySteps$steps,type="l")
max_row_id<-which.max(averageActivitySteps$steps)
averageActivitySteps[max_row_id, ]
Totalnulls<-Activity[!complete.cases(Activity),]
for (i in 1:nrow(Activity)){
  if (is.na(Activity$steps[i])){
    interval_val <- Activity$interval[i]
    row_id <- which(averageActivitySteps$interval == interval_val)
    steps_val <- averageActivitySteps$steps[row_id]
    Activity$steps[i] <- steps_val
  }
}

imputed<-aggregate(steps~date,Activity,sum)
hist(imputed$steps, col=1, main="(Imputed) Histogram of total number of steps per day", xlab="Total number of steps in a day")
mean(imputed$steps)
median(imputed$steps)

# convert date from string to Date class
Activity$date <- as.Date(Activity$date, "%Y-%m-%d")

# add a new column indicating day of the week 
Activity$day <- weekdays(Activity$date)

# add a new column called day type and initialize to weekday

Activity$day_type <- c("weekday")

# If day is Saturday or Sunday, make day_type as weekend
for (i in 1:nrow(Activity)){
  if (Activity$day[i] == "Saturday" || Activity$day[i] == "Sunday"){
    Activity$day_type[i] <- "weekend"
  }
}

# convert day_time from character to factor
Activity$day_type <- as.factor(Activity$day_type)

# aggregate steps as interval to get average number of steps in an interval across all days
table_interval_steps_imputed <- aggregate(steps ~ interval+day_type, Activity, mean)

# make the panel plot for weekdays and weekends
library(ggplot2)
qplot(interval, steps, data=table_interval_steps_imputed, geom=c("line"), xlab="Interval", 
      ylab="Number of steps", main="") + facet_wrap(~ day_type, ncol=1)
