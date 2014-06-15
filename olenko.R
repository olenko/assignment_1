activity <- read.csv("activity.csv", header=TRUE)
head(activity)
daily <- aggregate(steps ~ date, data=activity, FUN=sum,  na.action = na.omit)
head(daily)
hist(daily$steps,breaks=15, xlab = "Steps")
mean(daily$steps)
median(daily$steps)

min5average <- aggregate(steps ~ interval, data=activity, FUN=mean,  na.action = na.omit)
plot(min5average, type="l", xlab="time", xaxt="n")
axis(1, at = c(0,400,800,1200,1600,2000,2355),labels=c("00:00","4:00","8:00","12:00","16:00","20:00","24:00"))
maxinterval <- min5average$interval[min5average$steps==max(min5average$steps)]
maxinterval
min5average$steps[min5average$interval==maxinterval]

activityfill <- activity
sum(is.na(activity$steps)) 
for (i in 1:length(activity$steps)){
  if (is.na(activity$steps[i])==TRUE)
{activityfill$steps[i] <- min5average$steps[min5average$interval==activity$interval[i]]}
}
head(activityfill)
sum(is.na(activityfill$steps)) 

dailyfill <- aggregate(steps ~ date, data=activityfill, FUN=sum)
hist(dailyfill$steps,breaks=15)
mean(dailyfill$steps)
median(dailyfill$steps)
hist(dailyfill$steps-daily$steps,breaks=15)


activityfill$weekends <- ifelse(weekdays(as.Date(activityfill$date))=="Saturday" | weekdays(as.Date(activityfill$date))=="Sunday", "Weekend","Weekday")
activityfill$weekends <- as.factor(activityfill$weekends)

activi<- aggregate(steps ~ interval + weekends, data=activityfill, FUN=mean)
xyplot(steps~interval | weekends, data=activi, type="l", layout=c(1,2))

# str(activityfill$weekends)
# min5weekends <- aggregate(steps ~ interval, data=activityfill[activityfill$weekends == "Weekend",], FUN=mean)
# min5weekdays <- aggregate(steps ~ interval, data=activityfill[activityfill$weekends == "Weekday",], FUN=mean)
# par(mfrow=c(2,1))
# plot(min5weekends, type="l", xlab="time", xaxt="n", main= "Weekends", ylim=c(0,250))
# axis(1, at = c(0,400,800,1200,1600,2000,2355),labels=c("00:00","4:00","8:00","12:00","16:00","20:00","24:00"))
# plot(min5weekdays, type="l", xlab="time", xaxt="n", main= "Weekdays", ylim=c(0,250))
# axis(1, at = c(0,400,800,1200,1600,2000,2355),labels=c("00:00","4:00","8:00","12:00","16:00","20:00","24:00"))
# 
