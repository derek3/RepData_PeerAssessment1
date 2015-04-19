# Reproducible Research: Peer Assessment 1

```r
## Loading and preprocessing the data
rawdata <- read.csv("./activity/activity.csv")

# Remove days of NA data from original/raw data
data <- rawdata[complete.cases(rawdata),]
```



## What is mean total number of steps taken per day?


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(chron)

#1. Total steps per day
steps.per.day <- summarize(group_by(data,date),sum(steps))

#2. Histogram of steps per day
hist(steps.per.day$"sum(steps)",col="red",xlab="Total Steps per Day",
     main="Frequency of Total Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
#3. Mean and median of total steps per day
mean.steps.per.day <- summarize(steps.per.day,mean(steps.per.day$"sum(steps)"))
median.steps.per.day <- summarize(steps.per.day,median(steps.per.day$"sum(steps)"))
```
The mean total number of steps taken per day is 1.0766189\times 10^{4} 
and the median is 10765.  

## What is the average daily activity pattern?

```r
#1. Plot average steps taken per interval(x) vs. all days (y)
avg.steps.per.interval <- summarize(group_by(data,interval),mean(steps))
avg.steps.per.interval$interval <- formatC(avg.steps.per.interval$interval, 
                                           width=4, format="d", flag="0")
avg.steps.per.interval$interval <- strptime(avg.steps.per.interval$interval,format="%H%M")
plot(avg.steps.per.interval$interval,avg.steps.per.interval$"mean(steps)",type="l",
     xlab="24 Hr. Time",ylab="Average Steps",main="Average Steps per Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
#2. Maximum average 5-minute interval
max.avg.interval <- avg.steps.per.interval[which.max(avg.steps.per.interval$"mean(steps)"),]
```




## Imputing missing values

```r
#1. Calculate and report the total number of missing values in the dataset
missing.values <- rawdata[!complete.cases(rawdata),]
number.missing.values <- length(missing.values$steps)

#2. Fill in missing values with means from complete days' five-minute intervals
mean.steps.per.interval <- summarize(group_by(data,interval),mean(steps))
# library(sqldf)
# means.for.missing.values <- sqldf("select interval,steps from 
#                                   'mean.steps.per.interval' inner join 'missing.values'
#                                   on 'mean.steps.per.interval$mean(steps)' = 'missing.values$steps'")
# means.for.missing.values$steps <- factor(mean.steps.per.interval$steps,
#                                   levels=mean.steps.per.interval$interval,
#                                   labels=mean.steps.per.interval$"mean(steps)")


means.for.missing.values <- missing.values
match.indices <- match(means.for.missing.values$interval,mean.steps.per.interval$interval)
means.for.missing.values[,"steps"] <- mean.steps.per.interval[match.indices,"mean(steps)"]

#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
# Combined data subset with NAs removed with a data subset with means replacing the NAs
imputeddata <- rbind(data[,1:3],means.for.missing.values[,1:3])



#4. Histogram for imputed data and mean and median
imputed.steps.per.day <- summarize(group_by(imputeddata,date),sum(steps))

hist(imputed.steps.per.day$"sum(steps)",col="red",xlab="Total Steps per Day",
     main="Frequency of Total Steps per Day (Imputed Data)")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
# Impact of imputing missing on mean and median.
mean.imputed.steps.per.day <- summarize(imputed.steps.per.day,mean(imputed.steps.per.day$"sum(steps)"))
median.imputed.steps.per.day <- summarize(imputed.steps.per.day,median(imputed.steps.per.day$"sum(steps)"))

# Mean does not change (not surprising), but the median changes slightly from 10765 to 
# 10766.19 (which is the mean).
```



## Are there differences in activity patterns between weekdays and weekends?


```r
weekdays.data <- data
weekdays <- weekdays(as.Date(weekdays.data$date))
weekdays <- factor(weekdays)
weekdays.data <- cbind(weekdays.data,weekdays)

daytype <- ifelse(weekdays.data$weekdays=="Sunday"|
                    weekdays.data$weekdays=="Saturday"
                  ,"weekend", "weekday")
daytype <- as.factor(daytype)
weekdays.data <- cbind(weekdays.data,daytype)

weekdays.avg.steps.per.interval <- summarize(group_by(weekdays.data,daytype,interval),mean(steps))

# Plot the results
library(ggplot2)
ggplot(weekdays.avg.steps.per.interval,aes(interval,weekdays.avg.steps.per.interval$"mean(steps)")) + geom_line(aes(color=daytype)) +
  facet_grid(daytype~.) + xlab("Interval") + ylab("Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
# Yes, there is a difference in the patterns of steps between weekdays and weekends. On weekdays, there are more
# steps taken early in the day, presumably as the subject is going to work, then fewer steps throughout the day.
# On weekends, there are more steps taken in the core of the day and extending slightly longer into
# the early evening, likely due to weekend activities.
```

