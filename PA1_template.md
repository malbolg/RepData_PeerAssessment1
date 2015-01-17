# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activity <- read.csv(unz("activity.zip", "activity.csv"), colClasses=c("numeric","Date","numeric"))
```

## What is mean total number of steps taken per day?

```r
steps <- activity$steps
hist(steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean(steps, na.rm=TRUE)
```

```
## [1] 37.3826
```

```r
median(steps, na.rm=TRUE)
```

```
## [1] 0
```

## What is the average daily activity pattern?

```r
# split time according to interval
bytime <- split(activity[,c("steps","interval")], activity$interval)

#compute means by interval
meansteps <- as.numeric(sapply(bytime, function(x) mean(x$steps, na.rm=TRUE)))

#get the x axis intervals for plotting
intervals <- unique(activity$interval)

plot(intervals,meansteps, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

## Imputing missing values
For imputing I simply use the rounded mean values from the specific interval of time, i.e., the meansteps data from the last part.

```r
# get the rows that have missing values
missing <- activity[!complete.cases(activity),]
# print number of rows with missing values
dim(missing)[1]
```

```
## [1] 2304
```

```r
defaultSteps <- data.frame(steps=meansteps, interval=intervals)
imputedSteps <- merge(missing, defaultSteps, by.x="interval", by.y="interval")

# rename the imputed steps column and round it
imputedSteps$steps <- round(imputedSteps$steps.y,0)

# delete unnecessary columns
imputedSteps$steps.x <- NULL
imputedSteps$steps.y <- NULL

# activityImputed is the same dataset as activity, with the imputed values
activityImputed <- merge(activity, imputedSteps, by=c("date","interval"), all.x=TRUE)

# use default values
activityImputed$steps <- activityImputed$steps.x

# choose the number of steps that is not NA
for (i in 1:dim(activity)[1]) {
        if (is.na(activityImputed[i,"steps"])) activityImputed[i,"steps"] <- activityImputed[i,"steps.y"] 
}
activityImputed$steps.x <- NULL
activityImputed$steps.y <- NULL



steps <- activityImputed$steps
hist(steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
# na.rm=FALSE should work as well since there should NOT be any NA values!
mean(steps, na.rm=FALSE)
```

```
## [1] 37.38069
```

```r
median(steps, na.rm=FALSE)
```

```
## [1] 0
```
After imputing, the mean differs by about 0.002 and the histogram has a bit higher spike at the lower end of the spectrum, probably because most of NAs correspond to only a few steps at max.


## Are there differences in activity patterns between weekdays and weekends?

```r
activityImputed$weekday <- weekdays(activityImputed$date)

# This kind of looping is slow but works
for (i in 1:dim(activity)[1]) {
        weekday <- activityImputed[i,"weekday"]
        if (weekday =="Sunday" || weekday=="Saturday") activityImputed[i, "weekday"] <- "weekend"
        else activityImputed[i,"weekday"]  <- "weekday"
}

# the following lines only compute the mean by interval and weekday for plotting
# I am betting there is some single plot call type to plot this directly.
byweekday <- split(activityImputed, activityImputed$weekday)
weekdays <- byweekday$weekday
weekends <- byweekday$weekend

weekdayints <- split(weekdays, weekdays$interval)
weekendints <- split(weekends, weekends$interval)

weekdaymeans <- as.numeric(lapply(weekdayints, function(x) mean(x$steps, na.rm=TRUE)))
weekendmeans <- as.numeric(lapply(weekendints, function(x) mean(x$steps, na.rm=TRUE)))


intervals <- unique(activityImputed$interval)

weekdayframe <- data.frame(steps=weekdaymeans, interval=intervals, weekday=rep("weekday",288))
weekendframe <- data.frame(steps=weekendmeans, interval=intervals, weekday=rep("weekend",288))

plottableframe <- data.frame(steps=c(weekdayframe$steps, weekendframe$steps),
                                interval=c(weekdayframe$interval, weekendframe$interval),
                                weekday=c(as.character(weekdayframe$weekday), as.character(weekendframe$weekday)))


#compute means by interval and weekday

library(lattice)
# finally plot the means
xyplot(steps~interval|weekday,data=plottableframe, layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
