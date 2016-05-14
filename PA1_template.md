# Reproducible Research: Peer Assessment 1
Guillermo Pachón  
14 de mayo de 2016  


## Loading and preprocessing the data

1. Load the data.


```r
activity <- read.table(unz("activity.zip", "activity.csv"), header=T, sep=",", na.strings = c("NA"))
dim(activity)
```

```
## [1] 17568     3
```

```r
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

As you can see, 17568 rows with 3 columns (steps, date, interval) where loaded.

2. Process/transform the data (if necessary) into a format suitable for your analysis.

Update the format for *date* column:


```r
activity$date <- as.Date(as.character(activity$date), "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

Lets ignore the missing values.


```r
activity1 <- na.omit(activity)
dim(activity1)
```

```
## [1] 15264     3
```

1. Calculate the total number of steps taken per day.


```r
stepsByDay <- with(activity1, tapply(steps, as.factor(date), sum, na.rm = TRUE))
summary(stepsByDay, digits=5)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

2. Make a histogram of the total number of steps taken each day.


```r
hist(stepsByDay, main = "Histogram of Steps By Day")
abline(v = mean(stepsByDay), lwd = 2, col = "red")
abline(v = median(stepsByDay), lwd = 1, col = "blue")
legend("topright", lwd = 1, col = c("red", "blue"), legend = c("Mean", "Median"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day.

The mean (10766) and the median (10765) 
are shown in the same position.

## What is the average daily activity pattern?

Calculate the average steps by interval for all days and make a time series plot 
of the 5-minute interval and the average number of steps taken.  


```r
StepsByInterval <- aggregate(steps ~ interval, data = activity1, mean)
```

Next calculate the 5-minute interval that contains the max-average number of steps.


```r
MaxSBI <- StepsByInterval[StepsByInterval$steps == max(StepsByInterval$steps),]$interval
```

1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
with(StepsByInterval, plot(interval, steps, type = "l", main = "Average Daily Activity Pattern", 
                              xlab = "5-minute Interval", ylab = "Average Steps"))
abline(v = MaxSBI, lwd = 1, col = "red")
legend("topright", lwd = 1, col = c("red"), legend = c(paste("Max = ", MaxSBI)))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

As the graphic show, the 5-minute interval, on average across all the days in the dataset, 
that contains the maximum number of steps is the **835**.

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).

The following code show if the columns has any NA value:


```r
c(anyNA(activity$steps), anyNA(activity$date), anyNA(activity$interval))
```

```
## [1]  TRUE FALSE FALSE
```

So the number of missing values in the data set is determined by the missing values in **steps** column:


```r
nrow(activity[is.na(activity$steps),])
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset.

Stretegy selected: *Use the mean for that 5-minute interval for filling the missing values:*

Use this custom function:


```r
MyFillNA <- function(act, meanSBI) {
    for(i in 1:nrow(act)) {
        if (is.na(act$steps[i])) {
            ## If step is NA, assing mean steps for the corresponding interval
            act$steps[i] = meanSBI[meanSBI$interval == act[i,]$interval, ]$steps
        }
    }
    act
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in:


```r
activity2 <- MyFillNA(activity, StepsByInterval)
dim(activity2)
```

```
## [1] 17568     3
```

```r
summary(activity2)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean
and median total number of steps taken per day.

Calculate the total number of steps taken per day.


```r
stepsByDay2 <- with(activity2, tapply(steps, as.factor(date), sum))
```

Make the histogram.


```r
hist(stepsByDay2, main = "Histogram of Steps By Day")
abline(v = mean(stepsByDay2), lwd = 2, col = "red")
abline(v = median(stepsByDay2), lwd = 1, col = "blue")
legend("topright", lwd = 1, col = c("red", "blue"), legend = c("Mean", "Median"))
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

The mean (10766) and the median (10766) 
are shown in the same position.

**These values does NOT differ from the estimates from the first part of the assignment**. Remember the values: mean = 10766, and median = 10765. 

The impact of imputing missing data on the estimates of the total daily number of steps is **NONE**.

## Are there differences in activity patterns between weekdays and weekends?

Using the dataset with the fillen-in missing values (*activity2*).

1. Create a new factor variable in the dataset with two levels indicating whether a given date is a weekday or weekend day.


```r
activity2$isWeekend <- format(activity2$date, "%u") %in% c(6, 7)
activity2$DayType <- factor(activity2$isWeekend,labels=c("weekend","weekday"))
StepsByInterval2 <- aggregate(steps ~ interval + DayType, data = activity2, mean)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(ggplot2)
g <- ggplot(StepsByInterval2, aes(interval, steps)) + geom_line() + facet_grid(. ~ DayType)
g + labs(title = "Differences in Activity Patterns Between Weekdays and Weekends", x = "5-minute Interval", y = "Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

