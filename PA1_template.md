---
title: "Reproducible data assessment"
output: 
  html_document: 
    keep_md: yes
---



## Loading and preprocessing the data



```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
file <- "C:/Coursera/repdata/activity.csv"
data <- read.csv(file, header = TRUE, sep = ",")
```

## What is mean total number of steps taken per day?


### 1. Histogram of total number of steps taken each day

```r
activity_stepsday <-aggregate(steps ~ date,  data, sum, na.rm = TRUE)
```


```r
hist(activity_stepsday$steps, xlab = "steps per day", main = "Total number of steps taken each day")
```

![](PA1_template_files/figure-html/histogram-1.png)<!-- -->

### 2. Calculate and report the mean and median total number of steps taken per day

```r
mean(activity_stepsday$steps)
```

```
## [1] 10766.19
```

```r
median(activity_stepsday$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
activity_interval <-aggregate(steps ~ interval,  data, mean, na.rm = TRUE)
```


```r
plot(activity_interval$interval, activity_interval$steps, type = "l", xlab = "Intervals", main = "Steps per interval")
```

![](PA1_template_files/figure-html/plot-1.png)<!-- -->

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maximum <- max(activity_interval$steps)
max_steps <- filter(activity_interval, activity_interval$steps == maximum)
max_steps
```

```
##   interval    steps
## 1      835 206.1698
```
The interval 865 has the highest number of steps for a 5 minutes interval (206.1698)

## Imputing missing values

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(data))
```

```
## [1] 2304
```

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Overall average strategy:


```r
activity_daymean <-aggregate(steps ~ date,  data, mean, na.rm = TRUE)
head(activity_daymean)
```

```
##         date    steps
## 1 2012-10-02  0.43750
## 2 2012-10-03 39.41667
## 3 2012-10-04 42.06944
## 4 2012-10-05 46.15972
## 5 2012-10-06 53.54167
## 6 2012-10-07 38.24653
```

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
dataNA <- data
dataNA$steps[which(is.na(dataNA$steps))] <- mean(dataNA$steps, na.rm = TRUE)
```

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
dataNA_perday <- aggregate(steps ~ date, dataNA, sum, na.rm = TRUE)
mean(activity_stepsday$steps)
```

```
## [1] 10766.19
```

```r
median(activity_stepsday$steps)
```

```
## [1] 10765
```

```r
par(mfrow = c(1,2))
hist(activity_stepsday$steps, xlab = "steps per day", main = "Total number of steps taken each day", col = "red")
hist(dataNA_perday$steps, xlab = "steps per day (imputed NA)", main = "Total number of steps taken each day", col = "green")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Imputing missing values did not change the values (either mean nor median). Both histograms show similar figures.

## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
dataNAdate <- as.POSIXct(dataNA$date)
dataNA$date <- dataNAdate
dataNA <- mutate(dataNA, weekdays = "")
dataNA$weekdays <- ifelse( weekdays(dataNA$date) == "sábado" | weekdays(dataNA$date) == "domingo", "weekend", "weekday")
dataNA$weekdays <- factor(dataNA$weekdays)
str(dataNA)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  37.4 37.4 37.4 37.4 37.4 ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ weekdays: Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

### Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
steps_interval_weekday <- aggregate(steps ~ interval + weekdays, dataNA, mean)
#ggplot2
library(ggplot2)
g <- ggplot(steps_interval_weekday, aes(interval, steps))
g + geom_line(color = "blue") + facet_grid(weekdays~.) + labs(x = "Intervals", y ="Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
