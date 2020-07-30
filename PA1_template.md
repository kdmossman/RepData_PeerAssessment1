---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
First let's load the libraries we need

```r
## Load libraries
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ ggplot2 3.3.1     ✓ purrr   0.3.4
## ✓ tibble  3.0.1     ✓ dplyr   1.0.0
## ✓ tidyr   1.1.0     ✓ stringr 1.4.0
## ✓ readr   1.3.1     ✓ forcats 0.5.0
```

```
## ── Conflicts ────────────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(lattice)
```

Now let's read in the data file and convert it to a convenient TBL

```r
## Read in data file
activity <- read.csv("activity.csv")
## Convert to tibble
activity.tbl <- as_tibble(activity)
```


## What is mean total number of steps taken per day?

```r
## Plot histogram of steps taken in a given day
stepsDay <- activity.tbl %>%
        group_by(date) %>%
        summarize(steps=sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
h <- hist(stepsDay$steps, breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
## Calculate mean, median of steps per day, over given time period
meanSteps <- mean(stepsDay$steps, na.rm=TRUE)
print("The mean number of steps per day is:") 
```

```
## [1] "The mean number of steps per day is:"
```

```r
meanSteps
```

```
## [1] 10766.19
```

```r
medianSteps <- median(stepsDay$steps, na.rm=TRUE)
print("The median number of steps per day is:") 
```

```
## [1] "The median number of steps per day is:"
```

```r
medianSteps
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
## Time series plot of the average number of steps taken
## Remove rows with NA values 
stepsDay <- stepsDay[complete.cases(stepsDay),] ## actually not necessary
## Create plot
plot(as.Date(stepsDay$date), stepsDay$steps, xlab = "Date", ylab = "Daily Steps",
     type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
## The 5-minute interval that, on average, contains the maximum number of steps
## Group by "interval" and calculate mean steps for each interval
intervalMeans <- activity.tbl %>%
        group_by(interval) %>%
        summarize(intervalMean=mean(steps, na.rm=TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
maxInterval <- intervalMeans[which.max(intervalMeans$intervalMean),] ## Max row
## Plot steps vs interval for visual inspection
plot(intervalMeans$interval, intervalMeans$intervalMean, xlab="5-Min Interval",
     ylab="Steps Per Interval", type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

```r
maxRow <- as.integer(maxInterval[1,1]) ## Number of max row
print("The interval with maximum number of steps is:")
```

```
## [1] "The interval with maximum number of steps is:"
```

```r
maxRow
```

```
## [1] 835
```

## Imputing missing values

```r
## Imputing missing values: if "steps" missing for an interval, replace NA with
## mean value of steps for that interval over the whole dataset

## Calculate total number of missing rows
missingSteps <- is.na(activity.tbl$steps) ## logical vector indicates missing
missingRows <- sum(missingSteps) ## Number of rows with missing steps data

## Create copy of activity table in preparation for imputing values
activeImpute <- activity.tbl
lengthData <- length(activeImpute$steps) ## Number of entries: 17568
## Loop through rows and impute data if necessary
for (i in 1:lengthData){
        if(is.na(activeImpute$steps[i])){
                ## identify interval for this row
                intervalNow <- activeImpute$interval[i]
                numInterval <- (intervalNow / 5) + 1 ## Will be from 1 to 388
                stepsImputed <- as.integer(intervalMeans$intervalMean[numInterval])
                ## replace NA with imputed value
                activeImpute$steps[i] <- stepsImputed
        }
}
## Plot histogram of steps with imputed data
stepsDayImputed <- activeImpute %>%
        group_by(date) %>%
        summarize(steps=sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
h <- hist(stepsDayImputed$steps, breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
## Calculate mean, median of steps per day including imputed data
## Calculate mean, median of steps per day, over given time period
meanStepsImputed <- mean(stepsDayImputed$steps, na.rm=TRUE)
print("The mean steps per day from imputed dataset is:")
```

```
## [1] "The mean steps per day from imputed dataset is:"
```

```r
meanStepsImputed
```

```
## [1] 10766.19
```

```r
medianStepsImputed <- median(stepsDayImputed$steps, na.rm=TRUE)
print("The median steps per day from imputed dataset is:")
```

```
## [1] "The median steps per day from imputed dataset is:"
```

```r
medianStepsImputed
```

```
## [1] 10765
```

## Are there differences in activity patterns between weekdays and weekends?

```r
## Add factor variable with two values: "weekday" and "weekend"
dayStatus <- vector(mode = "character", length = lengthData)
for (i in 1:lengthData){
        dayOfWeek <- wday(as.Date(activeImpute$date[i]), label = TRUE)
        if ((as.character(dayOfWeek) == "Sat") || (as.character(dayOfWeek) 
                                                   == "Sun"))
                {dayStatus[i] = "weekend"}
        else {dayStatus[i] = "weekday"}
}
dayStatus<-as.factor(dayStatus) ## Because I couldn't initialize factor vector

## Add the weekday/weekend column to the imputed steps data
activeImputeDay <- cbind(activeImpute, dayStatus)

## Calculate mean steps for intervals separately for weekdays and weekends
intervalMeansDay <- activeImputeDay %>%
        group_by(dayStatus, interval) %>%
        summarize(stepsMeanDay=mean(steps, na.rm=TRUE))
```

```
## `summarise()` regrouping output by 'dayStatus' (override with `.groups` argument)
```

```r
## Produce lattice plot to compare activity on weekdays & weekends
xyplot(stepsMeanDay ~ interval | dayStatus, data = intervalMeansDay, 
       layout = c(1, 2), xlab = "Interval", ylab="Mean Steps Per Interval",
     type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
