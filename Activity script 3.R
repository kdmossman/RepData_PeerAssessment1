## Week 2 Reproducible Research Project

## Load libraries
library(tidyverse)
library(lubridate)
library(lattice)

## Read in data file
activity <- read.csv("activity.csv")
## Convert to tibble
activity.tbl <- as_tibble(activity)

## Plot histogram of steps taken in a given day
stepsDay <- activity.tbl %>%
        group_by(date) %>%
        summarize(steps=sum(steps))

h <- hist(stepsDay$steps, breaks = 20)

## Calculate mean, median of steps per day, over given time period
meanSteps <- mean(stepsDay$steps, na.rm=TRUE)
medianSteps <- median(stepsDay$steps, na.rm=TRUE)

## Time series plot of the average number of steps taken
## Remove rows with NA values 
stepsDay <- stepsDay[complete.cases(stepsDay),] ## actually not necessary
## Create plot
plot(as.Date(stepsDay$date), stepsDay$steps, xlab = "Date", ylab = "Daily Steps")

## The 5-minute interval that, on average, contains the maximum number of steps
## Group by "interval" and calculate mean steps for each interval
intervalMeans <- activity.tbl %>%
        group_by(interval) %>%
        summarize(intervalMean=mean(steps, na.rm=TRUE))

maxInterval <- intervalMeans[which.max(intervalMeans$intervalMean),] ## Max row
## Plot steps vs interval for visual inspection
plot(intervalMeans$interval, intervalMeans$intervalMean, xlab="5-Min Interval",
     ylab="Steps Per Interval")
maxRow <- as.integer(maxInterval[1,1]) ## Number of max row

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
h <- hist(stepsDayImputed$steps, breaks = 20)

## Calculate mean, median of steps per day including imputed data
## Calculate mean, median of steps per day, over given time period
meanStepsImputed <- mean(stepsDayImputed$steps, na.rm=TRUE)
medianStepsImputed <- median(stepsDayImputed$steps, na.rm=TRUE)

## Are there differences in activity patterns between weekdays and weekends?
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

## Produce lattice plot to compare activity on weekdays & weekends
xyplot(stepsMeanDay ~ interval | dayStatus, data = intervalMeansDay, 
       layout = c(1, 2), xlab = "Interval", ylab="Mean Steps Per Interval")
