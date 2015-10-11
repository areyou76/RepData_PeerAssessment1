---
title: "PA1_template.Rmd"
author: "areyou76"
date: "October 11, 2015"
output: html_document
---

echo = TRUE  
options(scipen = 1)  


Loading and preprocessing the data

##Show any code that is needed to

1. Load the data (i.e. read.csv())

2. Process/transform the data (if necessary) into a format suitable for your analysis

## Load data

data <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
data$month <- as.numeric(format(data$date, "%m"))
noNA <- na.omit(data)
rownames(noNA) <- 1:nrow(noNA)
head(noNA)
dim(noNA)
library(ggplot2)


##What is mean total number of steps taken per day?
##For this part of the assignment, you can ignore the missing values in the dataset.

##1. Calculate the total number of steps taken per day


ggplot(noNA, aes(date, steps)) + geom_bar(stat = "identity", colour = "steelblue", fill = "steelblue", width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")


##2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


totalSteps <- aggregate(noNA$steps, list(Date = noNA$date), FUN = "sum")$x



##3. Calculate and report the mean and median of the total number of steps  taken per day

mean(totalSteps)
median(totalSteps)

## What is the average daily activity pattern?

##1. Make a time series plot (i.e. type = "l") of the 5-minute interval 
(x-axis) and the average number of steps taken, averaged across all days  (y-axis)

avgSteps <- aggregate(noNA$steps, list(interval = as.numeric(as.character(noNA$interval))), FUN = "mean")
names(avgSteps)[2] <- "meanOfSteps"

ggplot(avgSteps, aes(interval, meanOfSteps)) + geom_line(color = "steelblue", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")

##2. Which 5-minute interval, on average across all the days in the ## dataset, contains the maximum number of steps?

avgSteps[avgSteps$meanOfSteps == max(avgSteps$meanOfSteps), ]

## Imputing missing values

##1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


sum(is.na(data))

##2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

##3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

newData <- data 
for (i in 1:nrow(newData)) {
    if (is.na(newData$steps[i])) {
        newData$steps[i] <- avgSteps[which(newData$interval[i] == avgSteps$interval), ]$meanOfSteps
    }
}

head(newData)
sum(is.na(newData))


##4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


ggplot(newData, aes(date, steps)) + geom_bar(stat = "identity",
       colour = "steelblue",
       fill = "steelblue",
       width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day (no missing data)", x = "Date", y = "Total number of steps")

##Mean total number of steps taken per day:       
       
newTotalSteps <- aggregate(newData$steps,list(Date = newData$date),FUN = "sum")$x
newMean <- mean(newTotalSteps)
newMean


##Median total number of steps taken per day:

newMedian <- median(newTotalSteps)
newMedian

##comparision before imputing missing data:

oldMean <- mean(totalSteps)
oldMedian <- median(totalSteps)
newMean - oldMean
newMedian - oldMedian


##Are there differences in activity patterns between weekdays and weekends?

##For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

##1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


head(newData)
newData$weekdays <- factor(format(newData$date, "%A"))
levels(newData$weekdays)
levels(newData$weekdays) <- list(weekday = c("Monday", "Tuesday",
        "Wednesday","Thursday", "Friday"), weekend = c("Saturday", "Sunday"))
levels(newData$weekdays)
table(newData$weekdays)


##2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


avgSteps <- aggregate(newData$steps, list(interval = as.numeric(as.character(newData$interval)),weekdays = newData$weekdays), FUN = "mean")
names(avgSteps)[3] <- "meanOfSteps"
library(lattice)
xyplot(avgSteps$meanOfSteps ~ avgSteps$interval | avgSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
       
       
       
       
       
       