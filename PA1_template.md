# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
> setwd("~/Documents/Uni Stuff/Coursera/Reproducible Research/Project 1")
> unzip(zipfile = "repdata_data_activity.zip")
> activitydata <- read.csv("activity.csv")
> library(dplyr)
> head(activitydata)
  steps       date interval
1    NA 2012-10-01        0
2    NA 2012-10-01        5
3    NA 2012-10-01       10
4    NA 2012-10-01       15
5    NA 2012-10-01       20
6    NA 2012-10-01       25
> dim(activitydata)
[1] 17568     3
> glimpse(activitydata)
Observations: 17,568
Variables: 3
$ steps    <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
$ date     <date> 2012-10-01, 2012-10-01, 2012-10-01, 2012-10-01, 2012-10-01, 2012-...
$ interval <int> 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 100, 105, 110, 115, ...
> summary(activitydata)
     steps             date               interval     
 Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
 1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
 Median :  0.00   Median :2012-10-31   Median :1177.5  
 Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
 3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
 Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
 NA's   :2304 
 
## this transforms character to dates
> activitydata$date<- as.Date(activitydata$date)

## What is mean total number of steps taken per day?
## Calculate the total number of steps taken per day:
>library(ggplot2)
> Total_Steps<- activitydata%>%
  +     group_by(date)%>%
  +     filter(!is.na(steps))%>%
  +     summarise(total_steps = sum(steps, na.rm=TRUE))
## Plots histogram using ggplot:
> ggplot(Total_Steps, aes(x = total_steps)) +
  +     geom_histogram(fill = "blue", binwidth = 1000) +
  +     labs(title = "Daily Steps", x = "Total Steps", y = "Frequency")
## Mean and median of the total number of steps taken per day:
> Mean_Steps<- mean(Total_Steps$total_steps, na.rm=TRUE)
> Mean_Steps
[1] 10766.19
> Median_Steps<- median(Total_Steps$total_steps, na.rm=TRUE)
> Median_Steps
[1] 10765


## What is the average daily activity pattern?
## This calculates the average steps:
> Interval<- activitydata%>%
  +     group_by(interval)%>%
  +     filter(!is.na(steps))%>%
  +     summarise(avg_steps = mean(steps, na.rm=TRUE))
> Interval

# A tibble: 288 x 2
   interval avg_steps
      <int>     <dbl>
1         0 1.7169811
2         5 0.3396226
3        10 0.1320755
4        15 0.1509434
5        20 0.0754717
6        25 2.0943396
7        30 0.5283019
8        35 0.8679245
9        40 0.0000000
10       45 1.4716981
# ... with 278 more rows

## this plots the average steps using ggplot:
> ggplot(Interval, aes(x =interval , y=avg_steps)) +
  +     geom_line(color="blue", size=1) +
  +     labs(title = "Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")
  
## this shows the 5-minute interval on average across all the days in the dataset, 
##which contains the maximum number of steps?
> Interval[which.max(Interval$avg_steps),]

# A tibble: 1 x 2
  interval avg_steps
     <int>     <dbl>
1      835  206.1698


## Imputing missing values
## Total number of missing values in the dataset:
>sum(is.na(activitydata$steps))

## [1] 2304

## Imputing missing values using mean of each day and creating a new dataset like
## the original with the missing values filled in:
> activitydata2<- activitydata
> nas<- is.na(activitydata2$steps)
> avg_interval<- tapply(activitydata2$steps, activitydata2$interval, mean, na.rm=TRUE, simplify = TRUE)
> activitydata2$steps[nas] <- avg_interval[as.character(activitydata2$interval[nas])]
> names(activitydata2)

## [1] "steps"    "date"     "interval"


## This checks if there are missing values still in the dataset:
>sum(is.na(activitydata2))

## [1] 0

## reorders the columns to easily understand:
> activitydata2<- activitydata2[, c("date", "interval", "steps")]
> head(activitydata2)

##         date interval     steps
## 1 2012-10-01        0 1.7169811
## 2 2012-10-01        5 0.3396226
## 3 2012-10-01       10 0.1320755
## 4 2012-10-01       15 0.1509434
## 5 2012-10-01       20 0.0754717
## 6 2012-10-01       25 2.0943396

## this creates a histogram of total no. of steps taken each day and also the mean
## and median of total no. of steps taken each day:
> Total_Steps2<- activitydata2%>%
  +     group_by(date)%>%
  +     summarise(total_steps = sum(steps, na.rm=TRUE))
> Total_Steps2

# A tibble: 61 x 2
         date total_steps
       <date>       <dbl>
1  2012-10-01    10766.19
2  2012-10-02      126.00
3  2012-10-03    11352.00
4  2012-10-04    12116.00
5  2012-10-05    13294.00
6  2012-10-06    15420.00
7  2012-10-07    11015.00
8  2012-10-08    10766.19
9  2012-10-09    12811.00
10 2012-10-10     9900.00
# ... with 51 more rows

> ggplot(Total_Steps2, aes(x = total_steps)) +
  +     geom_histogram(fill = "blue", binwidth = 1000) +
  +     labs(title = "Daily Steps including Missing values", x = "Interval", y = "No. of Steps")
  
> Mean_Steps2<- mean(Total_Steps2$total_steps, na.rm=TRUE)
> Mean_Steps2
[1] 10766.19

> Median_Steps2<- median(Total_Steps2$total_steps, na.rm=TRUE)
> Median_Steps2
[1] 10766.19

##Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
 ##Answer:Only the median differs and also the impact of the missing data on the estimates is that mean and median are the same with a value of: 10766.19

## Are there differences in activity patterns between weekdays and weekends?
> head(activitydata2)
        date interval     steps
1 2012-10-01        0 1.7169811
2 2012-10-01        5 0.3396226
3 2012-10-01       10 0.1320755
4 2012-10-01       15 0.1509434
5 2012-10-01       20 0.0754717
6 2012-10-01       25 2.0943396

##this creates a new variable for week type (weekday or weekend):
> activitydata2<- activitydata2%>%
  +     mutate(weektype= ifelse(weekdays(activitydata2$date)=="Saturday" | weekdays(a
  +     ctivitydata2$date)=="Sunday", "Weekend", "Weekday"))

> head(activitydata2)
        date interval     steps weektype
1 2012-10-01        0 1.7169811  Weekday
2 2012-10-01        5 0.3396226  Weekday
3 2012-10-01       10 0.1320755  Weekday
4 2012-10-01       15 0.1509434  Weekday
5 2012-10-01       20 0.0754717  Weekday
6 2012-10-01       25 2.0943396  Weekday

## plotting:
> Interval2<- activitydata2%>%
  +     group_by(interval, weektype)%>%
  +     summarise(avg_steps2 = mean(steps, na.rm=TRUE))
> head(Interval2)
Source: local data frame [6 x 3]
Groups: interval [3]

  interval weektype avg_steps2
     <int>    <chr>      <dbl>
1        0  Weekday 2.25115304
2        0  Weekend 0.21462264
3        5  Weekday 0.44528302
4        5  Weekend 0.04245283
5       10  Weekday 0.17316562
6       10  Weekend 0.01650943

> plot<- ggplot(Interval2, aes(x =interval , y=avg_steps2, color=weektype)) +
  +     geom_line() +
  +     labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") +
  +     facet_wrap(~weektype, ncol = 1, nrow=2)

 ## Answer:During the weekdays, the object is more active earlier however during the weekends the object is more active throughout the day. This may be due to work on weekdays creating less movement and more time during the weekends to be active.
