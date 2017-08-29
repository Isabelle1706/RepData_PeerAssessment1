
Reproducible Research Project 1

Isabelle You

August 28, 2017


Loading and preprocessing the activity data


1.Unizip the activity data and read into R.
zipf="C:/Users/iyou/Documents/R/coursera/repdata_data_activity.zip"
outdir="C:/Users/iyou/Documents/R/coursera"
unzip(zipf, exdir=outdir)

activity=read.csv("C:/Users/iyou/Documents/R/coursera/repdata_data_activity/activity.csv", sep=",", header=TRUE,  stringsAsFactors = FALSE)
str(activity)
## 'data.frame':    17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
summary(activity)
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304


2.clean-up the activity data: converte data from character to date.
activity$date=as.Date(activity$date)

class(activity$date)
## [1] "Date"


What is mean total number of steps taken per day


1. Make a histogram of the total number of steps taken each day?

Calculate the total number of steps each day
stepsperday=aggregate(steps~date, activity, sum, na.rm=TRUE)

Histogram of the total number of steps each day:
hist(stepsperday$steps, breaks=20, col="green", 
     xlab="Steps Per Day", ylab="Number of Days", main="Total Steps Per Day")
     abline(v=median(stepsperday$steps), lty=1, lwd=2, col="red")
     legend(legend="Median", "topright", lty=1, lwd=2, col="red")




2. Calculate and report the mean and median total number of steps taken per day
mean_steps=round(mean(stepsperday$steps, na.rm=T))
median_steps=median(stepsperday$steps, na.rm=T)

The mean of total number of steps per day is1.076610^{4}. The median of total number of steps per day is 10765.


What is the average daily activity pattern?


Make a time series plot (i.e=“l”) of the 5-minute interval(x-axis) and average numbers taken, average acroos all days(y-axis)

Calcalate the average steps acroos all days by interval
mean_steps_interval=aggregate(steps~interval, activity, mean, na.rm=T)

The time series plot of averaged munber of steps taken acrross the intervals:
library(ggplot2)

g <- ggplot(mean_steps_interval, aes(interval, steps))
g + geom_line() +
        geom_point() +
        xlab("Interval") +
        ylab("Mean steps") +
        ggtitle("Mean Steps Across All Day by Intervals")




2. Which 5-minute interval, on the average across all the days in the dataset, contains the maximum number of steps?
maxsteps=max(mean_steps_interval$steps, na.rm=T)

interval_max=subset(mean_steps_interval, steps==maxsteps)

interval_max_steps=interval_max[1,1]

The 5-minute interval, on avaerage across all the day in the dataset, contains the max steps is 835.


Imputing missing values


1. Calculate and report the total number of rows with missing values in the dataset
nrowswithNA=nrow(activity[!complete.cases(activity),])

The total number of rows with NA is 2304.


2. Devise a stratege to fill in all of missing values in the dataset

First check those uncompleted cases:
uncompleted=activity[!complete.cases(activity),]

impute the missing values of steps in activity dataset using the mean of 5-minute interval of steps.
# remane steps to mean_steps

library(plyr)

mean_steps_interval=rename(mean_steps_interval, c("steps"="mean_step"))

# merge activity and mean_steps_interval two datasets

activity=merge(activity, mean_steps_interval, by="interval")

# impute missing values of steps using interval mean steps

activity$imputed_steps=ifelse( is.na(activity$steps), activity$mean_step, activity$steps)


3. Create a new dataset that is equal to tht original dataset but with the missing data filled in.
library(dplyr)
## Warning: package 'dplyr' was built under R version 3.4.1
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
activity_new=activity %>%
        select(interval, imputed_steps, date)


4. Make a histogram of the total number of steps take each day and calulate and report the mean and median after missing values are imputed

Calculate the total number of steps each day after missing values are imputed with the mean of 5-minute interval of steps:
stepsperday1=aggregate(imputed_steps~date, activity_new, sum)

Histogram of the total number of steps each day after missing values are imputed with the mean of 5-minute interval of steps:
hist(stepsperday1$imputed_steps, breaks=20, col="blue", 
     xlab="Steps Per Day", ylab="Number of Days", main="Total Steps Per Day after Missing Values Imputed")
     abline(v=median(stepsperday$steps), lty=1, lwd=2, col="red")
     legend(legend="Median", "topright", lty=1, lwd=2, col="red")



The mean and median steps each day after missing values are imputed with the mean of 5-minute interval of steps:
mean_steps=round(mean(stepsperday1$imputed_steps))
median_steps=round(median(stepsperday1$imputed_steps))

The mean of total number of steps per day is1.076610^{4}. The median of total number of steps per day is 1.076610^{4}.


Are there differences in activity patters between weekdays and weekends?


1. Create a nre factor variable in the dataset with two levels–“weekday” and “weekend”
activity_new$week_days=weekdays(activity_new$date)

activity_new$weekday=ifelse(activity_new$week_days %in% c("Saturday", "Sunday"), "weekend", "weekday")


2. Make a plane containing a time series plot (i.e. type=“l”) of the 5-minute interval and average numbers of steps taken, avaraged across all weekdays or weekenddays.
# calculate mean steps by interval and weekdays or weekend

steps_interval_weekdays=aggregate(imputed_steps~interval+weekday, activity_new, mean)

# line plots

# make line plot using ggplot2

library(ggplot2)

g <- ggplot(steps_interval_weekdays, aes(interval, imputed_steps, col=weekday))
g + geom_line() +
        geom_point() +
        facet_grid(weekday~.) +
        xlab("Interval") +
        ylab("Mean steps") +
        ggtitle("Mean Steps Across 5-minute Intervals by Weekdays or Weekend")


