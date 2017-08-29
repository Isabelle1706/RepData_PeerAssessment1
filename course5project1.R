
# Reproducible research project 1

# 1. Loading and preprocessing the data

# unzip the activity data


unzip("~R/coursera/repdata_data_activity.zip")activity=read.csv("~R/coursera/activity.csv",sep=",", stringsAsFactors = FALSE)
str(activity)

zipf="C:/Users/iyou/Documents/R/coursera/repdata_data_activity.zip"
outdir="C:/Users/iyou/Documents/R/coursera"
unzip(zipf, exdir=outdir)

activity=read.csv("C:/Users/iyou/Documents/R/coursera/repdata_data_activity/activity.csv", sep=",", header=TRUE,  stringsAsFactors = FALSE)

str(activity)

# 2. What is mean total number of steps per day

activity$date=as.Date(activity$date)

class(activity$date)


stepsperday=aggregate(steps~date, activity, sum, na.rm=TRUE)

hist(stepsperday$steps, breaks=20, col="green", 
     xlab="Steps Per Day", ylab="Number of Days", main="Total Steps Per Day")
     abline(v=median(stepsperday$steps), lty=1, lwd=2, col="red")
     legend(legend="Median", "topright", lty=1, lwd=2, col="red")

     
mean_steps=round(mean(stepsperday$steps, na.rm=T))
median_steps=median(stepsperday$steps, na.rm=T)    

# 3. What is the average daily activity pattern?
          
mean_steps_interval=aggregate(steps~interval, activity, mean, na.rm=T)

# line plot

# make line plot using ggplot2

library(ggplot2)

g <- ggplot(mean_steps_interval, aes(interval, steps))
g + geom_line() +
        geom_point() +
        xlab("Interval") +
        ylab("Mean steps") +
        ggtitle("Mean Steps Across All Day by Intervals")


# 4. Imputing missing values

# the total number of rows with NA

nrow(na.omit(activity))
nrowwithNA=nrow(activity[!complete.cases(activity),])

# which 5-minute interval, on average all the days in the dataset, contains max steps

maxsteps=max(mean_steps_interval$steps, na.rm=T)

interval_max=subset(mean_steps_interval, steps==maxsteps)

interval_max_steps=interval_max[1,1]

uncompleted=activity[!complete.cases(activity),]


# rename steps to mean_steps

library(plyr)

mean_steps_interval=rename(mean_steps_interval, c("steps"="mean_step"))

# merge activity and mean_steps_interval two datasets

activity=merge(activity, mean_steps_interval, by="interval")


# impute missing values of steps using interval mean steps

activity$imputed_steps=ifelse( is.na(activity$steps), activity$mean_step, activity$steps)


library(dplyr)

activity_new=activity %>%
        select(interval, imputed_steps, date)

activity_new1=rename(activity_new, c("imputed_steps"="steps"))


stepsperday1=aggregate(imputed_steps~date, activity_new, sum)

hist(stepsperday1$imputed_steps, breaks=20, col="green", 
     xlab="Steps Per Day", ylab="Number of Days", main="Total Steps Per Day after Missing Values Imputed")
abline(v=median(stepsperday$steps), lty=1, lwd=2, col="red")
legend(legend="Median", "topright", lty=1, lwd=2, col="red")

mean_steps=round(mean(stepsperday1$imputed_steps))
median_steps=round(median(stepsperday1$imputed_steps))

# 5. Are there differences in activity patterns between weekdays and weekends?

activity_new$week_days=weekdays(activity_new$date)

activity_new$weekday=ifelse(activity_new$week_days %in% c("Saturday", "Sunday"), "weekend", "weekday")


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
