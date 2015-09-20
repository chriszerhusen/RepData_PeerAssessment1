---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

This project analyzes a data set from a personal monitoring device, which records
the number of steps a person has taken in 5 minute intervals throughout the day.

##Loading and preprocessing the data

Read the data.  Use the date and interval columns to create a Time column that
recognizes the intervals as times of day.  Use this time information to glean the 
days of the week for each recording, which will be used in later analysis.


```r
# read in the data from file
dat <- read.csv("activity.csv")
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
# add columns for time, days of the week, and weekdays which will be used later
# The sprintf function is used to make all entries in the interval column four
# characters long, so that strptime() can be used
dat$interval <- sprintf("%04d", dat$interval)
```

```
## Error in sprintf("%04d", dat$interval): object 'dat' not found
```

```r
dat$time <- paste(dat$date, dat$interval)
```

```
## Error in paste(dat$date, dat$interval): object 'dat' not found
```

```r
dat$time <- strptime(dat$time, format = "%Y-%m-%d %H%M")
```

```
## Error in strptime(dat$time, format = "%Y-%m-%d %H%M"): object 'dat' not found
```

```r
dat$weekday <- weekdays(dat$time)
```

```
## Error in weekdays(dat$time): object 'dat' not found
```

```r
weekends <- c("Saturday", "Sunday")
dat$isweekday <- factor(dat$weekday %in% weekends, 
                                levels = c(FALSE, TRUE), 
                                labels = c("weekday", "weekend")
                                )
```

```
## Error in match(x, table, nomatch = 0L): object 'dat' not found
```

```r
# create a version of the table with NA values omitted        
cleandat <- na.omit(dat)
```

```
## Error in na.omit(dat): object 'dat' not found
```

## What is the mean total number of steps taken per day?

To find the mean number of steps taken per day, we will use tapply to sum the number
of steps for each day, and then take the mean of those sums.  It is important to use
the clean data (NAs removed) when taking the sums, or days with all NA entries will 
be summed to 0, rather than being left as NA, which will affect the mean and median.


```r
# create vector with the number of steps taken each day
dailysteps <- tapply(cleandat$steps, cleandat$date, sum)
```

```
## Error in tapply(cleandat$steps, cleandat$date, sum): object 'cleandat' not found
```

```r
# create histogram
hist(dailysteps, breaks = 15, col = "steelblue", xlab = "Steps per Day",
     main = "Steps per Day")
```

```
## Error in hist(dailysteps, breaks = 15, col = "steelblue", xlab = "Steps per Day", : object 'dailysteps' not found
```

We can find the mean number of steps taken per day.

```r
mean(dailysteps, na.rm = TRUE)
```

```
## Error in mean(dailysteps, na.rm = TRUE): object 'dailysteps' not found
```

And the median number of steps taken per day.

```r
median(dailysteps, na.rm = TRUE)
```

```
## Error in median(dailysteps, na.rm = TRUE): object 'dailysteps' not found
```

##What is the average daily activity pattern?

To find the average number of steps per time interval we do the following...


```r
# create vector with the average number of steps for each time interval
intervalaverage <- tapply(dat$steps, dat$interval, mean, na.rm = TRUE)
```

```
## Error in tapply(dat$steps, dat$interval, mean, na.rm = TRUE): object 'dat' not found
```

```r
# create time vector with every 5 minute interval throughout the day so that 
# our plot will know the intervals are times of day
times <- seq(ISOdatetime(2012,10,01,0,0,0), ISOdatetime(2012,10,1,23,55,0), by=(60*5))

#creat our plot, and find the interval that has the highest average value
plot(times, intervalaverage, type = "l", ylab = "Steps", xlab = "Time")
```

```
## Error in xy.coords(x, y, xlabel, ylabel, log): object 'intervalaverage' not found
```

```r
which.max(intervalaverage)
```

```
## Error in which.max(intervalaverage): object 'intervalaverage' not found
```

We can see that the interval with the highest average number of steps is the 8:35
interval (the 104th interval in the series).

## Imputing missing values

The number of NA values in our original dataset can be found with the following...


```r
sum(is.na(dat))
```

```
## Error in eval(expr, envir, enclos): object 'dat' not found
```

There are 2304 NA values in our original set.

Lets replace these NA values with an estimate of what their values may have been.  I 
will use the average values for each five minute interval that I calculated earlier
as my imputed values, replacing an NA data point with the average value for that 
time interval.


```r
# create new table to hold our imputed values
imputeddat <- dat
```

```
## Error in eval(expr, envir, enclos): object 'dat' not found
```

```r
# create vector with the values we will replace the NA values with
imputevector <- rep(intervalaverage, 61)
```

```
## Error in eval(expr, envir, enclos): object 'intervalaverage' not found
```

```r
# replace the NA values in our new table with the corresponding values from our vector
imputeddat$steps[is.na(imputeddat$steps)] <- imputevector[is.na(dat$steps)]
```

```
## Error in eval(expr, envir, enclos): object 'imputevector' not found
```

We can now re-examine the daily number of steps taken and see how our imputed values 
have affected the distribution.


```r
# This is the same process that we used before to find daily averages
impdailysteps <- tapply(imputeddat$steps, imputeddat$date, sum)
```

```
## Error in tapply(imputeddat$steps, imputeddat$date, sum): object 'imputeddat' not found
```

```r
hist(impdailysteps, breaks = 15, col = "forestgreen", xlab = "Steps per Day",
     main = "Imputed Steps per Day")
```

```
## Error in hist(impdailysteps, breaks = 15, col = "forestgreen", xlab = "Steps per Day", : object 'impdailysteps' not found
```

It looks as though the only real difference is that the highest frequency bin go even
taller, suggesting that the NA values were probably very clumped together on a couple
of days so that the entire day ended up being replaced with an average day.

Since we simply used the average values for each time interval, the mean of our data
will remain unchanged, as you can check.

```r
mean(impdailysteps, na.rm = TRUE)
```

```
## Error in mean(impdailysteps, na.rm = TRUE): object 'impdailysteps' not found
```

The median simply becomes the mean.

```r
median(impdailysteps, na.rm = TRUE)
```

```
## Error in median(impdailysteps, na.rm = TRUE): object 'impdailysteps' not found
```

## Are there differences in activity patterns between weekdays and weekends?

The factor variable was created when the data was initially read into R.


```r
# create vector with the average number of steps for each time interval on weekdays
weekdayintav <- tapply(imputeddat$steps[imputeddat$isweekday == "weekday"], 
                       imputeddat$interval[imputeddat$isweekday == "weekday"],
                       mean, 
                       na.rm = TRUE)
```

```
## Error in tapply(imputeddat$steps[imputeddat$isweekday == "weekday"], imputeddat$interval[imputeddat$isweekday == : object 'imputeddat' not found
```

```r
# create vector for weekends
weekendintav <- tapply(imputeddat$steps[imputeddat$isweekday == "weekend"], 
                       imputeddat$interval[imputeddat$isweekday == "weekend"],
                       mean, 
                       na.rm = TRUE)
```

```
## Error in tapply(imputeddat$steps[imputeddat$isweekday == "weekend"], imputeddat$interval[imputeddat$isweekday == : object 'imputeddat' not found
```

```r
# creat our plot, and find the interval that has the highest average value. The times
# vector used was created for the last time series plot
par(mfrow = (c(2,1)), mar = c(3,4,4,1))
plot(times, weekdayintav, type = "l", ylab = "Steps", xlab = "Time", 
     main = "Average Weekday Activity")
```

```
## Error in xy.coords(x, y, xlabel, ylabel, log): object 'weekdayintav' not found
```

```r
plot(times, weekendintav, type = "l", ylab = "Steps", xlab = "Time", 
     main = "Average Weekend Activity")
```

```
## Error in xy.coords(x, y, xlabel, ylabel, log): object 'weekendintav' not found
```

We can see that the weekday activity is much more concentrated in the morning, while
activity on the weekends was more spread out throughout the day.
