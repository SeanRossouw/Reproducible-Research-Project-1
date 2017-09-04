``` r
library(lattice)
```

    ## Warning: package 'lattice' was built under R version 3.4.1

``` r
library(plyr)

knitr::opts_chunk$set(echo = TRUE)
```

Data
----

The data for this assignment can be downloaded from the course web site.

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement data was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value file and there are a total of 17,568 observations.

Loading and Pre-processing of data
----------------------------------

``` r
activityRaw <- read.csv("activity.csv")
activityRaw$day <- weekdays(as.Date(activityRaw$date))
activityRaw$date<- as.POSIXct(activityRaw$date, format="%Y-%m-%d")
activity <- activityRaw[!is.na(activityRaw$steps),]
```

What is the mean total number of steps taken per day?

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

``` r
sumSteps <- aggregate(activity$steps ~ activity$date, FUN=sum)
colnames(sumSteps)<- c("Date", "Steps")

hist(sumSteps$Steps, breaks=10, xlab="Steps", main = "Total Steps per day")
```

![Histogram 1](PA1_template_files/figure-markdown_github/project%201b-1.png)

Mean and Median

``` r
as.integer(mean(sumSteps$Steps))
```

    ## [1] 10766

``` r
as.integer(median(sumSteps$Steps))
```

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

Plot the mean number of steps across all days per interval

``` r
meanByInterval <- aggregate(activity$steps, by=list(activity$interval), FUN=mean)
names(meanByInterval) <- c("interval", "mean")

plot(meanByInterval$interval, meanByInterval$mean, type='l', col="red", lwd=3, xlab="Interval (minutes)", ylab="Average steps", main="Average steps per interval")
```

![Activity Pattern 1](PA1_template_files/figure-markdown_github/project%201d-1.png)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
-------------------------------------------------------------------------------------------------------------

``` r
# We lookup the value of interval at this position
max_interval <- meanByInterval[which(meanByInterval$mean == max(meanByInterval$mean)), 1]
max_interval
```

    ## [1] 835

Inputing missing values
-----------------------

How many rows have steps as 'NA'

``` r
nrow(activityRaw[is.na(activityRaw$steps),])
```

    ## [1] 2304

To fill the missing values, the average value for that interval will be used To find this, we take the mean of the clean data steps for each interval and merge with the rows missing steps by interval. This is then appended onto the cleaned data with rbind

``` r
nadata<- activityRaw[is.na(activityRaw$steps),]
nadata<-nadata[-1]
colnames(nadata)<-c("date","interval","day")
meanByInterval<-aggregate(activity[,1], list(activity$interval), mean)
colnames(meanByInterval)<-c("interval","steps")
replacement<-merge(nadata,meanByInterval, by='interval', all=TRUE)

activityFilled<-rbind(activity, replacement)
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

``` r
sumSteps2 <- aggregate(activityFilled$steps ~ activityFilled$date, FUN=sum)
colnames(sumSteps2)<- c("Date", "Steps")

hist(sumSteps2$Steps, breaks=10, xlab="Steps", main = "Total Steps per day")
```

![Histogram 2](PA1_template_files/figure-markdown_github/project%201h-1.png)

Mean and Median

``` r
as.integer(mean(sumSteps2$Steps))
```

    ## [1] 10766

``` r
as.integer(median(sumSteps2$Steps))
```

    ## [1] 10766

What difference did imputing data have on the outcome
-----------------------------------------------------

``` r
hist(sumSteps2$Steps, breaks=10, xlab="Steps", main = "Effect of imputing data by mean interval filling", col="Red")
hist(sumSteps$Steps, breaks=10, col="Blue", add=T)
legend("topright", c("Filled data", "Cleaned data"), fill=c("red", "blue") )
```

![Histogram 3](PA1_template_files/figure-markdown_github/project%201j-1.png)

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

Add a column, the value of which is set depending on whether the 'day' variable falls on a weekend or not.Average the interval steps by day type and plot a latticeplot separated by day type

``` r
activity$dayType <- ifelse(activity$day %in% c('Saturday', 'Sunday'), "weekend", "weekday")
activityMean <- aggregate(activity$steps, by=list(activity$dayType, activity$interval), mean)
colnames(activityMean)<-c("dayType","interval","steps")

xyplot(steps ~ interval | dayType, activityMean, type="l", lwd=2, xlab="Interval", ylab="Number of steps", layout=c(1,2))
```

![Lattice plot 1](PA1_template_files/figure-markdown_github/project%201k-1.png)

It can be seen that activity trends do differ based on weekday or weekend
