# Loading and preprocessing data

Load the data

    activityData <- read.csv("activity.csv")
    summary(activityData)

    ##      steps            date              interval     
    ##  Min.   :  0.00   Length:17568       Min.   :   0.0  
    ##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
    ##  Median :  0.00   Mode  :character   Median :1177.5  
    ##  Mean   : 37.38                      Mean   :1177.5  
    ##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
    ##  Max.   :806.00                      Max.   :2355.0  
    ##  NA's   :2304

# Calculate the steps

1.  Calculate the total number of steps taken per day

<!-- -->

    stepsperday<-aggregate(steps~ date, activityData, sum, na.rm=TRUE)

1.  Make a histogram of the total number of steps taken each day

<!-- -->

    hist(stepsperday$steps)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)
3.Calculate and report the mean and median of the total number of steps
taken per day Mean

    meanStepsPerDay<-mean(stepsperday$steps)
    meanStepsPerDay

    ## [1] 10766.19

Median

    medianStepsPerDay<-median(stepsperday$steps)
    medianStepsPerDay

    ## [1] 10765

# What is the average daily activity pattern?

1.Make a time series plot (i.e. type = “l”) of the 5-minute interval
(x-axis) and the average number of steps taken, averaged across all days
(y-axis)

    stepsPerInterval<-aggregate(steps~interval, data=activityData, mean, na.rm=TRUE)
    plot(steps~interval, data=stepsPerInterval, type="l")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    intervalWithMaxNbSteps <- stepsPerInterval[which.max(stepsPerInterval$steps),]$interval
    intervalWithMaxNbSteps

    ## [1] 835

# Imputing missing values

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with NAs)

<!-- -->

    totalValuesMissings <- sum(is.na(activityData$steps))
    totalValuesMissings

    ## [1] 2304

1.  Devise a strategy for filling in all of the missing values in the
    dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc.

<!-- -->

    getMeanStepsPerInterval<-function(interval){
        stepsPerInterval[stepsPerInterval$interval==interval,]$steps
    }

3.Create a new dataset that is equal to the original dataset but with
the missing data filled in.

    activityDataNoNA<-activityData
    for(i in 1:nrow(activityDataNoNA)){
        if(is.na(activityDataNoNA[i,]$steps)){
            activityDataNoNA[i,]$steps <- getMeanStepsPerInterval(activityDataNoNA[i,]$interval)
        }
    }

1.  Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day

<!-- -->

    totalStepsPerDayNoNA <- aggregate(steps ~ date, data=activityDataNoNA, sum)
    hist(totalStepsPerDayNoNA$steps)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png) Do
these values differ from the estimates from the first part of the
assignment? What is the impact of imputing missing data on the estimates
of the total daily number of steps?

    meanStepsPerDayNoNA <- mean(totalStepsPerDayNoNA$steps)
    medianStepsPerDayNoNA <- median(totalStepsPerDayNoNA$steps)

# Are there differences in activity patterns between weekdays and weekends?

1.  Create a new factor variable in the dataset with two levels –
    “weekday” and “weekend” indicating whether a given date is a weekday
    or weekend day.

<!-- -->

    activityDataNoNA$date <- as.Date(strptime(activityDataNoNA$date, format="%Y-%m-%d"))
    activityDataNoNA$day <- weekdays(activityDataNoNA$date)
    for (i in 1:nrow(activityDataNoNA)) {
        if (activityDataNoNA[i,]$day %in% c("Saturday","Sunday")) {
            activityDataNoNA[i,]$day<-"weekend"
        }
        else{
            activityDataNoNA[i,]$day<-"weekday"
        }
    }
    stepsByDay <- aggregate(activityDataNoNA$steps ~ activityDataNoNA$interval + activityDataNoNA$day, activityDataNoNA, mean)

1.  Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = “𝚕”) of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).
    See the README file in the GitHub repository to see an example of
    what this plot should look like using simulated data.

<!-- -->

    names(stepsByDay) <- c("interval", "day", "steps")
    library(lattice)
    xyplot(steps ~ interval | day, stepsByDay, type = "l", layout = c(1, 2), 
        xlab = "Interval", ylab = "Number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-14-1.png)
