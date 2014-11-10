# Reproducible Research: Peer Assessment 1


### Loading and preprocessing the data



```r
library(ggplot2)
library(plyr)

unzipData<-function()
    {
    filename<-"activity.zip"
    unzip(filename)
    rawData<<-read.csv("activity.csv")
    }

unzipData()

head(rawData)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
summary(rawData)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```




### What is mean total number of steps taken per day?


```r
rawStepsPerDay<-aggregate(steps ~ date, data=rawData,sum)

plotHistogram<-function(stepsPerDay,title)
{   
    p<-ggplot(stepsPerDay,aes(x=steps))
    p<-p+geom_histogram(aes(x=steps),binwidth=3000,colour="black",fill="white")
    p<-p+xlab("Total Daily Steps")
    p<-p+ggtitle(paste("Total Daily Steps Frequency - ",title))
    p<-p+geom_vline(aes(xintercept=mean(steps)),linetype="dotted",color="blue")
    p<-p+geom_vline(aes(xintercept=median(steps)),linetype="dotted",color="red")
    c<-5000*ceiling(max(stepsPerDay$steps,na.rm=TRUE)/5000)
    p<-p+scale_x_continuous(breaks=seq(0,c,5000))
    p<-p+geom_text(aes(x=mean(steps),y=2),color="blue", label="mean",vjust=-0.5,angle=90, size=3)
    p<-p+geom_text(aes(x=median(steps),y=2),color="red", label="median",vjust=-0.5,angle=-90, size=3)
    
    p
}

plotHistogram(rawStepsPerDay,"raw data")
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

Based on the histogram the mean and median should be close, let's see...


```r
mean(rawStepsPerDay$steps)
```

```
## [1] 10766.19
```

```r
median(rawStepsPerDay$steps)
```

```
## [1] 10765
```



### What is the average daily activity pattern?

```r
avgStepsPerTimeInterval<-aggregate(steps~interval,data=rawData,mean)
maxInterval<-function() { avgStepsPerTimeInterval$interval[which.max(avgStepsPerTimeInterval$steps)] }

plotTimeSeries<-function()
{
    p<-ggplot(avgStepsPerTimeInterval,aes(interval,steps))
    p<-p+geom_line()
    p<-p+xlab("Time Interval") + ylab("Average Across All Days")
    p<-p+ggtitle("Average Number of Steps Taken by Time Interval")
    p<-p+geom_vline(aes(xintercept=maxInterval()),linetype="dotted")
    p<-p+scale_x_continuous(breaks=c(seq(0,2400,600),maxInterval()),labels={function(z) sprintf("%04d",z)})
    p
}

plotTimeSeries()
```

![](./PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
maxInterval()
```

```
## [1] 835
```



### Imputing missing values

Missing data is comprised of:

1. data which is reported as NA
2. missing intervals within a date
this data

NA type data will first be imputed by the median of values from the dates which do have data.
Missing intervals will be imputed by dat from the next availalable interval within the date.

Missing NA data:

```r
naStepCount<-sum(is.na(rawData$steps))
naStepCount
```

```
## [1] 2304
```

Missing interval data:

```r
allIntervals<-seq(0,2355,5)
actualIntervals<-unique(rawData$interval)
missingIntervals<-allIntervals[!allIntervals %in% actualIntervals]

length(missingIntervals)
```

```
## [1] 184
```

The following will impute the missing data for the above two types:

```r
impute.median<-function(x) replace(x, is.na(x), median(x, na.rm = TRUE))

imputeMissing <-function()
{
    allIntervals<-seq(0,2355,5)
    actualIntervals<-unique(rawData$interval)
    missingIntervals<-allIntervals[!allIntervals %in% actualIntervals]
    length(missing); head(missing); tail(missing)
 
    
    adjRawData<-ddply(rawData, ~interval, transform, steps=impute.median(steps))
    
    dates<-unique(rawData$date)
    adjData<-data.frame(rep(dates,each=length(allIntervals)),rep(allIntervals,length(dates)))
    colnames(adjData)<-c("date","interval")
    adjData<-merge(adjRawData,adjData,all=TRUE)
    
    imputedSteps<-adjData$steps
    last=tail(imputedSteps,1)
    
    for (i in (length(imputedSteps)):1)
    {
        if (is.na(imputedSteps[i]))
            imputedSteps[i]<-last
        last<-imputedSteps[i]
    }
    
    
    adjData$steps<-imputedSteps
    adjData
}

adjData<-imputeMissing()
adjStepsPerDay<-aggregate(steps ~ date, data=adjData,sum)

plotHistogram(adjStepsPerDay,"imputed data")
```

![](./PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
mean(adjStepsPerDay$steps)
```

```
## [1] 15409.84
```

```r
median(adjStepsPerDay$steps)
```

```
## [1] 16355
```

As can be seen from the plots, imputing values for the missing data has a significant effect on the resulting distribution.  The mean and medians are significantly higher.



### Are there differences in activity patterns between weekdays and weekends?

```r
addWeekend <- function(data)
{
    w<-weekdays(as.Date(data$date))
    data$weekend<-"weekday"
    data$weekend[w %in% c("Saturday","SUnday")]<-"weekend"
    data$weekend<-as.factor(data$weekend)
    data
}

plotWeekendTimeSeries<-function(inData)
{
    avgStepsPerTimeInterval<-ddply(inData,.(interval,weekend),summarize,steps=mean(steps))
    p<-ggplot(avgStepsPerTimeInterval,aes(interval,steps))
    p<-p+geom_line()
    p<-p+xlab("Time Interval") + ylab("Average Across All Days")
    p<-p+ggtitle("Average Number of Steps Taken by Time Interval")
    p<-p+scale_x_continuous(breaks=c(seq(0,2400,600)),labels={function(z) sprintf("%04d",z)})
    p<-p+facet_grid(weekend~.)
    p
}

adjData<-addWeekend(adjData)
plotWeekendTimeSeries(adjData)
```

![](./PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

