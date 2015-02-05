# Reproducible Research Peer Assignment 1
Damien Edwards  
February 2, 2015  

This is an assignment that uses data from a personal activity monitoring device. The device collects data at 5 minute intervals through the day. The data consists of two months of data from an anonymous individual collected during the months of October and November,2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:

1.steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

2.date: The date on which the measurement was taken in YYYY-MM-DD format

3.interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

### Loading and preprocessing the data

The data is downloaded from the Course Website:

https://class.coursera.org/repdata-011/human_grading/view/courses/973512/assessments/3/submissions

The data is in zip file:

repdata-data-activity.zip


Below the data is read and the column names are shown.

```r
activity <- read.csv("activity.csv")
head(activity,3)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
```

```r
summary(activity)
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
Data for the date will be transformed later in the report.

###What is mean total number of steps taken per day?

Total Number of steps taken per day.

```r
ag_activity<-aggregate(steps ~ date, activity, sum)$steps
hist(ag_activity, breaks = 6, col = "blue", main = "Frequency of Total Steps per day", xlab = "Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

The mean:

```r
mean_steps<-mean(ag_activity,na.rm = T)
mean_steps
```

```
## [1] 10766.19
```

The median:

```r
median_steps<-median(ag_activity,na.rm = T)
median_steps
```

```
## [1] 10765
```

###What is the average daily activity pattern?
Time series Plot:


```r
ag2_activity<-aggregate(steps ~ interval, activity, mean)
plot(ag2_activity$steps, type="l", main="Step Interval daily average", ylab="Number of Steps",xlab="Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
The 5 month interval with the largest number of steps is :


```r
interval_max<- which.max(ag2_activity$steps)
interval_max
```

```
## [1] 104
```
###Inputing missing values
