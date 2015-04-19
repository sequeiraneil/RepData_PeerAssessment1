# Reproducible Research: Peer Assessment 1

This assignment analyses the data from a personal activity monitoring device. The device collects data at 5 min intervals throughout the day. The data, from an anonymous individual, consists of the number of steps taken in 5 min intervals each day for 2 months (Oct & Nov 2012)

## Loading and preprocessing the data
1. The data file is downloaded if it isn't available
2. It is uncompressed an read as a data frame.
3. The interval column is converted into a factor. 
4. The date column is coerced to a Date type
5. The data is examined via a summary & str method


```r
library(ggplot2) # we'll be using ggplot2 for graphing
```

```
## Warning: package 'ggplot2' was built under R version 3.0.3
```

```r
# read the data
a <- read.csv("activity.csv", header = T)
a$interval <- factor(a$interval)
a$date <- as.Date(a$date, format = "%Y-%m-%d")
```

Examine the data

```r
summary(a)
```

```
##      steps             date               interval    
##  Min.   :  0.00   Min.   :2012-10-01   0      :   61  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   5      :   61  
##  Median :  0.00   Median :2012-10-31   10     :   61  
##  Mean   : 37.38   Mean   :2012-10-31   15     :   61  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   20     :   61  
##  Max.   :806.00   Max.   :2012-11-30   25     :   61  
##  NA's   :2304                          (Other):17202
```

```r
str(a)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```


### What is mean total number of steps taken per day?
We ignoring missing values in the data for this purpose.

The total number of steps taken per day is accomplished by:

```r
library(dplyr) #we'll use the dplyr package 
```

```
## Warning: package 'dplyr' was built under R version 3.0.3
```

```r
totSteps <- a %>%
                group_by(date) %>%
                summarise(tot = sum(steps))
```



```r
mean_steps <- round(mean(totSteps$tot, na.rm = TRUE),0)
median_steps <- round(median(totSteps$tot, na.rm = TRUE),0)
mean_steps
```

```
## [1] 10766
```

```r
median_steps
```

```
## [1] 10765
```


Plotted as a histogram, using the ggplot package

```r
ggplot(totSteps, aes(x=tot)) + 
        geom_histogram(fill="steelblue", binwidth=1500) + 
        geom_point(aes(x=mean_steps, y=0, color="green"), size=4, shape=15) + 
        geom_point(aes(x=median_steps, y=0, color="yellow"), size=4, shape=15)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 




## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
