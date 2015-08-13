#Peer Assessment 1


##Loading and preprocessing the data


```r
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp,method="curl")
activity <- read.csv(unz(temp, "activity.csv"),header=TRUE)
unlink(temp)
```

##Load useful packages


```r
library(data.table)
```

```
## data.table 1.9.2  For help type: help("data.table")
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:data.table':
## 
##     between, last
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(knitr)
```

##Q1: What is the average number of steps taken per day?


```r
totalsteps<-group_by(activity,date)
dailysum<-summarise(totalsteps,dailysteps=sum(steps),dailymean=mean(steps),dailymedian=median(steps))
meansteps<-as.integer(mean(dailysum$dailysteps,na.rm=TRUE))
mediansteps<-median(dailysum$dailysteps,na.rm=TRUE)
```

The mean of the total number of steps per day is 10766.

The median of the total number of steps per day is 10765.

The histogram of the total number of steps taken each day:

![plot of chunk histogram](figure/histogram-1.png) 

##Q2: What is the average daily activity pattern?


```r
# plot2 find average daily activity pattern
# first make interval a factor and unfactor date
dailyint<-activity
dailyint$interval<-as.factor(dailyint$interval)
dailyint$date<-as.character(dailyint$date)
# group by interval
dailyint<-group_by(dailyint,interval)
intervalsum<-summarise(dailyint,intervalsteps=sum(steps,na.rm=TRUE),intervalmean=mean(steps,na.rm=TRUE))
# find the 5-minute interval that contains the maximum number of steps across all days
maxinterval<-subset(intervalsum,intervalsteps==max(intervalsteps),select=c(interval,intervalsteps))
maxnum<-maxinterval[1,1]
```

The time series showing the average steps by time interval:

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

###The interval with the maximum number of steps is 835.

##Q3: What is the impact of missing values in the data?

###3.1: How many missing values are in the data?


```r
# identify number of rows with missing values
countna<-is.na(activity$steps)
countmissing<-as.matrix(table(countna))
countmissing<-countmissing[2]
```

The number of missing values is 2304.

###3.2 Missing values will be replaced with the mean steps by interval


```r
# replace NA values with steps mean by interval mean
# first join tables to assign interval mean to every reading
newactivity<-merge(dailyint,intervalsum,by.x="interval",by.y="interval",all=TRUE)
completeactivity<-newactivity
completeactivity$steps<-ifelse(is.na(completeactivity$steps),completeactivity$intervalmean,completeactivity$steps)
totalsteps2<-group_by(completeactivity,date)
dailysum2<-summarise(totalsteps2,dailysteps=sum(steps),dailymean=mean(steps),dailymedian=median(steps))
```

###3.3 The histogram of the total steps taken each day, with missing data filled in:

![plot of chunk plot 3](figure/plot 3-1.png) 

###3.4 How are the results (mean and median) different when missing values are imputed?


```r
meansteps2<-as.integer(mean(dailysum2$dailysteps,na.rm=TRUE))
mediansteps2<-median(dailysum2$dailysteps,na.rm=TRUE)
```

Clearly there is virtually no difference when replacing missing values with imputed values.  

Old mean (missing values): 10766. New mean (imputed): 10766.
Old median (missing values): 10765. New median (imputed): 1.0766189 &times; 10<sup>4</sup>.

##Q4: Are there differences in activity patterns between weekdays and weekends?


```r
# create weekend and weekday factors
completeactivity$newdate<-as.Date(completeactivity$date)
completeactivity$day<-weekdays(completeactivity$newdate)
# group by interval
dailyint2<-completeactivity
dailyint2<-group_by(dailyint2,interval)
#create weekend/weekday factor variable
dailyint2$days<-as.factor(ifelse(dailyint2$day=="Saturday"|dailyint2$day=="Sunday","weekend","weekd"))
# summarize the steps by interval for each plot (weekend,weekday)
subset_we<-dailyint2$days=="weekend"
intervalsum_we<-summarise(dailyint2[subset_we,],mean_day_we=mean(steps))
subset_wd<-dailyint2$days=="weekd"
intervalsum_wd<-summarise(dailyint2[subset_wd,],mean_day_wd=mean(steps))
# merge files
intervalsum4<-merge(intervalsum_we,intervalsum_wd,by.x="interval",by.y="interval",all=TRUE)
```

The pattern of steps on weekends vs. weekdays shows significant differences:

![plot of chunk plot4](figure/plot4-1.png) 
