Code for reading in the dataset and/or processing the data

# Setting & testing work directory
setwd("C:/Users/Documents/Data_Science/Activity")

getwd()
"C:/Users/Documents/Data_Science/Activity"

# Loading library 
library(ggplot2)

# Unzipping & reading data
unzip("Activity.zip")
actdata <- read.csv("Activity.csv")

# Head data 
head(actdata)

steps       date interval
1    NA 2012-10-01        0
2    NA 2012-10-01        5
3    NA 2012-10-01       10
4    NA 2012-10-01       15
5    NA 2012-10-01       20
6    NA 2012-10-01       25

#Data Summary
summary(actdata)

steps                date          interval     
Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
Median :  0.00   2012-10-03:  288   Median :1177.5  
Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
NA's   :2304     (Other)   :15840

# Loading library
library(dplyr)

Histogram of the total number of steps taken each day

stepsbydate <- actdata %>% select(date, steps) %>% group_by(date) %>% summarize(tsteps= sum(steps)) %>%na.omit()
hist(databydate$tsteps, xlab = "Total Steps Taken Daily",main="Histogram of Total Steps Taken Each Day", breaks = 10)

Mean and median number of steps taken each day

#Calculation of mean and median of the total number of steps taken daily

mean(stepsbydate$tsteps)

[1] 10766.19

median(stepsbydate$tsteps)

[1] 10765

Time series plot of the average number of steps taken

stepsbyinterval <- actdata%>% select(interval, steps) %>% na.omit() %>% group_by(interval) %>% summarize(tsteps= mean(steps)) 
ggplot(stepsbyinterval, aes(x=interval, y=tsteps))+ geom_line()


The 5-minute interval that, on average, contains the maximum number of steps

# Make a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) type = "l" means the plot is line graph.

5-minute interval, on average across all the days in the dataset containing the maximum number of steps

head(stepsbyinterval)

# A tibble: 6 x 2
  interval    tsteps
<int>     <dbl>
1        0 1.7169811
2        5 0.3396226
3       10 0.1320755
4       15 0.1509434
5       20 0.0754717
6       25 2.0943396

stepsbyinterval[which(stepsbyinterval$tsteps == max(stepsbyinterval$tsteps)),]

# A tibble: 1 x 2
  interval   tsteps
<int>    <dbl>
1      835 206.1698

Code to describe and show a strategy for imputing missing data

#Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA????????s)

sapply(X = actdata, FUN = function(x) sum(is.na(x)))

   steps     date interval 
    2304        0        0 

Devise a strategy for filling in all of the missing values in the dataset. 
The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. 
I will use the mean for that 5 -minute interval to replace all the missing values in the dataset. At the end, I will check if all the NAs have been replaced.

replace_with_mean <- function(num) replace(num, is.na(num), mean(num, na.rm = TRUE))
meandata <- (actdata %>% group_by(interval) %>% mutate(steps = replace_with_mean(steps)))
head(meandata)

# A tibble: 6 x 3
# Groups:   interval [6]
steps       date interval
<dbl>     <fctr>    <int>
1 1.7169811 2012-10-01        0
2 0.3396226 2012-10-01        5
3 0.1320755 2012-10-01       10
4 0.1509434 2012-10-01       15
5 0.0754717 2012-10-01       20
6 2.0943396 2012-10-01       25

summary(meandata)

     steps                date          interval     
 Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
Median :  0.00   2012-10-03:  288   Median :1177.5  
Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
3rd Qu.: 27.00   2012-10-05:  288   3rd Qu.:1766.2  
Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
(Other)   :15840  

Histogram of the total number of steps taken each day after missing values are imputed

totalstepsbyday <- aggregate(meandata$steps, by=list(meandata$date), sum)

names(totalstepsbyday)[1] ="date"
names(totalstepsbyday)[2] ="totalsteps"
head(totalstepsbyday,15)

         date totalsteps
1  2012-10-01   10766.19
2  2012-10-02     126.00
3  2012-10-03   11352.00
4  2012-10-04   12116.00
5  2012-10-05   13294.00
6  2012-10-06   15420.00
7  2012-10-07   11015.00
8  2012-10-08   10766.19
9  2012-10-09   12811.00
10 2012-10-10    9900.00
11 2012-10-11   10304.00
12 2012-10-12   17382.00
13 2012-10-13   12426.00
14 2012-10-14   15098.00
15 2012-10-15   10139.00

summary(totalstepsbyday)

         date      totalsteps   
 2012-10-01: 1   Min.   :   41  
2012-10-02: 1   1st Qu.: 9819  
2012-10-03: 1   Median :10766  
2012-10-04: 1   Mean   :10766  
2012-10-05: 1   3rd Qu.:12811  
2012-10-06: 1   Max.   :21194  
(Other)   :55 

Histogram

hist(totalstepsbyday$totalsteps, xlab = "Number of Steps", ylab = "Frequency", main = "Histogram: Number of Daily Steps", breaks = 10)


Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

Comparison of previous and current mean and median

prev_mean <- mean(stepsbydate$tsteps, na.rm = TRUE)
cur_mean <- mean(totalstepsbyday$totalsteps)

# Comparison

prev_mean

[1] 10766.19

cur_mean

[1] 10766.19

prev_median <- median(stepsbydate$tsteps, na.rm = TRUE)
cur_median <- median(totalstepsbyday$totalsteps)


# Comparison

prev_median
[1] 10765

cur_median
[1] 10766.19

From the comparison, we can see that the highest count of the new version data is larger than the one we have with NAs. 
The means of each dataset are same. The medians of each dataset are slightly different.

Are there differences in activity patterns between weekdays and weekends?
Use the dataset with the filled-in missing values which is called new_steps dataset.

totalstepsbyday$WeekendOrWeekday <- ifelse(weekdays(as.Date(totalstepsbyday$date)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")
head(totalstepsbyday)

        date totalsteps WeekendOrWeekday
1 2012-10-01   10766.19          Weekday
2 2012-10-02     126.00          Weekday
3 2012-10-03   11352.00          Weekday
4 2012-10-04   12116.00          Weekday
5 2012-10-05   13294.00          Weekday
6 2012-10-06   15420.00          Weekend

Make a panel plot containing a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 



meandata$date <- as.Date(meandata$date)
meandata$weekday <- weekdays(meandata$date)
meandata$weekend <- ifelse(meandata$weekday=="Saturday" | meandata$weekday=="Sunday", "Weekend", "Weekday" )


meandataweekendweekday <- aggregate(meandata$steps , by= list(meandata$weekend, meandata$interval), na.omit(mean))
names(meandataweekendweekday) <- c("weekend", "interval", "steps")

ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
ggtitle("Comparison of Average Number of Steps in Each Interval")

meandataweekendweekday <- aggregate(meandata$steps , by= list(meandata$weekend, meandata$interval), na.omit(mean))
names(meandataweekendweekday) <- c("weekend", "interval", "steps")


ggplot(meandataweekendweekday, aes(x=interval, y=steps)) + geom_line()+
facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
ggtitle("Comparison of Average Number of Steps in Each Interval")







