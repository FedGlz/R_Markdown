------------------------------------------------------------------------

Introduction
------------

##### It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

------------------------------------------------------------------------

Data
----

##### The variables included in this dataset are:

1.  steps: Number of steps taking in a 5-minute interval (missing values
    are coded as NA)

2.  date: The date on which the measurement was taken in YYYY-MM-DD
    format

3.  interval: Identifier for the 5-minute interval in which measurement
    was taken

##### The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

------------------------------------------------------------------------

Loading dependencies
--------------------

    library(ggplot2)
    library(dplyr)

------------------------------------------------------------------------

Loading and preprocessing the data
----------------------------------

    #remove all objects
    rm(list=ls())

    destfile <-  "Activity_Monitoring_Data.zip"

        if (!file.exists(destfile)){
            download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","Activity_Monitoring_Data.zip")
            unzip("Activity_Monitoring_Data.zip")
        }

    # Read data
    Activity <- read.csv("activity.csv")
    Activity$date <- as.Date(as.character(Activity$date))

------------------------------------------------------------------------

What is mean total number of steps taken per day?
-------------------------------------------------

    ##histogram Steps Taken per day
    g <- ggplot(subset(Activity,!is.na(steps)),aes(x=date , y=steps)) + geom_col(fill="blue") + labs(title="Steps Taken per Day")
    print(g)

![](PA1_template_files/figure-markdown_strict/Stepsmean-1.png)

    ##Grouping total steps by date
    Activity2 <- Activity[!is.na(Activity$steps),] %>% group_by(date) %>% summarise(steps=sum(steps))

    ##Calculate mean and median of total number of steps taken     by day
    dayMean <- prettyNum(mean(Activity2$steps, na.rm = T), big.mark= ",")
    dayMedian <- prettyNum(median(Activity2$steps, na.rm = T), big.mark = ",")

##### Steps mean by day is 10,766.19 and steps median is 10,765.

------------------------------------------------------------------------

What is the average daily activity pattern?
-------------------------------------------

    d <- Activity[!is.na(Activity$steps),] %>% group_by(interval) %>% summarise(Mean = mean(steps))
    with(d, plot(interval,Mean, type="l", main= "Avg Steps Taken by Interval" ))

![](PA1_template_files/figure-markdown_strict/Dailypattern-1.png)

    Maxmean <- d[which.max(d$Mean),]

##### Maximum steps in a 5-minute interval is 206.1698113 in the interval 835.

------------------------------------------------------------------------

Imputing missing values
-----------------------

    na_count <-as.data.frame(lapply(Activity, function(y) length(which(is.na(y)))))

##### Number of NA's in the dataset is 2304, all NA's found in the "steps" column.

     #Imput missing values as per interval mean
    for (i in 1:ncol(Activity)) {
        for (j in which(is.na(Activity[, i]))) {
            Activity[j, i] <- mean(Activity[Activity[, "interval"] == Activity[j, "interval"], i],  na.rm = TRUE)
        }
    }

    #Plotting data with new dataset
    g2 <- ggplot(Activity,aes(x=date , y=steps)) + 
        geom_col(fill=rgb(.2,0.5,.5)) + 
        labs(title="Steps Taken per Day NAs subs by Interval Mean")

    print(g2)

![](PA1_template_files/figure-markdown_strict/Imputing-1.png)

    #Calculate Mean and Median with new dataset with imputed values
    dayMean2 <- format(mean(Activity2$steps, na.rm = T), digits = 1)
    dayMedian2 <- format(median(Activity2$steps, na.rm = T),digits = 1)

    dif <- as.numeric(dayMean)-as.numeric(dayMean2)

    ## Warning: NAs introduced by coercion

    dif2 <- as.numeric(dayMedian) - as.numeric(dayMedian2)

    ## Warning: NAs introduced by coercion

##### Steps mean by day is 10766 and steps median is 10765, the difference vs the previous Mean and median calculation is NA and NA respectivly.

------------------------------------------------------------------------

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

    #Create a new factor indicating if its a weekday or weekend
    weekenddays <- c("Saturday","Sunday")
    Activity$Wdays <- factor(weekdays(Activity$date) %in% weekenddays , levels = c(FALSE,TRUE), labels = c("Weekdays","Weekend"))

    #Mean calculation by weekday/interval group
    Ac <- Activity %>% group_by(Wdays,interval) %>% summarise(steps = mean(steps))

    #Plotting steps taken by interval for weekdays and weekends
    g <- ggplot(Ac,aes(interval,steps, colour=Wdays)) + 
        geom_line() +facet_grid(Wdays~.) + 
        labs(title="Avg Steps Taken by Interval on Weekdays and Weekends")

    print(g)

![](PA1_template_files/figure-markdown_strict/Wdays-1.png)
