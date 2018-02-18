library(ggplot2)
library(lattice)

rawData2 <- read.csv("activity.csv")

head(rawData)
summary(rawData)
str(rawData)
lapply(rawData, class)

rawData$date <- as.Date(rawData$date, format = "%Y-%m-%d")

#total steps per day: calculations and plots

aggregatedSteps <- aggregate(steps ~ date, rawData, sum)
hist(aggregatedSteps$steps, xlab = "total steps per day", 
     col = "red", xlim = c(0,25000), ylim = c(0,30), 
     main = "Histogram of total daily steps")

meanTotSteps <- mean(aggregatedSteps$steps)
medianTotSteps <- median(aggregatedSteps$steps)
descriptiveSteps <- c(meanTotSteps, medianTotSteps)
descriptiveSteps

#average steps per interval across days: calculations and plots

StepsByInterval <- aggregate(steps ~ interval, rawData, mean)

plot(StepsByInterval$interval, StepsByInterval$steps, type = "l", 
     xlab = "5-min interval", ylab = "average of steps across days", 
     main = "timecourse of the averaged number across days")

maxInterval <- StepsByInterval[which.max(StepsByInterval$steps),1]
maxInterval

#imputing nas
# number of missing values in the data
total_na <- nrow(rawData[is.na(rawData$steps),])

#create new dataset and replace missing values by mean of steps for this
# interval across days
data_noNA <- rawData
meanStepsInterval <- aggregate(steps ~ interval, data_noNA, mean)
for (i in 1:nrow(data_noNA)){
    if (is.na(data_noNA[i,1]) == TRUE){
        naInterval <- data_noNA[i,3]
        data_noNA[i,"steps_imputted"] <- meanStepsInterval[
            which(meanStepsInterval[,"interval"] %in% naInterval),2]
    } else {
        data_noNA[i,"steps_imputted"] <- data_noNA[i, "steps"]
    }
}

#Make a histogram of the total number of steps taken each day

totalSteps_noNA <- aggregate(steps_imputted ~ date, data_noNA, sum)
totalSteps_NA <- aggregate(steps ~ date, data_noNA, sum)

hist(totalSteps_noNA$steps, xlab = "total steps per day", 
     col = "red",ylim = c(0,40),
     main = "Histogram of total daily steps",
     breaks = 5)
hist(totalSteps_NA$steps, col = rgb(0,1,1,0.55), add = TRUE, 
     breaks = 5)
legend("topright", col = c("red", "cyan"), lwd = 5,
       legend = c("steps (imputted)","steps (with NA)"))

#Calculate and report the mean and median total number of steps taken per day.
meanTotSteps_noNA <- mean(totalSteps_noNA$steps)
medianTotSteps_noNA <- median(totalSteps_noNA$steps)
descriptiveSteps <- c(meanTotSteps_noNA, medianTotSteps_noNA)
descriptiveSteps

diff_meanSteps <- meanTotSteps_noNA - meanTotSteps
diff_medianSteps <- medianTotSteps_noNA - medianTotSteps
diff_meanSteps
diff_medianSteps

#define weekdays and weekends
#
for (i in 1:nrow(data_noNA)){
    data_noNA[i,"day"] <- weekdays(data_noNA[i,"date"])
}

data_noNA$weekday <- ifelse((data_noNA$day == "Saturday" | 
                            data_noNA$day == "Sunday"), "weekend", "weekday")

stepsByDaytype <- aggregate(steps_imputted ~ interval + weekday, data = data_noNA, 
                           FUN = mean)

g <- ggplot(data = stepsByDaytype, aes(interval,steps_imputted))
     g+ geom_line(col="blue") + facet_grid(weekday~.)
     
xyplot(x= steps_imputted ~ interval | weekday, data = stepsByDaytype, 
       type = "l", layout = c(1,2), ylab = "steps across days (mean)",
       main = "average of steps across day by interval")     
