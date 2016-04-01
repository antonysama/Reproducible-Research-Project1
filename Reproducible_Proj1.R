#Loading and preprocessing the data
df<-read.csv("activity.csv")
df$date<-ymd(df$date)

# mean total number of steps taken per day?
library("dplyr") 
i<-df[,1:2]
 e<-i%>% 
  group_by(date)%>% 
  summarise_each(funs(sum, mean)) 
hist(e$mean)
dev.off()
mean(e$sum, na.rm = T) #10766.19
median(e$sum, na.rm = T) #10765

#average daily activity pattern?
library("mice")
md.pattern(df)#15264 observations are complete, Steps have NAs 2304
tempdf<-mice(df[,c(1,3)], seed=100)
tempdf$imp$steps
completedf<-complete(tempdf,1)
dim(completedf)
dim(df)
df$steps<-completedf$steps
library("dplyr")
i<-df[,1:2]
e<-i%>% 
  group_by(date)%>% 
  summarise_each(funs(sum, mean)) 
hist(e$mean)
mean(e$sum, na.rm = T) #10677.85
median(e$sum, na.rm = T) #10600
library("ggplot2")
ggplot(data=f, aes(x=interval, y=steps, group=date)) +
  geom_point(aes(color=date)) + 
  geom_line(aes(color=date))
dev.off()
which.max(f$steps)
f$interval[4633]#615 interval

#differences in activity patterns between weekdays and weekends?
df$date<-weekdays(df$date, abbreviate = F)

library("dplyr")
g<-df%>% 
  group_by(interval,date)%>% 
  summarise_each(funs(mean)) 
g$logical<-ifelse(g$date=="Saturday" | g$date=="Sunday", "wknd", "wkdy")
library("lattice")
dev.off() 
par(mfrow=c(2,1))
xyplot(steps ~ interval | logical, type="l", data = g, layout=c(1,2))
dev.off()              

install.packages("knitr")
library("knitr")
knit2html("PA1_template.Rmd", output="PA1_template.html")
list.files()


#calc mean number of steps by time interval
avgSteps<-aggregate(steps ~ interval, activity, mean)

#plot results
plot(avgSteps$interval, avgSteps$steps, type='l', col=1, main="Mean Steps by Interval", xlab="5-Minute Interval Number", ylab="Mean Number of Steps")

#figure out which interval has the most number of steps:
maxVal<-subset(avgSteps, avgSteps$steps==max(avgSteps$steps))