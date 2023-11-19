library(lubridate)

daily_activity <- read.csv("dailyActivity_merged.csv")
daily_activity$Id <- as.factor(daily_activity$Id)
daily_activity$ActivityDate <- mdy(daily_activity$ActivityDate)
summary(daily_activity)

#Logged activities dstance does not seem to be well documented/useful for the product
#Mean steps = 8533
#tracker distance = total distance

plot(TotalSteps~Calorie_Expenditure,data=daily_activity)

sleep_day <- read.csv("sleepDay_merged.csv")
sleep_day$Id <- as.factor(sleep_day$Id)
sleep_day$SleepDay <- as.character(sleep_day$SleepDay)
library(tidyr) #will use the seperate function from this library to modify the bed time variable
sleep_day <- separate(sleep_day, SleepDay, into=c('ActivityDate','sleep.time'),sep=" ",remove=T)
sleep_day$ActivityDate <- mdy(sleep_day$ActivityDate)
sleep_day$sleep_efficiency <- sleep_day$TotalMinutesAsleep/sleep_day$TotalTimeInBed
sleep_day <- sleep_day[,-c(3)]
summary(sleep_day)

weightlog <- read.csv("weightLogInfo_merged.csv")
weightlog$Id <- as.factor(weightlog$Id)
weightlog$Date <- as.character(weightlog$Date)
weightlog <- separate(weightlog, Date, into=c('ActivityDate','sleep.time'),sep=" ",remove=T)
weightlog$ActivityDate <- mdy(weightlog$ActivityDate)
weightlog <- weightlog[,c('Id','ActivityDate','WeightKg','WeightPounds','Fat','BMI')]
summary(weightlog)

dailyCalories <- read.csv("dailyCalories_merged.csv")
dailyCalories$Id <- as.factor(dailyCalories$Id)
dailyCalories$ActivityDay <- mdy(dailyCalories$ActivityDay)
colnames(dailyCalories)[which(names(dailyCalories)=="ActivityDay")] <- "ActivityDate"
summary(dailyCalories)

m0 <- merge(daily_activity, sleep_day, by=c("Id","ActivityDate"),all=T)
summary(m0)
m1 <- merge(m0, weightlog, by=c("Id","ActivityDate"),all=T)
summary(m1)
activity_merged <- merge(m1,dailyCalories,by=c("Id","ActivityDate","Calories"),all=T)

summary(activity_merged)

write.csv(activity_merged,"activity_merged.csv")

----------------------------------------------------------------------

effac1 <- aggregate(sleep_efficiency~VeryActiveMinutes, data= activity_merged, FUN=mean)
boxplot(sleep_efficiency~VeryActiveMinutes, data=effac1)
barplot(avgcalhr$Calories,names.arg=avgcalhr$ActivityHour)

---------------------------------------------------------------------
ordered_activity <- activity_merged[order(activity_merged$ActivityDate),]
summary(ordered_activity)
head(ordered_activity)

plot(Calorie_Expenditure~sleep_efficiency, data=ordered_activity)
plot(Calorie_Intake~sleep_efficiency, data=ordered_activity) #these are the same Calories, need to find out whether this is calories burned or intook
plot(LightlyActiveMinutes~sleep_efficiency, data=ordered_activity)

avgcal <- aggregate(WeightKg~ActivityDate, data=ordered_activity, FUN=mean)
barplot(avgcal$WeightKg,names.arg=avgcal$ActivityDate)

--------------------------------------------------------------------
hourly_calories <- read.csv("hourlyCalories.csv")
hourly_calories$Id <- as.factor(hourly_calories$Id)
hourly_calories$ActivityDate <- mdy(hourly_calories$ActivityDate)
hourly_calories <- separate(hourly_calories, Activity.Time, into=c('ActivityHour','na'),sep=":",remove=T)
hourly_calories <- hourly_calories[,-c(4)]
hourly_calories$ActivityHour <- as.factor(hourly_calories$ActivityHour)

summary(hourly_calories)
head(hourly_calories)


hourly_intensities <- read.csv("hourlyIntensities.csv")
hourly_intensities$Id <- as.factor(hourly_intensities$Id)
hourly_intensities$ActivityDate <- mdy(hourly_intensities$ActivityDate)
hourly_intensities <- separate(hourly_intensities, ActivityTime, into=c('ActivityHour','na'),sep=":",remove=T)
hourly_intensities <- hourly_intensities[,-c(4)]
hourly_intensities$ActivityHour <- as.factor(hourly_intensities$ActivityHour)

summary(hourly_intensities)


hourly_steps <- read.csv("hourlySteps.csv")
hourly_steps$Id <- as.factor(hourly_steps$Id)
hourly_steps$ActivityDate <- mdy(hourly_steps$ActivityDate)
hourly_steps <- separate(hourly_steps, ActivityTime, into=c('ActivityHour','na'),sep=":",remove=T)
hourly_steps <- hourly_steps[,-c(4)]
hourly_steps$ActivityHour <- as.factor(hourly_steps$ActivityHour)

summary(hourly_steps)

hourly_merge0 <- merge(hourly_calories, hourly_intensities, by=c("Id","ActivityDate","ActivityHour"),all=T)
hourly_merge <- merge(hourly_merge0, hourly_steps, by=c("Id","ActivityDate","ActivityHour"),all=T)
hourly_merge$ActivityHour <- as.character(hourly_merge$ActivityHour)
hourly_merge$ActivityHour <- factor(hourly_merge$ActivityHour,levels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))

summary(hourly_merge)


avgcalhr <- aggregate(Calories~ActivityHour, data=hourly_merge, FUN=mean)
barplot(avgcalhr$Calories,names.arg=avgcalhr$ActivityHour)

avgstepshr <- aggregate(StepTotal~ActivityHour, data=hourly_merge, FUN=mean)
barplot(avgstepshr$StepTotal,names.arg= avgstepshr$ActivityHour)

write.csv(hourly_merge,"hourly_merge.csv")

hourly_merge$caloric_intensity <- hourly_merge$Calories/hourly_merge$AverageIntensity



plot(VeryActiveMinutes~Calories,data=activity_merged)
plot(FairlyActiveMinutes~Calories,data=activity_merged)
plot(SedentaryMinutes~Calories,data=activity_merged)

-------------------------------------------------------------------------------

