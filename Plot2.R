
########## create plot 2

Project1<-read.table("household_power_consumption.txt",
                     header=TRUE, sep=";", na.strings="NA", dec=".", strip.white=TRUE)

######step 1 convert column 1 to date format
day<-as.Date(project1$Date, "%d/%m/%Y")
project1$days<-weekdays(day)
class(day)

######step 2 convert column2 to time format
project1$Time<-strptime(project1$Time, "%X")
class(time)

######Step 3 Create the historam
plot1<-c(project1$Global_active_power , project1$Sub_metering_3)
hist(plot1, col="red", xlab="Global_active_power",  xlim=c(0,3100))

######Step 4 create plot 2
#####Step 4. Filter records with Thursday, Friday and Saturday
project1$days<-as.factor(project1$days)
project1$Global_active_power<-as.numeric(project1$Global_active_power)
project1$Sub_metering_1<-as.numeric(project1$Sub_metering_1)
project1$Sub_metering_2<-as.numeric(project1$Sub_metering_2)
project1$Sub_metering_3<-as.numeric(project1$Sub_metering_3)
project1$Global_reactive_power<-as.numeric(project1$Global_reactive_power)
project1$Voltage<-as.numeric(project1$Voltage)

project2<-project1[project1$days  %in% c('Thursday' , 'Friday' , 'Saturday'), ]

######Step 4. Create the time serie for GPA for Thursday, Friday and Saturday
project2.ts<-ts(project2$Global_active_power, start=1, end=3, freq=24)
cycle(project2.ts)
g_range<-range(0,project2.ts)
plot(project2.ts, axes=F , ann=F, ylim=g_range)
axis(1, at=1:3,labels=c('Thur','Fri','Sat'))
axis(2, las=1, at=seq(0,3795,by=759))  
title(main="Plot 2")
