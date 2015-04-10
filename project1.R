setwd("~/My Dropbox/01 Chapingo/01 Taller de herramientas analiticas/01 Exploratory Data Analysis/01 Project 01")
remove(project2)
remove(project3)
remove(project3.Met1.ts)
remove(project3.Met2.ts)
remove(project3.Met3.ts)
Project1<-read.table("household_power_consumption.txt",
   header=TRUE, sep=";", na.strings="NA", dec=".", strip.white=TRUE)

labels(Project1) ### pegruntar a R el nombre de las etiquetas
summary(Project1)  ### hacer un resumen. 
class(Project1$Time)  # class or type of an object
names(Project1)  # names


#######create a dataset for preliminary calculations
project2<-Project1[1:60000 , 1:9]
View(project2)

######step 1 convert column 1 to date format
day<-as.Date(project2$Date, "%d/%m/%Y")
project2$days<-weekdays(day)
class(day)

######step 2 convert column2 to time format
project2$Time<-strptime(project2$Time, "%X")
class(time)

######Step 3 Create the historam
plot1<-c(project2$Global_active_power , project2$Sub_metering_3)
hist(plot1, col="red", xlab="Global_active_power",  xlim=c(0,3100))


#####Step 4. Filter records with Thursday, Friday and Saturday
project2$days<-as.factor(project2$days)
project2$Global_active_power<-as.numeric(project2$Global_active_power)
project2$Sub_metering_1<-as.numeric(project2$Sub_metering_1)
project2$Sub_metering_2<-as.numeric(project2$Sub_metering_2)
project2$Sub_metering_3<-as.numeric(project2$Sub_metering_3)
project2$Global_reactive_power<-as.numeric(project2$Global_reactive_power)
project2$Voltage<-as.numeric(project2$Voltage)

project3<-project2[project2$days  %in% c('Thursday' , 'Friday' , 'Saturday'), ]

######Step 4. Create the time serie for GPA for Thursday, Friday and Saturday
project3.ts<-ts(project3$Global_active_power, start=1, end=3, freq=24)
cycle(project3.ts)
g_range<-range(0,project3.ts)
plot(project3.ts, axes=F , ann=F, ylim=g_range)
axis(1, at=1:3,labels=c('Thur','Fri','Sat'))
axis(2, las=1, at=seq(0,3795,by=759))  
title(main="Plot 2")


#####step 5. create time series for Sub_metering_1  Sub_metering_2  Sub_metering_3
project3.Met1.ts<-ts(project3$Sub_metering_1, start=1, end=3, freq=24)
project3.Met2.ts<-ts(project3$Sub_metering_2, start=1, end=3, freq=24)
project3.Met3.ts<-ts(project3$Sub_metering_3, start=1, end=3, freq=24)


#####step 7. plot time series on the same chart
h_range<-range(0,project3.Met2.ts)
plot_colors <- c("red","black","blue")
plot(project3.Met2.ts, axes=F , ann=F, ylim=h_range)
axis(1, at=1:3,labels=c('Thur','Fri','Sat'))
axis(2, las=1, at=seq(0,33,by=5.5))
lines(project3.Met1.ts, type="o", pch=22, lty=2, col="red")
lines(project3.Met3.ts, type="o", pch=22, lty=2, col="blue")
for.legend<-project3[0:1,7:9]
legend(1, 33, names(for.legend), cex=0.8, col=plot_colors, pch=21:23, lty=1:3)
title(main="Plot 3")

#####step 8. generate plot 4.  wich consist on 4 charts
par(mfrow=c(2,2))


######first plot
project3.ts<-ts(project3$Global_active_power, start=1, end=3, freq=24)
cycle(project3.ts)
g_range<-range(0,project3.ts)
plot(project3.ts, axes=F , ann=F, ylim=g_range)
axis(1, at=1:3,labels=c('Thur','Fri','Sat'))
axis(2, las=0, at=seq(0,3795,by=759)) 

######second plot
project3.Vol.ts<-ts(project3$Voltage, start=1, end=3, freq=24)
i_range<-range(0,project3.Vol.ts)
plot(project3.Vol.ts, axes=F , ann=F, ylim=i_range)
axis(1, at=1:3,labels=c('Thur','Fri','Sat'))
axis(2, las=0, at=seq(0,1336,by=167))

######Third plot
h_range<-range(0,project3.Met2.ts)
plot_colors <- c("red","black","blue")
plot(project3.Met2.ts, axes=F , ann=F, ylim=h_range)
axis(1, at=1:3,labels=c('Thur','Fri','Sat'))
axis(2, las=0, at=seq(0,33,by=5.5))
lines(project3.Met1.ts, type="o", pch=10, lty=2, col="red")
lines(project3.Met3.ts, type="o", pch=10, lty=2, col="blue")
for.legend<-project3[0:1,7:9]
legend(1, 33, names(for.legend), cex=0.2, col=plot_colors, pch=21:23, lty=1:3)


########Fourth plot
project3.GRP.ts<-ts(project3$Global_reactive_power, start=1, end=3, freq=24)
j_range<-range(0,project3.Vol.ts)
plot(project3.GRP.ts, axes=F , ann=F, ylim=j_range)
axis(1, at=1:3,labels=c('Thur','Fri','Sat'))
axis(2, las=0, at=seq(0,244,by=61))



    
    