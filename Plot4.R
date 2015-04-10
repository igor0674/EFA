########## create plot 3

Project1<-read.table("household_power_consumption.txt",
                     header=TRUE, sep=";", na.strings="NA", dec=".", strip.white=TRUE)

######step 1 convert column 1 to date format
day<-as.Date(project1$Date, "%d/%m/%Y")
project1$days<-weekdays(day)
class(day)

######step 2 convert column2 to time format
project1$Time<-strptime(project1$Time, "%X")
class(time)

######Step 3. Filter records with Thursday, Friday and Saturday
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


#####step 5. create time series for Sub_metering_1  Sub_metering_2  Sub_metering_3
project2.Met1.ts<-ts(project2$Sub_metering_1, start=1, end=3, freq=24)
project2.Met2.ts<-ts(project2$Sub_metering_2, start=1, end=3, freq=24)
project2.Met3.ts<-ts(project2$Sub_metering_3, start=1, end=3, freq=24)

#####step 6. Make plot 3
h_range<-range(0,project2.Met2.ts)
plot_colors <- c("red","black","blue")
plot(project2.Met2.ts, axes=F , ann=F, ylim=h_range)
axis(1, at=1:3,labels=c('Thur','Fri','Sat'))
axis(2, las=1, at=seq(0,33,by=5.5))
lines(project2.Met1.ts, type="o", pch=22, lty=2, col="red")
lines(project2.Met3.ts, type="o", pch=22, lty=2, col="blue")
for.legend<-project2[0:1,7:9]
legend(1, 33, names(for.legend), cex=0.8, col=plot_colors, pch=21:23, lty=1:3)

#####step 8. generate plot 4.  wich consist on 4 charts
par(mfrow=c(2,2))

######first plot
project2.ts<-ts(project2$Global_active_power, start=1, end=3, freq=24)
cycle(project2.ts)
g_range<-range(0,project2.ts)
plot(project2.ts, axes=F , ann=F, ylim=g_range)
axis(1, at=1:3,labels=c('Thur','Fri','Sat'))
axis(2, las=0, at=seq(0,3795,by=759)) 

######second plot
project2.Vol.ts<-ts(project2$Voltage, start=1, end=3, freq=24)
i_range<-range(0,project2.Vol.ts)
plot(project2.Vol.ts, axes=F , ann=F, ylim=i_range)
axis(1, at=1:3,labels=c('Thur','Fri','Sat'))
axis(2, las=0, at=seq(0,1336,by=167))

######Third plot
h_range<-range(0,project2.Met2.ts)
plot_colors <- c("red","black","blue")
plot(project2.Met2.ts, axes=F , ann=F, ylim=h_range)
axis(1, at=1:3,labels=c('Thur','Fri','Sat'))
axis(2, las=0, at=seq(0,33,by=5.5))
lines(project2.Met1.ts, type="o", pch=10, lty=2, col="red")
lines(project2.Met3.ts, type="o", pch=10, lty=2, col="blue")
for.legend<-project2[0:1,7:9]
legend(1, 33, names(for.legend), cex=0.2, col=plot_colors, pch=21:23, lty=1:3)


########Fourth plot
project2.GRP.ts<-ts(project2$Global_reactive_power, start=1, end=3, freq=24)
j_range<-range(0,project2.Vol.ts)
plot(project2.GRP.ts, axes=F , ann=F, ylim=j_range)
axis(1, at=1:3,labels=c('Thur','Fri','Sat'))
axis(2, las=0, at=seq(0,244,by=61))
title(main="Plot 3")