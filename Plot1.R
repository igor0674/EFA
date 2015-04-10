
########## create plot 1

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

