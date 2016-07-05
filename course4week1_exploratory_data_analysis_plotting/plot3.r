# setwd("D:/My Folders/R/2016/coursera/mod_4_week_1_programming/ExData_Plotting1")

# download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", "data/household_power_consumption.zip")

# Load data
data <- read.table("data/household_power_consumption.txt", sep=";", header = TRUE, na.strings = "?")

# process dates
data$datetime <- strptime(paste(data$Date, data$Time), format = "%d/%m/%Y %H:%M:%S", tz = "US/Pacific")

# subset data 2007-02-01 00:00:00 to 2007-02-02 23:59:59
s.dt <- strptime("2007-02-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific")
e.dt <- strptime("2007-02-03 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific")
index.dt <- which(data$datetime >= s.dt & data$datetime < e.dt)
data.sub <- data[index.dt,]

# change columns to numerics for caluculations
data.sub[,3:8] <- sapply(data.sub[,3:8], as.character)
data.sub[,3:8] <- sapply(data.sub[,3:8], as.numeric)

## Plot 3
png("plot3.png", width = 480, height = 480)
with(data.sub, plot(datetime, Sub_metering_1, 
                    type = "l",
                    xlab = "",
                    ylab = "Energy sub metering"))
with(data.sub, lines(datetime, Sub_metering_2,
                     col = "red"))
with(data.sub, lines(datetime, Sub_metering_3,
                     col = "blue"))
with(data.sub, legend("topright",
                      legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
                      lty=c(1),
                      col=c("black", "red", "blue")))
dev.off()
