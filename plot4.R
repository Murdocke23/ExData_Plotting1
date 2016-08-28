## This file contains generic functions to read data of household power consumption
## from the UC Irvine Machine Learning Repository
## original source file found at:
## https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip



# ######################################
# Get Power data function: 
# reads power data between and including specified rows
# and combines date and time character data into POSIXlt Date/time value
# returns data set of those records
getPowerData <-function(filePath, firstRow, lastRow) {
  
  colNames <- c("Date","Time", 
                "Global_active_power", "Global_reactive_power","Voltage","Global_intensity",
                "Sub_metering_1","Sub_metering_2","Sub_metering_3")
  
  columnClasses = c(rep("character", 2), 
                    rep("numeric",7))
  
  data <- read.table(filePath, sep=";", 
                     na.strings="?", 
                     col.names=colNames, 
                     colClasses=columnClasses, 
                     skip=firstRow-1, 
                     nrows=lastRow-firstRow+1 )
  
  datetime <- strptime(paste(data$Date, data$Time, sep=" "),"%d/%m/%Y %H:%M:%S")
  
  cbind(datetime,data[,3:9])
  
}


# ######################################
# Output Submetering value Over Time function: 
# produces PNG file of plot of submetering values over time
outputSubMeteringPlot <- function(powerData, yLabel) {

  minMetering <- min(powerData[,6:8])
  maxMetering <- max(powerData[,6:8])
  
  plot(powerData$datetime, 
       rep(0, length(powerData$datetime)), #placeholder data for intial plot frame
       type="n", 
       ylab=yLabel,
       xlab="",
       ylim=c(minMetering, maxMetering))
  
  lines(powerData$datetime, powerData$Sub_metering_1, col="black")
  lines(powerData$datetime, powerData$Sub_metering_2, col="dark orange")
  lines(powerData$datetime, powerData$Sub_metering_3, col="blue")
  
  legend("topright", col=c("black","dark orange","blue"), bty="n", lty=1, legend= colnames(powerData[,6:8]))
  
}

# ######################################
# Output Power Over Time function: 
# produces PNG file of plot of column value over time
outputPowerPlot <- function(powerData, powerDataColumnName, yLabel,xLabel) {

  plot(powerData$datetime, 
       powerData$Global_active_power,
       type="l", 
       lwd=1,
       lty=1,
       ylab="Global Active Power",
       xlab="")

}


# ######################################
# As assignment requires,
# completing the actual reading of data and output of PNG file in code
# code below assumes:
#     - source file is plain text file in parent directory of working directory
#       with name "household_power_consumption.txt"
#     -first and last row values remain the ones of interest for examination

filePath <- "..\\household_power_consumption.txt"
firstRow <- 66638
lastRow <- 69517

powerData <- getPowerData(filePath, firstRow, lastRow)


png(filename = paste0("plot4.png"), 
    width = 480, 
    height = 480, 
    units = "px")

par(mfcol=c(2,2))

plot(powerData$datetime, 
     powerData$Global_active_power,
     type="l", 
     lwd=1,
     lty=1,
     ylab="Global Active Power",
     xlab="")


outputSubMeteringPlot(powerData,
                      yLabel= "Energy sub metering")

with(powerData, 
     plot(datetime, 
          Voltage,
          type="l", 
          lwd=1,
          lty=1)
)

with(powerData, 
     plot(datetime, 
          Global_reactive_power,
          type="l", 
          lwd=1,
          lty=1)
)

dev.off()
