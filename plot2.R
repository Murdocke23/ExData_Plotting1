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
  
  powerDates <- strptime(paste(data$Date, data$Time, sep=" "),"%d/%m/%Y %H:%M:%S")
  
  cbind(powerDates,data[,3:9])
  
}


# ######################################
# Output Power Over Time function: 
# produces PNG file of plot of column value over time
outputPowerPlotPNG <- function(filename, powerData, powerDataColumnName, yLabel) {
  
  png(filename = paste0(filename,".png"), 
      width = 480, 
      height = 480, 
      units = "px")
  
  plot(powerData$powerDates, 
       powerData[,powerDataColumnName],
       type="l", 
       lwd=1,
       lty=1,
       ylab=yLabel,
       xlab="")
  
  dev.off()
}


# ######################################
# As assignment requires, including specific calls to functions above
# completing the actual reading of data and output of PNG file in code
# code below assumes:
#     - source file is plain text file in parent directory of working directory
#       with name "household_power_consumption.txt"
#     -first and last row values remain the ones of interest for examination

filePath <- "..\\household_power_consumption.txt"
firstRow <- 66638
lastRow <- 69517

powerData <- getPowerData(filePath, firstRow, lastRow)

outputPowerPlotPNG("plot2", 
                        powerData,
                        powerDataColumnName= "Global_active_power", 
                        yLabel= "Global Active Power (kilowatts)")


