install.packages("pryr")
library(pryr)

readData <- function() {
        ## Read data corresonding to Dates "2007-02-01" and "2007-02-02"
        ## from file 'household_power_consumption.txt' and
        ## returns data frame with fields and headers:
        ##
        ##  DateTime: Date-timestamp in format YYYY/mm/dd hh:mm:ss
        ##  Date: Date in character format dd/mm/yyyy
        ##  Date2: Date converted to Date format YYYY/mm/dd
        ##  Time: time in character format hh:mm:ss
        ##  Global_active_power: household global minute-averaged active power (in kilowatt)
        ##  Global_reactive_power: household global minute-averaged reactive power (in kilowatt)
        ##  Voltage: minute-averaged voltage (in volt)
        ##  Global_intensity: household global minute-averaged current intensity (in ampere)
        ##  Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered).
        ##  Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
        ##  Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.
        ##################################################################
        
        ## get current working dir
        wdir <- getwd()
        
        ## Read 'household_power_consumption.txt' into data frame
        ## Read only rows with Dates "2007-02-01" and "2007-02-02"
        ## Start row:           (66638-1)       = 66637
        ## No of rows to read:  (69518-66638)   = 2880
        ## Missing values are coded as ?
        testfilepath <- paste(wdir, "household_power_consumption.txt", sep="/")
        ## Read header
        header <- read.table(file = testfilepath, 
                             nrows = 1, 
                             header = FALSE, 
                             sep =';', 
                             stringsAsFactors = FALSE)
        header <- make.names(header, unique = TRUE)
        consumption <- read.table(file = testfilepath,
                                  header = FALSE,
                                  sep = ";",
                                  na.strings = "?",
                                  stringsAsFactors = FALSE,
                                  skip = 66637,
                                  nrow = 2880)
        ## Set headers
        colnames(consumption) <- unlist(header) 
        
        ## Add new column for timeseries
        ## consumption$DateTime <- as.POSIXct(paste(consumption$Date, consumption$Time), format="%d/%m/%Y %H:%M:%S")
        consumption$DateTime <- strptime(paste(consumption$Date, consumption$Time),"%d/%m/%Y %H:%M:%S")
        consumption$Date2 <-as.Date(consumption$Date,format="%d/%m/%Y")
        ## Re-order the columns
        consumption <- consumption[ , c("DateTime", 
                                        "Date", 
                                        "Date2",
                                        "Time", 
                                        "Global_active_power", 
                                        "Global_reactive_power",
                                        "Voltage",
                                        "Global_intensity",
                                        "Sub_metering_1",
                                        "Sub_metering_2",
                                        "Sub_metering_3")]
        return (consumption)
        
}

##################################################################################

## 4th plot
plotGraph4 <- function() {
        ## Plots 4 graphs in a 2 x 2 matrix
        ## Graph 1: Global Active Power against date-time
        ## Graph 2: Voltage against date-time
        ## Graph 3: 3 sets of "Energy sub metering" readings against date-time
        ## Graph 4: Global Reactive Power against date-time
        ## and save it to PNG file
        ##################################################################
        
        df <- readData()
        png(file="plot4.png",
            width = 504, 
            height = 504, 
            units = "px",
            bg = "transparent")
        # 4 figures arranged in 2 rows and 2 columns
        par(mfrow=c(2,2))
        # plot 4.1
        with(df, plot(DateTime, 
                      Global_active_power, 
                      type = "l",
                      xlab = "",
                      ylab ="Global Active Power"))

        # plot 4.2
        with(df, plot(DateTime, 
                      Voltage, 
                      type = "l",
                      xlab = "datetime",
                      ylab ="Voltage"))
        
        # plot 4.3
        with(df, plot(DateTime, 
                               Sub_metering_1, 
                               type="n",
                               xlab = "",
                               ylab ="Energy sub metering"))
        with(df, lines(DateTime, 
                                Sub_metering_1, 
                                col = "black"))
        with(df, lines(DateTime, 
                                Sub_metering_2, 
                                col = "red"))
        with(df, lines(DateTime, 
                                Sub_metering_3, 
                                col = "blue"))
        # add legend to plot 4.3
        legend("topright", 
               legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
               lwd=1,
               col=c("black", "red", "blue"),
               bty="n")
        
        # plot 4.4
        with(df, plot(DateTime, 
                      Global_reactive_power, 
                      type = "l",
                      xlab = "datetime",
                      ylab ="Global_reactive_power"))
        
        dev.off()
        
}

