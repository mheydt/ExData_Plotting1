create_plot1 <- function(data, tofile = TRUE)
{
  if (tofile) print("Generating plot to file")
  else print("Generating plot to screen device")
  init_plot_defaults()
  if (tofile) png("plot1.png", width=480, height=480)
  global_active_power_hist(data)
  if (tofile) dev.off()
}

create_plot2 <- function(data, tofile = TRUE)
{
  if (tofile) print("Generating plot to file")
  else print("Generating plot to screen device")
  init_plot_defaults()
  png("plot2.png", width=480, height=480)
  global_active_power_plot(data)
  dev.off()
}

create_plot3 <- function(data, tofile = TRUE)
{  
   if (tofile) print("Generating plot to file")
   else print("Generating plot to screen device")
   init_plot_defaults()
   png("plot3.png", width=480, height=480)
   energy_sub_metering_plot(data)
   dev.off()
}  
  
create_plot4 <- function(data, tofile = TRUE)
{  
  if (tofile) print("Generating plot to file")
  else print("Generating plot to screen device")
  init_plot_defaults()
  png("plot4.png", width=480, height=480)
  par(mfrow=c(2,2))
  global_active_power_plot(data, "Global Active Power")
  voltage_plot(data)
  energy_sub_metering_plot(data)
  global_reactive_power_plot(data)
  dev.off()
}  

init_plot_defaults <- function()
{
  par(mfrow=c(1,1))
}

global_active_power_hist <- function(data)
{
  hist(
    data$GAP, 
    col="red",
    main="Global Active Power",
    xlab="Global Active Power (kilowatts)",
    ylab="Frequency")
}

global_active_power_plot <- function(data, ylabel = "Global Active Power (kilowatts)")
{
  plot(
    data$pDate,
    data$GAP, 
    type="l",
    ylab=ylabel,
    xlab="")
}

energy_sub_metering_plot <- function(data)
{
  plot(
    data$pDate,
    data$Sub_metering_1,
    type="l",
    ylab="Energy sub metering",
    xlab="")
  lines(data$pDate, data$Sub_metering_2, col="red")
  lines(data$pDate, data$Sub_metering_3, col="blue")
  legend("topright",
         col=c("black", "red", "blue"), 
         lty=c(1,1),
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
}

voltage_plot <- function(data)
{
  plot(
    data$pDate,
    data$Voltage,
    type="l",
    ylab="Voltage",
    xlab="datetime")
}

global_reactive_power_plot <- function(data)
{
  plot(
    data$pDate,
    data$GRP,
    type="l",
    ylab="Global_reactive_power",
    xlab="datetime")
}

build_data <- function()
{
  print("Reading data")
  
  data <- read.table("household_power_consumption.txt", header=TRUE, sep=";")
  
  print("Making it tidy")
  
  print("tidy Date")
  data$dDate <- as.Date(data$Date, format="%d/%m/%Y")
  
  print("Generating subset")
  from = as.Date("2007-02-01")
  to = as.Date("2007-02-02")
  data.subset <- subset(data, dDate>=from & dDate<=to)
  
  print("Making other columns tidy")
  data.subset$pDate <- strptime(paste(data.subset$Date, data.subset$Time), format='%d/%m/%Y %H:%M:%S')
  data.subset$GAP <- as.numeric(as.character(data.subset$Global_active_power))
  data.subset$GRP <- as.numeric(as.character(data.subset$Global_reactive_power))
  data.subset$Sub_metering_1 <- as.numeric(as.character(data.subset$Sub_metering_1))
  data.subset$Sub_metering_2 <- as.numeric(as.character(data.subset$Sub_metering_2))
  data.subset$Sub_metering_3 <- as.numeric(as.character(data.subset$Sub_metering_3))
  data.subset$Voltage <- as.numeric(as.character(data.subset$Voltage))
  
  return (data.subset)
}

