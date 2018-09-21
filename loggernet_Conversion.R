library(ggplot2)

table <- read.table("/Users/Andrew/Downloads/LabTest09192018.csv",header = TRUE, skip = 0, sep = ",")
colnames(table) <- c('TIMESTAMP','RECORD','CO2_1_avg','CO2_1_Min','CO2_1_Max','CO2_1_TMx','CO2_2_Avg','CO2_2_Max',
                     'CO2_2_TMx','CO2_2_Min','CO2_2_TMin','EOS_Flux_Avg','EOS_Flux_Max','EOS_Flux_TMx','EOS_Flux_Min',
                     'EOS_Flux_TMn','EOS_CO2_Avg','EOS_Temp_Avg','BattV_Avg','PTemp_C_Avg','Temp_C_Avg')
#Convert time to Posixct
table$time <- as.POSIXct(table$TIMESTAMP)

#Convert Analog outputs to PPM
table$CO2_1_PPM <- (table$CO2_1_avg / 1000) * 10000
table$CO2_2_PPM <- (table$CO2_2_Avg / 1000) * 10000
table$EOS_CO2_PPM <- (table$EOS_CO2_Avg / 5000) * 5000
table$EOS_tempC <- ((table$EOS_Temp_Avg / 5000) * 70) -20

#Convert Flux to range of -10 to 10 umol m2s1
table$EOS_Flux_umol <- ((table$EOS_Flux_Avg / 5000) * 20) -10

#Build a plot to visualize variation in CO2 measurements

#Create simplified table with CO2 Measurements by Sensor
CO2_1 <- data.frame(table$CO2_1_PPM)
CO2_1$Sensor <- "Sensor 1"
CO2_1$Time <- as.POSIXct(table$TIMESTAMP)
colnames(CO2_1) <- c("PPM","Sensor","Time")

CO2_2 <- data.frame(table$CO2_2_PPM)
CO2_2$Sensor <- "Sensor 2"
CO2_2$Time <- as.POSIXct(table$TIMESTAMP)
colnames(CO2_2) <- c("PPM","Sensor","Time")

EOS_CO2 <- data.frame(table$EOS_CO2_PPM)
EOS_CO2$Sensor <- "EOS Sensor"
EOS_CO2$Time <- as.POSIXct(table$TIMESTAMP)
colnames(EOS_CO2) <- c("PPM","Sensor", "Time")

CO2_All <- rbind(CO2_1,CO2_2,EOS_CO2)

#Simple CO2 plot with lines

lines <- ggplot(CO2_All, aes(x=Time, y=PPM, group=Sensor, colour = Sensor))+geom_line()+
  ggtitle("Lab Test of Multiple CO2 Sensors")
lines

#CO2 Flux Plot
flux <- data.frame(as.POSIXct(table$TIMESTAMP))
flux$flux <- table$EOS_Flux_umol
colnames(flux) <- c('time','flux')

flux_lines <- ggplot(flux, aes(x=time, y=flux, group=1))+geom_line(colour = '#4B9CD3')+
  ggtitle("Lab Test of Flux Sensor")+
  ylab(expression(mu ~ moles ~ m^{-2} ~ s^{-1}))
flux_lines

#Temperature Plot
EOS_temp <- data.frame(table$EOS_tempC)
EOS_temp$sensor <- "EOS"
colnames(EOS_temp) <- c('Temp','Sensor')

Amb_temp <- data.frame(table$Temp_C_Avg)
Amb_temp$sensor <- "Thermocouple"
colnames(Amb_temp) <- c('Temp','Sensor')

Log_temp <-  data.frame(table$PTemp_C_Avg)
Log_temp$sensor <- "Data Logger Internal"
colnames(Log_temp) <- c('Temp','Sensor')

temp <- rbind(EOS_temp,Amb_temp,Log_temp)
temp$time <- as.POSIXct(table$TIMESTAMP)

temp_lines <- ggplot(temp, aes(x=time, y=Temp, group=Sensor, colour = Sensor))+geom_line()+
  ggtitle("Lab Test of Multiple Temp Sensors")
temp_lines