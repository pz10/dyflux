#comes from calibration_correction.R
setkey(data, time)
# setkey(cal1, end)
setkey(cal1, time)
# # remove first cal1 values (change in calibration procedure)
# cal1.notvalid <- as.POSIXct("2014-05-08 16:41:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
# cal1 <- cal1[time > cal1.notvalid,]
# 

# par(mfcol=c(3,1))
# with(cal1[!CLD_NO=="NA" & bin_cal1=="1-0001",],{
#         plot(CLD_NO ~time)
#         plot(CLD_Temp ~time)
#         plot(CLD_Press ~time)
#         title("0ppb NO", outer=T, line = -2)
# })
# 
# par(mfcol=c(3,1))
# with(cal1[!CLD_NO=="NA" & bin_cal1=="1-0010",],{
#         plot(CLD_NO ~time)
#         plot(CLD_Temp ~time)
#         plot(CLD_Press ~time)
#         title("50ppb NO", outer=T, line = -2)
# })

# remove not valid cal1 (there was not enough flow these days, SYNT AIR bottle empty)
# cal1[CLD_NO > 0.5 & bin_cal1=="1-0001", CLD_NO:= NA]
# cal1[CLD_NO > 100 & bin_cal1=="1-0010", CLD_NO:= NA]

# to.interpolate.0 <- cal1[!NO=="NA" & blend==0, list(end, NO)]
# to.interpolate.50 <- cal1[!NO=="NA" & blend==50, list(end, NO)]
to.interpolate.0 <- cal1[!CLD_NO=="NA" & bin_cal1=="1-0001", list(time, CLD_NO, days)]
to.interpolate.50 <- cal1[!CLD_NO=="NA" & bin_cal1=="1-0010", list(time, CLD_NO, days)]
to.interpolate.200 <- cal1[!CLD_NO=="NA" & bin_cal1=="1-0100", list(time, CLD_NO, days)]
to.interpolate.500 <- cal1[!CLD_NO=="NA" & bin_cal1=="1-1000", list(time, CLD_NO, days)]

# get calibration sumary
source("cal1.summary.R")

int.0 <- approx(x = to.interpolate.0$time, y = to.interpolate.0$CLD_NO, xout = data$time, method = "linear", rule = 2:2)
int.50 <- approx(x = to.interpolate.50$time, y = to.interpolate.50$CLD_NO, xout = data$time, method = "linear", rule = 2:2)
int.200 <- approx(x = to.interpolate.200$time, y = to.interpolate.200$CLD_NO, xout = data$time, method = "linear", rule = 2:2)
int.500 <- approx(x = to.interpolate.500$time, y = to.interpolate.500$CLD_NO, xout = data$time, method = "linear", rule = 2:2)

int.slope <- approx(x = (myNO$time.NO.0 + 1.5*180*60), y = myNO$NOslope.lm, xout = data$time, method = "linear", rule = 2:2)
int.intercept <- approx(x = (myNO$time.NO.0 + 1.5*180*60), y = myNO$NO.0, xout = data$time, method = "linear", rule = 2:2)

# addition of interpolated 0 ,50, 200 and 500 ppb NO values to 'data'
data[,NOoffsetRTO := int.0$y]
data[,NO50int := int.50$y]
data[,NO200int := int.200$y]
data[,NO500int := int.500$y]

data[,NOslope := int.slope$y]
data[,NOintercept := int.intercept$y]

# 
data[,NOslopeRTO.50 := 50/(NO50int - NOoffsetRTO)]
data[,NOslopeRTO.200 := 200/(NO200int - NOoffsetRTO)]
data[,NOslopeRTO.500 := 500/(NO500int - NOoffsetRTO)]


# plots
# range <- range(data$time)

plot <- paste0(folder_calibration_plots, "/",incubation, "_NO_50ppb.png")
png(filename = plot, width = 1920, height = 1200, units = "px")
par(mfcol=c(1,1))
# plot(NO ~ end, data=to.interpolate.50, xlim= range, main = "50 ppb", ylab="", xlab="", type= "l")
plot(CLD_NO ~ days, data=to.interpolate.50, xlim= range, main = paste0(incubation, "_NO_50_ppb"), ylab="", xlab="", type= "l")
points(CLD_NO ~ days, data=to.interpolate.50, xlim= range)
dev.off()

plot <- paste0(folder_calibration_plots, "/",incubation, "_NO_200ppb.png")
png(filename = plot, width = 1920, height = 1200, units = "px")
par(mfcol=c(1,1))
plot(CLD_NO ~ days, data=to.interpolate.200, xlim= range, main = paste0(incubation, "_NO_200_ppb"), ylab="", xlab="", type= "l")
points(CLD_NO ~ days, data=to.interpolate.200, xlim= range)
dev.off()

plot <- paste0(folder_calibration_plots, "/",incubation, "_NO_500ppb.png")
png(filename = plot, width = 1920, height = 1200, units = "px")
par(mfcol=c(1,1))
plot(CLD_NO ~ days, data=to.interpolate.500, xlim= range, main = paste0(incubation, "_NO_500_ppb"), ylab="", xlab="", type= "l")
points(CLD_NO ~ days, data=to.interpolate.500, xlim= range)
dev.off()
##

plot <- paste0(folder_calibration_plots, "/",incubation, "_NOslopeRTO.50.png")
png(filename = plot, width = 1920, height = 1200, units = "px")
plot(NOslopeRTO.50 ~ days, data=data, xlim= range, main = paste0(incubation, "_NOslopeRTO.50"), ylab="", xlab="", type= "l")
dev.off()

plot <- paste0(folder_calibration_plots, "/",incubation, "_NOslopeRTO.200.png")
png(filename = plot, width = 1920, height = 1200, units = "px")
plot(NOslopeRTO.200 ~ days, data=data, xlim= range, main = paste0(incubation, "_NOslopeRTO.200"), ylab="", xlab="", type= "l")
dev.off()

plot <- paste0(folder_calibration_plots, "/",incubation, "_NOslopeRTO.500.png")
png(filename = plot, width = 1920, height = 1200, units = "px")
plot(NOslopeRTO.500 ~ days, data=data, xlim= range, main = paste0(incubation, "_NOslopeRTO.500"), ylab="", xlab="", type= "l")
dev.off()

plot <- paste0(folder_calibration_plots, "/",incubation, "_NOoffsetRTO.png")
png(filename = plot, width = 1920, height = 1200, units = "px")
plot(NOoffsetRTO ~ days, data=data, xlim= range, main = paste0(incubation, "_NOoffsetRTO"), ylab="", xlab="", type= "l")
dev.off()

#############################
#### Extra calibration plots
source("cal1plots.R")


# delete 'NO50int'
data[,NOoffsetRTO := NULL]
data[,NO50int := NULL]
data[,NO200int := NULL]
data[,NO500int := NULL]

data[,NOslopeRTO.50 := NULL]
data[,NOslopeRTO.200 := NULL]
data[,NOslopeRTO.500 := NULL]