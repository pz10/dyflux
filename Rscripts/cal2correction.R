# comes from calibration_correction.R

# ### laser failure times
# QCLfailure <- paste0(input_path, "/", incubation, "_extra/", incubation, "_incubation_QCLfailure_times.csv")
# QCLfailure <- fread(input= QCLfailure)
# QCLfailure[,start:= as.POSIXct(start, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
# QCLfailure[,end:= as.POSIXct(end, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
# 
# # Li840 not valid cal2 file
# Li840cal2.NOTVALID <- paste0(input_path, "/", incubation, "_extra/", incubation, "_Li840_cal2_isnotvalid.csv")
# Li840cal2.NOTVALID <- fread(input= Li840cal2.NOTVALID)
# Li840cal2.NOTVALID[,start:= as.POSIXct(start, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
# Li840cal2.NOTVALID[,end:= as.POSIXct(end, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]

### select data time range
# range <- range(data$time)
range <- range(data$days)

################################################################################
# get sd and rg values for cal2
cal2[ ,unclass.time:= unclass(time)]
times.cal2 <- cal2$unclass.time

stdev <- fread(tomodify_files[4])
stdev[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
stdev[,unclass.time:= unclass(time)]
stdev <- stdev[unclass.time %in% times.cal2, .(
        unclass.time, sd_flow = flow, sd_exhaust = exhaust, sd_N2O_ppb, sd_CO2_ppm, sd_CH4_ppb, sd_H2O_ppm, sd_NH3_ppb
)]

rg <- fread(tomodify_files[2])
rg[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
rg[,unclass.time:= unclass(time)]
rg <- rg[unclass.time %in% times.cal2, .(
        unclass.time, rg_flow = flow, rg_exhaust = exhaust, rg_N2O_ppb, rg_CO2_ppm, rg_CH4_ppb, rg_H2O_ppm, rg_NH3_ppb
)]

setkey(cal2, unclass.time)
setkey(stdev, unclass.time)
setkey(rg, unclass.time)

cal2 <- cal2[stdev[rg]]
cal2[ ,unclass.time:=NULL]

mydata <- copy(cal2)

to.format6 <- c("days")
mydata[, (to.format6):= lapply(.SD, function(x) formatC(x, format = "f", digits = 6, flag = 0)), .SDcols = to.format6] # 6 digits

write.table(mydata, file= cal2file.sd.rg, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
print(paste(Sys.time(),"--> cal2 file including sd and rg values was written to:"))
print(cal2file.sd.rg)

# ### cal2: remove obvious outliers: (have checked the data first)
# # PIC
# #values 
# par(mfrow= c(2,2))
# with(cal2, {
#         plot(N2O_ppb ~ time, xlim = range)
#         plot(CO2_ppm ~ time, xlim = range)
#         plot(CH4_ppb ~ time, xlim = range)
#         plot(H2O_ppm ~ time, xlim = range)
# })
# 
# par(mfrow= c(2,2))
# with(cal2, {
#         plot(flow ~ time, xlim = range)
#         plot(exhaust ~ time, xlim = range)
# })
# 
# #sd 
# par(mfrow= c(2,2))
# with(cal2, {
#         plot(sd_N2O_ppb ~ time, xlim = range)
#         plot(sd_CO2_ppm ~ time, xlim = range)
#         plot(sd_CH4_ppb ~ time, xlim = range)
#         plot(sd_H2O_ppm ~ time, xlim = range)
# })
# 
# par(mfrow= c(2,2))
# with(cal2, {
#         plot(sd_flow ~ time, xlim = range)
#         plot(sd_exhaust ~ time, xlim = range)
# })
# 
# # rg 
# par(mfrow= c(2,2))
# with(cal2, {
#         plot(rg_N2O_ppb ~ time, xlim = range)
#         plot(rg_CO2_ppm ~ time, xlim = range)
#         plot(rg_CH4_ppb ~ time, xlim = range)
#         plot(rg_H2O_ppm ~ time, xlim = range)
# })
# 
# par(mfrow= c(2,2))
# with(cal2, {
#         plot(rg_flow ~ time, xlim = range)
#         plot(rg_exhaust ~ time, xlim = range)
# })

# 
# cal2[(N2O_ppb < "350") | (N2O_ppb > "500"), N2O_ppb:=NA]
# cal2[(CO2_ppm < "350") | (CO2_ppm > "500"), CO2_ppm:=NA]
# cal2[(CH4_ppb < "3900") | (CH4_ppb > "4000"), CH4_ppb:=NA]

################################################################################
####plots

# plot(CO2_ppm ~ time, data = cal2, xlim= range)
# abline(v=QCLfailure$start, col="red")
# abline(v=QCLfailure$end, col="black")
# 
# 
# plot(N2O_ppb ~ time, data = cal2, xlim= range)
# abline(v=QCLfailure$start, col="red")
# abline(v=QCLfailure$end, col="black")
# 
# plot(CH4_ppb ~ time, data = cal2, xlim= range)
# abline(v=QCLfailure$start, col="red")
# abline(v=QCLfailure$end, col="black")
# 
# 
# plot(Li_CO2 ~ time, data = cal2, xlim= range)
# 
# plot(PICARRON2O ~ time, data = cal2, xlim= range)
# plot(PICARROCO2 ~ time, data = cal2, xlim= range)
# plot(PICARROCH4 ~ time, data = cal2, xlim= range)
# plot(PICARRONH3 ~ time, data = cal2, xlim= range)

################################################################################
# ### QCL stretchs
# cut<- c(paste(QCLfailure$start), paste(QCLfailure$end), paste(range))
# cut <- as.POSIXct(cut, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
# cut <- sort(cut)
# 
# data[,QCLstretch:= cut(data$time, breaks = cut, labels = FALSE, include.lowest = TRUE, right = TRUE)]
# 
# # labeling:
# # QCLstretch positive ==> QCL works fine
# # QCLstretch negative ==> QCL is not valid
# for(i in 1:nrow(QCLfailure)){
#         data[time >= QCLfailure$start[i] & time <= QCLfailure$end[i], QCLstretch:= -i]
# }
# 
# stretch.labels<- sort( unique(data$QCLstretch) )
# stretch.labels <- stretch.labels[stretch.labels>0]
# 
# for(i in 1:length(stretch.labels) ){
#         data[QCLstretch == stretch.labels[i], QCLstretch:= i]
# }

################################################################################
###interpolation
setkey(data, time)
setkey(cal2, time)

#add failure times to the data to interpolate 'to.int
to.int<- cal2[, list(time, N2O_ppb, CO2_ppm, CH4_ppb)]

# fail.start <- QCLfailure[,list(time = start)]
# fail.end <- QCLfailure[,list(time = end)]
# fail.start[,c("N2O_ppb", "CO2_ppm", "CH4_ppb"):= -99]
# fail.end[,c("N2O_ppb", "CO2_ppm", "CH4_ppb"):= -99]
# 
# to.int[,fail:= "noFAIL"]
# fail.start[,fail:= "start"]
# fail.end[,fail:= "end"]
# 
# to.int <- rbind(to.int, fail.start, fail.end)

# create'to.int'objects by species, with failure times with NA
setkey(to.int, time)

to.int.QCL.N2O <- to.int[!N2O_ppb=="NA", list(time, N2O_ppb)]
to.int.QCL.CO2 <- to.int[!CO2_ppm=="NA", list(time, CO2_ppm)]
to.int.QCL.CH4 <- to.int[!CH4_ppb=="NA", list(time, CH4_ppb)]

#interpolation

int.QCL.N2O <- approx(x = to.int.QCL.N2O$time, y = to.int.QCL.N2O$N2O_ppb, xout = data$time, method = "linear", rule = 2:2)
int.QCL.CO2 <- approx(x = to.int.QCL.CO2$time, y = to.int.QCL.CO2$CO2_ppm, xout = data$time, method = "linear", rule = 2:2)
int.QCL.CH4 <- approx(x = to.int.QCL.CH4$time, y = to.int.QCL.CH4$CH4_ppb, xout = data$time, method = "linear", rule = 2:2)


# addition of interpolated values
data[, PICslopeN2O:= Cal2_N2O/int.QCL.N2O$y]
data[, PICslopeCO2:= Cal2_CO2/int.QCL.CO2$y]
data[, PICslopeCH4:= Cal2_CH4/int.QCL.CH4$y]

################################################################################
###plots

#######slopes
# N2O
plot <- paste0(folder_calibration_plots, "/",incubation, "_N2O_slopes.png")
png(filename = plot, width = 1920, height = 1200, units = "px")

par(mfcol = c(1,1), cex = 1.5 )
plot(Cal2_N2O/N2O_ppb ~ days, data = cal2, xlim= range)
lines(PICslopeN2O ~ days, data = data, xlim= range)
abline(h=1, col="red")
title(paste0(incubation,"_N2O_slopes"))
dev.off()


# CO2
plot <- paste0(folder_calibration_plots, "/",incubation, "_CO2_slopes.png")
png(filename = plot, width = 1920, height = 1200, units = "px")

par(mfcol = c(1,1), cex = 1.5 )
plot(Cal2_CO2/CO2_ppm ~ days, data = cal2, xlim= range)
lines(PICslopeCO2 ~ days, data = data, xlim= range)
abline(h=1, col="red")
title(paste0(incubation,"_CO2_slopes"))
dev.off()

# CH4
plot <- paste0(folder_calibration_plots, "/",incubation, "_CH4_slopes.png")
png(filename = plot, width = 1920, height = 1200, units = "px")

par(mfcol = c(1,1), cex = 1.5 )
plot(Cal2_CH4/CH4_ppb ~ days, data = cal2, xlim= range)
lines(PICslopeCH4 ~ days, data = data, xlim= range)
abline(h=1, col="red")
title(paste0(incubation,"_CH4_slopes"))
dev.off()


#######RAW calibration values (QCL and Li840)
plot <- paste0(folder_calibration_plots, "/",incubation, "_calibration_values.png")
png(filename = plot, width = 1920, height = 1200, units = "px")

par(mfcol = c(2,2), cex = 1.5 )
# n2o
plot(N2O_ppb ~ days, data = cal2, xlim= range)
lines(N2O_ppb ~ days, data = cal2, xlim= range)
abline(h=Cal2_N2O, col= "red")
points(N2O_ppb ~ days, data = cal2, xlim= range)

# co2
plot(CO2_ppm ~ days, data = cal2, xlim= range)
lines(CO2_ppm ~ days, data = cal2, xlim= range)
abline(h=Cal2_CO2, col= "red")
points(CO2_ppm ~ days, data = cal2, xlim= range)


# ch4
plot(CH4_ppb ~ days, data = cal2, xlim= range)
lines(CH4_ppb ~ days, data = cal2, xlim= range)
abline(h=Cal2_CH4, col= "red")
points(CH4_ppb ~ days, data = cal2, xlim= range)


title(paste0(incubation,"_calibration_values"), line = -2, outer = TRUE)

dev.off()
