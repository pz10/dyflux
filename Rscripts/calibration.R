# find 6-minute midnight syncronized times
# data <- fread(full60files[1])
data[, is.360:= as.character(  as.numeric(round(epoch_time, -1)%%360 == 0)  )]
data[, bin_WORK:= as.character(bin_WORK)]

# cal1
# source("calibration_cal1.R")
source("cal1.R")
# cal2 
setkey(data, bin_INC2, bin_INC_valve, is.360)
cal2 <- data[J("1-0-00-0000-0000", "0001", "1")
             , list(time, epoch_time, flow, exhaust, p_flow, N2O_ppb, CO2_ppm, CH4_ppb, H2O_ppm, NH3_ppb,
                    bin_INC2, bin_INC_valve)]
# cal2[,is.360:=NULL]
# cal2.names <- names(cal2)
# cal2.names <- cal2.names[c(3:13,1,2)]
# setcolorder(cal2, cal2.names)
setkey(cal2, time)

#write cal2
no.format <- c("time", "epoch_time", "bin_INC2", "bin_INC_valve")
no.format <- copy(names(cal2)) %in% no.format
to.format <- copy(names(cal2))[!no.format]

cal2[,(to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format] #digit format


write.table(cal2, file= cal2file, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
print(  paste0( "CAL2 values ",Sys.time(), " CAL2 file was written to:  ")  )
print(cal2file)