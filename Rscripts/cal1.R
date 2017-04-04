# cal1
setkey(data, bin_cal1, is.360)

cal1.0 <- data[J("1-0001", "1"), list(time, epoch_time, CLD_NO, CLD_Temp, CLD_Press, CLD_warning, CLD_error, bin_cal1)]
cal1.50 <- data[J("1-0010", "1"), list(time, epoch_time, CLD_NO, CLD_Temp, CLD_Press, CLD_warning, CLD_error, bin_cal1)]
cal1.200 <- data[J("1-0100", "1"), list(time, epoch_time, CLD_NO, CLD_Temp, CLD_Press, CLD_warning, CLD_error, bin_cal1)]
cal1.500 <- data[J("1-1000", "1"), list(time, epoch_time, CLD_NO, CLD_Temp, CLD_Press, CLD_warning, CLD_error, bin_cal1)]


cal1 <- rbindlist(list(cal1.0, cal1.50, cal1.200, cal1.500))

# delete CAL1 failure times
cal1fail <- fread(cal1failure)
cal1fail[,start:= as.POSIXct(start, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
cal1fail[,end:= as.POSIXct(end, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]

setkey(cal1, time)
for(i in nrow(cal1fail)){
        cal1 <- cal1[time <= cal1fail$start[i] | time >= cal1fail$end[i], ]
}

# export file

# cal1[,is.360:=NULL]
# cal1.names <- names(cal1)
# cal1.names <- cal1.names[c(2:8,1)]
# setcolorder(cal1, cal1.names)
setkey(cal1, time)
setkey(cal1, bin_cal1)

#write cal1
write.table(cal1, file= cal1file, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
print(  paste0( "CAL1 values ",Sys.time(), " CAL1 file was written to:  ")  )
print(cal1file)
