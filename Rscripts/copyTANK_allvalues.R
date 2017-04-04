TANKraw <- copy(TANK)

TANKraw[, is360:=NULL]


# format columns
no.format <- c("time", "epoch_time", "bin_INC1", "bin_INC2", "bin_INC_valve",
               "bin_TANK", "bin_WORK", "bin_cal1", "TANKcode",
               "QCL_time", "QCL_epoch_time", "QCLstretch",
               "PIC_time", "PIC_epoch_time",
               "PICARROtime", "PICARROepoch_time", "start", "end")
no.format <- names(TANKraw) %in% no.format
to.format <- names(TANKraw)[!no.format]

TANKraw[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4, flag = 0)), .SDcols = to.format]

# chronological ordering of TANKraw
setkey(TANKraw, time)

write.table(TANKraw, file= tankfileraw, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
print(  paste0( "TANK (allvalues) ",Sys.time(), " TANK (all values) file was written to:  ")  )
print(tankfileraw)