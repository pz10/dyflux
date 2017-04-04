#to be used over outputs of "fullsync.R" located at *_full_60 folder/value60_IDASw_*

#time recording
print(  paste0(Sys.time(), " calibration_correction.R")  )
###

###           create output folders
# (for value, max, min and stdev)
folder_out <- paste0(IDASw_output_folder, "/", incubation, "_full_60_slopes")
dir.create(folder_out, recursive = TRUE)

valuefile <- paste0(folder_out, "/", incubation, "_full", sync,"_slopes_value.dat")
maxfile <- paste0(folder_out, "/", incubation, "_full", sync,"_slopes_max.dat")
minfile <- paste0(folder_out, "/", incubation, "_full", sync,"_slopes_min.dat")
stdevfile <- paste0(folder_out, "/", incubation, "_full", sync,"_slopes_stdev.dat")

fullslope60files <- c(valuefile, maxfile, minfile, stdevfile)
fullslope60labels <- c("value", "max", "min", "stdev")

#(for value, max, min and stdev)
folder_calibration_plots <- paste0(IDASw_output_folder, "/", incubation, "_calibration_plots")
dir.create(folder_calibration_plots, recursive = TRUE)

cal2file.sd.rg <- paste0(IDASw_output_folder, "/", incubation, "_CAL2_sd_rg.dat")
cal1summary <- paste0(IDASw_output_folder, "/", incubation, "_CAL1.summary.dat")


################################################################################
### load cal files
cal1file <- paste0(IDASw_output_folder, "/", incubation, "_CAL1.dat")
cal2file <- paste0(IDASw_output_folder, "/", incubation, "_CAL2.dat")

cal1 <- fread(input = cal1file)
cal2 <- fread(input = cal2file)

cal2[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
# cal2 <- cal2[time < cal2change,] #--> not to be used values later than 'cal2change', since it is just one calibration point at the end of incubation
cal1[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
# cal1[,start:= as.POSIXct(start, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
# cal1[,end:= as.POSIXct(end, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]

cal1[, days:= as.numeric( difftime(time, time.water, units= "days") )]
cal2[, days:= as.numeric( difftime(time, time.water, units= "days") )]


### files to be edited with calibration correction factors
tomodify_folder <- paste0(IDASw_output_folder, "/", incubation, "_full_60")
tomodify_files <- list.files(path = tomodify_folder, full.names = TRUE)
tomodify_files <- tomodify_files[c(4,1:3)] #  "Value" on 1st position

### 1st file reading
data <- fread(input = tomodify_files[1])
data[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
data[, days:= as.numeric( difftime(time, time.water, units= "days") )]

source("cal2correction.R")
source("cal1correction.R")
source("MeanFlows.R")

# format columns
no.format <- c("time", "epoch_time", "bin_INC1", "bin_INC2", "bin_INC_valve",
               "bin_TANK", "bin_WORK", "bin_cal1", "TANKcode",
               "PIC_time", "PIC_epoch_time")
to.format6 <- c("days", "PICslopeCO2", "PICslopeN2O", "PICslopeCH4", "NOslope", "NOintercept")

no.format4 <- c(no.format, to.format6)
no.format4 <- names(data) %in% no.format4
to.format4 <- names(data)[!no.format4]

data[, (to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4, flag = 0)), .SDcols = to.format4] # 4 digits
data[, (to.format6):= lapply(.SD, function(x) formatC(x, format = "f", digits = 6, flag = 0)), .SDcols = to.format6] # 6 digits

# write into output folder

write.table(data, file= fullslope60files[1], row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")

print(  paste0( "1", " of 4  ",Sys.time(), " ",fullslope60labels[1], " file was written to:  ")  )
print(fullslope60files[1])


# rest of files : *_full60_***.dat (*** = "max", "min", "stdev")

# select columns to insert in the rest of files
toinsert <- data[, list(days, PICslopeCO2, PICslopeN2O, PICslopeCH4,
                        NOslope, NOintercept, 
                        MEANflow, MEANp_flow)]


for(i in 2:4){
        data <- fread(input = tomodify_files[i])
        data[,  c("days", "PICslopeCO2", "PICslopeN2O", "PICslopeCH4",
                  "NOslope", "NOintercept", 
                  "MEANflow", "MEANp_flow")
             := toinsert]
        
        # format columns
        no.format4 <- c(no.format, to.format6)
        no.format4 <- names(data) %in% no.format4
        to.format4 <- names(data)[!no.format4]
        
        data[, (to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4, flag = 0)), .SDcols = to.format4]
        data[, (to.format6):= lapply(.SD, function(x) formatC(x, format = "f", digits = 6, flag = 0)), .SDcols = to.format6]
        
        # write data
        write.table(data, file= fullslope60files[i], row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
        
        print(  paste0( i, " of 4  ",Sys.time(), " ",fullslope60labels[i], " file was written to:  ")  )
        print(fullslope60files[i])
        
        
}

print(  paste0(Sys.time(), " calibration_correction.R is finnished")  )