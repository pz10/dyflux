#to be used over outputs of "IDASw_wide_60.R" and "QCLsync.R"
#it will merge IDASw60 data with QCL data at these times

#time recording
print(  paste0(Sys.time(), " start fullsync.R")  )
###

##           create output folder and define file names
# (for value, max, min and stdev)
folder_out <- paste0(IDASw_output_folder, "/", incubation, "_full_60")
dir.create(folder_out, recursive = TRUE)

valuefile<- paste0(folder_out, "/", incubation, "_full", sync, "_value.dat")
maxfile<- paste0(folder_out, "/", incubation, "_full", sync, "_max.dat")
minfile<- paste0(folder_out, "/", incubation, "_full", sync, "_min.dat")
stdevfile<- paste0(folder_out, "/", incubation, "_full", sync, "_stdev.dat")

full60files <- c(valuefile, maxfile, minfile, stdevfile)
full60labels <- c("value", "max", "min", "stdev")

# tank
tankfile <- paste0(IDASw_output_folder, "/", incubation, "_TANK_noncorrected.dat")
tankfilesd <- paste0(IDASw_output_folder, "/", incubation, "_TANKsd_noncorrected.dat")
tankfileraw <- paste0(IDASw_output_folder, "/", incubation, "_TANK_allvalues_noncorrected.dat")
TANK_plot <- paste0(IDASw_output_folder, "/", incubation, "_incubation_TANK_noncorrected.png")

# calibration
cal2file <- paste0(IDASw_output_folder, "/", incubation, "_CAL2.dat")
cal1fileRAW <- paste0(IDASw_output_folder, "/", incubation, "_CAL1_raw.dat")
cal1file <- paste0(IDASw_output_folder, "/", incubation, "_CAL1.dat")
cal1failure <- paste0(input_path, "/", incubation, "_extra/CAL1_failure.dat")
folder_calibration_plots <- paste0(IDASw_output_folder, "/", incubation, "_calibration_plots")
dir.create(folder_calibration_plots, recursive = TRUE)


##           define input folders (for IDASw: min, max, stdev and value)(PIC: range, stdev and value)
#

PIC_files <- list.files(path = PIC_output_folder, pattern = paste0("PIC", sync), full.names = TRUE)
PIC_files <- PIC_files[c(1,2,2,3)]

IDASw_folder <- paste0(IDASw_output_folder, "/", incubation, "_IDASw_60")
IDASw_files <- list.files(path = IDASw_folder, full.names = TRUE)
IDASw_files <- IDASw_files[c(4,1:3)] # so that PIC_files and IDASw_files has the same order. "Value" on 1st position

# usage of data.table


# 1st file proccessing (VALUE) and creation of filter to pick up target IDAsw time from QCL data
data <- fread(input = IDASw_files[1], header=T, sep = "\t")
data[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]

# create new column : "TANKcode", every time Tank valve is open, it will increase the code by 1.
# useful for later background air calculation
# find TANK opening times
# source("TANKcutsINSERT.R")
a0 <- data[,bin_TANK]
a1 <- c(a0[1],a0[1:(length(a0)-1)])
flag <- as.numeric(  (a0-a1) > 0  )

for( i in 2:nrow(data)){
        flag[i] <- flag[i-1] + flag[i]
}

data[,TANKcode:=flag]

# columns to be exported
export_cols <- c("time", "epoch_time", "bin_INC1", "bin_INC2", "bin_INC_valve", "bin_TANK", "bin_WORK", "bin_cal1", "TANKcode",
                 "C1001", "C1002", "C1010", "C1020", "C1030",
                 "C2001", "C2002", "C2003", "C2004", "C2005", "C2006")
new_col_names <- c("time", "epoch_time", "bin_INC1", "bin_INC2", "bin_INC_valve", "bin_TANK", "bin_WORK", "bin_cal1", "TANKcode",
                   "RH", "T", "flow", "exhaust", "p_flow",
                   "CLD_NO", "CLD_NO360", "CLD_Temp", "CLD_Press", "CLD_warning", "CLD_error")
# check if columns exist 
is.col <- export_cols %in% copy(names(data)) #!is.na(  match(export_cols, names(data))  )
# set column new names to export
export_cols <- export_cols[is.col]
new_col_names <- new_col_names[is.col]
# locate columns to delete
todelete <- copy(names(data)) %in% export_cols
todelete1 <- copy(names(data))[!todelete]

# delete "todelete" columns 
set(data, j = todelete1, value=NULL)
# reorder columns and rename
setcolorder(data, export_cols)
setnames(data, export_cols, new_col_names)

# read PIC60 data
PICdata <- fread(input = PIC_files[1], header=T, sep = "\t")
PICdata[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
oldnames <- copy( names(PICdata)  )
PICnames <- c("PIC_time", "PIC_epoch_time", oldnames[3:7] )
setnames( PICdata, oldnames, PICnames)

# target times filtration; merge of IDASw and PIC data.tables
data[,time1:=time]
setkey(data, time1)
setkey(PICdata, PIC_time)
data <- PICdata[data]

# remove not valid values (several reasons) 
# source("notvalid.R")

############################
# append TANK values
source("TANK.R")
set(dataTANK, j = c("TANKcode", "epoch_time"), value=NULL)
data <- cbind(data, dataTANK)

# columns reorder and rename
data_newnames <- c(new_col_names, TANK_ids, PICnames)
setcolorder(data, data_newnames)

# format columns
no.format <- c("time", "epoch_time",
               "bin_INC1", "bin_INC2", "bin_INC_valve", "bin_TANK", "bin_WORK", "bin_cal1", "TANKcode",
               "PIC_time", "PIC_epoch_time")
no.format <- data_newnames %in% no.format
to.format <- data_newnames[!no.format]

data[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format]

# chronological ordering of data
setkey(data, epoch_time)

############################
#
#write into output folder
write.table(data, file= full60files[1], row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")

print(  paste0( "1 of 4  ",Sys.time(), " value file was written to:  ")  )
print(full60files[1])


# write cal1 and cal 2 data to files
source("calibration.R")

# rest of files proccessing (MAX, MIN, STDEV)
for(i in 2:4){
        data <- fread(input = IDASw_files[i], header=T, sep = "\t")
        data[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
        
        # create new column : "TANKcode"--> assign already-calculated TANK codes, storaged at "flag"
        data[,TANKcode:=flag]
        
        # columns to be exported
        export_cols <- c("time", "epoch_time", "bin_INC1", "bin_INC2", "bin_INC_valve", "bin_TANK", "bin_WORK", "bin_cal1", "TANKcode",
                         "C1001", "C1002", "C1010", "C1020", "C1030",
                         "C2001", "C2002", "C2003", "C2004", "C2005", "C2006")
        new_col_names <- c("time", "epoch_time", "bin_INC1", "bin_INC2", "bin_INC_valve", "bin_TANK", "bin_WORK", "bin_cal1", "TANKcode",
                           "RH", "T", "flow", "exhaust", "p_flow",
                           "CLD_NO", "CLD_NO360", "CLD_Temp", "CLD_Press", "CLD_warning", "CLD_error")
        # check if columns exist 
        is.col <- export_cols %in% copy(names(data)) #!is.na(  match(export_cols, names(data))  )
        # set column new names to export
        export_cols <- export_cols[is.col]
        new_col_names <- new_col_names[is.col]
        # locate columns to delete
        todelete <- copy(names(data)) %in% export_cols
        todelete1 <- copy(names(data))[!todelete]
        
        # delete "todelete" columns 
        set(data, j = todelete1, value=NULL)
        # reorder columns and rename
        setcolorder(data, export_cols)
        setnames(data, export_cols, new_col_names)
        
        # read PIC60 data
        PICdata <- fread(input = PIC_files[i], header=T, sep = "\t")
        PICdata[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
        oldnames <- copy( names(PICdata) )
        PICnames <- c("PIC_time", "PIC_epoch_time", oldnames[3:7] )
        setnames( PICdata, old = oldnames, new = PICnames)
        
        # target times filtration; merge of IDASw and PIC data.tables
        data[,time1:=time]
        setkey(data, time1)
        setkey(PICdata, PIC_time)
        data <- PICdata[data]
        
        ############################
        # convert bin_INC_valve to character in the 0000 format
        data[,bin_INC_valve:= formatC(bin_INC_valve, 3, flag=0)]
        
        # TANK_values added from already-calculated median of "value" data in a given TANKcode (storaged at dataTANK)
        # It does not make sense to calculate max, min or stdev for max, min and stdev files.
        setkey(data, epoch_time)
        data <- cbind(data, dataTANK)
        
        # columns reorder and rename
        data_newnames <- c(new_col_names, TANK_ids, PICnames)
        setcolorder(data, data_newnames)
        
        # format columns
        no.format <- c("time", "epoch_time",
                       "bin_INC1", "bin_INC2", "bin_INC_valve", "bin_TANK", "bin_WORK", "bin_cal1", "TANKcode",
                       "PIC_time", "PIC_epoch_time")
        no.format <- data_newnames %in% no.format
        to.format <- data_newnames[!no.format]
        
        data[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4, flag = 0)), .SDcols = to.format]
        
        # chronological ordering of data
        setkey(data, epoch_time)
        
        ############################
        #
        #write into output folder
        write.table(data, file= full60files[i], row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
        
        print(  paste0( i, " of 4  ",Sys.time(), " ",full60labels[i], " file was written to:  ")  )
        print(full60files[i])
        
}


### plot 'TANK' values
#
png(filename = TANK_plot, width = 1920, height = 1200, units = "px")

rgN2O <- range(TANK$TANK_N2O_ppb, na.rm= T)
rgCO2 <- range(TANK$TANK_CO2_ppm, na.rm= T)
rgCH4 <- range(TANK$TANK_CH4_ppb, na.rm= T)

TANK[,days:= as.numeric( difftime(start, time.water, units= "days") )]

par(mfcol = c(2,2), cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5)
# with(TANK,  {
#         plot(TANK_CLD_NO ~ start, main = paste0(incubation, "_TANK_NO_ppb"), ylab= "", xlab= "", pch= 20, type = "l")
#         plot(TANK_N2O_ppb ~ start, main = paste0(incubation, "_TANK_N2O_ppb"), ylab= "", xlab= "", pch= 20, type = "l", ylim = rgN2O)
#         #         lines(x= TANK[, start], y= TANK[,TANK_PICARRON2O], col= "red")
#         
#         plot(TANK_CO2_ppm ~ start, main = paste0(incubation, "_TANK_CO2_ppm (Li840 blue)"), ylab= "", xlab= "", pch= 20, type = "l", ylim = rgCO2)
#         #         lines(x= TANK[, start], y= TANK[,TANK_PICARROCO2], col= "red")
#         
#         plot(TANK_CH4_ppb ~ start, main = paste0(incubation, "_TANK_CH4_ppb"), ylab= "", xlab= "", pch= 20, type = "l", ylim = rgCH4)
#         #         lines(x= TANK[, start], y= TANK[,TANK_PICARROCH4], col= "red")
#         
# }
# )
# dev.off()
with(TANK,  {
        plot(TANK_CLD_NO ~ days, main = paste0(incubation, "_TANK_NO_ppb"), ylab= "", xlab= "", pch= 20, type = "l")
        plot(TANK_N2O_ppb ~ days, main = paste0(incubation, "_TANK_N2O_ppb"), ylab= "", xlab= "days", pch= 20, type = "l", ylim = rgN2O)
        #         lines(x= TANK[, days], y= TANK[,TANK_PICARRON2O], col= "red")
        
        plot(TANK_CO2_ppm ~ days, main = paste0(incubation, "_TANK_CO2_ppm"), ylab= "", xlab= "", pch= 20, type = "l", ylim = rgCO2)
        #         lines(x= TANK[, days], y= TANK[,TANK_PICARROCO2], col= "red")
        
        plot(TANK_CH4_ppb ~ days, main = paste0(incubation, "_TANK_CH4_ppb"), ylab= "", xlab= "days", pch= 20, type = "l", ylim = rgCH4)
        #         lines(x= TANK[, days], y= TANK[,TANK_PICARROCH4], col= "red")
        
}
)
dev.off()
# TANK[,days:= NULL]

### write TANK values
#
# digit format
TANK[,(TANK_ids):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = TANK_ids]
TANK[,days:= formatC(days, format = "f", digits = 6)]

write.table(TANK, file= tankfile, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
print(  paste0( "TANK values ",Sys.time(), " TANK file was written to:  ")  )
print(tankfile)


print(  paste0(Sys.time(), " fullsync.R is finnished")  )