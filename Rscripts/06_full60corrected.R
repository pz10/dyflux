#to be used over outputs of "calibration_correction.R" located at *_incubation_ful_60_slopes

#time recording
print(  paste0(Sys.time(), " start ful60corrected.R")  )
###

###           create output folders
#(for value, max, min and stdev)
folder_out <- paste0(IDASw_output_folder, "/", incubation, "_full_60_corrected")
dir.create(folder_out, recursive = TRUE)

valuefile <- paste0(folder_out, "/", incubation, "_full", sync,"_corrected", "_value.dat")
maxfile <- paste0(folder_out, "/", incubation, "_full", sync,"_corrected", "_max.dat")
minfile <- paste0(folder_out, "/", incubation, "_full", sync,"_corrected", "_min.dat")
stdevfile <- paste0(folder_out, "/", incubation, "_full", sync,"_corrected", "_stdev.dat")

# # tank
tankfile <- paste0(IDASw_output_folder, "/", incubation, "_TANK_corrected.dat")
tankfilesd <- paste0(IDASw_output_folder, "/", incubation, "_TANKsd_corrected.dat")
tankfileraw <- paste0(IDASw_output_folder, "/", incubation, "_TANK_allvalues_corrected.dat")
TANK_plot <- paste0(IDASw_output_folder, "/", incubation, "_incubation_TANK_corrected.png")

full60correctedfiles <- c(valuefile, maxfile, minfile, stdevfile)
full60correctedlabels <- c("value", "max", "min", "stdev")


### input folders
folder_in <- paste0(IDASw_output_folder, "/", incubation, "_full_60_slopes")
files_in <- list.files(path = folder_in, full.names = TRUE)
files_in <- files_in[c(4,1:3)] #  "Value" on 1st position

# value file (1st file)
data <- fread(input = files_in[1])
data[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
working_names <- copy( names(data) )

# to modify PIC value, use the following formula:
#         PICvalue * factor (intercept = 0 is considered)
data[,CO2_ppm:= CO2_ppm * PICslopeCO2]
data[,CH4_ppb:= CH4_ppb * PICslopeCH4]
data[,N2O_ppb:= N2O_ppb * myN2O.slope] # 'myN2O.slope' hardcoded value, see _index.R
# data[,N2O_ppb:= N2O_ppb * PICslopeN2O]

# to modify CLD_NO value, use the following formula:
#         (NOvalue - NOzero) * factor
data[,CLD_NO:= (CLD_NO - NOintercept) * NOslope]
data[,CLD_NO360:= (CLD_NO360 - NOintercept) * NOslope] # assume no change of slope or offset in 1-5 minutes

# delete slopes and offsets
todelete <- c("PICslopeCO2", "PICslopeN2O", "PICslopeCH4",
              "NOslope", "NOintercept")
set(data, j = todelete, value=NULL)
# keep original names for later ordering
#
original_names <- copy( names(data)  )

############################
# append recalculated 'TANK' values
source("TANK.R")
set(data, j = TANK_ids, value=NULL) #we 1st delete old values before appending the new calculated ones

set(dataTANK, j = c("TANKcode", "epoch_time"), value=NULL)
data <- cbind(data, dataTANK)

# format columns
no.format <- c("time", "epoch_time", "bin_INC1", "bin_INC2", "bin_INC_valve",
               "bin_TANK", "bin_WORK", "bin_cal1", "TANKcode", "PIC_time", "PIC_epoch_time")
no.format <- names(data) %in% no.format
to.format <- names(data)[!no.format]

data[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format]        

# chronological ordering of data and column reordering
setkey(data, epoch_time)
setcolorder(data, original_names)

############################
#
#write into output folder
write.table(data, file= full60correctedfiles[1], row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")

print(  paste0( "1 of 4  ",Sys.time(), " value file was written to:  ")  )
print(full60correctedfiles[1])


# files proccessing (MAX, MIN and STDEV)
w <- working_names # just to keep sd and rg prefix in the following files
for(i in 2:4){
        working_names <- w
        data <- fread(input = files_in[i])
        data[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
        original_names <- copy( names(data) )
        setnames(data, original_names,working_names)
        
        # to modify PIC value, use the following formula:
        #         PICvalue * factor (intercept = 0 is considered)
        data[,CO2_ppm:= CO2_ppm * PICslopeCO2]
        data[,CH4_ppb:= CH4_ppb * PICslopeCH4]
        data[,N2O_ppb:= N2O_ppb * myN2O.slope] # 'myN2O.slope' hardcoded value, see _index.R
        # data[,N2O_ppb:= N2O_ppb * PICslopeN2O]

        
        # CLD_NO MAX, MIN and SD values are senseless since only one data point per minute is recorded:
        data[,CLD_NO:= NA]
        data[,CLD_NO360:= NA]
        data[,CLD_Temp:= NA]
        data[,CLD_Press:= NA]
        data[,CLD_warning:= NA]
        data[,CLD_error:= NA]
        
        # delete slopes and offsets
        todelete <- c("PICslopeCO2", "PICslopeN2O", "PICslopeCH4",
                      "NOslope", "NOintercept")
        set(data, j = todelete, value=NULL)
        # keep original names for later ordering
        #
        working_names <- copy( names(data) )
        original_names <- original_names[!original_names %in% todelete]
        
        
        #we 1st delete old values before appending the new calculated ones
        set(data, j = TANK_ids, value=NULL)
        
        
        setkey(data, epoch_time)
        data <- cbind(data, dataTANK)
        
        # format columns
        no.format <- c("time", "epoch_time", "bin_INC1", "bin_INC2", "bin_INC_valve",
                       "bin_TANK", "bin_WORK", "bin_cal1", "TANKcode", "PIC_time", "PIC_epoch_time")
        no.format <- names(data) %in% no.format
        to.format <- names(data)[!no.format]
        
        data[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format]        
        
        # chronological ordering of data and column reordering
        setkey(data, epoch_time)
        setcolorder(data, working_names)
        setnames(data, working_names, original_names)
        
        ############################
        #
        #write into output folder
        write.table(data, file= full60correctedfiles[i], row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
        
        print(  paste0( i," of 4  ",Sys.time(), " ",full60correctedlabels[i]," file was written to:  ")  )
        print(full60correctedfiles[i])
        
}


### plot 'TANK' values
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

### write TANK values (AIR) to .dat file
#
# digit format
TANK[,(TANK_ids):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = TANK_ids]
TANK[,days:= formatC(days, format = "f", digits = 6)]

write.table(TANK, file= tankfile, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
print(  paste0( "TANK values ",Sys.time(), " TANK file was written to:  ")  )
print(tankfile)


print(  paste0(Sys.time(), " ful60corrected.R is finnished")  )