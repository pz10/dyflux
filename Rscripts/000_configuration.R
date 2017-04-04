.libPaths(c(.libPaths(), "C:/Users/zuazo-p/Documents/R/win-library/3.0"))

# cleanning of workspace and time recording
rm(list = ls())
Sys.setenv(TZ='UTC') #--> very important
print(Sys.time())
setwd("Rscripts")

###
#           initial parameters
#0) set incubation replicate
incubation <- "Ruth"

# core area [m2]
core.area <- 0.126^2*pi/4

# calibration gas values
# 20-02-15 11:00
Cal2_CO2 <- 400.1
Cal2_N2O <- 403
Cal2_CH4 <- 4308
# Li840zero <- -55.37

# events times
time.origin <- as.POSIXct("2015-08-03 14:00:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
time.water <- as.POSIXct("2015-08-11 16:03:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
time.manure <- as.POSIXct("2015-08-31 14:55:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")



# hardcoding of N2O slope
myN2O.slope <- 0.97 # having a look to Ruth_cal_compilation.png; might be better than interpolation, due to PICARRO lack of precision


#1)   set common target input folder and target output folder (Use /, not \).

input_path <- "../man/RAW data/Ruth"

output_path <- "../OUTPUTexample/Ruth"
dir.create(output_path, recursive = T)


#2)   set desire date and time format (date_format) and date origin (date_origin) (IDASW and PICARRO)
#     see (http://www.stat.berkeley.edu/classes/s133/dates.html) for date formats
date_format<-"%Y-%m-%d %H:%M:%S"
date_origin<-"1970-01-01"
#     UTC (Coordinated Universal Time) offset of the winter-time of our time zone
#     data must be without DST(day saving time ) transition (e.g. always winter-time)
UTC_offset <- 3600



#3)     set IDASw processing parameters

#       set IDAsw input / output folders
#       CAUTION!!! --> alphabetical order of input file names has to match chronological order
IDASw_input_folder <- paste0(input_path, "/", incubation, "_IDASw")

IDASw_output_folder <- paste0(output_path, "/", incubation, "_IDASw")
# IDASw_output_folder <- paste0(output_path, "/", incubation, "_IDASw_extra")

#       set the expected maximum number of rows of IDASw Raw files
IDASw_nrow <- 42000

#       set any expected column names
channel_names <- c("C1001", "C1002", "C1010", "C1011", "C1020", "C1030", "C1101", "C1102",
                   "C1201", "C1202", "C1203", "C1204", "C1205", "C1206", "C1207", "C1208", "C1209", "C1210",
                   "C2001", "C2002", "C2003", "C2004", "C2005", "C2006",
                   "C4001", "C4002", "C4011", "C4012", "C5001")

# c("C1001-RH", "C1002-T", "C1010-FLOW", "C1011-FLOW1", "C1020-EXHAUST", "C1030-NH3_FLOW(same as p_flow)", "C1101-LICOR7_H2O_DAC2", "C1102-LICOR9_CO2_DAC1",
#   valve position(C1201:C1209; 7060_1:7060_09)--> "C1201", "C1202", "C1203", "C1204", "C1205", "C1206", "C1207", "C1208", "C1209",
#   "C2001-CLD_NO", "C2002-CLD_NO360", "C2003-CLD_Temp", "C2004-CLD_Press", "C2005-CLD_warning", "C2006-CLD_error",
#   "C4001-CORE", "C4011-CORE_1", "C4012-CORE_2", "C5001-NH3_CORE")

# bin_INC1                              bin_INC2
# 0-0-0-0000-0000                       0-0-00-0000-0000
# c8_cal1, c6_AIR1, c9, c2, c1          c5_cal2, c6_AIR2, c5_1718, c4, c3       --> see 'binary.R'

#4)     set QCL processing parameters (in fact PICARRO instrument)

#       set QCL input / output folders
# QCL_input_folder <- paste0(input_path, "/", incubation, "_QCL", "/", "str_files")
# QCL_output_folder <- paste0(output_path, "/", incubation, "_QCL")



# select column number for PICARRO N2O proccessing
N2O_column <- 24 #22(1s); 24(60s)
if(N2O_column==24){
        PIC_input_folder <- paste0(input_path, "/Ruth_PIC/to_run")
        PIC_output_folder <- paste0(output_path, "/", incubation, "_PIC_N2O_dry1min")
        IDASw_output_folder <- paste0(IDASw_output_folder,"_N2O_dry1min")
}else{
        PIC_input_folder <- paste0(input_path, "/Ruth_PIC/to_run")
        PIC_output_folder <- paste0(output_path, "/", incubation, "_PIC")
}


#       set date origin (date_origin)
QCL_date_origin <- "1904-01-01"

#       set average window in seconds
aw<-20

#       set midnight-syncronized time interval in seconds (i.e. evrey 6 minutes from midnight is 360)
#       (sync from 15 to 3600 seconds)
sync<-60

#       set time delay for the sample to reach the QCL (40s)
delay<-40 #not used (delay is around 5 s)

# set the expected maximum number of rows of QCL Raw files
# (and number of real seconds of QCL Raw files)
# QCL_nrow <- 86500
# QCLfileseconds <- 86400

fileseconds <- 3600

# #5)     path to PICARRO60 files (not used)
# PIC_input_folder <- paste0(input_path, "/PICARRO")
# lastPICARRO <- as.POSIXct("2014-03-25 10:38:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC") # last PICARRO value
# # this files have already been processed from raw PICARRO data. They contain values from both C and B incubations


require(data.table)
require(lattice)
require(ggplot2)
require(reshape2)
require(flux)
require(zoo)
