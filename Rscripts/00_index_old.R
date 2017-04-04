# .libPaths(c(.libPaths(), "C:/Users/zuazo-p/Documents/R/win-library/3.0"))

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


# 1
# transformation of IDASw files to wide format files (i.e. one time per row, channels at columns)
# output written to the following folder: --> *_incubation_IDASw_daily
source("01_IDASw_wide.R")

# 2
# merge all 60syncronized values from *_incubation_IDASw_daily to a single file
# it also creates binary codes of valve position to identify which core was being measured
# output written to the following folder:       --> *_incubation_IDASw_60
source("02_IDASw_wide_60.R")

# 3
# Proccess of PIC raw data
# it will merge all "sync"-midnight-syncronized values to a single file
# (values == average, standard deviation and range of an average window of "aw"seconds length at "sync"-syncronized times)
# output written to the following folder:       --> "incubation"_PIC (e.g. Ruth_PIC)
source("03_PICARRO60_sync.R")

# 4
# merge of IDASw60 data with PIC data
# to be used over outputs of "IDASw_wide_60.R" and "PICARRO60_sync.R"
# output written to the following folder:       --> *_full_60 (e.g. *_full_60)
#                                               --> no folder:  (*_CAL*.dat)
#                                                               (*_TANK*.dat)
#                                                               (*_TANK_noncorrected.png) plots
# *it will merge IDASw60 data with PIC data at IDASw data times
# *it also calculates, exports and plots raw TANK values (value of concentration inside the Tank for every TANK code)
# *it also export cal1 and cal2 calibration values (*_CAL*.dat)
source("04_fullsync.R")

# 5
# to be used over outputs of "fullsync.R" located at *_incubation_ful_60 folder/value60_IDASw_*
# output written to the following folder:       --> *_incubation_full_60_slopes (e.g. A_incubation_full_60_slopes)
#                                               --> *_calibration_plots (e.g. A_calibration_plots)
# *It calculates slope and offset for correcting concentrations (from calibration gases)
#  i.e.:("Li840slopeRTO_CO2", "Li840offsetRTO_CO2", "QCLslopeCO2", "QCLslopeN2O", "QCLslopeCH4", "NOslopeRTO", "NOoffsetRTO")
# *It also export graphs into *_calibration_plots folder
# *It also calculates MEANflow and MEANp_flow from flow and p_flow values
#  (mean flow of [0:5] minutes before the 6-minutes-midnight-syncronized values)
source("05_calibration_correction.R")

# 6
# to be used over outputs of "calibration_correction.R" located at *_incubation_ful_60_slopes
# output written to the following folder:       --> *_incubation_full_60_corrected (e.g. A_incubation_full_60_corrected)
#                                               --> no folder:  (*_incubation_TANK_corrected.dat)
#                                                               (*_incubation_TANK_corrected.png) plots
# *it corrects concentration values taking into account slope and offset calculated in "calibration_correction.R
# *it also recalculates, exports and plots TANK values using recalculated concentrations
source("06_full60corrected.R")

# 7
# to be used over outputs of "ful60corrected.R" located at *_incubation_full_60_corrected
# output written to the following folder:       --> *_incubation_filtered_360_corrected (e.g. A_incubation_filtered_360_corrected)

source("07_filtered360corrected.R")

# 8 to be run manually
# source("08_final_plots.R")

# 9
# source("09_acid_trap.R")

################################################################################

# used R files:

# 1
# IDASw_wide.R

# 2
# IDASw_wide_60.R
#         binary.R

# 3
# PICARRO60_sync.R

# 4
# fullsync.R
#       TANKcutsINSERT.R
#       TANK.R
#               core_filtration.R (for 'IDASw_filter1000' function)
#               copyTANK_allvalues.R
#               TANK_notvalid_removal.R (set quality thersholds for TANK values)
#               copyTANK_allvalues.R (rewriting)
#               copyTANKsd.R
#       calibration.R
#               cal1.R


# 5
# calibration_correction.R
#         cal2correction.R
#         cal1correction.R
#               cal1.summary.R
#               cal1plots.R
#         MeanFlows.R

# 6
# full60corrected.R
#         TANK.R
#               core_filtration.R (for 'IDASw_filter1000' function)
#               copyTANK_allvalues.R
#               TANK_notvalid_removal.R (set quality thersholds for TANK values)
#               copyTANK_allvalues.R (rewriting)
#               copyTANKsd.R

# 7
# filtered360corrected.R
#         core_filtration.R
#         flux.R


# 8
# final_plots.R
#       solve_core_translocation.R
#       add_treatment_columns.R
#       times_rain_events.R
#       (final_plots_flowparameters.R)
#       (final_plots_deviceparameters.R)
#       parameter_thresholds.R

#       (final_plots2ggplot.R)
#       (final_plots_byTreatment.R)

#       (add_water_content.R)
#       (cum_fluxes.R)
#       (cum_fluxes_byspecies.R)
#       (flux_wide_byspecies.R)
#       (final_plots_byTreatment_summaries.R)
#       (final_CUMplots_byTreatment_summaries.R)
#       (final_plots_byTreatment_summaries_2treat.R)
#       (final_CUMplots_byTreatment_summaries_2treat.R)





# 9
# acid_trap.R

#       acid_trap_estimation.R
#               -> acid_trap_estimation.dat
#               -> acid_trap_estimation_summary.dat

#       acid_trap_calculation.R


# airNH3_vs_preN3.R
