# with 'data' creates 'TANK' (with TANK values) and 'dataTANK' (with TANK values written every minute) 
################################################################################


# load core_filtration.R to use 'IDASw_filter1000'function 
# (it consider a time buffer after 'bin_WORK'==1 so that data is not consider)
source("core_filtration.R")

# set target columns
ids <- c("CLD_NO", "N2O_ppb", "CO2_ppm", "CH4_ppb", "H2O_ppm", "NH3_ppb")

TANK_ids <- c("TANK_CLD_NO", "TANK_N2O_ppb", "TANK_CO2_ppm", "TANK_CH4_ppb", "TANK_H2O_ppm", "TANK_NH3_ppb")
TANK_ids_sd <- c("TANK_CLD_NO_sd", "TANK_N2O_ppb_sd", "TANK_CO2_ppm_sd", "TANK_CH4_ppb_sd", "TANK_H2O_ppm_sd", "TANK_NH3_ppb_sd")

# convert bin_INC_valve to character in the 0000 format
data[,bin_INC_valve:= formatC(bin_INC_valve, 3, flag=0)]
# create to delete columns with starting and ending TANk time
data[,  start:= min(time, na.rm = TRUE) , by = TANKcode]
data[,  end:= max(time, na.rm = TRUE) , by = TANKcode]

# create to delete column to check if recorded data was influenced by 'bin_WORK'==1 (after maintenance operations)
# it will be used to get rid of data recorded some minutes (delete_minutes_after1000) after 'bin_WORK'==1
setkey(data, epoch_time)
iswork <- IDASw_filter1000(data)
data[,iswork:= iswork]

# calculate median of each target species for each TANKcode (only at 360second midnight-syncronized values) storaged in 'TANK'
setkey(data, bin_INC1, bin_INC_valve)
AIR1 <- data[J("0-1-0-0000-0000","0010")]

setkey(data, bin_INC2, bin_INC_valve)
AIR2 <- data[J("0-1-00-0000-0000","0001")]

setkey(data, epoch_time)
set(data, j = c("start", "end"), value=NULL) #delete these created columns

# TANK <- rbindlist(list(AIR1,AIR2))
TANK <- rbind(AIR1, AIR2, use.names=TRUE)
set(data, j = "iswork", value=NULL) #delete iswork-created-column from 'data'
TANK <- TANK[iswork==FALSE,]
set(TANK, j = "iswork", value=NULL)
TANK[,is360:= epoch_time %% 360]
setkey(TANK,is360)
TANK <- TANK[J(0)]

source("copyTANK_allvalues.R")

# load thresholds values for considering a measurement invalid
source("TANK_notvalid_removal.R")
# get rid of extreme outliers, due to failure...
# QCL
TANK[N2O_ppb < 200 | N2O_ppb > 400, N2O_ppb:= NA]
TANK[CO2_ppm < 300 | CO2_ppm > 600, CO2_ppm:= NA]
TANK[CH4_ppb < 1800 | CH4_ppb > 2400, CH4_ppb:= NA]
#due to not enough flow
TANK[exhaust < min.exhaust & exhaust > max.exhaust, N2O_ppb:= NA]
TANK[exhaust < min.exhaust & exhaust > max.exhaust, CO2_ppm:= NA]
TANK[exhaust < min.exhaust & exhaust > max.exhaust, CH4_ppb:= NA]
TANK[exhaust < min.exhaust & exhaust > max.exhaust, CLD_NO:= NA]

TANK[flow < min.flow & flow > max.flow, N2O_ppb:= NA]
TANK[flow < min.flow & flow > max.flow, CO2_ppm:= NA]
TANK[flow < min.flow & flow > max.flow, CH4_ppb:= NA]
TANK[flow < min.flow & flow > max.flow, CLD_NO:= NA]

TANK[CLD_Temp < min.CLD_temp, CLD_NO:= NA]
TANK[CLD_Temp > max.CLD_temp, CLD_NO:= NA]

TANK[CLD_Press < min.CLD_Press, CLD_NO:= NA]
TANK[CLD_Press > max.CLD_Press, CLD_NO:= NA]


source("copyTANK_allvalues.R")

setkey(TANK, time)
setkey(TANK, TANKcode)

TANKsd <- copy(TANK)

TANK <- TANK[,  lapply(.SD, median ,na.rm = TRUE) , by = TANKcode, .SDcols = c(ids, "start", "end")]
setnames(TANK, c("TANKcode", ids, "start", "end"), c("TANKcode", TANK_ids, "start", "end"))
source("copyTANKsd.R")

# maxTANKcode <- max(max(TANK$TANKcode), max(PICARROTANK$TANKcode))
maxTANKcode <- max(TANK$TANKcode, na.rm=T)
allTANKcodes <- data.table(TANKcode = 1:maxTANKcode)


# TANK[, start:= NULL]
# TANK[, end:= NULL]
setkey(allTANKcodes, TANKcode)
setkey(TANK, TANKcode)
TANK <- TANK[allTANKcodes]
# TANK[!is.na(start.1), start:= start.1]
# TANK[!is.na(end.1), end:= end.1]
# TANK[,start.1:=NULL]
# TANK[,end.1:=NULL]

# TANK_ids <- c(TANK_ids, PICARROTANK_ids)

# addition of 'TANK' values to data (through intermediate 'dataTANK')
dataTANK <- data[, list(epoch_time, TANKcode)]
setkey(dataTANK, TANKcode)
setkey(TANK, TANKcode)

dataTANK <- TANK[dataTANK]
setkey(dataTANK, epoch_time)
set(dataTANK, j = c("start", "end"), value=NULL)

setkey(data, epoch_time)

# as data and dataTANk are keyed by epoch_time, we can just add TANK columns to data without problem
# I do it out of this file
