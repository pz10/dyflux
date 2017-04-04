# bin_TANK will be set to #1 (OPEN) everytime:
# 1. QCL fail (start and end, no matter which laser is the failing one)
# 2. TANk valve is open manually
# 3. IDASW starts or ends (sometimes end is not recorded)
# This will mean more TANKcodes than actually occuring, but we are on the safe side.

################################################################################
# extra files paths: 
extra_input_folder <- paste0(input_path, "/", incubation, "_extra")

IDASwSTART <- paste0(extra_input_folder, "/", incubation, "_IDASw_start.dat")

# read data
IDASwSTART <- fread(input = IDASwSTART, header=T)
IDASwSTART[,start:= as.POSIXct(start, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
IDASwSTART[,end:= as.POSIXct(end, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]

# perform bin_TANK modification (#0 to #1) on selected times (data0 aux data.table is used)
# We need two #1 on a row so that it is flag as a new TANKcode
data0 <- copy(data[, list(time, bin_TANK, bin_INC_valve)])

# failures <- c(QCLfailure[,start], QCLfailure[,end])
# failures <- c(failures, failures + 60)

# manualTANK <- c(manualTANKfilling[,start])
# manualTANK <- c(manualTANK, manualTANK + 60)

startIDASw <- c(IDASwSTART[,start])
startIDASw <- c(startIDASw, startIDASw + 60)

#
# data0[time %in% failures, bin_TANK:= 1]
# data0[time %in% manualTANK, bin_TANK:= 1]
data0[time %in% startIDASw, bin_TANK:= 1]
