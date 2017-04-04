### threshold file
threshold.file <- paste0(input_path, "/",incubation, "_extra/incubation_thresholds.dat")
thr <- fread(input = threshold.file)
to.delete <- grep(incubation, names(thr))
to.delete <- c(1,2,3,to.delete)
to.delete <- names(thr)[-to.delete]

set(thr, j = to.delete, value=NULL)
setnames(thr, names(thr), c("target", "order", "parameter", "min", "max"))

min <- thr[!is.na(min), list(target, parameter, min)]
max <- thr[!is.na(max), list(target, parameter, max)]

# get previous value for QC report file
fisrt.date <- data$time[1]
fisrt.date <- substring(as.character(fisrt.date),1,19)
fisrt.date <- paste0(fisrt.date, "  local winter time")
last.date <- tail(data$time,1)
last.date <- substring(as.character(last.date),1,19)
last.date <- paste0(last.date, "  local winter time")
records <- nrow(data)

myraw <- data[,.(
        raw.NO = sum(!is.na(NO)),
        raw.N2O = sum(!is.na(N2O)),
        raw.NH3 = sum(!is.na(NH3)),
        
        raw.CO2 = sum(!is.na(CO2)),
        raw.CH4 = sum(!is.na(CH4))
), by=CORE]
################################################################################
#global flow values
check <- nrow(data)

min.global <- min[target=="GLOBAL" | target=="variable2",]
if(nrow(min.global) > 0){
        
        for(i in 1:nrow(min.global)){
                data <- data[get(min.global$parameter[i]) > min.global$min[i],]
        }
}
###
max.global <- max[target=="GLOBAL" | target=="variable2",]
if(nrow(max.global) > 0){
        
        for(i in 1:nrow(max.global)){
                data <- data[get(max.global$parameter[i]) < max.global$max[i],]
        }
}
check1 <- nrow(data)
print(paste( format(100*(1-check1/check),digits=2) , " % of values deleted due to flow-related problems"))
my.flow <- paste( format(100*(1-check1/check),digits=2) , " % of values deleted due to flow-related problems")

################################################################################
# CLD
check <- sum(!is.na(data$NO))

mytarget <- c("NO", "mNO")
###
min.CLD <- min[target=="CLD",]
if(nrow(min.CLD) > 0){
        
        for(i in 1:nrow(min.CLD)){
                data[get(min.CLD$parameter[i]) < min.CLD$min[i], I(mytarget):=NA]
        }
}
###
max.CLD <- max[target=="CLD",]
if(nrow(max.CLD) > 0){
        
        for(i in 1:nrow(max.CLD)){
                data <- data[get(max.CLD$parameter[i]) > max.CLD$max[i], I(mytarget):=NA]
        }
}
check1 <- sum(!is.na(data$NO))
print(paste( format(100*(1-check1/check),digits=2) , " % of NO values deleted due to CLD malfunction"))
my.NO <- paste( format(100*(1-check1/check),digits=2) , " % of NO values deleted due to CLD malfunction")
################################################################################
################################################################################
# QCLN2O
check <- sum(!is.na(data$N2O))
mytarget <- c("N2O", "mN2O")
###
max.QCLN2O <- max[target=="QCLN2O",]
if(nrow(max.QCLN2O) > 0){
        
        for(i in 1:nrow(max.QCLN2O)){
                data <- data[get(max.QCLN2O$parameter[i]) > max.QCLN2O$max[i], I(mytarget):=NA]
        }
}
check1 <- sum(!is.na(data$N2O))
print(paste( format(100*(1-check1/check),digits=2) , " % of PICARRO-N2O values deleted due to funny N2O rg or sd"))
my.N2O <- paste( format(100*(1-check1/check),digits=2) , " % of PICARRO-N2O values deleted due to funny N2O rg or sd")
##################################
# QCLCO2
check <- sum(!is.na(data$CO2))

mytarget <- c("CO2", "mCO2")
###
max.QCLCO2 <- max[target=="QCLCO2",]
if(nrow(max.QCLCO2) > 0){
        
        for(i in 1:nrow(max.QCLCO2)){
                data <- data[get(max.QCLCO2$parameter[i]) > max.QCLCO2$max[i], I(mytarget):=NA]
        }
}
check1 <- sum(!is.na(data$CO2))
print(paste( format(100*(1-check1/check),digits=2) , " % of PICARRO-CO2 values deleted due to funny CO2 rg or sd"))
my.CO2 <- paste( format(100*(1-check1/check),digits=2) , " % of PICARRO-CO2 values deleted due to funny CO2 rg or sd")
##################################
# QCLCH4
check <- sum(!is.na(data$CH4))

mytarget <- c("CH4", "mCH4")
###
max.QCLCH4 <- max[target=="QCLCH4",]
if(nrow(max.QCLCH4) > 0){
        
        for(i in 1:nrow(max.QCLCH4)){
                data <- data[get(max.QCLCH4$parameter[i]) > max.QCLCH4$max[i], I(mytarget):=NA]
        }
}
check1 <- sum(!is.na(data$CH4))
print(paste( format(100*(1-check1/check),digits=2) , " % of PICARRO-CH4 values deleted due to funny CH4 rg or sd"))
my.CH4 <- paste( format(100*(1-check1/check),digits=2) , " % of PICARRO-CH4 values deleted due to funny CH4 rg or sd")
################################################################################

# export summary of Quality control
myfiltered <- data[,.(
        valid.NO = sum(!is.na(NO)),
        valid.N2O = sum(!is.na(N2O)),
        valid.NH3 = sum(!is.na(NH3)),
        
        valid.CO2 = sum(!is.na(CO2)),
        valid.CH4 = sum(!is.na(CH4))
), by=CORE]

setkey(myraw, CORE)
setkey(myfiltered, CORE)
mysummary <- myraw[myfiltered]
mycolorder <- c(grep("NO", names(mysummary), value=T),
                grep("N2O", names(mysummary), value=T),
                grep("NH3", names(mysummary), value=T),
                grep("CO2", names(mysummary), value=T),
                grep("CH4", names(mysummary), value=T)
                )
setcolorder(mysummary, c("CORE", mycolorder))
mysummary <- list(
        first.record = fisrt.date,
        last.record = last.date,
        total.records = records,
        record.by.CORE = mysummary
#         flow.QC = paste(my.flow, ";  NOTE --> next summaries are from valid values"),
#         NO.QC = paste(my.NO),
#         N2O.QC = paste(my.N2O),
#         CO2.QC = paste(my.CO2),
#         CH4.QC = paste(my.CH4)
        
        
)
# write output
sink(file= QC.file)

        print(mysummary)
        
        print("filtration summary:")
        print(my.flow)
        
        print("     from flow-valid values:")
        print(my.NO)
        print(my.N2O)
        print(my.CO2)
        print(my.CH4)
        
unlink(QC.file)
sink()



print(  paste0( Sys.time(), "  QC report file was written to:  ")  )
print(fileout)