IDASw_filter1000 <- function(data, delete_minutes_after1000 = 10, date_origin = "1970-01-01" , date_format= "%Y-%m-%d %H:%M:%S"){
        #returns logical vector (length as data columns) --> TRUE:CORE#1000; FALSE: rest of values
        time_1000 <- as.vector( data[bin_WORK == 1, epoch_time] )
        time_1000_extended <- time_1000
        
        for (i in 1:delete_minutes_after1000){
                extend <- time_1000 + 60*i
                time_1000_extended <- c(time_1000_extended, extend)
        }
        unique1000 <- sort(unique(time_1000_extended), na.last = FALSE)
        filter1000 <- format(as.POSIXct(unique1000, origin = date_origin),format=date_format)
        filter1000 <- as.POSIXct(filter1000, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
        
        # generate a logical vector (TRUE:CORE#1000; FALSE: rest of values)
        logical_1000 <- data[,time] %in% filter1000
        logical_1000
}




filterData <- function(data) {
        #source("IDASw_filter1000.R")  
        is1000 <- IDASw_filter1000(data)
        
        is360 <- floor(data[,"epoch_time"])%%360==0
        filter <- (!is1000) & (is360)
        
        return(filter)
}





# return a list with logical vectors for the occurrence of each core
core_filtration <- function(data){
        #   INC VALVEs POSITION
        isINC1 <- data[,bin_INC_valve]== "0001"
        isINC2 <- data[,bin_INC_valve]== "0010"
        
        #   INCUBATOR 1 active valve
        isCORE_1 <- data[,bin_INC1]== "0-0-0-0000-0001"
        isCORE_2 <- data[,bin_INC1]== "0-0-0-0000-0010"
        isCORE_3 <- data[,bin_INC1]== "0-0-0-0000-0100"
        isCORE_4 <- data[,bin_INC1]== "0-0-0-0000-1000"
        isCORE_5 <- data[,bin_INC1]== "0-0-0-0001-0000"
        isCORE_6 <- data[,bin_INC1]== "0-0-0-0010-0000"
        isCORE_7 <- data[,bin_INC1]== "0-0-0-0100-0000"
        isCORE_8 <- data[,bin_INC1]== "0-0-0-1000-0000"
        isCORE_9 <- data[,bin_INC1]== "0-0-1-0000-0000"
        isAIR1 <- data[,bin_INC1]== "0-1-0-0000-0000"
        
        #   INCUBATOR 2 active valve
        isCORE_10 <- data[,bin_INC2]== "0-0-00-0000-0010"
        isCORE_11 <- data[,bin_INC2]== "0-0-00-0000-0100"
        isCORE_12 <- data[,bin_INC2]== "0-0-00-0000-1000"
        isCORE_13 <- data[,bin_INC2]== "0-0-00-0001-0000"
        isCORE_14 <- data[,bin_INC2]== "0-0-00-0010-0000"
        isCORE_15 <- data[,bin_INC2]== "0-0-00-0100-0000"
        isCORE_16 <- data[,bin_INC2]== "0-0-00-1000-0000"
        isCORE_17 <- data[,bin_INC2]== "0-0-01-0000-0000"
        isCORE_18 <- data[,bin_INC2]== "0-0-10-0000-0000"
        isAIR2 <- data[,bin_INC2]== "0-1-00-0000-0000"
        isCAL2 <- data[,bin_INC2]== "1-0-00-0000-0000"
        
        
        ##############
        #QCL sampling conditions
        QCL1 <- (isINC2) & (isCORE_1)
        QCL2 <- (isINC2) & (isCORE_2)
        QCL3 <- (isINC2) & (isCORE_3)
        QCL4 <- (isINC2) & (isCORE_4)
        QCL5 <- (isINC2) & (isCORE_5)
        QCL6 <- (isINC2) & (isCORE_6)
        QCL7 <- (isINC2) & (isCORE_7)
        QCL8 <- (isINC2) & (isCORE_8)
        QCL9 <- (isINC2) & (isCORE_9)
        
        QCL10 <- (isINC1) & (isCORE_10)
        QCL11 <- (isINC1) & (isCORE_11)
        QCL12 <- (isINC1) & (isCORE_12)
        QCL13 <- (isINC1) & (isCORE_13)
        QCL14 <- (isINC1) & (isCORE_14)
        QCL15 <- (isINC1) & (isCORE_15)
        QCL16 <- (isINC1) & (isCORE_16)
        QCL17 <- (isINC1) & (isCORE_17)
        QCL18 <- (isINC1) & (isCORE_18)
        
        QCLair1 <- (!isINC1) & (isINC2) & (isAIR1)
        QCLair2 <- (isINC1) & (!isINC2) & (isAIR2)
        QCLair <- (QCLair1) | (QCLair2)
        
        QCLCAL2 <- (isINC1) & (isCAL2)
        
        ##############
        #PICARRO sampling conditions
        PICARRO1 <- (isINC1) & (isCORE_1)
        PICARRO2 <- (isINC1) & (isCORE_2)
        PICARRO3 <- (isINC1) & (isCORE_3)
        PICARRO4 <- (isINC1) & (isCORE_4)
        PICARRO5 <- (isINC1) & (isCORE_5)
        PICARRO6 <- (isINC1) & (isCORE_6)
        PICARRO7 <- (isINC1) & (isCORE_7)
        PICARRO8 <- (isINC1) & (isCORE_8)
        PICARRO9 <- (isINC1) & (isCORE_9)
        
        PICARRO10 <- (isINC2) & (isCORE_10)
        PICARRO11 <- (isINC2) & (isCORE_11)
        PICARRO12 <- (isINC2) & (isCORE_12)
        PICARRO13 <- (isINC2) & (isCORE_13)
        PICARRO14 <- (isINC2) & (isCORE_14)
        PICARRO15 <- (isINC2) & (isCORE_15)
        PICARRO16 <- (isINC2) & (isCORE_16)
        PICARRO17 <- (isINC2) & (isCORE_17)
        PICARRO18 <- (isINC2) & (isCORE_18)
        
        PICARROair1 <- (isINC1) & (!isINC2) & (isAIR1)
        PICARROair2 <- (!isINC1) & (isINC2) & (isAIR2)
        PICARROair <- (PICARROair1) | (PICARROair2)
        
        PICARROCAL2 <- QCLCAL2
        ####################
        
        
        
        ################
        QCL <- list(QCL_CORE_01 = QCL1, QCL_CORE_02 = QCL2, QCL_CORE_03 = QCL3, QCL_CORE_04 = QCL4,
                    QCL_CORE_05 = QCL5, QCL_CORE_06 = QCL6, QCL_CORE_07 = QCL7, QCL_CORE_08 = QCL8,
                    QCL_CORE_09 = QCL9, QCL_CORE_10 = QCL10, QCL_CORE_11 = QCL11, QCL_CORE_12 = QCL12,
                    QCL_CORE_13 = QCL13, QCL_CORE_14 = QCL14, QCL_CORE_15 = QCL15, QCL_CORE_16 = QCL16,
                    QCL_CORE_17 = QCL17, QCL_CORE_18 = QCL18, QCL_CORE_19_air = QCLair, QCL_CORE_20_CAL2 = QCLCAL2)
        PICARRO <- list(PICARRO_CORE_01 = PICARRO1, PICARRO_CORE_02 = PICARRO2, PICARRO_CORE_03 = PICARRO3,
                        PICARRO_CORE_04 = PICARRO4, PICARRO_CORE_05 = PICARRO5, PICARRO_CORE_06 = PICARRO6,
                        PICARRO_CORE_07 = PICARRO7, PICARRO_CORE_08 = PICARRO8, PICARRO_CORE_09 = PICARRO9,
                        PICARRO_CORE_10 = PICARRO10, PICARRO_CORE_11 = PICARRO11, PICARRO_CORE_12 = PICARRO12,
                        PICARRO_CORE_13 = PICARRO13, PICARRO_CORE_14 = PICARRO14, PICARRO_CORE_15 = PICARRO15,
                        PICARRO_CORE_16 = PICARRO16, PICARRO_CORE_17 = PICARRO17, PICARRO_CORE_18 = PICARRO18,
                        PICARRO_CORE_19_air = PICARROair, PICARRO_CORE_20_CAL2 = PICARROCAL2)
        core_list<- list (QCL= QCL, PICARRO = PICARRO)
        
        core_list
}

# example: data[core_list$QCL$QCL_CORE_01, CORE :="QCL_CORE_01"]


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
###original
# return a list with logical vectors for the occurrence of each core
core_filtration_DF <- function(data360){
        #   INC VALVEs POSITION
        isINC1 <- data360[,"bin_INC_valve"]== "0001"
        isINC2 <- data360[,"bin_INC_valve"]== "0010"
        
        #   INCUBATOR 1 active valve
        isCORE_1 <- data360[,"bin_INC1"]== "0-0-0-0000-0001"
        isCORE_2 <- data360[,"bin_INC1"]== "0-0-0-0000-0010"
        isCORE_3 <- data360[,"bin_INC1"]== "0-0-0-0000-0100"
        isCORE_4 <- data360[,"bin_INC1"]== "0-0-0-0000-1000"
        isCORE_5 <- data360[,"bin_INC1"]== "0-0-0-0001-0000"
        isCORE_6 <- data360[,"bin_INC1"]== "0-0-0-0010-0000"
        isCORE_7 <- data360[,"bin_INC1"]== "0-0-0-0100-0000"
        isCORE_8 <- data360[,"bin_INC1"]== "0-0-0-1000-0000"
        isCORE_9 <- data360[,"bin_INC1"]== "0-0-1-0000-0000"
        isAIR1 <- data360[,"bin_INC1"]== "0-1-0-0000-0000"
        
        #   INCUBATOR 2 active valve
        isCORE_10 <- data360[,"bin_INC2"]== "0-0-00-0000-0010"
        isCORE_11 <- data360[,"bin_INC2"]== "0-0-00-0000-0100"
        isCORE_12 <- data360[,"bin_INC2"]== "0-0-00-0000-1000"
        isCORE_13 <- data360[,"bin_INC2"]== "0-0-00-0001-0000"
        isCORE_14 <- data360[,"bin_INC2"]== "0-0-00-0010-0000"
        isCORE_15 <- data360[,"bin_INC2"]== "0-0-00-0100-0000"
        isCORE_16 <- data360[,"bin_INC2"]== "0-0-00-1000-0000"
        isCORE_17 <- data360[,"bin_INC2"]== "0-0-01-0000-0000"
        isCORE_18 <- data360[,"bin_INC2"]== "0-0-10-0000-0000"
        isAIR2 <- data360[,"bin_INC2"]== "0-1-00-0000-0000"
        isCAL2 <- data360[,"bin_INC2"]== "1-0-00-0000-0000"
        
        
        ##############
        #QCL sampling conditions
        QCL1 <- (isINC2) & (isCORE_1)
        QCL2 <- (isINC2) & (isCORE_2)
        QCL3 <- (isINC2) & (isCORE_3)
        QCL4 <- (isINC2) & (isCORE_4)
        QCL5 <- (isINC2) & (isCORE_5)
        QCL6 <- (isINC2) & (isCORE_6)
        QCL7 <- (isINC2) & (isCORE_7)
        QCL8 <- (isINC2) & (isCORE_8)
        QCL9 <- (isINC2) & (isCORE_9)
        
        QCL10 <- (isINC1) & (isCORE_10)
        QCL11 <- (isINC1) & (isCORE_11)
        QCL12 <- (isINC1) & (isCORE_12)
        QCL13 <- (isINC1) & (isCORE_13)
        QCL14 <- (isINC1) & (isCORE_14)
        QCL15 <- (isINC1) & (isCORE_15)
        QCL16 <- (isINC1) & (isCORE_16)
        QCL17 <- (isINC1) & (isCORE_17)
        QCL18 <- (isINC1) & (isCORE_18)
        
        QCLair1 <- (!isINC1) & (isINC2) & (isAIR1)
        QCLair2 <- (isINC1) & (!isINC2) & (isAIR2)
        QCLair <- (QCLair1) | (QCLair2)
        
        QCLCAL2 <- (isINC1) & (isCAL2)
        
        ##############
        #PICARRO sampling conditions
        PICARRO1 <- (isINC1) & (isCORE_1)
        PICARRO2 <- (isINC1) & (isCORE_2)
        PICARRO3 <- (isINC1) & (isCORE_3)
        PICARRO4 <- (isINC1) & (isCORE_4)
        PICARRO5 <- (isINC1) & (isCORE_5)
        PICARRO6 <- (isINC1) & (isCORE_6)
        PICARRO7 <- (isINC1) & (isCORE_7)
        PICARRO8 <- (isINC1) & (isCORE_8)
        PICARRO9 <- (isINC1) & (isCORE_9)
        
        PICARRO10 <- (isINC2) & (isCORE_10)
        PICARRO11 <- (isINC2) & (isCORE_11)
        PICARRO12 <- (isINC2) & (isCORE_12)
        PICARRO13 <- (isINC2) & (isCORE_13)
        PICARRO14 <- (isINC2) & (isCORE_14)
        PICARRO15 <- (isINC2) & (isCORE_15)
        PICARRO16 <- (isINC2) & (isCORE_16)
        PICARRO17 <- (isINC2) & (isCORE_17)
        PICARRO18 <- (isINC2) & (isCORE_18)
        
        PICARROair1 <- (isINC1) & (!isINC2) & (isAIR1)
        PICARROair2 <- (!isINC1) & (isINC2) & (isAIR2)
        PICARROair <- (PICARROair1) | (PICARROair2)
        
        PICARROCAL2 <- QCLCAL2
        ####################
        
        
        
        ################
        QCL <- list(QCL_CORE_01 = QCL1, QCL_CORE_02 = QCL2, QCL_CORE_03 = QCL3, QCL_CORE_04 = QCL4,
                    QCL_CORE_05 = QCL5, QCL_CORE_06 = QCL6, QCL_CORE_07 = QCL7, QCL_CORE_08 = QCL8,
                    QCL_CORE_09 = QCL9, QCL_CORE_10 = QCL10, QCL_CORE_11 = QCL11, QCL_CORE_12 = QCL12,
                    QCL_CORE_13 = QCL13, QCL_CORE_14 = QCL14, QCL_CORE_15 = QCL15, QCL_CORE_16 = QCL16,
                    QCL_CORE_17 = QCL17, QCL_CORE_18 = QCL18, QCL_CORE_19_air = QCLair, QCL_CORE_20_CAL2 = QCLCAL2)
        PICARRO <- list(PICARRO_CORE_01 = PICARRO1, PICARRO_CORE_02 = PICARRO2, PICARRO_CORE_03 = PICARRO3,
                        PICARRO_CORE_04 = PICARRO4, PICARRO_CORE_05 = PICARRO5, PICARRO_CORE_06 = PICARRO6,
                        PICARRO_CORE_07 = PICARRO7, PICARRO_CORE_08 = PICARRO8, PICARRO_CORE_09 = PICARRO9,
                        PICARRO_CORE_10 = PICARRO10, PICARRO_CORE_11 = PICARRO11, PICARRO_CORE_12 = PICARRO12,
                        PICARRO_CORE_13 = PICARRO13, PICARRO_CORE_14 = PICARRO14, PICARRO_CORE_15 = PICARRO15,
                        PICARRO_CORE_16 = PICARRO16, PICARRO_CORE_17 = PICARRO17, PICARRO_CORE_18 = PICARRO18,
                        PICARRO_CORE_19_air = PICARROair, PICARRO_CORE_20_CAL2 = PICARROCAL2)
        core_list<- list (QCL, PICARRO)
        
        core_list
}