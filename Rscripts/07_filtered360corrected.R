# to be used over outputs of "ful60corrected.R" located at *_incubation_full_60_corrected
# output written to the following folder:       --> *_incubation_filtered_360_corrected (e.g. A_incubation_filtered_360_corrected)

#time recording
print(  paste0(Sys.time(), " start filtered360corrected.R")  )
###

###           create output folders
#(for value, max, min and stdev)
folder_out <- paste0(IDASw_output_folder, "/", incubation, "_filtered_360_corrected")
dir.create(folder_out, recursive = TRUE)

fileout <- paste0(folder_out, "/", incubation, "_filtered360_corrected", ".dat")


### input folders
folder_in <- paste0(IDASw_output_folder, "/", incubation, "_full_60_corrected")
files_in <- list.files(path = folder_in, full.names = TRUE)
files_in <- files_in[c(4,1:3)] #  "Value" on 1st position
treatment_file <- paste0(input_path, "/", incubation, "_extra/", incubation, "_treatments.dat")

# ### load treatments
# treatment <- fread(input = treatment_file)
# treatments <- treatment[,treatment]
# tillage <- treatment[,tillage]
# fertilization <- treatment[,fertilization]
# precipitation <- treatment[,precipitation]

# value file (1st file)
data <- fread(input = files_in[1])
setkey(data,epoch_time)
data[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]

# rest of files (2nd:4th files)
# I just append max, min and sd for exhaust (also Li840-max, min, sd-; and QCL-rg,sd)
# rest of parametes (max, min and sd values) are not used because:
# 1 - are recorded once per minute (CLD_*)
# 2 - variations within a minute are not relevant (RH, T)
# 3 - their fluctuation is consider with averages ("flow" and "MEANflow"; "p_flow" and "MEANp_flow")
data2 <- fread(input = files_in[2])
setkey(data2,epoch_time)
data[,exhaustMAX:= data2$exhaust]
data[,flowMAX:= data2$flow]
data[,rg_N2O_ppb:= data2$rg_N2O_ppb]
data[,rg_CO2_ppm:= data2$rg_CO2_ppm]
data[,rg_CH4_ppb:= data2$rg_CH4_ppb]
data[,rg_H2O_ppm:= data2$rg_H2O_ppm]
data[,rg_NH3_ppb:= data2$rg_NH3_ppb]


data2 <- fread(input = files_in[3])
setkey(data2,epoch_time)
data[,exhaustMIN:= data2$exhaust]
data[,flowMIN:= data2$flow]

data2 <- fread(input = files_in[4])
setkey(data2,epoch_time)
data[,exhaustSD:= data2$exhaust]
data[,flowSD:= data2$flow]
data[,sd_N2O_ppb:= data2$sd_N2O_ppb]
data[,sd_CO2_ppm:= data2$sd_CO2_ppm]
data[,sd_CH4_ppb:= data2$sd_CH4_ppb]
data[,sd_H2O_ppm:= data2$sd_H2O_ppm]
data[,sd_NH3_ppb:= data2$sd_NH3_ppb]

# load picarroCORE_filtration.R to use 'IDASw_filter1000'function 
# (it consider a time buffer after 'bin_WORK'==1 so that data is not consider)
source("core_filtration.R")

# select only valid values
# 1 - not influenced by 'bin_WORK'==1;
# 2 - only 360s-midnight-syncronized
data <- data[!IDASw_filter1000(data),]
data <- data[epoch_time%%360 == 0,]

# flux calculation
source("flux.R")

# unit: mass of element (i.e.: mg-C/m2/h; µg-C/m2/h; µg-N/m2/h)
data[,NO:= flux(data, compound = "NO", device = "QCL")] #device QCL, since it is sampling from main line
data[,N2O:= flux(data, compound = "N2O", device = "QCL")]
data[,CO2:= flux(data, compound = "CO2", device = "QCL")]
data[,CH4:= flux(data, compound = "CH4", device = "QCL")]
data[,NH3:= flux(data, compound = "NH3", device = "QCL")]
data[,H2O:= flux(data, compound = "H2O", device = "QCL")]

# unit: mass of molucule (i.e.: mg-CO2/m2/h; µg-NO/m2/h;; µg-N2O/m2/h µg-CH4/m2/h)
data[,mNO:= flux(data, compound = "NO", ofmolecule = TRUE, device = "QCL")]
data[,mN2O:= flux(data, compound = "N2O", ofmolecule = TRUE, device = "QCL")]
data[,mCO2:= flux(data, compound = "CO2", ofmolecule = TRUE, device = "QCL")]
data[,mCH4:= flux(data, compound = "CH4", ofmolecule = TRUE, device = "QCL")]
data[,mNH3:= flux(data, compound = "NH3", ofmolecule = TRUE, device = "QCL")]
data[,mH2O:= flux(data, compound = "H2O", ofmolecule = TRUE, device = "QCL")]


# assign core name
# convert bin_INC_valve to character in the 0000 format
data[,bin_INC_valve:= formatC(bin_INC_valve, 3, flag=0)]

# create logical vectors for labeling the cores using 'core_filtration' function (storaged in 'core_filtration.R')
core_list <- core_filtration(data)

# labeling of data : to.edit columns are created for QCL and PICARRO

### QCL
data[,CORE:= "no_QCLCORE"]
to.edit <- c("CORE")
# to.edit <- c("CORE", "treatment", "tillage", "fertilization", "precipitation")

data[core_list$QCL$QCL_CORE_01, CORE := "CORE_01"]
data[core_list$QCL$QCL_CORE_02, CORE := "CORE_02"]
data[core_list$QCL$QCL_CORE_03, CORE := "CORE_03"]
data[core_list$QCL$QCL_CORE_04, CORE := "CORE_04"]
data[core_list$QCL$QCL_CORE_05, CORE := "CORE_05"]
data[core_list$QCL$QCL_CORE_06, CORE := "CORE_06"]
data[core_list$QCL$QCL_CORE_07, CORE := "CORE_07"]
data[core_list$QCL$QCL_CORE_08, CORE := "CORE_08"]
data[core_list$QCL$QCL_CORE_09, CORE := "CORE_09"]
data[core_list$QCL$QCL_CORE_10, CORE := "CORE_10"]
data[core_list$QCL$QCL_CORE_11, CORE := "CORE_11"]
data[core_list$QCL$QCL_CORE_12, CORE := "CORE_12"]
data[core_list$QCL$QCL_CORE_13, CORE := "CORE_13"]
data[core_list$QCL$QCL_CORE_14, CORE := "CORE_14"]
data[core_list$QCL$QCL_CORE_15, CORE := "CORE_15"]
data[core_list$QCL$QCL_CORE_16, CORE := "CORE_16"]
data[core_list$QCL$QCL_CORE_17, CORE := "CORE_17"]
data[core_list$QCL$QCL_CORE_18, CORE := "CORE_18"]

# data[core_list$QCL$QCL_CORE_01, I(to.edit) := list("QCL_CORE_01", treatments[1], tillage[1], fertilization[1], precipitation[1])]
# data[core_list$QCL$QCL_CORE_02, I(to.edit) := list("QCL_CORE_02", treatments[2], tillage[2], fertilization[2], precipitation[2])]
# data[core_list$QCL$QCL_CORE_03, I(to.edit) := list("QCL_CORE_03", treatments[3], tillage[3], fertilization[3], precipitation[3])]
# data[core_list$QCL$QCL_CORE_04, I(to.edit) := list("QCL_CORE_04", treatments[4], tillage[4], fertilization[4], precipitation[4])]
# data[core_list$QCL$QCL_CORE_05, I(to.edit) := list("QCL_CORE_05", treatments[5], tillage[5], fertilization[5], precipitation[5])]
# data[core_list$QCL$QCL_CORE_06, I(to.edit) := list("QCL_CORE_06", treatments[6], tillage[6], fertilization[6], precipitation[6])]
# data[core_list$QCL$QCL_CORE_07, I(to.edit) := list("QCL_CORE_07", treatments[7], tillage[7], fertilization[7], precipitation[7])]
# data[core_list$QCL$QCL_CORE_08, I(to.edit) := list("QCL_CORE_08", treatments[8], tillage[8], fertilization[8], precipitation[8])]
# data[core_list$QCL$QCL_CORE_09, I(to.edit) := list("QCL_CORE_09", treatments[9], tillage[9], fertilization[9], precipitation[9])]
# data[core_list$QCL$QCL_CORE_10, I(to.edit) := list("QCL_CORE_10", treatments[10], tillage[10], fertilization[10], precipitation[10])]
# data[core_list$QCL$QCL_CORE_11, I(to.edit) := list("QCL_CORE_11", treatments[11], tillage[11], fertilization[11], precipitation[11])]
# data[core_list$QCL$QCL_CORE_12, I(to.edit) := list("QCL_CORE_12", treatments[12], tillage[12], fertilization[12], precipitation[12])]
# data[core_list$QCL$QCL_CORE_13, I(to.edit) := list("QCL_CORE_13", treatments[13], tillage[13], fertilization[13], precipitation[13])]
# data[core_list$QCL$QCL_CORE_14, I(to.edit) := list("QCL_CORE_14", treatments[14], tillage[14], fertilization[14], precipitation[14])]
# data[core_list$QCL$QCL_CORE_15, I(to.edit) := list("QCL_CORE_15", treatments[15], tillage[15], fertilization[15], precipitation[15])]
# data[core_list$QCL$QCL_CORE_16, I(to.edit) := list("QCL_CORE_16", treatments[16], tillage[16], fertilization[16], precipitation[16])]
# data[core_list$QCL$QCL_CORE_17, I(to.edit) := list("QCL_CORE_17", treatments[17], tillage[17], fertilization[17], precipitation[17])]
# data[core_list$QCL$QCL_CORE_18, I(to.edit) := list("QCL_CORE_18", treatments[18], tillage[18], fertilization[18], precipitation[18])]



# delete rows with no propper 'CORE' (i.e. AIR, WORk ...)
# data <- data[(!CORE=="no_QCLCORE") | (!picarroCORE=="no_PICARRO_core"),]
data <- data[!CORE=="no_QCLCORE",]

# column selection
parameters<- c("RH" ,"T" , "flow", "MEANflow", "flowMAX", "flowMIN", "flowSD", "exhaust", "exhaustMAX", "exhaustMIN", "exhaustSD",
               "p_flow", "MEANp_flow", "CLD_Temp", "CLD_Press", "CLD_warning", "CLD_error")

concentrations <- c("TANK_CLD_NO", "CLD_NO", "CLD_NO360",
                    "TANK_N2O_ppb", "N2O_ppb", "rg_N2O_ppb", "sd_N2O_ppb",
                    "TANK_CO2_ppm", "CO2_ppm", "rg_CO2_ppm", "sd_CO2_ppm",
                    "TANK_CH4_ppb", "CH4_ppb", "rg_CH4_ppb", "sd_CH4_ppb",
                    "TANK_NH3_ppb", "NH3_ppb", "rg_NH3_ppb", "sd_NH3_ppb",
                    "TANK_H2O_ppm", "H2O_ppm", "rg_H2O_ppm", "sd_H2O_ppm")

# fluxes <- c("NO", "N2O", "CO2", "CH4", "Li840CO2", "picarroN2O", "picarroCO2", "picarroCH4", "picarroNH3",
#             "mNO", "mN2O", "mCO2", "mCH4", "mLi840CO2", "picarro_mN2O", "picarro_mCO2", "picarro_mCH4", "picarro_mNH3")
fluxes <- c("NO", "N2O", "CO2", "CH4", "NH3", "H2O",
            "mNO", "mN2O", "mCO2", "mCH4", "mNH3", "mH2O")


final_order <- c("time", "epoch_time", "TANKcode", "CORE", parameters, concentrations, fluxes)


tokeep <- names(data) %in% final_order
todelete <- names(data)[!tokeep]
set(data, j = todelete, value=NULL)
setcolorder(data, final_order)

# ###add treatments columns
# 
# 
#         load0 <- c( "000T_con", "000NT_con", "000T_inc", "000NT_inc", "000T_dec", "000NT_dec" )
#         load50 <- c( "050T_con", "050NT_con", "050T_inc", "050NT_inc", "050T_dec", "050NT_dec" )
#         load100 <- c( "100T_con", "100NT_con", "100T_inc", "100NT_inc", "100T_dec", "100NT_dec" )
#         
#         constant <- c( "000T_con", "000NT_con", "050T_con", "050NT_con", "100T_con", "100NT_con" )
#         increasing <- c( "000T_inc", "000NT_inc", "050T_inc", "050NT_inc", "100T_inc", "100NT_inc" )
#         decreasing <- c( "000T_dec", "000NT_dec", "050T_dec", "050NT_dec", "100T_dec", "100NT_dec" )
#         
#         TT <- c( "000T_con", "000T_inc",  "000T_dec", "050T_con", "050T_inc", "050T_dec", "100T_con", "100T_inc", "100T_dec")
#         NT <- c( "000NT_con", "000NT_inc",  "000NT_dec", "050NT_con", "050NT_inc", "050NT_dec", "100NT_con", "100NT_inc", "100NT_dec")
#         
#         ### QCL
#         data[, fertilizer:= 999]
#         data[, precipitation:= "nodata"]
#         data[, tillage:= "nodata"]
#         
#         
#         
#         data[data$treatment %in% load0, fertilizer:= 0]
#         data[data$treatment %in% load50, fertilizer:= 50]
#         data[data$treatment %in% load100, fertilizer:= 100]
#         
#         data[data$treatment %in% constant, precipitation:= "cons"]
#         data[data$treatment %in% increasing, precipitation:= "incr"]
#         data[data$treatment %in% decreasing, precipitation:= "decr"]
#         
#         data[data$treatment %in% TT, tillage:= "T"]
#         data[data$treatment %in% NT, tillage:= "NT"]
# 
#         
#         ### PICARRO
#         data[, PICARROfertilizer:= 999]
#         data[, PICARROprecipitation:= "nodata"]
#         data[, PICARROtillage:= "nodata"]
#         
#         
#         
#         data[data$picarro_treatment %in% load0, PICARROfertilizer:= 0]
#         data[data$picarro_treatment %in% load50, PICARROfertilizer:= 50]
#         data[data$picarro_treatment %in% load100, PICARROfertilizer:= 100]
#         
#         data[data$picarro_treatment %in% constant, PICARROprecipitation:= "cons"]
#         data[data$picarro_treatment %in% increasing, PICARROprecipitation:= "incr"]
#         data[data$picarro_treatment %in% decreasing, PICARROprecipitation:= "decr"]
#         
#         data[data$picarro_treatment %in% TT, PICARROtillage:= "T"]
#         data[data$picarro_treatment %in% NT, PICARROtillage:= "NT"]
# 

# format columns
# no.format <- c("time", "epoch_time", "TANKcode", "QCLstretch", "CORE", "treatment", "picarroCORE", "picarro_treatment")
no.format <- c("time", "epoch_time", "TANKcode", "CORE")

no.format <- names(data) %in% no.format
to.format <- names(data)[!no.format]

data[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format]        

# chronological ordering of data and column reordering
setkey(data, epoch_time)

############################
#
#write into output folder
write.table(data, file= fileout, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")

print(  paste0( "1 of 1  ",Sys.time(), " file was written to:  ")  )
print(fileout)
print(  paste0(Sys.time(), " filtered360corrected.R is finnished")  )
