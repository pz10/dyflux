# ###add treatments columns

## load treatments
treatment <- fread(input = treatment_file)
treatments <- treatment[,treatment]

setkey(data, CORE)
setkey(treatment, CORE)
data <- treatment[data]
# 
# 
# 
# load0 <- c( "0T_con", "0NT_con", "0T_inc", "0NT_inc", "0T_dec", "0NT_dec" )
# load50 <- c( "50T_con", "50NT_con", "50T_inc", "50NT_inc", "50T_dec", "50NT_dec" )
# load100 <- c( "100T_con", "100NT_con", "100T_inc", "100NT_inc", "100T_dec", "100NT_dec" )
# 
# constant <- c( "0T_con", "0NT_con", "50T_con", "50NT_con", "100T_con", "100NT_con" )
# increasing <- c( "0T_inc", "0NT_inc", "50T_inc", "50NT_inc", "100T_inc", "100NT_inc" )
# decreasing <- c( "0T_dec", "0NT_dec", "50T_dec", "50NT_dec", "100T_dec", "100NT_dec" )
# 
# TT <- c( "0T_con", "0T_inc",  "0T_dec", "50T_con", "50T_inc", "50T_dec", "100T_con", "100T_inc", "100T_dec")
# NT <- c( "0NT_con", "0NT_inc",  "0NT_dec", "50NT_con", "50NT_inc", "50NT_dec", "100NT_con", "100NT_inc", "100NT_dec")
# 
# ### QCL
# data[, fertilizer:= 999]
# data[, precipitation:= "nodata"]
# data[, tillage:= "nodata"]
# 
# 
# 
# data[data$treatment %in% load0, fertilizer:= 0]
# data[data$treatment %in% load50, fertilizer:= 50]
# data[data$treatment %in% load100, fertilizer:= 100]
# 
# data[data$treatment %in% constant, precipitation:= "cons"]
# data[data$treatment %in% increasing, precipitation:= "incr"]
# data[data$treatment %in% decreasing, precipitation:= "decr"]
# 
# data[data$treatment %in% TT, tillage:= "T"]
# data[data$treatment %in% NT, tillage:= "NT"]


#
setkey(data,time)