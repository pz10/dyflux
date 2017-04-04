# data0 <- fread(input = tankfileraw)
# data0[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
# data0[,start:= as.POSIXct(start, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
# data0[,end:= as.POSIXct(end, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
# ################################################################################
# ## 1st look to data0
# # par(mfcol= c(1,1))
# #         with(data0, plot(RH ~ time))
# #         with(data0, plot(T ~ time))
# #         with(data0, plot(flow ~ time))
# #         with(data0, plot(exhaust ~ time))
# #         with(data0, plot(p_flow ~ time))
# #         with(data0, plot(Li_H2O ~ time))
# #         with(data0, plot(CLD_Temp ~ time))
# #         with(data0, plot(CLD_Press ~ time))
# #         with(data0, plot(CLD_warning ~ time))
# #         with(data0, plot(CLD_error ~ time))
# #         
# #         with(data0, plot(CH4_ppb ~ time))
# #         with(data0, plot(H2O_ppm ~ time))
# #         
# #         with(data0, plot(N2O_ppb ~ time))
# #         with(data0, plot(CO2_ppm ~ time))
# #         with(data0, plot(NH3_ppb ~ time))
# #         with(data0, plot(Li_CO2 ~ time))
# # 
# # ###############################################################################
# # ## Thresholds
# # 
# # ###############################################################################
# # ## 1 RH and T --> do nothing
# # with(data0, plot(RH ~ time))
# # with(data0, plot(T ~ time))
# # 
# # ################################################################################
# # ### 2 flow --> >=330 & <= 333
# # with(data0, plot(flow ~ time))
# # with(data0, plot(flow ~ time, ylim = c(374.5,375.5)))
# # 
# # 
# # with(data0, plot(exhaust ~ time))
# # with(data0, plot(exhaust ~ time, ylim = c(0,150)))
# # with(data0, plot(exhaust ~ time, ylim = c(0,100)))
# # with(data0, plot(exhaust ~ time, ylim = c(0,40)))
# # 
# # with(data0, plot(CLD_Temp ~ time, ylim = c(44.5,45.5)))
# 
# # myvalues after checking the graphs
# min.exhaust <- 50
# max.exhaust <- 100
# min.CLD_temp <- 44.7
# max.CLD_temp <- 45.3
# 
# min.CLD_Press <- 19
# max.CLD_Press <- 21.0
# min.flow <- 374.5
# max.flow <- 375.5

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

###
min.flow <- min[parameter=="flow", min]
max.flow <- max[parameter=="flow", max]

min.exhaust <- min[parameter=="exhaust", min]
max.exhaust <- max[parameter=="exhaust", max]

min.CLD_temp <- min[parameter=="CLD_Temp", min]
max.CLD_temp <- max[parameter=="CLD_Temp", max]

min.CLD_Press <- min[parameter=="CLD_Press", min]
max.CLD_Press <- max[parameter=="CLD_Press", max]


