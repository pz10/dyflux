mytime <- unclass(time.water)
data[, days:= (unclass(time) - mytime)/60/60/24]

mytime <- unclass(time.manure)
data[, days.manure:= (unclass(time) - mytime)/60/60/24]

mytime <- unclass(time.origin)
data[, days.origin:= (unclass(time) - mytime)/60/60/24]
# 
# 
# 
# ### Rainevents file
# events.file <- paste0(input_path, "/", incubation, "_extra/", incubation, "_rain_events_time.dat")
# 
# events <- fread(input = events.file)
# events[,RainEvent1:= as.POSIXct(RainEvent1, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
# events[,RainEvent2:= as.POSIXct(RainEvent2, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
# events[,RainEvent3:= as.POSIXct(RainEvent3, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
# 
# setkey(data, treatment)
# ################################################################################
# # days after 1st rain event := 'days'
# for (i in 1:nrow(events)){
#         mytime <- unclass( events$RainEvent1[i] ) [1]
#         #         data[J(events$treatment[i]), days:= (time - mytime)/24] no funciona
#         data[J(events$treatment[i]), days:= (unclass(time) - mytime)/60/60/24]
# }
# 
# # days after 2nd rain event := 'days2ndEvent'
# for (i in 1:nrow(events)){
#         mytime <- unclass( events$RainEvent2[i] ) [1]
#         data[J(events$treatment[i]), days2ndEvent:= (unclass(time) - mytime)/60/60/24]
# }
# 
# # days after 3rd rain event := 'days3rdEvent'
# for (i in 1:nrow(events)){
#         mytime <- unclass( events$RainEvent3[i] ) [1]
#         data[J(events$treatment[i]), days3rdEvent:= (unclass(time) - mytime)/60/60/24]
# }
# ################################################################################
# # add 'days.adjusted' column:
# data[,days.adjusted:=as.numeric(NA)]
# data[days > (-3) & days < 14, days.adjusted:=days]
# data[days2ndEvent > 0 & days2ndEvent < 14, days.adjusted:=days2ndEvent + 15]
# data[days3rdEvent > 0 & days3rdEvent < 14, days.adjusted:=days3rdEvent + 30]
# 
# data[days > (-1) & days < 0, days.adjusted:=NA]
# # data[days2ndEvent > 13.8 & days2ndEvent < 14, days.adjusted:=NA]
# 
# # add 'days.adj.full' column:
# data[,days.adj.full:=as.numeric(NA)]
# data[days < 15, days.adj.full:=days]
# data[days2ndEvent > 0 & days2ndEvent < 15, days.adj.full:=days2ndEvent + 15]
# data[days3rdEvent > 0 & days3rdEvent < 15, days.adj.full:=days3rdEvent + 30]
# 
# 
# ##
setkey(data, time)