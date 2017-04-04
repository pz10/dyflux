################################################################################
### MEANflow

# a flow-average over last 6 minutes is done (minute1 not included)
setkey(data,epoch_time)
p2 <- data[,list(epoch_time = epoch_time + 360 + 240, flow120 = flow)]
p3 <- data[,list(epoch_time = epoch_time + 360 + 180, flow180 = flow)]
p4 <- data[,list(epoch_time = epoch_time + 360 + 120, flow240 = flow)]
p5 <- data[,list(epoch_time = epoch_time + 360 + 60, flow300 = flow)]
p6 <- data[,list(epoch_time = epoch_time + 360 + 0, flow360 = flow)]

setkey(p2, epoch_time)
setkey(p3, epoch_time)
setkey(p4, epoch_time)
setkey(p5, epoch_time)
setkey(p6, epoch_time)

p <- p2[p3[p4[p5[p6]]]]

# match lengths
p <- p[epoch_time <= ( data[nrow(data), epoch_time] ), ]

# calculate MEANflow
p[,MEANflow:= mean( c(flow120, flow180, flow240, flow300, flow360) ), by = epoch_time ]

# add it to data (last column)
toadd <- p[data][,list(epoch_time, MEANflow)]
data[,MEANflow:= toadd$MEANflow]

################################################################################
### MEANp_flow

# a p_flow-average over last 6 minutes is done (minute1 not included)
setkey(data,epoch_time)
p2 <- data[,list(epoch_time = epoch_time + 360 + 240, p_flow120 = p_flow)]
p3 <- data[,list(epoch_time = epoch_time + 360 + 180, p_flow180 = p_flow)]
p4 <- data[,list(epoch_time = epoch_time + 360 + 120, p_flow240 = p_flow)]
p5 <- data[,list(epoch_time = epoch_time + 360 + 60, p_flow300 = p_flow)]
p6 <- data[,list(epoch_time = epoch_time + 360 + 0, p_flow360 = p_flow)]

setkey(p2, epoch_time)
setkey(p3, epoch_time)
setkey(p4, epoch_time)
setkey(p5, epoch_time)
setkey(p6, epoch_time)

p <- p2[p3[p4[p5[p6]]]]

# match lengths
p <- p[epoch_time <= ( data[nrow(data), epoch_time] ), ]

# calculate MEANp_flow
p[,MEANp_flow:= mean( c(p_flow120, p_flow180, p_flow240, p_flow300, p_flow360) ), by = epoch_time ]

# add it to data (last column)
toadd <- p[data][,list(epoch_time, MEANp_flow)]
data[,MEANp_flow:= toadd$MEANp_flow]

################################################################################
setkey(data,epoch_time)
