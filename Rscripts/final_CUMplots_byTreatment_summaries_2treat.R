### get cum files
mylongdata <- fread(all.cum)
mylongdata[,days:= days.water]
mylongdata <- mylongdata[days.manure<=19.875,]
mylongdata <- mylongdata[days.origin>=4,]# make manure event same length as water event

# delete CORE_3 and used only fendt and New.Graswang; change names
mylongdata[CORE=="CORE_03", cumNO:=NA]
mylongdata[CORE=="CORE_03", cumN2O:=NA]
mylongdata[CORE=="CORE_03", cumCO2:=NA]
mylongdata[CORE=="CORE_03", cumCH4:=NA]
mylongdata[CORE=="CORE_03", cumNH3:=NA]
mylongdata[CORE=="CORE_03", cumH2O:=NA]

mylongdata <- mylongdata[newColumn %in% c("Fendt", "New.Graswang"), ]
mylongdata[newColumn == "New.Graswang", newColumn:= "Graswang"]

# reset cummulative sum at events (water, manure)
mylongdata <- mylongdata[days.origin > 3.125,]
mywater <- mylongdata[days.water<=0, .SD[.N], by=code]
mymanure <- mylongdata[days.manure<=0, .SD[.N], by=code]

mywater <- mywater[,.(
        code,
        cumNO.w = cumNO, cumN2O.w = cumN2O,
        cumCO2.w = cumCO2, cumCH4.w = cumCH4,
        cumNH3.w = cumNH3, cumH2O.w = cumH2O
)]

mymanure <- mymanure[,.(
        code,
        cumNO.m = cumNO, cumN2O.m = cumN2O,
        cumCO2.m = cumCO2, cumCH4.m = cumCH4,
        cumNH3.m = cumNH3, cumH2O.m = cumH2O
)]

setkey(mywater, code)
setkey(mymanure, code)
setkey(mylongdata, code)

mylongdata <- mylongdata[mywater[mymanure]]

mylongdata[days.water>=0 & days.manure<0, cumNO:= cumNO - cumNO.w]
mylongdata[days.manure>=0, cumNO:= cumNO - cumNO.m]

mylongdata[days.water>=0 & days.manure<0, cumN2O:= cumN2O - cumN2O.w]
mylongdata[days.manure>=0, cumN2O:= cumN2O - cumN2O.m]

mylongdata[days.water>=0 & days.manure<0, cumCO2:= cumCO2 - cumCO2.w]
mylongdata[days.manure>=0, cumCO2:= cumCO2 - cumCO2.m]

mylongdata[days.water>=0 & days.manure<0, cumCH4:= cumCH4 - cumCH4.w]
mylongdata[days.manure>=0, cumCH4:= cumCH4 - cumCH4.m]

mylongdata[days.water>=0 & days.manure<0, cumH2O:= cumH2O - cumH2O.w]
mylongdata[days.manure>=0, cumH2O:= cumH2O - cumH2O.m]

mylongdata[days.water>=0 & days.manure<0, cumNH3:= cumNH3 - cumNH3.w]
mylongdata[days.manure>=0, cumNH3:= cumNH3 - cumNH3.m]


# correct data before water addition so it is equivalent to cumsum in 19.875 days
myorigin.4 <- mylongdata[days.origin==4, .SD[.N], by=code]
myorigin.8 <- mylongdata[days.origin==8, .SD[.N], by=code]

myorigin.4 <- myorigin.4[,.(
        code,
        cumNO.or4 = cumNO, cumN2O.or4 = cumN2O,
        cumCO2.or4 = cumCO2, cumCH4.or4 = cumCH4,
        cumNH3.or4 = cumNH3, cumH2O.or4 = cumH2O
)]

myorigin.8 <- myorigin.8[,.(
        code,
        cumNO.or8 = cumNO, cumN2O.or8 = cumN2O,
        cumCO2.or8 = cumCO2, cumCH4.or8 = cumCH4,
        cumNH3.or8 = cumNH3, cumH2O.or8 = cumH2O
)]

setkey(myorigin.4, code)
setkey(myorigin.8, code)
setkey(mylongdata, code)

mylongdata <- mylongdata[myorigin.4[myorigin.8]]

mylongdata[days.water<0, cumNO:= cumNO + (cumNO.or8 - cumNO.or4)/4*19.875]
mylongdata[days.water<0, cumN2O:= cumN2O + (cumN2O.or8 - cumN2O.or4)/4*19.875]
mylongdata[days.water<0, cumCO2:= cumCO2 + (cumCO2.or8 - cumCO2.or4)/4*19.875]
mylongdata[days.water<0, cumCH4:= cumCH4 + (cumCH4.or8 - cumCH4.or4)/4*19.875]
mylongdata[days.water<0, cumNH3:= cumNH3 + (cumNH3.or8 - cumNH3.or4)/4*19.875]
mylongdata[days.water<0, cumH2O:= cumH2O + (cumH2O.or8 - cumH2O.or4)/4*19.875]
# 
# mylongdata[days.water>=0 & days.manure<0, cumN2O:= cumN2O - cumN2O.w]
# mylongdata[days.manure>=0, cumN2O:= cumN2O - cumN2O.m]
# 
# mylongdata[days.water>=0 & days.manure<0, cumCO2:= cumCO2 - cumCO2.w]
# mylongdata[days.manure>=0, cumCO2:= cumCO2 - cumCO2.m]
# 
# mylongdata[days.water>=0 & days.manure<0, cumCH4:= cumCH4 - cumCH4.w]
# mylongdata[days.manure>=0, cumCH4:= cumCH4 - cumCH4.m]
# 
# mylongdata[days.water>=0 & days.manure<0, cumH2O:= cumH2O - cumH2O.w]
# mylongdata[days.manure>=0, cumH2O:= cumH2O - cumH2O.m]
# 
# mylongdata[days.water>=0 & days.manure<0, cumNH3:= cumNH3 - cumNH3.w]
# mylongdata[days.manure>=0, cumNH3:= cumNH3 - cumNH3.m]


### get files with summary statistics
myfiles <- list.files(cum.by.species.reset.origin.adapted)
files <- list.files(cum.by.species.reset.origin.adapted, full.names = T)
myNO <- files[grep("NO", myfiles)]
myN2O <- files[grep("N2O", myfiles)]
myCO2 <- files[grep("CO2", myfiles)]
myCH4 <- files[grep("CH4", myfiles)]
myNH3 <- files[grep("NH3", myfiles)]
myH2O <- files[grep("H2O", myfiles)]

myNO <- fread(myNO)
myN2O <- fread(myN2O)
myCO2 <- fread(myCO2)
myCH4 <- fread(myCH4)
myNH3 <- fread(myNH3)
myH2O <- fread(myH2O)

# make manure event same length as water event
myNO <- myNO[days.manure<=19.875 & days.origin>=4,]
myN2O <- myN2O[days.manure<=19.875 & days.origin>=4,]
myCO2 <- myCO2[days.manure<=19.875 & days.origin>=4,]
myCH4 <- myCH4[days.manure<=19.875 & days.origin>=4,]
myNH3 <- myNH3[days.manure<=19.875 & days.origin>=4,]
myH2O <- myH2O[days.manure<=19.875 & days.origin>=4,]

#######################################
# NO
myNO.F <- myNO[,.(
        days = days.water,
        newColumn = "Fendt",
        mymean = mean.F,
        mySE = se.F
)]
myNO.GW <- myNO[,.(
        days = days.water,
        newColumn = "New.Graswang",
        mymean = mean.GW,
        mySE = se.GW
)]
myNO.GWL <- myNO[,.(
        days = days.water,
        newColumn = "Graswang",
        mymean = mean.GWL,
        mySE = se.GWL
)]
myNO <- rbindlist(list(myNO.F, myNO.GW, myNO.GWL))
myNO <- rbindlist(list(myNO.F, myNO.GW, myNO.GWL))
myNO <- myNO [newColumn %in% c("Fendt", "New.Graswang"), ]
myNO[newColumn=="New.Graswang", newColumn:= "Graswang"]
#######################################
# N2O
myN2O.F <- myN2O[,.(
        days = days.water,
        newColumn = "Fendt",
        mymean = mean.F,
        mySE = se.F
)]
myN2O.GW <- myN2O[,.(
        days = days.water,
        newColumn = "New.Graswang",
        mymean = mean.GW,
        mySE = se.GW
)]
myN2O.GWL <- myN2O[,.(
        days = days.water,
        newColumn = "Graswang",
        mymean = mean.GWL,
        mySE = se.GWL
)]
myN2O <- rbindlist(list(myN2O.F, myN2O.GW, myN2O.GWL))
myN2O <- rbindlist(list(myN2O.F, myN2O.GW, myN2O.GWL))
myN2O <- myN2O [newColumn %in% c("Fendt", "New.Graswang"), ]
myN2O[newColumn=="New.Graswang", newColumn:= "Graswang"]
#######################################
# CO2
myCO2.F <- myCO2[,.(
        days = days.water,
        newColumn = "Fendt",
        mymean = mean.F,
        mySE = se.F
)]
myCO2.GW <- myCO2[,.(
        days = days.water,
        newColumn = "New.Graswang",
        mymean = mean.GW,
        mySE = se.GW
)]
myCO2.GWL <- myCO2[,.(
        days = days.water,
        newColumn = "Graswang",
        mymean = mean.GWL,
        mySE = se.GWL
)]
myCO2 <- rbindlist(list(myCO2.F, myCO2.GW, myCO2.GWL))
myCO2 <- rbindlist(list(myCO2.F, myCO2.GW, myCO2.GWL))
myCO2 <- myCO2 [newColumn %in% c("Fendt", "New.Graswang"), ]
myCO2[newColumn=="New.Graswang", newColumn:= "Graswang"]
#######################################
# CH4
myCH4.F <- myCH4[,.(
        days = days.water,
        newColumn = "Fendt",
        mymean = mean.F,
        mySE = se.F
)]
myCH4.GW <- myCH4[,.(
        days = days.water,
        newColumn = "New.Graswang",
        mymean = mean.GW,
        mySE = se.GW
)]
myCH4.GWL <- myCH4[,.(
        days = days.water,
        newColumn = "Graswang",
        mymean = mean.GWL,
        mySE = se.GWL
)]
myCH4 <- rbindlist(list(myCH4.F, myCH4.GW, myCH4.GWL))
myCH4 <- rbindlist(list(myCH4.F, myCH4.GW, myCH4.GWL))
myCH4 <- myCH4 [newColumn %in% c("Fendt", "New.Graswang"), ]
myCH4[newColumn=="New.Graswang", newColumn:= "Graswang"]
#######################################
# NH3
myNH3.F <- myNH3[,.(
        days = days.water,
        newColumn = "Fendt",
        mymean = mean.F,
        mySE = se.F
)]
myNH3.GW <- myNH3[,.(
        days = days.water,
        newColumn = "New.Graswang",
        mymean = mean.GW,
        mySE = se.GW
)]
myNH3.GWL <- myNH3[,.(
        days = days.water,
        newColumn = "Graswang",
        mymean = mean.GWL,
        mySE = se.GWL
)]
myNH3 <- rbindlist(list(myNH3.F, myNH3.GW, myNH3.GWL))
myNH3 <- rbindlist(list(myNH3.F, myNH3.GW, myNH3.GWL))
myNH3 <- myNH3 [newColumn %in% c("Fendt", "New.Graswang"), ]
myNH3[newColumn=="New.Graswang", newColumn:= "Graswang"]
#######################################
# H2O
myH2O.F <- myH2O[,.(
        days = days.water,
        newColumn = "Fendt",
        mymean = mean.F,
        mySE = se.F
)]
myH2O.GW <- myH2O[,.(
        days = days.water,
        newColumn = "New.Graswang",
        mymean = mean.GW,
        mySE = se.GW
)]
myH2O.GWL <- myH2O[,.(
        days = days.water,
        newColumn = "Graswang",
        mymean = mean.GWL,
        mySE = se.GWL
)]
myH2O <- rbindlist(list(myH2O.F, myH2O.GW, myH2O.GWL))
myH2O[,mymean:= mymean*core.area*24]
myH2O[,mySE:= mySE*core.area*24]
myH2O <- rbindlist(list(myH2O.F, myH2O.GW, myH2O.GWL))
myH2O <- myH2O [newColumn %in% c("Fendt", "New.Graswang"), ]
myH2O[newColumn=="New.Graswang", newColumn:= "Graswang"]

################################################################################################################################################################
################################################################################################################################################################
# collapsed with summary statistics
################################################################################
folder_out <- folder_out_byTreat.collapsed.mean.FG
# range <- range(data$days, na.rm =T)
range <- range(mylongdata[,days])
range[1]<- floor(range[1])
range[2]<- ceiling(range[2])

setkey(mylongdata, days)

# my color palette
cbPalette <- c("#999999", "#E69F00", "#CC79A7", "#009E73", "#0072B2", "#D55E00")
################################################################################
################################################################################
### CLD
################################################################################
# NO
i <- "cumNO"
state <- "fullscale"

toplot <- mylongdata[!is.na(cumNO)
                     , list(days, cumNO, newColumn, newLabel)]

g.1 <- ggplot(toplot, aes(x= days, y = cumNO))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                # strip.text.x = element_blank(),
                legend.position =   "none",
                strip.background =   element_blank(),
                # strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        # + ylab("NO [µg-N/m2/h]")
        + ylab(expression('NO [mg-N/m'^2*']'))
        # + ylab(expression('NO'[2] * ' / dt     [ppm / min]'))
        #         + xlab("")
        #         + labs(title = "(a)")
        #         + coord_cartesian(xlim= c(0, 100))
        + scale_x_continuous(breaks = c(0, 10, 20, 30, 40))
        #       + ggtitle(paste0("Flow change during sampling: \n influence on outlet air concentration"))
)

g.1 <- (g.1
        + facet_grid(~ newColumn)
        + geom_point(size=0.25, colour="black", alpha=0.5)
        + geom_ribbon(data=myNO, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
        + geom_point(data=myNO, aes(x = days, y = mymean), size=0.5, col="red")
)
g.1
myplot <- paste0(folder_out, "/", incubation, "__", paste(i), "_", state, ".png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

################################################################################
################################################################################
### QCL
################################################################################
# N2O
i <- "cumN2O"
state <- "fullscale"

toplot <- mylongdata[!is.na(cumN2O)
                     , list(days, cumN2O, newColumn, newLabel)]

g.1 <- ggplot(toplot, aes(x= days, y = cumN2O))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('N'[2]*'O [mg-N/m'^2*']'))
        + scale_x_continuous(breaks = c(0, 10, 20, 30, 40))
)

g.1 <- (g.1
        + facet_grid(~ newColumn)
        + geom_point(size=0.25, colour="black", alpha=0.5)
        + geom_ribbon(data=myN2O, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
        + geom_point(data=myN2O, aes(x = days, y = mymean), size=0.5, col="red")
)
g.1
myplot <- paste0(folder_out, "/", incubation, "__", paste(i), "_", state, ".png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

################################################################################
# CO2
i <- "cumCO2"
state <- "fullscale"

toplot <- mylongdata[!is.na(cumCO2)
                     , list(days, cumCO2, newColumn, newLabel)]

g.1 <- ggplot(toplot, aes(x= days, y = cumCO2))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' [g-C/m'^2*']'))
        + scale_x_continuous(breaks = c(0, 10, 20, 30, 40))
)

g.1 <- (g.1
        + facet_grid(~ newColumn)
        + geom_point(size=0.25, colour="black", alpha=0.5)
        + geom_ribbon(data=myCO2, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
        + geom_point(data=myCO2, aes(x = days, y = mymean), size=0.5, col="red")
)
g.1
myplot <- paste0(folder_out, "/", incubation, "__", paste(i), "_", state, ".png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

################################################################################
# CH4
i <- "cumCH4"
state <- "fullscale"

toplot <- mylongdata[!is.na(cumCH4)
                     , list(days, cumCH4, newColumn, newLabel)]

g.1 <- ggplot(toplot, aes(x= days, y = cumCH4))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CH'[4]*' [mg-C/m'^2*']'))
        + scale_x_continuous(breaks = c(0, 10, 20, 30, 40))
)

g.1 <- (g.1
        + facet_grid(~ newColumn)
        + geom_point(size=0.25, colour="black", alpha=0.5)
        + geom_ribbon(data=myCH4, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
        + geom_point(data=myCH4, aes(x = days, y = mymean), size=0.5, col="red")
)
g.1
myplot <- paste0(folder_out, "/", incubation, "__", paste(i), "_", state, ".png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()


################################################################################
# NH3
i <- "cumNH3"
state <- "fullscale"

toplot <- mylongdata[!is.na(cumNH3)
                     , list(days, cumNH3, newColumn, newLabel)]

g.1 <- ggplot(toplot, aes(x= days, y = cumNH3))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('NH'[3]*' [mg-N/m'^2*']'))
        + scale_x_continuous(breaks = c(0, 10, 20, 30, 40))
)

g.1 <- (g.1
        + facet_grid(~ newColumn)
        + geom_point(size=0.25, colour="black", alpha=0.5)
        + geom_ribbon(data=myNH3, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
        + geom_point(data=myNH3, aes(x = days, y = mymean), size=0.5, col="red")
)
g.1
myplot <- paste0(folder_out, "/", incubation, "__", paste(i), "_", state, ".png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()


################################################################################
# H2O
i <- "cumH2O"
state <- "fullscale"

toplot <- mylongdata[ !is.na(cumH2O)
                      , list(days, cumH2O, newColumn, newLabel)]
# toplot[,cumH2O:= cumH2O*core.area*24]
toplot[,cumH2O:= cumH2O]

g.1 <- ggplot(toplot, aes(x= days, y = cumH2O))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('H'[2]*'O [g-H'[2]*'O/core]'))
        + scale_x_continuous(breaks = c(0, 10, 20, 30, 40))
)

g.1 <- (g.1
        + facet_grid(~ newColumn)
        + geom_point(size=0.25, colour="black", alpha=0.5)
        + geom_ribbon(data=myH2O, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
        + geom_point(data=myH2O, aes(x = days, y = mymean), size=0.5, col="red")
)
g.1
myplot <- paste0(folder_out, "/", incubation, "__", paste(i), "_", state, ".png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

