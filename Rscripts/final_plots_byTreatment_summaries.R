# get files with summary statistics
myfiles <- list.files(instant.by.species)
files <- list.files(instant.by.species, full.names = T)
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
myH2O[,mymean:= mymean*core.area*24/1000]
myH2O[,mySE:= mySE*core.area*24/1000]


################################################################################################################################################################
################################################################################################################################################################
# collapsed with summary statistics
################################################################################
folder_out <- folder_out_byTreat.collapsed.mean
# range <- range(data$days, na.rm =T)
range <- range(myQCLdata[,days])
range[1]<- floor(range[1])
range[2]<- ceiling(range[2])

setkey(myQCLdata, days)

# my color palette
cbPalette <- c("#999999", "#E69F00", "#CC79A7", "#009E73", "#0072B2", "#D55E00")
################################################################################
################################################################################
### CLD
################################################################################
# NO
i <- "NO"
state <- "fullscale"

toplot <- myQCLdata[!is.na(NO)
                    , list(days, NO, mNO, newColumn, newLabel)]

g.1 <- ggplot(toplot, aes(x= days, y = NO))
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
        + ylab(expression('NO [µg-N/m'^2*'/h]'))
        # + ylab(expression('NO'[2] * ' / dt     [ppm / min]'))
#         + xlab("")
#         + labs(title = "(a)")
#         + coord_cartesian(xlim= c(0, 100))
        + scale_x_continuous(breaks = c(0, 10, 20, 30, 40))
        #       + ggtitle(paste0("Flow change during sampling: \n influence on outlet air concentration"))
)

g.1 <- (g.1
        + facet_grid(~ newColumn)
        + geom_point(size=0.25, colour="black")
        + geom_ribbon(data=myNO, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
        + geom_line(data=myNO, aes(x = days, y = mymean), size=0.5, col="red")
)
g.1
myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

################################################################################
################################################################################
### QCL
################################################################################
# N2O
i <- "N2O"
state <- "fullscale"

toplot <- myQCLdata[!is.na(N2O)
                    , list(days, N2O, mN2O, newColumn, newLabel)]

g.1 <- ggplot(toplot, aes(x= days, y = N2O))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('N'[2]*'O [µg-N/m'^2*'/h]'))
        + scale_x_continuous(breaks = c(0, 10, 20, 30, 40))
)

g.1 <- (g.1
        + facet_grid(~ newColumn)
        + geom_point(size=0.25, colour="black")
        + geom_ribbon(data=myN2O, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
        + geom_line(data=myN2O, aes(x = days, y = mymean), size=0.5, col="red")
)
g.1
myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

################################################################################
# CO2
i <- "CO2"
state <- "fullscale"

toplot <- myQCLdata[!is.na(CO2)
                    , list(days, CO2, mCO2, newColumn, newLabel)]

g.1 <- ggplot(toplot, aes(x= days, y = CO2))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' [mg-C/m'^2*'/h]'))
        + scale_x_continuous(breaks = c(0, 10, 20, 30, 40))
)

g.1 <- (g.1
        + facet_grid(~ newColumn)
        + geom_point(size=0.25, colour="black")
        + geom_ribbon(data=myCO2, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
        + geom_line(data=myCO2, aes(x = days, y = mymean), size=0.5, col="red")
)
g.1
myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

################################################################################
# CH4
i <- "CH4"
state <- "fullscale"

toplot <- myQCLdata[!is.na(CH4)
                    , list(days, CH4, mCH4, newColumn, newLabel)]

g.1 <- ggplot(toplot, aes(x= days, y = CH4))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CH'[4]*' [µg-C/m'^2*'/h]'))
        + scale_x_continuous(breaks = c(0, 10, 20, 30, 40))
)

g.1 <- (g.1
        + facet_grid(~ newColumn)
        + geom_point(size=0.25, colour="black")
        + geom_ribbon(data=myCH4, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
        + geom_line(data=myCH4, aes(x = days, y = mymean), size=0.5, col="red")
)
g.1
myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()


################################################################################
# NH3
i <- "NH3"
state <- "fullscale"

toplot <- myQCLdata[!is.na(NH3)
                    , list(days, NH3, mNH3, newColumn, newLabel)]

g.1 <- ggplot(toplot, aes(x= days, y = NH3))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('NH'[3]*' [µg-N/m'^2*'/h]'))
        + scale_x_continuous(breaks = c(0, 10, 20, 30, 40))
)

g.1 <- (g.1
        + facet_grid(~ newColumn)
        + geom_point(size=0.25, colour="black")
        + geom_ribbon(data=myNH3, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
        + geom_line(data=myNH3, aes(x = days, y = mymean), size=0.5, col="red")
)
g.1
myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()


################################################################################
# H2O
i <- "H2O"
state <- "fullscale"

toplot <- myQCLdata[ !is.na(H2O)
                     , list(days, H2O, mH2O, newColumn, newLabel)]
toplot[,H2O:= H2O*core.area*24/1000]

g.1 <- ggplot(toplot, aes(x= days, y = H2O))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('H'[2]*'O [g-H'[2]*'O/core/day'))
        + scale_x_continuous(breaks = c(0, 10, 20, 30, 40))
)

g.1 <- (g.1
        + facet_grid(~ newColumn)
        + geom_point(size=0.25, colour="black")
        + geom_ribbon(data=myH2O, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
        + geom_line(data=myH2O, aes(x = days, y = mymean), size=0.5, col="red")
)
g.1
myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

################################################################################
setkey(myQCLdata, time)
