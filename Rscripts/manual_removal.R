to.remove <- fread(manual.remove)
to.remove[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]


# remove
myspecies <- unique(to.remove$species)

for(i in myspecies){
        to.rem.i <- to.remove[species==i,]
        mycores <- unique(to.rem.i$CORE)
        for(j in mycores){
                mydata <- to.rem.i[CORE==j,]
                mytimes <- mydata$time
                data[time %in% mytimes & CORE == j, eval(i):= NA]
        }
}

# remove data after failure event
data[days>14.5 & days<15.5, NO:= NA]
data[days>14.5 & days<15.5, N2O:= NA]
data[days>14.5 & days<15.5, CH4:= NA]
data[days>14.5 & days<15.5, NH3:= NA]
data[days>14.5 & days<15.5, H2O:= NA]

data[days>14.5 & days<17.5, CO2:= NA]