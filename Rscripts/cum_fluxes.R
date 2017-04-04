# get data
cum <- data[,.(
        CORE, Column, Row, Number, treatment, location, pre_translocated, newColumn, newRow, newNumber, newLabel, newOrder, code,
        time, days.origin, days.water, days.manure,
        NO, N2O, CO2, CH4, NH3, H2O
        # mNO, mN2O, mCO2, mCH4, mNH3, mH2O
)]

# initial parameters
mystep <- 1/8 # time points where to calculate cumulative values
minNO <- 0 #minimum flux to be taken into account
minN2O <- 0 #minimum flux to be taken into account
minCO2 <- 0 #minimum flux to be taken into account
# minCH4 <- 0 #minimum flux to be taken into account

# initial and final dates
days.origin.1st <- min(cum$days.origin, na.rm=T)
days.origin.1st <- ceiling(days.origin.1st/mystep)*mystep
days.origin.final <- max(cum$days.origin, na.rm=T)
days.origin.final <- ceiling(days.origin.final/mystep)*mystep

days.water.1st <- min(cum$days.water, na.rm=T)
days.water.1st <- ceiling(days.water.1st/mystep)*mystep
days.water.final <- max(cum$days.water, na.rm=T)
days.water.final <- ceiling(days.water.final/mystep)*mystep

days.manure.1st <- min(cum$days.manure, na.rm=T)
days.manure.1st <- ceiling(days.manure.1st/mystep)*mystep
days.manure.final <- max(cum$days.manure, na.rm=T)
days.manure.final <- ceiling(days.manure.final/mystep)*mystep

# add extrapolated flux at events (e.g. water or manure) times
mywater <- cum[days.water<=0, .SD[.N], by=CORE]
mywater[, days.origin:= days.origin - days.water]
mywater[, days.water:= days.water - days.water]
mywater[, days.manure:= days.manure - days.water]

mymanure <- cum[days.manure<=0, .SD[.N], by=CORE]
mymanure[, days.origin:= days.origin - days.manure]
mymanure[, days.water:= days.water - days.manure]
mymanure[, days.manure:= days.manure - days.manure]

cum <- rbindlist(list(cum, mywater, mymanure))
setkey(cum, CORE, days.water)

################################################################################
## create interpolated and gap-filled flux files

# days.origin
daysorigin <- seq(from = days.origin.1st, to = days.origin.final, by = mystep)
offset.water <- cum$days.origin[1] - cum$days.water[1]
offset.manure <- cum$days.origin[1] - cum$days.manure[1]
cores <- sort(unique(cum$CORE))

cum.origin <- data.table(
        days.origin = rep(daysorigin, times = length(cores)),
        CORE = rep(cores, each=length(daysorigin), times = 1)
)
cum.origin[, days.water:= days.origin - offset.water]
cum.origin[, days.manure:= days.origin - offset.manure]

setkey(cum.origin, CORE)
setkey(treatment, CORE)
cum.origin <- treatment[cum.origin]

cum.origin[, NO:= as.numeric(NA)]
cum.origin[, N2O:= as.numeric(NA)]
cum.origin[, CO2:= as.numeric(NA)]
cum.origin[, CH4:= as.numeric(NA)]
cum.origin[, NH3:= as.numeric(NA)]
cum.origin[, H2O:= as.numeric(NA)]

# days.water
dayswater <- seq(from = days.water.1st, to = days.water.final, by = mystep)
cum.water <- copy(cum.origin)
cum.water[,days.water:= rep(dayswater, times = length(cores))]
cum.water[,days.origin:= days.water + offset.water]
cum.water[,days.manure:= days.water + offset.water - offset.manure]

# days.manure
daysmanure <- seq(from = days.manure.1st, to = days.manure.final, by = mystep)
cum.manure <- copy(cum.origin)
cum.manure[,days.manure:= rep(daysmanure, times = length(cores))]
cum.manure[,days.origin:= days.manure + offset.manure]
cum.manure[,days.water:= days.manure + offset.manure - offset.water]

################################################################################################################################################################
# calculate area under the curve (integral) at 3h interpolated values (centred at origin, water addition and manure addition)
cores <- sort(unique(data$CORE))
species <- c("NO", "N2O", "CO2", "CH4", "NH3", "H2O")

# add offset so everything is above zero
offset <- 1000
cum[,NO:= NO + offset]
cum[,N2O:= N2O + offset]
cum[,CO2:= CO2 + offset]
cum[,CH4:= CH4 + offset]
cum[,NH3:= NH3 + offset]
cum[,H2O:= H2O + offset]

for(i in 1:length(cores)){
        mycore <- cores[i]
        
        for(j in 1:length(species)){
                mygas <- species[j]
                
                # select target data (remove NA values)
                mydata <- cum[CORE==mycore & !is.na(eval(mygas)), c("days.origin", mygas), with=F]
                mydata <- cum[CORE==mycore & !is.na(get(mygas)), .(days.origin, mygas = get(mygas))]
                mydata[,hours:= days.origin * 24]
                mydata[1, hours:= 0]
                
                # get area under the curve (integral, trapezoid)
                mydata[,cum:= as.numeric(NA)]
                for(k in 2:nrow(mydata)){
                        mydt <- mydata[1:k,]
                        mydata[k, cum:= auc(mydt$hours,mydt$mygas, thresh=0)]
                }
                
                # interpolate cum values to target times
                ### cum.origin
                int <- approx(x = mydata$days.origin,
                              y = mydata$cum,
                              xout = cum.origin$days.origin[1:length(daysorigin)],
                              method = "linear", rule = 2)
                int <- int$y
                cum.origin[CORE==mycore & !is.na(eval(mygas)), eval(mygas):= int]
                
                ### cum.water
                int <- approx(x = mydata$days.origin,
                              y = mydata$cum,
                              xout = cum.water$days.origin[1:length(daysorigin)],
                              method = "linear", rule = 2)
                int <- int$y
                cum.water[CORE==mycore & !is.na(eval(mygas)), eval(mygas):= int]
                
                ### cum.origin
                int <- approx(x = mydata$days.origin,
                              y = mydata$cum,
                              xout = cum.manure$days.origin[1:length(daysorigin)],
                              method = "linear", rule = 2)
                int <- int$y
                cum.manure[CORE==mycore & !is.na(eval(mygas)), eval(mygas):= int]
        }
        print(paste0(Sys.time(), " CORE", i, " values have been integrated"))
}

# remove previously added offset and change units (µg-N to mg-N; mg-C to g-C and so on)
cum.origin[, NO:= (NO - offset*days.origin*24)/1000]
cum.origin[, N2O:= (N2O - offset*days.origin*24)/1000]
cum.origin[, CO2:= (CO2 - offset*days.origin*24)/1000]
cum.origin[, CH4:= (CH4 - offset*days.origin*24)/1000]
cum.origin[, NH3:= (NH3 - offset*days.origin*24)/1000]
cum.origin[, H2O:= (H2O - offset*days.origin*24)/1000]
cum.origin[, H2O:= core.area * H2O] #total water loss in g-H2O per CORE

cum.water[, NO:= (NO - offset*days.origin*24)/1000]
cum.water[, N2O:= (N2O - offset*days.origin*24)/1000]
cum.water[, CO2:= (CO2 - offset*days.origin*24)/1000]
cum.water[, CH4:= (CH4 - offset*days.origin*24)/1000]
cum.water[, NH3:= (NH3 - offset*days.origin*24)/1000]
cum.water[, H2O:= (H2O - offset*days.origin*24)/1000]
cum.water[, H2O:= core.area * H2O] #total water loss in g-H2O per CORE

cum.manure[, NO:= (NO - offset*days.origin*24)/1000]
cum.manure[, N2O:= (N2O - offset*days.origin*24)/1000]
cum.manure[, CO2:= (CO2 - offset*days.origin*24)/1000]
cum.manure[, CH4:= (CH4 - offset*days.origin*24)/1000]
cum.manure[, NH3:= (NH3 - offset*days.origin*24)/1000]
cum.manure[, H2O:= (H2O - offset*days.origin*24)/1000]
cum.manure[, H2O:= core.area * H2O] #total water loss in g-H2O per CORE

# write files
to.format6 <- c("days.origin", "days.water", "days.manure")
to.format4 <- species
oldnames <- species
newnames <- paste0("cum", species)

## days.origin
mydata <- copy(cum.origin)
mydata[,comment:=NULL]
setkey(mydata, CORE, days.origin)
mydata[, (to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4, flag = 0)), .SDcols = to.format4]
mydata[, (to.format6):= lapply(.SD, function(x) formatC(x, format = "f", digits = 6, flag = 0)), .SDcols = to.format6]
setnames(mydata, oldnames, newnames)
write.table(mydata, file= origin.cum, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
print(  paste0( Sys.time(), " cum fluxes file was written to:  ")  )
print(origin.cum)

## days.water
mydata <- copy(cum.water)
mydata[,comment:=NULL]
setkey(mydata, CORE, days.origin)
mydata[, (to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4, flag = 0)), .SDcols = to.format4]
mydata[, (to.format6):= lapply(.SD, function(x) formatC(x, format = "f", digits = 6, flag = 0)), .SDcols = to.format6]
setnames(mydata, oldnames, newnames)
write.table(mydata, file= water.cum, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
print(  paste0( Sys.time(), " cum fluxes file was written to:  ")  )
print(water.cum)

## days.manure
mydata <- copy(cum.manure)
mydata[,comment:=NULL]
setkey(mydata, CORE, days.origin)
mydata[, (to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4, flag = 0)), .SDcols = to.format4]
mydata[, (to.format6):= lapply(.SD, function(x) formatC(x, format = "f", digits = 6, flag = 0)), .SDcols = to.format6]
setnames(mydata, oldnames, newnames)
write.table(mydata, file= manure.cum, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
print(  paste0( Sys.time(), " cum fluxes file was written to:  ")  )
print(manure.cum)


################################################################################
# single file with each event (origin, water or manure being centred)
st.cum.origin <- cum.origin[days.water <0]
st.cum.water <- cum.water[days.water >=0 & days.manure < 0]
st.cum.manure <- cum.manure[days.manure >=0]

singletime.cum <- rbindlist(list(st.cum.origin, st.cum.water, st.cum.manure))

# write file
mydata <- copy(singletime.cum)
mydata[,comment:=NULL]
setkey(mydata, CORE, days.origin)
mydata[, (to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4, flag = 0)), .SDcols = to.format4]
mydata[, (to.format6):= lapply(.SD, function(x) formatC(x, format = "f", digits = 6, flag = 0)), .SDcols = to.format6]
setnames(mydata, oldnames, newnames)
write.table(mydata, file= all.cum, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
print(  paste0( Sys.time(), " cum fluxes file was written to:  ")  )
print(all.cum)

