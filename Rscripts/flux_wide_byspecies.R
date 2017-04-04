# get data
instant <- data[,.(
        CORE, Column, Row, Number, treatment, location, pre_translocated, newColumn, newRow, newNumber, newLabel, newOrder, code,
        time, days.origin, days.water, days.manure,
        NO, N2O, CO2, CH4, NH3, H2O
        # mNO, mN2O, mCO2, mCH4, mNH3, mH2O
)]

# initial parameters
mystep <- 1/8 # time points where to calculate instant values
minNO <- 0 #minimum flux to be taken into account
minN2O <- 0 #minimum flux to be taken into account
minCO2 <- 0 #minimum flux to be taken into account
# minCH4 <- 0 #minimum flux to be taken into account

# initial and final dates
days.origin.1st <- min(instant$days.origin, na.rm=T)
days.origin.1st <- ceiling(days.origin.1st/mystep)*mystep
days.origin.final <- max(instant$days.origin, na.rm=T)
days.origin.final <- ceiling(days.origin.final/mystep)*mystep

days.water.1st <- min(instant$days.water, na.rm=T)
days.water.1st <- ceiling(days.water.1st/mystep)*mystep
days.water.final <- max(instant$days.water, na.rm=T)
days.water.final <- ceiling(days.water.final/mystep)*mystep

days.manure.1st <- min(instant$days.manure, na.rm=T)
days.manure.1st <- ceiling(days.manure.1st/mystep)*mystep
days.manure.final <- max(instant$days.manure, na.rm=T)
days.manure.final <- ceiling(days.manure.final/mystep)*mystep

# add extrapolated flux at events (e.g. water or manure) times
mywater <- instant[days.water<=0, .SD[.N], by=CORE]
mywater[, days.origin:= days.origin - days.water]
mywater[, days.water:= days.water - days.water]
mywater[, days.manure:= days.manure - days.water]

mymanure <- instant[days.manure<=0, .SD[.N], by=CORE]
mymanure[, days.origin:= days.origin - days.manure]
mymanure[, days.water:= days.water - days.manure]
mymanure[, days.manure:= days.manure - days.manure]

instant <- rbindlist(list(instant, mywater, mymanure))
setkey(instant, CORE, days.water)

instant <- instant[,.(code, newColumn, days.origin, days.water, days.manure,
           NO, N2O, CO2, CH4, NH3, H2O)]
setkey(instant, code, days.origin)

################################################################################
## create interpolated and gap-filled flux files
 # get target times from cumNO filefolder_out <- cum.by.species
folder_out <- cum.by.species
myfile<- paste0(folder_out, "/", incubation, "_cumNO.dat")
my.times <- fread(myfile)
my.times <- my.times[,.(days.origin, days.water, days.manure)]
target.times <- my.times$days.origin

# get species to export
species <- c("NO", "N2O", "CO2", "CH4", "NH3", "H2O")
mycodes <- unique(instant$code)

# initialize interpolated object
my.fluxes <- data.table(
        days.origin = rep(target.times, times = length(mycodes)),
        code = rep(mycodes, each=length(target.times), times = 1)
)
setkey(my.times, days.origin)
setkey(my.fluxes, days.origin)
my.fluxes <- my.times[my.fluxes]
setkey(my.fluxes,code, days.origin)

my.fluxes[, NO:= as.numeric(NA)]
my.fluxes[, N2O:= as.numeric(NA)]
my.fluxes[, CO2:= as.numeric(NA)]
my.fluxes[, CH4:= as.numeric(NA)]
my.fluxes[, NH3:= as.numeric(NA)]
my.fluxes[, H2O:= as.numeric(NA)]

# interpolate instant values to target times
for(i in 1:length(mycodes)){
        mycode <- mycodes[i]
        
        for(j in 1:length(species)){
                mygas <- species[j]
                
                # select target data (remove NA values)
                mydata <- instant[code==mycode & !is.na(get(mygas)), .(days.origin, mygas = get(mygas))]
                
                # interpolate instant values to target times
                int <- approx(x = mydata$days.origin,
                              y = mydata$mygas,
                              xout = target.times,
                              method = "linear", rule = 2)
                int <- int$y
                my.fluxes[code==mycode & !is.na(eval(mygas)), eval(mygas):= int]
                
        }
        print(paste0(Sys.time(), " code:", mycode, " have been interpolated"))
}

# export file
to.format4 <- species

mydata <- copy(my.fluxes)
mydata[, (to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4, flag = 0)), .SDcols = to.format4]
write.table(mydata, file= fileout.wide, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
print(  paste0( Sys.time(), " cum fluxes by species file was written to:  ")  )
print(myfile)

################################################################################
## export to wide format by species, with summary statistics
# delete CORE_03, due to incubation problem
my.fluxes <- fread(fileout.wide)
my.fluxes[code=="F07", (species):=NA ] # CORE_03

# oad treatments (add newColumn)
treatment <- fread(input = treatment_file)
treatments <- treatment[,.(code, newColumn)]
setkey(my.fluxes, code)
setkey(treatments, code)
my.fluxes <- my.fluxes[treatments]

for(i in 1:length(species)){
        target <- species[i]
        to.delete <- species[-c(i)]
        flux.i <- copy(my.fluxes[,.(code, newColumn, days.origin, days.water, days.manure,
                                 NO, N2O, CO2, CH4, NH3, H2O)])
        flux.i[,(to.delete):=NULL]
        
        # get summary statistics
        my.summary <- flux.i[,.(
                
                my.mean = mean(get(target), na.rm=T),
                my.sd = sd(get(target), na.rm=T),
                my.se = sd(get(target), na.rm=T)/sqrt(sum(!is.na(get(target))))
                
        ), by=.(days.origin, newColumn)]
        
        fendt <- my.summary[newColumn=="Fendt",
                            .(days.origin, mean.F = my.mean, sd.F = my.sd, se.F = my.se)]
        
        GW <- my.summary[newColumn=="New.Graswang",
                         .(days.origin, mean.GW = my.mean, sd.GW = my.sd, se.GW = my.se)]
        
        GWL <- my.summary[newColumn=="Graswang",
                          .(days.origin, mean.GWL = my.mean, sd.GWL = my.sd, se.GWL = my.se)]
        
        # to wide format
        flux.i <- dcast.data.table(flux.i,
                                  days.origin + days.water + days.manure ~ code,
                                  value.var = target)
        
        # attach summary
        setkey(flux.i, days.origin)
        setkey(fendt, days.origin)
        setkey(GW, days.origin)
        setkey(GWL, days.origin)
        flux.i <- flux.i[fendt[GW[GWL]]]
        
        # export data
        my.sd <- grep("sd.",names(flux.i), value=T)
        my.se <- grep("se.",names(flux.i), value=T)
        to.format2 <- c(my.sd, my.se)
        to.format4 <- grep("mean.",names(flux.i), value=T)
        
        mydata <- copy(flux.i)
        mydata[, (to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2, flag = 0)), .SDcols = to.format2]
        mydata[, (to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4, flag = 0)), .SDcols = to.format4]
        folder_out <- instant.by.species
        myfile<- paste0(folder_out, "/", incubation, "_", target, ".dat")
        write.table(mydata, file= myfile, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
        print(  paste0( Sys.time(), " instant interpolated fluxes by species file was written to:  ")  )
        print(myfile)
        
}