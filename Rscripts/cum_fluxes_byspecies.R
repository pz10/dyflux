## read cum file
cum.all <- fread(all.cum)
setkey(cum.all, code, days.origin)

# get species to export
species <- grep("cum", names(cum.all))
species <- names(cum.all)[species]

# # get code by treatment
# codes <- unique(cum.all$code)
# fendt <- codes [grep("F", codes)]
# GW <- codes [grep("GW0", codes)]
# GWL <- codes [grep("GWL", codes)]

# delete CORE_03, due to incubation problem
cum.all[CORE=="CORE_03", (species):=NA ]
cum.all <- cum.all[days.manure<=19.875,]


for(i in 1:length(species)){
        target <- species[i]
        to.delete <- species[-c(i)]
        cum.i <- copy(cum.all[,.(code, newColumn, days.origin, days.water, days.manure,
                                 cumNO, cumN2O, cumCO2, cumCH4, cumNH3, cumH2O)])
        cum.i[,(to.delete):=NULL]
        
        # get summary statistics
        my.summary <- cum.i[,.(
                
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
        cum.i <- dcast.data.table(cum.i,
                                  days.origin + days.water + days.manure ~ code,
                                  value.var = target)
        
        # attach summary
        setkey(cum.i, days.origin)
        setkey(fendt, days.origin)
        setkey(GW, days.origin)
        setkey(GWL, days.origin)
        cum.i <- cum.i[fendt[GW[GWL]]]
        
        # export data
        my.sd <- grep("sd.",names(cum.i), value=T)
        my.se <- grep("se.",names(cum.i), value=T)
        to.format2 <- c(my.sd, my.se)
        to.format4 <- grep("mean.",names(cum.i), value=T)
        
        mydata <- copy(cum.i)
        mydata[, (to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2, flag = 0)), .SDcols = to.format2]
        mydata[, (to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4, flag = 0)), .SDcols = to.format4]
        folder_out <- cum.by.species
        myfile<- paste0(folder_out, "/", incubation, "_", target, ".dat")
        write.table(mydata, file= myfile, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
        print(  paste0( Sys.time(), " cum fluxes by species file was written to:  ")  )
        print(myfile)
        
}

################################################################################
# reset cumsum at every event (water, manure)
## read cum file
cum.all <- fread(all.cum)
setkey(cum.all, code, days.origin)

# get species to export
species <- grep("cum", names(cum.all))
species <- names(cum.all)[species]

# # get code by treatment
# codes <- unique(cum.all$code)
# fendt <- codes [grep("F", codes)]
# GW <- codes [grep("GW0", codes)]
# GWL <- codes [grep("GWL", codes)]

# delete CORE_03, due to incubation problem
cum.all[CORE=="CORE_03", (species):=NA ]
cum.all <- cum.all[days.manure<=19.875,]


for(i in 1:length(species)){
        target <- species[i]
        to.delete <- species[-c(i)]
        cum.i <- copy(cum.all[,.(code, newColumn, days.origin, days.water, days.manure,
                                 cumNO, cumN2O, cumCO2, cumCH4, cumNH3, cumH2O)])
        
        # reset cummulative sum every event (manure, water)
        mywater <- cum.i[days.water<=0, .SD[.N], by=code]
        mymanure <- cum.i[days.manure<=0, .SD[.N], by=code]
        
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
        setkey(cum.i, code)
        
        cum.i <- cum.i[mywater[mymanure]]
        # setkey(cum.i, days.origin)
        
        cum.i[days.water>=0 & days.manure<0, cumNO:= cumNO - cumNO.w]
        cum.i[days.manure>=0, cumNO:= cumNO - cumNO.m]
        
        cum.i[days.water>=0 & days.manure<0, cumN2O:= cumN2O - cumN2O.w]
        cum.i[days.manure>=0, cumN2O:= cumN2O - cumN2O.m]
        
        cum.i[days.water>=0 & days.manure<0, cumCO2:= cumCO2 - cumCO2.w]
        cum.i[days.manure>=0, cumCO2:= cumCO2 - cumCO2.m]
        
        cum.i[days.water>=0 & days.manure<0, cumCH4:= cumCH4 - cumCH4.w]
        cum.i[days.manure>=0, cumCH4:= cumCH4 - cumCH4.m]
        
        cum.i[days.water>=0 & days.manure<0, cumH2O:= cumH2O - cumH2O.w]
        cum.i[days.manure>=0, cumH2O:= cumH2O - cumH2O.m]
        
        cum.i[days.water>=0 & days.manure<0, cumNH3:= cumNH3 - cumNH3.w]
        cum.i[days.manure>=0, cumNH3:= cumNH3 - cumNH3.m]
        
        # get target species
        cum.i[,(to.delete):=NULL]
        
        # get summary statistics
        my.summary <- cum.i[,.(
                
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
        cum.i <- dcast.data.table(cum.i,
                                  days.origin + days.water + days.manure ~ code,
                                  value.var = target)
        
        # attach summary
        setkey(cum.i, days.origin)
        setkey(fendt, days.origin)
        setkey(GW, days.origin)
        setkey(GWL, days.origin)
        cum.i <- cum.i[fendt[GW[GWL]]]
        
        # export data
        my.sd <- grep("sd.",names(cum.i), value=T)
        my.se <- grep("se.",names(cum.i), value=T)
        to.format2 <- c(my.sd, my.se)
        no.format <- c(to.format2, "days.origin", "days.water", "days.manure")
        to.format4 <- names(cum.i) [! names(cum.i) %in% no.format]
        
        mydata <- copy(cum.i)
        mydata[, (to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2, flag = 0)), .SDcols = to.format2]
        mydata[, (to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4, flag = 0)), .SDcols = to.format4]
        folder_out <- cum.by.species.reset
        myfile<- paste0(folder_out, "/", incubation, "__", target, "_reset.dat")
        write.table(mydata, file= myfile, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
        print(  paste0( Sys.time(), " cum fluxes by species file was written to:  ")  )
        print(myfile)
        
}

################################################################################
# reset cumsum at every event (water, manure)
# ORIGIN (before water addition) corrected so that cumsum is of 19.875 days
## read cum file
cum.all <- fread(all.cum)
setkey(cum.all, code, days.origin)

# get species to export
species <- grep("cum", names(cum.all))
species <- names(cum.all)[species]

# # get code by treatment
# codes <- unique(cum.all$code)
# fendt <- codes [grep("F", codes)]
# GW <- codes [grep("GW0", codes)]
# GWL <- codes [grep("GWL", codes)]

# delete CORE_03, due to incubation problem
cum.all[CORE=="CORE_03", (species):=NA ]
cum.all <- cum.all[days.manure<=19.875 & days.origin>=4,]


for(i in 1:length(species)){
        target <- species[i]
        to.delete <- species[-c(i)]
        cum.i <- copy(cum.all[,.(code, newColumn, days.origin, days.water, days.manure,
                                 cumNO, cumN2O, cumCO2, cumCH4, cumNH3, cumH2O)])
        
        # reset cummulative sum every event (manure, water)
        mywater <- cum.i[days.water<=0, .SD[.N], by=code]
        mymanure <- cum.i[days.manure<=0, .SD[.N], by=code]
        
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
        setkey(cum.i, code)
        
        cum.i <- cum.i[mywater[mymanure]]
        # setkey(cum.i, days.origin)
        
        cum.i[days.water>=0 & days.manure<0, cumNO:= cumNO - cumNO.w]
        cum.i[days.manure>=0, cumNO:= cumNO - cumNO.m]
        
        cum.i[days.water>=0 & days.manure<0, cumN2O:= cumN2O - cumN2O.w]
        cum.i[days.manure>=0, cumN2O:= cumN2O - cumN2O.m]
        
        cum.i[days.water>=0 & days.manure<0, cumCO2:= cumCO2 - cumCO2.w]
        cum.i[days.manure>=0, cumCO2:= cumCO2 - cumCO2.m]
        
        cum.i[days.water>=0 & days.manure<0, cumCH4:= cumCH4 - cumCH4.w]
        cum.i[days.manure>=0, cumCH4:= cumCH4 - cumCH4.m]
        
        cum.i[days.water>=0 & days.manure<0, cumH2O:= cumH2O - cumH2O.w]
        cum.i[days.manure>=0, cumH2O:= cumH2O - cumH2O.m]
        
        cum.i[days.water>=0 & days.manure<0, cumNH3:= cumNH3 - cumNH3.w]
        cum.i[days.manure>=0, cumNH3:= cumNH3 - cumNH3.m]
        
        # correct data before water addition so it is equivalent to cumsum in 19.875 days
        myorigin.4 <- cum.i[days.origin==4, .SD[.N], by=code]
        myorigin.8 <- cum.i[days.origin==8, .SD[.N], by=code]
        
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
        setkey(cum.i, code)
        
        cum.i <- cum.i[myorigin.4[myorigin.8]]
        
        cum.i[days.water<0, cumNO:= cumNO + (cumNO.or8 - cumNO.or4)/4*19.875]
        cum.i[days.water<0, cumN2O:= cumN2O + (cumN2O.or8 - cumN2O.or4)/4*19.875]
        cum.i[days.water<0, cumCO2:= cumCO2 + (cumCO2.or8 - cumCO2.or4)/4*19.875]
        cum.i[days.water<0, cumCH4:= cumCH4 + (cumCH4.or8 - cumCH4.or4)/4*19.875]
        cum.i[days.water<0, cumNH3:= cumNH3 + (cumNH3.or8 - cumNH3.or4)/4*19.875]
        cum.i[days.water<0, cumH2O:= cumH2O + (cumH2O.or8 - cumH2O.or4)/4*19.875]
        
        # get target species
        cum.i[,(to.delete):=NULL]
        
        # get summary statistics
        my.summary <- cum.i[,.(
                
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
        cum.i <- dcast.data.table(cum.i,
                                  days.origin + days.water + days.manure ~ code,
                                  value.var = target)
        
        # attach summary
        setkey(cum.i, days.origin)
        setkey(fendt, days.origin)
        setkey(GW, days.origin)
        setkey(GWL, days.origin)
        cum.i <- cum.i[fendt[GW[GWL]]]
        
        # export data
        my.sd <- grep("sd.",names(cum.i), value=T)
        my.se <- grep("se.",names(cum.i), value=T)
        to.format2 <- c(my.sd, my.se)
        no.format <- c(to.format2, "days.origin", "days.water", "days.manure")
        to.format4 <- names(cum.i) [! names(cum.i) %in% no.format]
        
        mydata <- copy(cum.i)
        mydata[, (to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2, flag = 0)), .SDcols = to.format2]
        mydata[, (to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4, flag = 0)), .SDcols = to.format4]
        folder_out <- cum.by.species.reset.origin.adapted
        myfile<- paste0(folder_out, "/", incubation, "__", target, "_reset.dat")
        write.table(mydata, file= myfile, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
        print(  paste0( Sys.time(), " cum fluxes by species file was written to:  ")  )
        print(myfile)
        
}