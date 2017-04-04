#time recording
print(  paste0(Sys.time(), " start acid_trap_estimation.R")  )
###
### input files
folder <- paste0(input_path, "/", incubation, "_extra")
empty <- paste0(folder, "/acid_trap_empty_weigths.dat")
total <- paste0(folder, "/acid_trap_total_weigths.dat")

folder_out <- paste0(IDASw_output_folder, "/", incubation, "_fluxes/cum_fluxes")
origin.cum<- paste0(folder_out, "/", incubation, "_cum_fluxes_origin_centred.dat")

### output folders
folder_out <- paste0(IDASw_output_folder, "/", incubation, "_fluxes/acid_trap")
dir.create(folder_out, recursive = TRUE)
tot <- paste0(folder_out, "/", incubation, "_acid_trap_estimation.dat")
tot.summary <- paste0(folder_out, "/", incubation, "_acid_trap_estimation_summary.dat")

# read files
empty <- fread(empty)
total <- fread(total)
NH3cum <- fread(origin.cum)

# merge files
setkey(empty, CORE, trap)
setkey(total, CORE, trap)

total <- total[empty]
total[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]

mytime <- unclass(time.origin)
total[, days.origin:= (unclass(time) - mytime)/60/60/24]

total[,volume:= before - flask.head]
total[,NH3.PIC:= as.numeric(0)]

cores <- sort(unique(total$CORE))
cores00 <- sort(unique(NH3cum$CORE))
for(i in 1:length(cores)){
        # subset for target core
        mycore <- cores[i]
        mydata <- NH3cum[CORE==cores00[i],.(days.origin, cumNH3)]
        
        # interpolate value at sampling times
        mytimes <- total[CORE==i & trap=="a", days.origin]
        int <- approx(x = mydata$days.origin,
                      y = mydata$cumNH3,
                      xout = mytimes,
                      method = "linear", rule = 2)
        int <- int$y
        total[CORE==i & trap=="a", NH3.PIC:= int]
}
rm(NH3cum)
total[days.origin==0, NH3.PIC:= 0 ]
total[, NH3.PIC:= NH3.PIC * core.area * 1000] # cumulative NH3 measured by PICARRO, in µg-N/CORE
total[,NH3.PIC.conc.mgL:= NH3.PIC/volume] # estimated concentration in µg-N/ml; withdraw sample not considered

# remove sampled N
total[,NH3.sampled:= as.numeric(NA)]
total[,NH3.cum.sampled:= as.numeric(NA)]
total[,NH3.inTrap:= as.numeric(NA)]
total[,NH3.inTrap.conc:= as.numeric(NA)]
for(i in 1:length(cores)){
        # subset for target core
        mydata <- total[CORE==i & trap=="a", .(
                volume, NH3.PIC, NH3.PIC.conc.mgL,
                NH3.sampled,NH3.cum.sampled,
                NH3.inTrap, NH3.inTrap.conc
                )]
        mydata[1, NH3.sampled:= NH3.PIC.conc.mgL * 2]
        mydata[1, NH3.inTrap:= NH3.PIC]
        mydata[1, NH3.inTrap.conc:= NH3.inTrap/volume]
        previously.sampled <- mydata[1, NH3.sampled]
        mydata[1, NH3.cum.sampled:= previously.sampled]

        
        for(j in 2:nrow(mydata)){
                mydata[j, NH3.inTrap:= NH3.PIC - previously.sampled]
                mydata[j, NH3.inTrap.conc:= NH3.inTrap/volume]
                mydata[j, NH3.sampled:= NH3.inTrap.conc * 2]
                
                previously.sampled <- previously.sampled + mydata[j, NH3.sampled]
                mydata[j, NH3.cum.sampled:= previously.sampled]
        }
        mydata <- mydata[,.(NH3.sampled, NH3.cum.sampled, NH3.inTrap, NH3.inTrap.conc)]
        mycols <- names(mydata)
        
        # insert calculation into 'total'
        total[CORE==i & trap=="a", (mycols):= mydata]
}

# total[trap=="a",max(NH3.inTrap.conc, na.rm=T), by=.(CORE)]
total[, dilution:= NH3.inTrap.conc/5]
total[NH3.inTrap.conc <= 4.5 & NH3.inTrap.conc >= 0, dilution:= 1]

total[,dilution.folds:= log(NH3.inTrap.conc/5, base=2)]
total[NH3.inTrap.conc <= 4.5 & NH3.inTrap.conc >= 0, dilution.folds:= 0]
total[,folds.min:= floor(dilution.folds)]
total[,folds.max:= ceiling(dilution.folds)]
total[,conc.min:= NH3.inTrap.conc/ 2^folds.min]
total[,conc.max:= NH3.inTrap.conc/ 2^folds.max]
total[,is.min:= conc.min > 1.5 & conc.min < 5]
total[,is.max:= conc.max > 1.5 & conc.max < 5]

total[dilution.folds==0, is.min:= TRUE]
total[dilution.folds==0,is.max:= TRUE]
total[ ,suggested.dilution:= 2^folds.max]
total[is.min==T, suggested.dilution:= 2^folds.min]

to.export<- !names(total) %in% c("dilution.folds", "folds.min", "folds.max", "conc.min",  "conc.max", "is.min", "is.max")
to.export <- names(total)[to.export]
total <- total[,to.export, with=F]

total.summary <- total[trap=="a", .(time, CORE, trap, days.origin, volume,
                                    NH3.inTrap.conc, suggested.dilution, dilution)]

# write files
to.format2 <- c("NH3.inTrap.conc", "dilution")
to.format6 <- c("days.origin")
mydata <- copy(total.summary)
setkey(mydata, CORE, days.origin)
mydata[, (to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2, flag = 0)), .SDcols = to.format2]
mydata[, (to.format6):= lapply(.SD, function(x) formatC(x, format = "f", digits = 6, flag = 0)), .SDcols = to.format6]
write.table(mydata, file= tot.summary, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
print(  paste0( Sys.time(), " acid trap estimation summary file was written to:  ")  )
print(tot.summary)

# write files
to.format2 <- c("dilution", "NH3.PIC", "NH3.PIC.conc.mgL", "NH3.sampled", "NH3.cum.sampled", "NH3.inTrap", "NH3.inTrap.conc")
to.format6 <- c("days.origin")
mydata <- copy(total)
setkey(mydata, CORE, days.origin)
mydata[, (to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2, flag = 0)), .SDcols = to.format2]
mydata[, (to.format6):= lapply(.SD, function(x) formatC(x, format = "f", digits = 6, flag = 0)), .SDcols = to.format6]
write.table(mydata, file= tot, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
print(  paste0( Sys.time(), " acid trap estimation file was written to:  ")  )
print(tot)

