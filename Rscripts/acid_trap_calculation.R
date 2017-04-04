#time recording
print(  paste0(Sys.time(), " start acid_trap_calculation.R")  )
###
### input files
folder <- paste0(input_path, "/", incubation, "_extra")
empty <- paste0(folder, "/acid_trap_empty_weigths.dat")
total <- paste0(folder, "/acid_trap_total_weigths.dat")
conc <- paste0(folder, "/acid_trap_N_conc.dat")

folder_out <- paste0(IDASw_output_folder, "/", incubation, "_fluxes/cum_fluxes")
origin.cum<- paste0(folder_out, "/", incubation, "_cum_fluxes_origin_centred.dat")

folder_out <- paste0(IDASw_output_folder, "/", incubation, "_fluxes/acid_trap")
tot <- paste0(folder_out, "/", incubation, "_acid_trap_estimation.dat")
tot.summary <- paste0(folder_out, "/", incubation, "_acid_trap_estimation_summary.dat")

### output folder
folder_out <- paste0(IDASw_output_folder, "/", incubation, "_fluxes/acid_trap")
dir.create(folder_out, recursive = TRUE)
myfile <- paste0(folder_out, "/", incubation, "_acid_trap_provisional.dat")

# read files
empty <- fread(empty)
total <- fread(total)
NH3cum <- fread(origin.cum)
conc <- fread(conc)
tot <- fread(tot)
tot.summary <- fread(tot.summary)

# merge trap-weight and trap-concentration
setkey(tot, time, trap, CORE)
setkey(conc, time, trap, CORE)

conc <- conc[tot]

##
conc[, N.conc.mgL:= NH3.inTrap.conc]#just to have somethong before we get the real conc
##

conc <- conc[,.(
        time, CORE, trap, N.conc.mgL, days.origin,
        volume, sampled.vol.ml, NH3.PIC, est.NH3.inTrap.conc = NH3.inTrap.conc
)]
conc[,NH3.sampled:= N.conc.mgL * sampled.vol.ml]
conc[,NH3.intrap:= N.conc.mgL * volume]
conc[,NH3.cum.sampled:= as.numeric(NA)]
conc[,id:= paste(CORE, trap)]
setkey(conc, id)
for(i in unique(conc$id)){
        conc[id==i,NH3.cum.sampled:= cumsum(NH3.sampled)]
}
for(i in unique(conc$id)){
        conc[id==i, aux.NH3.cum.sampled:= c(0, NH3.cum.sampled)[1:.N]]
}
conc[,NH3:= NH3.intrap + aux.NH3.cum.sampled]

conc <- conc[,.(
        time, CORE, trap, N.conc.mgL, days.origin, NH3.PIC, NH3.PIC.inTrap.conc = est.NH3.inTrap.conc,
        volume, sampled.vol.ml,
        NH3.intrap, NH3.sampled, NH3.cum.sampled, NH3
)]

# write files
to.format2 <- c("N.conc.mgL", "NH3.PIC", "NH3.PIC.inTrap.conc",
                "volume", "NH3.intrap", "NH3.sampled", "NH3.cum.sampled", "NH3")
to.format6 <- c("days.origin")
mydata <- copy(conc)
setkey(mydata, trap, CORE, days.origin)
mydata[, (to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2, flag = 0)), .SDcols = to.format2]
mydata[, (to.format6):= lapply(.SD, function(x) formatC(x, format = "f", digits = 6, flag = 0)), .SDcols = to.format6]
write.table(mydata, file= myfile, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
print(  paste0( Sys.time(), " acid trap file was written to:  ")  )
print(myfile)

