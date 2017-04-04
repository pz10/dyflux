###
### input file
folder <- paste0(IDASw_output_folder, "/", incubation, "_full_60_corrected")
air <- paste0(folder, "/", incubation, "_full60_corrected_value.dat")
treatment_file <- paste0(input_path, "/", incubation, "_extra/", incubation, "_treatments.dat")

folder <- paste0(IDASw_output_folder, "/", incubation, "_fluxes")
fileout<- paste0(folder, "/", incubation, "_fluxes.dat")

# output
folder_flux_plots <- paste0(IDASw_output_folder, "/", incubation, "_incubation_plots")
myNH3folder <- paste0(folder_flux_plots, "/", incubation, "_airNH3_vs_preNH3")
dir.create(myNH3folder, recursive = TRUE)

##
air <- fread(air)


setkey(air, bin_INC1, bin_INC_valve)
AIR1 <- air[J("0-1-0-0000-0000",10)]

setkey(air, bin_INC2, bin_INC_valve)
AIR2 <- air[J("0-1-00-0000-0000",1)]

air <- rbind(AIR1, AIR2, use.names=TRUE)

air <- air[exhaust<100 & exhaust>40,]
air <- air[flow<390 & flow>350,]

air <- air[epoch_time%%360 == 0,.(
        time, bin_INC_valve,
        NO = CLD_NO,
        N2O = N2O_ppb,
        CO2 = CO2_ppm,
        CH4 = CH4_ppb,
        H2O = H2O_ppm,
        NH3 = NH3_ppb
)]
air[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
setnames(air, paste0("air.", names(air)))
air[,epoch_time := unclass(air.time)]

pre <- fread(fileout)
pre[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
pre[,epoch_time := unclass(time)+360]
pre <- pre[epoch_time %in% (air$epoch_time),]
setkey(pre, epoch_time)
setkey(air, epoch_time)

data <- air[pre]
setkey(data, CORE)

treatments <- fread(treatment_file)
setkey(treatments, CORE)

data <- treatments[data]

with(data, plot(air.NH3~NH3, pch=19))


mydata <- copy(data)

ammonia <- data[,.(NH3,air.NH3)]
ammonia <- ammonia[!(is.na(NH3))]
ammonia <- ammonia[!(is.na(air.NH3))]
setkey(ammonia, NH3)
ammonia[,air.NH3.mean:= rollapply(ammonia$air.NH3, width = 10, FUN = mean, fill=NA)]
ammonia[,air.NH3.sd:= rollapply(ammonia$air.NH3, width = 10, FUN = sd, fill=NA)]
ammonia[,air.NH3.mean:= rollapply(air.NH3.mean, width = 10, FUN = mean, fill=NA)]
ammonia[,air.NH3.sd:= rollapply(ammonia$air.NH3.sd, width = 10, FUN = sd, fill=NA)]


g.1 <- ggplot(data, aes(x= NH3, y = air.NH3))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + xlab(expression('NH'[3]*'in previous core [ppm]'))
        + ylab(expression('NH'[3]*'in background air [ppm]'))
        # + scale_x_continuous(breaks = c(0, 10, 20, 30, 40))
)

g.1 <- (g.1
        # + facet_grid(Row ~ Column)
        + coord_cartesian(xlim= c(0,5000), ylim= c(0,1000))
        # + geom_ribbon(data=ammonia, aes(x = NH3, y = air.NH3.mean, ymin=air.NH3.mean - air.NH3.sd, ymax=air.NH3.mean + air.NH3.sd, fill= "red"), alpha=0.5)
        + geom_point(size=1, colour="black")
        + stat_smooth(n=50, color="red", se=F, size=1, method="lm", formula=y ~ poly(x, 10))
#         + geom_point(data=myH2O, aes(x = days, y = mymean), size=0.5, col="red")
)
g.1
myplot <- paste0(myNH3folder, "/", incubation, "_airNH3_vs_preNH3.png")
# myplot <- paste0(myNH3folder, "/", incubation, "_airNH3_vs_preNH3_bycore.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)
print(g.1)
dev.off()








### output folders
# plots
folder_thresholds <- paste0(IDASw_output_folder, "/", incubation, "_incubation_plots/thresholds")
dir.create(folder_thresholds, recursive = TRUE)

folder_flux_plots <- paste0(IDASw_output_folder, "/", incubation, "_incubation_plots")
dir.create(folder_flux_plots, recursive = TRUE)

folder_out_byTreatment <- paste0(IDASw_output_folder, "/", incubation, "_incubation_plots_byTreatment")
dir.create(folder_out_byTreatment, recursive = TRUE)
folder_out_byTreat.collapsed <- paste0(IDASw_output_folder, "/", incubation, "_incubation_plots_byTreatment/collapsed")
dir.create(folder_out_byTreat.collapsed, recursive = TRUE)

folder_out_byTreat.collapsed.mean <- paste0(IDASw_output_folder, "/", incubation, "_incubation_plots_byTreatment/collapsed_mean_plus_SE")
dir.create(folder_out_byTreat.collapsed.mean, recursive = TRUE)

folder_out_byTreat.collapsed.mean.FG <- paste0(IDASw_output_folder, "/", incubation, "_incubation_plots_byTreatment/collapsed_mean_plus_SE/2treatments")
dir.create(folder_out_byTreat.collapsed.mean.FG, recursive = TRUE)

# fluxes file
folder_out <- paste0(IDASw_output_folder, "/", incubation, "_fluxes")
dir.create(folder_out, recursive = TRUE)
fileout<- paste0(folder_out, "/", incubation, "_fluxes.dat")
fileout.wide<- paste0(folder_out, "/", incubation, "_interpol_fluxes_all_centred.dat")
QC.file <- paste0(folder_out, "/", incubation, "_QC_report.dat")

instant.by.species <- paste0(IDASw_output_folder, "/", incubation, "_fluxes/fluxes_by_species")
dir.create(instant.by.species, recursive = TRUE)

# cumulative fluxes file
folder_out <- paste0(IDASw_output_folder, "/", incubation, "_fluxes/cum_fluxes")
dir.create(folder_out, recursive = TRUE)
origin.cum<- paste0(folder_out, "/", incubation, "_cum_fluxes_origin_centred.dat")
water.cum<- paste0(folder_out, "/", incubation, "_cum_fluxes_water_centred.dat")
manure.cum<- paste0(folder_out, "/", incubation, "_cum_fluxes_manure_centred.dat")
all.cum<- paste0(folder_out, "/", incubation, "_cum_fluxes_all_centred.dat")

cum.by.species <- paste0(IDASw_output_folder, "/", incubation, "_fluxes/cum_fluxes/by_species")
dir.create(cum.by.species, recursive = TRUE)

cum.by.species.reset <- paste0(IDASw_output_folder, "/", incubation, "_fluxes/cum_fluxes/by_species/reset")
dir.create(cum.by.species.reset, recursive = TRUE)

cum.by.species.reset.origin.adapted <- paste0(IDASw_output_folder, "/", incubation, "_fluxes/cum_fluxes/by_species/reset_adpated")
dir.create(cum.by.species.reset.origin.adapted, recursive = TRUE)

# manual data removal
manual.remove <- paste0(input_path, "/", incubation, "_extra/manual_data_removal.dat")
################################################################################

data <- fread(input = file)
setkey(data,time)
data[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]