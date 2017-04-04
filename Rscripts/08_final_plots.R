        #time recording
        print(  paste0(Sys.time(), " start final_plots.R")  )
        ###
        ### input file
        folder <- paste0(IDASw_output_folder, "/", incubation, "_filtered_360_corrected")
        file <- paste0(folder, "/", incubation, "_filtered360", "_corrected", ".dat")
        treatment_file <- paste0(input_path, "/", incubation, "_extra/", incubation, "_treatments.dat")
        
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
        
        ### in case it is needed it can implemented
        # source("solve_core_translocation.R")
        
        source("add_treatment_columns.R")
        source("times_rain_events.R")
        
        ### check
        #       final_plots_flowparameters.R
        #       final_plots_deviceparameters.R
        
        source("parameter_thresholds.R") # to be edited after having a look to output plots of *parameter.R
        source("manual_removal.R")
        
myQCLdata <- copy( data[!is.na(treatment), ] )
        # source("final_plots2ggplot.R")
        # source("final_plots_byTreatment.R")
# source("add_water_content.R")
################################################################################

data <- data[!is.na(treatment), ]

# write fluxes
setnames(data, "days", "days.water")
times <- c("time", "days.origin", "days.water", "days.manure")

source("cum_fluxes.R")
source("cum_fluxes_byspecies.R")
source("flux_wide_byspecies.R")
source("final_plots_byTreatment_summaries.R")
source("final_CUMplots_byTreatment_summaries.R")
source("final_plots_byTreatment_summaries_2treat.R")
source("final_CUMplots_byTreatment_summaries_2treat.R")


data[,mH2O:= mH2O*core.area*24/1000]

treatments <- c("CORE", "treatment")
# treatments <- c("CORE", "treatment", "fertilizer", "precipitation", "tillage")
fluxes <- c("NO", "N2O", "CO2", "CH4", "NH3",
            "mNO", "mN2O", "mCO2", "mCH4", "mNH3", "mH2O")
# water <- c( "weight", "drysoil", "drysoil_est", "drysoil_dif", 
#             "water", "addedwater",
#             "WCv", "WCm", "WCm_est",
#             "WFPS", "WFPSg",
#             "WFPS_0", "WCv_0",
#             "WFPS_f", "WCv_f")

to.delete <- !names(data) %in% c(times, treatments, fluxes)
to.delete <- names(data)[to.delete]
set(data, j = to.delete, value=NULL)

new.order <- c(times, treatments, fluxes)
setcolorder(data, new.order)

no.format <- c("time", treatments)
no.format <- names(data) %in% no.format
to.format <- names(data)[!no.format]
to.format6 <- to.format [grep("days",to.format)]
to.format4 <- to.format [-grep("days",to.format)]

data[, (to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4, flag = 0)), .SDcols = to.format4]
data[, (to.format6):= lapply(.SD, function(x) formatC(x, format = "f", digits = 6, flag = 0)), .SDcols = to.format6]

# chronological ordering of data
setkey(data, time)

############################
#
#write into output folder
write.table(data, file= fileout, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")

print(  paste0( Sys.time(), "  fluxes file was written to:  ")  )
print(fileout)
