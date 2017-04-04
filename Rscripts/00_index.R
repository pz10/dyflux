source("./Rscripts/000_configuration.R")
source("01_IDASw_wide.R")
source("02_IDASw_wide_60.R")
source("03_PICARRO60_sync.R")
source("04_fullsync.R")
source("05_calibration_correction.R")
source("06_full60corrected.R")
source("07_filtered360corrected.R")


################################################################################
### NOTES

# # 0
# # load configuration file with initial parameters
# source("000_configuration.R")
#
# # 1
# # transformation of IDASw files to wide format files (i.e. one time per row, channels at columns)
# # output written to the following folder: --> *_incubation_IDASw_daily
# source("01_IDASw_wide.R")
#
# # 2
# # merge all 60syncronized values from *_incubation_IDASw_daily to a single file
# # it also creates binary codes of valve position to identify which core was being measured
# # output written to the following folder:       --> *_incubation_IDASw_60
# source("02_IDASw_wide_60.R")
#
# # 3
# # Proccess of PIC raw data
# # it will merge all "sync"-midnight-syncronized values to a single file
# # (values == average, standard deviation and range of an average window of "aw"seconds length at "sync"-syncronized times)
# # output written to the following folder:       --> "incubation"_PIC (e.g. Ruth_PIC)
# source("03_PICARRO60_sync.R")
#
# # 4
# # merge of IDASw60 data with PIC data
# # to be used over outputs of "IDASw_wide_60.R" and "PICARRO60_sync.R"
# # output written to the following folder:       --> *_full_60 (e.g. *_full_60)
# #                                               --> no folder:  (*_CAL*.dat)
# #                                                               (*_TANK*.dat)
# #                                                               (*_TANK_noncorrected.png) plots
# # *it will merge IDASw60 data with PIC data at IDASw data times
# # *it also calculates, exports and plots raw TANK values (value of concentration inside the Tank for every TANK code)
# # *it also export cal1 and cal2 calibration values (*_CAL*.dat)
# source("04_fullsync.R")
#
# # 5
# # to be used over outputs of "fullsync.R" located at *_incubation_ful_60 folder/value60_IDASw_*
# # output written to the following folder:       --> *_incubation_full_60_slopes (e.g. A_incubation_full_60_slopes)
# #                                               --> *_calibration_plots (e.g. A_calibration_plots)
# # *It calculates slope and offset for correcting concentrations (from calibration gases)
# #  i.e.:("Li840slopeRTO_CO2", "Li840offsetRTO_CO2", "QCLslopeCO2", "QCLslopeN2O", "QCLslopeCH4", "NOslopeRTO", "NOoffsetRTO")
# # *It also export graphs into *_calibration_plots folder
# # *It also calculates MEANflow and MEANp_flow from flow and p_flow values
# #  (mean flow of [0:5] minutes before the 6-minutes-midnight-syncronized values)
# source("05_calibration_correction.R")
#
# # 6
# # to be used over outputs of "calibration_correction.R" located at *_incubation_ful_60_slopes
# # output written to the following folder:       --> *_incubation_full_60_corrected (e.g. A_incubation_full_60_corrected)
# #                                               --> no folder:  (*_incubation_TANK_corrected.dat)
# #                                                               (*_incubation_TANK_corrected.png) plots
# # *it corrects concentration values taking into account slope and offset calculated in "calibration_correction.R
# # *it also recalculates, exports and plots TANK values using recalculated concentrations
# source("06_full60corrected.R")
#
# # 7
# # to be used over outputs of "ful60corrected.R" located at *_incubation_full_60_corrected
# # output written to the following folder:       --> *_incubation_filtered_360_corrected (e.g. A_incubation_filtered_360_corrected)
#
# source("07_filtered360corrected.R")
#
# # 8 to be run manually
# # source("08_final_plots.R")
#
# # 9
# # source("09_acid_trap.R")

################################################################################

# used R files:

# 1
# IDASw_wide.R

# 2
# IDASw_wide_60.R
#         binary.R

# 3
# PICARRO60_sync.R

# 4
# fullsync.R
#       TANKcutsINSERT.R
#       TANK.R
#               core_filtration.R (for 'IDASw_filter1000' function)
#               copyTANK_allvalues.R
#               TANK_notvalid_removal.R (set quality thersholds for TANK values)
#               copyTANK_allvalues.R (rewriting)
#               copyTANKsd.R
#       calibration.R
#               cal1.R


# 5
# calibration_correction.R
#         cal2correction.R
#         cal1correction.R
#               cal1.summary.R
#               cal1plots.R
#         MeanFlows.R

# 6
# full60corrected.R
#         TANK.R
#               core_filtration.R (for 'IDASw_filter1000' function)
#               copyTANK_allvalues.R
#               TANK_notvalid_removal.R (set quality thersholds for TANK values)
#               copyTANK_allvalues.R (rewriting)
#               copyTANKsd.R

# 7
# filtered360corrected.R
#         core_filtration.R
#         flux.R


# 8
# final_plots.R
#       solve_core_translocation.R
#       add_treatment_columns.R
#       times_rain_events.R
#       (final_plots_flowparameters.R)
#       (final_plots_deviceparameters.R)
#       parameter_thresholds.R

#       (final_plots2ggplot.R)
#       (final_plots_byTreatment.R)

#       (add_water_content.R)
#       (cum_fluxes.R)
#       (cum_fluxes_byspecies.R)
#       (flux_wide_byspecies.R)
#       (final_plots_byTreatment_summaries.R)
#       (final_CUMplots_byTreatment_summaries.R)
#       (final_plots_byTreatment_summaries_2treat.R)
#       (final_CUMplots_byTreatment_summaries_2treat.R)





# 9
# acid_trap.R

#       acid_trap_estimation.R
#               -> acid_trap_estimation.dat
#               -> acid_trap_estimation_summary.dat

#       acid_trap_calculation.R


# airNH3_vs_preN3.R
