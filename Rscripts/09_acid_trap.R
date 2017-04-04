#time recording
print(  paste0(Sys.time(), " start acid_trap.R")  )

source("acid_trap_estimation.R") # estimate NH3 trapped from PICARRO NH3 data
source("acid_trap_calculation.R") # calculates actual NH3 trapped from measured concentrations and flask volumes
