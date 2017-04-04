# plot of calibration slopes and offsets compilation
calibration_compilation <- paste0(folder_calibration_plots, "/",incubation, "_cal_compilation.png")
png(filename = calibration_compilation, width = 1920, height = 1200, units = "px")
par(mfcol = c(4,2), cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )
with(data, {

        plot(NOoffsetRTO ~ days)
        plot(NOslopeRTO.50 ~ days)
        plot(NOslopeRTO.200 ~ days)
        plot(NOslopeRTO.500 ~ days)
        
        plot(PICslopeCO2 ~ days)
        abline(h=1, col="red")
        plot(PICslopeN2O ~ days)
        abline(h=1, col="red")
        plot(PICslopeCH4 ~ days)
        abline(h=1, col="red")
        plot(NOslope ~ days)
        abline(h=1, col="red")
        
        title(paste0(incubation, "_Calibration compilation: slopes and offsets"), line = -2, outer = TRUE)
} 
)
dev.off()
