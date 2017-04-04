folder_out <- folder_thresholds
###
mydata <- data[!is.na(treatment), ]
setkey(mydata, days)
################################################################################
################################################################################
### CLD
################################################################################
################################################################################
# CLD_Temp ...
i <- "CLD_Temp"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_device_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, CLD_Temp, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
###################################
# # CLD_Temp ...
i <- "CLD_Temp filtered"
ylim <- c(44,46)
state <- "filtered"

myplot <- paste0(folder_out, "/", incubation, "_device_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, CLD_Temp, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, ylim = ylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################
# CLD_Press ...
i <- "CLD_Press"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_device_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, CLD_Press, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, , na.rm = TRUE)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
###################################
# CLD_Press ...
# i <- "CLD_Press 300"
# ylim <- c(300,350)
# state <- "filter"
# 
# myplot <- paste0(folder_out, "/", incubation, "_device_", paste(i), "_", state, ".png")
# png(filename = myplot, width = 1920, height = 1200, units = "px")
# title <- paste0(incubation, " incubation: ", i)
# 
# p <- qplot(days, CLD_Press, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
#            xlab="", ylab="", main= title, , na.rm = TRUE, ylim = ylim)
# p +
#         theme_bw() +
#         scale_colour_manual(values = c("black","red")) +
#         scale_shape_manual(values = c(6,1)) +
#         theme(legend.position = c(1, 1))+
#         theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
#         theme(strip.text = element_text(size = 15)) +
#         theme(axis.text.y = element_text(size = 12))
# 
# 
# dev.off()
################################################################################
# CLD_warning ...
i <- "CLD_warning"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_device_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, CLD_warning, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, , na.rm = TRUE)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################
# CLD_error ...
i <- "CLD_error"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_device_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, CLD_error, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, , na.rm = TRUE)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################
################################################################################
################################################################################
### QCL
################################################################################
# rg_N2O_ppb ...
i <- "rg_N2O_ppb"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_device_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, rg_N2O_ppb, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, , na.rm = TRUE)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
###################################
# rg_N2O_ppb ...
i <- "rg_N2O_ppb 20"
ylim <- c(0,20)
state <- "filter"

myplot <- paste0(folder_out, "/", incubation, "_device_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, rg_N2O_ppb, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, , na.rm = TRUE, ylim = ylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################
# sd_N2O_ppb ...
i <- "sd_N2O_ppb"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_device_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, sd_N2O_ppb, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, , na.rm = TRUE)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
###################################
# sd_N2O_ppb ...
i <- "sd_N2O_ppb 5"
ylim <- c(0,5)
state <- "filter"

myplot <- paste0(folder_out, "/", incubation, "_device_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, sd_N2O_ppb, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, , na.rm = TRUE, ylim = ylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################
# rg_CO2_ppm ...
i <- "rg_CO2_ppm"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_device_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, rg_CO2_ppm, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, , na.rm = TRUE)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
###################################
# rg_CO2_ppm ...
i <- "rg_CO2_ppm 20"
ylim <- c(0,20)
state <- "filter"

myplot <- paste0(folder_out, "/", incubation, "_device_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, rg_CO2_ppm, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, , na.rm = TRUE, ylim = ylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################
# sd_CO2_ppm ...
i <- "sd_CO2_ppm"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_device_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, sd_CO2_ppm, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, , na.rm = TRUE)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
###################################
# sd_CO2_ppm ...
i <- "sd_CO2_ppm 5"
ylim <- c(0,5)
state <- "filter"

myplot <- paste0(folder_out, "/", incubation, "_device_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, sd_CO2_ppm, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, , na.rm = TRUE, ylim = ylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################
# rg_CH4_ppb ...
i <- "rg_CH4_ppb"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_device_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, rg_CH4_ppb, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, , na.rm = TRUE)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
###################################
# rg_CH4_ppb ...
i <- "rg_CH4_ppb 20"
ylim <- c(0,20)
state <- "filter"

myplot <- paste0(folder_out, "/", incubation, "_device_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, rg_CH4_ppb, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, , na.rm = TRUE, ylim = ylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################
# sd_CH4_ppb ...
i <- "sd_CH4_ppb"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_device_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, sd_CH4_ppb, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, , na.rm = TRUE)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
###################################
# sd_CH4_ppb ...
i <- "sd_CH4_ppb 10"
ylim <- c(0,10)
state <- "filter"

myplot <- paste0(folder_out, "/", incubation, "_device_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, sd_CH4_ppb, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, , na.rm = TRUE, ylim = ylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()