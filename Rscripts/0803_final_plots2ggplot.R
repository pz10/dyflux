folder_out <- folder_flux_plots
# range <- range(data$days, na.rm =T)
range <- range(myQCLdata[,days])
range[1]<- floor(range[1])
range[2]<- ceiling(range[2])

setkey(myQCLdata, days)
################################################################################
################################################################################
### CLD
################################################################################
# NO
i <- "NO"
state <- "fullscale"

myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: NO [µg-N/m2/h] ", state)

toplot <- myQCLdata[!is.na(NO)                    
                    , list(days, NO, mNO, Column, Row, Number)]


p <- qplot(days, NO, data=toplot, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()

# NO scaled 1000
i <- "NO"
state <- "scaled1000"
ylim <- c(-10,1000)

myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: NO [µg-N/m2/h] ", state)

toplot <- myQCLdata[!is.na(NO)
                    , list(days, NO, mNO, Column, Row, Number)]


p <- qplot(days, NO, data=toplot, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = ylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
# NO scaled 2500
i <- "NO"
state <- "scaled2500"
ylim <- c(-10,2500)

myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: NO [µg-N/m2/h] ", state)

toplot <- myQCLdata[!is.na(NO)                    
                    , list(days, NO, mNO, Column, Row, Number)]


p <- qplot(days, NO, data=toplot, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = ylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()

# NO scaled zero
i <- "NO"
state <- "scaled_zero"
ylim <- c(-10,10)

myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: NO [µg-N/m2/h] ", state)

toplot <- myQCLdata[!is.na(NO)
                    , list(days, NO, mNO, Column, Row, Number)]


p <- qplot(days, NO, data=toplot, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = ylim)
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
### QCL
################################################################################
#N2O
i <- "N2O"
state <- "fullscale"

myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: N2O [µg-N/m2/h] ", state)

toplot <- myQCLdata[!is.na(N2O)
                    , list(days, N2O, mN2O, Column, Row, Number)]

p <- qplot(days, N2O, data=toplot, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()

#N2O < scaled 1000
i <- "N2O"
state <- "scaled1000"
ylim <- c(-10,1000)

myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: N2O [µg-N/m2/h] ", state)

toplot <- myQCLdata[!is.na(N2O)
                    , list(days, N2O, mN2O, Column, Row, Number)]

p <- qplot(days, N2O, data=toplot, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = ylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()

#N2O < scaled 2500
i <- "N2O"
state <- "scaled2500"
ylim <- c(-10,2500)

myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: N2O [µg-N/m2/h] ", state)

toplot <- myQCLdata[!is.na(N2O)
                    , list(days, N2O, mN2O, Column, Row, Number)]

p <- qplot(days, N2O, data=toplot, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = ylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()

#N2O < scaled zero
i <- "N2O"
state <- "scaled_zero"
ylim <- c(-100,100)

myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: N2O [µg-N/m2/h] ", state)

toplot <- myQCLdata[!is.na(N2O)
                    , list(days, N2O, mN2O, Column, Row, Number)]

p <- qplot(days, N2O, data=toplot, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = ylim)
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
#CO2
i <- "CO2"
state <- "full scale"

myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: CO2 [mg-C/m2/h]", state)

toplot <- myQCLdata[ !is.na(CO2)
                     , list(days, CO2, mCO2, Column, Row, Number)]

p <- qplot(days, CO2, data = toplot, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()

#CO2 0-300
i <- "CO2"
state <- "scaled300"
ylim <- c(-10,300)

myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: CO2 [mg-C/m2/h]", state)

toplot <- myQCLdata[ !is.na(CO2)
                     , list(days, CO2, mCO2, Column, Row, Number)]

p <- qplot(days, CO2, data = toplot, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = ylim)
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
#CH4
i <- "CH4"
state <- "full scale"

myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: CH4 [µg-C/m2/h]", state)

toplot <- myQCLdata[ !is.na(CH4)
                     , list(days, CH4, mCH4, Column, Row, Number)]

p <- qplot(days, CH4, data = toplot, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range)

p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()

#CH4
i <- "CH4"
state <- "scaledzero"
ylim <- c(-60, 20)

myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: CH4 [µg-C/m2/h]", state)

toplot <- myQCLdata[ !is.na(CH4)
                     , list(days, CH4, mCH4, Column, Row, Number)]

p <- qplot(days, CH4, data = toplot, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = ylim)

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
#NH3
i <- "NH3"
state <- "full scale"

myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: NH3 [µg-N/m2/h rough]  ", state)

toplot <- myQCLdata[ !is.na(NH3)
                     , list(days, NH3, mNH3, Column, Row, Number)]

p <- qplot(days, NH3, data = toplot, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range)

p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()



#NH3
i <- "NH3"
state <- "scaled1000"
ylim <- c(0, 1000)

myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: NH3 [µg-N/m2/h rough]", state)

toplot <- myQCLdata[ !is.na(NH3)
                     , list(days, NH3, mNH3, Column, Row, Number)]

p <- qplot(days, NH3, data = toplot, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = ylim)

p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()

#NH3
i <- "NH3"
state <- "scaled100"
ylim <- c(0, 100)

myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: NH3 [µg-N/m2/h rough]", state)

toplot <- myQCLdata[ !is.na(NH3)
                     , list(days, NH3, mNH3, Column, Row, Number)]

p <- qplot(days, NH3, data = toplot, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = ylim)

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
#H2O
i <- "H2O"
state <- "full scale"

myplot <- paste0(folder_out, "/", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: H2O [g-H2O/core/day (rough)]  ", state)

toplot <- myQCLdata[ !is.na(H2O)
                     , list(days, H2O, mH2O, Column, Row, Number)]
toplot[,H2O:= H2O*core.area*24/1000]

p <- qplot(days, H2O, data = toplot, color=Number, facets=Row ~ Column, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range)

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


setkey(myQCLdata, time)
