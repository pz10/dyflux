folder_out <- folder_thresholds
###
mydata <- data[!is.na(treatment), ]
setkey(mydata, days)
#                list(days,
#                     T, RH,
#                     flow, MEANflow, flowMAX, flowMIN, flowSD, 
#                     exhaust, exhaustMAX, exhaustMIN, exhaustSD,
#                     p_flow, MEANp_flow,
#                     Column, Row, Number)]
################################################################################
################################################################################
### FLOW related parameters (for QCL line)
################################################################################
###################################
# flow ...
i <- "flow"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, flow, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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
# flow ...
i <- "flow 350"
ylim <- c(350,400)
state <- "filter"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, flow, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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

# MEANflow ...
i <- "MEANflow"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, MEANflow, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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
# MEANflow ...
i <- "MEANflow 350"
ylim <- c(350,400)
state <- "filter"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, MEANflow, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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

# flowMAX ...
i <- "flowMAX"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, flowMAX, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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
# flowMAX ...
i <- "flowMAX 350"
ylim <- c(350,400)
state <- "filter"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, flowMAX, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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

# flowMIN ...
i <- "flowMIN"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, flowMIN, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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
# flowMIN ...
i <- "flowMIN 350"
ylim <- c(350,400)
state <- "filter"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, flowMIN, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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

# flowSD ...
i <- "flowSD"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, flowSD, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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
# flowSD ...
i <- "flowSD 1"
ylim <- c(0,1)
state <- "filter"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, flowSD, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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
###################################
# flowSD ...
i <- "flowSD 5"
ylim <- c(0,5)
state <- "filter"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, flowSD, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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

# exhaust ...
i <- "exhaust"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, exhaust, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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
# exhaust ...
i <- "exhaust 100"
ylim <- c(-5,100)
state <- "filter"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, exhaust, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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

# exhaustMAX ...
i <- "exhaustMAX"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, exhaustMAX, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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
# exhaustMAX ...
i <- "exhaustMAX 30"
ylim <- c(-5,30)
state <- "filter"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, exhaustMAX, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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

# exhaustMIN ...
i <- "exhaustMIN"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, exhaustMIN, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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
# exhaustMIN ...
i <- "exhaustMIN 50"
ylim <- c(-5,50)
state <- "filter"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, exhaustMIN, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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

# exhaustSD ...
i <- "exhaustSD"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, exhaustSD, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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
# exhaustSD ...
i <- "exhaustSD 5"
ylim <- c(0,5)
state <- "filter"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, exhaustSD, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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
###################################
# exhaustSD ...
i <- "exhaustSD 1"
ylim <- c(0,1)
state <- "filter"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, exhaustSD, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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


i <- "MEANp_flow"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, MEANp_flow, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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
# MEANp_flow ... (extrange values for 4 cores, (something wrong when calculatingthis parameter))
i <- "MEANp_flow 375"
ylim <- c(375,425)
state <- "filter"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, MEANp_flow, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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
################################################################################
### T and RH
################################################################################

# RH ...
i <- "RH"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, RH, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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
# RH ...
i <- "RH 30"
ylim <- c(30,50)
state <- "filter"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, RH, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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

# T ...
i <- "T"
state <- "raw"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, T, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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
# T ...
i <- "T 15"
ylim <- c(15,25)
state <- "filter"

myplot <- paste0(folder_out, "/", incubation, "_parameters_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: ", i)

p <- qplot(days, T, data=mydata, color=Number, facets=Row ~ Column, size=I(2),
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