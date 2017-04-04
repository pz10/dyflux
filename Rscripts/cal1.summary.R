# get summary No calibrations
times.cal1 <- to.interpolate.0$time
to.bind0 <- data.table(time.NO.0 = times.cal1[1])
to.bind0[,days:= as.numeric(NA)]
to.bind0[,NO.0:=  as.numeric(NA)]
to.bind0[,NO.50:=  as.numeric(NA)]
to.bind0[,NO.200:=  as.numeric(NA)]
to.bind0[,NO.500:=  as.numeric(NA)]

for(i in 1:length(times.cal1)){
        to.bind <- copy(to.bind0)
        mytime<- unclass(times.cal1[i])[1]
        to.bind[,time.NO.0:= times.cal1[i]]
        if(mytime %in% to.interpolate.0$time == T) to.bind[,NO.0:= to.interpolate.0[unclass(time)==mytime, CLD_NO]]
        if((mytime + 180*60) %in% to.interpolate.50$time == T) to.bind[,NO.50:= to.interpolate.50[unclass(time)==mytime + 180*60, CLD_NO]]
        if((mytime + 2*180*60) %in% to.interpolate.200$time == T) to.bind[,NO.200:= to.interpolate.200[unclass(time)==mytime + 2*180*60, CLD_NO]]
        if((mytime + 3*180*60) %in% to.interpolate.500$time == T) to.bind[,NO.500:= to.interpolate.500[unclass(time)==mytime + 3*180*60, CLD_NO]]
        
        if(i==1){
                myNO <- copy(to.bind)
        }else{
                myNO <- rbindlist(list(myNO, to.bind))
        }
}
myNO[,days:= as.numeric( difftime(time.NO.0, time.water, units= "days") )]
myblends <- c(0, 50, 200, 500)
myNO[,NOslopeRTO.50 := 50/NO.50]
myNO[,NOslopeRTO.200 := 200/NO.200]
myNO[,NOslopeRTO.500 := 500/NO.500]

myNO[,NOslope.lm:= as.numeric(NA)]
myNO[,NOintercept.lm:= as.numeric(NA)]

for(i in 1:nrow(myNO)){
        x <- c(myNO[i, NO.0], myNO[i, NO.50], myNO[i, NO.200], myNO[i, NO.500])
        y <- c(0,50,200,500)
        mymodel <- lm(y ~x)
        myNO[i,NOslope.lm:= mymodel[[1]][[2]]]
        myNO[i,NOintercept.lm:= mymodel[[1]][[1]]]
        myNO[i, R2.lm:= summary(mymodel)$r.squared]
}

mydata <- copy(myNO)

no.format <- c("time.NO.0", "days")
no.format <- copy(names(mydata)) %in% no.format
to.format <- copy(names(mydata))[!no.format]
to.format6 <- c("days")

mydata[,(to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format] #digit format
mydata[,(to.format6):= lapply(.SD, function(x) formatC(x, format = "f", digits = 6)), .SDcols = to.format6] #digit format
write.table(mydata, file= cal1summary, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
print(  paste0(Sys.time(), " CAL1 summary file was written to:  ")  )
print(cal1summary)
