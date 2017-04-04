#time recording
print(  paste0(Sys.time(), " start IDASw_wide.R")  )
###

##     auto-set target input folder and target output folder

##           create output folders (for min, max, stdev and value)
#
out_min <- paste0(IDASw_output_folder, "/", incubation, "_IDASw_daily", "/", "min60")
out_max <- paste0(IDASw_output_folder, "/", incubation, "_IDASw_daily", "/", "max60")
out_stdev <- paste0(IDASw_output_folder, "/", incubation, "_IDASw_daily", "/", "stdev60")
out_value <- paste0(IDASw_output_folder, "/", incubation, "_IDASw_daily", "/", "value60")

dir.create(out_min, recursive = TRUE)
dir.create(out_max, recursive = FALSE, showWarnings = FALSE)
dir.create(out_stdev, recursive = FALSE, showWarnings = FALSE)
dir.create(out_value, recursive = FALSE, showWarnings = FALSE)

##           Check for files already transformed
#
out_files <- list.files (path=out_min, full.names = FALSE)
done <- length(out_files)

#           Start of data extraction
#
#all files of any directory within "IDASw_input_folder" will be listed in "files" vector
#(alphabetical order of folders; alphabetical order within a folder)-->chronological order within PICARRO's output frame
files <- list.files (path=IDASw_input_folder, full.names = TRUE, recursive = FALSE)
just_files <- list.files (path=IDASw_input_folder, full.names = FALSE, recursive = FALSE)


# for(i in (done+1):length(files) ){
if(done==0){done<-1}
for(i in (done):length(files) ){
        
        data <- read.csv2 (
                file=files[i], header=F, dec=".", stringsAsFactors=FALSE,
                col.names=c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11"),
                nrows = IDASw_nrow
        )
        
        #find 1st value and target columns
        check_ini <- data [,1] == as.character("[Values]")
        first <- which(check_ini)+2
        
        d_value <- data[first:length(data[,1]),1:3]
        d_min <- data[first:length(data[,1]),c(1,2,4)]
        d_max <- data[first:length(data[,1]),c(1,2,5)]
        d_stdev <- data[first:length(data[,1]),c(1,2,6)]
        
        #reshape to wide format
        d_value_wide <- reshape(d_value, v.names = "V3", idvar = "V2",timevar = "V1", direction = "wide")
        d_min_wide <- reshape(d_min, v.names = "V4", idvar = "V2",timevar = "V1", direction = "wide")
        d_max_wide <- reshape(d_max, v.names = "V5", idvar = "V2",timevar = "V1", direction = "wide")
        d_stdev_wide <- reshape(d_stdev, v.names = "V6", idvar = "V2",timevar = "V1", direction = "wide")
        
        #change column names
        names(d_value_wide)[1]<- paste("V3.0_time")
        old_names <- strsplit(names(d_value_wide), split='.', fixed=TRUE)
        new_names <- sapply(old_names,function(x) x[2])
        new_names <- paste0("C", new_names)
        new_names[1] <- "0_time"
        
        names(d_value_wide) <- new_names
        names(d_min_wide) <- new_names
        names(d_max_wide) <- new_names
        names(d_stdev_wide) <- new_names
        #    names(d_value_wide) <- sapply(old_names,function(x) x[2])
        
        
        #
        #take only 60seconds syncronized values
        d_value_wide[,1]<-as.numeric(d_value_wide[,1])
        is60 <- floor(d_value_wide[,1])%%60==0  #it should be the same vector for all 4 d_*_wide
        
        d_value_wide <- d_value_wide[is60,]    
        d_min_wide <- d_min_wide[is60,]
        d_max_wide <- d_max_wide[is60,]
        d_stdev_wide <- d_stdev_wide[is60,]
        
        #time formating
        #create date column from epoch_time
        #UTC time zone has to be used to avoid DST transitions
        #conversion to UTC
        d_value_wide[,1] <- d_value_wide[,1] + UTC_offset
        #create "date_format" column from UTC epoch_times
        times <- format(as.POSIXct(d_value_wide[,1], origin = date_origin, tz = "UTC"),format=date_format)
        
        d_value_wide["01_epoch_time"]<-d_value_wide[,1] #new column "01_epoch_time"
        d_min_wide["01_epoch_time"]<-d_value_wide[,1] #new column "01_epoch_time"
        d_max_wide["01_epoch_time"]<-d_value_wide[,1] #new column "01_epoch_time"
        d_stdev_wide["01_epoch_time"]<-d_value_wide[,1] #new column "01_epoch_time"
        
        d_value_wide[,1]<-times
        d_min_wide[,1]<-times
        d_max_wide[,1]<-times
        d_stdev_wide[,1]<-times
        
        #column rearrangement and rename of time and epoch_time columns
        d_value_wide <- d_value_wide[,order(names(d_value_wide))]
        names(d_value_wide)[1]<- paste("time")
        names(d_value_wide)[2]<- paste("epoch_time")
        
        d_min_wide <- d_min_wide[,order(names(d_min_wide))]
        names(d_min_wide)[1]<- paste("time")
        names(d_min_wide)[2]<- paste("epoch_time")
        
        d_max_wide <- d_max_wide[,order(names(d_max_wide))]
        names(d_max_wide)[1]<- paste("time")
        names(d_max_wide)[2]<- paste("epoch_time")
        
        d_stdev_wide <- d_stdev_wide[,order(names(d_stdev_wide))]
        names(d_stdev_wide)[1]<- paste("time")
        names(d_stdev_wide)[2]<- paste("epoch_time")
        
        #column values converted to numbers (except for time column)
        wide_length<-length(names(d_value_wide))
        
        d_value_wide[,2:wide_length]<-sapply(d_value_wide[,2:wide_length], as.numeric )
        d_min_wide[,2:wide_length]<-sapply(d_min_wide[,2:wide_length], as.numeric )
        d_max_wide[,2:wide_length]<-sapply(d_max_wide[,2:wide_length], as.numeric )
        d_stdev_wide[,2:wide_length]<-sapply(d_stdev_wide[,2:wide_length], as.numeric )
        
        #column number of digits formating
        digit4 <- c("C1001", "C1002", "C1010", "C1011", "C1020", "C1030", "C1101", "C1102",
                    "C2001", "C2002", "C2003", "C2004", "C2005", "C2006")
        digit2 <- c("C1201", "C1202", "C1203", "C1204", "C1205", "C1206", "C1207", "C1208", "C1209", "C1210",
                    "C4001", "C4002",  "C4011", "C4012", "C5001")
        # check if proposed channel actually exist in this file
        is.col4 <- !is.na(  match(digit4, new_names)  )
        is.col2 <- !is.na(  match(digit2, new_names)  )
        digit4 <- digit4[is.col4]
        digit2 <- digit2[is.col2]
        
        d_value_wide[,digit4] <- sapply(d_value_wide[,digit4], function(x) formatC(x, format = "f", digits = 4) )
        d_value_wide[,digit2] <- sapply(d_value_wide[,digit2], function(x) formatC(x, format = "f", digits = 2) )
        
        d_min_wide[,digit4] <- sapply(d_min_wide[,digit4], function(x) formatC(x, format = "f", digits = 4) )
        d_min_wide[,digit2] <- sapply(d_min_wide[,digit2], function(x) formatC(x, format = "f", digits = 2) )
        
        d_max_wide[,digit4] <- sapply(d_max_wide[,digit4], function(x) formatC(x, format = "f", digits = 4) )
        d_max_wide[,digit2] <- sapply(d_max_wide[,digit2], function(x) formatC(x, format = "f", digits = 2) )
        
        d_stdev_wide[,digit4] <- sapply(d_stdev_wide[,digit4], function(x) formatC(x, format = "f", digits = 4) )
        d_stdev_wide[,digit2] <- sapply(d_stdev_wide[,digit2], function(x) formatC(x, format = "f", digits = 2) )
        
        #write to output file
        d_value_wide_file<-paste0(out_value,"/","value60_", just_files[i])
        d_min_wide_file<-paste0(out_min,"/","min60_", just_files[i])
        d_max_wide_file<-paste0(out_max,"/","max60_", just_files[i])
        d_stdev_wide_file<-paste0(out_stdev,"/","stdev60_", just_files[i])
        
        write.table(d_value_wide, file= d_value_wide_file, row.names = FALSE, sep = "\t", quote = FALSE)
        write.table(d_min_wide, file= d_min_wide_file, row.names = FALSE, sep = "\t", quote = FALSE)
        write.table(d_max_wide, file= d_max_wide_file, row.names = FALSE, sep = "\t", quote = FALSE)
        write.table(d_stdev_wide, file= d_stdev_wide_file, row.names = FALSE, sep = "\t", quote = FALSE)
        
        #print confirmation
        print(paste(as.character(i)," of ",as.character(length(files)),"  ",just_files[i]))
}

if(done > 0){
        print(
                paste0("the following ", done, " output files were already transformed (value, min, max, stdev):")
        )
        print(out_files)    
}


print(
        paste0(length(files)-done+1, " - output files (value, min, max, stdev) were written to:")
)
print(paste0(IDASw_output_folder,"/","*60_*.dat", collapse=NULL))
print(  paste0(Sys.time(), " IDASw_wide.R is finnished")  )
