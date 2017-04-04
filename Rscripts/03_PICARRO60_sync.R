# Proccess of PICARRO raw data
# it will merge all "sync"-midnight-syncronized values to a single file (e.g. "sync" -->360; 360 seconds midnight syncronized values)
# (values == average, standard deviation and range of an average window of "aw"seconds length at "sync"-syncronized times)

# time recording
print(Sys.time())
###
#           initial parameters

#1)   set target input folder and target output folder (Use /, not \).
#     Output folder shoul not be equal to or contained within input folder
#     Input folder shoul only contain the files to be extracted
folder <- PIC_input_folder
folder_out <- PIC_output_folder
dir.create(folder_out, recursive = T)

# #2)   set average window in seconds
# aw <- 20
# 
# #3)   set midnight-syncronized time interval in seconds (i.e. evrey 6 minutes from midnight is 360)
# #     (sync from 15 to 3600 seconds)
# sync <- 60
# # set duration of each file in seconds
# fileseconds <- 3600
###
#4)   set desire date and time format (date_format, time_format) and date origin (date_origin)
#     see (http://www.stat.berkeley.edu/classes/s133/dates.html) for date formats
# date_origin<-"1970-01-01 00:00:00"
# date_format <- "%Y-%m-%d %H:%M:%S"
#     UTC (Coordinated Universal Time) offset of the winter-time of our time zone
#     data must be without DST(day saving time ) transition (e.g. always winter-time)
# UTC_offset <- 3600

###
#           Start of data extraction
#
# read previous output file(if any)
previous_files <- list.files (path=folder_out, full.names = TRUE, recursive = FALSE)
myfile <- previous_files[ grep("avg",previous_files) ]

if(length(myfile) == 1){
        
        pdata <- fread(myfile)
        pdata[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
        mylasttime <- tail(pdata$time,1)
        mylast <- format(mylasttime, "%Y%m%d")
        
        # explore input raw data; select just from last availabe day on
        files <- list.files (path=folder, full.names = TRUE, recursive = TRUE)
        just_files <- list.files (path=folder, full.names = FALSE, recursive = TRUE)
        
        milestone <- grep(mylast, files)[1]
        
        files <- files[milestone:length(files)]
        just_files <- just_files[milestone:length(just_files)]
        
}else{
        # explore input raw data; select all
        #all files of any directory within "folder" will be listed in "files" vector
        #(alphabetical order of folders; alphabetical order within a folder)-->chronological order within PICARRO's output frame
        files <- list.files (path=folder, full.names = TRUE, recursive = TRUE)
        just_files <- list.files (path=folder, full.names = FALSE, recursive = TRUE)  
}

#read only target columns (more efficient)
mycols <- rep("NULL", 38)
mycols[c(6, N2O_column, 27, 29:31)] <- "numeric" # N2O_column -->  22(1s; N2O_dry); 24(60s; N2O_dry1min) 

#1st file reading
data <- read.table (file=files[1], header = T, dec = ".", colClasses = mycols, comment.char = "", nrows = 4000)

#inicialize output matrix (more efficient)
nrows <- ((fileseconds/sync)+1)*length(files)
heading_avg <- c("PICARRON2O", "PICARROCO2", "PICARROCH4", "PICARROH2O", "PICARRONH3")
heading_sd <- c("PICARRON2O_sd", "PICARROCO2_sd", "PICARROCH4_sd", "PICARROH2O_sd", "PICARRONH3_sd")
heading_rg <- c("PICARRON2O_rg", "PICARROCO2_rg", "PICARROCH4_rg", "PICARROH2O_rg", "PICARRONH3_rg")
heading <- c("EPOCH_TIME", heading_avg, heading_sd, heading_rg)
dim_names <- list(NULL, heading)
ncolumns <- length(heading)

species <- matrix(data = NA, nrow = nrows, ncol = ncolumns, byrow = TRUE, dimnames = dim_names)

#start data processing

count <- 0
for(i in 2:length(files)){
        #for(i in 2:100){
        #open next file
        data_next <- read.table (file=files[i], header = T, dec = ".", colClasses = mycols, comment.char = "", nrows = 4000)
        
        #append first "aw-1" lines from next file to current file
        l <-length (data[,1])
        data[(l+1):(l+aw-1),] <- data_next[1:(aw-1),]
        #find sync_indeces
        a0 <- floor(data[,"EPOCH_TIME"])%%sync
        a1 <- c(a0[1],a0[1:(length(a0)-1)])
        flag<-which((a1-a0)>(sync-5)) #indeces_sync
        flag <- flag[flag > (aw-1)] #delete values contained in first "aw-1" seconds (they were appended to previous file)
        #
        lf <- length (flag)
        
        #
        #data processing at these indeces
        # epoch_time: columns 1  
        species[(count+1):(count+lf),1] <- data[flag,"EPOCH_TIME"]
        
        # rest of columns: average over the set average window ("aw")i.e. flagged time and aw seconds before 
        for (j in flag){
                if ( (data[j,1]-data[(j-aw+1),1])<(aw+5) ){
                        count <-count+1
                        species[count,2:6] <- colMeans(data[(j-aw+1):j,2:6],2)
                        species[count,7:11] <- apply(data[(j-aw+1):j,2:6],2,sd) 
                        species[count,12:16] <- apply(data[(j-aw+1):j,2:6],2,function(x) max(x)-min(x)) 
                        
                }else{
                        count <-count+1      
                        species[count,2:6] <- NA
                        species[count,7:11] <- NA
                        species[count,12:16] <- NA
                }
                
        }
        
        #print confirmation
        print(paste(as.character(i-1)," of ",as.character(length(files)),"  ",just_files[i-1]))
        
        #print(Sys.time())
        
        #move data_next to data    
        data <- data_next
}
#################################################################################
###################################################################################################################
copy.species <- data.table(copy(species)) # species <- copy(copy.species)
#processing of last file in input folder (tail(files, n=1))
#find sync_indeces
a0 <- floor(data[,"EPOCH_TIME"])%%sync
a1 <- c(a0[1],a0[1:(length(a0)-1)])
flag<-which((a1-a0)>(sync-5)) #indeces_sync
flag <- flag[flag > (aw-1)] #delete values contained in first "aw-1" seconds (they were appended to previous file)
#
lf <- length (flag)
#
#data processing at these indeces
# epoch_time: columns 1  
species[(count+1):(count+lf),1] <- data[flag,"EPOCH_TIME"]

# rest of columns: average over the set average window ("aw")i.e. flagged time and aw seconds before 
for (j in flag){
        if ((data[j,1]-data[(j-aw+1),1])<(aw+5)){
                count <-count+1
                species[count,2:6] <- colMeans(data[(j-aw+1):j,2:6],2)
                species[count,7:11] <- apply(data[(j-aw+1):j,2:6],2,sd) 
                species[count,12:16] <- apply(data[(j-aw+1):j,2:6],2,function(x) max(x)-min(x)) 
                
        }else{
                count <-count+1      
                species[count,2:6] <- NA
                species[count,7:11] <- NA
                species[count,12:16] <- NA
        }
        
}

#print confirmation
print(paste(as.character(length(files))," of ",as.character(length(files)),"  ",just_files[length(files)]))

print(Sys.time())

##############
#select written rows only
species <- species[1:count,]
#convert into dataframe
species <- data.frame(species, stringsAsFactors = FALSE)
#change units
species[,"PICARRON2O"] <- species[,"PICARRON2O"]*1000
species[,"PICARROCH4"] <- species[,"PICARROCH4"]*1000
species[,"PICARROH2O"] <- species[,"PICARROH2O"]*10000

species[,"PICARRON2O_sd"] <- species[,"PICARRON2O_sd"]*1000
species[,"PICARROCH4_sd"] <- species[,"PICARROCH4_sd"]*1000
species[,"PICARROH2O_sd"] <- species[,"PICARROH2O_sd"]*10000

species[,"PICARRON2O_rg"] <- species[,"PICARRON2O_rg"]*1000
species[,"PICARROCH4_rg"] <- species[,"PICARROCH4_rg"]*1000
species[,"PICARROH2O_rg"] <- species[,"PICARROH2O_rg"]*10000


#round numbers
species[,2:16] <- sapply(species[,2:16], function(x) formatC(x, format = "f", digits = 3) )

#create date column from epoch_time
#UTC time zone has to be used to avoid DST transitions
#conversion to UTC
species[,"EPOCH_TIME"] <- species[,"EPOCH_TIME"] + UTC_offset
names(species) <- c("PICARROepoch_time", heading_avg, heading_sd, heading_rg)
# names(species) <- c("epoch_time", heading_avg, heading_sd, heading_rg)
#create date column from UTC epoch_times
species[,"epoch_time_round"] <- round(species[,"PICARROepoch_time"]/60)*60
species[,"time"] <- format(as.POSIXct(species[,"epoch_time_round"], origin = date_origin, tz = "UTC"),format=date_format)
#species[,"EPOCH_TIME"] <- format(as.POSIXct(species[,"EPOCH_TIME"], origin = date_origin),format=time_format)
#round epoch_time
species[,"PICARROepoch_time"] <- species[,"PICARROepoch_time"] - UTC_offset
species[,"PICARROepoch_time"] <- sapply(species[,"PICARROepoch_time"], function(x) formatC(x, format = "f", digits = 3) )


# rename column names to math QCL script
new_heading_avg <- c("N2O_ppb", "CO2_ppm", "CH4_ppb", "H2O_ppm", "NH3_ppb")
new_heading_sd <- c("sd_N2O_ppb", "sd_CO2_ppm", "sd_CH4_ppb", "sd_H2O_ppm", "sd_NH3_ppb")
new_heading_rg <- c("rg_N2O_ppb", "rg_CO2_ppm", "rg_CH4_ppb", "rg_H2O_ppm", "rg_NH3_ppb")

names(species) [names(species) %in% heading_avg] <- new_heading_avg
names(species) [names(species) %in% heading_sd] <- new_heading_sd
names(species) [names(species) %in% heading_rg] <- new_heading_rg

names(species)[1]<- "PICepoch_time"

# append to existing file if existing
if(length(myfile) == 1){
        species <- data.table(species)
        species[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
        species <- species[time > mylasttime]
}
species <- data.table(species)

avg <- species[,c("time", "PICepoch_time", new_heading_avg), with=F]
stdev <- species[,c("time", "PICepoch_time", new_heading_sd), with=F]
rg <- species[,c("time", "PICepoch_time", new_heading_rg), with=F]

if(length(myfile) == 1){
        avg <- rbindlist(list(pdata, avg))
        
        myfile <- previous_files[ grep("stdev",previous_files) ]
        pdata <- fread(myfile)
        pdata[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
        stdev <- rbindlist(list(pdata, stdev))
        
        myfile <- previous_files[ grep("range",previous_files) ]
        pdata <- fread(myfile)
        pdata[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
        rg <- rbindlist(list(pdata, rg))
}

#write into output folders
file_avg<- paste0(folder_out, "/", incubation, "_PIC60_avg.dat", collapse = NULL)
file_sd<- paste0(folder_out, "/", incubation, "_PIC60_stdev.dat", collapse = NULL)
file_rg<- paste0(folder_out, "/", incubation, "_PIC60_range.dat", collapse = NULL)

print(Sys.time())
write.table(avg, file= file_avg, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
write.table(stdev, file= file_sd, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
write.table(rg, file= file_rg, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")

#print confirmation
print("")
if(length(myfile) == 1){
        print("output data was appended to existing files:")
}else{
        print("output files were written to:")   
}
print(file_avg)
print(file_sd)
print(file_rg)

# # remove data from environment
# rm(species)
# rm(avg)
# rm(stdev)
# rm(rg)
# rm(pdata)
# rm(data)
# rm(data_next)

print(Sys.time())