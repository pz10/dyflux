#to be used over outputs of IDASw_wide.R (i.e.output files -value, min, max, stdev- in the form of value60_*.dat")
#it will merge all 60syncronized values to a single file

#time recording
print(  paste0(Sys.time(), " start IDASw_wide_60.R")  )
###

##           create output folders (for min, max, stdev and value)
#
folder_out <- paste0(IDASw_output_folder, "/", incubation, "_IDASw_60")
dir.create(folder_out, recursive = TRUE)

##           define input folders (for min, max, stdev and value)
#

folder_in <- paste0(IDASw_output_folder, "/", incubation, "_IDASw_daily")
target <- c("value60", "max60", "min60", "stdev60")

##
#
#           Start of data extraction
#
for (i in target){
        # read previous output file(if any)
        previous_files <- list.files (path=folder_out, full.names = TRUE, recursive = FALSE)
        myfile <- previous_files[ grep(i,previous_files) ]

        if(length(myfile) == 1){
                
                pdata <- fread(myfile)
                pdata[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
                mylasttime <- tail(pdata$time,1)
                mylast <- format(mylasttime, "%y%m%d")
                
                # explore input raw data; select just from last availabe day on
                target_folder <- paste0(folder_in, "/", i)
                files <- list.files (path=target_folder, full.names = TRUE, recursive = FALSE)
                just_files <- list.files (path=target_folder, full.names = FALSE, recursive = FALSE)

                milestone <- grep(mylast, files)[1] - 1 # (-1)--> we want at least two files, otherwise it won't work
                
                files <- files[milestone:length(files)]
                just_files <- just_files[milestone:length(just_files)]
                
        }else{
                # make list of target files
                #(alphabetical order of folders; alphabetical order within a folder)
                target_folder <- paste0(folder_in, "/", i)
                files <- list.files (path=target_folder, full.names = TRUE, recursive = FALSE)
                just_files <- list.files (path=target_folder, full.names = FALSE, recursive = FALSE)
        }

        print(  paste0(Sys.time(), " start data extraction at ", target_folder)  )
        
        #1st file reading (we need a heading)
        data0 <- read.table(file=files[1], header=T, dec=".", stringsAsFactors=FALSE, nrows = 1440, sep = "\t")       
        print(paste( i, ": ", as.character(1)," of ",as.character(length(files)),"  ",just_files[1]))
        
        ##           count of rows for later export
        #
        count <- nrow(data0)
        nrows <- nrow(data0)
        
        ##           initialize output dataframe
        #
        nrows_data <- 1440*length(files)
        heading <- c("time", "epoch_time", channel_names)
        ncolumns <- length(heading)
        dim_names <- list(NULL, heading)
        
        data <- matrix(data = NA, nrow = nrows_data, ncol = ncolumns, byrow = TRUE, dimnames = dim_names)
        data <- data.frame(data)
        
        # insert data0 values to data
        data[ 1:nrows , names(data0) ] <- data0[,names(data0)]
        
        # reading rest of files
        for(j in 2:length(files)){
                data0 <- read.table (file=files[j], header=T, dec=".", stringsAsFactors=FALSE, nrows = 1440, sep = "\t")
                #merge content to existing "data"
                #data<-merge(data, data_next, by = intersect(names(data), names(data_next)), all=TRUE)
                
                nrows <- nrow(data0)
                data[ (count+1):(count+nrows) , names(data0) ] <- data0[,names(data0)]
                count <- count + nrows
                #print confirmation
                print(paste( i, ": ", as.character(j)," of ",as.character(length(files)),"  ",just_files[j]))
        }
        print(  paste0(Sys.time(), "  data arrangement and numer formatting:")  )
        
        # getting rid of the non-written-initialized-NA values at the tail
        data <- data[1:count,]
        # getting rid of empty channels
        is.empty <- sapply(  heading, function(x) sum(is.na( data[,x] ))  )
        is.empty <- sapply(is.empty , function(x) x[[1]] >= count)
        data <- data[,!is.empty]
        
        #column order by channel
        names(data)[1]<- paste("00time")
        names(data)[2]<- paste("01epoch_time")
        data <- data[,order(names(data))]
        names(data)[1]<- paste("time")
        names(data)[2]<- paste("epoch_time")
        
        #order data by epoch_time
        data<- data[ order(data[,2]), ]
        
        #column values converted to numbers (except for time column)
        wide_length<-length(names(data))        
        suppressWarnings(   data[,3:wide_length]<-sapply(data[,3:wide_length], as.numeric )   )
        
        # create bincode columns and rearrange column order (value60 has to be the 1st one at the i loop)
        mycolumns <- names(data)
        codes <- c("bin_INC1", "bin_INC2", "bin_INC_valve", "bin_TANK", "bin_WORK", "bin_cal1")
        
        if(i == "value60"){
                source("binary.R")
                bincodes <- binary_generator(data, incubation)
                # bincodes
        }
        
        data[,names(bincodes)] <- bincodes
        is.code <- 1 : ncol(bincodes)
        new_mycolumns <- c(mycolumns[1:2], codes[is.code],  mycolumns[-c(1:2)])
        data <- data[,new_mycolumns]
        
        # append to previously existing file (if existing)
        if(length(myfile) == 1){
                
                data <- data.table(data)
                data[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
                pdata[,bin_INC_valve:= formatC(bin_INC_valve,3, flag="0")]
                
                myfirsttime <- data$time[1]
                pdata <- pdata[time < myfirsttime]
                
                data <- rbindlist(list(pdata, data))
                data <- as.data.frame(data)
        }
        
        # number of digits formating
        digit4 <- c("C1001", "C1002", "C1010", "C1011", "C1020", "C1030", "C1101", "C1102",
                    "C2001", "C2002", "C2003", "C2004", "C2005", "C2006")
        digit2 <- c("C1201", "C1202", "C1203", "C1204", "C1205", "C1206", "C1207", "C1208", "C1209", "C1210",
                    "C4001", "C4002",  "C4011", "C4012", "C5001")
        # check if proposed channel actually exist in this file
        is.col4 <- !is.na(  match(digit4, names(data))  )
        is.col2 <- !is.na(  match(digit2, names(data))  )
        digit4 <- digit4[is.col4]
        digit2 <- digit2[is.col2]
        
        data[,digit4] <- sapply(data[,digit4], function(x) formatC(x, format = "f", digits = 4) )
        data[,digit2] <- sapply(data[,digit2], function(x) formatC(x, format = "f", digits = 2) )
        
        #write file
        print(  paste0(Sys.time(), "  ",i, " file writting:")  )
        
        file_out <- paste0(folder_out, "/", i, "_IDASw_", incubation, ".dat")
        write.table(data, file= file_out, quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
        
        print(  paste0(Sys.time(), "output file was written to:")  )
        print(file_out)
}

print(  paste0(Sys.time(), " IDASw_wide_60.R is finnished")  )