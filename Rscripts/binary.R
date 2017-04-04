binary<-function(n) {
        while(is.na(n)==TRUE){
                n<-0
        }
        n<-round(n)
        bsum<-0
        bexp<-1
        while (n > 0) {
                digit<-n %% 2
                n<-floor(n / 2)
                bsum<-bsum + digit * bexp
                bexp<-bexp * 10
        }
        bsum4digits<-formatC(as.integer(bsum), 3, flag=0)
        return(paste(bsum4digits))
}
################################################################################

binary_generator <- function(data, incubation){
        if ( incubation == "Ruth" ){
                
                c1<-sapply(data[,"C1201"],binary)
                c2<-sapply(data[,"C1202"],binary)
                c9<-data[,"C1209"]
                c9[is.na(c9)]<-0
                c9<-formatC(as.integer(c9), 0, flag=0)
                
                c3<-sapply(data[,"C1203"],binary)
                c4<-sapply(data[,"C1204"],binary)
                c5<-sapply(data[,"C1205"],binary)
                c5_1718<-substring(c5,3,4)
                c5_cal2<-substring(c5,1,1)
                c5_cal1<-substring(c5,2,2)
                
                c6<-sapply(data[,"C1206"],binary)
                c6_AIR1<-substring(c6,4,4)
                c6_AIR2<-substring(c6,3,3)
                c6_TANK<-substring(c6,1,1)
                
                c7<-sapply(data[,"C1207"],binary)
                c7_INC_valve<-substring(c7,3,4)
                
                c8a<-data[,"C1208"]
                c8a[c8a>0] <- 1
                c8<-sapply(c8a,binary)
                c8_cal1<-substring(c8,4,4)
                
                c10<-sapply(data[,"C1210"],binary)
                
                codes <- c("bin_INC1", "bin_INC2", "bin_INC_valve", "bin_cal1")
                bincodes <- data.frame(matrix  (NA, nrow = nrow(data), ncol = 4, byrow = TRUE, dimnames = list(NULL, codes))  )
                
                bincodes[,"bin_INC1"]<-paste("0", c6_AIR1, c9, c2, c1, sep="-", collapse=NULL)
                bincodes[,"bin_INC2"]<-paste(c5_cal2, c6_AIR2, c5_1718, c4, c3, sep="-",collapse=NULL)
                bincodes[,"bin_INC_valve"]<-c7
                bincodes[,"bin_TANK"]<-paste(c6_TANK)
                bincodes[,"bin_WORK"]<-paste(c8_cal1)
                bincodes[,"bin_cal1"]<-paste(c5_cal1, c10, sep="-",collapse=NULL)
                
                bincodes
                
        }else{
                stop("invalid incubation")
        }
}