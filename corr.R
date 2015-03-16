# corr.R
#---------------------------
# Programming Assignment 1
# Author: Harold L Trammel
# --------------------------    

corr <- function(directory, threshold = 0) {
    get_cor <- function(fle) {
        f_data <- read.csv(fle)
        good <- na.omit(f_data)
        r <- cor(good$sulfate, good$nitrate)
        if (!is.na(r)) {
            return(r)
        }
    }
    
    f_list <- list.files(directory, full.names=TRUE, no..=TRUE)
    cc <- complete(directory)
    #return(cc)
    
    mtx <- NULL  
    for (i in 1:332) {
        tmp <- cc[cc[,"id"] ==i,]
        if ( tmp$nobs > threshold) {
            f <- f_list[i]
            r <- get_cor(f)
            mtx <- rbind(mtx, r) 
        }        
    }
    return(mtx)
}