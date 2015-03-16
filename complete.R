# complete.R

complete <- function (directory, id = 1:332 ) {
    monitorData <- function(d, rng) {
        f_list <- list.files(d, full.names=TRUE)
        df <- NULL
        for (i in rng) {
            if (length(rng) == 1 || rng[1] == i) {
                df <- read.csv(f_list[i])
            } else {
                tmp <- read.csv(f_list[i])
                df <- rbind(df, tmp) 
            }
        }
        return(df)
    }
    
    checkCases <- function (d_frame, rng) {
        mtx <- NULL
        for (i in rng) {
            tmp <- d_frame[ d_frame[, "ID"] == i,]
            good <- complete.cases(tmp)
            x <- tmp[good,]
            mtx <- rbind(mtx, c(i, nrow(x)))    
        }
        df <- data.frame(mtx)
        rownames(df) <- 1:nrow(mtx)
        colnames(df) <- c("id", "nobs")
        return(df)
    }
    
    dfrm <- monitorData (directory, id)
    x <- checkCases(dfrm, id)
    return(x)
}

