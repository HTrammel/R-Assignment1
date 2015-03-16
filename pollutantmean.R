# polluntantmean.R

pollutantmean <- function (directory, pollutant, id = 1:332 ) {
    monitorData <- function(f, rng) {
        for (i in rng) {
            if (length(rng) == 1 || i == rng[1] ) {
                df <- read.csv(f[i])
            } else {
                tmp <- read.csv(f[i])
                df <- rbind(df, tmp) 
            }
        }
        df
    }
    
    cleanData <- function(d_frame, c_name) {
         tmp <- d_frame[[c_name]]
         bad <- is.na(tmp) 
         good <- tmp[!bad]
    }
    
    f_list <- list.files(directory, full.names=TRUE)
    
    dfrm <- monitorData (f_list, id)
    x <- cleanData(dfrm, pollutant)
    mean(x)
}


