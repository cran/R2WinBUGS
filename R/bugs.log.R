bugs.log <- function(file)
{
    if(!file.exists(file))
        stop("Log file", file, "does not exist")
    logfile <- readLines(file)
    
    statsStart <- which(logfile == "Node statistics") + 2
    if(!length(statsStart))
        stop("Log file", file, "does not contain node statistics.")
    ## + 2 to remove
    ## "Node statistics"
    ## "\t node\t mean\t sd\t MC error\t2.5%\tmedian\t97.5%\tstart\tsample"
    
    statsEnd <- which(logfile == "dic.stats()") - 1
    ## + 1 to remove
    ## "dic.stats()"
    
    statsTable <- logfile[statsStart:statsEnd]
    statsTable <- sub("\t", "", statsTable)
    statsTable <- sapply(strsplit(statsTable, "\t"), "[")
    colnames(statsTable) <- statsTable[1,]
    statsTable <- t(apply(statsTable[-1,], 2, as.numeric))
    colnames(statsTable) <- c("mean", "sd", "MC error", "2.5%", "median", "97.5%", "start", "sample")
    
    ## DIC
    DICStart <- which(logfile == "DIC") + 3
    ## + 3 to remove
    ## "DIC"
    ## "Dbar = post.mean of -2logL; Dhat = -2LogL at post.mean of stochastic nodes"
    ## "\tDbar\tDhat\tpD\tDIC\t"
    
    DICEnd <- grep("history(", logfile, fixed=TRUE) - 1
    ## - 1 to remove
    ## "history(+, ..."
    
    if(!length(DICEnd) || !length(DICStart) || (DICEnd < DICStart)){ 
        DICTable <- NA
    }else{
        DICTable <- logfile[DICStart:DICEnd]
        DICTable <- sapply(strsplit(DICTable, "\t"), "[")
        colnames(DICTable) <- DICTable[1,]
        DICTable <- t(apply(DICTable[-1,], 2, as.numeric))
        colnames(DICTable) <- c("Dbar", "Dhat", "pD", "DIC")
    }
    return(list(stats = statsTable, DIC = DICTable))
}
