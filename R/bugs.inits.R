"bugs.inits" <- 
function (inits, n.chains, digits){
    if(!is.null(inits)) {
        for(i in 1:n.chains) {
            if(is.function(inits))
                if(is.R()) {
                    write.datafile(lapply(inits(), formatC, digits = digits, format = "E"),
                        paste("inits", i, ".txt", sep = ""))
                } else {
                    writeDatafileS4(inits(), towhere = 
                        paste("inits", i, ".txt", sep = ""))
                }
            else if(is.R()) {
                write.datafile(lapply(inits[[i]], formatC, digits = digits, format = "E"), 
                    paste("inits", i, ".txt", sep = ""))
                } else {
                    writeDatafileS4(inits[[i]], towhere = paste(
                        "inits", i, ".txt", sep = ""))
                }
        }
    }
}
