read.bugs <- function(codafiles, ...){
    if(!require(coda))
        stop("package 'coda' is required to use this function")
    coda::mcmc.list(lapply(codafiles, coda::read.coda, 
                     index.file = file.path(dirname(codafiles[1]), "codaIndex.txt"), 
                     ...))
}
