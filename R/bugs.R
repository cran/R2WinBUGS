"bugs" <-
function (data, inits, parameters.to.save, model.file = "model.txt",
    n.chains = 3, n.iter = 2000, n.burnin = floor(n.iter / 2),
    n.thin = max(1, floor(n.chains * (n.iter - n.burnin) / 1000)), debug = FALSE,
    DIC = TRUE, digits = 5, codaPkg = FALSE, 
    bugs.directory = "c:/Program Files/WinBUGS14/", working.directory = NULL){
  # Checking number of inits, which is NOT save here:
  if(!missing(inits) && !is.function(inits) && (length(inits) != n.chains)) 
    stop("Number of initialized chains (length(inits)) != n.chains")
  if(!is.null(working.directory)){
      savedWD <- getwd()
      setwd(working.directory)
      on.exit(setwd(savedWD))
  }
  if(!file.exists(model.file)) stop(paste(model.file, "does not exist."))
  bugs.data.inits(data, inits, n.chains, digits)
  if(DIC) parameters.to.save <- c(parameters.to.save, "deviance")
  # Model files with extension ".bug" need to be renamed to ".txt"
  if(length(grep("\\.bug$", model.file))){
    new.model.file <- sub("\\.bug$", "\\.txt", model.file)
    file.copy(model.file, new.model.file, overwrite = TRUE)
    on.exit(file.remove(new.model.file))
  }
  else new.model.file <- model.file
  bugs.script(parameters.to.save, n.chains, n.iter, n.burnin, n.thin,
    bugs.directory, new.model.file, debug=debug, is.inits=!is.null(inits))
  bugs.run(n.burnin, bugs.directory)
  if(codaPkg){
    for(i in 1:n.chains){
        file.rename(paste("coda", i, ".txt", sep=""), paste("coda", i, ".out", sep=""))
        file.copy("codaIndex.txt", paste("coda", i, ".ind", sep=""), overwrite = TRUE)
    }
    return(file.path(getwd(), paste("coda", 1:n.chains, sep="")))
  }
  else{
    sims <- c(bugs.sims(parameters.to.save, n.chains, n.iter, n.burnin, n.thin, DIC), 
        model.file = model.file, is.DIC = DIC)
    class(sims) <- "bugs"
    return(sims)
  }
}
