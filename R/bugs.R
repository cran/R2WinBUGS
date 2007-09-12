"bugs" <-
function(data, inits, parameters.to.save, model.file="model.bug",
    n.chains=3, n.iter=2000, n.burnin=floor(n.iter / 2),
    n.thin=max(1, floor(n.chains * (n.iter - n.burnin) / 1000)),
    bin=(n.iter - n.burnin) / n.thin,
    debug=FALSE, DIC=TRUE, digits=5, codaPkg=FALSE,
    bugs.directory="c:/Program Files/WinBUGS14/",
    program=c("WinBUGS", "OpenBUGS", "winbugs", "openbugs"),
    working.directory=NULL,
    clearWD=FALSE, useWINE=.Platform$OS.type != "windows", WINE=NULL,
    newWINE=TRUE, WINEPATH=NULL)
{
  program <- match.arg(program)
  if(program %in% c("openbugs", "OpenBUGS", "OpenBugs")) {
    if(!is.R()) stop("OpenBUGS is not yet available in S-PLUS")
    ## If OpenBUGS, we only call openbugs() and exit...
    return(openbugs(data, inits, parameters.to.save, model.file,
                    n.chains, n.iter, n.burnin, n.thin, DIC=DIC,
                    bugs.directory, working.directory, digits))
  }
  ## Checking number of inits, which is NOT save here:
  if(!missing(inits) && !is.function(inits) && !is.null(inits) && (length(inits) != n.chains))
    stop("Number of initialized chains (length(inits)) != n.chains")

  ## Wine
  if(useWINE) {
    if(!is.R())
      stop("Non-Windows platforms not yet supported in R2WinBUGS for S-PLUS")
    ## Attempt to find wine and winepath
    if(is.null(WINE)) WINE <- findUnixBinary(x="wine")
    if(is.null(WINEPATH)) WINEPATH <- findUnixBinary(x="winepath")
  }

  if(!is.null(working.directory)) {
    savedWD <- getwd()
    setwd(working.directory)
    on.exit(setwd(savedWD))
  }
  if(!file.exists(model.file))
    stop(paste(model.file, "does not exist."))
  if(file.info(model.file)$isdir)
    stop(paste(model.file, "is a directory, but a file is required."))
  if(!(length(data) == 1 && is.vector(data) && is.character(data) && data == "data.txt")) {
    bugs.data(data, dir=getwd(), digits)
  } else {
    if(!file.exists(data)) stop("File data.txt does not exist.")
  }
  bugs.inits(inits, n.chains, digits)
  if(DIC) parameters.to.save <- c(parameters.to.save, "deviance")
  ## Model files with extension ".bug" need to be renamed to ".txt"
  if(length(grep("[.]bug$", model.file))) {
    new.model.file <- sub("[.]bug$", ".txt", model.file)
    file.copy(model.file, new.model.file, overwrite=TRUE)
    on.exit(try(file.remove(new.model.file)), add=TRUE)
  } else {
    new.model.file <- model.file
  }
  bugs.script(parameters.to.save, n.chains, n.iter, n.burnin, n.thin,
              new.model.file, debug=debug, is.inits=!is.null(inits),
              bin=bin, DIC=DIC, useWINE=useWINE, newWINE=newWINE,
              WINEPATH=WINEPATH)
  bugs.run(n.burnin, bugs.directory, WINE=WINE, useWINE=useWINE,
           newWINE=newWINE, WINEPATH=WINEPATH)
  if(codaPkg)
    return(file.path(getwd(), paste("coda", 1:n.chains, ".txt", sep="")))
  sims <- c(bugs.sims(parameters.to.save, n.chains, n.iter, n.burnin,
                      n.thin, DIC),
            model.file=model.file, program=program)
  if(clearWD) {
    file.remove(c("data.txt", "log.odc", "log.txt", "codaIndex.txt",
                  paste("inits", 1:n.chains, ".txt", sep=""),
                  paste("coda", 1:n.chains, ".txt", sep="")))
  }
  class(sims) <- "bugs"
  sims
}
