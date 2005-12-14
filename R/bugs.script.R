"bugs.script" <-
function (parameters.to.save, n.chains, n.iter, n.burnin,
    n.thin, bugs.directory, model.file, debug=FALSE, is.inits, bin, 
    DIC = FALSE, useWINE = FALSE){
### Write file script.txt for Bugs to read
###  if (n.chains<2) stop ("n.chains must be at least 2")
  trfile <- function(x) { 
    if (useWINE) winedriveTr(x) 
    else x 
  }
  rtrfile <- function(x) { 
    if (useWINE) winedriveRTr(x) 
    else x 
  }
  script.bugs.directory <- bugs.directory
  bugs.directory <- trfile(bugs.directory)

  if((ceiling(n.iter/n.thin) - ceiling(n.burnin/n.thin)) < 2) 
    stop ("(n.iter-n.burnin)/n.thin must be at least 2")
  working.directory <- getwd()
  script <- file.path(bugs.directory, "script.txt")
  model <- if(length(grep("\\\\", model.file)) || length(grep("/", model.file)))
    model.file
    else file.path(working.directory, model.file)
  data <- file.path(working.directory, "data.txt")
  history <- file.path(working.directory, "history.odc")
  coda  <- file.path(working.directory, "coda")
  logfile <- file.path(working.directory, "log.odc")
  inits <- sapply(paste(working.directory, "/inits", 1:n.chains, ".txt", sep=""), rtrfile)
  initlist <- paste("inits (", 1:n.chains, ", '", inits, "')\n", sep="")
  savelist <- paste("set (", parameters.to.save, ")\n", sep="")
  redo <- ceiling((n.iter-n.burnin)/(n.thin*bin))
  cat(
    "display ('log')\n",
    "check ('", rtrfile(model), "')\n",
    "data ('", rtrfile(data), "')\n",
    "compile (", n.chains, ")\n",
    if(is.inits) initlist,
    "gen.inits()\n",
    "thin.updater (", n.thin, ")\n",
    "update (", ceiling(n.burnin/n.thin), ")\n",
     savelist,
    if(DIC) "dic.set()\n",
    rep(
    c("update (", formatC(ceiling(bin), format = "d"), ")\n",
    "coda (*, '", rtrfile(coda), "')\n"),redo),
    "stats (*)\n",
    if(DIC) "dic.stats()\n",
    "history (*, '", rtrfile(history), "')\n",
    "save ('", rtrfile(logfile), "')\n", file=script, sep="", append=FALSE)
  if (!debug) cat ("quit ()\n", file=script, append=TRUE)
  sims.files <- paste ("coda", 1:n.chains, ".txt", sep="")
  for (i in 1:n.chains) cat ("WinBUGS did not run correctly.\n",
    file=sims.files[i], append=FALSE)
}
