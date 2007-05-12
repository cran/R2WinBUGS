"bugs.script" <-
function (parameters.to.save, n.chains, n.iter, n.burnin,
    n.thin, model.file, debug=FALSE, is.inits, bin,
    DIC = FALSE, useWINE = FALSE, newWINE = FALSE, WINEPATH = NULL){
### Write file script.txt for Bugs to read
###  if (n.chains<2) stop ("n.chains must be at least 2")

  if((ceiling(n.iter/n.thin) - ceiling(n.burnin/n.thin)) < 2)
    stop ("(n.iter-n.burnin)/n.thin must be at least 2")
  working.directory <- getwd()
  script <- "script.txt"
  model <- if(length(grep("\\\\", model.file)) || length(grep("/", model.file))){
    model.file
  } else file.path(working.directory, model.file)
  data <- file.path(working.directory, "data.txt")
  history <- file.path(working.directory, "history.odc")
  coda  <- file.path(working.directory, "coda")
  logfile <- file.path(working.directory, "log.odc")
  logfileTxt <- file.path(working.directory, "log.txt")
  inits <- sapply(paste(working.directory, "/inits", 1:n.chains, ".txt", sep=""), native2win)
  initlist <- paste("inits (", 1:n.chains, ", '", inits, "')\n", sep="")
  savelist <- paste("set (", parameters.to.save, ")\n", sep="")
  redo <- ceiling((n.iter-n.burnin)/(n.thin*bin))
  
  if (is.R()){
    thinUpdateCommand <- paste("thin.updater (", n.thin, ")\n",
      "update (", ceiling(n.burnin/n.thin), ")\n", sep = "")
  } else{
    ## In S-PLUS, the handling of the thinning is done differently than in R.
    ## bin represents the number of iterations between saves, before thinning,
    ## where in R it is the number of iterations between saves, after thinning.
    ## This alternative handling of the thinning is done so that the resulting 
    ## samples have the correct iteration indexes in the output (coda) files.
    ## Therefore, if the samples are read into S-PLUS using the coda package,
    ## the thinning will be correctly labelled in the resulting mcmc object.
    ## In R, the thinning is always labelled as 1, even if thinning was done.
  	thinUpdateCommand <- paste("update (", n.burnin, ")\n",
			"thin.samples (", n.thin, ")\n", sep = "")
		bin = bin * n.thin
  }
  
  cat(
    "display ('log')\n",
    "check ('", native2win(model), "')\n",
    "data ('", native2win(data), "')\n",
    "compile (", n.chains, ")\n",
    if(is.inits) initlist,
    "gen.inits()\n",
    thinUpdateCommand,
     savelist,
    if(DIC) "dic.set()\n",
    rep(
    c("update (", formatC(ceiling(bin), format = "d"), ")\n",
    "coda (*, '", native2win(coda), "')\n"),redo),
    "stats (*)\n",
    if(DIC) "dic.stats()\n",
    "history (*, '", native2win(history), "')\n",
    "save ('", native2win(logfile), "')\n", 
    "save ('", native2win(logfileTxt), "')\n",
    file=script, sep="", append=FALSE)
  if (!debug) cat ("quit ()\n", file=script, append=TRUE)
  sims.files <- paste ("coda", 1:n.chains, ".txt", sep="")
  for (i in 1:n.chains) cat ("WinBUGS did not run correctly.\n",
    file=sims.files[i], append=FALSE)
}
