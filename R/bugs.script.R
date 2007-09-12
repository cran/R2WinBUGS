"bugs.script" <-
  function(parameters.to.save, n.chains, n.iter, n.burnin,
           n.thin, model.file, debug=FALSE, is.inits, bin,
           DIC=FALSE, useWINE=.Platform$OS.type != "windows",
           newWINE=TRUE, WINEPATH=NULL)
{
  ## Write file script.txt for Bugs

  if((ceiling(n.iter/n.thin) - ceiling(n.burnin/n.thin)) < 2)
    stop ("(n.iter-n.burnin)/n.thin must be at least 2")
  working.directory <- getwd()
  script <- "script.txt"

  test <- length(grep("\\\\", model.file)) || length(grep("/", model.file))
  model <- ifelse(test, model.file, file.path(working.directory, model.file))
  model <- native2win(model, useWINE=useWINE, newWINE=newWINE, WINEPATH=WINEPATH)

  data <- file.path(working.directory, "data.txt")
  data <- native2win(data, useWINE=useWINE, newWINE=newWINE, WINEPATH=WINEPATH)

  coda  <- file.path(working.directory, "coda")
  coda <- native2win(coda, useWINE=useWINE, newWINE=newWINE, WINEPATH=WINEPATH)

  logFile <- file.path(working.directory, "log.odc")
  logFile <- native2win(logFile, useWINE=useWINE, newWINE=newWINE, WINEPATH=WINEPATH)
  logFileTxt <- file.path(working.directory, "log.txt")
  logFileTxt <- native2win(logFileTxt, useWINE=useWINE, newWINE=newWINE, WINEPATH=WINEPATH)

  inits <- paste(working.directory, "/inits", 1:n.chains, ".txt", sep="")
  inits <- sapply(inits, useWINE=useWINE, newWINE=newWINE, WINEPATH=WINEPATH, 
  	function(x, useWINE, newWINE, WINEPATH) 
  	{native2win(x, useWINE=useWINE, newWINE=newWINE, WINEPATH=WINEPATH)})

  initlist <- paste("inits (", 1:n.chains, ", '", inits, "')\n", sep="")

  savelist <- paste("set (", parameters.to.save, ")\n", sep="")
  redo <- ceiling((n.iter-n.burnin)/(n.thin*bin))

  thinUpdate <- paste("thin.updater (", n.thin, ")\n",
                      "update (", ceiling(n.burnin/n.thin), ")\n", sep="")

  cat(
    "display ('log')\n",
    "check ('", model, "')\n",
    "data ('", data, "')\n",
    "compile (", n.chains, ")\n",
    if(is.inits) initlist,
    "gen.inits()\n",
    thinUpdate,
     savelist,
    if(DIC) "dic.set()\n",
    rep(
    c("update (", formatC(ceiling(bin), format="d"), ")\n",
    "coda (*, '", coda, "')\n"),redo),
    "stats (*)\n",
    if(DIC) "dic.stats()\n",
    "history (*)\n",
    "save ('", logFile, "')\n",
    "save ('", logFileTxt, "')\n",
    file=script, sep="", append=FALSE)

  if(!debug) cat("quit ()\n", file=script, append=TRUE)

  sims.files <- paste("coda", 1:n.chains, ".txt", sep="")
  for(i in 1:n.chains)
    cat("WinBUGS did not run correctly.\n", file=sims.files[i], append=FALSE)
}
