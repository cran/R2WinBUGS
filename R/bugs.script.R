"bugs.script" <-
function (parameters.to.save, n.chains, n.iter, n.burnin,
    n.thin, bugs.directory, model.file, debug=FALSE, is.inits){
# Write file script.txt for Bugs to read
#  if (n.chains<2) stop ("n.chains must be at least 2")
  n.keep <- ceiling(n.iter/n.thin)-ceiling(n.burnin/n.thin)
  if (n.keep < 2) stop ("(n.iter-n.burnin)/n.thin must be at least 2")
  working.directory <- getwd()
  script <- file.path(bugs.directory, "script.txt")
  model <- if(length(grep("\\\\", model.file)) || length(grep("/", model.file)))
    model.file
    else file.path(working.directory, model.file)
  data <- file.path(working.directory, "data.txt")
  history <- file.path(working.directory, "history.odc")
  coda  <- file.path(working.directory, "coda")
  logfile <- file.path(working.directory, "log.odc")
  inits <- paste(working.directory, "/inits", 1:n.chains, ".txt", sep="")
  initlist <- paste("inits (", 1:n.chains, ", '", inits, "')\n", sep="")
  savelist <- paste("set (", parameters.to.save, ")\n", sep="")
  cat(
    "display ('log')\n",
    "check ('", model, "')\n",
    "data ('", data, "')\n",
    "compile (", n.chains, ")\n",
    if(is.inits) initlist,
    "gen.inits()\n",
    "beg (", ceiling(n.burnin/n.thin)+1, ")\n",
    "thin.updater (", n.thin, ")\n",
    savelist,
#    "dic.set()\n",
    "update (", ceiling(n.iter/n.thin), ")\n",
    "stats (*)\n",
#    "dic.stats()\n",
    "history (*, '", history, "')\n",
    "coda (*, '", coda, "')\n",
    "save ('", logfile, "')\n", file=script, sep="", append=FALSE)
  if (!debug) cat ("quit ()\n", file=script, append=TRUE)
  sims.files <- paste ("coda", 1:n.chains, ".txt", sep="")
  for (i in 1:n.chains) cat ("WinBUGS did not run correctly.\n",
    file=sims.files[i], append=FALSE)
}
