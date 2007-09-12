
if (is.R()) {

openbugs <- function(data, inits, parameters.to.save, model.file="model.txt",
  n.chains = 3, n.iter = 2000, n.burnin = floor(n.iter/2),
  n.thin = max(1, floor(n.chains *(n.iter - n.burnin)/1000)),
  DIC = TRUE, bugs.directory = "c:/Program Files/OpenBUGS/",
  working.directory=NULL, digits = 5)
{
  if(!require(BRugs))
    stop("BRugs is required")
  ## switching from bugs() to BRugsFit() notation
  modelFile <- model.file
  numChains <- n.chains
  nBurnin <- n.burnin
  nIter <- n.iter - n.burnin
  nThin <- n.thin
  if(DIC) parameters.to.save <- c(parameters.to.save, "deviance")
  parametersToSave <- parameters.to.save
  if(!is.null(working.directory)) {
    savedWD <- getwd()
    setwd(working.directory)
    on.exit(setwd(savedWD))
  }
  if(!file.exists(modelFile)) {
    stop(modelFile, " does not exist")
  }
  if(file.info(modelFile)$isdir) {
    stop(modelFile, " is a directory, but a file is required")
  }
  ## kludge to add carriage-returns (line-feeds?) to the model file
  if(!length(grep("\r\n", readChar(modelFile, 10^3)))) {
    message("Carriage returns added to model file ", modelFile)
    model <- readLines(modelFile)
    try(writeLines(model, modelFile))
  }
  BRugs::modelCheck(modelFile)
  if(!(is.vector(data) && is.character(data) && all(file.exists(data)))) {
    data <- BRugs::bugsData(data, digits = digits)
  }
  BRugs::modelData(data)
  BRugs::modelCompile(numChains)
  if(missing(inits) || is.null(inits)) {
    BRugs::modelGenInits()
  } else {
    if(is.list(inits) || is.function(inits) || (is.character(inits) &&
      !any(file.exists(inits)))) {
      inits <- BRugs::bugsInits(inits = inits, numChains = numChains,
                                digits = digits)
    }
    BRugs::modelInits(inits)
    BRugs::modelGenInits()
  }
  BRugs::samplesSetThin(nThin)
  ## set the adaptive phases
  adaptivelines <- scan(system.file("OpenBUGS", "Bugs", "Rsrc",
                                    "Registry.txt", package="BRugs"),
                        what="character")
  factories <- sub(".adaptivePhase", "",
                   adaptivelines[grep("adaptivePhase",adaptivelines)])
  sapply(factories, BRugs::modelSetAP, max(0, nBurnin-1))

  BRugs::modelUpdate(nBurnin)
  ## BRugs::samplesSetThin(nThin)
  if(DIC) {
    BRugs::dicSet()
    on.exit(BRugs::dicClear(), add = TRUE)
  }
  BRugs::samplesSet(parametersToSave)
  BRugs::modelUpdate(nIter)
  params <- sort.name(BRugs::samplesMonitors("*"), parametersToSave)
  samples <- sapply(params, BRugs::samplesSample)
  n.saved.per.chain <- nrow(samples)/numChains
  samples.array <- array(samples, c(n.saved.per.chain, numChains, ncol(samples)))
  dimnames(samples.array)[[3]] <- dimnames(samples)[[2]]
  if(DIC) {
    DICOutput <- BRugs::dicStats()
  } else {
    DICOutput <- NULL
  }
  bugs.output <- as.bugs.array(sims.array=samples.array,
                               model.file=modelFile, program="OpenBUGS",
                               DIC=DIC, DICOutput=DICOutput,
                               n.iter=n.iter, n.burnin=n.burnin, n.thin=n.thin)
  bugs.output
}

## sorter called by openbugs() to save the parameters in the specified order
sort.name <- function(a, b){
  ## sort the scalar parameter names in the vector "a" according to the
  ## ordering of their stems in the shorter vector "b"
  bracket.pos <- regexpr("\\[", a)
  a.stem <- substr(a, 1, ifelse(bracket.pos>0, bracket.pos-1, nchar(a)))
  a[order(match(a.stem, b))]
}

}
