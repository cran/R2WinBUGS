"bugs.run" <-
function (n.burnin, bugs.directory, WINE = ""){
## Update the lengths of the adaptive phases in the Bugs updaters
  bugs.update.settings(n.burnin, bugs.directory)
## Return the lengths of the adaptive phases to their original settings
  on.exit(file.copy(paste(bugs.directory, "System/Rsrc/Registry_Rsave.odc", sep=""), 
      paste(bugs.directory, "System/Rsrc/Registry.odc", sep=""),
      overwrite = TRUE))
## Search Win*.exe (WinBUGS executable) within bugs.directory
  dos.location <- file.path(bugs.directory, 
    grep("^Win[[:alnum:]]*\.exe$", list.files(bugs.directory), value = TRUE)[1])
  if(!file.exists(dos.location)) 
    stop(paste("WinBUGS executable does not exist in", bugs.directory))
## Call Bugs and have it run with script.txt
  temp <- system(paste(WINE, ' "', dos.location, '"', " /par ", "script.txt", sep = ""))
  if(temp == -1)
      stop("Error in bugs.run().\nCheck that WinBUGS is in the specified directory.")
## Stop and print an error message if Bugs did not run correctly
  if (length(grep("Bugs did not run correctly",
    scan("coda1.txt", character(), quiet=TRUE, sep="\n"))) > 0)
      stop("Look at the log file and\ntry again with debug=TRUE and figure out what went wrong within Bugs.")
}
