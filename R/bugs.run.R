"bugs.run" <-
    function(n.burnin, bugs.directory, WINE = "", 
             useWINE = .Platform$OS.type != "windows", newWINE = TRUE){

if(useWINE && !newWINE) bugs.directory <- win2native(bugs.directory)

## Update the lengths of the adaptive phases in the Bugs updaters
  try(bugs.update.settings(n.burnin, bugs.directory))
## Return the lengths of the adaptive phases to their original settings
  on.exit(try(file.copy(file.path(bugs.directory, "System/Rsrc/Registry_Rsave.odc"),
                        file.path(bugs.directory, "System/Rsrc/Registry.odc"),
                        overwrite = TRUE)))
## Search Win*.exe (WinBUGS executable) within bugs.directory
  dos.location <- file.path(bugs.directory, 
    grep("^Win[[:alnum:]]*[.]exe$", list.files(bugs.directory), value = TRUE)[1])
  if(!file.exists(dos.location)) 
    stop(paste("WinBUGS executable does not exist in", bugs.directory))
## Call Bugs and have it run with script.txt
  bugsCall <- paste("\"", dos.location, "\" /par \"",
                    native2win(file.path(getwd(), "script.txt"), newWINE = newWINE),
                    "\"", sep = "")
  if (useWINE)
    bugsCall <- paste(WINE, bugsCall)
  temp <- system(bugsCall)

  if(temp == -1)
      stop("Error in bugs.run().\nCheck that WinBUGS is in the specified directory.")
## Stop and print an error message if Bugs did not run correctly
  if (length(grep("Bugs did not run correctly",
    scan("coda1.txt", character(), quiet=TRUE, sep="\n"))) > 0)
      stop("Look at the log file and\ntry again with debug=TRUE and figure out what went wrong within Bugs.")
}


 
