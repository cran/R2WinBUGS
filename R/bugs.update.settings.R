"bugs.update.settings" <-
function (n.burnin, bugs.directory){
  char.burnin <- as.character(n.burnin)
  file.copy(file.path(bugs.directory, "System/Rsrc/Registry.odc"),
            file.path(bugs.directory, "System/Rsrc/Registry_Rsave.odc"),
            overwrite = TRUE)
  registry <- readBin (file.path(bugs.directory,
      "System/Rsrc/Registry.odc"), "character", 400, size=1)
  info <- registry[regexpr("Int",registry)>0]
  while (regexpr("\r",info)>0){
    newline <- regexpr("\r",info)
    info <- substring (info, newline+1)
      line <- substring (info, 1, regexpr("\r",info)-1)
    if (regexpr ("AdaptivePhase", line) > 0){
      numpos <- regexpr ("Int", line) + 4
      num <- substring (line, numpos)
      if (as.numeric(num) > n.burnin){
        num.new <- paste (paste (rep(" ", nchar(num)-nchar(char.burnin)),
                                 sep="", collapse=""), char.burnin, sep="")
        line.new <- sub (num, num.new, line)
        registry <- sub (line, line.new, registry)
      }
    }
  }
  writeBin (registry,
      file.path(bugs.directory, "System/Rsrc/Registry.odc"))
}
