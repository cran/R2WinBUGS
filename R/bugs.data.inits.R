"bugs.data.inits" <-
function (data, inits, n.chains, digits){
  if(is.numeric(unlist(data)))
    write.datafile(lapply(data, formatC, digits = digits, format = "E"), "data.txt")   
  else {
    data.list <- lapply(as.list(data), get, pos = parent.frame(2))
    names(data.list) <- as.list(data)
    write.datafile(lapply(data.list, formatC, digits = digits, format = "E"), "data.txt")
  }
  if(!is.null(inits)){
      for (i in 1:n.chains){
        if (is.function(inits))
          write.datafile(lapply(inits(), formatC, digits = digits, format = "E"),
            paste ("inits", i, ".txt", sep=""))
        else
          write.datafile(lapply(inits[[i]], formatC, digits = digits, format = "E"),
            paste ("inits", i, ".txt", sep=""))
      }
  }
}
