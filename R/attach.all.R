attach.all <- function(.a, overwrite = FALSE){
  if(class(.a) != "bugs")
    stop("attach.all() requires a bugs object.")
  .a <- c(.a, .a$sims.list)
  if (overwrite){
    for (j in 1:length(.a)){
      if (names(.a)[j] %in% ls(.GlobalEnv))
        remove (list=names(.a)[j], envir=.GlobalEnv)
    }
  }
  attach(.a)
}

detach.all <- function() detach(.a)
