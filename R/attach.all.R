attach.all <- function(x, overwrite = NA, name = "attach.all"){
    rem <- names(x) %in% ls(.GlobalEnv)
    if(!any(rem)) overwrite <- FALSE
    rem <- names(x)[rem]
    if(is.na(overwrite))
        if(interactive()){
            overwrite <- "YES" == winDialog(type = "yesno", 
                paste("The following objects in .GlobalEnv will mask\nobjects in the attached database:\n", 
                      paste(rem, collapse=", "), 
                      "\nRemove these objects from .GlobalEnv?", sep=""))
        }
        else overwrite <- FALSE
    if(overwrite) remove(list=rem, envir=.GlobalEnv)
    attach(x, name=name)
}

attach.bugs <- function (x, overwrite = NA){
  if(class(x) != "bugs")
    stop("attach.all() requires a bugs object.")
  r1 <- attach.all(x, overwrite = overwrite, name = "bugs.all")
  r2 <- attach.all(x$sims.list, overwrite = overwrite, name = "bugs.sims")
  invisible(c(bugs.all=r1, bugs.sims=r2))
}


detach.all <- function(name = "attach.all")
    do.call("detach", list(name=name))

detach.bugs <- function(){
    detach.all("bugs.all")
    detach.all("bugs.sims")
}
