## from Jun Yan's rbugs package, extended

if (is.R()){

## get drive mapping table from ~/.wine/config
winedriveMap <- function(config="~/.wine/config") {
  if (!file.exists(config)) return (NULL);
  con <- readLines(config)
  con <- con[- grep("^;", con)]
  drive <- con[grep("^\\[Drive ", con)]
  drive <- substr(drive, 8, 8)
  drive <- paste(drive, ":", sep="")
  path <- con[grep("Path", con)]
  len <- length(drive)
  path <- path[1:len]
  dir <- sapply(path, 
                 function(x) {
                   foo <- unlist(strsplit(x, "\""))
                   foo[length(foo)]
                 })
  dir <- sub("%HOME%",tools::file_path_as_absolute("~"),dir)
  data.frame(drive = I(drive), path = I(dir), row.names=NULL)
}

## translate windows dir to native dir
winedriveTr <- function(windir, DriveTable=winedriveMap()) {
  win.dr <- substr(windir, 1, 2)
  ind <- pmatch(toupper(win.dr), DriveTable$drive)
  native.dr <- DriveTable$path[ind]
  sub(win.dr, native.dr, windir)
}

## translate full Unix path to Wine path
winedriveRTr <- function(unixpath, DriveTable=winedriveMap()) {
  blocks <- strsplit(unixpath,"/")[[1]]
  cblocks <- c("/",sapply(1+seq(along=blocks[-1]),
                    function(n) paste(blocks[1:n],collapse="/")))
  path <- match(cblocks,DriveTable$path)
  if (any(!is.na(path))) {
    unixdir <- cblocks[which.min(path)]
    windrive <- paste(DriveTable$drive[min(path,na.rm=TRUE)],"/",sep="")
    winpath <- sub("//","/",sub(unixdir,windrive,unixpath)) ## kluge
  } else stop("can't find equivalent Windows path: file may be inaccessible")
  winpath
}


win2native <- function(x, useWINE=.Platform$OS.type != "windows") { # win -> native
  if (useWINE) winedriveTr(x)
  else x
}

} # end if (is.R())

native2win <- function(x, useWINE=.Platform$OS.type != "windows", newWINE=TRUE) { # native -> win
  if(is.R()){
      if (useWINE && !newWINE) return(winedriveRTr(x))
      if (useWINE && newWINE) {
          x <- system(paste(WINEPATH, "-w", x), intern = TRUE)
          return(gsub("\\\\", "/", x)) ## under wine BUGS cannot use \ or \\
      } else x
  } else { #S-PLUS
      gsub("\\\\", "/", x)  
  }
}
