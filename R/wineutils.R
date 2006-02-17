## from Jun Yan's rbugs package, extended

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
