plot.bugs <- function (x, display.parallel=FALSE, ...){
    mar.old <- par("mar")
    par (pty = "m")
    layout(matrix(c(1,2),1,2))
    bugs.plot.summary (x, ...)
    bugs.plot.inferences (x, display.parallel, ...)
    mtext (paste ("Bugs model at \"", x$model.file, "\", ", x$n.chains,
      " chains, each with ", x$n.iter, " iterations", sep=""),
           outer=TRUE, line=-1, cex=.7)
    par (mar = mar.old)
}
