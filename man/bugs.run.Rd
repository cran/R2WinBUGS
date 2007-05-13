\name{bugs.run}
\alias{bugs.run}
\title{Calling WinBUGS - intended for internal use only}
\description{
Updates the lengths of the adaptive phases in the WinBUGS registry (using \code{\link{bugs.update.settings}}),
calls WinBUGS and runs it with \file{script.txt}. Intended for internal use only}
\usage{
bugs.run(n.burnin, bugs.directory, WINE = "", 
         useWINE = .Platform$OS.type != "windows", 
         newWINE = TRUE, WINEPATH = NULL)
}
\arguments{
  \item{n.burnin}{length of burn in}
  \item{bugs.directory}{directory that contains the WinBUGS executable}  
  \item{WINE}{name of WINE binary, for Windows emulation}
  \item{useWINE}{logical; attempt to use the WINE emulator to run WinBUGS, 
    defaults to \code{TRUE} on Windows, and \code{FALSE} otherwise.
    The \code{useWINE = TRUE} option is not available in S-PLUS.}
  \item{newWINE}{set this one to \code{TRUE} for new versions of WINE.} 
  \item{WINEPATH}{Path the WINE, \code{bugs} tries hard to get the information automatically and 
    pass it to this function, if not given.}
}
\value{
    Does not return anything.
}
\seealso{The main function to be called by the user is \code{\link{bugs}}.}
\keyword{internal}
