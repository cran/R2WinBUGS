\name{bugs.run}
\alias{bugs.run}
\title{Calling WinBUGS - intended for internal use only}
\description{
Updates the lengths of the adaptive phases in the WinBUGS registry (using \code{\link{bugs.update.settings}}),
calls WinBUGS and runs it with \file{script.txt}. Intended for internal use only}
\usage{
bugs.run(n.burnin, bugs.directory, WINE = "")
}
\arguments{
  \item{n.burnin}{length of burn in}
  \item{bugs.directory}{directory that contains the WinBUGS executable}  
  \item{WINE}{name of WINE binary, for Windows emulation}
}
\value{
    Does not return anything.
}
\seealso{The main function to be called by the user is \code{\link{bugs}}.}
\keyword{internal}
