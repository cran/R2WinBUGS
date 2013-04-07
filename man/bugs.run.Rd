\name{bugs.run}
\alias{bugs.run}
\title{Calling WinBUGS}

\description{Updates the lengths of the adaptive phases in the
  \pkg{WinBUGS} registry (using \code{\link{bugs.update.settings}}),
  calls \pkg{WinBUGS} and runs it with \file{script.txt} - intended for
  internal use}

\usage{
bugs.run(n.burnin, bugs.directory,
         useWINE=.Platform$OS.type != "windows", WINE=NULL,
         newWINE=TRUE, WINEPATH=NULL)
}
\arguments{
  \item{n.burnin}{length of burn in}
  \item{bugs.directory}{directory that contains the \pkg{WinBUGS} executable}
  \item{useWINE}{as in \code{\link{bugs}} meta function}
  \item{WINE}{as in \code{\link{bugs}} meta function}
  \item{newWINE}{as in \code{\link{bugs}} meta function}
  \item{WINEPATH}{as in \code{\link{bugs}} meta function}
}

\value{Nothing, but has side effects as documented in
  \code{\link{bugs.update.settings}} and calls \pkg{WinBUGS}.}

\seealso{The main function to be called by the user is \code{\link{bugs}}.}
\keyword{internal}
