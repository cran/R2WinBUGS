\name{bugs.inits}
\alias{bugs.inits}
\title{Writing input for WinBUGS - intended for internal use only}
\description{Write files \file{inits1.txt}, \file{inits2.txt}, etc., in the working directory 
for WinBUGS to read. For internal use only.}
\usage{
bugs.inits(inits, n.chains, digits)
}
\arguments{
  \item{inits}{a list with \code{n.chains} elements; each element of the list is
    itself a list of starting values for the WinBUGS model, \emph{or} 
    a function creating (possibly random) initial values}
  \item{n.chains}{number of Markov chains}
  \item{digits}{number of significant digits used for WinBUGS input, see \code{\link{formatC}}}
}
\value{
    Does not return anything.
}
\seealso{The main function to be called by the user is \code{\link{bugs}}.}
\keyword{internal}
