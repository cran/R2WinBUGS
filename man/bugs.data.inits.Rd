\name{bugs.data.inits}
\alias{bugs.data.inits}
\title{Writing input for WinBUGS - intended for internal use only}
\description{Write files \file{data.txt} and \file{inits1.txt}, \file{inits2.txt}, etc., in the working directory 
for WinBUGS to read. For internal use only.}
\usage{
bugs.data.inits(data, inits, n.chains, digits)
}
\arguments{
  \item{data}{list of the data for the WinBUGS model, \emph{or}
    a vector of the names of the data objects used by the model}
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
