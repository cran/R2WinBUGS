\name{read.bugs}
\alias{read.bugs}
\title{Read output files in CODA format}
\description{This function reads Markov Chain Monte Carlo output in the CODA format produced by WinBUGS
    and returns an object of class \code{\link[coda]{mcmc.list}} for further output analysis 
    using the \sQuote{coda} package.}
\usage{
read.bugs(codafiles, ...)
}
\arguments{
  \item{codafiles}{character vector of filenames (e.g. returned from \code{\link{bugs}} 
    in call such as \code{bugs(....., codaPkg = TRUE, .....)}). 
    Each of the files contains coda output for one chain produced by WinBUGS,
    the \emph{directory} name of the corresponding file \file{codaIndex.txt} 
    is extracted from the first element of \code{codafiles}.}
  \item{...}{further arguments to be passed to \code{\link[coda]{read.coda}}}
}
\seealso{\code{\link{bugs}}, \code{\link[coda]{read.coda}}, \code{\link[coda]{mcmc.list}}}
\keyword{file}
