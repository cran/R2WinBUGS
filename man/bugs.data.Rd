\name{bugs.data}
\alias{bugs.data}
\title{Writing input for WinBUGS}
\description{Write file \file{data.txt} for WinBUGS to read.}
\usage{
bugs.data(data, dir = getwd(), digits = 5)
}
\arguments{
  \item{data}{either a named list (names corresponding to variable names in the \code{model.file}) 
    of the data for the WinBUGS model, \emph{or}
    a vector or list of the names of the data objects used by the model}
  \item{dir}{the directory to write the file \file{data.txt} to}
  \item{digits}{number of significant digits used for WinBUGS input, see \code{\link{formatC}}}
}
\value{
    Does not return anything.
}
\seealso{\code{\link{bugs}}}
\keyword{file}
