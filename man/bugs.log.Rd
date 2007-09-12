\name{bugs.log}
\alias{bugs.log}
\title{Read data from WinBUGS logfile}
\description{Read data such as summary statistics and DIC information
  from the \pkg{WinBUGS} logfile}
\usage{
bugs.log(file)
}
\arguments{
  \item{file}{Location of the \pkg{WinBUGS} logfile}
}
\value{
  A list with components:
  \item{stats}{A matrix containing summary statistics for each saved
    parameter. Comparable to the information in the element
    \code{summary} of a bugs object as returned by \code{\link{bugs}}.}
  \item{DIC}{A matrix containing the DIC statistics as returned from
    \pkg{WinBUGS}.}
}

\details{ In later releases of \pkg{R2WinBUGS}, this function is
considered to read the relevant data from the log file rather than
analysing and calculating the relevant data in \R / S-PLUS again.  }

\seealso{The main function that generates the log file is \code{\link{bugs}}.}
\keyword{IO}
\keyword{file}
