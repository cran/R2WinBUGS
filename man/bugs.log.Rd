\name{bugs.log}
\alias{bugs.log}
\title{Read data from WinBUGS logfile}
\description{Read data such as summary statistics and DIC information from the WinBUGS logfile.}
\usage{
bugs.log(file)
}
\arguments{
  \item{file}{Location of the WinBUGS logfile.}
}
\value{
    A list with components
    \item{stats}{A matrix containing summary statistics for each element of each saved parameter. 
        Comparable to the information in the element \code{summary} of a bugs object as returned by \code{\link{bugs}}.}
    \item{DIC}{A matrix containg the DIC statistics as returned from WinBUGS 
        (which is different from the way \code{\link{bugs}} calculates it).}
}
\details{
In later releases of R2WiNBUGS, this function is considered to read the relevant data from the log file rather than 
analysing and calculating the relevant data in R again.
}
\seealso{The main function that generates the log file is \code{\link{bugs}}.}
\keyword{IO}
