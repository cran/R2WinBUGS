\name{write.datafile}
\alias{write.datafile}
\alias{formatdata}
\title{Write data for WinBUGS - intended for internal use only}
\description{Write data in files that can be read by WinBUGS - intended for internal use only}
\usage{
write.datafile(datalist, towhere, fill = TRUE)
formatdata(datalist)
}
\arguments{
  \item{datalist}{a \emph{list} to be written into an appropriate structure}
  \item{towhere}{the name of the file which the data are to be written to}
  \item{fill}{see \code{\link{cat}}, defaults to \code{TRUE}}
}
\value{
  \item{datalist.tofile}{A structure appropriate to be read in by WinBUGS.}
}
\seealso{The main function to be called by the user is \code{\link{bugs}}.}
\keyword{internal}
