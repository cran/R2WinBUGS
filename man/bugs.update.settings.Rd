\name{bugs.update.settings}
\alias{bugs.update.settings}
\title{Adjusting WinBUGS registry - intended for internal use only}
\description{Adjusts WinBUGS registry - intended for internal use only
}
\usage{
bugs.update.settings(n.burnin, bugs.directory)
}
\arguments{
  \item{n.burnin}{length of burn in}
  \item{bugs.directory}{directory that contains the WinBUGS executable}
}
\details{
Alter the adaptive phases in the WinBUGS updaters (in the \file{Registry.odc} file)
so that all adaptive updating is done during the burnin stage. Otherwise
WinBUGS will not report results if the burnin is less than a minimum value
such as 500 or 4000 (depending on the model).
}
\value{
Does not return anything.
}
\seealso{The main function to be called by the user is \code{\link{bugs}}.}
\keyword{internal}

