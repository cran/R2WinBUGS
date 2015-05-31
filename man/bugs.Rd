\name{bugs}
\alias{bugs}
\title{Run WinBUGS and OpenBUGS from R or S-PLUS}

\description{The \code{bugs} function takes data and starting values as
  input.  It automatically writes a \pkg{WinBUGS} script, calls the model, and
  saves the simulations for easy access in \R or S-PLUS.}

\usage{
bugs(data, inits, parameters.to.save, model.file="model.bug",
    n.chains=3, n.iter=2000, n.burnin=floor(n.iter/2),
    n.thin=max(1, floor(n.chains * (n.iter - n.burnin) / n.sims)),
    n.sims = 1000, bin=(n.iter - n.burnin) / n.thin,
    debug=FALSE, DIC=TRUE, digits=5, codaPkg=FALSE,
    bugs.directory="c:/Program Files/WinBUGS14/",
    program=c("WinBUGS", "OpenBUGS", "winbugs", "openbugs"),
    working.directory=NULL, clearWD=FALSE,
    useWINE=.Platform$OS.type != "windows", WINE=NULL,
    newWINE=TRUE, WINEPATH=NULL, bugs.seed=NULL, summary.only=FALSE,
    save.history=!summary.only, over.relax = FALSE)
}

\arguments{
  \item{data}{either a named list (names corresponding to variable names
    in the \code{model.file}) of the data for the \pkg{WinBUGS} model,
    \emph{or} (which is not recommended and unsafe) a vector or list of the names of the data objects used by
    the model. If \code{data} is a one element character vector (such as \code{"data.txt"}), 
    it is assumed that data have already been written to the working directory into that file, 
    e.g. by the function \code{\link{bugs.data}}.}
  \item{inits}{a list with \code{n.chains} elements; each element of the
    list is itself a list of starting values for the \pkg{WinBUGS} model,
    \emph{or} a function creating (possibly random) initial values.
    Alternatively, if \code{inits=NULL}, initial values are generated
    by \pkg{WinBUGS}. If \code{inits} is a character vector with \code{n.chains} elements, 
    it is assumed that inits have already been written to the working directory into those files, 
    e.g. by the function \code{\link{bugs.inits}}.}
  \item{parameters.to.save}{character vector of the names of the
    parameters to save which should be monitored}
  \item{model.file}{file containing the model written in \pkg{WinBUGS} code.
    The extension can be either \file{.bug} or \file{.txt}.
    If the extension is \file{.bug} and \code{program=="WinBUGS"},
    a copy of the file with extension \file{.txt} will be created
    in the \code{bugs()} call and removed afterwards. Note that
    similarly named \file{.txt} files will be overwritten.
    Alternatively, \code{model.file} can be an R function that contains a BUGS model that is written to a 
    temporary model file (see \code{\link{tempfile}}) using \code{\link{write.model}}.}
  \item{n.chains}{number of Markov chains (default: 3)}
  \item{n.iter}{number of total iterations per chain (including burn in;
    default: 2000)}
  \item{n.burnin}{length of burn in, i.e. number of iterations to
    discard at the beginning. Default is \code{n.iter/2}, that is,
    discarding the first half of the simulations.}
  \item{n.thin}{thinning rate. Must be a positive integer.  Set
    \code{n.thin} > 1 to save memory and computation time if
    \code{n.iter} is large.  Default is \code{max(1, floor(n.chains *
    (n.iter-n.burnin) / 1000))} which will only thin if there are at
    least 2000 simulations.}
  \item{n.sims}{The approximate number of simulations to keep after thinning.}
  \item{bin}{number of iterations between saving of results
    (i.e. the coda files are saved after each \code{bin} iterations);
    default is to save only at the end.}
  \item{debug}{if \code{FALSE} (default), \pkg{WinBUGS} is closed automatically
    when the script has finished running, otherwise \pkg{WinBUGS} remains open
    for further investigation}
  \item{DIC}{logical; if \code{TRUE} (default), compute deviance, pD,
    and DIC. This is done in \pkg{WinBUGS} directly using the rule \code{pD =
    Dbar - Dhat}.  If there are less iterations than required for the
    adaptive phase, the rule \code{pD=var(deviance) / 2} is used.}
  \item{digits}{number of significant digits used for \pkg{WinBUGS} input, see
    \code{\link{formatC}}}
  \item{codaPkg}{logical; if \code{FALSE} (default) a \code{bugs} object
    is returned, if \code{TRUE} file names of \pkg{WinBUGS} output are
    returned for easy access by the \pkg{coda} package through function
    \code{\link{read.bugs}} (not used if \code{program="OpenBUGS"}).
    A \code{bugs} object can be converted to an \code{mcmc.list} object as 
    used by the \pkg{coda} package with the method \code{\link[coda]{as.mcmc.list}} 
    (for which a method is provided by R2WinBUGS).}
  \item{bugs.directory}{directory that contains the \pkg{WinBUGS} executable.
    If the global option \code{R2WinBUGS.bugs.directory} is not \code{NULL}, 
    it will be used as the default.}
  \item{program}{the program to use, either
    \code{winbugs}/\code{WinBUGS} or \code{openbugs}/\code{OpenBUGS},
    the latter makes use of function \code{\link{openbugs}} and requires
    the CRAN package \pkg{BRugs}. The \code{openbugs}/\code{OpenBUGS}
    choice is not available in S-PLUS.}
  \item{working.directory}{sets working directory during execution of
    this function; \pkg{WinBUGS}' in- and output will be stored in this
    directory; if \code{NULL}, a temporary working directory via
    \code{\link{tempdir}} is used.}
  \item{clearWD}{logical; indicating whether the files \file{data.txt},
    \file{inits[1:n.chains].txt}, \file{log.odc}, \file{codaIndex.txt},
    and \file{coda[1:nchains].txt} should be removed after \pkg{WinBUGS} has
    finished.  If set to \code{TRUE}, this argument is only respected if
    \code{codaPkg=FALSE}.}

  \item{useWINE}{logical; attempt to use the Wine emulator to run
    \pkg{WinBUGS}, defaults to \code{FALSE} on Windows, and \code{TRUE}
    otherwise. Not available in S-PLUS.}
  \item{WINE}{character, path to \file{wine} binary file, it is
    tried hard (by a guess and the utilities \code{which} and \code{locate}) 
    to get the information automatically if not given.}
  \item{newWINE}{Use new versions of Wine that have \file{winepath}
    utility}
  \item{WINEPATH}{character, path to \file{winepath} binary file, it is
    tried hard (by a guess and the utilities \code{which} and \code{locate}) 
    to get the information automatically if not given.}
  \item{bugs.seed}{random seed for \pkg{WinBUGS} (default is no seed)}
  \item{summary.only}{If \code{TRUE}, only a parameter summary for very quick analyses is given, 
    temporary created files are not removed in that case.}
  \item{save.history}{If \code{TRUE} (the default), trace plots are generated at the end.}
  \item{over.relax}{If \code{TRUE}, over-relaxed form of MCMC is used if available from WinBUGS.}
}

\details{
  To run:
  \enumerate{
    \item Write a \pkg{BUGS} model in an ASCII file (hint: use
      \code{\link{write.model}}).
    \item Go into \R / S-PLUS.
    \item Prepare the inputs for the \code{bugs} function and run it (see
      Example section).
    \item A \pkg{WinBUGS} window will pop up and \R / S-PLUS will freeze
    up. The model will now run in \pkg{WinBUGS}. It might take awhile. You
    will see things happening in the Log window within \pkg{WinBUGS}. When
    \pkg{WinBUGS} is done, its window will close and \R / S-PLUS will work
    again.
    \item If an error message appears, re-run with \code{debug=TRUE}.
  }

  BUGS version support:
  \itemize{
    \item{\pkg{WinBUGS} 1.4.*}{default}
    \item{\pkg{OpenBUGS} 2.*}{via argument \code{program="OpenBUGS"}}
  }

  Operation system support:
  \itemize{
    \item{\pkg{MS Windows}}{no problem}
    \item{\pkg{Linux}, \pkg{Mac OS X} and \pkg{Unix} in
      general}{possible with Wine emulation via \code{useWINE=TRUE}, but
      only for \pkg{WinBUGS} 1.4.*}
  }

  If \code{useWINE=TRUE} is used, all paths (such as
  \code{working.directory} and \code{model.file}, must be given in
  native (Unix) style, but \code{bugs.directory} can be given in
  Windows path style (e.g. \dQuote{c:/Program Files/WinBUGS14/}) or
  native (Unix) style
  (e.g. \dQuote{/path/to/wine/folder/dosdevices/c:/Program
  Files/WinBUGS14}).  This is done to achieve greatest portability with
  default argument value for \code{bugs.directory}.

}

\value{
  If \code{codaPkg=TRUE} the returned values are the names
  of coda output files written by \pkg{WinBUGS} containing
  the Markov Chain Monte Carlo output in the CODA format.
  This is useful for direct access with \code{\link{read.bugs}}.

  If \code{codaPkg=FALSE}, the following values are returned:
  \item{n.chains}{see Section \sQuote{Arguments}}
  \item{n.iter}{see Section \sQuote{Arguments}}
  \item{n.burnin}{see Section \sQuote{Arguments}}
  \item{n.thin}{see Section \sQuote{Arguments}}
  \item{n.keep}{number of iterations kept per chain (equal to
    \code{(n.iter-n.burnin) / n.thin})}
  \item{n.sims}{number of posterior simulations (equal to
    \code{n.chains * n.keep})}
  \item{sims.array}{3-way array of simulation output, with dimensions
    n.keep, n.chains, and length of combined parameter vector}
  \item{sims.list}{list of simulated parameters:
    for each scalar parameter, a vector of length n.sims
    for each vector parameter, a 2-way array of simulations,
    for each matrix parameter, a 3-way array of simulations, etc.
    (for convenience, the \code{n.keep*n.chains} simulations in
    sims.matrix and sims.list (but NOT sims.array) have been randomly
    permuted)}
  \item{sims.matrix}{matrix of simulation output, with
    \code{n.chains*n.keep} rows and one column for each element of
    each saved parameter (for convenience, the \code{n.keep*n.chains}
    simulations in sims.matrix and sims.list (but NOT sims.array) have
    been randomly permuted)}
  \item{summary}{summary statistics and convergence information for
    each saved parameter.}
  \item{mean}{a list of the estimated parameter means}
  \item{sd}{a list of the estimated parameter standard deviations}
  \item{median}{a list of the estimated parameter medians}
  \item{root.short}{names of argument \code{parameters.to.save} and
    \dQuote{deviance}}
  \item{long.short}{indexes; programming stuff}
  \item{dimension.short}{dimension of \code{indexes.short}}
  \item{indexes.short}{indexes of \code{root.short}}
  \item{last.values}{list of simulations from the most recent
    iteration; they can be used as starting points if you wish to run
    \pkg{WinBUGS} for further iterations}
  \item{pD}{an estimate of the effective number of parameters, for
    calculations see the section \dQuote{Arguments}.}
  \item{DIC}{\code{mean(deviance) + pD}}
}

\references{
    Gelman, A., Carlin, J.B., Stern, H.S., Rubin, D.B. (2003):
    \emph{Bayesian Data Analysis}, 2nd edition, CRC Press.

     Sturtz, S., Ligges, U., Gelman, A. (2005):
     R2WinBUGS: A Package for Running WinBUGS from R.
     \emph{Journal of Statistical Software} 12(3), 1-16.
}

\author{Andrew Gelman, \email{gelman@stat.columbia.edu}; modifications and
  packaged by Sibylle Sturtz, \email{sturtz@statistik.tu-dortmund.de},
  and Uwe Ligges.}

\seealso{\code{\link{print.bugs}}, \code{\link{plot.bugs}}, as well as
  \pkg{coda} and \pkg{BRugs} packages}

\examples{
# An example model file is given in:
model.file <- system.file(package="R2WinBUGS", "model", "schools.txt")
# Let's take a look:
file.show(model.file)

# Some example data (see ?schools for details):
data(schools)
schools

J <- nrow(schools)
y <- schools$estimate
sigma.y <- schools$sd
data <- list(J=J, y=y, sigma.y=sigma.y)
inits <- function(){
    list(theta=rnorm(J, 0, 100), mu.theta=rnorm(1, 0, 100),
         sigma.theta=runif(1, 0, 100))
}
## or alternatively something like:
# inits <- list(
#   list(theta=rnorm(J, 0, 90), mu.theta=rnorm(1, 0, 90),
#        sigma.theta=runif(1, 0, 90)),
#   list(theta=rnorm(J, 0, 100), mu.theta=rnorm(1, 0, 100),
#        sigma.theta=runif(1, 0, 100))
#   list(theta=rnorm(J, 0, 110), mu.theta=rnorm(1, 0, 110),
#        sigma.theta=runif(1, 0, 110)))

parameters <- c("theta", "mu.theta", "sigma.theta")

\dontrun{
## You may need to edit "bugs.directory",
## also you need write access in the working directory:
schools.sim <- bugs(data, inits, parameters, model.file,
    n.chains=3, n.iter=5000,
    bugs.directory="c:/Program Files/WinBUGS14/")
print(schools.sim)
plot(schools.sim)
}
}

\keyword{interface}
\keyword{models}
