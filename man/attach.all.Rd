\name{attach.all}
\alias{attach.all}
\alias{detach.all}
\title{Attach / detach elements of a bugs objects to search path}
\description{The database is attached to the \R search path. See \code{\link{attach}} for details.}
\usage{
    attach.all(.a, overwrite = FALSE)
    detach.all()
}
\arguments{
  \item{.a}{A \code{bugs} object}
  \item{overwrite}{If \code{TRUE}, overwrites variables if necessary.
    (The usual "attach" function in R only attaches the variables that are not yet defined.)}
}
\note{
    Without detaching, do not use \code{attach.all} on another \code{bugs} object,
    because instead of the given name, an object called \code{.a} is attached.
    Therefore strange things may happen...
}
\value{
  \code{attach.all} invisibly returns the \code{\link{environment}}.\cr
  \code{detach.all} detaches the \code{environment} named \code{.a} created by \code{attach.all}.
}
\seealso{\code{\link{bugs}}, \code{\link{attach}}, \code{\link{detach}}}
\examples{
# An example model file is given in:
model.file <- file.path(.path.package("R2WinBUGS"), "model", "schools.txt")
# Some example data (see ?schools for details):
data(schools)  
J <- nrow(schools)
y <- schools$estimate
sigma.y <- schools$sd
data <- list ("J", "y", "sigma.y")
inits <- function(){
    list(theta = rnorm(J, 0, 100), mu.theta = rnorm(1, 0, 100), 
        sigma.theta = runif(1, 0, 100))
}
parameters <- c("theta", "mu.theta", "sigma.theta")      
\dontrun{
## You may need to edit "bugs.directory",
## also you need write access in the working directory:
schools.sim <- bugs(data, inits, parameters, model.file, 
    n.chains = 3, n.iter = 1000, 
    bugs.directory = "c:/Program Files/WinBUGS14/",
    working.directory = NULL)

# Do some inferential summaries
attach.all(schools.sim)
# posterior probability that the coaching program in school A 
# is better than in school C:
print(mean(theta[,1] > theta[,3]))
# 50% posterior interval for the difference between school A's 
# and school C's program:
print(quantile(theta[,1] - theta[,3], c(.25, .75)))  
plot(theta[,1], theta[,3]) 
}
}
\keyword{data}
