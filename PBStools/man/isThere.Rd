\name{isThere}
\alias{isThere}
\title{
  Check Whether an Object Physically Exists
}
\description{
  Check to see whether objects physically exist in the specified environment.
  This differs from the function \code{exists} in that the latter sees objects across 
  environments.
}
\usage{
isThere(x, envir=parent.frame())
}
\arguments{
  \item{x}{a variable name (given as a character string).}
  \item{envir}{specify an environment to look in (e.g., \code{sys.frame(sys.nframe())}) }
}
\details{
  This function looks in the specified environment and the object must be physically
  present to elicit a \code{TRUE} response. This contrasts with the base function \code{exists}.
}
\value{
  A Boolean vector.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2021-06-15}\cr
}
\seealso{
  \code{\link{getFile}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function() {
  cat("Data object 'swiss' appears to exist in pos=0\n")
  cat(paste("   exists(\"swiss\",envir=sys.frame(0))",
    exists("swiss",envir=sys.frame(0))),"\n")
  cat("But it isn't really there...\n")
  cat(paste("   isThere(\"swiss\",envir=sys.frame(0))",
    isThere("swiss",envir=sys.frame(0))),"\n")
  invisible() }
pbsfun()
})
}
\keyword{logic}
\concept{M01_Utility}

