\name{dtget}
\alias{dtget}
\alias{dtcall}
\alias{dtprint}
\alias{dtput}
\alias{dlisp}
\title{
  Get/Print Objects From or Put Objects Into Temporary Work Environment for PBSdata
}
\description{
  These functions are wrappers to the PBSmodelling accessor functions that
  get/print objects from or put objects into a temporary work environment,
  in this case \code{.PBSdataEnv}.
}
\usage{
dtget(...)
dtcall(...)
dtprint(...)
dtput(...)
dlisp(...)
}
\arguments{
  \item{...}{For \code{dtget} through to \code{dtput}, the only free argument is: \cr
    \code{x} -- name (with or without quotes) of an object to retrieve or store 
    in the temporary environment; cannot be represented by a variable. \cr
    Fixed arguments: \code{penv = parent.frame(), tenv = .PBSdataEnv} \cr
    See \code{\link[PBSmodelling]{tget}} for additional information.

    For \code{dlisp}, there is only one fixed argument: \cr
    \code{pos = .PBSdataEnv} \cr
    All other arguments are available -- see \code{\link[PBSmodelling]{lisp}} }
}
\details{
  These accessor functions were developed as a response to the CRAN
  repository policy statement: \dQuote{Packages should not modify the
  global environment (user's workspace).}
  
}
\value{
  Objects are retrieved from or sent to the temporary working 
  environment to/from the place where the function(s) are called.
  Additionally, \code{dtcall} invisibly returns the object without 
  transferring, which is useful when the object is a function that the
  user may wish to call, for example, \code{dtcall(myfunc)()}.
}
\references{
  CRAN Repository Policy: 
  \url{https://cran.r-project.org/web/packages/policies.html}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-27}
}
\seealso{
\code{\link[PBSmodelling]{tget}} and \code{\link[PBSmodelling]{lisp}} in \pkg{PBSmodelling}
}
\keyword{manip}
\keyword{environment}
\concept{M01_Utility}
