\name{ttget}
\alias{ttget}
\alias{ttcall}
\alias{ttprint}
\alias{ttput}
\alias{tlisp}
\title{
  Get/Print Objects From or Put Objects Into Temporary Work Environment for PBStools
}
\description{
  Thes functions are wrappers to the PBSmodelling accessor functions that
  get/print objects from or put objects into a temporary work environment,
  in this case \code{.PBStoolEnv}. Working objects include \code{PBSfish},
  which acts as a storage object for many of the functions.
}
\usage{
ttget(...)
ttcall(...)
ttprint(...)
ttput(...)
tlisp(...)
}
\arguments{
  \item{...}{For \code{ttget} and related, only allow the \code{\link[PBSmodelling]{tget}} arguments: \cr
    \code{x} -- name (with or without quotes) of an object to retrieve or store in the temporary environment; \cr
    \code{penv} -- parent environment, defaults to \code{parent.frame()} called from within the function. \cr\cr
    Fixed argument:\cr
    \code{tenv} -- temporary working environment, set to \code{.PBStoolEnv}.\cr

    For \code{tlisp}, only allow the \code{\link[PBSmodelling]{lisp}} arguments: \cr
    \code{name} -- which environment to use in listing the available objects 
    (see the details section of \code{\link[base]{ls}}); \cr
    \code{envir} --  an alternative argument to \code{name} for specifying the environment; \cr
    \code{all.names} --  a logical value: if \code{TRUE}, all object names are returned;
    if \code{FALSE}, names which begin with a  \samp{.} are omitted; \cr
    \code{pattern} --  an optional \link{regular expression}; only names matching \code{pattern} are returned
    (see \code{\link[base]{ls}} for additional details).\cr\cr
    Fixed argument:\cr
    \code{pos} -- an alternative argument for specifying the environment, set to \code{.PBStoolEnv}.}
}
\details{
  These accessor functions were developed as a response to the CRAN
  repository policy statement: \dQuote{Packages should not modify the
  global environment (user's workspace).}
  
  [NOT YET IMPLEMENTED] There are also wrapper functions called \code{.win.ttget}, 
  \code{.win.ttcall}, and \code{.win.ttprint} that can be used in 
  \emph{window description files} to launch functions or print objects
  from the \code{.PBStoolEnv} workspace. 
  The wrapper uses \code{getWinAct} to get the function (or object) 
  name that a user specifies in the \code{action} argument of a 
  widget command.
}
\note{
  Additional wrapper functions to access functions in \code{.PBStoolEnv}
  are named with the prefix \code{.win}.
}
\value{
  Objects are retrieved from or sent to the temporary working 
  environment to/from the place where the function(s) are called.
  Additionally, \code{ttcall} invisibly returns the object without 
  transferring, which is useful when the object is a function that the
  user may wish to call, for example, \code{ttcall(myfunc)()}.
}
\references{
  CRAN Repository Policy: 
  \url{https://cran.r-project.org/web/packages/policies.html}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2020-10-08}\cr
}
%\seealso{}
\keyword{manip}
\keyword{environment}
\concept{M01_Utility}

