\name{getName}
\alias{getName}
\title{Get String Names from Literals or Named Objects}
\description{
  Get string names from user supplied input. If the name supplied
  exists as an object in the parent frame, the object will be assessed
  for its potential as a source of names.
}
\usage{
getName(fnam)
}
\arguments{
  \item{fnam}{ file name(s) specified directly or through names in objects.}
}
\details{
  If \code{fnam} exists as a list, the function returns the names of the list.\cr
  If \code{fnam} exists as a string vector, the function returns the strings in the vector.\cr
  If \code{fnam} does not exist as an object, it simply returns itself as a string.
}
\value{
  A vector of string names.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\seealso{ 
  \code{\link{getFile}}, \code{\link{getData}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function() {
  cat("Data object 'swiss' doesn't appear in the parent frame\n")
  print(getName(swiss))
  swiss=swiss
  cat("And now it does, so it acts like a source of names\n")
  print(getName(swiss))
  invisible() }
pbsfun()
})
}
\keyword{ data }
\keyword{ utilities }
