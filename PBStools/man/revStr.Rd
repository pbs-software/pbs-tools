\name{revStr}
\alias{revStr}
\title{Reverse a String}
\description{
  Reverse a string character by character.
}
\usage{
revStr(x)
}
\arguments{
  \item{x}{vector of strings}
}
\value{
  A vector strings that have been reversed.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2021-06-15}\cr
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function() {
  cat(revStr("Nardwuar the Human Serviette"),"\n")
  invisible() }
pbsfun()
})
}
\keyword{character}
\concept{M01_Utility}
