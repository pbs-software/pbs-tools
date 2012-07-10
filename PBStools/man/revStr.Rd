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
\author{\code{base::strsplit}}
\seealso{ \code{\link{wrapText}} }
\examples{
pbsfun=function() {
  cat(revStr("Nardwuar the Human Serviette"),"\n")
  invisible() }
pbsfun()
}
\keyword{character}
