\name{prime}
\alias{prime}
\title{
  Report Prime Numbers from an Integer Vector
}
\description{
  Report the prime numbers given an integer vector. 
  If none are found, the function returns \code{NULL}.
}
\usage{
prime(x = 2:100)
}
\arguments{
  \item{x}{an integer vector that may or may not contain prime numbers.}
}
\details{
  The function will reject non-vector objects, non-numeric vectors,
  and numeric vectors that do not comprise integers.
}
\value{
  A vector of prime numbers (including duplicates) that appear in the input vector.
  If no prime numbers are present, the function returns \code{NULL}.
}
\references{
  \url{http://en.wikipedia.org/wiki/Prime_number}
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\seealso{
  \code{\link{biteData}}, \code{\link{isThere}}, \code{\link{revStr}}
}
\keyword{IO}
\keyword{manip}
