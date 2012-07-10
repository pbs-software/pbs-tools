\name{chewData}
\alias{chewData}
\title{Remove Sparse Category Records from Data Matrix/Frame}
\description{
  Remove records from a data frame or matrix that contribute little 
  information to unique categories of the factor specified.
}
\usage{
chewData(dat, fac, nmin=3, na.rm=TRUE)
}
\arguments{
  \item{dat}{matrix or data frame with named fields (row names).}
  \item{fac}{string scalar corresponding to a field name in \code{dat}.}
  \item{nmin}{minimum number of records in a factor category; if reached, 
    records are discarded from \code{dat}.}
  \item{na.rm}{logical: if \code{TRUE}, remove records with unlabelled categories.}
}
\details{
  Records are removed from a data frame/matrix if a specified factor's 
  categories don't number more than \code{nmin}.
  If there are no fields with the name of \code{fac}, the data 
  matrix/frame is returned unaltered.
}
\value{
  A subset of \code{dat} after thinning \code{fac} by \code{nmin}.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\seealso{ 
  \code{\link{biteData}}, \code{\link{getData}}, \code{\link{getFile}}
}
\examples{
pbsfun=function(){
  cat("Unique records in 'iris$Petal.Width'\n")
  print(sapply(split(iris$Petal.Width,iris$Petal.Width),length))
  test=chewData(iris,"Petal.Width",5)
  cat("'Petal.Width' categories fewer than 5 removed\n")
  print(sapply(split(test$Petal.Width,test$Petal.Width),length))
  invisible() }
pbsfun()
}
\keyword{data}
\keyword{utilities}

