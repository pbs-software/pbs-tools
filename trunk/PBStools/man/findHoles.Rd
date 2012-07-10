\name{findHoles}
\alias{findHoles}
\title{
  Find Holes in Parent Polygons
}
\description{
  Identifies polygons within polygons of the same PID, and transforms them 
  into holes, if they are not already set that way. Also weeds out small polygons
  before the hole identification routine runs.
}
\usage{
findHoles(polyset, minVerts = 10)
}
\arguments{
  \item{polyset}{a valid PBSmapping PolySet.}
  \item{minVerts}{minimum number of vertices required for a polygon to be retained 
    before the algorithm identifying holes begins.}
}
\details{
  This function can become very slow when there are hundreds of polygons.
  Hence the argument \code{minVerts} can reduce the number of candidate polygons.
  Ideally, \code{findHoles} should be programmed in C.
}
\value{
  A PolySet that identifies parents and holes based on the position 
  of polygons within polygons.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\seealso{
  \code{\link{calcHabitat}} \cr
  \pkg{sp}: \code{\link[sp]{point.in.polygon}}
}
\keyword{manip}

