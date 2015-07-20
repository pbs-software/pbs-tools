\name{findHoles}
\alias{findHoles}
\title{
  Find Holes in Parent Polygons
}
\description{
  Calculate a unique ID for each polygon using PID and SID, and then identify
  solid polygons (without holes) and polygons enclosing other polygons to create
  swiss cheese polygons (polygons with holes).
}
\usage{
findHoles(polyset, minVerts=25)
}
\arguments{
  \item{polyset}{a valid PBSmapping PolySet.}
  \item{minVerts}{minimum number of vertices required for a polygon to be retained 
    before the algorithm identifying holes begins.}
}
\details{
  This function use an outer product routine and is faster than it was 
  previously; however, it can become slow when there are hundreds of polygons.
  
  The original PID and SID values are reset but can be inferred from an 
  added field called \code{ID} which comprises the original PID and SID.
}
\value{
  A PolySet that identifies solids and holes based on the position 
  of polygons within polygons.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link{calcHabitat}} \cr
  \pkg{sp}: \code{\link[sp]{point.in.polygon}}
}
\keyword{manip}

