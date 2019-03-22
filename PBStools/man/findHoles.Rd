\name{findHoles}
\alias{findHoles}
\title{
  Find Holes in Solid Polygons
}
\description{
  Calculate a unique ID for each polygon using PID and SID, and then identify
  solid polygons (without holes) and polygons enclosing other polygons to create
  swiss cheese polygons (polygons with holes).
}
\usage{
findHoles(polyset, minVerts=25, nlevs=1, use.sp.pkg=TRUE)
}
\arguments{
  \item{polyset}{\code{numeric} -- a valid PBSmapping PolySet.}
  \item{minVerts}{\code{numeric} -- minimum number of vertices required for a polygon to be retained 
    before the algorithm identifying holes begins.}
  \item{nlevs}{\code{numeric} -- number of levels (1 or 2) to find solids with holes.
    Usually one level is sufficient while the second level is redundant.}
  \item{use.sp.pkg}{\code{logical} -- if \code{TRUE}, use \code{sp::point.in.polygon},\cr
    otherwise use \code{PBSmapping::.is.in} (~3x slower).}
}
\details{
  This function use an outer product routine and is faster than it was 
  previously; however, it can become slow when there are hundreds of polygons.

  The original PID and SID values are reset but can be inferred from an 
  added field called \code{ID} which comprises the original PID and SID.

  Depending on the value of the argument \code{'use.sp.pkg'}, 
  the user can choose to rely on a third-party package \code{'sp'} (Spatial Data)
  or use an in-house \code{PBSmapping} dot function created to find points in polygons.
  The Spatial Data function is approximately three times faster than the \code{PBSmapping}
  function, but it requires that the user has the package \code{'sp'} installed.
}
\value{
  A PolySet that identifies solids and holes based on the position 
  of polygons within polygons.
}
\note{
  The function saves some of the internal objects created to a list object called
  \code{'obj.findHoles'} in the PBStools' environment \code{'.PBStoolEnv'}.
  Users can access this list object using the accessor functions \code{'ttget'}, 
  \code{'ttcall'}, etc.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2019-03-22}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{calcHabitat}},
  \code{\link[PBStools]{zapHoles}}\cr
In package \pkg{PBSmapping}:\cr
  \code{\link[PBSmapping]{joinPolys}},
  \code{\link[PBSmapping]{placeHoles}}\cr
In package \pkg{sp}:\cr
  \code{\link[sp]{point.in.polygon}}
}
\keyword{manip}


