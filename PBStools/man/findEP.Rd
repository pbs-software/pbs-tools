\name{findEP}
\alias{findEP}
\title{
  Find Events in PolySet
}
\description{
  For every event record, locate an enclosing polygon from a PolySet and return an
  identifier key based on a combination of area descriptors -- 
  e.g. (\code{major} \code{minor} \code{locality}) -- available in the PolySet.
}
\usage{
findEP(events, polys, maxRows=1e+08)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{events}{\code{EventData} -- events in a data frame that satisfies the condition of a
    \code{PBSmapping} \code{'EventData'} object.}
  \item{polys}{\code{PolySet} -- polygon shapes in a data frame that satisfies the condition of a
    \code{PBSmapping} \code{'PolySet'} object.}
  \item{maxRows}{estimated maximum number of rows in the \code{PBSmapping} output object \code{'LocationSet'}.}
}
\details{
  The function is designed primarily for finding events in the \code{PBSdata} PolySet
  object \code{'locality.plus'} and adopting the unique identifiers \code{'ID'} for those 
  polygons stored in the PolySet's \code{'PolyData'} summary object (attached as an attribute).

  The function uses the \code{PBSmapping} function \code{'findPolys'}
  and automatically uses \code{'includeBdry=1'}, which is an argument in
  \code{'findPolys'} that can be set to:\cr
  \code{NULL} -- report all points on polygon boundaries (default behaviour); \cr
  \code{0} -- exclude all points on polygon boundaries; \cr
  \code{1} -- report only the first (lowest PID/SID) polygon boundary; \cr
  \code{2,...,n} -- report the last (highest PID/SID) polygon boundary.\cr
  The value 1 is chosen to avoid duplication of finding an event on 
  boundaries shared by two or more polygons.
}
\value{
  The input \code{'events'} object is returned with additional fields:\cr
  \code{ID} -- an ID field based solely on combinations of area fields
  \code{'major'} and/or \code{'minor'} and/or \code{'locality'} in the \code{EventData} object.\cr
  \code{ID2} -- an ID field based on the ID field in the PolySet's
  \code{'PolyData'} attribute for the polygon in which the event is located.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2019-03-22}
}
\note{
  For an event that cannot be located in any of the PolySet's polygons, the 
  field \code{'ID2'} for that record is set to the value in the field \code{'ID'}.
}
\seealso{
In \pkg{PBStools} (relevant):\cr
  \code{\link[PBStools]{plotLocal}}

In \pkg{PBStools} (aside):\cr
  \code{\link[PBStools]{findHoles}},
  \code{\link[PBStools]{findPV}},
  \code{\link[PBStools]{inWord}},
  \code{\link[PBStools]{plotEO}},
  \code{\link[PBStools]{zapDupes}},
  \code{\link[PBStools]{zapHoles}}

In \pkg{PBSmapping}:\cr
  \code{\link[PBSmapping]{EventData}},
  \code{\link[PBSmapping]{PolySet}},
  \code{\link[PBSmapping]{LocationSet}},
  \code{\link[PBSmapping]{findPolys}}

In \pkg{PBSdata}:\cr
  \code{\link[PBSdata]{locality}} and 
  \code{locality.plus} (in same help file).
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
