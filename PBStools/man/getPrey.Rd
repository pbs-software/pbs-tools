\name{getPrey}
\alias{getPrey}
\title{
  Get Prey Items in Stomach
}
\description{
  Get the prey items identified in the stomach contents of 
  specimens that match those in an existing \code{bioDat} object.
}
\usage{
getPrey(bioDat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bioDat}{\code{data.frame} -- object of biological records for a given species;
    object is usually created by an SQL query to \code{GFBioSQL}:\cr
    \code{getData("gfb_bio.sql",dbName="GFBioSQL",strSpp="435")} }
}
\details{
  Function uses SQL queries to the \code{GFBioSQL} database:\cr
  \code{getData("B05e_Specimen_Stomach","GFBioSQL")}\cr
  \code{getData("C_Species","GFBioSQL")}
}
\value{
  Table of prey items in selected species' stomachs, listed in
  descending order of frequency.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2020-01-28}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{calcHabitat}},
  \code{\link[PBStools]{clarify}},
  \code{\link[PBStools]{extractAges}},
  \code{\link[PBStools]{getData}},
  \code{\link[PBStools]{plotEO}},
  \code{\link[PBStools]{quantAges}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{biology}
\concept{prey}
% Use only one concept per line.
