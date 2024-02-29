\name{tabSFW}
\alias{tabSFW}
\title{
  Tables for Seafood Watch
}
\description{
  Make tables of BC commercial fishery catch using target groups and subareas for Seafood Watch.
}
\usage{
tabSFW(dat, gear=1, shallow=300, nspp=10, year, major)
}
\arguments{
  \item{dat}{\code{data.frame} -- data table resulting from the SQL query `fos_gabe_andrews.sql'}
  \item{gear}{\code{numeric} -- gear type code: 1=BT, 2=TR, 3=MW, 4=HL, 5=LL}
  \item{shallow}{\code{numeric} -- depth delimiting shallow species from deep ones}
  \item{nspp}{\code{numeric} -- number of highest-catch species to gather in final summary (set to NULL for all species)}
  \item{year}{\code{numeric} -- vector of years}
  \item{major}{\code{numeric} -- vector of major PMFC codes: 3=3C, 4=3D, 5=5A, 6=5B, 7=5C, 8=5D, 9=5E}
}
\details{
  Process catch data from the DFO database \code{GFFOS} to summarise catch (total, landed, released, all in tonnes).
  The routine filters records based on specified years and gear type before tallying species catch by target group and subarea.
  For each subset (group-subarea), the records are sorted by total catch by species in descending order.
  Subset results with less than three observations by species are removed.
}
\value{
  A named list (`\code{sfwlist}') of data frames by target group and subarea for the specified years and gear type.
  List names comprise target group number and subarea (e.g., `3-5ABC' for Rockfish in PMFC area 5ABC).
  Target groups: 1=Shallow flatfish, 2=Deep flatfish, 3=Rockfish, 4=Pelagic fish, 5=Invertebrates.
  Data frames have six columns named `\code{code}' (GFFOS Hart-like code), `\code{species}' (common name), 
  `\code{neid}' (number of fishing events), `\code{ctot}' (total catch in tonnes), 
  `\code{cland}' (landed catch in tonnes), and `\code{cdisc}' (released catch in tonnes).
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-29}\cr
}
\note{
  Code was created based on requests from Gabe Andrews of Seafood Watch, 886 Cannery Row, Monterey, CA 93940
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{createMA}},
  \code{\link[PBStools]{crossTab}},
  \code{\link[PBStools]{formatCatch}},
  \code{\link[PBStools]{listTables}},
  \code{\link[PBStools]{makeCATtables}},
  \code{\link[PBStools]{makeLTH}},
  \code{\link[PBStools]{makePMRtables}},
  \code{\link[PBStools]{splitTab}},
  \code{\link[PBStools]{texArray}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M03_Fishery}
% \concept{ ~cpt2 }
% Use only one concept per line.
