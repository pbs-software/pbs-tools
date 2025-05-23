\name{createMA}
\alias{createMA}
\title{
  Create DFO Management Actions Table
}
\description{
  Create a table of DFO management actions and a table of historical
  TACs for an offshore rockfish stock assessment Appendix A (catch data)
  tailored to the specified species.
}
\usage{
createMA(yrs=1979:2024, strSpp="POP", addletters=TRUE, gears="TRW")
}
\arguments{
  \item{yrs}{\code{numeric} -- range of years to select management actions applicable to \code{'strSpp'}}
  \item{strSpp}{\code{character} -- species three-letter code used in Groundfish stock assessments}
  \item{addletters}{\code{logical} -- if \code{TRUE}, prefix each line with a letter for notation}
  \item{gears}{\code{character} -- three-letter code depicting gear type:
    \code{'TRW'}= trawl, \code{'ZNO'}= hook & line outside, \code{'ALL'}= all gear types}
}
\details{
  Internally, a list object uses key value pairs \code{'[["year"]][["strSpp"]]'} to
  index management actions relevant to a species. This object will need to be
  amended for new years and species not already in the object.
  
  Note that \code{'[["year"]][["@@@"]]'} designates a management action relevant to
  all species.
  
  If multiple management actions appear in any one year for any one species
  (or group of species), then the key needs to be made unique. For example,
  letters \code{'a, b, c, ...'} could be added: \code{'[["2019"]][["BORa"]]'}
}
\value{
  Returns a data frame of the management actions; also writes comma-delimited files:\cr
  \code{'dfo.mgmt.actions.ABC.csv'} and\cr
  \code{'dfo.mgmt.quotas.ABC.csv'}\cr
  where \code{ABC}= three-letter species code (e.g., \code{POP}).
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-10-28}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{buildCatch}},
  \code{\link[PBStools]{calcStockArea}},
  \code{\link[PBStools]{plotGMA}},
  \code{\link[PBStools]{plotLocal}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{file}
\keyword{documentation}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M06_Assessment}
% Use only one concept per line.
