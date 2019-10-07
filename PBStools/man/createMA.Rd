\name{createMA}
\alias{createMA}
\title{
  Create DFO Management Actions Table
}
\description{
  Create a table of DFO management actions for an Offshore stock
  assessment Appendix A (catch data) tailored to the specified species.
}
\usage{
createMA(yrs=1979:2019, strSpp="POP", addletters=TRUE)
}
\arguments{
  \item{yrs}{\code{numeric} -- range of years to select management actions applicable to \code{'strSpp'}.}
  \item{strSpp}{\code{character} -- species three-letter code used in Groundfish stock assessments.}
  \item{addletters}{\code{logical} -- if \code{TRUE}, prefix each line with a letter  for notation.}
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
  Returns a data frame of the management actions; also writes table to a \code{'.csv'} file.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2019-09-30}
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
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{"M06_Assessment"}
\concept{appendix}
\concept{management}
% Use only one concept per line.
