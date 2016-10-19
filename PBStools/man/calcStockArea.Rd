\name{calcStockArea}
\alias{calcStockArea}
\alias{calcWAParea}
\title{
  Calculate Stock Area
}
\description{
  Assign a stock area designation based on species HART code and PMFC
  major and/or minor areas.
}
\usage{
calcStockArea(strSpp, dat, stockFld="stock")

calcWAParea(major, minor, strat, wts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{strSpp}{Species string code (alpha-numeric), usually taken as a page number from Hart (1973).}
  \item{dat}{Data file with location fields (\code{major}, \code{minor}, \code{Y}, etc.).}
  \item{stockFld}{Name of new field to add to the input data file \code{dat}.}
  \item{major}{Vector of numeric codes for major PMFC statistical areas.}
  \item{minor}{Vector of numeric codes for minor PMFC statistical areas
    (sometimes referred to as DFO salmon areas).}
  \item{strat}{Vector of values (e.g., \code{year}) used to stratify the subarea delineation.
    Vector length must equal that for \code{major} and \code{minor}.}
  \item{wts}{Vector of values  (e.g., \code{catch}) used to weight the 
    occurrence of subareas in a major area to determine allocation of unknown
    subareas to known ones. Vector length must equal that for \code{major} and \code{minor}.}
}
\details{
  \code{calcStockArea}:\cr
  Assigns a stock designator for a given species based on major and/or minor areas.
  If both \code{major} and \code{minor} are specified as inputs, the two 
  vectors must have the same length. Additional stocks will be added over time as needed.

  \code{calcWAParea}:\cr
  Assigns a stock designation for Walleye Pollock based on major and minor 
  areas. There are a fair number of records in the Strait of Georgia (\code{major=1})
  labelled 0 or 99, which means they could have occurred anywhere in the Inside
  waters of BC (between Vancover Island and the mainland). This function attempts
  to assign a proportion of unknown tow locations to Minor areas 12 and 20 based on
  known tow locations in PMFC 4B that occur in these areas. Tows in Minor area 12 
  are assigned to the 5AB stock while those in Area 20 are assigned to the 3CD stock.
}
\value{
  \code{calcStockArea} -- a new field of stock identifiers is added to
  the input data file.\cr
  \code{calcWAParea} -- a character vector of stock identifiers with
  the same number of records as that of the input vector(s).
}
\references{
  Hart, J.L. (1973) Pacific fishes of Canada. \emph{Fisheries Research Board of Canada} \bold{180}: ix + 740 p.
}
\author{
   Rowan Haigh, Research Biologist,\cr
   Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\note{
  There is no ideal way of parsing out the percentage of tows from the unknown subarea 
  pool to the known one. Tows are chosen in the order that they appear in the recordset
  (i.e., non-random).
}
\seealso{
  \code{\link[PBStools]{calcSRFA}},
  \code{\link[PBStools]{plotGMA}},
  \code{\link[PBSdata]{species}},
  \code{\link[PBSdata]{major}},
  \code{\link[PBSdata]{minor}}
}
\examples{
\dontrun{
getFile(gfmdat)
gfmdat$catch = gfmdat$landed + gfmdat$discard
gfmdat.new   = calcStockArea(strSpp="228", gfmdat, stockFld="stock")
gfmdat$stock = calcWAParea(gfmdat$major, gfmdat$minor, 
   strat=as.numeric(substring(gfmdat$date,1,4)), wts=gfmdat$catch)
}
}
\keyword{manip}

