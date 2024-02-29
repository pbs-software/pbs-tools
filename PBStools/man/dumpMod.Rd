\name{dumpMod}
\alias{dumpMod}
\title{
  Dump Modern Catch Used by Reconstruction into Tables
}
\description{
  Dump catch from modern databases that has been collected and organised during 
  a catch reconstruction (see \code{\link{buildCatch}}) into CSV tables.
}
\usage{
dumpMod(dat, catch=c("landed","discard"), fid=1:5,
    strSpp="396", dbs=TRUE)
}
\arguments{
  \item{dat}{numeric array of catch (e.g., \code{catmod1} with dimensions 
    named \code{c("year", "major", "fid", "catch", "dbs")}). }
  \item{catch}{character vector of elements in the dimension labelled \code{"catch"}. }
  \item{fid}{numeric vector of fishery ID codes (1=Trawl, 2=Halibut, 3=Sablefish, 4=Dogfish-Lingcod, 5=H&L Rockfish). }
  \item{strSpp}{character string specifying species Hart code (e.g., \code{"396"} = Pacific Ocean Perch). }
  \item{dbs}{logical: if \code{TRUE}, the input array \code{dat} has the dimension labelled \code{"dbs"},
    and the catch dump will report values from all available database sources. }
}
\details{
  See the catch reconstruction algorithm (\code{\link{buildCatch}}) for more details.
}
\value{
  The data in the array \code{dat} are flattened from four or five dimensions into
  two dimensions and sent to a comma-delimited text file (\code{.csv}).
}
\references{
  Haigh, R. and Yamanaka, K.L. (2011) Catch history reconstruction for rockfish
  (\emph{Sebastes} spp.) caught in British Columbia coastal waters. 
  \emph{Canadian Technical Report of Fisheries and Aquatic Sciences} \bold{0000}: xx + 00 p.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-27}
}
\seealso{
  \code{\link{buildCatch}}, \code{\link{dumpRat}}, \code{\link{plotRecon}}
}
\keyword{array}
\concept{M03_Fishery}
