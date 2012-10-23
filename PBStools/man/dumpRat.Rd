\name{dumpRat}
\alias{dumpRat}
\title{
  Dump Ratios from Catch Reconstruction into Tables
}
\description{
  Dump catch ratios calculated during a catch reconstruction into CSV tables.
}
\usage{
dumpRat(strSpp="396", rats=c("alpha","beta","gamma","delta","lambda"))
}
\arguments{
  \item{strSpp}{character string specifying species Hart code (e.g., \code{"396"} = Pacific Ocean Perch). }
  \item{rats}{character vector of named ratios used in a catch reconstruction. }
}
\details{
  See the catch reconstruction algorithm (\code{\link{buildCatch}}) for more details. \cr
  Note: POP = Pacific Ocean Perch, ORF = all rockfish other than POP, TRF = total rockfish,
  TAR = target species/group, RRF = reconstructed rockfish species. \cr
  Briefly: \cr
  \eqn{\alpha} = proportion RRF caught in PMFC areas by all fisheries; \cr
  \eqn{\beta} = proportion RRF caught in PMFC areas by H&L fisheries; \cr
  \eqn{\gamma} = ratio of RRF to a prominent group (POP, ORF, TRF, or TAR); \cr
  \eqn{\delta} = discard rate of RRF from observer logs; \cr
  \eqn{\lambda} = proportion of early ORF/TRF catch by general gear type.
}
\value{
  The ratio arrays are stored in a list object \code{PBSfish123} spat out by the 
  reconstruction procedure. The arrays are extracted and sent to a comma-delimited text file (\code{.csv}).
}
\references{
  Haigh, R. and Yamanaka, K.L. (2011) Catch history reconstruction for rockfish
  (\emph{Sebastes} spp.) caught in British Columbia coastal waters. 
  \emph{Canadian Technical Report of Fisheries and Aquatic Sciences} \bold{0000}: xx + 00 p.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC.
}
\seealso{
  \code{\link{buildCatch}}, \code{\link{dumpMod}}, \code{\link{plotRecon}}
}
\keyword{list}
