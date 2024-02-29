\name{dumpRat}
\alias{dumpRat}
\title{
  Dump Ratios from Catch Reconstruction into Tables
}
\description{
  Dump catch ratios calculated during a catch reconstruction into CSV tables.
}
\usage{
dumpRat(strSpp="396", rats=c("alpha","beta","gamma","delta","lambda"),
   ioenv=.GlobalEnv)
}
\arguments{
  \item{strSpp}{character string specifying species Hart code (e.g., \code{"396"} = Pacific Ocean Perch). }
  \item{rats}{character vector of named ratios used in a catch reconstruction. }
  \item{ioenv}{input/output environment for function input data and output results.}
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
  The ratio arrays are stored in a list object \code{PBStool123} spat out by the 
  reconstruction procedure. The arrays are extracted and sent to a comma-delimited text file (\code{.csv}).
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
  \code{\link{buildCatch}}, \code{\link{dumpMod}}, \code{\link{plotRecon}}
}
\keyword{list}
\concept{M03_Fishery}
