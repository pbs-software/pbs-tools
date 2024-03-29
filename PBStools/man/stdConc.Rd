\name{stdConc}
\alias{stdConc}
\title{
  Standardise Chemical Concentration
}
\description{
  Standardise a chemical concentration from various aavailable unit 
  combinations to a common concentration and adjust for moisture.
}
\usage{
stdConc(dat, nUout="mg", dUout="kg", fac=1)
}
\arguments{
  \item{dat}{\code{numeric} -- vector with 5 elements or a data frame with 1 row and 5 columns (labelled or not): \cr
    \code{namt....}numerator amount\cr
    \code{nunit...}numerator unit\cr
    \code{damt....}denominator amount\cr
    \code{dunit...}denominator unit\cr
    \code{moist...}moisture content as a proportion}
  \item{nUout}{\code{character} -- numerator unit desired}
  \item{dUout}{\code{character} -- denominator unit desired}
  \item{fac}{\code{numeric} --  multiplicative factor (\emph{e.g.}, \code{75} for 75 mg)}
}
\details{
  Note that the input object \code{dat} mixes data types (\code{numeric} 
  and \code{character}). These types are maintained in a single-row data frame 
  but a vector will change all numerics to character. The function 
  \code{stdConc} automatically forces all inputs back to their intended mode.
}
\value{
  A one-row data frame with fields \code{c(stdAmt, unit)}, which represents the 
  standardised concentration as a numeric value and a string unit fraction.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-29}
}
\note{
  The available units for combinations in the numerator and denominator are: 
  \tabular{llllll}{
    \code{mcg} \tab microgram     \tab \code{mg}  \tab milligram         \tab \code{g}   \tab gram \cr
    \code{kg}  \tab kilogram      \tab \code{L}   \tab litre             \tab \code{pct} \tab percent (\%) \cr
    \code{pdw} \tab \% dry weight \tab \code{ppm} \tab parts per million \tab \code{ppt} \tab parts per thousand
  }
}
\seealso{
  \code{\link[PBStools]{calcMA}}, \code{\link[PBStools]{crossTab}}
}
\keyword{arith}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
\concept{M01_Utility}
