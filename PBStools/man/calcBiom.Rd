\name{calcBiom}
\alias{calcBiom}
\alias{calcHBLL}
\title{
  PJS -- Calculate Swept-Area Biomass Estimates
}
\description{
  Calculate swept-area biomass estimates and bootstrap within strata by year or year and stratum.
}
\usage{
calcBiom(dat, reps=0, seed=42, meth=1, fix125flag, 
   xvar="year", areavar="area", stratumvar="stratum", towvar="set",
   postfilename)

calcHBLL(dat, ssid, reps=0, seed=42, meth=1, xvar="year")
}
\arguments{
  \item{dat}{\code{data.frame} -- output from function \code{prepGFsurv}}
  \item{reps}{\code{numeric} -- number of bootstrap replicates (if any)}
  \item{seed}{\code{numeric} -- random seed, defaults to the meaning of life, the universe, and everything}
  \item{meth}{\code{numeric} -- method of stratification, where\cr
    \code{0} = strata comprise \code{'xvar'} (usually \code{"year"}) only;\cr
    \code{1} = strata comprise \code{xvar="year"} and survey \code{stratumvar="stratum"}}
  \item{fix125flag}{\code{logical} -- not implemented at present}
  \item{xvar}{\code{character} -- field name of index variables (usually \code{"year"})}
  \item{areavar}{\code{character} -- field name of area variables (usually \code{"area"})}
  \item{stratumvar}{\code{character} --  field name of stratum variables (usually \code{"stratum"})}
  \item{towvar}{\code{character} --  field name of tow variables (usually \code{"set"})}
  \item{postfilename}{\code{character} -- currently does not get used}
  \item{ssid}{\code{numeric} -- survey series identification number 
    (e.g., 22=HBLL North outside, 36=HBLL South outside)}
}
\value{
  A list object containing:
  \itemize{
    \item \code{booty} -- bootstrapped results from a call to \code{boot::boot};
    \item \code{bootci} -- bias-corrected accelerated confidence intervals from \code{boot::boot.ci};
    \item \code{extra} -- list obect:
    \itemize{
      \item \code{ntows} -- number of tows per year (or \code{xvar});
      \item \code{ptows} -- proportion of tows with biomass density greater than zero.
    }
  }
}
\author{
  \href{mailto:paul@starrfish.net}{Paul J. Starr}, Chief Groundfish Scientist\cr
  Canadian Groundfish Research and Conservation Society (CGRCS), Nanaimo BC

  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-10-25}
}
\note{
  PJS maintains code in statistical software called \href{https://www.stata.com/}{STATA}.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{doSynoptic}},
  \code{\link[PBStools]{getLabels}},
  \code{\link[PBStools]{keepAtts}},
  \code{\link[PBStools]{plotIndex}},
  \code{\link[PBStools]{prepGFsurv}},
  \code{\link[PBStools]{restratify}},
  \code{\link[PBStools]{uniqtows}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{indices}
\concept{PJS}
% Use only one concept per line.
