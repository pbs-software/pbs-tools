\name{calcHBLL}
\alias{calcHBLL}
\title{
  Calculate Swept-Area Biomass Estimates for HBLL
}
\description{
  Calculate swept-area biomass estimates for the HBLL longline surveys,
  and bootstrap within strata by year or year and stratum.
}
\usage{
calcHBLL(dat, ssid, reps=0, seed=42, meth=1, xvar="year")
}
\arguments{
  \item{dat}{\code{data.frame} -- output from function \code{prepGFsurv}}
  \item{ssid}{\code{numeric} -- survey series identification number 
    (e.g., 22=HBLL North outside, 36=HBLL South outside)}
  \item{reps}{\code{numeric} -- number of bootstrap replicates (if any)}
  \item{seed}{\code{numeric} -- random seed, defaults to the meaning of life, the universe, and everything}
  \item{meth}{\code{numeric} -- method of stratification, where\cr
    \code{0} = strata comprise \code{'xvar'} (usually \code{"year"}) only;\cr
    \code{1} = strata comprise \code{xvar="year"} and survey \code{stratumvar="stratum"}}
  \item{xvar}{\code{character} -- field name of index variables (usually \code{"year"})}
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
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2025-03-20}
}
\note{
  \code{calcHBLL} mirrors PJS' code \code{calcBiom}, available in package \pkg{PJSsurvey}.

  \code{getHBLL} not in package due to reliance on Hadley Wickham overlay code.
  Modified from code supplied by \href{mailto:dana.haggarty@dfo-mpo.gc.ca}{Dana Haggarty} (2022-04-11).
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
In package \pkg{PBStools}:\cr
  \code{getHBLL} (see Note),
  \code{\link[PBStools]{plotHBLL}}\cr
In package \pkg{PJSsurvey}:\cr
  \code{\link[PJSsurvey]{calcBiom}},
  \code{\link[PJSsurvey]{plotIndex}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{indices}
\concept{M04_Survey}
% Use only one concept per line.
