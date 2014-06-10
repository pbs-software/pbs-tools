\name{weightBio}
\alias{weightBio}
\title{
  Weight Age Frequencies/Proportions by Catch/Density
}
\description{
  Weight age or length frequencies\eqn{\Vert}{|}proportions by: \cr
  \emph{commercial}: sample trip catch within quarters and quarterly catch within years, or \cr
  \emph{survey}: sample species density (kg/km\eqn{^2}) in stratum and stratum area (km\eqn{^2}) in survey.
}
\usage{
weightBio(adat, cdat, sunit="TID", sweight="catch", 
     ttype=NULL, stype=c(0,1,2,5:8), ameth=3, sex=2:1, major=NULL, 
     wSP=c(TRUE,TRUE), wN=TRUE, plus=60, Nmin=0, Amin=NULL, 
     ctype="C", per=90, SSID=32, tabs=TRUE, 
     plot=TRUE, ptype="bubb", size=0.15, powr=0.5, zfld="wp", 
     clrs=list(c("blue","cyan"),c("green4","chartreuse")),
     cohorts=NULL, regimes=list(1900:1908,1912:1915,1923:1929,1934:1943,
     1957:1960,1976:1988,1992:1998,2002:2006),  #PDO
     layout="portrait", eps=FALSE, pdf=FALSE, pix=FALSE, wmf=FALSE, 
     longside=10, ioenv=.GlobalEnv, ...)
}
\arguments{
  \item{adat}{data object of ageing information with fields that must include: \cr
    \code{TID}   = trip ID, \cr
    \code{FEID}  = fishing event ID, \cr
    \code{date}  = trip or fishing event date, \cr
    \code{age}   = age of fish, \cr
    \code{ameth} = ageing method code, \cr
    \code{sex}   = sex code (1 = males, 2 = females), \cr
    \code{major} = PMFC major area code, \cr
    \code{ttype} = trip type code, \cr
    \code{catch} = trip catch or fishing event catch. }
  \item{cdat}{data object of catch information with fields that must include: \cr
    \code{TID}   = trip ID, \cr
    \code{date}  = trip or fishing event date, \cr
    \code{year}  = trip or fishing event year, \cr
    \code{major} = PMFC major area code, \cr
    \code{catch} = trip catch or fishing event catch. }
  \item{sunit}{sample unit (e.g., \code{TID} for sampling at the trip level).}
  \item{sweight}{sample weighting field (e.g., \code{catch} or \code{density}).}
  \item{ttype}{trip type where: \cr
    \code{1}  = non-observed domestic, \cr
    \code{2}  = research, \cr
    \code{3}  = charter, \cr
    \code{4}  = observed domestic, \cr
    \code{5}  = observed j-v, \cr
    \code{6}  = non-observed j-v, \cr
    \code{7}  = Polish commercial national, \cr
    \code{8}  = Russian commercial national, \cr
    \code{9}  = Russian commercial supplemental, \cr
    \code{10}  = Polish commercial supplemental, \cr
    \code{11}  = recreational, \cr
    \code{12}  = Japanese observed national, \cr
    \code{13}  = Japanese commercial, \cr
    \code{14}  = unknown foreign.}
  \item{stype}{sample type where: \cr
    \code{0}  = unknown, \cr
    \code{1}  = total catch, \cr
    \code{2}  = random, \cr
    \code{4}  = selected, \cr
    \code{5}  = stratified, \cr
    \code{6}  = random from randomly assigned set, \cr
    \code{7}  = random from set after randomly assigned set, \cr
    \code{8}  = random from set requested by vessel master, \cr
    \code{9}  = selected juveniles, \cr
    \code{10} = selected adults.}
  \item{ameth}{ageing method code (\code{ameth=3} denotes break and burn).}
  \item{sex}{sex codes (1 = males, 2 = females).}
  \item{major}{major PMFC area codes (1=4A, 3=3C, 4=3D, 5=5A, 6=5B, 7=5C, 8=5D, 9=5E).}
  \item{wSP}{logical vector of length two for weighting by (i) sample catch and (ii)
    period (e.g., quarterly) commercial catch.}
  \item{wN}{logical scalar: if \code{TRUE}, weight the age frequencies else 
    weight the age proportions. }
  \item{plus}{age of the plus group (accumulate ages >= \code{plus}).}
  \item{Nmin}{minimum number of specimens by year and sex in order to display bubbles.}
  \item{Amin}{minimum age to display on the y-axis if \code{plot=TRUE}; 
    the default \code{NULL} means the function automatically finds the youngest age observed.}
  \item{ctype}{catch type where \code{"C"} = commercial and \code{"S"} = survey.}
  \item{per}{period length (days) within a year (e.g., 90 for quarterly periods).}
  \item{SSID}{survey series ID (can be a restratification series).}
  \item{tabs}{logical: if \code{TRUE}, create formatted tables for use in other applications.}
  \item{plot}{logical: if \code{TRUE}, plot the results as bubble plots.}
  \item{ptype}{plot type where \code{"bubb"} = bubbles and \code{"bars"} = horizontal bars.}
  \item{size}{specify size (inches) of largest bubble in all panels.}
  \item{powr}{power to scale bubbles, where \code{powr=0.5} makes bubble area roughly proportional to the z-value.}
  \item{zfld}{field name containing values for output (e.g., \code{"wp"} for weighted proportions).}
  \item{clrs}{specify bubble colours indexed by sex code (\code{0} = black).}
  \item{cohorts}{diagonal lines to plot indicating year-class cohorts (list of x- and y-values).}
  \item{regimes}{diagonal periods to plot indicating regime periods (e.g., positive PDO periods).}
  \item{layout}{string describing figure layout; one of: \cr
    \code{"portrait"} for vertically stacked plots, \cr
    \code{"landscape"} for plot placed horizontally side-by-side, \cr
    \code{"juxtapose"} for plots overlaid on each other (not implemented at present).}
  \item{eps}{logical: if \code{TRUE}, send figure to a postscript \code{.eps} file.}
  \item{pdf}{logical: if \code{TRUE}, send figure to a postscript \code{.pdf} file.}
  \item{pix}{logical: if \code{TRUE}, send figure to a raster \code{.png} file.}
  \item{wmf}{logical: if \code{TRUE}, send figure to a vector \code{.wmf} file.}
  \item{longside}{dimension (inches) of the longest side of the figure.}
  \item{ioenv}{input/output environment for function input data and output results.}
  \item{...}{additional arguments to pass to the function \code{plotBubbles}.}
}

\details{
  The function uses values in the field specified by \code{sweight} to weight the 
  proportions-at-age (or frequencies-at-age). For example, if 
  \code{sweight="catch"} then the first weighting for commercial samples might 
  be by trip catch with a quarter year to yield a weighted proportion-at-age
  vector within each quarter. Similarly, the first weighting for a survey might
  be by sample catch within a stratum to yield a weighted proportion-at-age
  vector within each stratum. The second weighting would further weight these 
  period/stratum vectors by total catch of the species in each period/stratum 
  within a year/survey.
  
  The function is not designed to scale up observations based on areal 
  densities, though it can use this information to weight the proportions.
  For surveys in particular, if \code{sweight="density"}, then the first 
  weighting is by density of the species in each sample to derive a vector of
  weighted proportions for each stratum, and the second weighting is simply
  by the stratum area (km\eqn{^2}).
  
  Weighting by density is treated differently here to 'emulate' a survey 
  sample design. The code's algorithm usually takes data for the first weighting
  from the age object and data for the second weighting from the catch object.
  In the special case outlined in the previous paragraph, data for both 
  weightings come from the age object.
}
\value{
  An array with the four dimensions: \code{age}, \code{year}, 
  \code{sex}, and \code{np} = numbers/proportions. \cr
  The age array has two attributes attached: \cr
  \code{wpatab} = two-dimensional matrix of weighted proportions-at-age flattened 
    by sex for input to the Awatea/Coleraine population model. \cr
  \code{sumtab} = supplementary array with dimensions \code{year}, \code{per}
    = period (quarters), and \code{stat} = statistic, where statistic is one of the following: \cr
    \code{Nsid}  = no. samples, \cr
    \code{Ntid}  = no. trips (which roughly equates to number of samples in the non-observed fishery), \cr
    \code{Scat}  = sample unit catch (in our case the sample unit is the trip), \cr
    \code{Fcat}  = commercial fishery catch, \cr
    \code{Psamp} = trip catch/fishery catch.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC \cr
  Paul J. Star, Canadian Groundfish Research and Conservation Society, New Westminster BC
}
\seealso{
  \code{\link[PBStools]{plotProp}}, \code{\link[PBStools]{reportCatchAge}}, \code{\link[PBStools]{requestAges}} \cr
  Available SQL queries: \code{\link[PBStools]{SQLcode}}
}
\keyword{datagen}

