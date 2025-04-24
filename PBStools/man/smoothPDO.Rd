\name{smoothPDO}
\alias{smoothPDO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Smooth Environmental Anomalies
}
\description{
  Smooth environmental indices, often expressed as anomalies, for regimes like
  the Pacific Decadal Oscillation (PDO) and North Pacific Gyre Oscillation (NPGO).
}
\usage{
smoothPDO(dat=pdo, zfld="anomaly", period=365, every=365/6, 
   addpos=TRUE, posgroup=4, addgrid=TRUE, gridcell=5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{\code{numeric} -- data frame of monthly environmental index anomalies}
  \item{zfld}{\code{character} -- field name that contains anomalies}
  \item{period}{\code{numeric} -- period in days over which each average anonmaly in the moving series is calculated; 
    \code{period} is passed to function \code{calcMA()}}
  \item{every}{\code{numeric} -- number of days to move the \code{period} window 
    forward for successive calculations of the average anomaly;
    \code{period} is passed to function \code{calcMA()}}
  \item{addpos}{\code{logical} -- if \code{TRUE}, add labels to positive anomalies
    indicating their duration}
  \item{posgroup}{\code{numeric} -- minimum number of period divisions (bars) before
    labels are added}
  \item{addgrid}{\code{logical} -- if \code{TRUE}, add a set of grid lines (vertical only)
    delineating calendar year divisions}
  \item{gridcell}{\code{numeric} -- number of years to use in calendar year divisions}
}
\details{
  Slides a window of time (\code{period}) forward by a specified value (\code{every}).
  For instance, if a time series of 200 days uses a period of 100 days moving forward every 10 days,
  11 mean anomalies are calculated: the first record reports the mean for days 1 to 100;
  the last record reports the mean for days 191 to 200.
}
\value{
  Data frame of \tabular{rl}{
    \code{x} \tab the end x-value of each period of the moving average \cr
    \code{y} \tab mean anomaly in period \cr
    \code{z} \tab the numeric value of x (above) taken from the cumulative sum of the input\cr
             \tab x-units starting at 1 (see \code{PBStools::calcMA}) \cr
  }
}
\references{
Edwards A.M., Tai T.C., Watson J., Pe\ifelse{latex}{\out{\~{n}}}{\ifelse{html}{\out{&ntilde;}}{n}}a M.A., Hilborn A., Hannah C.G., Rooper C.N., Flynn K.L., and Oldford, G.L. (2024)
\href{https://github.com/pbs-assess/pacea, https://zenodo.org/doi/10.5281/zenodo.13840804}{pacea}: 
An R package of Pacific ecosystem information to help facilitate an ecosystem approach to fisheries management.

Lefebvre, N.R., Boers, N., Godbout, L. and Haigh, R. (2017) 
\href{https://doi.org/10.31542/r.gm:1451}{PBSsatellite 1.0: user's guide}.
Technical Report MACEWANU-CMPT-TR-2017-1, 57p, Dept. Comp. Sci., MacEwan University, Edmonton AB.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2025-04-16}\cr
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
In package \pkg{PBSdata}:\cr
	\code{pdo},
	\code{npgo}\cr
In package \pkg{PBStools}:\cr
	\code{\link[PBStools]{calcMA}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{hplot}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{A01_Utility}
% \concept{ ~cpt2 }
% Use only one concept per line.
