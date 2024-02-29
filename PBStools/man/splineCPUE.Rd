\name{splineCPUE}
\alias{splineCPUE}
\title{
Spline Curve Through CPUE Series
}
\description{
Fit a series of spline curves through a CPUE index series using various degrees of freedom.
}
\usage{
splineCPUE(dat, ndf=50, strSpp="ZZZ", ufld="cpue",
   png=FALSE, pngres=400, PIN=c(8,8), lang=c("e","f"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{\code{data.frame} -- simple two-column data object of \code{'year'} and \code{'cpue'}, usually read in from a \code{'.csv'} file.}
  \item{ndf}{\code{numeric} -- number of degrees of freedom to apply iteratively (internally) to the function \code{'smooth.spline'}.}
  \item{strSpp}{\code{character} -- species code, usually Hart code (page number as a string).}
  \item{ufld}{\code{character} -- name of CPUE field in data.frame \code{'dat'}.}
  \item{png}{\code{logical} -- if \code{TRUE}, send the figure to a \code{'.png'} file.}
  \item{pngres}{\code{numeric} -- resolution of output figure (pixels per inch).}
  \item{PIN}{\code{numeric} -- width and height of output figure (inches).}
  \item{lang}{\code{character} -- a letter that denotes the language for output: currently only \code{'e'} (english) or \code{'f'} (french).}
  \item{\dots}{\code{dots} -- additional arguments that may or may not be recognised by the code.}
}
\details{
A procedure for estimating process error \eqn{c_\mathrm{p}}{c[p]} to 
add to the commercial CPUE using a spline-smoother analysis.
Francis (2011), citing Clark and Hare (2006), recommends using a smoothing
function to determine the appropriate level of process error to add to CPUE
data, with the goal of finding a balance between rigorously fitting the
indices while not removing the majority of the signal in the data.
An arbitrary sequence of length 50, comprising degrees of freedom 
(DF, \eqn{\nu_i}{nu[i]}), where \eqn{i = 2, ..., N} and \eqn{N} = number of 
CPUE values \eqn{U_t}{U[t]} from \eqn{t = 1996, ...., 2020} (for example),
is used to fit the CPUE data with a spline smoother.
At \eqn{i = N}, the spline curve fits the data perfectly and the residual
sum of squares (RSS, \eqn{\rho_N}{rho[N]}) is equal to 0.
Using spline fits across a range of trial DFs \eqn{\nu_i}{nu[i]}, values of RSS
\eqn{\rho_i}{rho[i]} form a logistic-type curve with an inflection point at \eqn{i=k}.
The difference between point estimates of \eqn{\rho_i}{rho[i]} (proxy for the slope
\eqn{\delta_i}{delta[i]}) yields a concave curve with a minimum \eqn{\delta_i}{delta[i]},
which occurs close to the inflection point \eqn{k}.
At the inflection point \eqn{k}, \eqn{\nu_k}{nu[k]} corresponds to \eqn{\rho_k}{rho[k]},
which is converted to \eqn{c_\mathrm{p}}{c[p]} using:

\deqn{c_\mathrm{p} = \sqrt{\frac{\rho_k}{N-2}}~~~{\left[ \frac{1}{N} \sum\limits_{t=1996}^{2020} U_t \right]}^{-1}.}{%
     c[p] = sqrt(rho[k]/(N-2)) / ( (1/N) sum_1996^2020 U[t] )} 
}
\value{
List of \code{'DF'} = optimal degrees of freedom, \code{'RSS'} = minimum residual sum of squares, \code{'CVpro'} = optimal process error.
}
\references{
Clark, W.G. and Hare, S.R. 2006.
\href{https://iphc.int/uploads/pdf/sr/IPHC-2006-SR083.pdf}{%
Assessment and management of Pacific halibut: data, methods, and policy}.
Sci. Rep. 83, International Pacific Halibut Commission, Seattle, WA.

Francis, R.I.C.C. 2011.
\href{https://cdnsciencepub.com/doi/10.1139/f2011-025}{Data weighting in statistical fisheries stock assessment models}.
Can. J. Fish. Aquat. Sci. 68(6). 1124-1138.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-28}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{findPV}},
  \code{\link[PBStools]{plotIndex}},
  \code{\link[PBStools]{trend}},
  \code{\link[PBStools]{weightBio}}.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{hplot}
\keyword{optimize}
\keyword{smooth}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M06_Assessment}
% Use only one concept per line.
