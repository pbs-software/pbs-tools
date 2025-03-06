\name{plotIndex}
\alias{plotIndex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  PJS -- Plot Survey Index Series
}
\description{
  Plot a survey index series with bootstrapped confidence intervals.
}
\usage{
plotIndex(bootbomb, analytic, type="PJS", surv=ttcall(surveyname),
   png=FALSE, pngres=400, PIN=c(8,7), lang=c("e","f"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bootbomb}{\code{list} -- output from call to function \code{calcBiom} with \code{reps=N}, where \code{N} > 0.}
  \item{analytic}{\code{list} -- output from call to function \code{calcBiom} with \code{reps=0}.}
  \item{type}{\code{character} -- if \code{type="PJS"}, plot the series as mean with bias-corrected accelerated confidence limits;
    if \code{type="RH"}, plot quantile boxes using 0.05, 0.25, 0.5, 0.75, and 0.95 quantiles.}
  \item{surv}{\code{character} -- survey name.}
  \item{png}{\code{logical} -- if \code{TRUE}, send the figure to a \code{'.png'} file.}
  \item{pngres}{\code{numeric} -- resolution (pixels per inch) of the \code{'.png'} file.}
  \item{PIN}{\code{numeric} -- two-element vector specifying the dimensions
    (width, height) in inches of the \code{'.png'} file.}
  \item{lang}{\code{character} -- a vector of letters that denote the language for output:
    currently only \code{'e'} (english) and \code{'f'} (french).}
}
\details{
  The plot mimics PJS' style of plotting index series in STATA.\cr
  See \code{'Examples'} for sequence of code calls.

  Years with no catch of species show index as an \code{'x'} at y=0.
}
\value{
  List of bootstrapped replicates by year for user's pleasure.
}
\author{
  \href{mailto:paul@starrfish.net}{Paul J. Starr}, Chief Groundfish Scientist\cr
  Canadian Groundfish Research and Conservation Society (CGRCS), Nanaimo BC

  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2023-02-10}
}
\note{
  PJS maintains code in statistical software called \href{https://www.stata.com/}{STATA}.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{calcBiom}},
  \code{\link[PBStools]{doSynoptic}},
  \code{\link[PBStools]{getLabels}},
  \code{\link[PBStools]{keepAtts}},
  \code{\link[PBStools]{prepGFsurv}},
  \code{\link[PBStools]{restratify}},
  \code{\link[PBStools]{uniqtows}}
}
\examples{
\dontrun{
  qu("gfb_survey_data.sql",dbName="GFBioSQL",strSpp="435",survserid=16)
  write.csv(PBSdat,"SSID=16&species=435.csv",row.names=FALSE)
  prepdat = prepGFsurv("SSID=16&species=435.csv", survey=9, strSpp="435")
  bootbomb = calcBiom(prepdat, reps=1000)
  analytic = calcBiom(prepdat, reps=0)
  plotIndex(bootbomb, analytic, lang="e", png=TRUE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{indices}
\concept{M09_PJSsurvey}
% Use only one concept per line.
