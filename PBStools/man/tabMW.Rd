\name{tabMW}
\alias{tabMW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Tabulate Mean Weights
}
\description{
  Tabulate mean weights (or some other morphometric).
}
\usage{
tabMW(dat, flds=c("year", "SSID"), zfld="wt", ttype, q=c(0.05,0.25,0.5,0.75,0.95),
   spp.name, debug=TRUE, tabs=TRUE, plot=TRUE, outnam="Species-Weights", 
   png=FALSE, pngres=400, PIN=c(10, 10), lang=c("f","e"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{\code{data.frame} -- biological data object derived from
    SQL query \code{'gfb_bio.sql'}}
  \item{flds}{\code{character} -- field names used in a cross tabulation}
  \item{zfld}{\code{character} -- field name of morphometric measure}
  \item{ttype}{\code{numeric} -- vector of trip type codes}
  \item{q}{\code{numeric} -- vector of quantile probabilities}
  \item{spp.name}{\code{character} -- species name for output labelling}
  \item{debug}{\code{logical} -- if \code{TRUE}, use the \code{crossTab} results saved in memory}
  \item{tabs}{\code{logical} -- if \code{TRUE}, save crosstab tables to \code{csv} files}
  \item{plot}{\code{logical} -- if \code{TRUE}, plot the results on a graphics device}
  \item{outnam}{\code{character} -- output name for figures and tables}
  \item{png}{\code{logical} -- if \code{TRUE}, send the figure to a \code{png} device}
  \item{pngres}{\code{numeric} -- figure resolution in pixels/inch}
  \item{PIN}{\code{numeric} -- plot size (width, height) in inches}
  \item{lang}{\code{character} -- language for figure labelling; `e'=english, `f'=french}
}
\details{
  Summarises morphometric measurement by factors (\code{flds}) using quantile plots and tables.
  Summary measurements comprise quantiles (including median) and mean values.
}
\value{
  Nothing in particular.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-12-18}
}
\note{
  Originally created for GMU to summarise Spiny Dogfish weights.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{crossTab}},
  \code{\link[PBStools]{linguaFranca}},
  \code{\link[PBStools]{quantBox}}\cr
In package \pkg{PBSdata}:\cr
  \code{\link[PBSdata]{gfb.codes}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{hplot}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M02_Biology}
% \concept{ ~cpt2 }
% Use only one concept per line.
