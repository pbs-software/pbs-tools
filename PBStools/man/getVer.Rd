\name{getVer}
\alias{getVer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Get Version
}
\description{
  Get version of loaded or installed R packages in user's R environment.
}
\usage{
getVer(print=TRUE, all=FALSE, pat="^.+$", expat="^zzz")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{print}{\code{logical} -- if \code{TRUE}, print packages and versions}
  \item{all}{\code{logical} -- if \code{TRUE}, report all packages installed in user's R environment}
  \item{pat}{\code{character} -- pattern to filter package names}
  \item{expat}{\code{character} -- exclude pattern when filtering package names}
}
\details{
  Loops through loaded (\code{all=FALSE}) or installed (\code{all=TRUE}) packages
  and retrieves their version numbers using \code{utils::packageVersion()}.
}
\value{
  Data frame with two colums named \code{'package'} and \code{'version'}.
}
\references{
\href{https://stackoverflow.com/questions/42901293/r-packages-installed-version-numbers}{%
Roasty247's solution posted 2023-05-17}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-12-18}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{.flush.cat}},
  \code{\link[PBStools]{coalesce}},
  \code{\link[PBStools]{createDSN}},
  \code{\link[PBStools]{gatherVals}},
  \code{\link[PBStools]{getFile}},
  \code{\link[PBStools]{readClog}}\cr
In package \pkg{base}:\cr
  \code{\link[base]{.packages}},
  \code{\link[base]{data.frame}},
  \code{\link[base]{grep}},
  \code{\link[base]{rbind}}\cr
In package \pkg{utils}:\cr
  \code{\link[utils]{packageVersion}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{environment}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M01_Utility}
% \concept{ ~cpt2 }
% Use only one concept per line.
