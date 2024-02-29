\name{clearFiles}
\alias{clearFiles}
\title{
  Clear Files with Archive Option
}
\description{
  Check to see if file exists and keep a copy if it does before removing it.
}
\usage{
clearFiles(x, archive=TRUE, archdir="./archive", short=TRUE)
}
\arguments{
  \item{x}{\code{character} -- vector of file names to archive (optional) and remove from the directory implied.
    If file name contains no directory information, the current working directory is assumed.}
  \item{archive}{\code{logical} -- if \code{TRUE}, save a copy of the file annotated by its creation date.}
  \item{archdir}{\code{character} -- archive directory; defaults to subdirectory called \code{'archive'} of implied directory.}
  \item{short}{\code{logical} -- if \code{TRUE}, use year-month-date annotation without minutes-seconds.}
}
\details{
  Primarily used to archive output files (chiefly image files) for commonly-run functions during stock assessments.
}
\value{
  Invisibly returns the number of files removed.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2021-06-14}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{collectFigs}},
  \code{\link[PBStools]{countLines}},
  \code{\link[PBStools]{createFdir}},
  \code{\link[PBStools]{readClog}},
  \code{\link[PBStools]{subsetFile}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{IO}
% \keyword{ ~kwd2 }
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
\concept{M01_Utility}
