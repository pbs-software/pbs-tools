\name{readClog}
\alias{readClog}
\title{
Read Change Log
}
\description{
  Read a ChangeLog file that has been formatted for use with this function.
}
\usage{
readClog(fnam)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fnam}{\code{character} -- file name of a ChangeLog archive.}
}
\details{
  This function will only work if it has been formatted to look like that in PBStools.\cr
  Specifically, lines of the file have the following meaning:
  \itemize{
    \item versions should appear with no leading space;
    \item code modules or categories should appear with 2 leading spaces and an asterisk (*);
    \item changes or actions should appear with 4 leading spaces and a plus sign (+);
    \item subactions should appear with 6 leading spaces and a minus sign (-);
    \item leading cross-hatches (#) will be treated as comments and discarded;
    \item header line 1 is treated as the ChangeLog title; and 
    \item header lines between \code{=====} and \code{-----} contain author names.
  }
}
\value{
  An named list object by version with attributes \code{title} and \code{authors}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Regional Headquarters (RHQ), Vancouver BC\cr
  Last modified \code{Rd: 2023-02-09}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{countLines}},
  \code{\link[PBStools]{revStr}},
  \code{\link[PBStools]{subsetFile}},
  \code{\link[PBStools]{toUpper}},
  \code{\link[PBStools]{wrapText}}
}
\examples{
\dontrun{
local(envir=.PBStoolEnv,expr={
pbsfun=function(){
  clog = readClog(fnam=system.file("doc/ChangeLog.txt",package="PBStools"))
  print(clog[[1]])
  invisible() }
pbsfun()
})
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{character}% use one of  RShowDoc("KEYWORDS")
\keyword{list}% __ONLY ONE__ keyword per line
\keyword{file}% __ONLY ONE__ keyword per line
\concept{M01_Utility}

