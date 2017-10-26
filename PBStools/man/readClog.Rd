\name{readClog}
\alias{readClog}
\title{
Read Change Log
}
\description{
Read in a ChangeLog file that has been formatted for use with this function.
}
\usage{
readClog(fnam)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fnam}{character name of a ChangeLog file.}
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
  Rowan Haigh, Research Biologist\cr
  Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
\code{\link[PBStools]{countLines}},
\code{\link[PBStools]{revStr}},
\code{\link[PBStools]{subsetFile}},
\code{\link[PBStools]{toUpper}},
\code{\link[PBStools]{wrapText}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{character}% use one of  RShowDoc("KEYWORDS")
\keyword{list}% __ONLY ONE__ keyword per line
\keyword{file}% __ONLY ONE__ keyword per line

