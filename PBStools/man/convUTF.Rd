\name{convUTF}
\alias{convUTF}
\title{
  Convert UTF-8 Code to Unicode
}
\description{
  Convert UTF-8 code in text to Unicode symbols (e.g., \code{"\\\\u{u00E9}"} rendered to e acute symbol).
}
\usage{
convUTF(x, guide=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\code{character} -- text containing UTF-8 codes (e.g., \code{"\\\\u{u00E9}"}).}
  \item{guide}{\code{logical} -- if \code{TRUE}, print a guide of UTF-8 to Unicode conversions on the command line.}
}
\details{
  Convert UTF-8 codes in a vector of character strings to Unicode symbols
  (e.g., \code{"\\u{u00E9}"} rendered to e acute symbol).
  If text elements contain no UTF-8 codes, text is returned as is.
  Primarily used to convert labels in the data file \code{'bcnames'} (in package \pkg{PBSvault}).
}
\value{
  Text with Unicode symbols (conversion guide attached as an attribute).
}
\note{
  The conversion guide of UTF-8 codes to Unicode is not complete.
}
\references{
  Solution proposed by \href{https://stackoverflow.com/questions/72591452/replace-double-by-single-backslash}{G. Grothendieck}.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Regional Headquarters (RHQ), Vancouver BC\cr
  Last modified \code{Rd: 2023-06-14}
}
\seealso{
In \pkg{PBStools}:
  \code{\link[PBStools]{createMA}},
  \code{\link[PBStools]{inWord}},
  \code{\link[PBStools]{linguaFranca}},
  \code{\link[PBStools]{readClog}},
  \code{\link[PBStools]{revStr}},
  \code{\link[PBStools]{showError}},
  \code{\link[PBStools]{texArray}},
  \code{\link[PBStools]{wrapText}}\cr
In \pkg{PBSvault}:
  \code{bcnames}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M01_Utility}
% \concept{ ~cpt2 }
% Use only one concept per line.
