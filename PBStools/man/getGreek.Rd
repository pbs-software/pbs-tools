\name{getGreek}
\alias{getGreek}
\title{
  Get Greek Letters
}
\description{
  Get Greek letters using unicode characters.
}
\usage{
getGreek(letter, lower=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{letter}{\code{character} -- vector of Greek letter names (e.g., "alpha");
    if not specified, entire Greek alphabet is returned}
  \item{lower}{\code{logical} -- if \code{TRUE}, return lower-case letters;
    if \code{FALSE}, return upper-case letters}
}
\details{
  Greek letters are generated within the function using unicodes passed to
  function `\code{convUTF}'.
}
\value{
  Named vector of Greek letters.
}
\references{
  \href{http://kestrel.nmt.edu/~raymond/software/howtos/greekscape.xhtml}{%
  Greek Unicode and Inkscape}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2025-03-10}
}
\note{
  To see Greek letters exported from R to a CSV file, import the CSV file into Excel using
  <Data><From Text> and choose encoding `650001 : Unicode (UTF-8)'.
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{convUTF}},
  \code{\link[PBStools]{createMA}},
  \code{\link[PBStools]{getVer}},
  \code{\link[PBStools]{inWord}},
  \code{\link[PBStools]{readClog}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{IO}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M01_Utility}
% \concept{ ~cpt2 }
% Use only one concept per line.
