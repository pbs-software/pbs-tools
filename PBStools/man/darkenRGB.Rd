\name{darkenRGB}
\alias{darkenRGB}
\title{
  Darken RGB Colours
}
\description{
  Programmatically darken the colour of existing RGB values.
}
\usage{
darkenRGB(cols, pct, add)
}
\arguments{
  \item{cols}{\code{character} -- vector of colour names used by R.}
  \item{pct}{\code{numeric} -- percentage by which to darken the RGBs.}
  \item{add}{\code{numeric} -- absolute amount of `darkening' to add.}
}
\details{
  Algorithm provided by \href{https://stackoverflow.com/questions/30219738/is-there-a-way-to-programmatically-darken-the-color-given-rgb-values}{Roland}
  on stackoverflow using R package \pkg{colorspace}.
}
\value{
  Returns a vector of hexadecimal colours.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2021-06-14}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{addStrip}},
  \code{\link[PBStools]{clarify}},
  \code{\link[PBStools]{plotTernary}}\cr
In package \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{lucent}},
  \code{\link[PBSmodelling]{pickCol}},
  \code{\link[PBSmodelling]{testCol}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{color}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% Use only one concept per line.
\concept{stackoverflow}
\concept{M01_Utility}
