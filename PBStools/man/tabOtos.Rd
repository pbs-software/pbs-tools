\name{tabOtos}
\alias{tabOtos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Tabulate Otoliths Available and Aged
}
\description{
  Produce tables giving the number of otoliths available and aged
  by various categories.
}
\usage{
tabOtos(strSpp="123", fpath=getwd())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{strSpp}{\code{character} -- species Hart code}
  \item{fpath}{\code{character} -- name of directory or path containing the file \code{'bio123'}.}
}
\details{
  Summarises number of otoliths by various categories:
  \itemize{
    \item \code{SSID}  -- survey series identification number,
    \item \code{ttype} -- trip type code number,
    \item \code{gear}  -- gear code,
    \item \code{major} -- major PMFC area code.
  }
}
\value{
  Creates \code{.csv} files per category for otoliths available and otoliths aged.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2023-06-08}
}
\note{
  If \code{strSpp=="396"} (POP), commercial otoliths are also summarised by adjusted PMFC major area.
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{calcStockArea}},
  \code{\link[PBStools]{expand5C}},
  \code{\link[PBStools]{makeAgeErr}},
  \code{\link[PBStools]{tabAmeth}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
\keyword{file}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
 \concept{M06_assessment}
% \concept{ ~cpt2 }
% Use only one concept per line.
