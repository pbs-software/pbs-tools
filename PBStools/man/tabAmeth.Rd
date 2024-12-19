\name{tabAmeth}
\alias{tabAmeth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Tabulate Ageing Methods
}
\description{
  Produce a table that reports the number of aged otoliths
  (specimens) and samples by trip type and ageing method for each sex.
}
\usage{
tabAmeth(dat, aged=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{\code{data.frame} -- biological data extract (\code{bio123})}
  \item{aged}{\code{logical} -- if \code{TRUE}, tabulate only the aged structures}
}
\details{
  Uses \code{'crossTab'} for each ageing method to 
  summarise the number of specimens and samples by sex.
}
\value{
  Creates \code{'ameth.summary.csv'} for use in Biological appendices (usually Appendix D).\cr
  Also returns the summary table as part of a list object.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-12-19}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{extractAges}},
  \code{\link[PBStools]{tabOtos}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
\keyword{file}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M06_Assessment}
% \concept{ ~cpt2 }
% Use only one concept per line.
