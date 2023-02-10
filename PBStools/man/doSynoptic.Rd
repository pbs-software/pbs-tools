\name{doSynoptic}
\alias{doSynoptic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  PJS -- Perform Common Machinations for Synoptic Surveys
}
\description{
  Get the synoptic file or data.frame for specified survey and go through PJS machinations.
}
\usage{
doSynoptic(dat, survey, logappendflag=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{\code{data.frame|character} -- input can be either a file name or a data frame that has already been imported.}
  \item{survey}{\code{character} -- abbreviated form of survey name; note: usage of PJS survey numbers is deprecated.}
  \item{logappendflag}{\code{logical} -- if \code{TRUE}, sets variable \code{append=TRUE}, which has no effect at present.}
}
\details{
  Although this function was designed by PJS for use with synoptic surveys (four in BC),
  it can be used on other surveys and serves as a standardising routine.

  PJS survey codes in relation to GFBioSQL (GFB) survey codes:
  \tabular{rrrl}{
    PJS \tab Abbr \tab GFB \tab GFBioSQL description\cr
    1  \tab \code{HSass}   \tab 2 \tab Hecate Strait Multispecies Assemblage Bottom Trawl\cr
    2  \tab \code{QCSsyn}  \tab  1 \tab Queen Charlotte Sound Synoptic Bottom Trawl\cr
    3  \tab \code{WCVIsyn} \tab  4 \tab  West Coast Vancouver Island Synoptic Bottom Trawl\cr
    4  \tab \code{WCVIshr} \tab  7 \tab  West Coast Vancouver Island Multispecies Small-mesh Bottom Trawl\cr
    5  \tab \code{NMFStri} \tab 79 \tab  National Marine Fisheries Service Triennial Bottom Trawl (Vancouver Region)\cr
    6  \tab \code{HSpac}   \tab  5 \tab  Hecate Strait Pacific Cod Monitoring Bottom Trawl\cr
    7  \tab \code{QCSshr}  \tab  6 \tab  Queen Charlotte Sound Multispecies Small-mesh Bottom Trawl\cr
    8  \tab \code{GBRhis}  \tab 26 \tab  Goose Island Gully Historic G.B. Reed Bottom Trawl\cr
    9  \tab \code{WCHGsyn} \tab 16 \tab  West Coast Haida Gwaii Synoptic Bottom Trawl\cr
    10 \tab \code{HSsyn}   \tab  3 \tab  Hecate Strait Synoptic Bottom Trawl\cr
    11 \tab \code{GIGhis}  \tab 21 \tab Goose Island Gully Bottom Trawl Retrospective\cr
    12 \tab \code{GIG1995} \tab 10 \tab 1995 Goose Island Gully Rockfish Bottom Trawl
  }
}
\value{
  Modified survey data frame object (e.g., adds best depth, calculates missing distance, etc.).
}
\author{
  \href{mailto:paul@starrfish.net}{Paul J. Starr}, Chief Groundfish Scientist\cr
  Canadian Groundfish Research and Conservation Society (CGRCS), Nanaimo BC

  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Regional Headquarters (RHQ), Vancouver BC\cr
  Last modified \code{Rd: 2023-02-10}
}
\note{
  PJS maintains code in statistical software called \href{https://www.stata.com/}{STATA}.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{calcBiom}},
  \code{\link[PBStools]{getLabels}},
  \code{\link[PBStools]{keepAtts}},
  \code{\link[PBStools]{plotIndex}},
  \code{\link[PBStools]{prepGFsurv}},
  \code{\link[PBStools]{restratify}},
  \code{\link[PBStools]{uniqtows}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{indices}
\concept{M09_PJSsurvey}
% Use only one concept per line.
