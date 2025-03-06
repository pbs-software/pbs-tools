\name{prepGFsurv}
\alias{prepGFsurv}
\alias{prepGIGhis}
\alias{prepHSass}
\alias{prepHSsyn}
\alias{prepNMFStri}
\alias{prepQCSsyn}
\alias{prepWCHGsyn}
\alias{prepWCVIsyn}
\title{
  PJS -- Prepare Groundfish Surveys for Analysis
}
\description{
  Master function that calls various preparation functions and cleans data to a standard format.
}
\usage{
prepGFsurv(file, survey, strSpp, datemask, doorspread=61.6,
   shrimpyear=1975, savefile=TRUE, usevar=FALSE, attended=FALSE,
   spath="C:/Users/haighr/Files/Projects/R/Develop/PBStools/Authors/SQLcode")

prepGIGhis (file, survey="GIGhis")
prepHSass  (file, survey="HSass")
prepHSsyn  (file, survey="HSsyn")
prepNMFStri(file, survey="NMFStri")
prepQCSsyn (file, survey)
prepWCHGsyn(file, survey="WCHGsyn")
prepWCVIsyn(file, survey="WCVIsyn")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{\code{character} -- string name of input file, e.g, \code{"SSID=2&species=435.csv"}.}
  \item{survey}{\code{character} -- survey abbreviations corresponding to original PJS survey numbers in parentheses (use only abbreviations):\cr
    \code{HSass   (= 1)} -- HS (Hecate Strait) Assemblage survey;\cr
    \code{QCSsyn  (= 2)} -- QCS (Queen Charlotte Sound) Synoptic survey;\cr
    \code{WCVIsyn (= 3)} -- WCVI (west coast Vancouver Island) Synoptic survey;\cr
    \code{WCVIshr (= 4)} -- WCVI Shrimp survey;\cr
    \code{NMFStri (= 5)} -- NMFS (US National Marine Fisheries Service) Triennial survey;\cr
    \code{HSpac   (= 6)} -- Pacific Cod Monitoring survey;\cr
    \code{QCSshr  (= 7)} -- QCS Shrimp survey;\cr
    \code{GBRhis  (= 8)} -- GIG (Goose Island Gully) Historical survey (originally GF Reed);\cr
    \code{WCHGsyn (= 9)} -- WCHG (west cosat Haida Gwaii) Synoptic survey (originally WCQCI: west coast Queen Charlotte Islands);\cr
    \code{HSsyn   (=10)} -- HS Synoptic survey;\cr
    \code{GIGhis  (=11)} -- GIG Retrospective survey (not for biomass);\cr
    \code{GIG1995 (=12)} -- 1995 GIG Retrospective (charter vessels \emph{Ocean Selector} and \emph{Frosti}). }
  \item{strSpp}{\code{character} -- optional if the species code appears in the \code{file} name.}
  \item{datemask}{\code{unknown} -- not used (probably deprecated).}
  \item{doorspread}{\code{numeric} -- default doorspread value (metres).}
  \item{shrimpyear}{\code{numeric} -- shrimp year (unknown usage).}
  \item{savefile}{\code{logical} -- if \code{TRUE}, save the processed results to a binary \code{'.rda'} file.}
  \item{usevar}{\code{logical} -- if \code{TRUE}, sets variable \code{useflag=1}, which does nothing.}
  \item{attended}{\code{logical} -- if \code{TRUE}, sets variable \code{attendflag=""}, which does nothing.}
  \item{spath}{\code{character} -- path to SQL code where SQL query \code{"gfb\_survey\_data.sql"} resides;
    query is made only if the supplied file by name is not present in the user's working directory.}
}
\details{
  The function \code{prepGFsurv} acts as a wrapper which calls specific peparation functions, depending on the survey.

  The argument \code{file} can be either a file name or a data frame that has already been imported, depending on circumstance.
  This flexibility may cause problems, but for now the argument plays a dual purpose.
}
\value{
  Survey data prepared for analysis and plotting.
}
\author{
  \href{mailto:paul@starrfish.net}{Paul J. Starr}, Chief Groundfish Scientist\cr
  Canadian Groundfish Research and Conservation Society (CGRCS), Nanaimo BC

  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2023-02-10}
}
\note{
  PJS maintains code in statistical software called \href{https://www.stata.com/}{STATA}.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{calcBiom}},
  \code{\link[PBStools]{doSynoptic}},
  \code{\link[PBStools]{getLabels}},
  \code{\link[PBStools]{keepAtts}},
  \code{\link[PBStools]{plotIndex}},
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
