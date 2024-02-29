\name{getLabels}
\alias{getLabels}
\alias{getLabelends}
\alias{getMonth}
\alias{getSpecies}
\alias{getUsability}
\alias{getVessel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  PJS -- Get Functions
}
\description{
  Primary \code{get} function is \code{getLabels}, which reads in data file, 
  extracts assorted data (metadata), and attaches metadata as labels (attributes) to data.
  See \sQuote{Details} section for information on all \code{get} functions.
}
\usage{
getLabels(file)
getLabelends(labelname)
getMonth(dates, type=1)
getSpecies(species)
getUsability(dat, use=c(0,1,2,6))
getVessel(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{\code{character} -- string name of input file, e.g, \code{"SSID=2&species=435.csv"}.}
  \item{labelname}{\code{character} -- usually a vector of stratum names that describe depth intervals.}
  \item{dates}{\code{character} -- a vector of dates.}
  \item{type}{\code{numeric} -- code for type of month format desired.}
  \item{species}{\code{character|numeric} -- species code from data|file|query.}
  \item{dat}{\code{data.frame} -- survey data object.}
  \item{use}{\code{numeric} -- usability codes.}
}
\details{
  The \code{get} functions:
  \itemize{
    \item \code{getLabels} -- acts as an archiving system by attaching metadata as attributes to survey data object;
    \item \code{getLabelends} -- get depth ranges from stratum names;
    \item \code{getMonth} -- convert dates to months using \code{type}:
    \itemize{
      \item \code{1} = numeric month,
      \item \code{2} = month abbreviation,
      \item \code{3} = month name,
      \item \code{4} = two-character string;
    }
    \item \code{getSpecies} -- a somewhat do-nothing function that makes sure that
       species codes are padded with zero if less than two characters;
    \item \code{getUsability} -- subset data for usable records using codes \code{use}:
    \itemize{
      \item \code{0} = Unknown,
      \item \code{1} = Fully usable,
      \item \code{2} = Fail, but all data usable,
      \item \code{6} = Gear torn, all data OK;
    }
    \item \code{getVessel} -- add PJS vessel codes to survey data object.
  }
}
\value{
  Data frame imported from input file with attached metadata as attributes.
}
\author{
  \href{mailto:paul@starrfish.net}{Paul J. Starr}, Chief Groundfish Scientist\cr
  Canadian Groundfish Research and Conservation Society (CGRCS), Nanaimo BC

  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2019-12-13}
}
\note{
  PJS maintains code in statistical software called \href{https://www.stata.com/}{STATA}.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{calcBiom}},
  \code{\link[PBStools]{doSynoptic}},
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
\concept{PJS}
% Use only one concept per line.
