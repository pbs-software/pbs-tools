\name{quickCat}
\alias{quickCat}
\title{
Quick Catch Summaries
}
\description{
Reduce large output from query \code{'fos_mcatORF.sql'} to records with positive catch and produce catch summary \code{'.csv'} files.
}
\usage{
quickCat(dat, strSpp="000", removeSM=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{\code{data.frame} -- data object created by a call to \code{'fos_mcatORF.sql'}.}
  \item{strSpp}{\code{character} -- string species code, usually Hart code (page number as a string).}
  \item{removeSM}{\code{logical} -- if \code{TRUE}, remove catch data from seamounts.}
}
\details{
Using the \code{'crossTab'} function, produce \code{'.csv'} files summarizing catch (tonnes) by year and one of the following factors:
\itemize{
  \item \code{'cat000.fid.sum.csv'} -- catch by fishery ID (e.g. Trawl);
  \item \code{'cat000.gear.sum.csv'} -- catch by gear type (e.g. Bottom Trawl);
  \item \code{'cat000.db.sum.csv'} -- catch by database (e.g. GFFOS);
  \item \code{'cat000.sector.sum.csv'} -- catch by fishing sector (e.g. GROUNDFISH TRAWL).
}
}
\value{
Invisibly returns data object with positive catch records; also saves object to a binary file \code{'cat000.rda'}.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2021-09-27}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{buildCatch}},
  \code{\link[PBStools]{crossTab}},
  \code{\link[PBStools]{formatCatch}},
  \code{\link[PBStools]{getCatch}},
  \code{\link[PBStools]{plotBTMW}},
  \code{\link[PBStools]{surveyCatch}}.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
\keyword{file}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M07_CatchRecon}
% Use only one concept per line.
