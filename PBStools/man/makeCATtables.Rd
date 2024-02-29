\name{makeCATtables}
\alias{makeCATtables}
\title{Make Catch Tables from Commercial Fishery Data}
\description{
  Create a \code{CSV} file containing tables of commercial 
  catch by management fishing year. Various fisheries can be specified.
}
\usage{
makeCATtables(strSpp, comm=1:7, path=.getSpath(), pout=FALSE) 
}
\arguments{
  \item{strSpp}{string specifying species code used in GFBio.}
  \item{comm}{numeric code specifying various fisheries:\cr
    \code{1} = GF Catch (GFC) trawl tows;\cr
    \code{2} = PacHarvest (PHT) trawl tows (observer or fisher logs);\cr
    \code{3} = PacHarvHL (PHHL) Zn hook and line from validation records;\cr
    \code{4} = PacHarvHL (PHHL) Zn hook and line from fisherlog records;\cr
    \code{5} = PacHarvHL (PHHL) Schedule II hook and line from validation records;\cr
    \code{6} = PacHarvHL (PHHL) Schedule II hook and line from fisherlog records;\cr
    \code{7} = PacHarvHL (PHHL) Halibut longline from validation records.}
  \item{path}{path leading to SQL code (available in \pkg{PBStools}'s directory \code{sql}).}
  \item{pout}{logical, if \code{TRUE} then print table results on the command line 
    (results are sent to the output \code{CSV} regardless).}
}
\details{
  The code scrolls through the specified fisheries, issuing the 
  appropriate SQL query for each catch summary. The results are 
  placed in a text file called \code{cattab-XXX.csv}, where 
  \code{XXX} = the \code{strSpp} specified.
}
\author{ 
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-27}
}
\seealso{ 
  \code{\link[PBStools]{getData}}, \code{\link[PBStools]{makePMRtables}} \cr
  Available SQL queries: \code{\link[PBStools]{SQLcode}}
}
\keyword{data}
\keyword{utilities}
\concept{M03_Fishery}
