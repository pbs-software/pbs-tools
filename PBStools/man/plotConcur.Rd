\name{plotConcur}
\alias{plotConcur}
\title{
  Plot Concurrent Species in Fishing Events at Depth
}
\description{
  Create a barplot of the top \emph{N} concurrent species from 
  fishing events capturing \code{strSpp} between \code{mindep} 
  and \code{maxdep} in PMFC \code{major} areas.
}
\usage{
plotConcur(strSpp="410", dbName="GFFOS", spath=.getSpath(),
     mindep=150, maxdep=435, major=NULL, minor=NULL, top=NULL, gear=1,
     saraSpp=c("027","034","394","410","424","435","437","440","442","453"),
     reset.mf=TRUE, eps=FALSE, png=FALSE, pngres=300, colour="topo")
}
\arguments{
  \item{strSpp}{\code{character} -- string specifying species of interest (often a Hart code for fish).}
  \item{dbName}{\code{character} -- name of database with species catch information. \cr
    If \code{dbName="GFFOS"}, then the \code{SQL} query \code{fos_concurrent.sql} is run. \cr
    If \code{dbName="PacHarvest"}, then the \code{SQL} query \code{pht_concurrent.sql} is run. \cr
    If \code{dbName="PacHarvHL"}, then the \code{SQL} query \code{phhl_concurrent.sql} is run.}
  \item{spath}{\code{character} --  path to the SQL query file chosen above through \code{dbName}.}
  \item{mindep}{\code{numeric} -- minimum depth (m) where tows occur.}
  \item{maxdep}{\code{numeric} -- maximum depth (m) where tows occur.}
  \item{major}{\code{numeric} -- codes for PMFC major areas; default selects \code{major=c(1,3:9)}.}
  \item{minor}{\code{numeric} -- codes for PMFC minor areas.}
  \item{top}{\code{numeric} -- top (first) \emph{N} records to return from an \code{SQL} query.}
  \item{gear}{\code{numeric} -- gear type code usually specified by gear subtype code: \cr
    \code{1 =} bottom, \code{2 =} shrimp, \code{3 =} midwater}
  \item{saraSpp}{\code{character} -- codes specifying species listed by COSEWIC as either: \cr
    \dQuote{Special Concern}, \dQuote{Threatened}, or \dQuote{Endangered}.}
  \item{reset.mf}{\code{logical} -- if \code{TRUE}, reset the multiple figure region to \code{c(1,1)}
    (only effective for plots sent to the graphics window device).}
  \item{eps}{\code{logical} -- if \code{TRUE}, send the barplot to an \code{.eps} file.}
  \item{png}{\code{logical} -- if \code{TRUE}, send the barplot to a \code{.png} file.}
  \item{pngres}{\code{numeric} -- resolution (pixels/inch) for \code{.png} file.}
  \item{colour}{\code{character} -- two colour choices for horizontal bars:\cr
    \code{"topo"} -- gradations from yellow through green to blue, top to bottom;\cr
    \code{"sombre"} -- grey for all species, pink for COSEWIC species, and light blue for \code{strSpp}.}
}
\details{
  This function originated from Norm Olsen's \code{SQL} code \code{pht_concurrent.sql}
  (located in the directory \code{.../library/PBStools/sql}) to query the observer
  trawl database \code{PacHarvest}. A variant of this \code{phhl_concurrent.sql}
  taps into the hook and line database \code{PacHarvHL}.
  
  Currently, the default SQL code queries the \code{GF_MERGED_CATCH} table in \code{GFFOS},
  which summarises multiple fisheries.
}
\value{
  Invisibly returns a data frame of top \emph{N} species ordered by percent catch weight.

  Also, objects of interest are saved to the global list object \code{PBStool}.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2020-07-14}
}
\seealso{
  \code{\link[PBStools]{preferDepth}}, \code{\link[PBStools]{getData}}, \code{\link[PBStools]{toUpper}} \cr
  Available SQL queries: \code{\link[PBStools]{SQLcode}}
}
\keyword{hplot}

