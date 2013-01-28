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
plotConcur(strSpp="410", dbName="PacHarvest", 
     mindep=150, maxdep=435, major=NULL, top=NULL, trawl=1,
     saraSpp=c("027","394","410","424","435","437","440","442","453"),
     reset.mf=TRUE, eps=FALSE, pix=FALSE)
}
\arguments{
  \item{strSpp}{string code specifying species of interest (often a Hart code for fish).}
  \item{dbName}{string name of database with species catch information. \cr
    If \code{dbName="PacHarvest"}, then the \code{SQL} query \code{pht_concurrent.sql} is run. \cr
    If \code{dbName="PacHarvHL"}, then the \code{SQL} query \code{phhl_concurrent.sql} is run.}
  \item{mindep}{minimum depth (m) where tows occur.}
  \item{maxdep}{maximum depth (m) where tows occur.}
  \item{major}{PMFC major areas; default selects \code{major=c(1,3:9)}.}
  \item{top}{top (first) \emph{N} records to return from an \code{SQL} query.}
  \item{trawl}{trawl type code usually specified by gear subtype code: \cr
    \code{1 =} bottom, \code{2 =} shrimp, \code{3 =} midwater}
  \item{saraSpp}{string codes specifying species listed by COSEWIC as either: \cr
    \dQuote{Special Concern}, \dQuote{Threatened}, or \dQuote{Endangered}.}
  \item{reset.mf}{logical: if \code{TRUE}, reset the multiple figure region to \code{c(1,1)}
    (only effective for plots sent to the graphics window device).}
  \item{eps}{logical: if \code{TRUE}, send the barplot to an \code{EPS} file.}
  \item{pix}{logical: if \code{TRUE}, send the barplot to a \code{PNG} file.}
}
\details{
  This function uses Norm Olsen's \code{SQL} code \code{pht_concurrent.sql}
  (located in the directory \cr \code{.../library/PBStools/sql}) to query the observer
  trawl database \code{PacHarvest}. A variant of this \code{phhl_concurrent.sql}
  taps into the hook and line database \code{PacHarvHL}.
}
\value{
  Invisibly returns a data frame of top \emph{N} species ordered by percent catch weight.

  Also, objects of interest are saved to the global list object \code{PBStool}.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link[PBStools]{preferDepth}}, \code{\link[PBStools]{getData}}, \code{\link[PBStools]{toUpper}} \cr
  Available SQL queries: \code{\link[PBStools]{SQLcode}}
}
\keyword{hplot}

