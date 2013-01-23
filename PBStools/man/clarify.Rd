\name{clarify}
\alias{clarify}
\title{Summarize Catch Data Into CLARA clusters}
\description{
  Summarize large datasets of catch represented by many species into 
  \emph{n} \code{CLARA} clusters.
}
\usage{
clarify(dat, cell=c(0.1,0.075), nG=8,
   xlim=c(-134.2,-127), ylim=c(49.6,54.8), zlim=NULL, targ="YMR", 
   clrs=c("red","orange","yellow","green","forestgreen","deepskyblue",
   "blue","midnightblue"), spp.names=FALSE, 
   wmf=FALSE, eps=FALSE, hpage=10)
}
\arguments{
  \item{dat}{an \code{EventData} obejct with fields \code{EID}, \code{X},
    \code{Y}, optional \code{Z}, and species fields named by 3-letter codes.}
  \item{cell}{cell size in degrees Lon/Lat (UTMs currently not implemented).}
  \item{nG}{number of CLARA groups to end up with.}
  \item{xlim}{X-limits of the mapping region.}
  \item{ylim}{Y-limits of the mapping region.}
  \item{zlim}{optional Z-limits (depth) to qualify data (if field available).}
  \item{targ}{target species to identify within the \code{CLARA} groups.}
  \item{clrs}{vector of colours to use for displaying \code{nG} \code{CLARA} groups.}
  \item{spp.names}{logical: if \code{TRUE}, replace species codes
    (if they are used in the data) with species names in the legend.}
  \item{wmf}{logical: if \code{TRUE}, send plot to a windows meta file \code{.wmf}.}
  \item{eps}{logical: if \code{TRUE}, send plot to an encapsulated postscript \code{.eps} file.}
  \item{hpage}{dimension in inches of the longest side of a plot page sent to an output file.}
}
\details{
  The function uses the function \code{clara} in the package 
  \pkg{cluster}. Essentially, \code{clara} sub-samples the large data set, 
  identifying the best \emph{k} medoids (centre is defined as the item 
  which has the smallest sum of distances to the other items in the cluster) 
  using a dissimilarity metric. In the end, all records are assigned to one 
  of the \code{k} clusters. Further routines in the R-package \code{PBSmapping}
  locate fishing events in grid cells and display the results.
  
  The results are displayed on a map and stored in the global list object \code{PBSfish}.
}
\references{
  Kaufman, L. and Rousseeuw, P.J. (1990) Finding Groups in Data: An Introduction 
  to Cluster Analysis. Wiley, New York.
}
\author{ 
  Rowan Haigh and Norm Olsen, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link[PBStools]{getFile}}, \code{\link[PBStools]{prepClara}} \cr
  \pkg{PBSmapping}: \code{\link[PBSmapping]{makeGrid}}, \code{\link[PBSmapping]{findCells}} \cr
  \pkg{cluster}: \code{\link[cluster]{clara}} (clustering large applications) \cr
  \pkg{PBSdata}: \code{\link[PBSdata]{claradat}} for an example dataset.
}
\examples{
pbsfun=function(){
  clarify(claradat,xlim=c(-132,-127.8),ylim=c(50.6,52.1),
    cell=c(.05,.03),nG=7)
  invisible() }
pbsfun()
}
\keyword{hplot}
