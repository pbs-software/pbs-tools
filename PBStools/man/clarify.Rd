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
   "blue","midnightblue"), land="ghostwhite", spp.names=FALSE, 
   png=FALSE, pngres=400, PIN=NULL, isobs=c(200,1000,1800), 
   wmf=FALSE, eps=FALSE, hpage=10, ioenv=.GlobalEnv)
}
\arguments{
  \item{dat}{\code{EventData} -- object with fields \code{EID}, \code{X},
    \code{Y}, optional \code{Z}, and species fields named by 3-letter codes.}
  \item{cell}{\code{numeric} -- cell size in degrees Lon/Lat (UTMs currently not implemented).}
  \item{nG}{\code{numeric} -- number of CLARA groups to end up with.}
  \item{xlim}{\code{numeric} -- X-limits of the mapping region.}
  \item{ylim}{\code{numeric} -- Y-limits of the mapping region.}
  \item{zlim}{\code{numeric} -- optional Z-limits (depth) to qualify data (if field available).}
  \item{targ}{\code{character} -- target species to identify within the \code{CLARA} groups.}
  \item{clrs}{\code{character} -- vector of colours to use for displaying \code{nG} \code{CLARA} groups.}
  \item{land}{\code{character} -- colour to shade land masses.}
  \item{spp.names}{\code{logical} -- if \code{TRUE}, replace species codes
    (if they are used in the data) with species names in the legend.}
  \item{png}{\code{logical} -- if \code{TRUE}, send plot to a portable network graphics \code{'.png'} file.}
  \item{pngres}{\code{numeric} -- resolution in pixels per inch for the png file.}
  \item{PIN}{\code{numeric} -- vector specifying width and height for output graphics file; if \code{NULL}
    the code determines \code{PIN} from the windows device.}
  \item{isobs}{\code{numeric} -- vector of isobaths to used from \code{PBSdata} object \code{isobath}.}
  \item{wmf}{\code{logical} -- if \code{TRUE}, send plot to a windows meta file \code{.wmf}.}
  \item{eps}{\code{logical} -- if \code{TRUE}, send plot to an encapsulated postscript \code{.eps} file.}
  \item{hpage}{\code{numeric} -- dimension in inches of the longest side of a plot page sent to an output file.}
  \item{ioenv}{\code{environment} -- input/output environment for function input data and output results.}
}
\details{
  The function uses the function \code{clara} in the package 
  \pkg{cluster}. Essentially, \code{clara} sub-samples the large data set, 
  identifying the best \emph{k} medoids (centre is defined as the item 
  which has the smallest sum of distances to the other items in the cluster) 
  using a dissimilarity metric. In the end, all records are assigned to one 
  of the \code{k} clusters. Further routines in the R-package \code{PBSmapping}
  locate fishing events in grid cells and display the results.
  
  The results are displayed on a map and stored in the global list object \code{PBStool}.
}
\references{
  Kaufman, L. and Rousseeuw, P.J. (1990) Finding Groups in Data: An Introduction 
  to Cluster Analysis. Wiley, New York.
}
\author{ 
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2019-03-20}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{getFile}},
  \code{\link[PBStools]{prepClara}} \cr
In package \pkg{PBSmapping}:\cr
  \code{\link[PBSmapping]{makeGrid}},
  \code{\link[PBSmapping]{findCells}} \cr
In package \pkg{cluster}:\cr
  \code{\link[cluster]{clara}} (clustering large applications) \cr
In package \pkg{PBSdata}:\cr
  \code{\link[PBSdata]{claradat}} for an example dataset.
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(){
  clarify(claradat,xlim=c(-132,-127.8),ylim=c(50.6,52.1),
    cell=c(.05,.03),nG=7)
  invisible() }
pbsfun()
})
}
\keyword{hplot}
