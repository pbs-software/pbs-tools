\name{calcSurficial}
\alias{calcSurficial}
\title{Calculate Intersection of Surficial Geology and Bathymetry Habitat}
\description{
  Calculate the intersection of surficial geology and potential 
  habitat using a bathymetry interval.
}
\usage{
calcSurficial(surf="qcb", hab,
    xlim=c(-133.4,-127.8), ylim=c(50.5,54.8),
    col.hab=c("aliceblue","grey"), col.cst="grey85", 
    pix=FALSE, wmf=FALSE, ioenv=.GlobalEnv)
}
\arguments{
  \item{surf}{surficial geology data object (e.g., \code{qcb}).}
  \item{hab}{bathymetry habitat data object created by \code{calcHabitat()}.}
  \item{xlim}{X-limits of the mapping region.}
  \item{ylim}{Y-limits of the mapping region.}
  \item{col.hab}{2-element vector of colours for \code{hab} (fill, border).}
  \item{col.cst}{scalar specifying fill colour for the coast.}
  \item{pix}{logical: if \code{TRUE}, send plot to a \code{.png} file.}
  \item{wmf}{logical: if \code{TRUE}, send plot to a \code{.wmf} file.}
  \item{ioenv}{input/output environment for function input data and output results.}
}
\details{
  The function uses available datasets \code{surf} (surficial geology) and 
  \code{hab} (bathymetry habitat) and calculates the intersection. The results 
  are displayed on a map and stored in the global list object \code{PBStool}.
}
\author{ 
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-27}\cr
}
\seealso{
  \code{\link[PBStools]{calcHabitat}}, \code{\link[PBStools]{preferDepth}},
   \code{\link[PBSdata]{qcb}}, \code{\link[PBSdata]{bctopo}} \cr
  \pkg{PBSmapping}: \code{\link[PBSmapping]{makeTopography}}, \code{\link[PBSmapping]{convCP}}, 
  \code{\link[PBSmapping]{joinPolys}}, \code{\link[PBSmapping]{calcArea}}
}
\examples{
\dontrun{
local(envir=.PBStoolEnv,expr={
pbsfun=function() {
  habitat=calcHabitat(isob=c(150,435), digits=1, plot=FALSE) # DBR
  calcSurficial(surf="qcb", hab=habitat)
  invisible() }
pbsfun()
})
}
}
\keyword{hplot}
\concept{M05_Spatial}
