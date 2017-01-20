\name{calcHabitat}
\alias{calcHabitat}
\title{Calculate Potential Habitat Using Bathymetry}
\description{
  Calculate potential habitat using specified bathymetry limits.
}
\usage{
calcHabitat(topofile="bctopo", isob=c(150,435),
     digits=1, minVerts=25, col.hab="greenyellow", col.land="moccasin",
     xlim=NULL, ylim=NULL,areas=list(), col.areas="red", isolab=TRUE, labtit="",
     plot=TRUE, pin=c(7,8), eps=FALSE, png=FALSE, wmf=FALSE, ioenv=.GlobalEnv)
}
\arguments{
  \item{topofile}{topographical ASCII XYZ file downloaded from the Scripps
    Institution of Oceanography \url{http://topex.ucsd.edu/WWW_html/mar_topo.html}.}
  \item{isob}{isobath limits (shallowest, deepest).}
  \item{digits}{the number of decimal places to round geographical coordinates.}
  \item{minVerts}{the minimum number of vertices per polygon (parent or hole) to retain the polygon
    (becomes critical when \code{digits>1}).}
  \item{col.hab}{colour to fill the bathymetry interval (potential habitat).}
  \item{col.land}{colour to fill the land area.}
  \item{xlim}{longitude limits of the x-axis}
  \item{ylim}{latitude limits of the y-axis}
  \item{areas}{list of area objects}
  \item{col.areas}{vector of colours for the boundary lines of area objects (recycled if necessary)}
  \item{isolab}{logical: if \code{TRUE}, add a legend label to the plot}
  \item{labtit}{title for the legend label}
  \item{plot}{logical: if \code{TRUE}, plot the results.}
  \item{pin}{numeric vector: width and height of a figure device plot in inches.}
  \item{eps}{logical: if \code{TRUE}, send plot to a postscript (\code{.eps}) file.}
  \item{png}{logical: if \code{TRUE}, send plot to a raster (\code{.png}) file.}
  \item{wmf}{logical: if \code{TRUE}, send plot to a metafile (\code{.wmf}) file.}
  \item{ioenv}{input/output environment for function input data and output results.}
}
\details{
  The function uses the dataset \code{bctopo} downloaded from the site 
  \url{http://topex.ucsd.edu/cgi-bin/get_data.cgi} using the boundaries: 
  west (-134.5), east (-124.5), south (48), north (55).

  The \code{PBSmapping} function \code{makeTopography} can become very slow as
  the precision of the longitude-latitude data is increased. 
  Setting \code{digits=1} provides a very fast but roughly estimated habitat region. 
  Note that you may need to reduce the value of \code{minVerts} to get get all the major shapes. 
  Increasing the precision to \code{digits=2} slows the code markedly but provides a much more detailed bathymetry outline.
  
  Some choice of bathymetry limits, noticeably shallow depths, cause the function 
  \code{joinPolys} to crash. Therefore, \code{calcHabitat} taps into a patch function \code{findHoles}
  which weeds out small polygons (less than \code{minVerts} vertices) and identifies holes within parents.
  The function \code{findHoles} can become very slow when there are hundreds of polygons.
  Ideally, \code{findHoles} should be programmed in C.
}
\value{
  Returns a a bathymetry habitat \code{PolySet}.
  Additionally, a global list object \code{PBStool} contains the object
  \code{bathy} created from the call to \code{makeTopography}, the final 
  bathymetry habitat \code{habitat}, and a variety of \code{PolySet}s 
  created in transforming \code{bathy} to \code{habitat}.
}
\author{ 
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link{calcSurficial}}, \code{\link{getFile}}, \code{\link{findHoles}} \cr
  \pkg{PBSmapping}: \code{\link[PBSmapping]{makeTopography}}, \code{\link[PBSmapping]{convCP}}, 
  \code{\link[PBSmapping]{joinPolys}}, \code{\link[PBSmapping]{calcArea}} \cr
  \pkg{grDevices}: \code{\link[grDevices]{contourLines}}
}
\examples{
\dontrun{
local(envir=.PBStoolEnv,expr={
pbsfun=function(){
  calcHabitat(isob=c(150,435),digits=1) # DBR
  invisible() }
pbsfun()
})
}
}
\keyword{hplot}
