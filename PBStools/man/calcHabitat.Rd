\name{calcHabitat}
\alias{calcHabitat}
\alias{plotHabitat}
\title{Calculate Potential Habitat Using Bathymetry}
\description{
  Calculate potential habitat using specified bathymetry limits.
}
\usage{
calcHabitat(topofile="bctopo", isob=c(150,435),
   digits=1, minVerts=25, use.sp.pkg=TRUE, areas=list(),
   col.hab="greenyellow", col.land="moccasin", col.areas="red",
   xlim=NULL, ylim=NULL, isolab=TRUE, labtit="", plot=TRUE,
   PIN=c(7,8), eps=FALSE, png=FALSE, wmf=FALSE, ioenv=.GlobalEnv)

plotHabitat(fnam="hab228", isob=c(62,448), col.hab="greenyellow",
   png=FALSE, pngres=400, PIN = c(9,8.7), fout="daHood", lang=c("e","f"))
}
\arguments{
  \item{topofile}{\code{character} -- name of topographical ASCII XYZ file downloaded from the Scripps
    Institution of Oceanography, \href{https://topex.ucsd.edu/WWW_html/mar_topo.html}{Satellite Geodesy}.}
  \item{isob}{\code{numeric} -- isobath limits (shallowest, deepest).}
  \item{digits}{\code{numeric} -- number of decimal places to round geographical coordinates.}
  \item{minVerts}{\code{numeric} -- minimum number of vertices per polygon (parent or hole) to retain the polygon
    (becomes critical when \code{digits>1}).}
  \item{use.sp.pkg}{\code{logical} -- if \code{TRUE}, use \code{sp::point.in.polygon},\cr
    otherwise use \code{PBSmapping::.is.in} (~3x slower) in call to \code{'findHoles'}.}
  \item{areas}{\code{list} -- list of area objects (e.g. \code{major}).}
  \item{col.hab}{\code{character} -- name of colour to fill the bathymetry interval (potential habitat).}
  \item{col.land}{\code{character} -- name of colour to fill the land area.}
  \item{col.areas}{\code{character} -- vector of colour names for the boundary lines of area objects (recycled if necessary).}
  \item{xlim}{\code{numeric} -- longitude limits of the x-axis.}
  \item{ylim}{\code{numeric} -- latitude limits of the y-axis.}
  \item{isolab}{\code{logical} -- if \code{TRUE}, add a legend label to the plot.}
  \item{labtit}{\code{character} -- title for the legend label.}
  \item{plot}{\code{logical} -- if \code{TRUE}, plot the results.}
  \item{PIN}{\code{numeric} -- vector of width and height for a figure device plot in inches.}
  \item{eps}{\code{logical} -- if \code{TRUE}, send plot to a postscript (\code{.eps}) file.}
  \item{png}{\code{logical} -- if \code{TRUE}, send plot to a raster (\code{.png}) file.}
  \item{wmf}{\code{logical} -- if \code{TRUE}, send plot to a metafile (\code{.wmf}) file.}
  \item{ioenv}{\code{environment} -- input/output environment for function input data and output results.}

  \item{fnam}{\code{character} -- name of bathymetry habitat \code{PolySet} output from \code{calcHabitat}.}
  \item{pngres}{\code{numeric} -- resolution in pixels per inch for the png file.}
  \item{fout}{\code{character} -- image file name output for \code{plotHabitat}.}
  \item{lang}{\code{character} -- language for image labels, where \code{'e'}=english and \code{'f'}=french.}
}
\details{
  The function uses the dataset \code{bctopo} downloaded from the TOPEX site 
  \href{https://topex.ucsd.edu/cgi-bin/get_data.cgi}{EXTRACT XYZ GRID - TOPOGRAPHY OR GRAVITY}
  using the boundaries:  west (-134.5), east (-124.5), south (48), north (55).

  The \code{PBSmapping} function \code{'makeTopography'} can become very slow as
  the precision of the longitude-latitude data is increased. 
  Setting \code{digits=1} provides a very fast but roughly estimated habitat region. 
  Note that you may need to reduce the value of \code{minVerts} to get get all the major shapes. 
  Increasing the precision to \code{digits=2} slows the code markedly but provides a much more detailed bathymetry outline.

  Some choices of bathymetry limits, noticeably shallow depths, cause the function 
  \code{'joinPolys'} to crash. Therefore, \code{'calcHabitat'} taps into a patch
  function \code{'findHoles'} which weeds out small polygons (< \code{minVerts} vertices)
  and identifies holes within parents.
  The function \code{'findHoles'} can become very slow when there are hundreds of polygons.
  Setting the argument \code{'use.sp.pkg'} to \code{TRUE} taps into a function called
  \code{'point.in.polygon'} from the Spatial Data package \code{'sp'}, which is
  approximately 3x faster than using the \code{PBSmapping} function \code{'.is.in'}
  (which calls the C program \code{'findPolys'}).
  
}
\value{
  Returns a a bathymetry habitat \code{PolySet}.
  Additionally, a global list object \code{PBStool} contains the object
  \code{bathy} created from the call to \code{makeTopography}, the final 
  bathymetry habitat \code{habitat}, and a variety of \code{PolySet}s 
  created in transforming \code{bathy} to \code{habitat}.
}
\author{ 
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2021-06-14}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{calcSurficial}},
  \code{\link[PBStools]{getFile}},
  \code{\link[PBStools]{findHoles}},
  \code{\link[PBStools]{ttget}} \cr
In package \pkg{PBSmapping}:\cr
  \code{\link[PBSmapping]{calcArea}},
  \code{\link[PBSmapping]{convCP}},
  \code{\link[PBSmapping]{joinPolys}},
  \code{\link[PBSmapping]{makeTopography}}\cr
In package \pkg{grDevices}:\cr
  \code{\link[grDevices]{contourLines}}
}
\examples{
\dontrun{
local(envir=.PBStoolEnv,expr={
pbsfun=function(){
  calcHabitat(isob=c(150,435),digits=1) ## DBR
  invisible() }
pbsfun()
})
}
}
\keyword{hplot}
\concept{COSEWIC}

