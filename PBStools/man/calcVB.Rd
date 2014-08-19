\name{calcVB}
\alias{calcVB}
\title{Calculate Fit using a von Bertalanffy Growth Model}
\description{
  Calculate the length or weight \emph{vs.} age relationship by fitting the 
  data using a von Bertalanffy growth model.
}
\usage{
calcVB(dat=pop.age, strSpp="", yfld="len", fixt0=FALSE, 
   areas=list(major=NULL, minor=NULL, locality=NULL, srfa=NULL,srfs=NULL, popa=NULL),
   ttype=list(Commercial=c(1,4),Research=c(2,3)), stype=c(1,2,6,7), scat=NULL,
   sex=list(Females=2,Males=1), rm.studs=NULL,
   year=NULL, ylim=NULL, ameth=3, allTT=TRUE, jit=c(0,0), 
   eps=FALSE, pdf=FALSE, pix=FALSE, wmf=FALSE,
   plotname, singles=FALSE, pages=FALSE, ioenv=.GlobalEnv)
}
\arguments{
  \item{dat}{biological data set with fields \code{yfld} and \code{age}.}
  \item{strSpp}{string code for species. If dataset has attribute \code{spp},
    this will be used before \code{strSpp}.}
  \item{yfld}{string name of the field in \code{dat} that contains y-values
    for the fit with \code{age}.}
  \item{fixt0}{logical: if \code{TRUE}, fix the parameter \code{t0} to equal 0.}
  \item{areas}{list of listed area codes; choices can be any/all of
    \code{major}, \code{minor}, \code{locality}, \code{srfa}, \code{srfs}, \code{popa}.}
  \item{ttype}{list of trip type codes to use from field \code{ttype}.}
  \item{stype}{vector of sample type codes to use from field \code{stype}.}
  \item{scat}{numeric code(s) for species category (usually to identify sorted vs. unsorted samples).}
  \item{sex}{list of sex codes, where \cr
    0=not looked at, 1=male, 2=female, 3=looked at but unknown \cr
    e.g. \code{list(Females=2, Males=1, Unknown=c(0,3))}.}
  \item{rm.studs}{numeric \emph{n}, if specified, remove Studentized residuals lying outside \emph{n} standard deviations.}
  \item{year}{numeric integer vector of years used to subset \code{dat}.}
  \item{ylim}{numeric vector specifying the limits of the y-axis.}
  \item{ameth}{code specifying ageing method protocol (defaults to \code{3}, specifying break & burn only).}
  \item{allTT}{logical: if \code{TRUE}, force the display of all trip types either specified or available.}
  \item{jit}{two-element vector specifying amount of jittering to apply in the 
    x- and -y-directions respectively. (See \code{jitter} in the \pkg{base} package.)}
  \item{eps}{logical -- if \code{TRUE}, send plot to a postscript \code{.eps} file.}
  \item{pdf}{logical -- if \code{TRUE}, send plot to a postscript \code{.pdf} file.}
  \item{pix}{logical -- if \code{TRUE}, send plot to a raster \code{.png} file.}
  \item{wmf}{logical -- if \code{TRUE}, send plot to a vector \code{.wmf} file.}
  \item{plotname}{explicit image file name to override an internally generated one.}
  \item{singles}{logical: if \code{TRUE} and \code{eps|pdf|pix|wmf}, send each area/trip type combination to separate files.}
  \item{pages}{logical: if \code{TRUE} and \code{eps|pdf|pix|wmf}, send each page of area plots to separate files.}
  \item{ioenv}{input/output environment for function input data and output results.}
}
\details{
  The function plots von Bertalanffy fits to length-age or weight-age data where each column 
  shows the fits for Males, Females, and M+F. If the data are plotted to the 
  screen device, each row shows the fits by trip type, and each page shows the 
  fits by area. If plots are sent to image files (\code{.eps}, \code{pdf}, \code{.png} or \code{.wmf}),
  each trip type and area combination goes to separate image files (handy for
  stacking in a \code{.doc} file).
  
  The von Bertalanffy fit requires initial parameter estimates and bounds. These 
  are taken from the data object \code{parVec}. If the species code you are 
  using is not contained in this file, the function uses the initial estimates for 
  Pacific Ocean Perch (\emph{Sebastes alutus}).
}
\value{
  No value is explicitly returned by the function. A global list object \code{PBStool}
  provides the following plus some labels:
  \item{out }{Output array of model parameter estimates and other metrics.}
  \item{pVec }{The initial parameter vector used for the model.}
  \item{xlim }{Limits of the x-axis.}
  \item{ylim }{Limits of the y-axis.}
  \item{fits }{List of x- and y-values that describe the von-Bertalanffy fits to the data.}
  
  The fits for females, males, and sexes combined (M+F) are also written to a 
  comma-separated \code{.csv} file using a name that includes areas, trip types, 
  and years (if specified).
}
\note{
  A suitable data set can be obtained by running the SQL query \code{gfb_bio.sql}.

  \code{getData("gfb_bio.sql","GFBioSQL","396",path=.getSpath())}
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link{calcSG}}, \code{\link{pop.age}}, \code{\link{parVec}}, \code{\link{getData}}, 
  \code{\link{biteData}}, \code{\link{pmfc}}, \code{\link{species}} \cr
  \pkg{PBSmodelling}: \code{\link[PBSmodelling]{calcMin}} \cr
  \pkg{base}: \code{\link[base]{jitter}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(){
  data(pop.age,envir=.PBStoolEnv)
  calcVB(areas=list(srfa=list(c("5AB","5CD"))))
  invisible() }
pbsfun()
})
}
\keyword{hplot}
