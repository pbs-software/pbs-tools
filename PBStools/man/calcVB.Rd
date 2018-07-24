\name{calcVB}
\alias{calcVB}
\title{Calculate Fit using a von Bertalanffy Growth Model}
\description{
  Calculate the length or weight \emph{vs.} age relationship by fitting the 
  data using a von Bertalanffy growth model.
}
\usage{
calcVB(dat=pop.age, strSpp="", yfld="len", fixt0=FALSE, 
   areas=list(major=NULL, minor=NULL, locality=NULL, 
              srfa=NULL,srfs=NULL, popa=NULL),
   ttype=list(commercial=c(1,4,5),research=c(2,3)), stype=c(1,2,6,7),
   scat=NULL, sex=list(Females=2,Males=1), rm.studs=NULL,
   year=NULL, ylim=NULL, ameth=c(3,17), allTT=TRUE, jit=c(0,0), 
   eps=FALSE, jpg=FALSE, pdf=FALSE, png=FALSE, tif=FALSE, wmf=FALSE,
   plotname, singles=FALSE, pages=FALSE, tables=TRUE,
   ioenv=.GlobalEnv, lang=c("e","f"))
}
\arguments{
  \item{dat}{\code{data.frame} -- biological data set with fields \code{yfld} and \code{age}.}
  \item{strSpp}{\code{character} -- string code for species. If dataset has attribute \code{spp},
    this will be used before \code{strSpp}.}
  \item{yfld}{\code{character} -- name of the field in \code{dat} that contains y-values
    for the fit with \code{age}.}
  \item{fixt0}{\code{logical} -- if \code{TRUE}, fix the parameter \code{t0} to equal 0.}
  \item{areas}{\code{numeric} -- list of area codes named by the field in which they may appear in dataset;
    choices can be any/all of \code{major}, \code{minor}, \code{locality}, \code{srfa}, \code{srfs}, \code{popa}.}
  \item{ttype}{\code{numeric} -- list of trip type codes to use from field \code{ttype}.}
  \item{stype}{\code{numeric} -- vector of sample type codes to use from field \code{stype}.}
  \item{scat}{\code{numeric} -- numeric code(s) for species category (usually to identify sorted vs. unsorted samples).}
  \item{sex}{\code{numeric} -- list of sex codes, where \cr
    0=not looked at, 1=male, 2=female, 3=looked at but unknown \cr
    e.g. \code{list(Females=2, Males=1, Unknown=c(0,3))}.}
  \item{rm.studs}{\code{numeric} -- \emph{n}, if specified, remove Studentized residuals lying outside \emph{n} standard deviations.}
  \item{year}{\code{numeric} -- integer vector of years used to subset \code{dat}.}
  \item{ylim}{\code{numeric} -- vector specifying the limits of the y-axis.}
  \item{ameth}{\code{numeric} -- code specifying ageing method protocol -- defaults to \code{3} (break & burn) plus \code{17} (break & bake).}
  \item{allTT}{\code{logical} -- if \code{TRUE}, force the display of all trip types either specified or available.}
  \item{jit}{\code{numeric} -- two-element vector specifying amount of jittering to apply in the 
    x- and -y-directions respectively. (See \code{jitter} in the \pkg{base} package.)}
  \item{eps}{\code{logical} -- if \code{TRUE}, send plot to a postscript \code{.eps} file.}
  \item{jpg}{\code{logical} -- if \code{TRUE}, send plot to a raster \code{.jpg} file.}
  \item{pdf}{\code{logical} -- if \code{TRUE}, send plot to a postscript \code{.pdf} file.}
  \item{png}{\code{logical} -- if \code{TRUE}, send plot to a raster \code{.png} file.}
  \item{tif}{\code{logical} -- if \code{TRUE}, send plot to a raster \code{.tif} file.}
  \item{wmf}{\code{logical} -- if \code{TRUE}, send plot to a vector \code{.wmf} file.}
  \item{plotname}{\code{character} -- explicit image file name to override an internally generated one.}
  \item{singles}{\code{logical} -- if \code{TRUE} and \code{eps|jpg|pdf|png|tif|wmf}, send each area/trip type combination to separate files.}
  \item{pages}{\code{logical} -- if \code{TRUE} and \code{eps|jpg|pdf|png|tif|wmf}, send each page of area plots to separate files.}
  \item{tables}{\code{logical} -- if \code{TRUE} and \code{lang="e"}, download output into \code{.csv} tables.}
  \item{ioenv}{\code{environment} -- input/output environment for function input data and output results.}
  \item{lang}{\code{character} -- a vector of letters that denote the language for output:
    currently only \code{"e"} (english) and "f" (french).}
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
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus: Institute of Ocean Sciences (IOS), Sidney BC}\cr
  Last modified \code{Rd: 2018-07-23}
}
\seealso{
  \pkg{PBStools}:
  \code{\link[PBStools]{calcSG}}, \code{\link[PBStools]{calcLW}},
  \code{\link[PBStools]{getData}}, \code{\link[PBStools]{biteData}} \cr
  \pkg{PBSdata}:
  \code{\link[PBSdata]{pop.age}}, \code{\link[PBSdata]{parVec}},
  \code{\link[PBSdata]{pmfc}}, \code{\link[PBSdata]{species}} \cr
  \pkg{PBSmodelling}:
  \code{\link[PBSmodelling]{calcMin}} \cr
  \pkg{base}: \code{\link[base]{jitter}} \cr
}
\examples{
\dontrun{
local(envir=.PBStoolEnv,expr={
pbsfun=function(){
  data(pop.age,envir=.PBStoolEnv)
  calcVB(areas=list(srfa=c("5AB","5CD")), lang="e")
  invisible() }
pbsfun()
})
}
}
\keyword{hplot}
