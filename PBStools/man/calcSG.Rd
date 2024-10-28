\name{calcSG}
\alias{calcSG}
\title{Calculate Fit using a Schnute Growth Model}
\description{
  Calculate the length or weight \emph{vs.} age relationship by fitting the 
  data using a versatile Schnute growth model.
}
\usage{
calcSG(dat=pop.age, strSpp="", yfld="len", tau=c(5,80), fixt0=FALSE,
   areas=list(major=NULL, minor=NULL, locality=NULL, srfa=NULL,srfs=NULL, popa=NULL),
   ttype=list(commercial=c(1,4,5),research=c(2,3)), stype=c(1,2,6,7), scat=NULL,
   sex=list(Females=2,Males=1), rm.studs=NULL,
   year=NULL, xlim=NULL, ylim=NULL, ameth=c(3,17), jit=c(0,0), 
   eps=FALSE, jpg=FALSE, pdf=FALSE, png=FALSE, tif=FALSE, wmf=FALSE,
   plotname, singles=FALSE, pages=FALSE, tables=TRUE, figures=TRUE,
   ioenv=.GlobalEnv, lang=c("e","f"))
}
\arguments{
  \item{dat}{\code{data.frame} -- biological data set with fields \code{yfld} and \code{age}.}
  \item{strSpp}{\code{character} -- string code for species. If dataset has attribute \code{spp},
    this will be used before \code{strSpp}.}
  \item{yfld}{\code{character} -- name of the field in \code{dat} that contains y-values
    for the fit with \code{age}.}
  \item{tau}{\code{numeric} -- vector specifying two fixed parameters: the age of a young fish and that of an old fish.}
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
  \item{xlim}{\code{numeric} -- vector specifying the limits of the x-axis.}
  \item{ylim}{\code{numeric} -- vector specifying the limits of the y-axis.}
  \item{ameth}{\code{numeric} -- code specifying ageing method protocol -- defaults to \code{3} (break & burn) plus \code{17} (break & bake).}
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
  \item{figures}{\code{logical} -- if \code{TRUE}, plot the figures.}
  \item{ioenv}{\code{environment} -- input/output environment for function input data and output results.}
  \item{lang}{\code{character} -- a vector of letters that denote the language for output:
    currently only \code{"e"} (english) and \code{"f"} (french).}
}
\details{
  The function plots versatile Schnute fits to length-age or weight-age data where each column 
  shows the fits for Males, Females, and M+F. If the data are plotted to the 
  screen device, each row shows the fits by trip type, and each page shows the 
  fits by area. If plots are sent to image files (\code{.png} or \code{.wmf}),
  each trip type and area combination goes to separate image files (handy for
  stacking in a \code{.doc} file).
  
  The versatile Schnute fit requires initial parameter estimates and bounds. These 
  are taken from the data object \code{parVec}. If the species code you are 
  using is not contained in this file, the function uses the initial estimates for 
  Pacific Ocean Perch (\emph{Sebastes alutus}).
}
\value{
  No value is explictly returned by the function. A global list object \code{PBStool}
  provides the following plus some labels:
  \item{out }{Output array of model parameter estimates and other metrics.}
  \item{pVec }{The initial parameter vector used for the model.}
  \item{xlim }{Limits of the x-axis.}
  \item{ylim }{Limits of the y-axis.}
  \item{fits }{List of x- and y-values that describe the versatile Schnute fits to the data.}
  
  The fits for females, males, and sexes combined (M+F) are also written to a 
  comma-separated \code{.csv} file using a name that includes areas, trip types, 
  and years (if specified).
}
\note{
  A suitable data set can be obtained by running the SQL query \code{gfb_bio.sql}.

  \code{getData("gfb_bio.sql","GFBioSQL","396",path=.getSpath())}
}
\references{
  Schnute, J. (1981) A versatile growth model with statistically stable parameters. 
  \emph{Canadian Journal of Fisheries and Aquatic Sciences} \bold{38}: 1128--1140.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-10-28}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{calcVB}},
  \code{\link[PBStools]{getData}}, 
  \code{\link[PBStools]{biteData}}\cr
In package \pkg{PBSdata}:\cr
  \code{\link[PBSdata]{parVec}}, 
  \code{\link[PBSdata]{pmfc}},
  \code{\link[PBSdata]{pop.age}}, 
  \code{\link[PBSdata]{species}} \cr
In package \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{calcMin}} \cr
In package \pkg{base}:\cr
  \code{\link[base]{jitter}}
}
\keyword{hplot}
\concept{M02_Biology}
