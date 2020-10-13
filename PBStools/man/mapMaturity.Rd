\name{mapMaturity}
\alias{mapMaturity}
\title{Map Maturity by Month}
\description{
  Map each maturity code to show its relative occurrence by month.
}
\usage{
mapMaturity(dat=pop.age, strSpp="", type="map", mats=1:7,
   sex=list(Females=2), ttype=1:5, stype=c(1,2,6,7), areas=list(major=3:9),
   stock, catch, brks=c(0,0.05,0.1,0.25,0.5,1), byrow=FALSE,
   clrs=list(colorRampPalette(c("honeydew","lightgreen","black"))(5),
   colorRampPalette(c("aliceblue","skyblue2","black"))(5)),
   outnam, eps=FALSE, png=FALSE, wmf=FALSE, pngres=400, PIN=c(8.5,6),
   ioenv=.GlobalEnv, lang=c("e","f"))
}

\arguments{
  \item{dat}{\code{data.frame} -- biological data set with field \code{"mat"} specifying maturity code.}
  \item{strSpp}{\code{character} -- string code for species. If dataset has attribute \code{spp},
    this will be used before \code{strSpp}.}
  \item{type}{\code{character} -- string specifying type of plot to produce: \cr
    \code{"map"} -- for original colour-coded tiles, and \cr
    \code{"bubb"} -- for bubbles proportional to relative frequency (PJS); \cr
    both essentially represent a frequency matrix of maturity code (rows) by month (columns). }
  \item{mats}{\code{numeric} -- maturity codes to map (defaults to 1:7).
    If \code{mats=NULL}, then function switches to representing sex proportions by month and area.}
  \item{sex}{\code{numeric list} -- sex codes categorised by name, where codes: \cr
    0=not looked at, 1=male, 2=female, 3=looked at but unknown \cr
    e.g. \code{list(`Females`=2, `Males`=1)}.}
  \item{ttype}{\code{numeric} -- trip type code, where \cr
    1=non-observed commercial, 2=research, 3=survey, 4=observed domestic \cr
    e.g. \code{list(Commercial=c(1,4), Research=c(2:3))}}
  \item{stype}{\code{numeric} -- sample type code, where \cr
    0=unknown, 1=total catch, 2=random, 4=selected,
    5=stratified, 6=random from randomly assigned set, 7=random from set after randomly assigned set,
    8=random from set requested by vessel master, 9=selected juveniles, and 10=selected adults.}
  \item{areas}{\code{numeric list} -- areas available in the data object, for example:\cr
    \code{list(major=9, major=5:8, major=3:4, region="shelf", region="slope")}}
  \item{stock}{\code{character} -- stock label, usually a combination of PMFC \code{major} and \code{minor} areas specific to a species.
    If this argument is specified, it precludes any specification of \code{major}
    and assumes that a field called \code{"stock"} appears in the data file.}
  \item{catch}{\code{data.frame} -- catch dataset that matches the biology data (\code{dat}) and 
    contains a minimum of two fields named: \code{c("month", "catKg")}.}
  \item{brks}{\code{numeric} -- relative breaks from 0 to 1 to partition the maturity data (only used for \code{type="map"}).}
  \item{byrow}{\code{logical} -- if \code{TRUE}, the observed monthly frequencies are transformed to proportions by maturity code; 
    if \code{FALSE}, the observed maturity frequencies are transformed to proportions by month.}
  \item{clrs}{\code{character} -- list of colour vectors that match the \code{sex} list (above). 
    If \code{type="map"}, the colour vectors should match the relative intervals defined by \code{brks}.
    \emph{This argument needs some work to accommodate the newly added "bubb" type.}}
  \item{outnam}{\code{character} -- explicit output name to override an internally generated one.}
  \item{eps}{\code{logical} -- if \code{TRUE}, send plot to a postscript \code{.eps} file.}
  \item{png}{\code{logical} -- if \code{TRUE}, send plot to a raster \code{.png} file.}
  \item{wmf}{\code{logical} -- if \code{TRUE}, send plot to a vector \code{.wmf} file.}
  \item{pngres}{\code{numeric} -- resolution (pixels/inch) of \code{".png"} file.}
  \item{PIN}{\code{numeric} -- vector of width and height (inches) for the output \code{png} file.}
  \item{ioenv}{\code{environment} -- input/output environment for function input data and output results.}
  \item{lang}{\code{character} -- a vector of letters that denote the language for output:
    currently only \code{"e"} (english) and \code{"f"} (french).}
}
\details{
  If \code{byrow=TRUE}, the function plots a row of bubbles (variable size)
  or monthly boxes (colour-coded) using the relative frequency of the maturity
  code in each month compared to the year.
  
  Alternatively, if \code{byrow=FALSE} (current default), the function
  plots a column of bubbles (variable size) or monthly boxes (colour-coded)
  using the relative frequency of the maturity code by month, regardless of other months.
  
  If a data frame \code{"catch"} is supplied, the function uses it to weight the
  maturity observations by the relative monthly catch (over the year).
  In this case, neither the rows nor the columns are converted to proportions.
  
  If \code{mats=NULL}, then function switches from displaying maturities to 
  proportions of sex (female and/or male) by area and month.
  In this case, neither the rows nor the columns are converted to proportions
  becasue they are already representative proportions that should not be re-scaled.
}
\value{
  A list object \code{CALCS} is invisibly returned and contains the frequency matrix 
  (maturity by month) for each sex configuration specified.
  
  A global list object \code{PBStool} stored in the environment \code{.PBStoolEnv}
  also  provides the following:
  \item{dat}{Qualified data used for the analysis.}
  \item{xlim}{Limits of the x-axis.}
  \item{ylim}{Limits of the y-axis.}
  \item{x}{Current value of x.}
  \item{y}{Current value of y.}
  \item{sdat}{Last subset of \code{dat}.}
  \item{mday}{Number of days in each month.}
  \item{mcut}{Cut points in days for months in a year from 0 to 365.}
  \item{idat}{Last subset of \code{sdat}.}
  \item{ibin}{Days of the month (observations) occurring in each month.}
  \item{icnt}{Relative frequency of observations in each month.}
  \item{iclr}{Colour code assigned to each month.}
}
\note{
  A suitable biological dataset for input can be obtained by running
  the SQL query \code{gfb_bio.sql}.

\preformatted{
getData("gfb_bio.sql","GFBioSQL",strSpp="396",path=.getSpath())
bio396 = processBio(PBSdat, strSpp="396")
}

  A suitable catch dataset (e.g., POP trawl) for input can be obtained by running
  the SQL query \code{fos_mcatORF.sql}.

\preformatted{
getData("gfb_mcatORF.sql","GFBioSQL",strSpp="396",path=.getSpath())
cat396 = PBSdat[is.element(PBSdat$fid, 1),]           ## trawl fishery
cat396 = cat396[is.element(cat396$gear, c(1,3)),]     ## bottom & midwater
cat396 = cat396[cat396$landed>0 | cat396$discard>0,]  ## positive catch
cat396$year  = as.numeric(substring(cat396$date,1,4))
cat396$month = as.numeric(substring(cat396$date,6,7))
cat396$catKg = cat396$landed + cat396$discard
}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(){
  data(pop.age,package="PBSdata",envir=.PBStoolEnv)
  mapMaturity(pop.age, sex=list(Males=1,Females=2), lang="e")
  invisible() }
pbsfun()
})
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2020-10-08}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{crossTab}},
  \code{\link[PBStools]{estOgive}},
  \code{\link[PBStools]{getData}},
  \code{\link[PBStools]{plotAgeErr}},
  \code{\link[PBStools]{processBio}},
  \code{\link[PBStools]{weightBio}}\cr
In package \pkg{PBStools}:\cr
  \code{\link[PBSdata]{pop.age}}
}
\keyword{hplot}
