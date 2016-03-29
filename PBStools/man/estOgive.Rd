\name{estOgive}
\alias{estOgive}
\title{Estimate Ogive Curves for Maturity}
\description{
  Estimate ogive curves using three methods: \cr
  (i) calculate the cumulative distribution of the proportion of mature fish smoothed by binned ages, \cr
  (ii) use a \code{glm} fit with binomial error linked to a logit function, and \cr
  (iii) fit proportion-at-age data to a double normal curve using the minimizer \code{nlm}.
}
\usage{
estOgive(dat=pop.age, strSpp="", method=c("dblnorm"),
   sex=list(Females=2,Males=1), mos=list(1:12,1:12),
   mat=3:7, ameth=3, amod=NULL, azero=NULL,
   ttype=list(Commercial=c(1,4),Research=c(2:3)), stype=c(1,2,6,7), scat=1, 
   SSID=NULL, surveys=NULL,
   ofld="age", obin=1, xlim=c(0,45), figdim=c(8,5),
   plines=TRUE, ppoints=TRUE, rpoints=FALSE, rtext=FALSE,
   fg=c("red","orange2","blue","green4"), Arcs=NULL, radius=0.2,
   parList = list(val=c(15,0.9,exp(100)),min=c(5,0.1,exp(10)),
   max=c(60,1000,exp(150)),active=c(TRUE,TRUE,FALSE)),
   outnam, eps=FALSE, png=FALSE, wmf=FALSE, ioenv=.GlobalEnv, ...)
}
\arguments{
  \item{dat}{biological data set with fields \code{mat} and \code{age}.}
  \item{strSpp}{string code for species. If dataset has attribute \code{spp},
    this will be used before \code{strSpp}.}
  \item{method}{string vector specifying methods used to fit the data: \cr
    \code{"empir"}   - empirical fit to cumulative curves of proportion maturity (only when specified alone), \cr
    \code{"logit"}   - binomial logit fit where maturity is specified as 0 or 1 for each specimen, \cr
    \code{"dblnorm"} - fit proportion-at-ages using a double normal curve.}
  \item{sex}{list of sex codes, where \cr
    0=not looked at, 1=male, 2=female, 3=looked at but unknown \cr
    e.g. \code{list(`Females`=2, `Males`=1, `Unknown`=c(0,3))}.}
  \item{mos}{list of months from which maturity data are used; 
    list length matches that of list argument \code{sex}.}
  \item{mat}{codes that define maturity (e.g., \code{3:7}).}
  \item{ameth}{ageing method code (e.g., 3 = break and burn).}
  \item{amod}{first age when model-derived proportions-mature are used in an assessment
    (use empirical proportions-mature prior to \code{amod}).}
  \item{azero}{ages for assessment where proportions-mature should be set to 0
    (also forces anomalous empirical values to zero).}
  \item{ttype}{trip type code, where \cr
    1=non-observed commercial, 2=research, 3=survey, 4=observed domestic \cr
    e.g. \code{list(Commercial=c(1,4), Research=c(2:3))}}
  \item{stype}{sample type code, where \cr
    0=unknown, 1=total catch, 2=random, 4=selected,
    5=stratified, 6=random from randomly assigned set, 7=random from set after randomly assigned set,
    8=random from set requested by vessel master, 9=selected juveniles, and 10=selected adults.}
  \item{SSID}{Survey Series ID (55+ at last count), where \cr
    1 = Queen Charlotte Sound Synoptic Survey, \cr
    2 = Hecate Strait Multispecies Assemblage Survey, \cr
    3 = Hecate Strait Synoptic Survey, \cr
    4 = West Coast Vancouver Island Synoptic Survey, \cr
    16 = West Coast Haida Gwaii Synoptic Survey, \cr
    e.g. \code{SSID=list(`QCS Synoptic`=1, `WCVI Synoptic`=4)}}
  \item{scat}{Species category code in table \code{"SPECIES_CATEGORY"}; code for unsorted fish: 1= \code{"UNSORTED"}.}
  \item{surveys}{text to add to legend if multiple surveys combined into one category.}
  \item{ofld}{ogive field (usually \code{age} or \code{len}).}
  \item{obin}{bin size (in x-units) to group \code{age} or \code{len}.}
  \item{xlim}{range of the x-metric (x-axis).}
  \item{figdim}{dimensions (width and height in inches) of output figure.}
  \item{plines}{logical: if \code{TRUE}, add the predicted curve.}
  \item{ppoints}{logical: if \code{TRUE}, add predicted points along predicted curve at integer ages.}
  \item{rpoints}{logical: if \code{TRUE}, add observed points (proportions mature at age).}
  \item{rtext}{logical: if \code{TRUE}, label observed points with number of observations.}
  \item{fg}{vector of foreground colours for sexes, trip types, and SSIDs; colours are cycled if short.}
  \item{Arcs}{degrees (0-360) away from age-at-50\% maturity to place label flags.}
  \item{radius}{length of the label flag poles}
  \item{parList}{initial vectors to construct a \code{parVec} --  (val, min, max, active).}
  \item{outnam}{explicit output name to override an internally generated one.}
  \item{eps}{logical: if \code{TRUE}, send plot to a postscript \code{.eps} file.}
  \item{png}{logical: if \code{TRUE}, send plot to a portable network graphics \code{.png} file.}
  \item{wmf}{logical: if \code{TRUE}, send plot to a vector \code{.wmf} file.}
  \item{ioenv}{input/output environment for function input data and output results.}
  \item{...}{additional arguments for use in \code{text} within \code{flagIt} called by \code{doLab}.}
}
\details{
  The cumulative distribution of mature fish is plotted and the point at 
  50\% maturity is interpolated. The x-axis (age or length) of empirical data
  can be binned to smooth out rough or sparse data. The user can also choose two
  methods to fit empirical data: \code{glm} fit of binary data (mature vs. not mature)
  using a linked logit function, or an \code{nlm} fit of the proportions-at-age data
  to a double normal curve (which is the default method used in Coleraine).
}
\value{
  An array of fitted values by \code{bin}, \code{val} (incl. three methods), \code{sex}, and 
  \code{sub} (e.g., trip type, SSID) is invisibly returned by the function. 
  This array is also dumped to a CSV file, as are the parameter estimates (to a PAR file).
  
  Additionally, a global list object \code{PBStool} (in the environment \code{.PBStoolEnv})
  provides the following:
  \item{pmat }{Function calculating proportions mature.}
  \item{DATA}{List object of data by sex as data frames.}
  \item{CALCS}{List object of data by sex and subset (trip type, SSID) giving 
    \code{mbin} (maturities binned by age or length) and \code{p50}
    (age-at-50\% maturity) for three methods -- empirical, binomial, and double normal.}
  \item{out}{Results table showing proportions mature at each binned age or length (also invisibly returned).}
  \item{pend}{List object of parameter estimates from the fit to the double normal method --
    rendered as a named vector of  \code{mu}, \code{nuL}, \code{nuR}, and \code{p50} --
    by sex and subset (trip type, SSID).}
  \item{strSpp}{String species code used in the estimation.}
}
\note{
  A suitable data set can be obtained by running the SQL query \code{gfb_bio.sql}.

  \code{getData("gfb_bio.sql","GFBioSQL",strSpp="396",path=.getSpath())}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(){
  data(pop.age,envir=.PBStoolEnv)
  estOgive(obin=2, xlim=c(0,30), sex=list(Females=2), method=c("empir","dblnorm"),
    fg=.colBlind[c("blue","orange")], Arcs=c(325,120))
  invisible() }
pbsfun()
})
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link[PBSdata]{pop.age}}, \code{\link[PBStools]{fitLogit}}, 
  \code{\link[PBStools]{mapMaturity}}, \code{\link[PBStools]{flagIt}}
}
\keyword{hplot}

