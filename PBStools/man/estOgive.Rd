\name{estOgive}
\alias{estOgive}
\title{Estimate Ogive Curves for Maturity}
\description{
  Estimate ogive curves using four methods: \cr
  (i) calculate the cumulative distribution of the proportion of mature fish smoothed by binned ages, \cr
  (ii) use a \code{glm} fit with binomial error linked to a logit function, \cr
  (iii) fit proportion-mature by age data with a logistic model, and \cr
  (iv) fit proportion-mature by age data to a double normal curve.
}
\usage{
estOgive(dat=pop.age, strSpp="", method=c("DN"),
   sex=list(Females=2,Males=1), years, mos=list(1:12,1:12), 
   mat=3:7, ameth=c(3,17), amod=NULL, azero=NULL,
   ttype=list(Commercial=c(1,4,5),Research=c(2:3)), 
   stype=c(1,2,6,7), scat = list(Unsorted=1), 
   SSID=NULL, surveys=NULL, subset="SSID",
   ofld="age", obin=1, xlim=c(0,45), PIN=c(8,5),
   mpoints=TRUE, ppoints=TRUE, plines=TRUE,
   rpoints=FALSE, rlines=FALSE, rtext=FALSE,
   fg=c("red","orange2","blue","green4"), Arcs=NULL, radius=0.2,
   parList=list(DN=list(val=c(15,2.5,100), min=c(5,0.01,10),
   max=c(60,10,150)), active=c(TRUE,TRUE,FALSE)), tabs=FALSE,
   outnam, eps=FALSE, png=FALSE, wmf=FALSE, ioenv=.GlobalEnv, lang=c("e","f"), ...)
}
\arguments{
  \item{dat}{\code{data.frame} -- biological data set with fields \code{mat} and \code{age}}
  \item{strSpp}{\code{character} -- string code for species. If dataset has attribute \code{spp},
    this will be used before \code{strSpp}}
  \item{method}{\code{character} -- string vector specifying methods used to fit the data: \cr
    \code{"EMP"}   - empirical fit to cumulative curves of proportion maturity (only when specified alone), \cr
    \code{"BL"}   - binomial logit fit where maturity is specified as 0 or 1 for each specimen, \cr
    \code{"VH"}   - Vivian Haist logistic fit estimating parameters \code{'mat50'} = size/age at 50\% maturity
      and \code{'mat95add'} = increment to get to 95\% maturity, \cr
    \code{"DN"} - fit proportion-at-ages using a double normal curve}
  \item{sex}{\code{numeric list} -- list of sex codes, where \cr
    0=not looked at, 1=male, 2=female, 3=looked at but unknown \cr
    e.g. \code{list(`Females`=2, `Males`=1, `Unknown`=c(0,3))}}
  \item{years}{\code{numeric} -- vector of years for which maturity data are used}
  \item{mos}{\code{numeric list} -- list of months from which maturity data are used; 
    list length matches that of list argument \code{sex}}
  \item{mat}{\code{numeric} -- codes that define maturity (e.g., \code{3:7})}
  \item{ameth}{\code{numeric} -- ageing method code (e.g., 3 = break and burn)}
  \item{amod}{\code{numeric} -- first age when model-derived proportions-mature are used in an assessment
    (use empirical proportions-mature prior to \code{amod})}
  \item{azero}{\code{numeric} -- ages for assessment where proportions-mature should be set to 0
    (also forces anomalous empirical values to zero)}
  \item{ttype}{\code{numeric list} -- trip type code, where \cr
    1=non-observed commercial, 2=research, 3=survey, 4=observed domestic \cr
    e.g. \code{list(Commercial=c(1,4), Research=c(2:3))}}
  \item{stype}{\code{numeric} -- sample type code, where \cr
    0=unknown, 1=total catch, 2=random, 4=selected,
    5=stratified, 6=random from randomly assigned set, 7=random from set after randomly assigned set,
    8=random from set requested by vessel master, 9=selected juveniles, and 10=selected adults}
  \item{scat}{\code{numeric list} -- list of species category codes (\code{GFBio}'s \code{"SPECIES_CATEGORY"} table),\cr
    e.g. \code{list(Unsorted=1, Keepers=3)}}
  \item{SSID}{\code{numeric list} -- Survey Series ID (71 as of 2018-12-21), where \cr
    1 = Queen Charlotte Sound Synoptic Survey, \cr
    2 = Hecate Strait Multispecies Assemblage Survey, \cr
    3 = Hecate Strait Synoptic Survey, \cr
    4 = West Coast Vancouver Island Synoptic Survey, \cr
    16 = West Coast Haida Gwaii Synoptic Survey, \cr
    e.g. \code{SSID=list(`QCS Synoptic`=1, `WCVI Synoptic`=4)}}
  \item{surveys}{\code{character} -- text to add to legend if multiple surveys combined into one category}
  \item{subset}{\code{character} -- field to use if subsetting each sex (e.g, by \code{SSID})}
  \item{ofld}{\code{character} -- ogive field (usually \code{age} or \code{len})}
  \item{obin}{\code{numeric} -- bin size (in x-units) to group \code{age} or \code{len}}
  \item{xlim}{\code{numeric} -- range of the x-metric (x-axis)}
  \item{PIN}{\code{numeric} -- dimensions (width and height in inches) of output figure}
  \item{mpoints}{\code{logical} -- if \code{TRUE}, add points used in population model at integer ages}
  \item{ppoints}{\code{logical} -- if \code{TRUE}, add predicted points along predicted curve at integer ages}
  \item{plines}{\code{logical} -- if \code{TRUE}, add the predicted curve; 
    if \code{FALSE} and \code{method="dblnorm"} then save the points from the predicted curve in a list object \code{CALCS}}
  \item{rpoints}{\code{logical} -- if \code{TRUE}, add observed points (proportions mature at age)}
  \item{rlines}{\code{logical} -- if \code{TRUE}, add lines connecting the the empirical (raw) points}
  \item{rtext}{\code{logical} -- if \code{TRUE}, label observed points with number of observations}
  \item{fg}{\code{character} -- vector of foreground colours for sexes, trip types, and SSIDs; colours are cycled if short}
  \item{Arcs}{\code{numeric} -- degrees (0-360) away from age-at-50\% maturity to place label flags}
  \item{radius}{\code{numeric} -- length of the label flag poles}
  \item{parList}{\code{data.frame} -- initial vectors to construct a \code{parVec} --  (val, min, max, active)}
  \item{tabs}{\code{logical} -- if \code{TRUE}, create tables of output}
  \item{outnam}{\code{character} -- explicit output name to override an internally generated one}
  \item{eps}{\code{logical} -- if \code{TRUE}, send plot to a postscript \code{.eps} file}
  \item{png}{\code{logical} -- if \code{TRUE}, send plot to a portable network graphics \code{.png} file}
  \item{wmf}{\code{logical} -- if \code{TRUE}, send plot to a vector \code{.wmf} file}
  \item{ioenv}{\code{environment} -- input/output environment for function input data and output results}
  \item{lang}{\code{character} -- a vector of letters that denote the language for output:
    currently only \code{"e"} (english) and \code{"f"} (french)}
  \item{...}{additional arguments for use in \code{text} within \code{flagIt} called by \code{doLab}}
}
\details{
  The cumulative distribution of mature fish is plotted and the point at 
  50\% maturity is interpolated.
  The x-axis (age or length) of empirical data can be binned to smooth out
  rough or sparse data.
  The user can also choose three methods to fit data:
  \enumerate{
    \item fit raw binary data (specimen mature or not) using a 
    \code{\link[stats]{glm}} function with model error distribution
    from the \code{binomial} family with a \code{logit} link;
    \item fit observed proportions-at-age data using a logistic function
    with two parameters: (i) \code{'mat50'} = age at 50\% maturity and 
    (ii) \code{mat95add} = an incremental age from \code{mat50} that 
    achieves age at 95\% maturity -- used in NZ Rock Lobster;
    \item fit observed proportions-at-age data to a double normal curve,
    which is the default method used in Coleraine, using three parameters:
    (i) \code{'mu'} = age at full maturity, (ii) \code{'nuL'} = variance 
    of left-side normal, and (iii) \code{'nuR'} = variance of right-side
    normal (fixed to a high value to ensure 100\% maturity older than \code{'mu'}).
  }
  The latter two fits use the \code{PBSmodelling} function \code{\link[PBSmodelling]{calcMin}} to perform the minimization.
}
\value{
  An array of fitted values by \code{bin}, \code{val} (incl. three methods), \code{sex}, and 
  \code{sub} (e.g., trip type, SSID) is invisibly returned by the function. 
  This array is also dumped to a CSV file, as are the parameter estimates (to a PAR file).
  
  Additionally, a global list object \code{PBStool} (in the environment \code{.PBStoolEnv})
  provides the following:
  \item{pmat }{Function calculating proportions mature.}
  \item{DATA}{List object of data by sex as data frames.}
  \item{CALCS}{List object of data by sex and subset (trip type, SSID) giving: 
    \code{mbin} (maturities binned by age or length), \code{p50}
    (age-at-50\% maturity) for three methods -- empirical, binomial, and double normal, and 
    plotting information for the double normal when \code{plines=FALSE} (# records, 
    parameter fits, predicted curve coordinates, and proportions-mature at each age).}
  \item{out}{Results table showing proportions mature at each binned age or length (also invisibly returned).}
  \item{pend}{List object of parameter estimates from the fit to the double normal method --
    rendered as a named vector of  \code{mu}, \code{nuL}, \code{nuR}, and \code{p50} --
    by sex and subset (trip type, SSID).}
  \item{strSpp}{String species code used in the estimation.}
}
\note{
  A suitable data set can be obtained by running the SQL query \code{gfb_bio.sql}:\cr
  \code{getData("gfb_bio.sql","GFBioSQL",strSpp="396",path=.getSpath())}

  Available survey series can be retrieved to \code{PBSdat} using:\cr
  \code{getData("SURVEY_SERIES","GFBioSQL")}
}
\examples{
\dontrun{
local(envir=.PBStoolEnv,expr={
  data(pop.age,envir=.PBStoolEnv)
  parList = list()
  parList[["DN"]] = list(val=c(20,20,exp(100)), min=c(5,5,exp(10)),
    max=c(70,200,exp(150)),active=c(TRUE,TRUE,FALSE))
  parList[["VH"]] = list(val=c(20,10),min=c(5,1), max=c(30,50), active=c(TRUE,TRUE))
  estOgive(pop.age, xlim=c(0,40), sex=list(Fem=2), method=c("DN","EMP"),
    ttype=list(Comm=c(1,4,5),Surv=c(2:3)), parList=parList, 
    ppoints=FALSE, fg=.colBlind[c("blue","orange")], Arcs=c(325,120), lang="e")
  invisible()
})
}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-29}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{fitLogit}},
  \code{\link[PBStools]{flagIt}},
  \code{\link[PBStools]{mapMaturity}}\cr\cr
In package \pkg{PBSdata}:\cr
  \code{\link[PBSdata]{pop.age}}
}
\keyword{hplot}
\concept{M02_Biology}
