\name{compAF}
\alias{compAF}
\title{
  Compare Age Frequencies
}
\description{
  Compare age frequencies between (or among) categories in a user-defined
  group (e.g., group by gear type -- bottom trawl vs. midwater trawl)
  using plots of discrete or cumulative frequency.
}
\usage{
compAF(x, allyrs=2003, sex=2, amax=40, pfld="wp",
   png=FALSE, pngres=400, PIN=c(10,7.5),
   outnam, clrs=c("red","black"), ltys=1, 
   type="cumul", fac="len", lang=c("e","f"))
}
\arguments{
  \item{x}{\code{list} -- list of arrays by group category containing
    age frequency data by age, year, sex, and stats (e.g., number of
    B&B otoliths by age, annual proportion by age, etc.)}
  \item{allyrs}{\code{numeric} -- vector of years to be binned}
  \item{sex}{\code{numeric} -- vector of GFBioSQL sex codes (e.g., 1=males, 2=females)}
  \item{amax}{\code{numeric} -- maximum age to use as a plus class}
  \item{pfld}{\code{character} -- name of field in AF arrays that contains the data for comparison}
  \item{png}{\code{logical} -- if \code{TRUE}, send the figure to a \code{PNG} device}
  \item{pngres}{\code{numeric} -- resolution in pixels per inch for the PNG file}
  \item{PIN}{\code{numeric} -- vector specifying width and height (inches) for PNG}
  \item{outnam}{\code{character} -- name for the output PNG}
  \item{clrs}{\code{character} -- vector of colour names for lines by category}
  \item{ltys}{\code{numeric} -- vector of line types by category}
  \item{type}{\code{character} -- type of line plot: \code{'cumul'} 
    for cumulative or \code{'discr'} for discrete frequency distributuon}
  \item{fac}{\code{character} -- field name in biodata object that contains data
    for comparison (usually \code{'age'} or \code{'len'})}
  \item{lang}{\code{character} -- vector of letters that denote the language for PNG:
    either \code{'e'} (english) and/or \code{'f'} (french)}
}
\details{
  Plot annual age frequencies by group category to compare the
  distribution of ages (or some other discrete variable) between 
  (or among) categories in a group -- e.g., bottom trawl vs. midwater
  trawl gear, unsorted vs. sorted samples.
  Each panel shows a set of years binned; some panels may be missing data.
}
\note{
  The year bin (e.g., 4 years) is determined outside the function when a
  list of data.frames is constructed for input (as \code{x}) to the function.
\preformatted{
ssid   = c(1,3,4,16); surveys=c("QCS","HS","WCVI","WCHG")
sex    = 2:1; fac="len"; fmax=70; yrbin=4
clrs   = c("blue","red","green4","purple"); ltys=c(1,2,4,3)
bioDat = bioDat[is.element(bioDat$SSID,ssid) & is.element(bioDat$sex,sex),]
facDat = bioDat[bioDat[,fac]>0 & !is.na(bioDat[,fac]),]
facDat[,fac] = round(facDat[,fac])
facDat[,fac][facDat[,fac] > fmax] = fmax
years  = min(facDat$year):max(facDat$year)
intvec = rep(1:length(years),each=yrbin)[1:length(years)]
names(intvec) = years
yrvec  = years[1] + (intvec-1) * yrbin
facDat$binyear = yrvec[as.character(facDat$year)]
nyrs   = length(years)
factab = array(0,dim=c(fmax, nyrs, length(sex), 2, length(ssid)), 
dimnames=list(fac=1:fmax, year=years, sex=sex, np=c("n","p"), survey=surveys))
for (i in ssid) {
  ii   = surveys[grep(paste0(i,"$"),ssid)]
  idat = facDat[is.element(facDat$SSID,i),]
  for (s in sex) {
    ss   = as.character(s) #switch(s,"M","F")
    sdat = idat[is.element(idat$sex,s),]
    facyrs  = crossTab(sdat,c(fac,"binyear"),fac,countVec)
    pfacyrs = sweep(facyrs,2,apply(facyrs,2,sum),"/")
    factab[rownames(facyrs),colnames(facyrs),ss,"n",ii] = facyrs
    factab[rownames(facyrs),colnames(facyrs),ss,"p",ii] = pfacyrs
  }
  gfactab = array(0,dim=c(fmax,nyrs,length(sex),2), 
    dimnames=list(fac=1:fmax,year=years,sex=sex,np=c("n","p")))
  temptab = factab[,,,,ii,drop=FALSE]
  gfactab[dimnames(temptab)$fac, dimnames(temptab)$year, dimnames(temptab)$sex,
    dimnames(temptab)$np] = temptab[dimnames(temptab)$fac, dimnames(temptab)$year,
    dimnames(temptab)$sex, dimnames(temptab)$np,ii]
  facTabs[[ii]] = gfactab
}
for (s in sex) {
  ss   = switch(s,"M","F")
  sexname = switch(s,"Males","Females")
  outnam  = paste0("compAF(",fac,")-", strSpp, "-SS-",sexname)
  compAF(x=facTabs, allyrs=years, sex=s, type="cumul", pfld="p", clrs=clrs, 
    ltys=ltys, png=png, outnam=outnam, amax=fmax, PIN=c(9,10), lang=lang, fac=fac)
}
}% end preformatted
}% end note
\value{
  No value returned currently.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2025-06-10}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{estOgive}},
  \code{\link[PBStools]{genPa}},
  \code{\link[PBStools]{plotProp}},
  \code{\link[PBStools]{quantAges}},
  \code{\link[PBStools]{requestAges}},
  \code{\link[PBStools]{weightBio}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}% use one of  RShowDoc("KEYWORDS")
\concept{M06_Assessment}

