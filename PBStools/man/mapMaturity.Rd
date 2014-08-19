\name{mapMaturity}
\alias{mapMaturity}
\title{Map Maturity by Month}
\description{
  Map each maturity code to show its relative occurrence by month.
}
\usage{
mapMaturity(dat=pop.age, strSpp="", type="map",
   mats=1:7, sex=list(Females=2), ttype=1:4, stype=c(1,2,6,7),  major=c(3:9), 
   brks=c(0,.05,.1,.25,.5,1), byrow=FALSE, hpage=6,
   clrs=list(colorRampPalette(c("honeydew","lightgreen","black"))(5),
   colorRampPalette(c("aliceblue","skyblue2","black"))(5)),
   outnam, eps=FALSE, pix=FALSE, wmf=FALSE, ioenv=.GlobalEnv)
}
\arguments{
  \item{dat}{biological data set with fields \code{mat} specifying maturity code.}
  \item{strSpp}{string code for species. If dataset has attribute \code{spp},
    this will be used before \code{strSpp}.}
  \item{type}{character string specifying type of plot to produce: \cr
    \code{"map"} -- for original colour-coded tiles, and \cr
    \code{"bubb"} -- for bubbles proportional to relative frequency (PJS); \cr
    both essentially represent a frequency matrix of maturity code (rows) by month (columns). }
  \item{mats}{maturity codes to map (defaults to 1:7).}
  \item{sex}{list of sex codes categorised by name, where codes: \cr
    0=not looked at, 1=male, 2=female, 3=looked at but unknown \cr
    e.g. \code{list(`Females`=2, `Males`=1)}.}
  \item{ttype}{trip type code, where \cr
    1=non-observed commercial, 2=research, 3=survey, 4=observed domestic \cr
    e.g. \code{list(Commercial=c(1,4), Research=c(2:3))}}
  \item{stype}{sample type code, where \cr
    0=unknown, 1=total catch, 2=random, 4=selected,
    5=stratified, 6=random from randomly assigned set, 7=random from set after randomly assigned set,
    8=random from set requested by vessel master, 9=selected juveniles, and 10=selected adults.}
  \item{major}{major PMFC area codes, where \cr
    3=3C, 4=3D, 5=5A, 6=5B, 7=5C, 8=5D, and 9=5E.}
  \item{brks}{relative breaks from 0 to 1 to partition the maturity data (only used for \code{type="map")}).}
  \item{byrow}{logical: if \code{TRUE}, the observed monthly frequencies are transformed to proportions by maturity code; 
    if \code{FALSE}, the observed maturity frequencies are transformed to proportions by month.}
  \item{hpage}{height of page in inches if figure is sent to an image file.}
  \item{clrs}{list of colour vectors that match the \code{sex} list (above). 
    If \code{type="map"}, the colour vectors should match the relative intervals defined by \code{brks}.
    \emph{This argument needs some work to accommodate the newly added "bubb" type.}}
  \item{outnam}{explicit output name to override an internally generated one.}
  \item{eps}{logical: if \code{TRUE}, send plot to an \code{.eps} file.}
  \item{pix}{logical: if \code{TRUE}, send plot to a \code{.png} file.}
  \item{wmf}{logical: if \code{TRUE}, send plot to a \code{.wmf} file.}
  \item{ioenv}{input/output environment for function input data and output results.}
}
\details{
  If \code{byrow=TRUE} (original default configuration), for each maturity code the function plots
  a row of monthly boxes (colour-coded) or bubbles by the relative frequency of the code
  in that month compared to the year.
  
  Alternatively, if \code{byrow=FALSE} (current default), the function plots a column of 
  monthly boxes (colour-coded) or bubbles by the relative frequency of the code in each month.
}
\value{
  A list object \code{CALCS} is invisibly returned and contains the frequency matrix 
  (maturity by month) for each sex configuration specified.
  
  A global list object \code{PBStool} stored in the environment \code{.PBStoolEnv} also  provides the following:
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
  A suitable input dataset can be obtained by running the SQL query \code{gfb_bio.sql}.

  \code{getData("gfb_bio.sql","GFBioSQL",strSpp="396",path=.getSpath())}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(){
  data(pop.age,envir=.PBStoolEnv)
  mapMaturity(brks=c(0,.01,.05,.25,.5,1),sex=list(Males=1,Females=2))
  invisible() }
pbsfun()
})
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link{pop.age}}, \code{\link{estOgive}}, \code{\link{getData}}
}
\keyword{hplot}
