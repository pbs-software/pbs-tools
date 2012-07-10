\name{mapMaturity}
\alias{mapMaturity}
\title{Map Maturity by Month}
\description{
  Map each maturity code to show its relative occurrence by month.
}
\usage{
mapMaturity(dat=pop.age, strSpp="", mats=1:7, sex=1:2, 
    brks=c(0,.05,.1,.25,.5,1),
    clrs=c("aliceblue","lightblue","skyblue3","steelblue4","black"),
    hpage=6, wmf=FALSE, pix=FALSE) 
}
\arguments{
  \item{dat}{biological data set with fields \code{mat} and \code{age}.}
  \item{strSpp}{string code for species. If dataset has attribute \code{spp},
    this will be used before \code{strSpp}.}
  \item{mats}{maturity codes to map.}
  \item{sex}{sex code: 1 = males, 2 = females.}
  \item{brks}{relative breaks from 0 to 1 to partition the maturity data.}
  \item{clrs}{colours to use for relative intervals defined by \code{brks}.}
  \item{hpage}{height of page in inches if figure is sent to an image file.}
  \item{wmf}{logical: if \code{TRUE}, send plot to a \code{.wmf} file.}
  \item{pix}{logical: if \code{TRUE}, send plot to a \code{.png} file.}
}
\details{
  For each maturity code the function plots a row of monthly boxes colour-
  coded by the relative occurrence of the code in that month compared to the year.
}
\value{
  No value is explictly returned by the function. A global list object \code{PBSfish}
  provides the following:
  \item{dat}{Qualified data used for the analysis.}
  \item{xlim}{Limits of the x-axis.}
  \item{ylim}{Limits of the y-axis.}
  \item{x}{Current value of x.}
  \item{y}{Current value of y.}
  \item{sdat}{Last subset of \code{dat}.}
  \item{mday}{Number of days in each month.}
  \item{mcut}{Cut points in days for months in a year from 0 to 365.}
  \item{idat}{Last subset of \code{sdat}.}
  \item{ibin}{Days of the month (observations) occuring in each month.}
  \item{icnt}{Relative frequency of observations in each month.}
  \item{iclr}{Colour code assigned to each month.}
}
\note{
  A suitable data set can be obtained by running the SQL query \code{gfb_bio.sql}.

  \code{getData("gfb_bio.sql","GFBioSQL","396",path=.getSpath())}
}
\examples{
pbsfun=function(){
  data(pop.age)
  mapMaturity(brks=c(0,.01,.05,.25,.5,1),sex=1:2)
  invisible() }
pbsfun()
}
\seealso{
  \code{\link{pop.age}}, \code{\link{estOgive}}, \code{\link{getData}}
}
\keyword{hplot}
