\name{histMetric}
\alias{histMetric}
\title{Plot Matrix of Histograms for Metric}
\description{
  Plot a matrix of histograms or cumulative frequencies for a 
  specified biology metric by year and trip type.
}
\usage{
histMetric(dat=pop.age, xfld="age", xint=1, minN=50,
  ttype=1:4, year=NULL, plus=NULL, ptype="bars", allYR=FALSE, allTT=FALSE,
  major=NULL, minor=NULL, locality=NULL, srfa=NULL, srfs=NULL,
  xlim=NULL, ylim=NULL, pxlab=c(.075,.85),axes=TRUE,
  fill=FALSE, bg="grey90", fg="black", 
  hrow=0.75, hpage=8, wmf=FALSE, pix=FALSE)
}
\arguments{
  \item{dat}{data file of biology metrics (e.g., \code{pop.age}).}
  \item{xfld}{string specifying the field containing the metric of interest.}
  \item{xint}{x-interval specfying the bar width. }
  \item{minN}{minimum number of specimens per matrix cell to display histogram.}
  \item{ttype}{explicit trip type codes: \code{1} = non-observed domestic commercial, 
    \code{2} = research, \code{3} = survey, \code{4} = observed domestic commercial.}
  \item{year}{set of years to display (handy when many years available).}
  \item{plus}{plus class value (accumulates all \code{xfld >= plus}).}
  \item{ptype}{string specifiying the plot type: \cr
    \code{"bars"} histogram-like bars; \cr
    \code{"csum"} cumulative sum curves with median point estimated; \cr
    \code{"vlin"} vertical lines.}
  \item{allYR}{logical: if \code{TRUE}, force the display of all years either specified or available.}
  \item{allTT}{logical: if \code{TRUE}, force the display of all trip types either specified or available.}
  \item{major}{numeric code for major PMFC areas.}
  \item{minor}{numeric code for minor PMFC areas.}
  \item{locality}{numeric code for fishing localities in minor PMFC areas.}
  \item{srfa}{alpha-numeric code for slope rockfish assessment areas.}
  \item{srfs}{character code for slope rockfish assessment subareas.}
  \item{xlim}{explicit x-limits.}
  \item{ylim}{explicit y-limits.}
  \item{pxlab}{relative position on histogram plots to add summary information.}
  \item{axes}{logical: if \code{FALSE}, suppress plotting most of the axes 
    (primarily for dense cumulative sum plots).}
  \item{fill}{logical: if \code{TRUE}, fill bars (if selected) with \code{bg} colour.}
  \item{bg}{background colour to fill bars or points.}
  \item{fg}{foreground colour for bar borders or lines.}
  \item{hrow}{height of each row in inches if figure is sent to an image file.}
  \item{hpage}{height of page in inches if figure is sent to an image file.
    The maximum height of a page will be the minimum of \code{hpage} and \code{m*hrow},
    where \code{m} is the number of rows in a multi-panel figure. }
  \item{wmf}{logical: if \code{TRUE}, send plot to a \code{.wmf} file.}
  \item{pix}{logical: if \code{TRUE}, send plot to a \code{.png} file.}
}
\details{
  The function displays the relative frequency of a specified metric like length
  by year (row) and trip type (column). Cells where the number of specimens 
  \code{<= minN} are left blank. If only one trip type is specified or available, 
  the cells display the annual histograms by row with an automatically calculated
  number of columns.
}
\value{
  No value is explictly returned by the function. A global list object \code{PBSfish}
  provides the following:
  \item{dat }{Massaged input data.}
  \item{xy }{Output from the function \code{hist()}.}
  \item{YRS }{Years used in the plots.}
  \item{ttype }{Trip type used in the plots.}
  \item{xlim }{Limits of the x-axis.}
  \item{ylim }{Limits of the y-axis.}
}
\examples{
pbsfun=function(){
  data(pop.age)
  histMetric(xfld="len",year=2003:2007,xlim=c(0,60),fill=TRUE,bg="moccasin",xint=2)
  invisible() }
pbsfun()
}
\seealso{
  \code{\link{histTail}}, \code{\link{getFile}} \cr
  \pkg{PBSmodelling}: \code{\link[PBSmodelling]{drawBars}} \cr
  \pkg{graphics}: \code{\link[graphics]{polygon}}
}
\keyword{hplot}
