\name{histTail}
\alias{histTail}
\title{Plot Histogram with Optional Tail Zoom}
\description{
  Plot a histogram for a specified biology metric with an 
  option to zoom in on the tail detail.
}
\usage{
histTail(dat=pop.age, xfld="age", tailmin=NULL, 
    bcol="gold", tcol="moccasin", hpage=3.5, 
    wmf=FALSE, pix=FALSE, ioenv=.GlobalEnv, ...)
}
\arguments{
  \item{dat}{data file of biology metrics (e.g., \code{pop.age}).}
  \item{xfld}{string specifying the field containing the metric of interest.}
  \item{tailmin}{numeric value of \code{xfld} to start the tail plot.}
  \item{bcol}{fill colour for the main histogram bars.}
  \item{tcol}{fill colour for the tail histogram bars.}
  \item{hpage}{height of page in inches if figure is sent to an image file.}
  \item{wmf}{logical: if \code{TRUE}, send plot to a \code{.wmf} file.}
  \item{pix}{logical: if \code{TRUE}, send plot to a \code{.png} file.}
  \item{ioenv}{input/output environment for function input data and output results.}
  \item{...}{additional arguments to pass to \code{truehist} and \code{hist}.}
}
\details{
  The function displays the relative frequency of a specified metric like age.
  Its functionality is minimal, providing a quick histogram for a specified field 
  in a data frame. The tail histogram appears as an inset on the right showing a
  \emph{zoom-in} of the frequency of the upper tail data.
  
  The user can specify whether to plot the relative probability distribution
  \code{prob=TRUE} or a frequency distribution \code{prob=FALSE}.
  The default provides the former for the main histogram and the latter for the tail zoom-in.
}
\value{
  No value is explictly returned by the function. A global list object \code{PBStool}
  provides the following:
  \item{x}{Vector of raw data (\code{xfld} in \code{dat}) used in the main histogram.}
  \item{nx}{Total number of observations \emph{N}.}
  \item{nz}{Number of observations in tail \emph{n}.}
  \item{brks}{Breaks used to get histogram bars in tail.}
  \item{xlab}{Label used for x-axis.}
  \item{ylab}{Label used for y-axis.}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(){
  data(pop.age,envir=.PBStoolEnv)
  histTail(xfld="age",tailmin=60,prob=FALSE)
  invisible() }
pbsfun()
})
}
\seealso{
  \code{\link{histMetric}} \cr
  \pkg{PBSmodelling}: \code{\link[PBSmodelling]{evalCall}} \cr
  \pkg{MASS}: \code{\link[MASS]{truehist}} \cr
  \pkg{graphics}: \code{\link[graphics]{hist}}
}
\keyword{hplot}
