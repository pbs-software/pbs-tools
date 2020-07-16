\name{histTail}
\alias{histTail}
\title{Plot Histogram with Optional Tail Zoom}
\description{
  Plot a histogram for a specified biology metric with an 
  option to zoom in on the tail detail.
}
\usage{
histTail(dat=pop.age, xfld="age", tailmin=NULL, 
   xlim=c(0,100), bcol="gold", tcol="moccasin",
   wmf=FALSE, png=FALSE, pngres=400, PIN=c(6.5,3.5), 
   ioenv=.GlobalEnv, lang=c("e","f"), ...)
}
\arguments{
  \item{dat}{\code{data.frame} -- data file of biology metrics (e.g., \code{pop.age}).}
  \item{xfld}{\code{character} -- string specifying the field containing the metric of interest.}
  \item{tailmin}{\code{numeric} -- value of \code{xfld} to start the tail plot.}
  \item{xlim}{\code{numeric} -- limits of the x-axis spanning ages or lengths.}
  \item{bcol}{\code{character|numeric} -- fill colour for the main histogram bars.}
  \item{tcol}{\code{character|numeric} -- fill colour for the tail histogram bars.}
  \item{wmf}{\code{logical} -- if \code{TRUE}, send plot to a \code{'.wmf'} file.}
  \item{png}{\code{logical} -- if \code{TRUE}, send plot to a \code{'.png'} file.}
  \item{pngres}{\code{numeric} -- resolution (pixels/inch) for the \code{'.png'} figure.}
  \item{PIN}{\code{numeric} -- two-element vector specifying output size (inches) 
    as width and height for the \code{'.png'} figure.}
  \item{ioenv}{\code{environment} -- input/output environment for function input data and output results.}
  \item{lang}{\code{character} -- a vector of letters that denote the language for output:
    currently only \code{"e"} (english) and \code{"f"} (french).}
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
\dontrun{
local(envir=.PBStoolEnv,expr={
pbsfun=function(){
  data(pop.age,envir=.PBStoolEnv)
  histTail(xfld="age", tailmin=60, lang="e")
  invisible() }
pbsfun()
})
}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2020-07-16}
}
\seealso{
  \code{\link[PBStools]{histMetric}} \cr
  \pkg{PBSmodelling}: \code{\link[PBSmodelling]{evalCall}} \cr
  \pkg{MASS}: \code{\link[MASS]{truehist}} \cr
  \pkg{graphics}: \code{\link[graphics]{hist}}
}
\keyword{hplot}
