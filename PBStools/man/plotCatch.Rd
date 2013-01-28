\name{plotCatch}
\alias{plotCatch}
\title{Plot Catch History as Annual Barplot}
\description{
  Display catch history where selected columns of catch are stacked by year.
}
\usage{
plotCatch(dat="dbr.rem", flds=c("CAtrawl","UStrawl","TotalHL"),
   yrlim=NULL, wmf=FALSE, ioenv=.GlobalEnv, ...)
}
\arguments{
  \item{dat}{ catch data object or string representing the object contained
    in file form (\code{*.rda},\code{*.r}.}
  \item{flds}{string specifying fields of catch to use in barplot.}
  \item{yrlim}{numeric limits of the years to display; default uses all in \code{dat}.}
  \item{wmf}{logical: if \code{TRUE}, send plot to a \code{.wmf} file.}
  \item{ioenv}{input/output environment for function input data and output results.}
  \item{...}{additional arguments passed to \code{barplot} and \code{legend}.}
}
\details{
  This function simply plots the catch history of the 
  supplied \code{dat} file. Each bar shows the cumulative catch from \code{flds}.
  Decadal mean catch is displayed in horizontal bands.
  
  If the user wishes to change the bar colours by passing in a vector 
  \code{col} to the function, s/he must also pass in a similar vector 
  \code{fill} for the legend.
}
\author{ 
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(){
  col=fill=c("yellow","green","blue")
  plotCatch(dat=ymr.rem,col=col,fill=fill)
  invisible() }
pbsfun()
})
}
\seealso{
  \code{\link{getData}}, \code{\link{getFile}}, \code{\link{dbr.rem}}
}
\keyword{hplot}
