\name{scaleVec}
\alias{scaleVec}
\title{
  Scale Vector to Lie Between Target Minimum and Maximum
}
\description{
  Scale a vector to span a target minimum and maximum. \cr
  (See \pkg{PBSmodelling} functions \code{scalePar} and \code{restorePar}.)
}
\usage{
scaleVec(V, Tmin = 0, Tmax = 1)
}
\arguments{
  \item{V}{numeric vector (\code{NA}s ignored).}
  \item{Tmin}{numeric target minimum value.}
  \item{Tmax}{numeric target maximum value.}
}
\details{
  This function combines the utility of \pkg{PBSmodelling}'s \code{scalePar} 
  and \code{restorePar} so that a user can rescale a vector to lie between
  any minimum and maximum value (including \code{c(0,1)}).

  The target minimum must be less than the target maximum, otherwise the function 
  stops execution.
}
\value{
  The numeric vector \code{V} re-scaled to range from \code{Tmin} and \code{Tmax}.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC.
}
\seealso{
  \code{\link[PBSmodelling]{scalePar}}, \code{\link[PBSmodelling]{restorePar}}
}
\examples{
pbsfun=function() {
  V = runif(50,100,1000)
  T = scaleVec(V, Tmin=-3, Tmax=5)
  lmf = lm(T~V)
  par0 = par(no.readonly=TRUE); on.exit(par(par0))
  expandGraph(cex=1.2,mgp=c(1.8,0.5,0))
  plot(V,T,type="n",cex.lab=1.5)
  abline(lmf,col="dodgerblue",lwd=2)
  points(V,T,pch=21,cex=1.2,col="navyblue",bg="yellow")
  expr=paste(c("addLabel(0.1,0.85,expression(T == ",round(lmf$coeff[1],5),
    " + ",round(lmf$coeff[2],5),"* V~~~~R^2 == ",
    round(summary(lmf)$adj.r.squared,2),"),adj=0)"),collapse="")
  eval(parse(text=expr))
  invisible(expr) }
pbsfun()
}
\keyword{arith}
\keyword{utilities}

