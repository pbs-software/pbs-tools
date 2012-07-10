\name{zapHoles}
\alias{zapHoles}
\title{Zap Holes That Will Be Filled Anyway}
\description{
  Zap (remove) holes from polygons that will ultimately be filled 
  by solids from other polygons.
}
\usage{
zapHoles(pset)
}
\arguments{
  \item{pset}{a valid PolySet object.}
}
\details{
  The function attempts to remove holes that will ultimately be filled
  anyway. It uses centroid matching between holes and solids to identify 
  candidate holes for removal.

  The original rationale for this function is to groom a PolySet before 
  using in \cr \code{addPolys(pset,...,colHoles="white")}. The \code{colHoles}
  option was designed to get rid of retrace lines that appear in 
  \code{.pdf} files made from metafiles.

  That said, the PolySet created by this function essentially comprises
  layers. To see all the layers, the user must add them sequentially from
  largest to smallest, which isn't terribly efficient.
}
\value{
  A PolySet potentially modified to remove holes that will be filled. 
  Non-essential attributes will be retained and supplemented with two 
  additional ones created by \code{calcCentroid} and \code{calcArea}:
  \code{keep} = data frame summarizing the kept polygons, and \code{zap}
  = data frame summarizing the holes removed.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\seealso{
  \code{\link[PBStools]{calcSurficial}} \cr
  \pkg{PBSmapping}: \code{\link[PBSmapping]{addPolys}}
}
\examples{
\dontrun{
pbsfun=function(){
  data(qcb); qcbz=zapHoles(qcb)
  pdata=attributes(qcb)$PolyData
  pdata=pdata[rev(order(pdata$area)),]
  expandGraph(mfrow=c(1,1),plt=c(.05,1,.05,1))
  plotMap(qcbz)
  for (i in 1:nrow(pdata)) {
    pid=pdata$PID[i]
    qcbi=qcbz[is.element(qcbz$PID,pid),]
    if (nrow(qcbi)==0) next
    addPolys(qcbi,col=pdata$col[i]) }
  invisible() }
pbsfun()
}}
\keyword{manip}
