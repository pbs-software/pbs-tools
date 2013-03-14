\name{plotGMA}
\alias{plotGMA}
\title{
  Plot the Groundfish Management Areas
}
\description{
  Plot the groundfish management areas (GMA) for Pacific Ocean Perch
  (\emph{Sebastes alutus}) and Yellowmouth Rockfish (\emph{S. reedi})
  used by Pacific groundfish managers at RHQ in Vancouver.
}
\usage{
plotGMA(gma=gma.popymr, xlim=c(-134,-123), ylim=c(48.05,54.95), 
     dimfig=c(9,9), pix=FALSE, extra.labels=NULL)
}
\arguments{
  \item{gma}{PolySet of Groundfish Management Areas (e.g. \bold{PBSdata}\code{::gma.popymr} GMAs).}
  \item{xlim}{numeric: latitude limits of the map region.}
  \item{ylim}{numeric: longitude limits of the map region.}
  \item{dimfig}{numeric: dimensions of the figure region (inches).}
  \item{pix}{logical: if \code{TRUE}, send the plot to a \code{.png} file.}
  \item{extra.labels}{EventData object that defines coordinates and labels for additional map annotation.}
}
\details{
  The code assumes that the PolySet of GMA areas contains an attribute called \code{PolyData} 
  that defines colours and labels for each of the GMAs.

  If \code{extra.labels = NULL}, then no extra labels are added. \cr
  If \code{extra.labels = "default"}, then function-supplied extra labels are added. \cr
  Otherwise, a user can specify an \code{EventData} object with fields \code{EID}, 
  \code{X}, \code{Y}, and \code{label}. The object should have the \code{projection} 
  attribute set to the same projection as the map (in this case \code{"LL"}).
}
\value{
  Invisibly returns a list of the \code{PolyData} attribute and the \code{EventData} object called 
  \code{extra.labels} (see Details section above).
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC.
}
\seealso{
The DFO proprietary package \pkg{PBSvault}: \code{gma.popymr}, \code{pfma} \cr
(contact \code{rowan.haigh@dfo-mpo.gc.ca} for availability)
}
\keyword{hplot}

