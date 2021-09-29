\name{plotGMA}
\alias{plotGMA}
\title{
  Plot the Groundfish Management Areas
}
\description{
  Plot the groundfish management areas (GMA) used by Pacific 
  groundfish managers at RHQ in Vancouver. Various regional
  differences occur for Pacific Ocean Perch, Yellowmouth Rockfish,
  and Walleye Pollock.
}
\usage{
plotGMA(gma=gma.popymr, xlim=c(-134,-123), ylim=c(48.05,54.95), 
   dimfig=c(9,9), eps=FALSE, png=FALSE, extra.labels=NULL, isobath,
   strSpp, lang=c("e","f"))
}
\arguments{
  \item{gma}{\code{PolySet} -- Groundfish Management Areas (e.g. \bold{PBSdata}\code{::gma.popymr} GMAs).}
  \item{xlim}{\code{numeric} -- latitude limits of the map region.}
  \item{ylim}{\code{numeric} -- longitude limits of the map region.}
  \item{dimfig}{\code{numeric} -- dimensions of the figure region (inches).}
  \item{eps}{\code{logical} -- if \code{TRUE}, send the plot to an \code{.eps} file.}
  \item{png}{\code{logical} -- if \code{TRUE}, send the plot to a \code{.png} file.}
  \item{extra.labels}{\code{EventData} -- coordinates and labels for additional map annotation.}
  \item{isobath}{\code{numeric} -- data frame of isobaths.}
  \item{strSpp}{\code{character} -- species code (usually Hart).}
  \item{lang}{\code{character} -- language for image labels, where \code{'e'}=english and \code{'f'}=french.}
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
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2021-06-14}
}
\seealso{
The DFO proprietary package \pkg{PBSvault}: \code{gma.popymr}, \code{pfma} \cr
(contact \code{rowan.haigh@dfo-mpo.gc.ca} for availability)
}
\keyword{hplot}

