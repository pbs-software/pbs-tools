\name{plotRecon}
\alias{plotRecon}
\title{
  Plot Reconstructed Annual Catch Series
}
\description{
  Plot reconstructed catch using barplots stacked by catch in PMFC areas.
}
\usage{
plotRecon(dat=cat440rec, strSpp="440", major=c(1,3:9), fidout=10,
   years=1918:2014, xlab=seq(1920,2015,5), 
   eps=FALSE, png=FALSE, wmf=FALSE, PIN=c(10,5))
}
\arguments{
  \item{dat}{numerix array of annual catch by PMFC area and fishery ID. }
  \item{strSpp}{character string specifying Hart species code (e.g., \code{"440"} = Yellowmouth Rockfish). }
  \item{major}{numeric codes specifying PMFC areas (1=4B, 3=3C, 4=3D, 5=5A, 6=5B, 7=5C, 8=5C, 9=5E). }
  \item{fidout}{numeric code indicating for which fishery the catch reconstruction will be plotted 
    (1=Trawl, 2=Halibut, 3=Sablefish, 4=Dogfish-Lingcod, 5=H&L Rockfish, 10=Combined). }
  \item{years}{numeric vector of years for which the catch reconstruction will be plotted. }
  \item{xlab}{numeric vector of years to use for major tick labels. }
  \item{eps}{logical: if \code{TRUE}, send the figure to a postscript file (\code{.eps}). }
  \item{png}{logical: if \code{TRUE}, send the figure to a raster file (\code{.png}). }
  \item{wmf}{logical: if \code{TRUE}, send the figure to a windows metafile (\code{.wmf}). }
  \item{PIN}{numeric vector indicating figure size (width, height) specified in inches; 
    only applies if the figure is sent to an output file.}
}
\details{
  See the catch reconstruction algorithm (\code{\link{buildCatch}}) for more details.
}
\references{
  Haigh, R. and Yamanaka, K.L. (2011) Catch history reconstruction for rockfish
  (\emph{Sebastes} spp.) caught in British Columbia coastal waters. 
  \emph{Canadian Technical Report of Fisheries and Aquatic Sciences} \bold{0000}: xx + 00 p.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC.
}
\seealso{
  \code{\link{buildCatch}}, \code{\link{plotFOScatch}}, \code{\link{plotCatch}}
}
\keyword{hplot}

