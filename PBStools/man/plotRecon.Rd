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
   years=1918:2015, xlab=seq(1920,2050,5), yrs.rec=attributes(dat)$yrs.rec,
   ymax=NULL, shade=FALSE, shadier=FALSE, figDir=getwd(),
   eps=FALSE, png=FALSE, wmf=FALSE, PIN=c(10,5))
}
\arguments{
  \item{dat}{\code{numeric} -- array of annual catch by PMFC area and fishery ID. }
  \item{strSpp}{\code{character} -- string specifying Hart species code (e.g., \code{"440"} = Yellowmouth Rockfish). }
  \item{major}{\code{numeric} -- codes specifying PMFC areas (\code{1=4B}, \code{3=3C},
    \code{4=3D}, \code{5=5A}, \code{6=5B}, \code{7=5C}, \code{8=5C}, \code{9=5E}). }
  \item{fidout}{\code{numeric} -- code indicating for which fishery the catch reconstruction will be plotted 
    (\code{1}=Trawl, \code{2}=Halibut, \code{3}=Sablefish, \code{4}=Dogfish-Lingcod, \code{5}=H&L Rockfish, \code{10}=Combined). }
  \item{years}{\code{numeric} -- vector of years for which the catch reconstruction will be plotted. }
  \item{xlab}{\code{numeric} -- vector of years to use for major tick labels. }
  \item{yrs.rec}{\code{numeric} -- breakpoints in years between catch periods: landings calculated using \code{gamma} and no releases (discards), 
    landings calculated using \code{gamma} and releases calculated using \code{delta}, reported landings and releases.}
  \item{ymax}{\code{numeric} -- maximum value for y-axis.}
  \item{shade}{\code{logical} -- if \code{TRUE}, shade the catch periods.}
  \item{shadier}{\code{logical} -- if \code{TRUE}, make the background shading darker for the catch periods.}
  \item{figDir}{\code{character} -- directory where figure output should be directed.}
  \item{eps}{\code{logical} -- if \code{TRUE}, send the figure to a postscript file (\code{.eps}). }
  \item{png}{\code{logical} -- if \code{TRUE}, send the figure to a raster file (\code{.png}). }
  \item{wmf}{\code{logical} -- if \code{TRUE}, send the figure to a windows metafile (\code{.wmf}). }
  \item{PIN}{\code{numeric} -- vector indicating figure size (width, height) specified in inches; 
    only applies if the figure is sent to an output file.}
}
\details{
  See the catch reconstruction algorithm (\code{\link[PBStools]{buildCatch}}) for more details.
}
\references{
  Haigh, R. and Yamanaka, K.L. (2011) Catch history reconstruction for rockfish
  (\emph{Sebastes} spp.) caught in British Columbia coastal waters. 
  \emph{Canadian Technical Report of Fisheries and Aquatic Sciences} \bold{2943}: viii + 124 p.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus: Institute of Ocean Sciences (IOS), Sidney BC}\cr
  Last modified \code{Rd: 2018-05-23}
}
\seealso{
  \code{\link[PBStools]{buildCatch}},
  \code{\link[PBStools]{plotDiag}},
  \code{\link[PBStools]{plotFOScatch}},
  \code{\link[PBStools]{plotCatch}},
  \code{\link[PBStools]{plotLocal}}
}
\keyword{hplot}

