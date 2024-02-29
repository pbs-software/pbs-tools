\name{plotSnail}
\alias{plotSnail}
\title{
  Phase Plot of Exploitation vs. Biomass
}
\description{
  Plot the historical progression of the ratio \eqn{u_{t-1}/u_\text{MSY}}{u[t-1]/u[MSY]}
  against \eqn{B_t/B_\text{MSY}}{B[t]/B[MSY]}. These plots are known as 
  \sQuote{phase plots} by traditionalists, \sQuote{Kobe plots} by 
  tuna commissions, and \sQuote{snail trails} by NZ wine connoisseurs.
}
\usage{
plotSnail(BoverBmsy, UoverUmsy, model="SS", yrs=1935:2023,
   p=c(0.05,0.95), xLim=NULL, yLim=NULL, Lwd=1.5, ngear=1,
   Cnames, assYrs=NULL, outs=FALSE, labYrs=NULL,  ## outs = outliers
   ptypes="win", pngres=400, PIN=c(8,6), onepanel=TRUE, subarea, add=FALSE,
   outnam, lang="e")
}
\arguments{
  \item{BoverBmsy}{\code{numeric} -- matrix of \eqn{B_t}{Bt} over \eqn{B_{\text{MSY}}}{Bmsy}}
  \item{UoverUmsy}{\code{numeric} -- matrix of \eqn{u_t}{ut} over \eqn{u_{\text{MSY}}}{umsy}}
  \item{model}{\code{character} -- model platform being used (SS=Stock Synthesis, AW=Awatea)}
  \item{yrs}{\code{numeric} -- years to track by snails/slugs/tunas/rockfish}
  \item{p}{\code{numeric} -- quantiles to show in the final year}
  \item{xLim}{\code{numeric} -- limits of the x-axis}
  \item{yLim}{\code{numeric} -- limits of the y-axis}
  \item{Lwd}{\code{numeric} -- line width of the snail trail}
  \item{ngear}{\code{numeric} -- number of commercial gears}
  \item{Cnames}{\code{character} -- vector of commercial gear or fleet names}
  \item{assYrs}{\code{numeric} -- model years of past assessments (if any)}
  \item{outs}{\code{logical} -- if \code{TRUE}, extend the range of the 
    quantile box whiskers to include the outliers (all data)}
  \item{labYrs}{\code{numeric} -- vector of years to label on the trajectory line(s)}
  \item{ptypes}{\code{character} -- plot types for the figures: \code{'win'} = windows graphics device, \code{'eps'} = encapsulated postscript, \code{'png'} = portable network graphics, \code{'wmf'} = windows metafile}
  \item{pngres}{\code{numeric} -- resolution of output figure (pixels per inch)}
  \item{PIN}{\code{numeric} -- width and height of output figure (inches)}
  \item{onepanel}{\code{logical} -- if \code{TRUE}, plot all subarea trajectories on the same panel}
  \item{subarea}{\code{numeric} -- choose a subarea by number from SS3 model output}
  \item{add}{\code{logical} -- if \code{TRUE}, plot all subarea trajectories in the same figure (with one or more panels)}
  \item{outnam}{\code{character} -- optional output name to override an internally generated one}
  \item{lang}{\code{character} -- a letter that denotes the language for output: currently only \code{'e'} (english) or \code{'f'} (french)}
}
\details{
  The plot shows the time history of the exploitation rate compared to the
  spawning biomass in MSY space and using default provisional DFO reference points.
}
\author{
First created by:\cr
  \href{mailto:andrew.edwards@dfo-mpo.gc.ca}{Andrew M. Edwards}, PBS, DFO, Nanaimo BC

Modified by:\cr
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-29}
}
\note{
  The term \emph{snail trail} comes from PJS.
  Others have adopted the term \emph{Kobe plot}, so named by the 
  first joint meeting of the tuna regional fisheries management
  organizations (RFMOs), held in Kobe, Japan, in January 2007
  (Anonymous 2007)
}
\references{
  Anonymous (2007)
  \href{https://www.tuna-org.org/documents/other/kobe\%20report\%20english-appendices.pdf}{%
    Report of the Joint Meeting of Tuna RFMOs, January 20-26, 2007, Kobe, Japan}
  Participants: IATTC, ICCAT, IOTC, WCPFC, CCSBT.
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{compBmsy}}
}
\keyword{hplot}
\concept{M06_Assessment}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
