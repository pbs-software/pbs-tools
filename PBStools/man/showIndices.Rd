\name{showIndices}
\alias{showIndices}
\title{Show Survey Indices from Bootstrap Tables}
\description{
  Show survey indices for a species from bootstrapped biomass
  estimates stored in the SQL database \code{GFBioSQL}.
}
\usage{
showIndices(strSpp= "396", serID=1, survID=NULL, bootID, 
   tenv=.PBStoolEnv, quiet=TRUE, addN=TRUE, addT=TRUE, 
   outnam="relAbund", png=FALSE, pngres=400, PIN=c(7,7))
}
\arguments{
  \item{strSpp}{\code{string} -- Hart code specifying species in survey.}
  \item{serID}{\code{numeric} -- survey series ID assigned to each survey series;
    if specified (i.e., not \code{NULL}) then \code{survID} is ignored.}
  \item{survID}{\code{numeric} -- survey ID assigned to each survey by the GFBio team.}
  \item{bootID}{\code{numeric} -- boot run ID assigned to each record in the BOOT header table.}
  \item{tenv}{\code{environment} -- temporary working environment.}
  \item{quiet}{\code{logical} -- if \code{TRUE}, plot an empty panel in a multi-panel figure;
    if \code{FALSE}, print species boot results to the console.}
  \item{addN}{\code{logical} -- if \code{TRUE}, report the number of specimens below each index.}
  \item{addT}{\code{logical} -- if \code{TRUE}, add the trend line, using an \code{lm}-fitted 
    line (performed in \code{log2} space), and annual rate of change to plot.}
  \item{outnam}{\code{string} -- name of output file name prefix for a comma-delimited
    file of Awatea/ADMB-style indices with field names \code{c("series", "year", "value", "CV")}.}
  \item{png}{\code{logical} -- if \code{TRUE}, send the figure output to a \code{png} file.}
  \item{pngres}{\code{numeric} -- resolution (pixels/inch) of \code{png} file.}
  \item{PIN}{\code{numeric} -- vector of width and height (inches) for the output \code{png} file.}
}
\details{
  The function relies on a short query function called \code{getBootRuns} 
  which in turn queries tables of survey information (\code{BOOT_HEADER}) 
  and relative biomass estimates by species for each survey (\code{BOOT_DETAIL}).
  The results (\code{PBSdat}) are sent to the specified temporary working environment.
  
  Species relative biomass indices and their bootstrapped confidence intervals are plotted.

  Note: Any survey ID \code{survID} can have multiple boot runs \code{bootID}
  (e.g., re-stratification trials). These are usually not consistent across an 
  entire survey series. If the user knows the \code{bootID} values that define a consistent series,
  s/he can specify them explicitly.
}
\value{
  Annual biomass indices for the specified \code{survID}s are return invisibly.
  A global list object \code{PBStool} provides the following:
  \item{surveys}{Data frame of survey information for the specified species.}
  \item{sppBoot}{Subset of \code{surveys} that has records with the specified survey IDs.}
  \item{survBoot}{Subset of \code{surveys} that shows boot run description for \code{bootID}.}
  \item{xy}{List of x- and y-vectors for the biomass points on the plot.}
  \item{cil}{List of x- and y-vectors for the lower points of the confidence limits.}
  \item{cih}{List of x- and y-vectors for the upper points of the confidence limits.}
  \item{clr}{List of x- and y-vectors for the colour numbers of biomass points.}
  \item{llab}{List of boot run labels for each point.}
  \item{ulab}{Vector of unique boot run labels for the legend.}
}
\author{ 
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-10-28}

  Bootstraps by \href{mailto:norm.olsen@dfo-mpo.gc.ca}{Norm Olsen} (PBS, Nanaimo)
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{getBootRuns}}, 
  \code{\link[PBStools]{calcMoments}}\cr
In package \pkg{PBSdata}:\cr
  \code{\link[PBSdata]{spn}}, 
  \code{\link[PBSdata]{species}}
}
\examples{
\dontrun{
local(envir=.PBStoolEnv,expr={
pbsfun=function(strSpp="602",region="QCS",dfo=FALSE){
  if (dfo) {
    choices=c("WCVI","QCS","WQCI","HS")
    i=match(region,choices,nomatch=0)
    if (i==0) showError(paste("Choose region from\n'",
      paste(choices,collapse="', '"),"'",sep=""),as.is=TRUE)
    survID=switch(i,
      c(16,58,70,124,125,126,128),
      c(1,2,3,59,121,127),
      c(57,122,123,129),
      c(15,111,162,163) )
    showIndices(strSpp,survID) }
  else showMessage("If connected to DFO network, set argument 'dfo=TRUE'")
  invisible() }
pbsfun()
})
}
}
\keyword{hplot}
\concept{M04_Survey}
