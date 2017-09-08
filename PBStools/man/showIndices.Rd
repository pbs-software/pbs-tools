\name{showIndices}
\alias{showIndices}
\title{Show Survey Indices from Bootstrap Tables}
\description{
  Show survey indices for a species from bootstrapped biomass
  estimates stored in the SQL database \code{GFBioSQL}.
}
\usage{
showIndices(strSpp="396",serID=1, survID=NULL, bootID, 
   tenv=.PBStoolEnv, quiet=FALSE, addN=FALSE, outnam,
   png=FALSE, pngres=300)
}
\arguments{
  \item{strSpp}{string specifying species code (\code{species$code}).}
  \item{serID}{numeric survey series ID assigned to each survey series;
    if specified (not NULL) then \code{survID} is ignored.}
  \item{survID}{numeric survey ID assigned to each survey by the GFBio team.}
  \item{bootID}{numeric boot run ID assigned to each record in the BOOT header table.}
  \item{tenv}{temporary working environment.}
  \item{quiet}{logical: if \code{TRUE}, plot an empty panel in and multi-panel figure; if \code{FALSE}, print species boot results to the console.}
  \item{addN}{logical: if \code{TRUE}, report the number of specimens below each boxplot.}
  \item{outnam}{string name of output file name prefix for a comma-delimited
    file of Awatea/ADMB-style indices with field names \code{c("series", "year", "value", "CV")}.}
  \item{png}{logical: if \code{TRUE}, send the figure output to a \code{png} file.}
  \item{pngres}{numeric: resolution of \code{png} file in pixels/inch.}
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
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC \cr
  Bootstraps by Norm Olsen (also PBS, Nanaimo).
}
\seealso{
  \code{\link{getBootRuns}}, \code{\link{calcMoments}}, 
  \code{\link{spn}}, \code{\link{species}}
}
\examples{
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
\keyword{hplot}
