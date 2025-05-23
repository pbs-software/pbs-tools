\name{calcRatio}
\alias{calcRatio}
\title{
  Calculate the Ratio of One Catch to Another
}
\description{
  Calculate the annual ratios of one catch to another 
  (or one field to another) for various PMFC major areas and coastwide.
}
\usage{
calcRatio(dat, nfld, dfld, nzero=TRUE, dzero=TRUE, sumF=mean, 
   major=NULL, startM=1, plot=FALSE, ylim=NULL, wmf=FALSE, 
   quiet=FALSE, ioenv=.GlobalEnv)
}
\arguments{
  \item{dat}{catch data file, either generated by a call to SQL or supplied by the user.}
  \item{nfld}{numerator field for the ratio calculations.}
  \item{dfld}{denominator field for the ratio calculations.}
  \item{nzero}{logical: if \code{TRUE}, retain zero-value numerators, otherwise remove.}
  \item{dzero}{logical: if \code{TRUE}, retain zero-value denominators, otherwise remove.}
  \item{sumF}{function that summarises ratios (e.g., \code{mean}).}
  \item{major}{Pacific Marine Fisheries Commission major area codes (e.g., 3:9).}
  \item{startM}{scalar integer from 1 to 12 specifying which month starts the fishing/fiscal year.}
  \item{plot}{logical: if \code{TRUE}, plot the annual ratios for the various PMFC areas.}
  \item{ylim}{explicit limits for the y-axis.}
  \item{wmf}{logical: if \code{TRUE}, send the plot to a \code{.wmf} file.}
  \item{quiet}{logical: if \code{TRUE}, do not interupt code with \code{showError} but 
    return \code{NULL}. (This is handy when using \code{calcRatio} in other functions.) }
  \item{ioenv}{input/output environment for function input data and output results.}
}
\details{
  For each numerator/denominator pair in the set, the following actions are taken:

  If the numerator or the denominator contains \code{NA}, discard the observation.
  
  If zero-value denominators are allowed (\code{dzero=TRUE}) then the folowing conditions apply:
  
  (a) if the numerator > 0 and the denominator = 0, then set the denominator to the 
  numerator (e.g., entire catch was discarded);
  
  (b) if the numerator and the denominator = 0, then set the numerator to 0 and set
  the denominator to the small value 0.0001 (e.g, discard rate is effectively zero).
}
\value{
  Invisibly returns the mean ratios in a matrix where rows are years 
  and columns are PMFC area codes.
  
  Other objects are saved to the global list object \code{PBStool}.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-10-28}\cr
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{makeCATtables}},
  \code{\link[PBStools]{convFY}}\cr
In package \pkg{PBSdata}:\cr
  \code{\link[PBSdata]{pmfc}}
}
\examples{
\dontrun{
local(envir=.PBStoolEnv,expr={
pbsfun=function(dfo=FALSE){
  if (dfo) {
    getData("pht_catORF.sql",strSpp="440",path=.getSpath())
    ymr.pop.orf = PBSdat
    pmean = calcRatio(dat=ymr.pop.orf, nfld="catch", dfld="ORF",
      major=3:9, plot=TRUE, ylim=c(0,.35))
    print(pmean) }
  else cat("If you are logged onto the DFO network,\nrun 'pbsfun(dfo=TRUE)'\n.")
  invisible() }
pbsfun()
})
}
}
\keyword{manip}
\keyword{hplot}
\concept{M03_Fishery}
