\name{sumCatTabs}
\alias{sumCatTabs}
\title{
  Summarize Catch by Year and PMFC Area
}
\description{
  Summarize catch by year and PMFC area from modern 
  catch arrays compiled during catch reconstruction.
}
\usage{
sumCatTabs(dat, facs=list(c("year","major")), 
     cflds=c("landed","discard"), fnam)
}
\arguments{
  \item{dat}{a 4-dimensional array object of catch created by the catch 
    reconstruction algorithm \code{buildCatch}; the dimensions comprise: \cr
    \code{year} = calendar year, \cr
    \code{major} = PMFC major area codes (1=4B, 3=3C,4=3D, 5=5A, 6=5B, 7=5C, 8=5D, 9=5E), \cr
    \code{fid} = fishery identifier code (1=trawl, 2=halibut, 3=sablefish, 4=dogfish/lingcod, 5=h&l rockfish, \cr
    \code{catch} = catch of principal species and various groups (landed, discard, POP, ORF, TRF, TAR).}
  \item{facs}{list of factors in pairs (two-element string vectors).}
  \item{cflds}{string names of columns containing catch to sum and report.}
  \item{fnam}{string name of the output file to which summary tables are sent.}
}
\value{
  No value is returned to the R console. The output is placed into the file name specified.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-27}
}
\seealso{
  \code{\link{buildCatch}}, \code{\link{processBio}}, \code{\link{sumBioTabs}}
}
\keyword{manip}
\keyword{file}
\concept{M03_Fishery}
