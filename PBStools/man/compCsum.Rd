\name{compCsum}
\alias{compCsum}
\title{Compare Cumulative Relative Frequency Curves}
\description{
  Compare cumulative relative frequency curves in a common plot space.
}
\usage{
compCsum(dat=pop.age, pro=TRUE, strSpp="", xfld="age", plus=60,
     yfac="year",areas=list(major=NULL, minor=NULL, locality=NULL, 
     srfa=NULL, srfs=NULL, popa=NULL), ttype=list(c(1,4),c(2,3)), 
     stype=c(1,2,5:8), ameth=3, allTT=TRUE, years=1998:2006,
     pix=FALSE, wmf=FALSE, singles=FALSE, pages=FALSE, ioenv=.GlobalEnv)
}
\arguments{
  \item{dat}{Biological data set with fields \code{xfld} and \code{yfac}.}
  \item{pro}{logical: if \code{TRUE}, transform the raw age observations to proportions-at-age.}
  \item{strSpp}{String code for species. If dataset has attribute \code{spp},
    this will be used before \code{strSpp}.}
  \item{xfld}{String name of \emph{x}-field for calculating cumulative frequencies.}
  \item{plus}{Plus class (accumulated) for \code{xfld}.}
  \item{yfac}{Vector of string names that correspond to field names in \code{dat}. \cr
    Special cases \code{"area"} and \code{"trip"} create new fields from input arguments 
    \code{areas} and \code{ttype}.}
  \item{areas}{List of listed area codes; choices can be any/all of
    \code{major}, \code{minor}, \code{locality}, \code{srfa}, \code{srfs}, \code{popa}.}
  \item{ttype}{List of trip type codes to use from field \code{ttype}.}
  \item{stype}{List of sample type codes to use from field \code{stype}.}
  \item{ameth}{Code specifying ageing method protocol (defaults to \code{3}, specifying break & burn only).}
  \item{allTT}{Logical: if \code{TRUE}, force the display of all trip types either specified or available.}
  \item{years}{Vector of numeric years used in special cases of \code{yfac} (see above).}
  \item{pix}{Logical: if \code{TRUE}, send plot to a \code{.png} file.}
  \item{wmf}{Logical: if \code{TRUE}, send plot to a \code{.wmf} file.}
  \item{singles}{Logical: if \code{TRUE} and \code{pix|wmf}, send each area/trip type combination to separate files.}
  \item{pages}{Logical: if \code{TRUE} and \code{pix|wmf}, send each page of area plots to separate files.}
  \item{ioenv}{input/output environment for function input data and output results.}
}
\details{
  The function compares cumulative relative frequency curves for specified factors 
  \code{yfac}. Typically, factors are fields in \code{dat} and comparisons are performed 
  for each specified area and trip type. If the data are plotted to the 
  screen device, each panel contains curves by combinations of trip type (rows) and 
  factor (columns), and each page shows these combinations by area.
  If plots are sent to image files (\code{.png} or \code{.wmf}),
  either each row is sent to separate files when \code{singles=TRUE} (handy for
  stacking in a \code{.doc} file) or each page is sent to a separate file when \code{pages=TRUE}.
  
  In special cases \code{yfac="area"} or \code{yfac="trip"}, each panel depicts 
  curves by year, and the factors (curves) are created as new fields from inputs 
  \code{areas} or \code{ttype}. (The option \code{yfac="trip"} is not implemented at present.)
}
\value{
  No value is explictly returned by the function. A global list object \code{PBStool}
  provides the following plus some labels:
  \item{dat}{Qualified data used for the analysis.}
  \item{xlim}{Limits of the x-axis.}
  \item{ylim}{Limits of the y-axis.}
  \item{clrs}{Colour vector used to plot cumulative curves for each factor.}
  \item{plotname}{Current plot name used by this function, but also by \code{.plotDev()}.}
}
\note{
  A suitable data set can be obtained by running the SQL query \code{gfb_bio.sql}.

  \code{getData("gfb_bio.sql","GFBioSQL","396",path=.getSpath())}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(){
  data(pop.age,envir=.PBStoolEnv)
  compCsum(areas=list(srfa=list(c("5AB","5CD"))),ttype=list(c(2,3)))
  invisible() }
pbsfun()
})
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2021-06-15}\cr
}
\seealso{
  \code{\link{pop.age}}, \code{\link{calcVB}}, \code{\link{histMetric}}, 
  \code{\link{getFile}}, \code{\link{biteData}}
}
\keyword{hplot}
\concept{M02_Biology}
