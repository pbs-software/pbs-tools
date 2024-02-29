\name{processBio}
\alias{processBio}
\alias{processMap}
\title{Process Results from SQL Queries}
\description{
  For bio data, add location fields to object \code{PBSdat},
  which results from the SQL query \code{gfb_bio.sql}.
  For map data, process effort and gear fields in \code{PBSdat},
  which results from the SQL query \code{fos_map_density.sql}.
}
\usage{
processBio(dat=PBSdat, strSpp, addsrfa=TRUE, 
   addsrfs=TRUE, addpopa=TRUE, addstock=TRUE, addsort=TRUE,
   useSM=FALSE, maxrows=5e4, zflds=c("ssrc","AC","FOSTID"))

processMap(dat=PBSdat, strSpp, prefix="map", useSM=FALSE)
}
\arguments{
  \item{dat}{\code{data frame} -- usually \code{PBSdat}, resulting from an 
    SQL query: \cr \code{getData("gfb_bio.sql","GFBioSQL","396")} }
  \item{strSpp}{\code{character} -- species Hart code used to determine stock name if 
    \code{addstock=TRUE}.}
  \item{addsrfa}{\code{logical} -- if \code{TRUE}, calculate slope rockfish 
    assessment areas from fields \code{major}, \code{minor}, and 
    \code{locality}, and add as a field \code{srfa} to \code{dat}.}
  \item{addsrfs}{\code{logical} -- if \code{TRUE}, calculate slope rockfish 
    assessment subareas (gullies) from fields \code{major}, \code{minor}, and 
    \code{locality}, and add as a field \code{srfs} to \code{dat}.}
  \item{addpopa}{\code{logical} -- if \code{TRUE}, determine Pacific Ocean Perch 
    areas using the PolySet \code{popa} and the fields
    \code{X} and \code{Y} in \code{dat}, then add as a field \code{popa}
    to \code{dat}.}
  \item{addstock}{\code{logical} -- if \code{TRUE}, calculate stock names based on
    species code and major and/or minor PMFC areas -- uses function 
    \code{calcStockArea}.}
  \item{addsort}{\code{logical} -- if \code{TRUE}, add a field called \code{'sort'}
    and populate with \code{"U"} (unsorted), \code{"K"} (keepers), and \code{"D"} (discards).}
  \item{useSM}{\code{logical} -- if \code{TRUE}, use data from seamounts else remove seamount data.}
  \item{maxrows}{\code{numeric} -- number of rows of \code{dat} to process at any 
    one time when \code{addpopa=TRUE}. Finding POP areas uses the function
    \code{findPolys}, which bogs down when the number of events gets very high.}
  \item{zflds}{\code{character} -- field names to add to end of bio data.frame.}
  \item{prefix}{\code{character} -- prefix string to name map object: <prefix><strSpp>.}
}
\details{
  For bio data, this function can add fields \code{srfa} (slope rockfish 
  assessment areas), \code{srfs} (slope rockfish assessment subareas or 
  gullies), \code{popa} (Pacific Ocean Perch areas identified by 
  Schnute \emph{et al}. 2001), and \code{stock} (stock names based on
  species code and PMFC areas). 
  These additional areas do not exist as fields in GFBioSQL.
  
  For map data, code attempts to replace zero-effort fields by 
  using an additive linear model estimating gear-specific effort
  from catch, depth, locality, and cfv (vessel).
}
\value{
  The data frame \code{dat} plus additional fields.
}
\references{
  Schnute, J.T., Haigh, R., Krishka, B. and Starr, P. (2001) 
  Pacific Ocean Perch assessment for the west coast of Canda in 2001. 
  \emph{Canadian Science Advisory Secretariat, Research Document} \bold{2001/138}, 90 pp.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2020-07-14}
}
\seealso{ 
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{getData}},
  \code{\link[PBStools]{getCatch}},
  \code{\link[PBStools]{calcSRFA}},
  \code{\link[PBStools]{calcStockArea}}
}
\keyword{manip}
\concept{M06_Assessment}
