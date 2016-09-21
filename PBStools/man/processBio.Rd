\name{processBio}
\alias{processBio}
\title{Process Results from SQL Query to GFBio}
\description{
  Add location fields to biological data object \code{PBSdat}
  that results from the SQL query \code{gfb_bio.sql}.
}
\usage{
processBio(dat=PBSdat, strSpp, addsrfa=TRUE, 
   addsrfs=TRUE, addpopa=TRUE, addstock=TRUE, maxrows=5e4)
}
\arguments{
  \item{dat}{ data frame, usually \code{PBSdat}, resulting from an 
    SQL query: \cr \code{getData("gfb_bio.sql","GFBioSQL","396")} }
  \item{strSpp}{species Hart code used to determine stock name if 
    \code{addstock=TRUE}.}
  \item{addsrfa}{ logical: if \code{TRUE}, calculate slope rockfish 
    assessment areas from fields \code{major}, \code{minor}, and 
    \code{locality}, and add as a field \code{srfa} to \code{dat}.}
  \item{addsrfs}{ logical: if \code{TRUE}, calculate slope rockfish 
    assessment subareas (gullies) from fields \code{major}, \code{minor}, and 
    \code{locality}, and add as a field \code{srfs} to \code{dat}.}
  \item{addpopa}{ logical: if \code{TRUE}, determine Pacific Ocean Perch 
    areas using the PolySet \code{popa} and the fields
    \code{X} and \code{Y} in \code{dat}, then add as a field \code{popa}
    to \code{dat}.}
  \item{addstock}{ logical: if \code{TRUE}, calculate stock names based on
    species code and major and/or minor PMFC areas -- uses function 
    \code{calcStockArea}.}
  \item{maxrows}{numeric: number of rows of \code{dat} to process at any 
    one time when \code{addpopa=TRUE}. Finding POP areas uses the function
    \code{findPolys}, which bogs down when the number of events gets very high. }
}
\details{
  Currently, this function can add fields \code{srfa} (slope rockfish 
  assessment areas), \code{srfs} (slope rockfish assessment subareas or 
  gullies), \code{popa} (Pacific Ocean Perch areas identified by 
  Schnute \emph{et al}. 2001), and \code{stock} (stock names based on
  species code and PMFC areas). 
  These additional areas do not exist as fields in GFBIO.
}
\value{
  The data frame \code{dat} plus additional fields.
}
\references{
  Schnute, J.T., Haigh, R., Krishka, B. and Starr, P. (2001) 
  Pacific Ocean Perch assessment for the west coast of Canda in 2001. 
  \emph{Canadian Science Advisory Secretariat, Research Document} \bold{2001/138}, 90 pp.
}
\seealso{ 
  \code{\link[PBStools]{getData}},
  \code{\link[PBStools]{getCatch}},
  \code{\link[PBStools]{calcSRFA}},
  \code{\link[PBStools]{calcStockArea}}
}
\keyword{manip}
