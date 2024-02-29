\name{trackBycat}
\alias{trackBycat}
\title{
  Track Annual Fish Group Catches
}
\description{
  Track fish group catches by year and PMFC area that occur 
  between depths corresponding to a target species' depth-of-occurrence.
}
\usage{
trackBycat(strSpp="396", major=5:7, mindep=70, maxdep=441, 
   dbs=c("gfb", "pht", "fos"), trawl="bottom", spath=.getSpath(),
   pyrs=1996:2010, rda=NULL, ioenv=.GlobalEnv)
}
\arguments{
  \item{strSpp}{string Hart code for a target fish species.}
  \item{major}{numeric codes for a set of PMFC major areas.}
  \item{mindep}{numeric specifying the minimum depth that defines \code{strSpp}'s depth-of-occurrence.}
  \item{maxdep}{numeric specifying the maximum depth that defines \code{strSpp}'s depth-of-occurrence.}
  \item{dbs}{string vector codes that specify particular DFO databases; currently uses: \cr
    \code{"gfb"} = GFBioSQL, \code{"pht"} = PacHarvTrawl, \code{"fos"} = GFFOS. }
  \item{trawl}{string specifying trawl type -- either \code{"bottom"} or \code{"midwater"}. }
  \item{spath}{path where SQL files are located; defaults to the package repository of SQL code.}
  \item{pyrs}{numeric vector of years used to plot the results.}
  \item{rda}{string name (without extension) of an R binary file (\code{.rda}) containing an array of catches called \code{bycatch}.}
  \item{ioenv}{input/output environment for function input data and output results.}
}
\details{
  The function currently uses three SQL query files called \code{gfb_bycatch.sql}, 
  \code{pht_bycatch.sql}, and \code{fos_bycatch.sql}, which tap into the DFO databases 
  GFBioSQL, PacHarvTrawl, and GFFOS, respectively.
  
  The function displays two barplots of annual catch, absolute and relative, 
  where bars are sectioned by fish groups.
}
\value{
  If \code{rda=NULL}, then three SQL queries extract annual fish group catches from the databases mentioned above.
  These catches are transferred to a 3-dimensional array \code{bycatch}: \cr
  \code{year...} years spanning all datasets; \cr
  \code{fish...} fish groups: POP, rockfish, turbot, flatfish, hake, sharks, other; \cr
  \code{db.....} DFO databases: \code{gfb} = GFBioSQL, \code{pht} = PacHarvest, \code{fos} = GFFOS. \cr
  The catch array is saved to an \code{.rda} file with a name that indicates major areas and trawl type (bottom or midwater).
  
  If the argument \code{rda} is given an \code{.rda} name, the \code{bycatch} array is loaded, by-passing the SQL calls.

  The \code{bycatch} array is returned invisibly. \cr
  Additionally, a list object called \code{PBStool}, located in the user's global environment, is populated with: \cr
  \code{module.....} the name of the \pkg{PBStools} module to which this function belongs; \cr
  \code{call.......} the call to the function \code{trackBycat}; \cr
  \code{plotname...} name of the plot, should the user which to use it, e.g. \code{.plotDev(act="png")}; \cr
  \code{bartab.....} matrix of catch (tonnes) by year and fish group, summed over the databases; \cr
  \code{amat.......} matrix of absolute catch (kt) used in the upper barplot; \cr
  \code{rmat.......} matrix of relative catch (0:1) used in the lower barplot; \cr
  \code{clrs.......} vector of colour names to distinguish the fish groups within any one bar.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-27}
}
\seealso{
  \code{\link[PBStools]{getData}}, \code{\link[PBStools]{getCatch}}, \code{\link[PBStools]{buildCatch}} \cr
  Available SQL queries: \code{\link[PBStools]{SQLcode}}
}
\keyword{hplot}
\keyword{array}
\concept{M03_Fishery}
