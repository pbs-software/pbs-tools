\name{plotFOScatch}
\alias{plotFOScatch}
\title{Plot Monthly Catch from FOS as Barplots}
\description{
  Plot monthly catch of specified species from FOS query 
  \code{fos_catch.sql} as bars stacked by PMFC area.
}
\usage{
plotFOScatch(strSpp="453", majors=c(1,3:9), space=0.5,
   fplot="Groundfish Trawl", xlim=c("2007-04","2011-06"),
   fyrM1=4, uid=Sys.info()["user"], pwd=uid, wmf=FALSE, ...)
}
\arguments{
  \item{strSpp}{string specifying species code (\code{species$code})}
  \item{majors}{major PMFC area codes (\code{pmfc$major})}
  \item{space}{space between bars of plot}
  \item{fplot}{name of fishery to plot: \cr
    \code{"Groundfish Trawl"} \cr
    \code{"Halibut Longline"} \cr
    \code{"Sablefish Trap"} \cr
    \code{"Dogfish & Lingcod"} \cr
    \code{"Rockfish H&L"} }
  \item{xlim}{character: lower and upper x-limits of the plot specified as categories \code{"YYYY-MM"}}
  \item{fyrM1}{numeric: first month of a fishing year (assumed to run at 12-month intervals)}
  \item{uid, pwd}{user ID and password for Oracle DB account authentication.}
  \item{wmf}{logical: if \code{TRUE}, send plot to a \code{WMF} file}
  \item{...}{additional arguments for \code{plotFOScatch()}}
}
\details{
  The function uses the SQL query \code{fos_catch.sql} to obtain
  catch from the main groundfish fisheries, and the user can specify which fishery 
  to display. Barplot catch categories are combinations of 
  \code{year-month}, with catch from PMFC areas stacked in each 
  category. The total catch is reported at the end of each fishing year.
  
  Additionally catch summaries by fishery, year, and PMFC area are dumped
  to a file called \code{catFOSfyr.csv}.
  
  Internally derived objects of interest (such as catch-by-month and catch-by-year
  matrices) are save in the global list object \code{PBSfish}.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link[PBStools]{getFile}}, \code{\link[PBSdata]{fos.fid}}, 
  \code{\link[PBSdata]{species}}, \code{\link[PBSdata]{pmfc}} \cr
  Available SQL queries: \code{\link[PBStools]{SQLcode}}
}
\keyword{hplot}
