\name{plotLocal}
\alias{plotLocal}
\title{
  Plot Catch by Locality
}
\description{
  Plot DFO fishing localities with the highest catch and colour code accordingly.
}
\usage{
plotLocal(dat, area, aflds=NULL, pcat=0.95, showAll=FALSE,
   fid=NULL, fidtype="PBStools", strSpp, Ntop=5, years, short=TRUE,
   plot=TRUE, png=FALSE, csv=FALSE, outnam="refA439", lang=c("e","f"))
}
\arguments{
  \item{dat}{\code{data.frame} -- data object containing catch information for a species and 
    one or more fields with area code information (e.g., \code{locality}).}
  \item{area}{\code{data.frame} -- a \code{PBSmapping} PolySet of areas for summarising catch data in \code{dat}.}
  \item{aflds}{\code{character} -- field names common to \code{dat} and \code{area} that can be used to 
    create a unique index for cross-referencing the two objects.}
  \item{pcat}{\code{numeric} -- cumulative proportion of catch to identify and display}
  \item{showAll}{\code{logical} -- if \code{TRUE}, show all available locality boundaries.}
  \item{fid}{\code{integer} -- specific set of fishery ID numbers to process.}
  \item{fidtype}{\code{character} -- default = \code{"PBStools"}, alternative = \code{"GFFOS"} fishery types.}
  \item{strSpp}{\code{character} -- species Hart code; if supplied, label on plot
    indicates species name and data date range; 
    required argument if using a map \code{EventData} \code{dat} object created by \code{fos_map_density.sql}.}
  \item{Ntop}{\code{numeric} -- top \code{N} localities to show in figure legend and to label on figure.}
  \item{years}{\code{numeric} -- vector of years to restrict the time period for the data summary.}
  \item{short}{\code{logical} -- if \code{TRUE}, use short file names (e.g., \code{"T"} instead of \code{"Trawl"}).}
  \item{plot}{\code{logical} -- if \code{TRUE}, plot the results.}
  \item{png}{\code{logical} -- if \code{TRUE}, send the plot results to a \code{.png} file.}
  \item{csv}{\code{logical} -- if \code{TRUE}, send the output to a \code{.csv} file.}
  \item{outnam}{\code{character} -- output prefix for \code{.png} and \code{.csv} file names.}
  \item{lang}{\code{character} -- a vector of letters that denote the language for output:
    currently only \code{"e"} (english) and \code{"f"} (french).}
}
\details{
  The function is designed primarily to derive DFO fishing localities that have the
  highest catch by fishery ID (\code{fid}):
  \itemize{
    \item \code{1 = Trawl}
    \item \code{2 = Halibut}
    \item \code{3 = Sablefish}
    \item \code{4 = Dogfish/Lingcod}
    \item \code{5 = H&L Rockfish}
  }
  The plots colour code the catch from the highest value displayed in \code{"red"}
  to zero catch in \code{"lightblue1"}, even though zero-catches are 
  removed from the PolyData set. The top 5 catches (in tonnes) by 
  locality are reported in the upper right corner of each figure.
}
\value{
  A named list of PolyData data frames, where the names are fishery ID code.
  The PolyData sets summarize catch by locality and exclude areas with zero catch.

  The results can be plotted and/or saved to individual \code{.csv} files based on
  the number of unique \code{fid} values in \code{dat} or \code{fid} codes specified by the user.
  The \code{.csv} files can be used passed as arguments to \code{refarea} in the function \code{buildCatch}.
}
\note{
  To see all catches, set \code{pcat=1}.\cr
  To see all \code{locality} boundaries, set \code{showAll=TRUE}.\cr
  Be aware that catches not falling into specific locality boundaries
  will \bold{not} be represented in the plot or in the results file.
}

\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{opus locus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2018-08-14}
}
\seealso{
\code{\link[PBStools]{buildCatch}},
\code{\link[PBStools]{calcSRFA}},
\code{\link[PBStools]{calcStockArea}},
\code{\link[PBStools]{plotCatch}},
\code{\link[PBStools]{plotConcur}},
\code{\link[PBStools]{plotGMA}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}% use one of  RShowDoc("KEYWORDS")
\keyword{manip}% __ONLY ONE__ keyword per line
