\name{sumBioTabs}
\alias{sumBioTabs}
\title{
  Summarize Frequency of Biological Samples
}
\description{
  Summarize frequency occurrence of biological samples
  and specimens, and send output to a data table.
}
\usage{
sumBioTabs(dat, fnam="sumBioTab.csv", samps=TRUE, specs=TRUE,
     facs=list(c("year","major"), c("year","ttype"), c("year","stype"),
     c("year","ameth")) )
}
\arguments{
  \item{dat}{data object with biological sample and specimen information (e.g., \code{pop.age}).}
  \item{fnam}{string name of the output file to which summary tables are sent.}
  \item{samps}{logical, if \code{TRUE}, report the number of samples.}
  \item{specs}{logical, if \code{TRUE}, report the number of specimens.}
  \item{facs}{list of factors in pairs (two-element string vectors).}
}
\value{
  No value is returned to the R console. The output is placed into the file name specified.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2019-03-22}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{sumCatTabs}},
  \code{\link[PBStools]{processBio}} \cr
In package \pkg{base}:\cr
  \code{\link[base]{table}} \cr
In package \pkg{reshape}:\cr
  \code{melt.data.frame},
  \code{cast}
}
\keyword{manip}
\keyword{file}

