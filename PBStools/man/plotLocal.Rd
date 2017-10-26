\name{plotLocal}
\alias{plotLocal}
\title{
Plot Locality Catch
}
\description{
  Plot DFO fishing localities with the highest catch and colour code accordingly.
}
\usage{
plotLocal(dat, area="locality", aflds=NULL, outnam="refA439")
}
\arguments{
  \item{dat}{data file containing catch information for a species and 
    one or more fields with area code information (e.g., \code{locality}).}
  \item{area}{area PolySet for summarising catch data in \code{dat}.}
  \item{aflds}{field names in \code{dat} and \code{area} that can be used to 
    create a unique index for cross-referencing the two objects.}
  \item{outnam}{output name for \code{.csv} file summarising catch in \code{area}.}
}
\details{
  The function is designed primarily to derive DFO fishg localities that have the
  highest catch by fishery ID (\code{fid}):
  \itemize{
    \item \code{1 = Trawl}
    \item \code{2 = Halibut}
    \item \code{3 = Sablefish}
    \item \code{4 = Dogfish/Lingcod}
    \item \code{5 = H\&L Rockfish}
  }
}
\value{
  The results are plotted and saved to individual \code{.csv} files based on
  the number of unique \code{fid} values in \code{dat}.
}
\note{
  Primarily used to create \code{.csv} files for argument \code{refarea}
  in function \code{buildCatch}.
}
\author{
  Rowan Haigh, Research Biologist\cr
  Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
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
