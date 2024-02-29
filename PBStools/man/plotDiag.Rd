\name{plotDiag}
\alias{plotDiag}
\title{
  Plot Diagnostic Data for Catch Reconstructions
}
\description{
  Plot data components of a catch reconstruction for user inspection and diagnostic checking.
}
\usage{
plotDiag(x, description="something", col=c("red","coral","gold",
   "green2","skyblue","blue","blueviolet","purple4"), ...)
}
\arguments{
  \item{x}{numeric matrix or data frame (two-dimensional) of \code{z}-values for each \code{x} and \code{y}.}
  \item{description}{short description that will be used to name the output image file.}
  \item{col}{vector of colours to use when plotting columns of the data.}
  \item{\dots}{additional control data. Currently, the only value used is \code{type="bars"}.}
}
\details{
  The function plots \code{z}-values of \code{y} for each \code{x}. The default plot shows lines
  because a typical \code{x} comprises years. An alternative is to show the \code{z}-values as
  bars for each \code{y} grouped by \code{x}.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-27}
}
\note{
  This plot is used by the catch reconstruction algorithm which automatically increments
  the plot number \code{pD} that is logged in the global list object \code{PBSfish}.
  
  Additionally, the catch reconstruction creates a subdirectory \code{CRdiag} to which 
  \code{plotDiag} sends image files.
}
\seealso{
  The catch reconstruction function \code{\link[PBStools]{buildCatch}} and \code{\link[PBStools]{plotRecon}}
}
\keyword{hplot}
\concept{M07_CatchRecon}
