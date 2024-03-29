\name{splitTab}
\alias{splitTab}
\title{
  Split a Table Sideways
}
\description{
  Split a long data table into side-by-side pieces for printing in LaTeX.
}
\usage{
splitTab(tab, np=3, row.names=TRUE, row.label="row", row.numeric=FALSE)
}
\arguments{
  \item{tab}{Two-dimensional data table (matrix or data.frame).}
  \item{np}{Number of pieces to split table into.}
  \item{row.names}{\emph{logical} -- If \code{TRUE}, include the row names as fields.}
  \item{row.label}{Label to assign the row name fields (will be repeated \code{np} times).}
  \item{row.numeric}{\emph{logical} -- If \code{TRUE}, convert the row names into numeric values.}
}
\details{
  The function is designed to reduce wasted space when building LaTeX tables.
  Essentially rather than printing a long skinny table, break it up into \code{np}
  pieces and place side-by-side.
}
\value{
  A new data frame that is shorter and wider than the input table.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-27}
}
\seealso{
  \code{\link[PBStools]{texArray}},
  \code{\link[PBStools]{makeLTH}},
  \code{\link[PBStools]{gatherVals}},
  \code{\link[PBStools]{crossTab}}
}
\keyword{manip}
\concept{M08_LaTeX}
