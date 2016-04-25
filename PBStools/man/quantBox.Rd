\name{quantBox}
\alias{quantBox}
\title{
  Redefine Boxplot to Show Quantiles
}
\description{
  Hijack boxplot code and replace \code{boxplot.stats} with \code{myboxplot.stats}.
}
\usage{
quantBox(x, use.cols = TRUE, ...)
}
\arguments{
  \item{x}{Data from which quantile plots are created.
    Either a numeric vector, a numeric matrix, or a single list containing numeric vectors.}
  \item{use.cols}{logical indicating if columns (by default) or rows (use.cols = FALSE) should be plotted.}
  \item{\dots}{Further arguments to \code{\link[graphics]{boxplot}}.}
}
\details{
  The objects \code{myboxplot.stats} and \code{qboxplot} (boxplot modified to render quantiles)
  are located in the package environment: \code{.PBStoolEnv}. The function \code{quantBox}
  grabs \code{qboxplot} using the accessor function \code{ttget}.
}
\value{
  Invisibly returns a list object containing:
  \tabular{ll}{
    \code{stats} \tab matrix of quantiles,\cr
    \code{n}     \tab a vector with the number of observations in each group,\cr
    \code{conf}  \tab a matrix where each column contains the lower and upper extremes of the notch;\cr
                 \tab \code{stats[3] + c(-1.58, 1.58) * diff(stats[c(2, 4)])/sqrt(n)}\cr
    \code{out}   \tab the values of any data points which lie beyond the extremes of the whiskers,\cr
    \code{group} \tab a vector of the same length as \code{out} whose elements indicate to which group the outlier belongs,\cr
    \code{names} \tab names for each box plotted.\cr
  }
}
\references{
  \url{http://r.789695.n4.nabble.com/Box-plot-with-5th-and-95th-percentiles-instead-of-1-5-IQR-problems-implementing-an-existing-solution-td3456123.html}
}
\author{
  Rowan Haigh, Research Biologist,\cr
  Fisheries and Oceans Canada, Pacific Biological Station, Nanaimo BC
}
\note{
  Why would anyone choose a boxplot over a quantile plot?
}
\seealso{
  \code{\link[graphics]{boxplot}},
  \code{\link[graphics]{boxplot.matrix}},
  \code{\link[PBStools]{ttget}},
  \code{\link[PBStools]{histMetric}},
  \code{\link[PBStools]{mapMaturity}},
  \code{\link[PBStools]{plotTernary}}
}
\examples{
local(envir=.PBStoolEnv,expr={
  rien = list('1997'=rnorm(100,10),'2005'=rnorm(100,12),'2016'=rnorm(100,14))
  quantBox(rien,outline=FALSE)
})
}
\keyword{hplot}

