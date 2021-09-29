\name{fitLogit}
\alias{fitLogit}
\title{
  Fit Binomial Data Using Logit Function
}
\description{
  Fit binomial data using a logit link function in a \code{glm}.
}
\usage{
fitLogit(dat, yfld="pmat", xflds="age")
}
\arguments{
  \item{dat}{data frame with  binomial observations (y-values) and independent
    explanators (x-values).}
  \item{yfld}{string name of field containing binomial data.}
  \item{xflds}{string name(s) of field(s) containing explanatory data.}
}
\details{
  The data are passed to the function \code{glm} in the \code{stats} package.
}
\value{
  The function returns a GLM fit of the binomial data using the specified independent
  observations. See the 'Value' section of the \code{glm} help file.
}
\author{
  Rob Kronlund, Pacific Biological Station, Nanaimo BC\cr
  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2021-06-15}\cr
}
\seealso{
  \code{\link{estOgive}}
}
\keyword{distribution}
\keyword{regression}
\concept{M01_Utility}

