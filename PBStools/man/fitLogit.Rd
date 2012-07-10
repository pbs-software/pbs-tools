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
  Rob Kronlund, Pacific Biological Station, Nanaimo BC
}
\seealso{
  \code{\link{estOgive}}
}
\keyword{distribution}
\keyword{regression}

