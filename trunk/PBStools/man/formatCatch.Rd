\name{formatCatch}
\alias{formatCatch}
\title{
  Format Numeric Catch Values as Strings
}
\description{
  Format a table of numeric values so that each cell displays \emph{N}
  significant figures in character format.
}
\usage{
formatCatch(dat, N=3, X=0, zero="0", na="---",
   K=",", exInt=TRUE)
}
\arguments{
  \item{dat}{data frame/matrix/vector of raw (catch) numbers}
  \item{N}{number of significant digits}
  \item{X}{numeric vector of columns to exclude (set to \code{NULL} or \code{0} if selecting all columns)}
  \item{zero}{character replacement for zero-values}
  \item{na}{character replacement for \code{NA} values}
  \item{K}{big mark separator (see \code{format})}
  \item{exInt}{logical: exclude integers from manipulation to signicant digits}
}
\details{
  The algorithm relies on string manipulation to achieve a result that
  cannot be obtained by the \pkg{base} package's \code{format} function alone.
  The formatted table can then be passed to \pkg{xtable}'s function 
  \code{xtable}, which produces output that can be passed to \code{print.xtable} for use in LaTeX.
}
\value{
  A data frame of formatted string values with dimensions and 
  \code{dimnames} of the input matrix or data frame.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\note{
  If the input table has no \code{dimnames}, one will be added with format \cr
  \code{list(paste("R",1:nrow(dat),sep=""),paste("C",1:ncol(dat),sep=""))}
  
  Function will accept a vector of numerics, which will be converted to a 
  one-column data frame.
}
\seealso{
  \code{\link[PBStools]{buildCatch}}, \code{\link[PBStools]{dumpMod}}, 
  \code{\link[PBStools]{plotRecon}}, \code{\link[PBStools]{makeLTH}},
  \code{\link[PBStools]{texArray}}
}
\examples{
local(envir=.PBStoolEnv,expr={
  print(formatCatch(genMatrix(20,5),X=0))
})
}
\keyword{manip}
\keyword{character}

