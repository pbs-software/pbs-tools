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
   K=",", exInt=TRUE, use.round=FALSE, scipen=20)
}
\arguments{
  \item{dat}{\code{numeric} -- data frame/matrix/vector of raw (catch) numbers}
  \item{N}{\code{numeric} -- number of significant digits}
  \item{X}{\code{numeric} -- numeric vector of columns to exclude (set to \code{NULL} or \code{0} if selecting all columns).}
  \item{zero}{\code{character} -- string replacement for zero-values}
  \item{na}{\code{character} -- string replacement for \code{NA} values}
  \item{K}{\code{character} -- big mark separator (see \code{format})}
  \item{exInt}{\code{logical} -- if \code{TRUE}, exclude integers from manipulation to signicant digits.}
  \item{use.round}{\code{logical} -- if \code{TRUE}, round the values to \code{N} decimal places rather than provide \code{N} significant figures.}
  \item{scipen}{\code{numeric} -- penalty to be applied when printing numeric values in fixed or exponential notation; 
    positive values bias towards fixed, negative values towards scientific notation: fixed notation will be preferred 
    unless it is more than \code{scipen} digits wider.}
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
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-27}
}
\note{
  If the input table has no \code{dimnames}, one will be added with format \cr
  \code{list(paste("R",1:nrow(dat),sep=""),paste("C",1:ncol(dat),sep=""))}
  
  Function will accept a vector of numerics, which will be converted to a 
  one-column data frame.
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{buildCatch}},
  \code{\link[PBStools]{dumpMod}},
  \code{\link[PBStools]{plotRecon}},
  \code{\link[PBStools]{makeLTH}},
  \code{\link[PBStools]{texArray}}
}
\examples{
local(envir=.PBStoolEnv,expr={
  print(formatCatch(genMatrix(20,5),X=0))
})
}
\keyword{manip}
\keyword{character}
\concept{M08_LaTeX}
