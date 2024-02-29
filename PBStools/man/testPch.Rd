\name{testPch}
\alias{testPch}
\title{Display Plotting Symbols or Octal Strings}

\description{
  Display plotting symbols or octal strings. 
  User can specify particular ranges (increasing continuous integer) for \code{pch}.
}
\usage{
testPch(pch=1:200, ncol=10, grid=TRUE, newframe=TRUE, octal=FALSE, ...)
}
\arguments{
  \item{pch}{symbol codes or octal string numbers.}
  \item{ncol}{number of columns in display (can only be 2, 5, or 10). Most 
    sensibly this is set to 10.}
  \item{grid}{logical: if \code{TRUE}, grid lines are plotted for visual aid.}
  \item{newframe}{logical: if \code{TRUE} reset the graph, otherwise overlay 
    on top of the current graph.}
  \item{octal}{logical: if \code{TRUE}, show octal strings (backslash characters) used in text 
    statements (e.g., \code{"30\\272C"} = 30\eqn{^\circ}{ degrees}C).}
  \item{...}{additional arguments for functions \code{points} or \code{text}.}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2023-10-18}
}
\note{
  This function was removed from \pkg{PBSmodelling} at the request of Prof. Ripley:\cr\cr
  \emph{
    Warning in text.default(x, y, cset, cex = 1.5, ...) : conversion failure
    on `<80<80>' in `mbcsToSbcs': dot substituted for <80>
    in a UTF-8 locale (which almost everyone uses nowadays).  What `testPch'
    does may have made a little sense in a 8-bit locale, but none in a
    multi-byte locale, and you are simply sending lots of invalid inputs to text(). }

    \emph{Please remove the function: I have wasted several hours today on the  reporting
    of invalid inputs so it does not crash the R session on platforms with unusual iconv functions.}

  Debugged function `testPch' after R (probably starting in R.4.4.0) reduced its rendering
  of pch symbols and octal strings. (RH 230726).
  Moved `testPch' to non-CRAN package `PBStools'.
}
\seealso{
In package \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{testLty}},
  \code{\link[PBSmodelling]{testLwd}},
  \code{\link[PBSmodelling]{addLabel}}
}
\examples{
\dontrun{
local(envir=.PBSmodEnv,expr={ testPch(123:255) })
local(envir=.PBSmodEnv,expr={ testPch(1:25,ncol=5) })
local(envir=.PBSmodEnv,expr={ testPch(41:277,octal=TRUE) })
}
}
\keyword{utilities}
\keyword{color}
\concept{M01_Utility}

