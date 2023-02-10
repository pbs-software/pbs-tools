\name{countLines}
\alias{countLines}
\title{
  Count Lines in Text File
}
\description{
  Count the number of lines in an ASCII file.
}
\usage{
countLines(fnam, os=.Platform$OS.type)
}
\arguments{
  \item{fnam}{character string denoting the full name of the file, including extension.}
  \item{os}{operating system -- currently only Windows supported.}
}
\details{
  The function basically constructs a Microsoft command line string that uses
  Windows native functions \code{findstr} and \code{find}, which is then passed
  to the R function \code{shell}.
}
\value{
  Returns a numeric value that reports the number of lines in the specified file.
}
\references{
\href{https://brett.batie.com/scripting/count-number-of-lines-in-a-file-using-dos/}{Count number of lines in a file using DOS}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Regional Headquarters (RHQ), Vancouver BC\cr
  Last modified \code{Rd: 2023-02-09}
}
\seealso{
  \code{\link[PBStools]{subsetFile}}, \code{\link[PBStools]{collectFigs}}, 
  \code{\link[PBStools]{formatCatch}}, \code{\link[PBStools]{makeLTH}}
}
\keyword{sysdata}
\keyword{character}
\concept{M01_Utility}

