\name{createFdir}
\alias{createFdir}
\alias{createTdir}
\title{
  Create Figure/Table Directories
}
\description{
  Create \code{english}/\code{french} subdirectories in the working directory
  where figures are deposited after creation.

  Ditto \code{tables} and \code{data} subdirectories.
}
\usage{
createFdir(lang, dir=".")
createTdir(dir=".")
}
\arguments{
  \item{lang}{\code{character} -- a vector of letters that denote the language for output:
    currently only \code{"e"} (english) and \code{"f"} (french)}
  \item{dir}{\code{character} -- directory in which figure/table subdirectories
    are created (if not already present)}
}
\details{
  Un petit fonction de convenience when creating french (and now english) figures via
  the function \code{linguaFranca}.
}
\value{
  Returns rien but creates subdirectories called \code{english}, \code{french},
  \code{tables}, and \code{data}.
}
\note{
  Started life as `create French directory'.
}
\references{
\href{http://www.dfo-mpo.gc.ca/csas-sccs/process-processus/translation-traduction-eng.html}{%
CSAS Policy on the translation of documents}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-12-18}
}
\seealso{
\code{\link[PBStools]{confODBC}},
\code{\link[PBStools]{countLines}},
\code{\link[PBStools]{createDSN}},
\code{\link[PBStools]{getODBC}},
\code{\link[PBStools]{linguaFranca}},
\code{\link[PBStools]{subsetFile}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
\concept{M01_Utility}
