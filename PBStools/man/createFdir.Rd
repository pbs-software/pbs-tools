\name{createFdir}
\alias{createFdir}
\title{
  Create a French Directory
}
\description{
  Create a french subdirectory in the directory (\sQuote{folder}
  to millennials) where figures are being deposited after creation.
}
\usage{
createFdir(lang, dir=".")
}
\arguments{
  \item{lang}{\code{character} -- a vector of letters that denote the language for output:
    currently only \code{"e"} (english) and \code{"f"} (french).}
  \item{dir}{\code{character} -- directory in which a subdirectory 
    called \code{french} is created (if not already present).}
}
\details{
  Un petit fonction de convenience when creating french figures via
  the function \code{linguaFranca}.
}
\value{
  Returns rien but creates a subdirecory called \code{french} (because the code is based in english).
}
\references{
  \href{http://www.dfo-mpo.gc.ca/csas-sccs/process-processus/translation-traduction-eng.html}{CSAS Policy on the translation of documents}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2020-10-08}
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
