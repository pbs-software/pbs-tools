\name{installPkgs}
\alias{installPkgs}
\title{
  Install R Packages
}
\description{
  Install specified packages if they are missing or if newer versions are available.
}
\usage{
installPkgs(pkg, repos=getOption("repos"), locdir=tempdir(), also.load=FALSE, ...)
}
\arguments{
  \item{pkg}{Character vector of package names.}
  \item{repos}{Character vector specifying the base URL(s) of the repositories to use,
    e.g., the URL of a CRAN mirror such as \url{https://cran.r-project.org/}.
    If \code{repos=NULL}, then \code{installPkgs} looks for binary zip packages
    in the directory \code{locdir}.}
  \item{locdir}{Character value specifying the directoriy in which local zip files reside.}
  \item{also.load}{Logical: if \code{TRUE}, the specified packages are also loaded (attached).}
  \item{\dots}{Additional arguments passed to the \code{install.packages} function.}
}
\details{
  The function serves to make life easier when updating packages, especially
  when more than one version of R is installed and the user wants to install
  from binaries downloaded and saved previously.
  This can save time when network connections are painfully slow.
}
\value{
  Invisibly returns the versions of currenly avaliable packages from
  the repository or in the local directory containing zip files.
}
\references{
  Function inspired by code posted on GitHub by 
  \href{https://gist.github.com/stevenworthington/3178163}{Steve Worthington}.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2021-06-15}
}
\note{
  At some point the function should deal with attached packages (use \code{sessionInfo}).
}
\seealso{
  \code{\link[PBStools]{countLines}},
  \code{\link[PBStools]{getData}},
  \code{\link[PBStools]{getFile}},
  \code{\link[PBStools]{getODBC}},
  \code{\link[PBStools]{subsetFile}},
}
\keyword{data}
\keyword{package}
\concept{M01_Utility}

