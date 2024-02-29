\name{zapSeamounts}
\alias{zapSeamounts}
\title{
  Zap Seamounts from Data Object
}
\description{
  Zap (remove) seamount records from a data object using the fields
  \code{major}, \code{minor}, and \code{locality} to identify select 
  seamounts.
}
\usage{
zapSeamounts(dat, only.mark=FALSE)
}
\arguments{
  \item{dat}{\code{data.frame} -- data object containing fields \code{major}, \code{minor}, and \code{locality}.}
  \item{only.mark}{\code{logical} -- if \code{TRUE}, do not remove seamount records
    but create a new field called \code{'seamount'} and populate it with the names
    of seamounts if records come from seamounts based on \code{major|minor|locality}.}
}
\details{
  The seamounts currently identified for removal are:
  \tabular{lrrr}{
    seamount \tab major \tab minor \tab locality\cr
    \code{bowie}      \tab  9 \tab 31 \tab 10\cr
    \code{brownbear}  \tab 67 \tab 67 \tab  2\cr
    \code{cobb}       \tab 67 \tab 67 \tab  1\cr
    \code{cowie}      \tab 10 \tab 40 \tab  5\cr
    \code{dellwood}   \tab  5 \tab 11 \tab  8\cr
    \code{durgin}     \tab 10 \tab 33 \tab  8\cr
    \code{eickelberg} \tab  3 \tab 24 \tab 12\cr
    \code{heck}       \tab  3 \tab 24 \tab 11\cr
    \code{miller}     \tab 11 \tab 42 \tab  3\cr
    \code{morton}     \tab 11 \tab 42 \tab  2\cr
    \code{murray}     \tab 10 \tab 40 \tab  4\cr
    \code{pathfinder} \tab 11 \tab 42 \tab  1\cr
    \code{pratt}      \tab 10 \tab 33 \tab  6\cr
    \code{surveyor}   \tab 10 \tab 33 \tab  7\cr
    \code{union}      \tab  4 \tab 27 \tab  5
  }
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  The input data object with seamount records removed.

  An array by year and seamount reporting number of records and total catch
  of the target species (usually RRF -- reconstructed rockfish) is
  created and attached to the output data object as an attribute called
  \code{"SMrem"} (seamounts removed).
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2020-07-16}
}
\note{
  Designed for use with \code{buildCatch}.
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{buildCatch}},
  \code{\link[PBStools]{plotDiag}},
  \code{\link[PBStools]{plotRecon}},
  \code{\link[PBStools]{surveyCatch}}
}
\keyword{manip}
\concept{seamount}
\concept{catch}

