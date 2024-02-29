\name{zapDupes}
\alias{zapDupes}
\title{Zap Records with Duplicated Indices}
\description{
  Zap (remove) records from a data frame using an \code{index}
  to determine whether duplicated indexed records exist.
}
\usage{
zapDupes(dat, index)
}
\arguments{
  \item{dat}{a data frame.}
  \item{index}{a vector of field names in \code{dat} that can be used 
    to construct an index.}
}
\details{
  The function orders the data frame based on the \code{index}, then removes 
  records based on duplicated indices. There is no control over which of the
  duplicated records will be zapped. Pre-existing attributes other than 
  \code{names} and \code{row.names} are transferred to the reduced object.
}
\value{
  An input data frame less records with duplicated indices.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2021-06-15}\cr
}
\seealso{
  \code{\link{zapHoles}}, \code{\link{processBio}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(){
  data(ltmose07,envir=.PBStoolEnv)
  cat(paste("'ltmose07' has",nrow(ltmose07),"rows\n"))
  zap=zapDupes(ltmose07,"PID")
  print(zap,quote=FALSE)
  cat(paste("After zapping using index 'PID', there are",nrow(zap),"rows\n"))
  invisible() }
pbsfun()
})
}
\keyword{manip}
\concept{M01_Utility}
