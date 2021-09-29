\name{biteData}
\alias{biteData}
\title{Subset Data Matrix/Frame Using a Vector Object}
\description{
  Subset a matrix or data frame using a vector object with the same name
  as one of the fields in the data matrix/frame.
}
\usage{
biteData(dat, vec)
}
\arguments{
  \item{dat}{matrix or data frame with named fields (row names).}
  \item{vec}{vector object of values to match in the field of \code{dat} with 
    the same name as \code{vec} \cr (e.g., \code{major=c(3:9)}).}
}
\details{
  The data matrix/frame is subset using the values in \code{vec} and the 
  subset data matrix/frame is returned. If there are no fields with the name
  of \code{vec}, the data matrix/frame is returned unaltered.
  
  You cannot supply an \emph{ad hoc} vector directly in the argument; 
  \code{vec} must be an existing object.\cr \cr
  NO CAN DO: \code{biteData(swiss,Education=1:5)} \cr \cr
  CAN DO: \code{Education=1:5; biteData(swiss,Education)}
}
\value{
  A subset of \code{dat} after qualifying by \code{vec}.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2021-06-15}\cr
}
\seealso{ 
  \code{\link{chewData}}, \code{\link{getData}}, \code{\link{getFile}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(){
  cat("Records in original swiss object by Education\n")
  print(sapply(split(swiss$Education,swiss$Education),length))
  Education=1:5; test=biteData(swiss,Education)
  cat("Records in swiss object subset where Education=1:5\n")
  print(sapply(split(test$Education,test$Education),length))
  invisible() }
pbsfun()
})
}
\keyword{data}
\keyword{utilities}
\concept{M01_Utility}
