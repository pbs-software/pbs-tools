\name{spooler}
\alias{spooler}
\title{Spool List Object Information Into New Fields}
\description{
  Spool a named list or list of lists into a new field of an existing 
  data frame.
}
\usage{
spooler(xlist, newfld="area", target)
}
\arguments{
  \item{xlist}{Named list or list of lists.}
  \item{newfld}{String name of a new field to be created in the target data frame.}
  \item{target}{Target data frame object.}
}
\details{
  Spool a named list or list of lists into a new field of an existing 
  data frame where the names of the lists match existing fields in the 
  target data frame. The elements of the list are values to match in the 
  existing fields. The values placed in \code{newfld} are either matched
  values (if a simple list is supplied) or list names concatenated with 
  matched values (if a list of lists is supplied).

  This function is most useful when multiple fields exist that describe a 
  similar concept. For example \code{area} might be any combination of the fields
  \code{major}, \code{minor}, \code{locality}, \code{srfa}, \code{srfs}, and 
  \code{popa}. Obviously some of these overlap and the user has to be mindful
  of which combinations to collapse into one field.  If multiple matches are 
  available, the code gives preference to the first search field in the list.
}
\value{
  The \code{target} data frame augmented with the new field \code{newfld}.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2021-06-15}\cr
}
\seealso{
  \code{\link{biteData}}, \code{\link{compCsum}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(){
  data(pop.age,envir=.PBStoolEnv)
  temp=pop.age[sample(1:nrow(pop.age),10),]
  spooler(list(major=list(3:4,7:8),srfs=list("GS","MI","MR")),"area",temp)
  print(temp)
  invisible() }
pbsfun()
})
}
\keyword{data}
\keyword{utilities}
\concept{M01_Utility}
