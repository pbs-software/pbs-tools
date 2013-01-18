\name{getFile}
\alias{getFile}
\title{Get Data from Binaries or ASCII}
\description{
  Attempt to get data from the specified sources (without specifying the extension).
}
\usage{
getFile(..., list=character(0), path=getwd(), tenv=.GlobalEnv, 
     use.pkg=FALSE, reload=FALSE, try.all.frames=FALSE,
     use.all.packages=FALSE)
}
\arguments{
  \item{...}{a sequence of names or literal character strings.}
  \item{list}{a character vector of potential objects.}
  \item{path}{string specifying path to data files \code{*.rda} and \code{*.r}.}
  \item{tenv}{target environment to which the specified object(s) will be sent.}
  \item{use.pkg}{logical: if \code{TRUE} look for data binaries in loaded packages.}
  \item{reload}{logical: if \code{TRUE}, force a reloading of the data object into R's memory.}
  \item{try.all.frames}{logical: if \code{TRUE}, look for named object(s)
    in all frames, starting from the parent frame and working backwards.}
  \item{use.all.packages}{logical: if \code{TRUE}, look for data binaries in all 
    the packages installed in the user's R library folder.}
}
\details{
  If a data object with this name exists in the specified target environment, 
  the function will not attempt to reload it unless the users specifies 
  \code{reload=TRUE}. Failing to find an R data object in memory, the function tries the binary
  libraries for package data objects, but only if the user specifies \code{use.pkg=TRUE}.
  Even with this option, the code only looks in those packages that are currently loaded
  and attached. If the user wishes to search all R libraries installed on his system, 
  he must further specify \code{use.all.packages=TRUE}. Searching through all libraries can 
  be slow if the user has installed many packages.

  Thereafter, the code searches for remote data files in the following order: 
  (i) local binary files with the extension \code{.rda}, (ii) ASCII data files created 
  using \code{dump} and with the extension \code{.r}, (iii) comma-data delimited files 
  \code{.csv}, (iv) and ordinary text files \code{.txt}.  
}
\value{
  No value is explicitly returned, data file is retrieved and placed in the target environment.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans, Nanaimo BC
}
\seealso{ 
  \code{\link{getData}}, \code{\link{getName}}
}
\keyword{ data }
\keyword{ utilities }

