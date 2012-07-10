\name{getFile}
\alias{getFile}
\title{Get Data from Binaries or ASCII}
\description{
  Attempt to get data from the specified sources (without specifying the extension).
}
\usage{
getFile(..., list=character(0), path=getwd(), scope="L", 
     use.pkg=FALSE, reload=FALSE, try.all.frames=FALSE,
     use.all.packages=FALSE)
}
\arguments{
  \item{...}{a sequence of names or literal character strings.}
  \item{list}{a character vector of potential objects.}
  \item{path}{string specifying path to data files \code{*.rda} and \code{*.r}.}
  \item{scope}{string specifying either local \code{"L"} or global 
    \code{"G"} placement (destination) of data file.}
  \item{use.pkg}{logical: if \code{TRUE} ignore binaries called by the \code{data} command.}
  \item{reload}{logical: if \code{TRUE}, force a reloading of the data object into R's memory.}
  \item{try.all.frames}{logical: if \code{TRUE}, look for named object(s)
    in all frames, starting from the parent frame and working backwards.}
  \item{use.all.packages}{logical: if \code{TRUE}, look for data binaries in all 
    the packages installed in the user's R library folder.}
}
\details{
  If a data object with this name exists in the specified target environment (local or 
  global), the function will not attempt to reload it unless the users specifies 
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

  The choice \code{scope="L"} puts the data file in \code{parent.frame(1)} while 
  \code{scope="G"} puts the data file in \code{.GlobalEnv}.
}
\value{
  No value is explicitly returned, data file is retrieved locally or globally.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\seealso{ 
  \code{\link{getData}}, \code{\link{getName}}
}
\keyword{ data }
\keyword{ utilities }
