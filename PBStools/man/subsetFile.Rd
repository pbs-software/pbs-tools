\name{subsetFile}
\alias{subsetFile}
\title{
  Subset ASCII File Every n Rows.
}
\description{
  Subset an ASCII file every \emph{n} rows. 
  Used mainly to subsample a large dataset before importing to R.
}
\usage{
subsetFile(fnam, enrow=30, header=TRUE, os=.Platform$OS.type)
}
\arguments{
  \item{fnam}{character string denoting the full name of the file, including extension.}
  \item{enrow}{periodic number of rows to sample, starting at \code{enrow} then every \code{enrow} rows.}
  \item{header}{logical: if \code{TRUE}, file contains a header line, and sampling starts at \code{enrow + 1}.}
  \item{os}{operating system -- currently only Windows supported.}
}
\details{
  The function basically constructs a Microsoft command line string that uses the 
  Rtools UNIX utility \code{sed} (stream editor). The function \code{subsetFile} assumes
  that the Windows directory leading to \code{sed} is on the path, i.e., available from the 
  command line in R (check using \code{Sys.getenv()["PATH"]}). 

  An example of a command constructed by \code{subsetFile} might be: \cr
  \code{sed -n -e 1p -e '31~30p' infile.txt > sub_infile.txt}
}
\value{
	No value is returned. Function creates a file in the user's working directory 
	that is a subset of the specified file.
}
\references{
\href{https://linuxcommando.blogspot.com/2008/04/use-sed-or-perl-to-extract-every-nth.html}{Linux Commando}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus: Institute of Ocean Sciences (IOS), Sidney BC}\cr
  Last modified \code{Rd: 2018-07-03}\cr
}
\seealso{
  \code{\link[PBStools]{countLines}}, \code{\link[PBStools]{collectFigs}}, 
  \code{\link[PBStools]{formatCatch}}, \code{\link[PBStools]{makeLTH}}
}
\keyword{sysdata}
\keyword{character}

