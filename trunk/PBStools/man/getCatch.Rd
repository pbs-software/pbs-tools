\name{getCatch}
\alias{getCatch}
\title{
  Get Catch from Various Databases and Create One Dataset
}
\description{
  Extract catch records for a species from various databases and 
  combine them into one catch file.
}
\usage{
getCatch(strSpp="394", dbs=c("gfb","gfc","pht","fos"),
   sql=FALSE, proBio=TRUE, uid=Sys.info()["user"], pwd=uid,
   ioenv=.GlobalEnv)
}
\arguments{
  \item{strSpp}{character string specifying a species code.}
  \item{dbs}{character string vector of 3-letter codes specifying DFO databases.
    Currently, only the following are allowed: \cr
    \code{"gfb"} = \code{GFBioSQL} (survey catch data), \cr
    \code{"gfc"} = \code{GFCatch} (primarily trawl data), \cr
    \code{"pht"} = \code{PacHarvest} (trawl data only), \cr
    \code{"fos"} = \code{GFFOS} (all fisheries).}
  \item{sql}{logical: if \code{TRUE}, grab the pre-queried catch files stored as binaries.}
  \item{proBio}{logical: if \code{TRUE} run the function \code{processBio} on 
    the combined dataset to add fields: \cr
    \code{srfa} = slope rockfish assessment areas, \cr
    \code{srfs} = slope rockfish assessment subareas or gullies, \cr
    \code{popa} = Pacific ocean perch areas.}
  \item{uid, pwd}{user ID and password for Oracle DB account authentication.}
  \item{ioenv}{input/output environment for function input data and output results.}
}
\details{
  This function provides a quick method of extracting contiguous datasets
  and combining them into one data object. The primary use for this dataset is
  to create the catch input for the function \code{weightBio}.
  Currently, \code{getCatch} only functions for the trawl fishery as the 
  hook and line databases are messy and not contiguous. For a comprehensive
  catch reconstruction, see \code{buildCatch}.
}
\value{
  Returns a data object of survey catch records \code{Scat123.wB} and commercial 
  catch records \code{Ccat123.wB} resulting from the merge of query results to 
  various DFO databases. The results are also written to system binary files
  \code{Scat123.wB.rda, Ccat123.wB.rda}, where \code{123} specifies the species code and \code{wB}
  specifies creation for use in the function \code{weightBio}.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link[PBStools]{buildCatch}}, \code{\link[PBStools]{processBio}}, \code{\link[PBStools]{weightBio}} \cr
  Available SQL queries: \code{\link[PBStools]{SQLcode}}
}
\keyword{manip}
\keyword{database}
\keyword{datagen}

