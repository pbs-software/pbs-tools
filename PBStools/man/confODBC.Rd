\name{confODBC}
\alias{confODBC}
\title{Configure an ODBC User Data Source Name}
\description{
  Use a command line utility found in Windows OS called 'odbcconf.exe' that 
  configures an ODBC Data Source Name from the system's command line.
}
\usage{
confODBC(dsn="PacHarvest", server="GFDB", db="PacHarvest",
   driver="SQL Server", descr="", trusted=TRUE, osbit=64) 
}
\arguments{
  \item{dsn}{Data Source Name for the ODBC connection}
  \item{server}{\code{string} -- name of server that hosts the SQL Server databases}
  \item{db}{\code{string} -- name of the default database for the DSN connection}
  \item{driver}{\code{string} -- name of the ODBC driver}
  \item{descr}{\code{string} -- optional description of the DSN connection}
  \item{trusted}{\code{logical} -- if \code{TRUE}, allow SQL Server to use a trusted DFO login ID}
  \item{osbit}{\code{numeric} -- operating system architecture (32-bit or 64-bit)}
}
\details{
  \code{confODBC()} runs the Windows command line utility with the user-supplied 
  information and configures a User DSN on the user's computer.\cr\cr
  In XP, the new User DSN can be seen by navigating to:\cr
  <Control Panel><Administrative Tools><Data Sources (ODBC)>.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2026-02-03}\cr
}
\seealso{
  In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{createDSN}},
  \code{\link[PBStools]{getODBC}},
  \code{\link[PBStools]{getData}},
  \code{\link[PBStools]{runModules}}
}
\examples{
\dontrun{
local(envir=.PBStoolEnv,expr={
pbsfun=function(os=.Platform$OS.type,dfo=FALSE) {
  if (os=="windows" && dfo)
    confODBC(dsn="Popsicle", server="GFDB", db="PacHarvest",
      driver="SQL Server", descr="Icy sweet nonsense", trusted=TRUE) 
  else showMessage("Only functional for DFO personnel using Microsoft Windows OS")
  invisible() }
pbsfun()
})
}
}
\keyword{data}
\keyword{utilities}
\concept{M01_Utility}

