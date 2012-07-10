\name{confODBC}
\alias{confODBC}
\title{Configure an ODBC User Data Source Name}
\description{
  Use a command line utility found in Windows OS called 'odbcconf.exe' that 
  configures an ODBC Data Source Name from the system's command line.
}
\usage{
confODBC(dsn="PacHarvest", server="GFDB", db="PacHarvest",
         driver="SQL Server", descr="", trusted=TRUE) 
}
\arguments{
  \item{dsn}{Data Source Name for the ODBC connection.}
  \item{server}{string specifying name of server that hosts the SQL Server databases.}
  \item{db}{string specifying the name of the default database for the DSN connection.}
  \item{driver}{string specifying the ODBC driver.}
  \item{descr}{string specifying optional description of the DSN connection.}
  \item{trusted}{logical: if \code{TRUE}, allow SQL Server to use a trusted DFO login ID.}
}
\details{
  \code{confODBC()} runs the Windows command line utility with the user-supplied 
  information and configures a User DSN on the user's computer.\cr\cr
  In XP, the new User DSN can be seen by navigating to:\cr
  <Control Panel><Administrative Tools><Data Sources (ODBC)>.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\seealso{ 
  \code{\link{getData}}, \code{\link{runModules}}
}
\examples{
pbsfun=function(os=.Platform$OS.type,dfo=FALSE) {
  if (os=="windows" && dfo)
    confODBC(dsn="Popsicle", server="GFDB", db="PacHarvest",
      driver="SQL Server", descr="Icy sweet nonsense", trusted=TRUE) 
  else showMessage("Only functional for DFO personnel using Microsoft Windows OS")
  invisible() }
pbsfun()
}
\keyword{data}
\keyword{utilities}
