\name{listTables}
\alias{listTables}
\title{List Tables in Database}
\description{
  List the tables in a specified SQL, Oracle or Microsoft ACCESS database.
  User can choose table type and/or search table names through pattern matching.
}
\usage{
listTables(dbName, pattern=NULL, path=getwd(), server=NULL,
   type="SQL", ttype=NULL, trusted=TRUE, uid="", pwd="",
   silent=FALSE, tenv=.GlobalEnv) 
}
\arguments{
  \item{dbName}{\code{character} -- name of a remote \code{SQL}/\code{ORA}
    or local \code{MDB} database}
  \item{pattern}{\code{character} -- search pattern for table names}
  \item{path}{\code{character} -- path to \code{MDB} database}
  \item{server}{\code{character} -- server name (\emph{e.g.}, \code{GFDB}, 
    \code{DFBCV9TWVASP001}, \code{ORADEV}, or \code{ORAPROD})}
  \item{type}{\code{character} -- type of server: \code{SQL}, \code{ORA} for Oracle, 
    or \code{MDB} for Microsoft ACCESS}
  \item{ttype}{\code{character} -- table type: \code{SEQUENCE}, \code{SYNONYM}, \code{SYSTEM TABLE},
    \code{TABLE}, or \code{VIEW}; default \code{NULL} shows all table types}
  \item{trusted}{\code{logical} -- if \code{TRUE}, allow \code{SQL}/\code{ORA}
    server to use a trusted DFO login ID}
  \item{uid, pwd}{\code{character} -- user ID and password for authentication (if \code{trusted=FALSE})}
  \item{silent}{\code{logical} -- if \code{TRUE}, do not print table names to the console}
  \item{tenv}{\code{environment} -- target environment to which the result object \code{PBSdat} will be sent}
}
\details{
  The results of \code{RODBC::sqlTables} are dumped to a data frame 
  object called \code{PBSdat}. If no pattern \code{pattern} is specified, 
  all table names in \code{dbName} are returned. If \code{pattern} is provided, 
  only table names containing the pattern are returned. Pattern matching 
  is case-sensitive. Additionally, the user can choose table types, 
  depending on the server type.
}
\value{
  Silently returns a vector of table names.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2025-03-20}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{getFile}},
  \code{\link[PBStools]{getData}}
}
\keyword{data}
\keyword{utilities}
\concept{M01_Utility}

