\name{listTables}
\alias{listTables}
\title{List Tables in Specified SQL/ORA/MDB Database}
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
  \item{dbName}{string specifying the name of a remote \code{SQL}/\code{ORA}
    or local \code{MDB} database.}
  \item{pattern}{string specifying search pattern for table names.}
  \item{path}{string specifying path to \code{MDB} database.}
  \item{server}{string specifying server name (\emph{e.g.}, \code{GFDB}, 
    \code{SVBCPBSGFIIS}, \code{ORADEV}, or \code{ORAPROD}).}
  \item{type}{type of server: \code{SQL}, \code{ORA} for Oracle, 
    or \code{MDB} for Microsoft ACCESS.}
  \item{ttype}{table type: \code{SEQUENCE}, \code{SYNONYM}, \code{SYSTEM TABLE},
    \code{TABLE}, or \code{VIEW}; default \code{NULL} shows all table types.}
  \item{trusted}{logical: if \code{TRUE}, allow \code{SQL}/\code{ORA} server 
    to use a trusted DFO login ID.}
  \item{uid, pwd}{user ID and password for authentication (if \code{trusted=FALSE}).}
  \item{silent}{logical: if \code{TRUE} then table names are not printed to the console.}
  \item{tenv}{target environment to which the result object \code{PBSdat} will be sent.}
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
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{ 
  \code{\link{getFile}}, \code{\link{getData}}
}
\examples{
pbsfun=function(os=.Platform$OS.type,dfo=FALSE) {
  if (os=="windows") {
    if(dfo) {
      cat("Tables in 'PacHarvest' matching pattern 'Species'\n")
      listTables("PacHarvest",pattern="Species") }
    else {
      cat("Tables in 'Examples.mdb'\n")
      listTables("Examples",type="MDB",ttype=c("TABLE","VIEW"),path=.getSpath()) } }
  else showMessage("If logged onto DFO network, set argument 'dfo=T'")
  invisible() }
pbsfun()
}
\keyword{data}
\keyword{utilities}
