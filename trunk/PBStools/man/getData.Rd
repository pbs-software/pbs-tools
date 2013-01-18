\name{getData}
\alias{getData}
\title{Get Data from a Variety of Sources}
\description{
  Attempt to get data from a variety of types (e.g., data objects/files,
  queries/tables) and remote sources (\code{ORA}, \code{SQL},
  \code{MDB}, \code{DBF}, \code{XLS}, \code{FILE}).
}
\usage{
getData(fqtName, dbName="PacHarvest", strSpp=NULL, server=NULL,
     type="SQL", path=getwd(), trusted=TRUE, uid="", pwd="",
     noFactors=TRUE, noLogicals=TRUE, rownum=0, mindep=NULL, 
     maxdep=NULL, surveyid=NULL, survserid=NULL, fisheryid=NULL, 
     logtype=NULL, doors=NULL, speed=NULL, mnwt=NULL, tarSpp=NULL, 
     major=NULL, top=NULL, dummy=NULL, tenv=.GlobalEnv, ...)
}
\arguments{
  \item{fqtName}{string specifying name of file, query, or table; 
    can also be an explicit SQL statement when \code{TYPE=SQLX} or \code{TYPE=ORAX}.}
  \item{dbName}{string specifying the name of a remote database. Types supported:
    \code{XLS} (Excel), \code{MDB} (ACCESS), \code{SQL} (Sequel Server), \code{ORA} (Oracle).}
  \item{strSpp}{string specifying species code for the SQL variable \code{@sppcode}.
    If not specified, a table or query is retrieved.}
  \item{server}{string specifying the name of a remote server (e.g., 
    \code{SVBCPBSGFIIS} for an SQL server, \code{GFSH} or \code{ORAPROD} for an Oracle server.}
  \item{type}{type of file:\cr
    \code{FILE} = local data object, a saved binary file \code{*.rda},
      a dumped ASCII file \code{.r}, a comma-delimited file \code{.csv}
      or \code{.txt}. Specify the file name without the extension.\cr
    \code{XLS} = Microsoft Excel spreadsheet (specific worksheet is specified
      by \code{fqtName}).\cr
    \code{DBF} = Dbase IV table. As each \code{.dbf} file acts like a 
      separate table, use \code{fqtName} to specify the \code{.dbf} file 
      without its extension.\cr
    \code{MDB} = Microsoft ACCESS database query or table.\cr
    \code{SQL} = SQL Server SQL query (code file) or table.\cr
    \code{ORA} = Oracle SQL query (code file) or table.\cr
    \code{SQLX} = SQL Server SQL query code (direct expression).\cr
    \code{ORAX} = Oracle SQL query code (direct expression).}
  \item{path}{string specifying path to local file, MDB database, or SQL query code file.}
  \item{trusted}{logical: if \code{TRUE}, allow SQL Server to use a trusted DFO login ID.}
  \item{uid, pwd}{user ID and password for authentication (if required).}
  \item{noFactors}{logical: if \code{TRUE}, convert all factor fields to character fields.}
  \item{noLogicals}{logical: if \code{TRUE}, convert all logical fields to characters \dQuote{T} or \dQuote{F}.}
  \item{rownum}{numeric indicating how many rows of a table to return. The default 0 means all rows are 
    returned (entire table). This argument only affects downloads of remote database tables.}
  \item{mindep}{numeric specifying minimum depth for the SQL variable \code{@mindep}.}
  \item{maxdep}{numeric specifying maximum depth for the SQL variable \code{@maxdep}.}
  \item{surveyid}{numeric specifying survey ID in GFBio for the SQL variable \code{@surveyid}.}
  \item{survserid}{numeric specifying survey series ID in GFBio for the SQL variable \code{@survserid}.}
  \item{fisheryid}{numeric specifying fishery ID number for the SQL variable \code{@fisheryid}.}
  \item{logtype}{string specifying log type code for the SQL variable \code{@logtypeval}.}
  \item{doors}{numeric specifying door spread width for the SQL variable \code{@doorsval}.}
  \item{speed}{numeric specifying vessel trawling speed for the SQL variable \code{@speedval}.}
  \item{mnwt}{numeric specifying mean weight (g) of a species for the SQL variable \code{@mnwt}.}
  \item{tarSpp}{string specifying species code(s) for the SQL variable \code{@tarcode}.}
  \item{major}{numeric specifying PMFC major area codes(s) for the SQL variable \code{@major}.}
  \item{top}{numeric specifying top \emph{N} records for the SQL variable \code{@top}.}
  \item{dummy}{numeric or character to use \emph{ad hoc} wherever the SQL variable \code{@dummy} appears.}
  \item{tenv}{target environment to which the result object \code{PBSdat} will be sent.}
  \item{...}{additional arguments for \code{RODBC}'s function \code{sqlQuery} 
    (specifically if the user wishes to specify \code{rows_at_time=1}).}
}
\details{
  The data table retrieved is placed in the data frame object \code{PBSdat}.
  If the type is \code{MDB}, the query/table name and the database name 
  are attached as attributes. If the type is \code{SQL} or \code{ORA}, the DB name, query name,
  and SQL code are attached as attributes.
}
\value{
  No value is explicitly returned; data frame is retrieved and place in the 
  target environment as an object called \code{PBSdat}.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{ 
  \code{\link[PBStools]{getCatch}}, \code{\link[PBStools]{getFile}}, \code{\link[PBStools]{listTables}} \cr
  Available SQL queries: \code{\link[PBStools]{SQLcode}}
}
\keyword{data}
\keyword{utilities}
