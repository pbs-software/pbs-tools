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
   subQtrust = list(trusted=TRUE, uid="", pwd=""),
   noFactors=TRUE, noLogicals=TRUE, rownum=0, mindep=NULL, 
   maxdep=NULL, surveyid=NULL, survserid=NULL, fisheryid=NULL, 
   logtype=NULL, doors=NULL, speed=NULL, mnwt=NULL, tarSpp=NULL, 
   major=NULL, top=NULL, gear=NULL, dummy=NULL, senv=NULL, tenv=.GlobalEnv, ...)
}
\arguments{
  \item{fqtName}{\emph{string}: Name of file, query, or table; 
    can also be an explicit SQL statement when \code{TYPE="SQLX"} or \code{TYPE="ORAX"}.}
  \item{dbName}{\emph{string}: Name of a remote database. Types supported:
    \code{XLS} (Excel), \code{MDB} (ACCESS), \code{SQL} (SQL Server), \code{ORA} (Oracle).}
  \item{strSpp}{\emph{string}: Species code for the SQL variable \code{@sppcode}.
    If not specified, a table or query is retrieved.}
  \item{server}{\emph{string}: Name of a remote server (e.g., 
    \code{DFBCV9TWVASP001} for an SQL server, \code{GFSH} or \code{ORAPROD} for an Oracle server.}
  \item{type}{\emph{string}: Type of file:\cr
    \code{"FILE"} = local data object, a saved binary file \code{*.rda},
      a dumped ASCII file \code{.r}, a comma-delimited file \code{.csv}
      or \code{.txt}. Specify the file name without the extension.\cr
    \code{"XLS"} = Microsoft Excel spreadsheet (specific worksheet is specified
      by \code{fqtName}).\cr
    \code{"DBF"} = Dbase IV table. As each \code{.dbf} file acts like a 
      separate table, use \code{fqtName} to specify the \code{.dbf} file 
      without its extension.\cr
    \code{"MDB"} = Microsoft ACCESS database query or table.\cr
    \code{"SQL"} = SQL Server SQL query (code file) or table.\cr
    \code{"ORA"} = Oracle SQL query (code file) or table.\cr
    \code{"SQLX"} = SQL Server SQL query code (direct expression).\cr
    \code{"ORAX"} = Oracle SQL query code (direct expression).}
  \item{path}{\emph{string}: Path to local file, MDB database, or SQL query code file.}
  \item{trusted}{\emph{logical}: If \code{TRUE}, allow SQL Server to use a trusted DFO login ID.}
  \item{uid, pwd}{\emph{string}: User ID and password for authentication (usually required on Oracle servers).}
  \item{subQtrust}{\emph{list}: List of credentials \emph{only} for users not \dQuote{trusted} on the DFO network. \cr
    If \code{(type=="SQL")}, then  \code{subQtrust=list(trusted=trusted, uid=uid, pwd=pwd)} automatically. \cr
    If \code{(type=="ORA")}, then user \bold{must} supply a \code{subQtrust} list for an account on the SQL server DFBCV9TWVASP001. }
  \item{noFactors}{\emph{logical}: If \code{TRUE}, convert all factor fields to character fields.}
  \item{noLogicals}{\emph{logical}: If \code{TRUE}, convert all logical fields to characters \dQuote{T} or \dQuote{F}.}
  \item{rownum}{\emph{numeric}: Value indicating how many rows of a table to return. The default 0 means all rows are 
    returned (entire table). This argument only affects downloads of remote database tables.}
  \item{mindep}{\emph{numeric}: Minimum depth for the SQL variable \code{@mindep}.}
  \item{maxdep}{\emph{numeric}: Maximum depth for the SQL variable \code{@maxdep}.}
  \item{surveyid}{\emph{numeric}: Survey ID in GFBio for the SQL variable \code{@surveyid}.}
  \item{survserid}{\emph{numeric}: Survey series ID in GFBio for the SQL variable \code{@survserid}.}
  \item{fisheryid}{\emph{numeric}: Fishery ID number for the SQL variable \code{@fisheryid}.}
  \item{logtype}{\emph{string}: Log type code for the SQL variable \code{@logtypeval}.}
  \item{doors}{\emph{numeric}: Door spread width for the SQL variable \code{@doorsval}.}
  \item{speed}{\emph{numeric}: Vessel trawling speed for the SQL variable \code{@speedval}.}
  \item{mnwt}{\emph{numeric}: Mean weight (g) of a species for the SQL variable \code{@mnwt}.}
  \item{tarSpp}{\emph{string}: Species code(s) for the SQL variable \code{@tarcode}.}
  \item{major}{\emph{numeric}: PMFC major area codes(s) for the SQL variable \code{@major}.}
  \item{top}{\emph{numeric}: Top \emph{N} records for the SQL variable \code{@top}.}
  \item{gear}{\emph{numeric}: Gear codes used by various databases (see \code{\link[PBSdata]{gear}}) 
    for the SQL variable \code{@gear}.}
  \item{dummy}{\emph{numeric} or \emph{string}: An \emph{ad hoc} value wherever the SQL variable \code{@dummy} appears.}
  \item{senv}{\emph{environment}: Source environment from which \code{fqtName} will be retrieved (if available).
    Only used when \code{type="FILE"}.}
  \item{tenv}{\emph{environment}: Target environment to which the result object \code{PBSdat} will be sent.}
  \item{...}{Additional arguments for \code{RODBC}'s function \code{sqlQuery} 
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
\section{SQL Code Variables}{
  The function \code{getData} automatically substitutes a number of 
  SQL code variables (if specified) in the called SQL script file 
  using the function arguments specified by the user:
  \preformatted{
    @doorsval   <-  doors
    @dummy      < - dummy
    @fisheryid  <-  fisheryid
    @gear       <-  gear
    @logtypeval <-  logtype
    @major      <-  major
    @maxdep     <-  maxdep
    @mindep     <-  mindep
    @mnwt       <-  mnwt
    @speedval   <-  speed
    @sppcode    <-  strSpp
    @surveyid   <-  surveyid
    @survserid  <-  survserid
    @table      <-  dbName
    @tarcode    <-  tarSpp
    @top        <-  top}

  There are a number of SQL code variables that are not controlled 
  directly by using \code{getData}'s arguments but will be substituted
  by fixed values if specified:
  \preformatted{
    @alfcode <- all fish species
    @trfcode <- total rockfish species  (Sebastes + Sebastolobus)
    @orfcode <- other rockfish species  (excludes POP)
    @tffcode <- total flatfish species
    @offcode <- other flatfish species  (excludes Dover Sole)
    @carcode <- cartilaginous species   (incl. sharks, skates, ratfish)
    @ssscode <- shark and skate species (catilaginous excl. ratfish)
    @invcode <- invertebrates
    
    @t01code <- taxon  1: Agnatha (hagfish, lampreys)
    @t02code <- taxon  2: Chondrichthyes (sharks, rays, chimaeras)
    @t03code <- taxon  3: Clupeiformes (herring, anchovies)
    @t04code <- taxon  4: Salmonidae (salmon, trout)
    @t05code <- taxon  5: Osmeridae (smelts)
    @t06code <- taxon  6: Myctophidae (lanternfishes)
    @t07code <- taxon  7: Gadidae (codfishes)
    @t08code <- taxon  8: Macrouridae (grenadiers)
    @t09code <- taxon  9: Scombridae (mackerels, tunas)
    @t10code <- taxon 10: Scorpaenidae (scorpionfishes)
    @t11code <- taxon 11: Hexagrammidae (greenlings)
    @t12code <- taxon 12: Pleuronectiformes (flounders, soles, halibut)
    @t00code <- taxon  0: All fish not part of a taxon group (above)
    
    @originid <- use original survey index or not
       if(is.null(survserid) && is.null(surveyid)) "'Y'" else "'Y','N'"}

  \bold{Note}: If Oracle SQL is called from SQL Server via \code{OPENQUERY},
  insert a '\code{~}' symbol after '\code{@}'\cr (e.g. \code{@~sppcode}).

  Additionally, the SQL code variable \code{@INSERT} acts a flag for a
  place in the SQL code at which independent SQL code can be inserted.
  For example,
  
  \code{@INSERT('meanSppWt.sql')}
  
  inserts the SQL file called \code{meanSppWt.sql}, assuming that it is
  on the \code{path} specified by the call to \code{getData}.
}
\author{
  Rowan Haigh, Research Biologist,\cr
  Fisheries and Oceans Canada, Pacific Biological Station, Nanaimo BC
}
\seealso{ 
  \code{\link[PBStools]{getCatch}}, \code{\link[PBStools]{getFile}}, \code{\link[PBStools]{listTables}} \cr
  Available SQL queries: \code{\link[PBStools]{SQLcode}}
}
\keyword{data}
\keyword{utilities}
