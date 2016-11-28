\name{preferDepth}
\alias{preferDepth}
\title{Display Depth Distribution for a Single Species}
\description{
  Display depth distribution for a selected species using an interactive 
  GUI. The SQL code grabs the depth table from a remote SQL Server database.
  The user can specify years and various areas, if desired.
}
\usage{
preferDepth(strSpp="410", fqtName="pht_fdep.sql", dbName="PacHarvest",
     spath=NULL, type="SQL", hnam=NULL, get.effort=TRUE)
}
\arguments{
  \item{strSpp}{string specifying the code for a species that appears in the
    target database }
  \item{fqtName}{string specifying the name of a local file (f), an SQL query 
    file (q), or an MDB table (t)}
  \item{dbName}{string specifying the name of an SQL DSN for an ODBC call to 
    the SQL Server database \code{DFBCV9TWVASP001}, or an MDB database}
  \item{spath}{string specifying the path to the file \code{fqtName}}
  \item{type}{string specifying the file type: \code{"SQL"}, \code{"MDB"}, or \code{"FILE"}}
  \item{hnam}{string specifying the name of a history file}
  \item{get.effort}{logical: if \code{TRUE} get the trawl effort data right away.}
}
\details{
  The function \code{preferDepth()} creates an interactive GUI that can be used
  to display histograms of depth distribution for specified species that 
  appear in SQL Server database \code{DFBCV9TWVASP001}. The use may also specify a 
  local file (see \code{getFile}). The cumulative catch is 
  superimposed to show the depths at which the species is caught in relation to 
  the tow frequency histogram. The total fleet effort is displayed as a 
  shaded histogram in the background.

  \bold{The GUI controls:}
  \tabular{ll}{
  \code{Window}      \tab Button to display the window description file.\cr
  \code{SQL}         \tab Button to display the SQL code (hard-wired to that at start-up).\cr
  \code{R code}      \tab Button to display the R code used to generate \code{preferDepth}.\cr
  \code{path}        \tab Path to the data/code file.\cr
  \code{sql}         \tab Button to set path to the \code{sql} directory in PBStools.\cr
  \code{cwd}         \tab Button to set path to the current working directory.\cr
  \code{File/Query/Table} \tab Name of (i) data file, (ii) SQL code file or MDB query, or (iii) MDB table.\cr
  \code{DB/DSN}      \tab Descriptor Name Service that provides an ODBC connection to \cr
                     \tab    the SQL Server database \code{DFBCV9TWVASP001}.\cr
  \code{type}        \tab Type of file: \code{SQL}, \code{MDB}, or \code{FILE}.\cr
  \code{Trusted}     \tab Check if using a trusted DFO identity.\cr
  \code{SQL User ID} \tab SQL user ID assigned by the database server administrators.\cr
  \code{password}    \tab Password assigned by the database server administrators.\cr \cr
  \bold{Choose Area} \tab \cr
  \code{All}         \tab Use depth record from all areas.\cr
  \code{Major}       \tab Use depth records with a PMFC major area code.\cr
  \code{Minor}       \tab Use depth records with a PMFC minor area code.\cr
  \code{Locality}    \tab Use depth records with a locality (fishing ground) code.\cr
  \code{DFO Area}    \tab Use depth records with a DFO management area code.\cr
  \code{DFO Subarea} \tab Use depth records with a DFO management subarea code.\cr
  \code{SRF Area}    \tab Use depth records with a slope rockfish assessment area code.\cr
  \code{Areas:}      \tab Specify area codes within the area type chosen above, \cr
                     \tab    either singly (e.g. 3C), as a range (e.g. 5:8, numeric only) \cr
                     \tab    or  as a group (e.g. 5AB,5CD,5E).\cr
  \code{Spp}         \tab Species code that appears in the target SQL Server database.\cr
  \code{Years}       \tab Specify years either singly (e.g. \code{2000}), as a range (e.g. \code{2000:2003}), \cr
                     \tab    as a group (e.g. \code{1996,2000,2006}), or as a sequence (e.g. \code{seq(1997,2007,2)}).\cr
  \code{X/Y Lims}    \tab Control the X- and Y-limits of the plot.\cr
  \code{Quants}      \tab Specify the bounding quantiles of the depth distribution.\cr
                     \tab    that will characterise the species depth range.\cr
  \code{Bar: colour} \tab Fill colours for histograms; note: specify \sQuote{\code{0}} (transparent) 
                          for species depth.\cr
  \code{width}       \tab Specify the width in X-units of the histogram bars.\cr
  \code{eff}         \tab If checked, shaded histogram of trawl effort is displayed in background.\cr
  \code{catch}       \tab If checked, display cumulative catch curve for selected species.\cr
  \code{legend}      \tab If checked, display the legend information.\cr
  \code{wmf}         \tab If checked, send the PLOT to a WMF file.\cr
  \code{col}         \tab Colour of cumulative catch curve.\cr
  \code{lwd}         \tab Line thickness (line width device) of cumulative catch curve.\cr
  \code{EFF}         \tab Button to explicitly refresh the trawl effort dataset.\cr
  \code{DATA}        \tab Button to explicitly refresh the depth dataset given the GUI settings.\cr
  \code{PLOT}        \tab Button to plot the depth distribution given the GUI settings.\cr \cr
  \bold{History widget} \tab 
  }
}
\value{
  No value is explicitly return; however, objects of interest are stored in the 
  global list object \code{PBStool}.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC.
}
\seealso{
  \code{\link[PBStools]{getData}}, \code{\link[PBStools]{calcHabitat}}, \code{\link[PBStools]{histMetric}} \cr
  Available SQL queries: \code{\link[PBStools]{SQLcode}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(dfo=FALSE,strSpp="LST"){
  if (dfo) {
    preferDepth(strSpp=strSpp, get.effort=FALSE) }
  else {
    data(testdatC,envir=.PBStoolEnv) 
    x=testdatC[testdatC[strSpp]>0 & !is.na(testdatC[strSpp]),]
    testdep=data.frame(year=as.numeric(substring(x$date,1,4)),depth=x$fdep,catch=x$LST)
    attr(testdep,"spp")=strSpp
    save("testdep",file=paste(tempdir(),"/testdep.rda",sep=""))
    preferDepth(strSpp=strSpp, fqtName="testdep", type="FILE", 
      spath=convSlashes(tempdir(), os="unix"), get.effort=FALSE) }
  invisible() }
pbsfun()
})
}
\keyword{hplot}
