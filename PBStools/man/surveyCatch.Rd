\name{surveyCatch}
\alias{surveyCatch}
\title{
  Survey Catch Summary from GFBioSQL
}
\description{
  Query the \code{GFBioSQL} database for survey catches of the specified species.
}
\usage{
surveyCatch(strSpp="396", spath=.getSpath(), gfbdat=NULL,
   hadley=FALSE, tabDir="./tables", datDir="./data")
}
\arguments{
  \item{strSpp}{\code{character} -- string specifying the Hart species code for a species in the \code{GFBioSQL} database.}
  \item{spath}{\code{character} -- string of path to the SQL code file \code{gfb_rcatORF.sql}; \cr
    defaults to the \code{sql} directory under \code{system.file(package="PBStools")} }
  \item{gfbdat}{\code{numeric} -- data frame object of survey data extracted from GFBioSQL; \cr
    if \code{NULL} run the query \code{gfb_rcatORF.sql} and save \code{gfbdat} to 
    \code{gfbdat.rda} and \code{Survey-Records-123.csv}, where \code{123} is the 
    three-character Hart code. }
  \item{hadley}{\code{logical} -- if \code{TRUE}, use Hadley Wickham's wizardry in \code{'crossTab'}.}
  \item{tabDir}{\code{character} -- path string where ascii tables are saved.}
  \item{datDir}{\code{character} -- path string where binary data files are saved/retrieved.}
}
\details{
  Needs the SQL code file \code{gfb_rcatORF.sql}, which is designed to return 
  survey catch data in a format similar to that for commercial catch builds 
  by the function \code{buildCatch}.
}
\value{
  Invisibly returns a table of catch (t) summarised by calendar year and PMFC area.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Regional Headquarters (RHQ), Vancouver BC\cr
  Last modified \code{Rd: 2023-06-08}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{buildCatch}},
  \code{\link[PBStools]{crossTab}},
  \code{\link[PBStools]{getCatch}},
  \code{\link[PBStools]{plotDiag}},
  \code{\link[PBStools]{plotRecon}},
  \code{\link[PBStools]{zapSeamounts}}
}
\keyword{database}
\concept{M07_CatchRecon}

