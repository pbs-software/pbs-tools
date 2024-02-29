\name{extractAges}
\alias{extractAges}
\title{
  Extract Age Records
}
\description{
  Extract age records from a biological object created by an SQL query to GFBioSQL.
}
\usage{
extractAges(dat, ameth=c(3,17), use.sfc.age=1:3, sex=c(2,1),
   rmSM=TRUE, only.markSM=FALSE, rmRC=9,
   show.age=FALSE, show.flds=c("ameth","ttype"), show.only=FALSE)
}
\arguments{
  \item{dat}{\code{data.frame} -- object containing biological data, usually created by a call to the query \sQuote{\code{gfb_bio.sql}}.}
  \item{ameth}{\code{numeric} -- ageing method code used in \code{GFBioSQL}.}
  \item{use.sfc.age}{\code{numeric} -- explicit ages (usually young) at which using surface otolith readings are acceptable.}
  \item{sex}{\code{numeric} -- sex code: 0=not sexed, 1=males, 2=females, 3=observed but uncertain.}
  \item{rmSM}{\code{logical} -- if \code{TRUE}, remove seamount records from age data.}
  \item{only.markSM}{\code{logical} -- if \code{TRUE}, only mark the seamount records, do not remove them.}
  \item{rmRC}{\code{numeric} -- remove records with specified `Reason Codes'.}
  \item{show.age}{\code{logical} -- if \code{TRUE}, show the number of available ages in a pop-up GUI.}
  \item{show.flds}{\code{character} -- two field names in \sQuote{\code{dat}} by which to summarise the number of ages;
    note: the third  dimension is automatically \sQuote{\code{sex}}, so don't choose this field.}
  \item{show.only}{\code{logical} -- if \code{TRUE}, do not extract positive-age records;
    a crosstab query is returned invisibly instead.}
}
\details{
  This function attempts to standardise the extraction of
  records with positive age, and should be used by other functions.
  Currently, functions that subset age are doing so inconsistently.
}
\value{
  A subset of records from the input file that contain records with positive
  ages specified by one or more ageing methods.

  If \sQuote{\code{show.age==TRUE && show.only==TRUE}}, then the function
  returns the results of a crosstab query -- number of ages for the 
  fields specified by \sQuote{\code{show.flds}}.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2021-06-15}
}
\note{
  The default arguments are set to extract broken and burnt/baked otoliths 
  plus surface-read otoliths for ages 1-3 years.
}
\seealso{
\code{\link[PBStools]{biteData}},
\code{\link[PBStools]{chewData}},
\code{\link[PBStools]{crossTab}},
\code{\link[PBStools]{getData}},
\code{\link[PBStools]{listTables}},
\code{\link[PBStools]{plotProp}},
\code{\link[PBStools]{processBio}},
\code{\link[PBStools]{SQLcode}},
\code{\link[PBStools]{subsetFile}},
\code{\link[PBStools]{texArray}},
\code{\link[PBStools]{zapSeamounts}}
}
\keyword{manip}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
\concept{M02_Biology}

