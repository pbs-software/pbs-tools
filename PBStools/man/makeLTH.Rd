\name{makeLTH}
\alias{makeLTH}
\title{
  Make a Longtable Header for Sweave
}
\description{
  Make a longtable header for printing an \code{xtable} in Sweave (see References).
}
\usage{
makeLTH(xtab.table, table.caption, table.label, 
   struts=FALSE, add.continue=FALSE)
}

\arguments{
  \item{xtab.table}{table object created by \code{xtable}.}
  \item{table.caption}{table caption for the xtable (no need to specify this in the \code{xtable} command).}
  \item{table.label}{table label used to reference the table from other places in the document.}
  \item{struts}{logical: if \code{TRUE}, add vertical space before and after table header.}
  \item{add.continue}{logical: if \code{TRUE}, add `continued' messages on tables spanning pages.}
}
\details{
  This code was provided by a clever Swedish fellow (pseudonym = \code{chepec}) to 
  elegantly break a longtable across pages by specifying \emph{Continued on next page} 
  as a footer where the table breaks and changing the caption on the following page to
  \dQuote{Table xx -- continued from previous page}.
}
\value{
   A longtable header formatted to the specifications of \code{print.table}'s argument
   \code{add.to.rows}.
}
\note{
  Note the percent sign paired with the number of rows in the table;
  it will cause that trailing \code{\\hline} command in your \code{.tex} file to be commented out.
  Not in any manual I've seen, by the way. Just a trick I use. (\code{chepec}, stackexchange user: 10824)

}
\author{
  Originally from user \href{https://tex.stackexchange.com/users/10824/chepec}{chepec} 
  on \code{tex.stackexchange}, and modified by:\cr
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{opus locus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2018-07-03}\cr
}
\references{
  \href{https://tex.stackexchange.com/questions/41067/caption-for-longtable-in-sweave?rq=1}{Caption for longtable in Sweave}
}
\seealso{
  \pkg{PBSawatea}: \code{runSweaveMCMC} \cr
  \pkg{xtable}: \code{\link[xtable]{xtable}}, \code{\link[xtable]{print.xtable}} \cr
}
\examples{
\dontrun{
<<echo=FALSE, results=tex>>=
# In a Sweave document, one might use makeLTH like this:
require(xtable)
# need the column name formats and the data to be character:
habtab1 = read.csv("concur396-3CD-gear1.csv",check.names=FALSE,colClasses="character") 
ncol   = dim(habtab1)[[2]] - 1
saraSpp=c("027","394","410","424","435","437","440","442","453")
rows.sara = (1:nrow(habtab1))[is.element(habtab1$Code,saraSpp)] - 1
xtab1 = xtable(habtab1,align=paste(c("cc",rep("r",ncol)),collapse=""))
longtable.header=makeLTH(xtab1,"Top 25 species by catch weight.", "tab:concur396-3CD-gear1")
print(xtab1, 
   floating = FALSE, # longtable never floats
   hline.after = NULL,
   add.to.row = list(pos = list(-1, rows.sara, nrow(xtab1)), 
      command = c(longtable.header, "\\\\rowcolor{rowclr}", "\%")),
   include.rownames = FALSE,
   include.colnames = FALSE,
   type = "latex",
   tabular.environment = "longtable",
   sanitize.text.function = function(x){x},
   math.style.negative = FALSE)
@
}
}
\keyword{print}
