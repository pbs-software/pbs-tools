\name{texArray}
\alias{texArray}
\title{
  Flatten and Format Array for LaTeX Output
}
\description{
  Flatten and format an array (or data frame) for LaTeX output.
}
\usage{
texArray(x, table.caption="My table", table.label="tab:mytable",
   strSpp=NULL, sigdig=3, zero="---", exInt=TRUE, use.round=FALSE,
   collab=NULL, dash.delim=NULL, tablewidth=6.5, 
   rm.empty=FALSE, start.page=1, ignore.col=NULL,
   select.rows=NULL, use.row.names=FALSE, name.row.names="row",
   add.header.column=FALSE, new.header=NULL, outnam="mytable", 
   alignHRC=c("l","l","r"), italics.file=NULL, ...)
}
\arguments{
  \item{x}{\code{numeric} -- array, matrix, or data frame.}
  \item{table.caption}{\code{character} -- table caption for \code{makeLTH}, which is passed to \code{xtable} to make a LaTeX \code{longtable}.}
  \item{table.label}{\code{character} -- table label used to reference the table from other places in the LaTeX document.}
  \item{strSpp}{\code{character} -- string species code, often the page number on which fish is described in Hart (1973).}
  \item{sigdig}{\code{numeric} -- number of significant digits to format table contents.}
  \item{zero}{\code{character} -- string replacement for zero-values.}
  \item{exInt}{\code{logical} -- if \code{TRUE}, exclude integers from manipulation to signicant digits.}
  \item{use.round}{\code{logical} -- if \code{TRUE}, round the values to \code{N} decimal places rather than provide \code{N} significant figures.}
  \item{collab}{\code{character} -- column headers for table; if \code{NULL}, use column names from input data.}
  \item{dash.delim}{\code{character} -- character pattern in data \code{rownames} that identify where to place a dashed line.}
  \item{tablewidth}{\code{numeric} -- width of table in inches (the column widths will be fit automatically).}
  \item{rm.empty}{\code{logical} -- if \code{TRUE}, remove rows \emph{and} columns that only contain zeroes, \code{NA}s, or \code{"---"}.}
  \item{start.page}{\code{numeric} -- page number to start the LaTeX document at.}
  \item{ignore.col}{\code{numeric} -- vector indicating which columns (by position) to \emph{not} run \code{formatCatch} on (e.g., columns of class \code{character}).}
  \item{select.rows}{\code{numeric} -- select rows depending on either a numeric vector of rows or a character vector of row names.}
  \item{use.row.names}{\code{logical} -- if \code{TRUE}, use the \code{rownames} from the input data table.}
  \item{name.row.names}{\code{character} -- table header name for column comprising the data table row names.}
  \item{add.header.column}{\code{logical} -- if \code{TRUE}, add a first column that delineates the various 
    dimensions of an array (> 2 dimensions) using the \code{names} of the \code{dimnames}.}
  \item{new.header}{\code{character} -- vector of replacement labels if the user knows the table header columns and wants to replace them with specific labels.}
  \item{outnam}{\code{character} -- prefix for an output LaTeX file (\code{strSpp} will be appended to the prefix,
    and the file extension \code{.tex} will be appended to the file name composition).}
  \item{alignHRC}{\code{character} -- align columns: Header, Row Names, and Columns (of data) using "l" (left), "r" (right), or "c" (centre).}
  \item{italics.file}{\code{character} -- file of string names specifying words in table columns to italicise (e.g., genus and species names).}
  \item{...}{arguments to pass to function \code{makeLTH}, specifically:\cr
    \code{struts} -- if \code{TRUE}, add vertical space before and after table header; and\cr
    \code{add.continue} -- if \code{TRUE}, add `continued' messages on tables spanning pages.}
}
\details{
  The function converts the input data into a two-dimensional data frame, formats the 
  numbers to the user-specified significant digits (using \code{formatCatch}), and
  creates a LaTeX file (\code{outnam-strSpp.tex}) that can be converted to PDF using
  MiKTeX's \code{TexWorks} program (or manually compiled using \code{texify --pdf} or 
  a basic sequence of commands: \code{latex}, \code{dvips}, and \code{ps2pdf}).
}
\value{
  Aside from the LaTeX file, the function invisibly returns a list of: \cr
  \code{goo} -- the flattened and re-formatted data file, and \cr
  \code{texfile} -- a vector of latex lines that make the output LaTeX file. \cr
  \code{tabfile} -- a vector of latex lines that describes the table only (i.e., a subset of \code{texfile}).
}
\note{
  An example of an \code{italics.file} called \code{"genus.species.txt"} is offered in \pkg{PBSplankton}'s \code{doc} directory.
}
\references{
  Hart, J.L. (1973) Pacific fishes of Canada. \emph{Fisheries Research Board of Canada} \bold{180}: ix + 740 p.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2020-10-08}
}
\seealso{
  \code{\link[PBStools]{formatCatch}}, \code{\link[PBStools]{makeLTH}}, \code{\link[PBStools]{collectFigs}}
}
\keyword{manip}
\keyword{print}

