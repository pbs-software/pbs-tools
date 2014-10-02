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
   strSpp=NULL, sigdig=3, zero="---", collab=NULL, dash.delim=NULL, 
   rm.empty=TRUE, start.page=1, ignore.col=NULL,
   select.rows=NULL, use.row.names=FALSE, name.row.names="row",
   add.header.column=FALSE, new.header=NULL, outnam="mytable")
}
\arguments{
  \item{x}{numeric array, matrix, or data frame.}
  \item{table.caption}{table caption for \code{makeLTH}, which is passed to \code{xtable} to make a LaTeX \code{longtable}.}
  \item{table.label}{table label used to reference the table from other places in the LaTeX document.}
  \item{strSpp}{string species code -- page number on which fish is described in Hart (1973).}
  \item{sigdig}{number of significant digits to format table contents.}
  \item{zero}{character replacement for zero-values.}
  \item{collab}{column headers for table; if \code{NULL}, use column names from input data.}
  \item{dash.delim}{character pattern in data \code{rownames} that identify where to place a dashed line.}
  \item{rm.empty}{logical -- if \code{TRUE}, remove rows without any positive numeric data.}
  \item{start.page}{page number to start the LaTeX document at.}
  \item{ignore.col}{numeric vector indicating which columns (by position) to \emph{not} run \code{formatCatch} on (e.g., columns of class \code{character}).}
  \item{select.rows}{select rows depending on either a numeric vector of rows or a character vector of row names.}
  \item{use.row.names}{logical -- if \code{TRUE}, use the \code{rownames} from the input data table.}
  \item{name.row.names}{table header name for column comprising the data table row names.}
  \item{add.header.column}{logical -- if \code{TRUE}, add a first column that delineates the various 
    dimensions of an array (> 2 dimensions) using the \code{names} of the \code{dimnames}.}
  \item{new.header}{character vector of replacement labels if the user knows the table header columns and wants to replace them with specific labels.}
  \item{outnam}{character prefix for an output LaTeX file (\code{strSpp} will be appended to the prefix,
    and the file extension \code{.tex} will be appended to the file name composition).}
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
\references{
  Hart, J.L. (1973) Pacific fishes of Canada. \emph{Fisheries Research Board of Canada} \bold{180}: ix + 740 p.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC.
}
\seealso{
  \code{\link[PBStools]{formatCatch}}, \code{\link[PBStools]{makeLTH}}, \code{\link[PBStools]{collectFigs}}
}
\keyword{manip}
\keyword{print}

