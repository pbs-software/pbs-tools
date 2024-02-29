\name{wrapText}
\alias{wrapText}
\title{Wrap, Mark and Indent a Long Text String}
\description{
  Wrap a long text string to a desired width, mark it with 
  prefixes, and indent the lines following the first.
}
\usage{
wrapText(string, width=50, prefix=c("> ","+ "), exdent=3)
}
\arguments{
  \item{string}{ a text string.}
  \item{width}{ a positive integer giving the target column for wrapping lines in the output.}
  \item{prefix}{ character strings to be used as prefixes for the first and subsequent lines.}
  \item{exdent}{ a non-negative integer specifying the indentation of subsequent lines.}
}
\details{
  Using \code{base::strwrap}, this function splits a long text string 
  into a target width, indents lines following the first, and adds 
  a prefix string \code{prefix[1]} to the first line and 
  \code{prefix[2]} to all indented lines.
}
\value{
  A text string with inserted prefixes and carriage returns.
  This output string can be sent to display functions
  like \code{cat} and \code{text}.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2021-06-15}\cr
}
\seealso{
  \code{\link{revStr}}, \code{\link{showError}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(){
  txt=wrapText(paste("USA state names:  ",
    paste(state.name,collapse=", "),sep=""),width=72,exdent=5)
  showMessage(txt,as.is=TRUE,adj=0,col="blue",cex=.9,x=.05)
  cat(txt,"\n"); invisible() }
pbsfun()
})
}
\keyword{character}
\concept{M01_Utility}
