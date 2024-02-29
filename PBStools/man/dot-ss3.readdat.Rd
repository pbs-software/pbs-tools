\name{dot-ss3.readdat}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{.ss3.readdat}
\title{
  Read Data File from SS v.3.30 (borrowed)
}
\description{
  Read Stock Synthesis (v.3.30) data file into list object in R.
  This is a `borrowed' version of the r4ss::SS_readdat_3.30 function.
}
\usage{
.ss3.readdat(file, verbose=TRUE, echoall=FALSE, section=NULL)
}
\arguments{
  \item{file}{\code{character} -- file name, either with full path or relative to working directory.}
  \item{verbose}{\code{logical} -- if \code{TRUE}, write output to the screen.}
  \item{echoall}{\code{logical} -- deprecated}
  \item{section}{\code{numeric|character} -- which data set to read. Only applies for a \code{data.ss_new} file created by Stock Synthesis.
    Allows the choice of either expected values (section=2) or bootstrap data (section=3+).
    Leaving default of section=NULL will read input data, (equivalent to section=1). }
}
%- maybe also 'usage' for other objects documented here.
\details{
  Quick way to read in SS3 input files.
}
\author{
  Ian Taylor et al. in package \pkg{r4ss}.

  Opportunist: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-29}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
In \pkg{r4ss}:\cr
  \code{SS_readdat},
  \code{SS_readdat_2.00},
  \code{SS_readdat_3.00},
  \code{SS_readdat_3.24}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{IO}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{Z01_Borrow}
% \concept{ ~cpt2 }
% Use only one concept per line.
