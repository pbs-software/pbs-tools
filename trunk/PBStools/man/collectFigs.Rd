\name{collectFigs}
\alias{collectFigs}
\title{
  Collect Postscript Figures Into One Document
}
\description{
  Collect figures (currently only encapsulated postscript \code{.eps}
  supported) into one document using a latex compiler (\code{latex.exe},
  \code{dvips.exe}, \code{ps2pdf.exe}).
}
\usage{
collectFigs(path=".", ext="eps", is.fnum=FALSE, 
   fout="collectFigs", width=6, capskip=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{path to \code{.eps} files; can be relative or absolute.}
  \item{ext}{extension of figure files (currently only \code{"eps"} supported). }
  \item{is.fnum}{logical: if \code{TRUE}, a figure number or identifier
    occurs in the first string sequence of a file name delimited by \code{"-"}.}
  \item{fout}{file name prefix of the output file.}
  \item{width}{width (inches) to render each figure in the collection.}
  \item{capskip}{space (pt) between each figure and its caption (negative values reduce the space).}
}
\details{
  The code constructs a \code{.tex} file that uses all available 
  \code{.eps} files in the specified directory. The final result is a
  bookmarked PDF file called \code{<fout>.pdf}.
}
\value{
  Returns a string vector of latex commands that form the \code{.tex}
  file when written to the system directory specified by the user.
}
\references{
  \url{http://latex-community.org/forum/viewtopic.php?f=5&t=21405}
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\note{
  The latex \code{bin} directory (e.g., \code{C:\\Apps\\MiKTeX\\miktex\\bin})
  must be on the system path.

  The tex code requires the latex packages: \code{geometry}, \code{epsfig},
  \code{caption}, and \code{hyperref}.
}
\seealso{
  \code{\link[PBStools]{buildCatch}},  \code{\link[base]{writeLines}}
}
\keyword{print}

