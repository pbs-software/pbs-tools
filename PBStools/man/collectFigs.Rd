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
   fout="collectFigs", width=6.5, capskip=0, pattern=NULL, show.output=FALSE)
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
  \item{pattern}{specify a pattern of figure files to collect if not all desired.}
  \item{show.output}{logical: if \code{TRUE}, display the latex build routine output.}
}
\details{
  The code constructs a \code{.tex} file that uses all available 
  \code{.eps} files in the specified directory. The final result is a
  bookmarked PDF file called \code{<fout>.pdf}.
}
\section{Ghostscript Trickery}{
  There area a few things to be aware of when compiling with a latex compiler.
  If you are using MiKTeX's TeXworks interface, you can specify a batch
  file that essentially runs the usual \code{latex}-\code{dvips}-\code{ps2pdf} chain. 
  
  A typical batch file looks like this:
  
  \preformatted{@latex -synctex=1 "\%1.tex" && bibtex "\%1" && latex "\%1.tex" && latex "\%1.tex" &&
        dvips "\%1.dvi" && ps2pdf "\%1.ps"}

  \itemize{
  \item In TeXWorks' menu, go to <Edit><Preferences> and select the Tyesetting tab.\cr
  \item In the Processing tools section, press the + button to create a new tool.\cr
  \item Give your new tool a name, then navigate to the batch file (\code{something.bat}) 
  that you created above.\cr
  \item In the arguments box, type in \sQuote{$basename} (without the quotes). Press OK.
  }
  This will be the tool that you can use to build your PDF using the latex compiler.

  Next check your Ghostscript environmental variables. They should look something like:
  \preformatted{GS=gswin64
GSC=gswin64c
GS_OPTIONS=-dEmbedAllFonts=true -dPDFSETTINGS=/printer -dAutoRotatePages=/None
}
  Adding in \code{-c "<</Orientation 0>> setpagedevice"} seems to 
  disable ghostview and/or disables encapsulated postscript's ability to see
  fonts like NimbusSan. I know not why...

  One final tweak will make life easier:
  \enumerate{
    \item Copy & paste \code{gswin64c.exe} from \code{C:\\Apps\\Ghostscript\\gs9.15\\bin} to\cr
      \code{C:\\Apps\\MiKTeX\\miktex\\bin} \cr
    \item Rename \code{mgs.exe} in \code{C:\\Apps\\MiKTeX\\miktex\\bin} to \code{original.mgs.exe} \cr
    \item Rename \code{gswin64.exe} in \code{C:\\Apps\\MiKTeX\\miktex\\bin} to \code{mgs.exe}
  }
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

  The tex code requires the latex packages: \code{geometry}, \code{graphicx},
  \code{caption}, and \code{hyperref}.
}
\seealso{
  \code{\link[PBStools]{buildCatch}},  \code{\link[base]{writeLines}}
}
\keyword{print}

