\name{adjustMajor}
\alias{adjustMajor}
\alias{expand5C}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Adjust PMFC Areas for GMU and GFB
}
\description{
  \code{adjustMajor} -- Evolution of function \code{'expand5C'}:
  \itemize{
    \item expand PMFC major areas for POP and YMR to be more 
      like those used by the Groundfish Management Unit (GMU);
    \item adjust new major area codes in the DFO Biological database
    \code{GFBioSQL} for samples taken shoreside from landings by
    trips that cover two PMFC areas;
    \item future adjustments may be added for other species like
      outside Walleye Pollock (WAP).
  }
  \code{expand5C} -- Expand PMFC 5C area by reassigning Morebsy Gully
  in PMFC 5B and Flamingo Inlet/Anthony Island in PMFC 5E to 5C;
  used only for Pacific Ocean Pearch (POP) and Yellowmouth Rockfish (YMR);
  largely \emph{deprecated} in favour of \code{adjustMajor}.
}
\usage{
adjustMajor(dat, strSpp, plot=FALSE, poly=c("5A","5C"), 
   png=FALSE, pngres=400, lang=c("f","e"))

expand5C(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{\code{data.frame} -- data records with field names:\cr
    \code{X}, \code{Y}, \code{major}, \code{minor}, and \code{locality}.}
  \item{strSpp}{\code{character} -- Hart code or three-letter code for a single species}
  \item{plot}{\code{logical} -- if \code{TRUE}, plot the results (mainly for debugging)}
  \item{poly}{\code{character} -- choice of polygons to plot (`5A' and/or `5C' for POP and YMR)}
  \item{png}{\code{logical} -- if \code{TRUE}, send plot to PNG file}
  \item{pngres}{\code{numeric} -- PNG resolution in pixels/inch}
  \item{lang}{\code{character} -- language of text in PNG: either english (\code{'e'}) or french (\code{'f'})}
}
\details{
  There is only reassignment if geographic coordinates (\code{X},\code{Y})
  can be placed in the expansion area(s) or if combinations of
  (\code{major}, \code{minor}, \code{locality}) identify the fishing grounds.\cr

  5C expansion for POP/YMR:
  \itemize{
    \item 5B -- SE Cape St. James (6, 8, 6)
    \item 5B -- Outside Cape St. James (6, 8, 12)
    \item 5E -- Anthony Island (9, 34, 1)
    \item 5E -- Flamingo Inlet (9, 34, 5)
  }
  5A expansion for POP:
  \itemize{
    \item 3D -- south to Brooks Peninsula (4, 27, \{1,2,3,6,8\})
  }

  Reassignment of new GFBioSQL major codes that cover two
  PMFC areas to single PMFC areas. The new codes are for 
  biological samples taken shoreside from landings by trips
  that cover two PMFC areas:
  \itemize{
    \item 3CD  (code 71) assigned to 3C (code 3)
    \item 3D5A (code 72) assigned to 3D (code 4)
    \item 5AB  (code 73) assigned to 5A (code 5)
    \item 5BC  (code 74) assigned to 5B (code 6)
    \item 5CD  (code 75) assigned to 5C (code 7)
    \item 5DE  (code 76) assigned to 5D (code 8)
    \item 5BE  (code 77) assigned to 5E (code 9)
  }

  Future adjustments may be added for other species like
  outside Walleye Pollock (WAP):
  \itemize{
    \item PMFC minor area 12 (Queen Charlotte Strait) is added to major PMFC 5A (i.e. expand 5A)
    \item PMFC minor area 20 (Juan de Fuca Strait) is added to major PMFC 3C (i.e. expand 3C)
  }
}
\value{
  Returns the input data frame with field \code{'major'} changed to 7 for 
    any record that is successfully placed in the 5C expansion area.
  Similarly, returns the input data frame with field \code{'major'} changed to 5 for
    any record that is successfully placed in the 5A expansion area.
	A new field called \code{'major_init'} is added and reports the original major area whether or not it was changed.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2025-06-10}
}
\note{
  Be aware that any algorithm that relies on combinations of (\code{major}, \code{minor}, \code{locality}), 
  e.g. \code{'plotLocal'}, will be affected by the reassignment of \code{'major'}.
  
  The function \code{'adjustMajor'} could also be used to shrink PMFC areas.
  For example, expanding PMFC areas for outside (offshore) WAP
  implies a shrinking of PMFC areas for inside (gulf) WAP.
  
  Some of the stock re-definition is currently handled by functions
  \code{'calcStockArea'} and \code{'calcWAParea'}, but the implementation is 
  \ifelse{latex}{\out{na\"{i}ve}}{\ifelse{html}{\out{na&#239;ve}}{naive}}.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{buildCatch}},
  \code{\link[PBStools]{calcStockArea}},
  \code{\link[PBStools]{calcVB}},
  \code{\link[PBStools]{plotLocal}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M01_Utility}
% \concept{ ~cpt2 }
% Use only one concept per line.
