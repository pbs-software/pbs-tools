\name{expand5C}
\alias{expand5C}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Expand PMFC 5C Area
}
\description{
  Expand the PMFC 5C area by reassigning Morebsy Gully in PMFC 5B and Flamingo Inlet/Anthony Island in PMFC 5E to 5C.
  Used only for Pacific Ocean Pearch (POP) and Yellowmouth Rockfish (YMR).
}
\usage{
expand5C(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{\code{data.frame} -- data records with field names:\cr
    \code{X}, \code{Y}, \code{major}, \code{minor}, and \code{locality}.}
}
\details{
    There is only reassignment if geographic coordinates (\code{X},\code{Y})
    can be placed in the 5C expansion area or if combinations of
    (\code{major}, \code{minor}, \code{locality}) identify the fishing grounds:
	\itemize{
		\item 5B -- SE Cape St. James (6, 8, 6);
		\item 5B -- Outside Cape St. James (6, 8, 12);
		\item 5E -- Anthony Island (9, 34, 1);
		\item 5E -- Flamingo Inlet (9, 34, 5).
	}
}
\value{
	Returns the input data frame with field \code{'major'} changed to 7 for any record that is successfully placed in the 5C expansion area.
	A new field called \code{'major_old'} is added and reports the original major area whether or not it was changed.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Regional Headquarters (RHQ), Vancouver BC\cr
  Last modified \code{Rd: 2023-06-06}
}
\note{
  Be aware that any algorithm that relies on combinations of (\code{major}, \code{minor}, \code{locality}), 
  e.g. \code{'plotLocal'}, will be affected by the reassignment of \code{'major'}.
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
