\name{plotHBLL}
\alias{plotHBLL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot HBLL Survey Sets
}
\description{
  Plot locations of the longline sets used in the HBLL surveys (north and south)
}
\usage{
plotHBLL(dat, xlim=c(-134.5,-122.5), ylim=c(48,54.8), 
   png=FALSE, pngres=400, PIN=c(9,8.25), lang=c("f","e"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{\code{numeric} -- data frame from a call to \code{'getHBLL()'},
    a function not in package because it uses the wicked machinations of Wickham}
  \item{xlim}{\code{numeric} -- limits of the x-axis (longitude)}
  \item{ylim}{\code{numeric} -- limits of the y-axis (latitude)}
  \item{png}{\code{logical} -- if \code{TRUE}, send the figure to a \code{'.png'} file}
  \item{pngres}{\code{numeric} -- resolution of the png file in pixels/inch}
  \item{PIN}{\code{numeric} -- plot size in inches}
  \item{lang}{\code{character} -- if \code{'e'}, plot the figure using english words;
    if \code{'f'}, plot the figure using french words; c'est toute}
}
\details{
  Plot the LL locations of the HBLL surveys to determine their coverage along the BC coast.
}
\value{
  Rien
}
\references{
Doherty, B., Benson, A.J., and Cox, S.P. (2019)
\href{https://waves-vagues.dfo-mpo.gc.ca/Library/40771118.pdf}{%
Data summary and review of the PHMA hard bottom longline survey in British Columbia after the first 10 years (2006-2016)}.
\emph{Can. Tech. Rep. Fish. Aquat. Sci.} \bold{3276}: ix + 75 p.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-10-28}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{hplot}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M04_Survey}
% \concept{ ~cpt2 }
% Use only one concept per line.
