\name{compVB}
\alias{compVB}
\title{
  Compare von Bertalanffy Fits
}
\description{
  Compare fitted von Bretalanffy curves using previously estimated parameters.
}
\usage{
compVB(dat, index, A=1:40, subset=list(sex=c("Male","Female")), 
   col=c("blue","green4","red"), lty=c(1,2,3), ymax,
   outnam="compVB-RSR", png=FALSE, pngres=400, lang=c("e","f"))
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{\code{numeric list} -- list of matrices where each row represents
    a fitted curve with four entries (columns) giving the number (n) of data pairs
    used in the estimation and three estimated parameters (Linf, K, t0).}
  \item{index}{\code{numeric} -- index numbers of the list indicating which curves to compare.}
  \item{A}{\code{numeric} -- vector of ages spanning the desired range for calculating y-values (e.g.,lengths).}
  \item{subset}{\code{character} -- plot type, but there is only one choice (\code{"sex"}) currently.}
  \item{col}{\code{character|numeric} -- vector of colours to use for each \code{index}.}
  \item{lty}{\code{numeric} -- vector of line types to use for each \code{index}.}
  \item{ymax}{\code{numeric} -- maximum y-value for plot.}
  \item{outnam}{\code{character} -- name for output file (\code{.png}).}
  \item{png}{\code{logical} -- if \code{TRUE}, save the plot as a portable network graphics (\code{".png"}) file.}
  \item{pngres}{\code{numeric} -- resolution (pixels/inch) of \code{".png"} file.}
  \item{lang}{\code{character} -- a vector of letters that denote the language for output:
    currently only \code{"e"} (english) and \code{"f"} (french).}
}
\details{
  Plots vonB fits colour-coded by \code{index} and separated by sex -- females using solid lines, males using dashed lines.
}
\value{
  Nada and rien
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2020-07-16}
}
\note{
  Code is currently hardwired to loop through \code{"Males"} and \code{"Females"}.
}
\seealso{
\code{\link[PBStools]{calcVB}},
\code{\link[PBStools]{compLen}},
\code{\link[PBStools]{plotAgeErr}},
\code{\link[PBStools]{plotMW}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the R documentation directory.
\keyword{hplot}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
