\name{makeAgeErr}
\alias{makeAgeErr}
\title{
  Make Ageing Error Matrix
}
\description{
  Make an ageing error matrix for Awatea/Coleraine input.
}
\usage{
makeAgeErr(type="simple", strSpp, sql=FALSE,
   Amax=45, ondiag=0.8, offdiag=0.1, corner=0.9,
   Ndiff=5, CV, Noff=1, less=0, more=0,
   png=FALSE, ptype="bubble", lang=c("e","f"))
}
\arguments{
  \item{type}{\code{character} -- type descriptor for building the ageing error matrix of proportions (ranging from 0 to 1).}
  \item{strSpp}{\code{character} -- species Hart code for marine fish or alpha-numeric code for invertebrates.}
  \item{sql}{\code{logical} -- if \code{TRUE}, query the GFBioSQL database for age reader precision records.}
  \item{Amax}{\code{numeric} -- maximum age or plus group for the error matrix.}
  \item{ondiag}{\code{numeric} -- if \code{type="simple"}, value to set along the diagonal of error matrix.}
  \item{offdiag}{\code{numeric} --  if \code{type="simple"}, value to put on each side of diagonal ( for +/- 1 year).}
  \item{corner}{\code{numeric} --  if \code{type="simple"}, value to put in the corners (upper left and lower right) of the diagonal.}
  \item{Ndiff}{\code{numeric} -- number of ages to the left and right of a table column labelled \code{"0"}, which is the diagonal of \code{errmat}.}
  \item{CV}{\code{numeric} -- CV error vector by age.}
  \item{Noff}{\code{numeric} -- number of row offsets from the diagonal of \code{errmat} to spread proportions incorrectly aged.}
  \item{less}{\code{numeric} -- tweak number of offsets \code{Noff} on the left.}
  \item{more}{\code{numeric} -- tweak number of offsets \code{Noff} on the right.}
  \item{png}{\code{logical} -- if \code{TRUE}, send the plot to a \code{'.png'} file.}
  \item{ptype}{\code{character} -- plot type, either \code{"bubble"} or \code{"bars"}.}
  \item{lang}{\code{character} -- language for image labels, where \code{'e'}=english and \code{'f'}=french.}
}
\details{
  Creates an ageing error matrix of proportions from zero to one that gives
  the error spread around each age row-wise, where the true age occurs along the diagonal.
  If the error for an observed age is 0, then the corresponding diagonal element will be 1.
}
\value{
  Depending on \code{'type'}, ageing error matrices can be:
  \itemize{
    \item \code{simple} -- symmetric matrix with constant values for diagonal and one-age off diagonal;
    \item \code{dnorm} -- normal distribution from quantiles 0.01 to 0.99 forced to occur between ages a-Noff and a+Noff;
    \item \code{cverr} -- CV errors-at-age determine standard deviation for a normal distribution centred on each age;
    \item \code{observe} -- observed ageing error from readers' records of minimum and maximum age.
  }
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2023-02-10}
}
\note{
  First used for the Yellowmouth Rockfish (YMR) assessment in 2011.
  Greatly elaborated for Bocaccio (BOR) in 2019.
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{calcVB}},
  \code{\link[PBStools]{extractAges}},
  \code{\link[PBStools]{plotAgeErr}},
  \code{\link[PBStools]{plotProp}},
  \code{\link[PBStools]{quantAges}},
  \code{\link[PBStools]{requestAges}},
  \code{\link[PBStools]{weightBio}}\cr
In package \pkg{PBSawatea}:\cr
  \code{runADMB}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arith}% use one of  RShowDoc("KEYWORDS")
\keyword{manip}% __ONLY ONE__ keyword per line
\concept{M06_Assessment}
