\name{makeAgeErr}
\alias{makeAgeErr}
\title{
  Make Ageing Error Matrix
}
\description{
  Make an ageing error matrix for Awatea/Coleraine input.
}
\usage{
makeAgeErr(type="simple", strSpp, sql=FALSE, Amax=45,
   ondiag=0.8, offdiag=0.1, corner=0.9, ndiff=5)
}
\arguments{
  \item{type}{\code{character} -- type descriptor for building the ageing error matrix of proportions (ranging from 0 to 1).}
  \item{strSpp}{\code{character} -- species Hart code for marine fish or alpha-numeric code for invertebrates.}
  \item{sql}{\code{logical} -- if \code{TRUE}, query the GFBioSQL database for age reader precision records.}
  \item{Amax}{\code{numeric} -- maximum age or plus group for the error matrix.}
  \item{ondiag}{\code{numeric} -- if \code{type="simple"}, value to set along the diagonal of error matrix.}
  \item{offdiag}{\code{numeric} --  if \code{type="simple"}, value to put on each side of diagonal ( for +/- 1 year).}
  \item{corner}{\code{numeric} --  if \code{type="simple"}, value to put in the corners (upper left and lower right) of the diagonal.}
  \item{ndiff}{\code{numeric} -- number of ages less than and greater than the actual age to report the error proportions in a table.}
}
\details{
  Creates an ageing error matrix of proportions from zero to one that gives
  the error spread around each age, which occurs along the diagonal.
  If the error for an observed age is 0, then the corresponding diagonal element will be 1.
}
\value{
  Ageing error matrix if type is simple; a list of the error matrix and the table of proportions if type is not simple.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2019-04-12}
}
\note{
  First used for the Yellowmouth Rockfish (YMR) assessment in 2011.
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
  \code{\link[PBSawatea]{runADMB}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arith}% use one of  RShowDoc("KEYWORDS")
\keyword{manip}% __ONLY ONE__ keyword per line
