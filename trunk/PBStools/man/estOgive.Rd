\name{estOgive}
\alias{estOgive}
\title{Estimate Ogive Curves for Maturity}
\description{
  Estimate ogive curves using three methods: \cr
  (i) calculate the cumulative distribution of the proportion of mature fish smoothed by binned ages, \cr
  (ii) use a \code{glm} fit with binomial error linked to a logit function, and \cr
  (iii) fit proportion-at-age data to a double normal curve using the minimizer \code{nlm}.
}
\usage{
estOgive(dat=pop.age, strSpp="", method=c("empir","dblnorm"), 
     sex=list(1,2,1:2), mos=list(1:12,1:12,1:12), mat=3:7, 
     ofld="age", obin=1, xlim=c(0,60), fg=c("blue","red","black"), 
     bg=c("cyan","pink","grey85"), wmf=FALSE, ...)
}
\arguments{
  \item{dat}{biological data set with fields \code{mat} and \code{age}.}
  \item{strSpp}{string code for species. If dataset has attribute \code{spp},
    this will be used before \code{strSpp}.}
  \item{method}{string vector specifying methods used to fit the data: \cr
    \code{"empir"}   - empirical fit to cumulative curves of proportion maturity (only when specified alone), \cr
    \code{"logit"}   - binomial logit fit where maturity is specified as 0 or 1 for each specimen, \cr
    \code{"dblnorm"} - fit proportion-at-ages using a double normal curve.}
  \item{sex}{sex code: 1 = males, 2 = females.}
  \item{mos}{list of months from which maturity data are used; 
    list length matches that of list argument \code{sex}.}
  \item{mat}{codes that define maturity (e.g., 3+).}
  \item{ofld}{ogive field (usually \code{age} or \code{len}).}
  \item{obin}{bin size (in x-units) to group \code{age} or \code{len}.}
  \item{xlim}{range of the x-metric (x-axis).}
  \item{fg}{vector of three foreground colours for the sexes listed in order.}
  \item{bg}{vector of three background colours for the sexes listed in order.}
  \item{wmf}{logical: if \code{TRUE}, send plot to a \code{.wmf} file.}
  \item{...}{additional arguments for use in \code{estOgive}.}
}
\details{
  The cumulative distribution of mature fish is plotted and the point at 
  50\% maturity is interpolated. The x-axis (age or length) of empirical data
  can be binned to smooth out rough or sparse data. The user can also choose two
  methods to fit empirical data: \code{glm} fit of binary data (mature vs. not mature)
  using a linked logit function, or an \code{nlm} fit of the proportions-at-age data
  to a double normal curve (which is the default method used in Coleraine).
}
\value{
  No value is explictly returned by the function. A global list object \code{PBSfish}
  provides the following:
  \item{mdxy}{Last median x-y values.}
  \item{mbin }{Maturities binned by age (or length) group.}
  \item{ld50}{x-value at y = 0.50.}
  \item{ldR}{x-y values at the right end of the logit-fitted curve visible.}
  \item{ldL}{x-y values at the left  end of the logit-fitted curve visible.}
  \item{out }{Results table showing proportions mature at each binned age or length.}
  \item{pmat }{Function calculating proportions mature.}
  \item{obin }{Bin size.}
  \item{strSpp}{String species code used.}
  
  Also, matrices by sex of empirical proportions-at-age and fits using a logit and 
  double normal are dumped to a \code{.csv} file.
}
\note{
  A suitable data set can be obtained by running the SQL query \code{gfb_bio.sql}.

  \code{getData("gfb_bio.sql","GFBioSQL","396",path=.getSpath())}
}
\examples{
pbsfun=function(){
  data(pop.age)
  estOgive(obin=2, xlim=c(0,30), sex=1:2, method=c("empir","dblnorm"), cex=0.9)
  invisible() }
pbsfun()
}
\seealso{
  \code{\link[PBSdata]{pop.age}}, \code{\link[PBStools]{fitLogit}}, 
  \code{\link[PBStools]{mapMaturity}}, \code{\link[PBStools]{getData}}
}
\keyword{hplot}

