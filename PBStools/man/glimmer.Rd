\name{glimmer}
\alias{glimmer}
\title{Perform Standardised GLM Analysis}
\description{
  Perform a standardised GLM analysis and plot the results.
}
\usage{
glimmer(file, facs=c("year","month","depth","vessel"),
     afld="latitude", yrs=1996:2014, mos=1:12, mo1=1, 
     dbrks=seq(100,500,100), gear=c(0,1,3), catch="catch", 
     areas=NULL, Vpro=0.03, Uplot=TRUE, Upanel=0.4, 
     Ionly=FALSE, Imax=NULL, crange=c(-2,2), erange=c(0,5), 
     facmin=10, effmax=1440, vline=NULL, wmf=FALSE, ioenv=.GlobalEnv)
}
\arguments{
  \item{file}{ name of file or data object containing fields for GLM analysis: \cr
    \code{year, month, depth, latitude, vessel, effort, catch}. \cr
    See the SQL file \code{pht_glm.sql}.}
  \item{facs}{string vector: factors (fields) other than those that describe area.}
  \item{afld}{string scalar: factor describing spatial area, choose one of: \cr
    \code{c("latitude", "major", "minor", "locality", "pfma", "pfms",} \cr 
    \code{"srfa", "srfs", "popa")}.}
  \item{yrs}{numeric vector of years.}
  \item{mos}{numeric vector of months.}
  \item{mo1}{first month in year (\emph{e.g.}, 1=calendar year, 4=fishing year).}
  \item{dbrks}{either a numeric vector of two or more cut points or a single number 
    (greater than or equal to 2) giving the number of intervals into which depth is to be cut.}
  \item{gear}{numeric vector: gear codes for trawl fishery (\emph{e.g.}, 1=bottom, 3=midwater).}
  \item{catch}{string scalar: field containing catch numbers.}
  \item{areas}{vector: specific area names/codes within \code{afld}.}
  \item{Vpro}{numeric scalar: minimum vessel's proportion of catch to total catch.}
  \item{Uplot}{logical: if \code{TRUE}, calculate and plot CPUE index and effects on the CPUE index.
    If \code{FALSE}, plot the parameter coefficients.}
  \item{Upanel}{numeric scalar: proportion of space to plot index panel if \code{Uplot=TRUE}.}
  \item{Ionly}{logical: if \code{TRUE}, plot only the first panel (index).}
  \item{Imax}{numeric scalar: maximum index value for annual index plot.}
  \item{crange}{numeric vector: y-limits (min,max) for coefficient plots.}
  \item{erange}{numeric vector: y-limits (min,max) for factor effect plots.}
  \item{facmin}{numeric vector: minimum number of values before a category is used in the factor.
    Separate minima can be specified for each factor.}
  \item{effmax}{effort maximum; all records with effort greater than this are discarded.}
  \item{vline}{numeric vector: value(s) on x-axis to place a vertical dashed line.}
  \item{wmf}{logical: if \code{TRUE}, send plot to a \code{.wmf} file.}
  \item{ioenv}{input/output environment for function input data and output results.}
}
\details{
  This function calculates parameter coefficients using the \code{lm} and 
  \code{aov} functions in the \code{stats} package. (The \code{aov} results are not
  currently used for anything.). See Section 7 of Schnute \emph{et al}. (2004) for a 
  detailed discussion on \sQuote{Standardized CPUE Analyses}.
}
\value{
  List \code{PBStool} containing:
  \item{dat}{Data frame of processed data to pass to \code{lm} and \code{aov}.}
  \item{coeffs}{Named vector of coefficients calculated by \code{lm}.}
  \item{lmres}{Results of \code{lm} analysis}
  \item{lmsum}{Summary (list) of \code{lmres} with \code{correlation=TRUE}.}
  \item{aovres}{Results of \code{aov} analysis}
  \item{facmin}{Vector of minimum number of records per category for the specified factor.}
  \item{year}{Data frame of parameter coefficients and standard errors for \code{year} factor.}
  \item{month}{Data frame of parameter coefficients and standard errors for \code{month} factor.}
  \item{depth}{Data frame of parameter coefficients and standard errors for \code{depth} factor.}
  \item{latitude}{Data frame of parameter coefficients and standard errors for \code{latitude} factor.}
  \item{vessel}{Data frame of parameter coefficients and standard errors for \code{vessel} factor.}
  \item{brR}{Slope (\eqn{b}), annualized rate (\eqn{r}), and accumulated rate (\eqn{R}) 
    from the lm-fit through the log\eqn{_2} annual indices.}
}
\references{
  Schnute, J., Haigh, R., Krishka, B., Sinclair, A. and Starr, P. (2004) 
  The British Columbia longspine thornyhead fishery: analysis of survey and 
  commercial data (1996--2003). \emph{Canadian Science Advisory Secretariat,
  Research Document} \bold{2004/059}. 75 p.
}
\author{ 
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2019-10-02}
}
\examples{
\dontrun{
local(envir=.PBStoolEnv,expr={
pbsfun=function(){
  data(testdatC,envir=.PBStoolEnv); x = testdatC
  glmdat=data.frame(
    year=as.numeric(substring(x$date,1,4)),
    month=as.numeric(substring(x$date,6,7)),
    depth=x$fdep,latitude=x$Y,vessel=x$cfv,
    effort=x$eff,catch=x$POP)
  attr(glmdat,"spp")="396"
  glimmer(glmdat,Vpro=.025,dbrks=seq(100,500,50))
  invisible() }
pbsfun()
})
}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{getData}},
  \code{\link[PBStools]{SQLcode}}\cr\cr
In package \pkg{stats}:\cr
  \code{\link[stats]{lm}},
  \code{\link[stats]{aov}}
}
\keyword{hplot}
\concept{M03_Fishery}
