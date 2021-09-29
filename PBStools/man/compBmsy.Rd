\name{compBmsy}
\alias{compBmsy}
\title{
  Compare Biomass Posteriors Relative to Bmsy
}
\description{
  Compare posterior distributions of biomass from MCMCs for species and model runs. 
  Biomass can be relative to \eqn{B_{\mathrm{MSY}}}{Bmsy} or other \eqn{B} values.
}
\usage{
compBmsy(Bspp, spp="POP", Mnams=c("Est M","Fix M"),
   ratios=c(0.4,0.8), oratios=NULL, t.yr=2018,
   quants=c(0.05,0.25,0.5,0.75,0.95),
   zones = c("Critical","Cautious","Healthy"),
   figgy=list(win=T), pngres=400, width=12, height=9, 
   rcol=c("red","green4","blue"), rlty=rep(2,3), rlwd=rep(2,3),
   ocol = c("#D55E00", "#009E73", "#56B4E9", "#F0E442"),
   lcol = c("red","darkorange","green4"),
   spplabs=TRUE, left.space=NULL, top.space=2, fout=NULL, 
   offset=c(-0.1,0.1), calcRat=TRUE, refpt="MSY", param=NULL, 
   boxlim=NULL, lang=c("e","f"), ...)
}
\arguments{
  \item{Bspp}{\code{numeric}: list of species MCMC results, which are lists of model runs, 
    each listing data frames of \eqn{B_t}{Bt} and \eqn{B_\mathrm{MSY}}{Bmsy}.}
  \item{spp}{\code{character}: species code(s) (e.g., 3-letter), which are the first-level names of \code{Bspp}.}
  \item{Mnams}{\code{character}: optional model names for the boxplots.}
  \item{ratios}{\code{numeric}: reference levels of \eqn{B_\mathrm{MSY}}{Bmsy} (usually 0.4 and 0.8).}
  \item{oratios}{\code{numeric}: other reference levels (e.g., proportion of \eqn{B_\mathrm{avg}}{Bavg}; \eqn{0.4B_\mathrm{avg}\equiv0.4B_0}{0.4Bvag~0.4B0}).}
  \item{t.yr}{\code{numeric} year that represents the time period for \eqn{B_t}{Bt}.}
  \item{quants}{\code{numeric}: vector of 5 quantiles that define the boxplot --\cr
    low whisker, box bottom, box middle, box top, high whisker.}
  \item{zones}{\code{character}: vector of names for zones delimited by \code{ratios}.}
  \item{figgy}{\code{logical}: if \code{TRUE}, send figure to four output files (\code{.eps}, \code{.pdf}, \code{.png}, \code{.wmf}).}
  \item{pngres}{\code{numeric}: resolution (pixels/inch) of \code{.png} file.}
  \item{width}{\code{numeric}: width of the output files in inches.}
  \item{height}{\code{numeric}: height of the output files in inches.}
  \item{rcol, rlty, rlwd}{\code{character|numeric}: vector of colour names, line types, and line widths, respectively, for the vertical lines depicting \code{ratios}.}
  \item{ocol}{\code{character}: vector of colour names for the dots depicting \code{oratios} (e.g. LRP and USR).}
  \item{lcol}{\code{character}: vector of colour names for the \code{zones} labels.}
  \item{spplabs}{\code{logical}: if \code{TRUE}, use the species code when labelling the y-axis.}
  \item{left.space}{\code{numeric}: amount of space to leave on left axis for labelling.}
  \item{top.space}{\code{numeric}: amount of space to leave at top of plot for labelling.}
  \item{fout}{\code{character}: name for output files (overrides internally-generated names).}
  \item{offset}{\code{numeric}: vertical offsets for LRP and USR from the horizontal quantile boxes.}
  \item{calcRat}{\code{logical}: if \code{TRUE}, calculate the ratio from the data \cr (e.g. \code{x[['Bt.MCMC']] / x[['Bmsy.MCMC']]}).}
  \item{refpt}{\code{character}: reference point subscript to appear in legend (e.g. \code{'MSY'}).}
  \item{param}{\code{character}: parameter name (e.g. \code{'M_2'}) if comparing parameter estimates instead of stock status ratios.}
  \item{boxlim}{\code{numeric}: horizontal boxplot limits to override \code{ylim} (which looks like \code{xlim}), useful for parameter comparisons.}
  \item{lang}{\code{character} -- a vector of letters that denote the language for output:
    currently only \code{'e'} (english) and \code{'f'} (french).}
  \item{\dots}{additional values for \code{\link[PBStools]{quantBox}}'s argument \code{pars}.}
}
\details{
  An example of the input list \code{Bspp}: \cr
  \code{List of 2} \cr
  \code{..$ POP:List of 2} \cr
  \code{....$ run23:List of 3} \cr
  \code{......$ B0.MCMC  : num [1:1000]} \cr
  \code{......$ Bt.MCMC  : num [1:1000]} \cr
  \code{......$ Bmsy.MCMC: num [1:1000]} \cr
  \code{....$ run16:List of 3} \cr
  \code{......$ B0.MCMC  : num [1:1000]} \cr
  \code{......$ Bt.MCMC  : num [1:1000]} \cr
  \code{......$ Bmsy.MCMC: num [1:1000]} \cr
  \code{....- attr(*, "spp")= chr "POP"} \cr
  \code{..$ YMR:List of 2} \cr
  \code{....$ 29.01:List of 3} \cr
  \code{......$ B0.MCMC  : num [1:1000]} \cr
  \code{......$ Bt.MCMC  : num [1:1000]} \cr
  \code{......$ Bmsy.MCMC: num [1:1000]} \cr
  \code{....$ 30.01:List of 3} \cr
  \code{......$ B0.MCMC  : num [1:1000]} \cr
  \code{......$ Bt.MCMC  : num [1:1000]} \cr
  \code{......$ Bmsy.MCMC: num [1:1000]} \cr
  \code{....- attr(*, "spp")= chr "YMR"} \cr

  The function creates a set of horizontal boxes delimited by the 
  quantiles (0.05, 0.25, 0.50, 0.75, 0.95) that illustrate the posterior
  MCMC samples of biomass relative to \eqn{B_\mathrm{MSY}}{Bmsy}. The default 
  reference points (0.4\eqn{B_\mathrm{MSY}}{Bmsy} and 0.8\eqn{B_\mathrm{MSY}}{Bmsy}) are 
  shown by vertical dashed lines.
}
\value{
  Invisibly returns the boxplot list object \code{Bmsy}.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2021-09-28}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{compAF}},
  \code{\link[PBStools]{plotRecon}},
  \code{\link[PBStools]{quantAges}},
  \code{\link[PBStools]{quantBox}}.
}
\keyword{hplot}
\concept{M06_Assessment}

