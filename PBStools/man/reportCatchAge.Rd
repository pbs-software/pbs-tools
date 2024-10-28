\name{reportCatchAge}
\alias{reportCatchAge}
\alias{.reportCatchAge.checkssb}
\alias{.reportCatchAge.getrep}
\alias{.reportCatchAge.plotrep}

\title{GUI: Display Results from Catch-Age Analysis}
\description{
  Display various results plots for catch-age analysis from 
  ADMB using an interactive GUI. 
}
\usage{
reportCatchAge(prefix="pop", path=getwd(), hnam=NULL, ...)

.reportCatchAge.checkssb()
.reportCatchAge.getrep()
.reportCatchAge.plotrep(plotcodes,unique.only=TRUE)
}
\arguments{
  \item{prefix}{\code{character} -- name of an ADMB project.}
  \item{path}{\code{character} -- path to the ADMB files:\cr
    (\code{'.tpl'}, \code{'.dat'}, \code{'.pin'}, \code{'.rep'});\cr
    default is current working directory.}
  \item{hnam}{\code{character} -- name of a history file.}
  \item{...}{\code{dots} -- additional arguments for function \code{reportCatchAge}.}
  \item{plotcodes}{\code{character} -- two-letter plot codes (see GUI controls below)}
  \item{unique.only}{\code{logical} -- if \code{TRUE}, get unique \code{plotcodes}}
}
\details{
  The function \code{'reportCatchAge'} creates an interactive GUI 
  that can be used to display the ADMB results from catch-age analysis.
  
  Original graphs designed by Jon Schnute in Schnute \emph{et al.} (2001).

  \tabular{ll}{
  \bold{GUI controls:} \tab \cr
  \code{Prefix}      \tab Prefix for an ADMB project.\cr
  \code{WDF}         \tab Button to display the window description file.\cr
  \code{R code}      \tab Button to display the R code for this function.\cr
  \code{TPL}         \tab Button to display the ADMB input files (\code{*.tpl}, \code{*.dat}, \code{*.pin}).\cr
  \code{REP}         \tab Button to display the ADMB output report file.\cr
  \code{GET}         \tab Button to get the ADMB report \code{*.rep} file.\cr
  \code{path}        \tab Path to the ADMB files: \code{*.tpl, *.dat, *pin, *.rep}.\cr
  \code{admb}        \tab Button to set path to the library \code{admb} directory.\cr
  \code{cwd}         \tab Button to set path to the current working directory.\cr
  \code{time steps}  \tab Number of time steps for the simulation.\cr
  \code{catch steps} \tab Number of catch steps for the simulation.\cr
  \code{catch range} \tab Limits of the simulated catch.\cr
  \code{recruit productivity} \tab Recruitment productivity for the simulation.\cr
  \code{r range}     \tab Recruitment productivity range and number of steps.\cr
  \code{recruit offset} \tab Number of years from recruitment age class to display.\cr
  \code{HS trace}    \tab Check button for tracing the harvest strategy trajectory.\cr
  \code{HS regime}   \tab Check button for showing regime changes in harvest strategy.\cr
  \code{History widget} \tab \cr
  \code{SMW}         \tab Button to display selectivity, maturity, and weight.\cr
  \code{Age}         \tab Button to display age bubble plots.\cr
  \code{Rec}         \tab Button to display recruitment and productivity.\cr
  \code{Sim}         \tab Button to display forward simulation.\cr
  \code{Risk}        \tab Button to display sustainable catch and productivity.\cr
  \code{BPA}         \tab Button to display biomass and precautionary approach \cr
                     \tab   (harvest strategy) diagram. \cr
  \code{PLOT}        \tab Button to display plots checked off below.\cr
  \code{Menu}        \tab Button to display a menu of codes for plots.\cr
  \code{AA}          \tab Ages actual \cr
  \code{AP}          \tab Ages predicted \cr
  \code{BE}          \tab Biomass estimates \cr
  \code{CP}          \tab Catch (sustainable) vs. productivity \cr
  \code{HS}          \tab Harvest strategy (precautionary approach) \cr
  \code{PP}          \tab Probability of achieving productivity \cr
  \code{RN}          \tab Recruitment numbers \cr
  \code{RP}          \tab Recruitment productivity \cr
  \code{SB}          \tab Simulation: biomass at fixed catches \cr
  \code{SG}          \tab Simulation: growth rate vs. catch \cr
  \code{SM}          \tab Selectivity and maturity vs. age \cr
  \code{WA}          \tab Weight vs. age \cr
  \code{plot name}   \tab File name for figure sent to PDF, PNG, or WMF format.
  }
}
\references{
  Schnute, J.T., Haigh, R., Krishka, B.A., and Starr, P. (2001)
  \href{https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2001/2001_138-eng.htm}{Pacific ocean perch assessment for the west coast of Canada in 2001}.
  \emph{Canadian Science Advisory Secretariat, Research Document} \bold{2001/138}. 90 pp.\cr
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-10-28}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{plotProp}}\cr
In package \pkg{PBSdata}:\cr
  \code{\link[PBSdata]{pop.age}} \cr
In package \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{admb}}
}
\keyword{hplot}
\concept{M02_Biology}
