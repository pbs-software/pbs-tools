\name{reportCatchAge}
\alias{reportCatchAge}
\title{GUI to Display Results from Catch-Age Analysis}
\description{
  Display various results plots for catch-age analysis from 
  ADMB using an interactive GUI. 
}
\usage{
reportCatchAge(prefix="pop", path=getwd(), hnam=NULL, ...)
}
\arguments{
  \item{prefix}{string specifying the name of an ADMB project.}
  \item{path}{string specifying the path to the ADMB files 
    \code{(*.tpl, *.dat, *.pin, *.rep)}; default is current working directory.}
  \item{hnam}{string specifying the name of a history file.}
  \item{...}{additional arguments for \code{reportCatchAge()}.}
}
\details{
  The function \code{reportCatchAge()} creates an interactive GUI 
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
  Schnute, J.T., Haigh, R., Krishka, B.A., and Starr, P. (2001) Pacific
  ocean perch assessment for the west coast of Canada in 2001. \emph{Canadian
  Science Advisory Secretariat, Research Document} \bold{2001/138}. 90 pp.\cr
  \url{http://www.dfo-mpo.gc.ca/csas/Csas/publications/ResDocs-DocRech/2001/2001_138_e.htm}
}
\seealso{
  \code{\link{plotProp}}, \code{\link{pop.age}} \cr
  \pkg{PBSadmb}: \code{\link[PBSadmb]{admb}}
}
\keyword{hplot}
