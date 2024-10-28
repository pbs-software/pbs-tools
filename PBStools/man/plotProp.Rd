\name{plotProp}
\alias{plotProp}
\alias{.plotProp.calcP}
\alias{.plotProp.chooseAreas}
\alias{.plotProp.chooseTypes}
\alias{.plotProp.plotP}
\alias{.plotProp.resetP}
\alias{.plotProp.resetT}

\title{GUI: Display Proportions as Bubbles}
\description{
  Display proportions-at-age (or proportions-at-length or proportions-at-weight)
  \emph{vs}. year using the \bold{PBSmodelling} function \code{plotBubbles}
  controlled by an interactive GUI.
}
\usage{
plotProp(fnam, hnam=NULL, ioenv=.GlobalEnv, ...) 

.plotProp.calcP(reload=FALSE)
.plotProp.chooseAreas(gui=TRUE,tab=FALSE)
.plotProp.chooseTypes(gui=TRUE,tab=FALSE)
.plotProp.plotP(wmf=FALSE,png=FALSE)
.plotProp.resetP()
.plotProp.resetT()
}
\arguments{
  \item{fnam}{\code{character} -- string specifying file name}
  \item{hnam}{\code{character} -- string name of a history file}
  \item{ioenv}{\code{environment} -- input/output environment for function input data and output results}
  \item{...}{\code{dots} -- additional arguments for \code{plotProp}}
  \item{reload}{\code{logical} -- if \code{TRUE}, reload data from source (e.g., binary file, package data object)}
  \item{gui}{\code{logical} -- if \code{TRUE}, choose values from GUI}
  \item{tab}{\code{logical} -- if \code{TRUE}, write values to a text file}
  \item{wmf}{\code{logical} -- if \code{TRUE}, create a Windows meta file image}
  \item{png}{\code{logical} -- if \code{TRUE}, create a portable network graphics image}
}
\details{
  This function creates an interactive GUI that can be used to display 
  porportions-at-metric data. 

  \bold{Plot Controls:}

  \tabular{ll}{
  \code{psize}  \tab Maximum size of bubbles (proportional to the difference of \eqn{x}-width) \cr
  \code{powr}   \tab Transformation of \eqn{z} using an exponential power to use for radius of \cr
                \tab    bubbles (\code{powr}=0.5 creates bubble areas proportional to \eqn{z}) \cr
  \code{lwd}    \tab Line thickness for bubbles \cr
  \code{pos}    \tab Colour for bubbes representing positive values \cr
  \code{neg}    \tab Colour for bubbes representing negative values \cr
  \code{zero}   \tab Colour for bubbes representing zero values \cr
  \code{line}   \tab Check for horizontal dotted lines across major y-values \cr
  \code{hide0}  \tab Check to hide zero-value bubbles \cr
  \code{ltype}  \tab Legend type: \cr
                \tab    1 = Detailed showing number of specimes by \cr
                \tab        \code{ttype}, \code{sstype}, \code{gear}, \code{srfs}, \code{srfa}, \code{major} \cr
                \tab    2 = Number of specimens only \cr
                \tab    3 = Number of samples only \cr
  \code{GO}     \tab Execute the \code{plotProp} procedure with all current settings \cr
  \code{Plot}   \tab Replot without recalculating the current settings \cr
  \code{wmf}    \tab Send the plot to a windows metafile image file (\code{.wmf}).\cr
  \code{png}    \tab Send the plot to a portable networks graphics file (\code{.png}).\cr
  \code{screen} \tab Send the plot on the current graphics device to various file formats:\cr
                \tab \code{.bmp}, \code{.jpg}, \code{.png}, \code{.eps}, \code{.pdf}, \code{.wmf}\cr
  \code{csv}    \tab Write the proportions-at-age (\code{pa}) or numbers-at-age (\code{Na}) to \cr
                \tab output files (\code{.csv}, \code{.r}, \code{.rda}). \cr
  }

  \bold{Input Controls:}

  \tabular{ll}{
  \code{File}     \tab Name of data file containing biological (morphometric) measurements. \cr
  \code{Window}   \tab Button to display the window description file. \cr
  \code{R code}   \tab Button to display the R code for the underlying functions of \code{plotProp}. \cr
  \code{Reset GUI} \tab Reset all settings in the GUI to the default state. \cr
  \code{Spp}      \tab Species code in field \code{spp}. \cr
  \code{Sex}      \tab ALL (codes 0:3), Male (1), Female (2), Uncertain (3), Unknown (0). \cr
  \code{field}    \tab Field names of \eqn{x} and \eqn{y}. \cr
  \code{limit1}   \tab Lower value limits of \eqn{x} and \eqn{y}. \cr
  \code{limit2}   \tab Upper value limits of \eqn{x} and \eqn{y}. \cr
  \code{interval} \tab Grouping interval for \eqn{x} and \eqn{y} values. \cr
  \code{Agg Y}    \tab Logical: if checked, aggregate the \eqn{y} values outside. \cr
                  \tab    the \eqn{y} limits to their lower or upper limit. \cr
  \code{Reset table} \tab Reset all settings in the (\eqn{x},\eqn{y}) table on the left. \cr
  \code{B&B}      \tab Check for specimens with otoliths broken & burnt (ageing method = 3). \cr
  \code{Stratify} \tab Field name of information to stratify proportions data (e.g, \code{SID}). \cr
  \code{weighted} \tab Check to weight proportions by the stratifier field (e.g, \code{catch}). \cr
  \code{Areas}    \tab Button activates a GUI of area codes (e.g., major) with check boxes for selection. \cr
  \code{Types}    \tab Button activates a GUI of type codes (e.g., gear) with check boxes for selection. \cr
  \code{Clear}    \tab Button clears all selections made via the previous two buttons. \cr
  \code{History}  \tab History widget to keep track of GUI settings that the user wishes to recall. \cr
  }
}
\note{
  When first invoking the GUI with \code{plotProp}, Press <GO> before qualifing
  the data with the <Areas> and <Types> buttons.
  These should be synchronised with the notebook tabs in the GUI.

  \emph{Warning}: the code was recently tweaked to deal with R objects rather than SQL query results.
  If starting the GUI without an R object, the notebook tabs <Areas> and <Types> will likely not display any options.
  The interactive pop-GUIs should still work, but the functionality has not been tested.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-10-28}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{getData}} \cr
In package \pkg{PBSdata}:\cr
  \code{\link[PBSdata]{pop.age}}\cr
In package \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{plotBubbles}}
}
\keyword{hplot}
\concept{M02_Biology}
