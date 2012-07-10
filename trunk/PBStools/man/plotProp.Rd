\name{plotProp}
\alias{plotProp}
\title{Display Proportions-at-Age Using Bubble Plots}
\description{
  Display proportions-at-age (or perhaps proportions-at-length or proportions-at-weight)
  \emph{vs}. year using the \bold{PBSmodelling} function \code{plotBubbles}
  controlled by an interactive GUI.
}
\usage{
plotProp(fnam="pop.age", hnam=NULL, ...) 
}
\arguments{
  \item{fnam}{ string specifying file name }
  \item{hnam}{string name of a history file}
  \item{...}{additional arguments for \code{plotProp}.}
}
\details{
  This function creates an interactive GUI that can be used to display 
  porportions-at-metric data. 

  \bold{The GUI controls:}

  \tabular{ll}{
  \code{Window}   \tab Button to display the window description file. \cr
  \code{Reset}    \tab Reset all settings in the GUI to an initial state. \cr
  \code{File}     \tab Name of data file containing biological (morphometric) measurements. \cr
  \code{Stratify} \tab Field name of information to stratify proportions data (e.g, \code{SID}). \cr
  \code{weighted} \tab Check to weight proportions by the stratifier field (e.g, \code{catch}). \cr
  \code{Spp}      \tab Species code in field \code{spp}. \cr
  \code{Sex}      \tab ALL (codes 0:3), Male (1), Female (2), Uncertain (3), Unknown (0). \cr
  \code{field}    \tab Field names of \eqn{x} and \eqn{y}. \cr
  \code{limit1}   \tab Lower value limits of \eqn{x} and \eqn{y}. \cr
  \code{limit2}   \tab Upper value limits of \eqn{x} and \eqn{y}. \cr
  \code{interval} \tab Grouping interval for \eqn{x} and \eqn{y} values. \cr
  \code{Agg Y}    \tab Logical: if checked, aggregate the \eqn{y} values outside. \cr
                  \tab    the \eqn{y} limits to their lower or upper limit. \cr
  \code{Reset}    \tab Reset all settings in the (\eqn{x},\eqn{y}) table on the left. \cr
  \code{B&B}      \tab Check for specimens with otoliths broken & burnt (ageing method = 3). \cr
  \code{Gear}     \tab Gear types (see \code{\link{pop.age}} for details). \cr
  \code{Trip}     \tab Trip types (see \code{\link{pop.age}} for details). \cr
  \code{Sample}   \tab Sample types (see \code{\link{pop.age}} for details). \cr
  \code{PMFC}     \tab Pacific Marine Fisheries Commission \code{major} areas. \cr
  \code{SRF}      \tab Slope rockfish assessment areas \code{srfa}. \cr
  \code{Gullies}  \tab Slope rockfish assessment subareas \code{srfs}
  }

  \bold{History widget and various plot controls:}

  \tabular{ll}{
  \code{psize} \tab Maximum size of bubbles (proportional to the difference of \eqn{x}-width) \cr
  \code{powr}  \tab Transformation of \eqn{z} using an exponential power to use for radius of \cr
               \tab    bubbles (\code{powr}=0.5 creates bubble areas proportional to \eqn{z}) \cr
  \code{lwd}   \tab Line thickness for bubbles \cr
  \code{pos}   \tab Colour for bubbes representing positive values \cr
  \code{neg}   \tab Colour for bubbes representing negative values \cr
  \code{zero}  \tab Colour for bubbes representing zero values \cr
  \code{line}  \tab Check for horizontal dotted lines across major y-values \cr
  \code{hide0} \tab Check to hide zero-value bubbles \cr
  \code{ltype} \tab Legend type: \cr
               \tab    1 = Detailed showing number of specimes by \cr
               \tab        \code{ttype}, \code{sstype}, \code{gear}, \code{srfs}, \code{srfa}, \code{major} \cr
               \tab    2 = Number of specimens only \cr
               \tab    3 = Number of samples only \cr
  \code{GO}    \tab Execute the \code{plotProp} procedure with all current settings \cr
  \code{Plot}  \tab Replot without recalculating the current settings \cr
  \code{wmf}   \tab Make a windows metafile image file (\code{.wmf}) from the current display
  }
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link{pop.age}}, \code{\link{getData}}, \cr
  \pkg{PBSmodelling}: \code{\link[PBSmodelling]{plotBubbles}}
}
\keyword{hplot}
