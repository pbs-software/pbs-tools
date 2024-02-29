\name{dot-win.funs}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{.chooseFQT}
\alias{.getApath}
\alias{.getEpath}
\alias{.getSpath}
\alias{.getWpath}
\alias{.grabPath}
\alias{.setApath}
\alias{.setCWD}
\alias{.setEpath}
\alias{.setSpath}
\alias{.setWpath}
\alias{.win.onClose}
\alias{.win.runModHelperQuit}
\title{
  GUI Helper Functions
}
\description{
  Helper routines to facilitate GUI interactions.
}
\usage{
.chooseFQT()
.getApath(where="admb",pkg="PBStools",win=FALSE)
.getEpath(where="examples",pkg="PBStools",win=FALSE)
.getSpath(where="sql",pkg="PBStools",win=FALSE)
.getWpath(where="win",pkg="PBStools",win=FALSE)
.grabPath(where,pkg="PBStools",win=FALSE)
.setApath(where="admb",pkg="PBStools",win=TRUE)
.setCWD(win=TRUE)
.setEpath(where="examples",pkg="PBStools",win=TRUE)
.setSpath(where="sql",pkg="PBStools",win=TRUE)
.setWpath(where="win",pkg="PBStools",win=TRUE)
.win.onClose()
.win.runModHelperQuit()
}
\arguments{
  \item{where}{\code{character} -- string specifying directory in package repository}
  \item{pkg}{\code{character} -- name of package repository}
  \item{win}{\code{logical} -- if \code{TRUE}, set the equivalent variable in the GUI}
}
%- maybe also 'usage' for other objects documented here.
\details{
  PBSmodelling Windows (Graphical User Interfaces or GUIs) either
  need helper functions or use wrapper functions.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-27}
}
\keyword{interface}
\concept{M01_Utility}
