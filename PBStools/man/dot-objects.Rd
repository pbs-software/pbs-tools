\name{dot-objects}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{.colBlind}
\alias{.colGnuplot}
\alias{.flush.cat}
\alias{.PBSserver}
\alias{.plotDev}
\alias{.rgbBlind}
\alias{.su}
\title{
  Objets d'art
}
\description{
  Hidden objects for convenience.
}
\usage{
.colBlind
.colGnuplot
.flush.cat(...)
.PBSserver
.plotDev(nam=NULL,act=NULL,lang=c("e"))
.rgbBlind
.su(x)
}
\arguments{
  \item{...}{\code{dots} -- stuff to pass to function \code{cat}}
  \item{nam}{\code{character} -- name (prefix) of output image file}
  \item{act}{\code{character} -- name (suffix) of output image file (e.g., \code{png})}
  \item{lang}{\code{character} -- language for image (\code{e}=english, \code{f}=french)}
  \item{x}{\code{character|numeric} -- vector of values for \code{sort(unique(x))} }
}
%- maybe also 'usage' for other objects documented here.
\details{
  A small collection of functions and data objects (examples below) for lazy people.

\code{.colBlind}
\verb{
    black    orange   skyblue bluegreen    yellow      blue vermillion redpurple
"#000000" "#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" "#CC79A7"
}
\code{.colGnuplot}
\verb{
"#e41a1c" "#377eb8" "#4daf4a" "#984ea3" "#ff7f00" "#ffff33" "#a65628" "#f781bf"
}
\code{.PBSserver[1]}
\verb{
         GFDB
"10.114.52.8"
}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-29}
}
\keyword{misc}
\concept{M01_Utility}
