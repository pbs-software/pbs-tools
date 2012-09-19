\name{toUpper}
\alias{toUpper}
\title{
  Capitalise First Letters of Words
}
\description{
  Capitalise the first letter of each word in a sentence or phrase.
}
\usage{
toUpper(x, exclude=c("&","and","exact","or","in","on","organic","pelagic",
   "sp.","spp.","species","undetermined","unidentified","birds","barnacles",
   "crabs","eggs","fishes","larvae","matter","objects","remains","sample",
   "shells","subfamily","tunicates","worms"))
}
\arguments{
  \item{x}{a vector of sentences or phrases}
  \item{exclude}{words to exclude from capitalisation}
}
\details{
  The function uses \code{strsplit} and \code{toupper} along with a whole mess
  of \code{sapply} calls. A vector of sentences or phrases can be passed to \code{x}.
}
\value{
	The input vector where the first letter of all words are capitalised.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link[PBStools]{revStr}}, \code{\link[PBStools]{scaleVec}}, \code{\link[PBStools]{plotConcur}} \cr
  \pkg{PBSdata}: \code{\link[PBSdata]{species}}
}
\keyword{character}

