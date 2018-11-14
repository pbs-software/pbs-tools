\name{inWord}
\alias{inWord}
\title{
  Is Morpheme in Word?
}
\description{
  Determine whether user-specified morphemes occur within a target word.
}
\usage{
inWord(word, morpheme, prefix=FALSE, suffix=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{word}{\code{character} -- target word (e.g. \code{'rockfish'}).}
  \item{morpheme}{\code{character} -- vector of word fragments (e.g. \code{c('rock','fish','eelpout')}).}
  \item{prefix}{\code{logical} -- if \code{TRUE}, find morphemes that occur at start of target word.}
  \item{suffix}{\code{logical} -- if \code{TRUE}, find morphemes that occur at end of target word.}
}
\details{
  Take each character in \code{'morpheme'} and add an option to match any 
  letter in \code{'word'} 0 or more times. (See user \code{'rawr'} response 
  2015-11-02 in reference below).
}
\value{
  Logical vector (\code{T|F}) with same length as input vector \code{'morpheme'}.
}
\references{
  \href{https://stackoverflow.com/questions/33483286/r-look-for-abbreviation-in-full-string}{Stack Overflow -- R look for abbreviation in full string}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2018-11-13}
}
\seealso{
  \code{\link[PBStools]{countLines}},
  \code{\link[PBStools]{findRC}},
  \code{\link[PBStools]{flagIt}},
  \code{\link[PBStools]{isThere}},
  \code{\link[PBStools]{linguaFranca}},
  \code{\link[PBStools]{prime}},
  \code{\link[PBStools]{readClog}},
  \code{\link[PBStools]{revStr}},
  \code{\link[PBStools]{showError}},
  \code{\link[PBStools]{texArray}},
  \code{\link[PBStools]{texThatVec}},
  \code{\link[PBStools]{toUpper}},
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

