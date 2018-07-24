\name{linguaFranca}
\alias{linguaFranca}
\title{
  Parler la lingua franca, n'est-ce pas?
}
\description{
  Translate common stock assessment english words (lingua) in figures
  into french words (franca). The function name is shamelessly adopted
  from the term \emph{lingua franca}, which has the opposite goal of 
  providing one language common to speakers of multiple languages (so ironic).
}
\usage{
linguaFranca(x, lang="e", little=4, strip=FALSE)
}
\arguments{
  \item{x}{\code{character} -- a vector of english words common in BC stock assessments.}
  \item{lang}{\code{character} -- a vector of letters that denote the language for output:
    currently only \code{"e"} (english) and \code{"f"} (french).}
  \item{little}{\code{numeric} -- the number of characters that defines a little word that is
    only translated when it appears by itself.}
  \item{strip}{\code{logical} -- if \code{TRUE}, strip off the translated words' names attribute.}
}
\details{
  The translation code is simplistic in that it throws each word or 
  phrase into a series of \code{gsub} statements, i.e., a series of
  pattern recognition calls. This is potentially dangerous in that a
  translated word may subsequently contain a pattern that is later
  matched again and rendered incomprehensible. This won't be a problem
  if the translated words contain accented characters.
  
  To render accented characters correctly (so they don't resemble 
  runes -- Sandinavian, Germanic, or just mysterious magic symbols),
  the code uses Unicode to render the accented letters without causing
  the heads of CRAN Wizards to spin.
}
\value{
  Yes, there is value added. On a functional note, if the language
  specified is english, the input vector is spat back out unadulterated.
  If the language is french (\code{lang="f"}), the vector goes through
  the pattern matching spin cycle and returns a beautiful translation
  that would make the ghosts of Wouters (vision song), Tootoo (love 
  triangle), and Shea (pie in face) proud.
}
\references{
  \href{http://www.dfo-mpo.gc.ca/csas-sccs/process-processus/translation-traduction-eng.html}{CSAS Policy on the translation of documents}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus: Institute of Ocean Sciences (IOS), Sidney BC}\cr
  Last modified \code{Rd: 2018-07-24}
}
\note{
  Contact \href{mailto:ann.mariscak@dfo-mpo.gc.ca}{Ann Mariscak} (CSAP office)
  for more information on acceptable french translations.
}
\seealso{
\code{\link[PBStools]{createFdir}},
\code{\link[PBStools]{makeLTH}},
\code{\link[PBStools]{readClog}},
\code{\link[PBStools]{revStr}},
\code{\link[PBStools]{showError}},
\code{\link[PBStools]{texArray}},
\code{\link[PBStools]{texThatVec}},
\code{\link[PBStools]{wrapText}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
