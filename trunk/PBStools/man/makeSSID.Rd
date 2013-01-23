\name{makeSSID}
\alias{makeSSID}
\title{
  Make a Data Object of Survey Series Information
}
\description{
  Make a data object of survey series information
  called \code{ssid} for use in the package \pkg{PBSdata}.
}
\usage{
makeSSID()
}
\details{
  The function imports the SQL table \code{SURVEY} from 
  the DFO database \code{GFBioSQL} using: \cr
  \code{getData("SURVEY","GFBioSQL")}

  The survey information is then summarised by its series number 
  and saved to a binary \code{.rda} file.
}
\value{
  A list of data frames, one for each database, of survey series.

  \code{gfb} (GFBioSQL): \cr
  Data frame with 118 series (rows) and 4 fields.
  \tabular{ll}{
  \code{ssid}     \tab survey series ID number; also repeated as row names. \cr
  \code{desc}     \tab survey description \cr
  \code{original} \tab logical: if \code{TRUE}, the series comprises an original set of surveys; \cr
                  \tab if \code{FALSE}, the series has been derived for some special purpose. \cr
  \code{trawl}    \tab logical: if \code{TRUE}, the series comprises trawl surveys.
  }
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\note{
  The data object \code{ssid} can be found in the package \pkg{PBSdata}.
}
\seealso{
  \code{\link[PBSdata]{ssid}}, \code{\link[PBStools]{getData}}
}
\keyword{database}
\keyword{utilities}

