\name{prepClara}
\alias{prepClara}
\title{
  Prepare a Data Object for Clustering Large Applications
}
\description{
  Prepare a data object for use by \code{clarify},
  which uses the \pkg{cluster} function \code{clara}.
}
\usage{
prepClara(ssid=7, gfld=c("X","Y"), sfld="spp", mfld="catKg",
   fnam=NULL, ioenv=.GlobalEnv)
}
\arguments{
  \item{ssid}{survey series ID}
  \item{gfld}{spatial grouping field name; the default is a special case
    where Lon-Lat is combined as a unique identifier.}
  \item{sfld}{species field name}
  \item{mfld}{metric field name (determines the impact of the species)}
  \item{fnam}{if \code{NULL}, the function queries the \code{GFBioSQL} database
    using the SQL query \cr \code{gfb_clara_survey.sql} (at present only queries invertebrates)}
  \item{ioenv}{input/output environment for function input data and output results.}
}
\details{
  The grouping variable \code{gfld} will determine how event data
  are collapsed into spatial units. The Lon-Lat default is likely
  equivalent to a sinlge field identifying each fishing event unless
  events happen to occupy the same Lat-Lon coordinate. A user may wish
  to create a field that delimits grid cells, but these should be the same
  as or smaller than those specified in the function \code{clarify}.

  The function assumes that the input data object is always called \sQuote{dat},
  but a source file can be called anything as long as its import to R
  creates \code{dat}. Similarly, the output data object is always called
  \sQuote{claradat} but can be saved to a system file with any name. The function 
  automatically names the output file containing \code{claradat} depending
  on whether an input name \code{fnam} was specified or SQl code was 
  executed to get the input data.
}
\value{
  Returns the data frame \code{claradat} for use in \code{clarify}.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link[PBStools]{clarify}}, \code{\link[PBStools]{crossTab}},
  \code{\link[PBStools]{getData}}, \code{\link[PBStools]{getFile}}
}
\keyword{manip}

