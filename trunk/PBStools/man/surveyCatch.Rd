\name{surveyCatch}
\alias{surveyCatch}
\title{
  Survey Catch Summary from GFBioSQL
}
\description{
  Query the \code{GFBioSQL} database for survey catches of the specified species.
}
\usage{
surveyCatch( strSpp="396", spath=.getSpath() )
}
\arguments{
  \item{strSpp}{character string specifying the Hart species code for a species in the \code{GFBioSQL} database.}
  \item{spath}{character string of path to the SQL code file \code{gfb_rcatORF.sql}; \cr
  defaults to the \code{sql} directory under \code{system.file(package="PBStools")} }
}
\details{
  Needs the SQL code file \code{gfb_rcatORF.sql}, which is designed to return 
  survey catch data in a format similar to that for commercial catch builds 
  by the function \code{buildCatch}.
}
\value{
  Invisibly returns a table of catch (t) summarised by calendar year and PMFC area.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link[PBStools]{buildCatch}}, \code{\link[PBStools]{getCatch}}
}
\keyword{database}

