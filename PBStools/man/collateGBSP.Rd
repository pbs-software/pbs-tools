\name{collateGBSP}
\alias{collateGBSP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Collate GBSP Samples
}
\description{
  Collate biological data collected by the Groundfish Biological Sampling
  Program (GBSP) from commercial fishing trips offloaded in various BC ports.
  Specifically, create tables of species samples by fishing year and port.
}
\usage{
collateGBSP(dat, datetag)
}
\arguments{
  \item{dat}{\code{composite} -- data frame results from calling \code{'fos_gfb_offloads.sql'}}
  \item{datetag}{\code{character} -- date stamp to apply to output files}
}
\details{
  Data from a call to the SQL script \code{'fos_gfb_offloads.sql'} 
  gathers biological sample information from commercial trip offloads
  for target species landed shoreside (in ports Ucluelet, Port Hardy, 
  Prince Rupert, and Port Simpson) after March 1, 2023.

  The R function arranges the SQL results into CSV tables,
  summarized by fishing year and port. 

  Samples appearing in GFBioSQL are ready for scientific analyses.
  Samples not appearing in GFBioSQL have been collected Shoreside
  but are still being prosessed (transported from ports to AMR and DFO for
  digitization, verification, and integration to the Oracle database GFBio).
}
\value{
  Tables exported to comma-delimited text files (CSV).
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: remote office, Vancouver BC\cr
  Last modified \code{Rd: 2026-04-10}
}
\note{
  SQL may need to add ports like French Creek for Gulf Hake.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
  In package \pkg{PBStools}:\cr
    \code{\link[PBStools]{getData}},
    \code{\link[PBStools]{SQLcode}}\cr
  In package \pkg{PBSdata}:\cr
    \code{\link[PBSdata]{species}}\cr
  In package \pkg{stats}:\cr
    \code{\link[stats]{aggregate}}\cr
  In package \pkg{utils}:\cr
    \code{\link[utils]{write.csv}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
\keyword{IO}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
\concept{M03_Fishery}
% Use only one concept per line.
