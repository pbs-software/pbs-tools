\name{buildCatch}
\alias{buildCatch}
\title{
  Catch Reconstruction Algorithm for BC Rockfish
}
\description{
  A catch reconstruction algorithm for BC rockfish that uses: \cr
  (i) reliable landings from modern databases, \cr
  (ii) estimated landings using ratios of species catch to ORF/TRF catch, \cr
  (iii) estimated discards using discard ratios from observer logs.
}
\usage{
buildCatch(dbdat, sql=FALSE, strSpp="424", dfld="ORF", 
     major=c(1,3:9), refyrs=1997:2005, fidout=c(1:5,10), 
     saveinfo=TRUE, wmf=FALSE, uid=Sys.info()["user"], pwd=uid,
     reconstruct=TRUE)
}
\arguments{
  \item{dbdat}{a list object of landing records from eight DFO databases: \cr
    (1) \code{ph3dat} = \code{PacHarv3} (all fisheries) actually called \code{HARVEST_V2_0}
      on the \code{ORAPROD} Oracle server, \cr
    (2) \code{gfcdat} = \code{GFCatch} (trawl, trap, h&l) on the \code{SVBCPBSGFIIS} SQL server, \cr
    (3) \code{phtdat} = \code{PacHarvest} (groundfish trawl) on the \code{SVBCPBSGFIIS} SQL server, \cr
    (4) \code{phhdat} = \code{PacHarvHL} (Halibut bycatch from DMP validated landings) on the \code{SVBCPBSGFIIS} SQL server, \cr
    (5) \code{phs dat} = \code{PacHarvSable} (Sablefish trap) on the \code{SVBCPBSGFIIS} SQL server, \cr
    (6) \code{phhdat} = \code{PacHarvHL} (validated landings Sched II & ZN) on the \code{SVBCPBSGFIIS} SQL server, \cr
    (7) \code{phfdat} = \code{PacHarvHL} (fisherlog records Sched II & ZN) on the \code{SVBCPBSGFIIS} SQL server, \cr
    (8) \code{fosdat} = \code{GFFOS} (all fisheries) on the \code{GFSH} Oracle server.}
  \item{sql}{logical: if \code{TRUE} query the databases, otherwise load 
    catch records from binary files saved from a previous query run (saves time).}
  \item{strSpp}{character string specifying the Hart species code for the rockfish to be reconstructed (\dQuote{RRF}).}
  \item{dfld}{field name of the denominator in the ratio of RRF 
    to other rockfish (usually \dQuote{ORF} but can be \dQuote{TRF} if total rockfish is more appropriate).}
  \item{major}{major PMFC area codes in which catch is reconstructed (usually \code{c(1,3:9)}).}
  \item{refyrs}{reference years to use for ratio calculations.}
  \item{fidout}{fishery IDs for which an annual series barplot stacked by PMFC area is produced: \cr
    \code{1}  = groundfish trawl, \cr
    \code{2}  = Halibut longline, \cr
    \code{3}  = Sablefish trap, \cr
    \code{4}  = Dogfish-Lingcod, \cr
    \code{5}  = hook and line (H&L) rockfish, \cr
    \code{10} = all fisheries combined.}
  \item{saveinfo}{logical: if \code{TRUE}, save various data objects created within the function
    to a list object called \code{PBSfish}; setting to \code{FALSE} may speed reconstruction.}
  \item{wmf}{logical: if \code{TRUE} send the figures to \code{.wmf} files.}
  \item{uid, pwd}{user ID and password for Oracle DB account authentication.}
  \item{reconstruct}{logical: if \code{TRUE} (default) complete the reconstruction as previously, \cr
	if \code{FALSE}, terminate the code once the modern catch array has been compiled and saved 
	(to a binary file \code{cat123mod.rda}, where \code{123} = species code). }
}
\details{
  For the time period 1918--1949, landings of other rockfish (ORF) and total rockfish (TRF) 
  are identical because catch agencies did not identify Pacific Ocean Perch (POP). 
  POP started showing up in catch records in 1950, but appear artificially low from 1950--1952. 
  Therefore, for the period 1918--1952 we predict ORF and POP using a linear regression of 
  ORF \emph{vs}. TRF landings from the 1953--1995 data (excluding an anomalous 1966 point):

  \deqn{O = 2^a T^b}{%
        O = 2^a T^b}

  where \eqn{a} = -0.8063372, \eqn{b} = 1.006260, \eqn{O} = ORF, \eqn{T} = TRF, and POP = TRF - ORF.
  
  The ratios calculated by \code{buildCatch} (\eqn{\alpha} proportion of RRF
  caught in a PMFC area for each fishery, \eqn{\beta} proportion of RRF caught in a 
  PMFC area for H&L fisheries, \eqn{\gamma} ratio of RRF to a larger group 
  like ORF or TRF, \eqn{\delta} discard rate of RRF per other  catch category 
  from observer logs, \eqn{\lambda} proportion of early ORF/TRF catch by general gear type)
  are applied to the historical data to estimate historical landings of the RRF
  species. For POP the historical trawl landings are known back to 1956 
  and so these are used instead of deriving estimates.

  The eight modern catch databases (detailed above) summarize landings as metric tonnes
  in a 5-dimensional array where \eqn{i} = calendar year, \eqn{j} = 
  PMFC major area code, \eqn{k} = fishery ID, \eqn{l} = species or catch group, 
  and \eqn{m} = the database. Then for each element \eqn{(i,j,k,l)}, the maximum 
  landing across database \eqn{m} is taken. This methodology is utilized due to the 
  non-contiguous nature of the data sources. This also means that compiling 
  individual catch records across the database sources, as performed for the trawl 
  fishery by \code{getCatch}, is too complicated for the hook and line fisheries. 
  It is possible if definite cut points can be identified for the various time series
  (which we no longer do for the reconstruction).
}
\value{
  The final reconstructed catch array is invisibly returned. Additionally, if 
  \code{saveinfo = TRUE}, various data objects internal to the function are saved to 
  a global list object called \code{PBSfish}. Also, various data objects, including the 
  eight database catch files consolidated and massaged, are saved as system 
  binary files \code{.rda}. The final reconstructed catch array is also flattened to a 
  comma-delimited text file \code{Catch-History-123.csv}, where \code{123} 
  represents the species code.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\note{
  The function only has one argument for an Oracle database user ID and one for 
  its password. If the user's authentication credentials are different for 
  \code{PacHarv3} and \code{GFFOS}, specify \code{uid} and \code{pwd} as two-
  element vectors. The SQL servers assume a trusted relationship with the user's 
  DFO logon identity.
}
\seealso{
  \code{\link[PBStools]{getCatch}}, \code{\link[PBStools]{getData}}, \code{\link[PBSdata]{orfhistory}} \cr
  Available SQL queries: \code{\link[PBStools]{SQLcode}}
}
\keyword{manip}
\keyword{database}
\keyword{datagen}

