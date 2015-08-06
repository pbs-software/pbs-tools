\name{buildCatch}
\alias{buildCatch}
\title{
  Commercial Catch Reconstruction for BC Rockfish
}
\description{
  Build a catch reconstruction for one BC rockfish species (\code{RRF}) that uses: \cr
  (i) reliable landings of \code{RRF} from modern databases, \cr
  (ii) estimated landings of \code{RRF} using ratios of \code{RRF} to a reference 
   species catch (\code{TRF}, \code{ORF}, or \code{POP}), \cr
  (iii) estimated discards of \code{RRF} using discard ratios from observer logs.
}
\usage{
buildCatch(dbdat, strSpp="396", orfSpp="TRF", major=c(1,3:9), fidout=c(1:5,10),
   refyrs=1997:2005, refarea=NULL, refgear=NULL, useYR1=NULL, useBG=FALSE,
   useSM=FALSE, useGFM=TRUE, useLS=TRUE, useFF=TRUE, strat.gamma=FALSE,
   strat.delta=FALSE, depbin=100, sensitivity=NULL, reconstruct=TRUE, diagnostics=FALSE,
   ascii.tables=TRUE, saveinfo=TRUE, sql=FALSE, only.sql=FALSE, sql.force=FALSE,
   spath=.getSpath(), dpath=getwd(), eps=FALSE, png=FALSE, wmf=FALSE,
   uid=Sys.info()["user"], pwd=uid, ioenv=.GlobalEnv, ...)
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
  \item{strSpp}{character string specifying the Hart species code for the rockfish to be reconstructed (\dQuote{RRF}).}
  \item{orfSpp}{field name of the denominator in the ratio of RRF 
    to other rockfish (usually \dQuote{ORF} but can be \dQuote{TRF} if total rockfish is more appropriate).}
  \item{major}{major PMFC area codes in which catch is reconstructed (usually \code{c(1,3:9)}).}
  \item{fidout}{fishery IDs for which an annual series barplot stacked by PMFC area is produced: \cr
    \code{1}  = groundfish trawl, \cr
    \code{2}  = Halibut longline, \cr
    \code{3}  = Sablefish trap, \cr
    \code{4}  = Dogfish-Lingcod, \cr
    \code{5}  = hook and line (H&L) rockfish, \cr
    \code{10} = all fisheries combined.}
  \item{refyrs}{reference years to use for ratio calculations.}
  \item{refarea}{list (by fishery) of file names of files containing reference
    areas to use when calculating RRF/ORF(TRF) 
    (e.g., \code{list(`1`="A418trawl.csv")}).}
  \item{refgear}{list (by fishery) of reference gear types to use for ratio
    calculations (e.g., \code{list(`1`=1)}).}
  \item{useYR1}{if specified, use this year to start accepting landings reported in the databases 
    instead of reconstructing landings from reference ratios of RRF to TRF or ORF.}
  \item{useBG}{logical: if \code{TRUE}, then sample from the binomial-gamma to estimate ratios RRF/ORF or RRF/TRF.}
  \item{useSM}{logical: if \code{TRUE}, use catch data from seamounts.}
  \item{useGFM}{logical: if \code{TRUE}, use the latest official GF_MERGED_CATCH table (compiled by Norm and Kate).}
  \item{useLS}{logical: if \code{TRUE}, use ORF catch from Langara Spit in \code{gamma} calculation.}
  \item{useFF}{logical: if \code{TRUE}, use ORF catch from the foreign (not CA/US) fleet.}
  \item{strat.gamma}{logical: if \code{TRUE}, stratify the RRF numerator and ORF denominator by depth zone and weight by frequency of RRF landed > 0.}
  \item{strat.delta}{logical: if \code{TRUE}, stratify the discard numerator and denominator by depth zone and weight by frequency of RRF discards > 0.}
  \item{depbin}{integer: depth (m) bin to use for \code{strat.gamma} and \code{strat.delta}.}
  \item{sensitivity}{character: sensitivity name for tweaking decisions}
  \item{reconstruct}{logical: if \code{TRUE} (default) complete the 
    reconstruction as previously, \cr  if \code{FALSE}, terminate the code once
    the modern catch array has been compiled and saved (to a binary file 
    \code{cat123mod.rda}, where \code{123} = species code). }
  \item{diagnostics}{logical: if \code{TRUE} (default) create automatically-
    numbered diagnostic images files (prefaced with \code{pD000}, where
    \code{000} is the automatic number) to a subdirectory called \code{CRdiag}.
    Subsequent calls to \code{buildCatch} remove all \code{pD...} files created
    by previous builds.}
  \item{ascii.tables}{logical: if \code{TRUE}, create ASCII ouput tables and dump them into the subdirectory called `tables'.}
  \item{saveinfo}{logical: if \code{TRUE}, save various data objects created within the function
    to a list object called \code{PBStool}; setting to \code{FALSE} may speed reconstruction.}
  \item{sql}{logical: if \code{TRUE} query the databases, otherwise load 
    catch records from binary files saved from a previous query run (saves time;
    also see \code{dpath}).}
  \item{only.sql}{logical: if \code{TRUE}, then only execute the queries to
    download catch data from remote databases.}
  \item{sql.force}{logical: if \code{TRUE}, force the SQL query even if the data file (.rda) exists.}
  \item{spath}{character: path to SQL code files -- defaults to the \code{sql} 
    directory under \cr \code{system.file(package="PBStools")}.}
  \item{dpath}{character: path to database (\code{.rda} files) already queried;
    useful when user wants to build alternative catch histories (in another 
    directory) using data already queried.}
  \item{eps}{logical: if \code{TRUE} send the figures to \code{.eps} files.}
  \item{png}{logical: if \code{TRUE} send the figures to \code{.png} files.}
  \item{wmf}{logical: if \code{TRUE} send the figures to \code{.wmf} files.}
  \item{uid, pwd}{user ID and password for Oracle DB account authentication.}
  \item{ioenv}{input/output environment for function input data and output results.}
  \item{...}{additional \emph{ad hoc} arguments to deal with PJS issues.}
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
  a global list object called \code{PBStool}, and if \code{diagnostics = TRUE}, numerous
  plots of the various intermediary data objects are stored as image files in a 
  subdirectory called \code{CRdiag}. Also, various data objects, including the 
  eight database catch files consolidated and massaged, are saved as system 
  binary files \code{.rda}. The final reconstructed catch array is also flattened to a 
  comma-delimited text file \code{Catch-History-123.csv}, where \code{123} 
  represents the species code.
}
\references{
  Haigh, R., and Yamanaka, K.L. 2011. Catch history reconstruction for rockfish
  (\emph{Sebastes} spp.) caught in British Columbia coastal waters. 
  \emph{Canadian Technical Report of Fisheries and Aquatic Sciences}
  \bold{2943}: viii + 124p.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\note{
  The function only has one argument for an Oracle database user ID and one for 
  its password. If the user's authentication credentials are different for different
  Oracle databases, specify \code{uid} and \code{pwd} as \emph{n}-element vectors. 
  
  The function now queries the \code{GFFOS} database hosted on the SQL Server 
  \code{SVBCPBSGFIIS} rather than the version hosted on the Oracale server \code{GFSH} previously.
  
  The SQL server \code{SVBCPBSGFIIS} does not require a password if the server recognises 
  a user's DFO login credentials.
}
\section{Yellowtail Rockfish 2014}{
  Rob Kronlund and Paul Starr met on Aug 12, 2014 with Brian Mose who made several important
  points regarding reconstruction results from \code{buildCatch}:

  A) Midwater trawling for YTR did not really begin in the domestic fishery until the mid-1980s.
  This is evident in the rapid rise of YTR landings from the mid-1980s of the reconstruction.
  It is not known if the foreign fleet which fished rockfish from the early 1960s to 1977 used 
  mid-water gear, but it is thought to be unlikely.

  B) The foreign fleet fished the outside waters off Vancouver Island and Queen Charlotte Islands
  and did not enter very far into Queen Charlotte Sound.  They also tended to fish deeper than
  the preferred depths for YTR, apparently preferring POP and allied species.

  Brian made several useful suggestions to improve the historical estimation method:

  A) Only include bottom trawl catches of YTR in the ratio estimation process, discarding all 
  midwater trawl catches of YTR as well as other rockfish species;

  B) Make the calculations based on the outside bottom trawl catches of rockfish, ignoring catches
  in the three Queen Charlotte Sound gullies (Goose, Mitchell and Moresby) and in Dixon Entrance.

  On the basis of these recommendations, Brian thought that very little YTR catches should come from
  the west coast of the Charlottes or from 5C which covers most the lower half of Hecate Strait.  
  He thought that most of the YTR catches should come from the outer parts of 3D, 5A and 5B.

  Paul's calculations:\cr
  Proportion of Yellowtail Rockfish in the catch, based on the catch composition in each PMFC area
  for the years 1997-2005 calculated using (a) outside localities only and (b) all localities.

  \tabular{crrrr}{
  PMFC \tab TRF Outside \tab TRF All \tab ORF Outside \tab ORF All\cr
  3C  \tab  0.260  \tab  0.260  \tab  0.314  \tab  0.314\cr
  3D  \tab  0.229  \tab  0.229  \tab  0.253  \tab  0.253\cr
  5A  \tab  0.039  \tab  0.160  \tab  0.047  \tab  0.220\cr
  5B  \tab  0.001  \tab  0.102  \tab  0.003  \tab  0.235\cr
  5C  \tab   ---   \tab  0.124  \tab   ---   \tab  0.186\cr
  5D  \tab  0.167  \tab  0.240  \tab  0.445  \tab  0.461\cr
  5E  \tab  0.003  \tab  0.003  \tab  0.005  \tab  0.005
  }

  These trawl \code{gamma} values are currently hardwired in the code and
  can be activated by the following arguments: 

  \tabular{rl}{
  \code{pjs}     \tab logical: if \code{TRUE},\cr
                 \tab replace the first column of the matrix \code{gamma} with one of the columns above.\cr
  \code{outside} \tab logical: if \code{TRUE}, \cr
                 \tab use the column above marked \sQuote{Outside}, otherwise use \sQuote{All}; \cr
                 \tab the denominator TRF or ORF is determined by the argument \code{orfSpp}.
  }
}
\section{Yelloweye Rockfish 2015}{
  On May 14th, 2015, the YYR working group held a workshop on catch 
  reconstruction with members from the five fishery sectors.
  
  Changes to the catch reconstruction from this meeting and subsequent follow-ups include:
  \itemize{
    \item Merging of six DFO database sources into one catch table called \code{GF_MERGED_CATCH};
    \item Development of a depth-stratified \code{gamma} (RRF landed/ORF landed) and delta (RRF discarded/TAR landed);
    \item Optional exclusion of ORF landings by the foreign and Langara Spit experimental fisheries;
    \item Optional exclusion of seamount catch records.
  }
}
\seealso{
  \code{\link[PBStools]{getCatch}}, \code{\link[PBStools]{getData}}, 
  \code{\link[PBSdata]{orfhistory}}, \code{\link[PBStools]{surveyCatch}} \cr
  Available SQL queries: \code{\link[PBStools]{SQLcode}}
}
\keyword{manip}
\keyword{database}
\keyword{datagen}

