\name{requestAges}
\alias{requestAges}
\title{
  Request Otoliths for Ageing
}
\description{
  Query the DFO database \code{GFBioSQL} for available otoliths to age.
  Choose random otoliths based on commercial or survey catch weights of the 
  specified species.
}
\usage{
requestAges(strSpp, nage=500, year=2016, 
   areas=list(major=3:9, minor=NULL), ttype=c(1,4), gear=1,
   sex=1:2, nfld = "nallo", run.sql=TRUE, only.sql=FALSE, bySID=FALSE,
   spath=.getSpath(), uid=Sys.info()["user"], pwd=uid, ...)
}
\arguments{
  \item{strSpp}{string HART code of a species with sampled otoliths.}
  \item{nage}{number of otoliths allocated for this species given the years and areas specified.}
  \item{year}{numeric vector of years.}
  \item{areas}{list of area vectors: those specified first in the list take precedence over those
     specified later, if overlap occurs.}
  \item{ttype}{numeric vector specifying trip type: \cr 
    1=non-observed commercial, 2=research, 3=charter, 4=observed commercial. \cr 
    Note that you can either choose purely 
    commercial samples (\code{ttype=c(1,4)}) OR survey samples (\code{ttype=c(2,3)}), not a mixture.}
  \item{gear}{numeric gear code used by \code{GFBioSQL}:\cr
    \preformatted{
    0 = unknown
    1 = bottom trawl     8 = unknown trawl
    2 = trap             9 = seine
    3 = gillnet         10 = jig
    4 = handline        11 = shrimp trawl
    5 = longline        12 = recreational rod & reel
    6 = midwater trawl  13 = prawn trap
    7 = troll           14 = tucker trawl}}
  \item{sex}{numeric sex code where 1=males, 2=females, c(1,2)=males + females.}
  \item{nfld}{string specifying field with number of otoliths to sample: \cr
    \code{"nfree"...}{number of otoliths available (not aged)}; \cr
    \code{"ncalc"...}{number of otoliths calculated after weighting by species catch,} \cr
    \code{..........}{less the number of otoliths already aged by break & burn;} \cr
    \code{"nallo"...}{default: essentially \code{ncalc} scaled to the user-specified \code{nage}.} \cr
    Both \code{ncalc} and \code{nallo} are constrained by \code{nfree}. }
  \item{run.sql}{logical: if \code{TRUE} query the databases for all otolith and catch information,
    otherwise grab the information from stored binaries (e.g., \code{Sdat396.rda} for POP
    otolith data, and \code{Ccat396.rda} for POP catch from \code{PacHarvest} and \code{GFFOS}.}
  \item{only.sql}{logical: if \code{TRUE} only query the databases for otolith and catch information;
    do not process the request for otoliths.}
  \item{bySID}{logical: if \code{TRUE} group the chosen otoliths by sample ID (SID) for presentation 
    in the CSV file (assumes that one tray houses one sample); otherwise assume that many small samples occur in one tray.}
  \item{spath}{string specifying system path to the SQL queries; default points to the SQL directory in \pkg{PBStools}.}
  \item{uid, pwd}{user ID and password for Oracle DB account authentication (no longer need as 
    queries are coded in SQL with embedded Oracle queries that use the SQL server's  credentials).}
  \item{...}{allows users to request specific trip IDs (either \code{TID_gfb} or \code{TID_fos}); \cr
    also user can specify a minimum number of free otoliths per sample (e.g., \code{nfree=32}) when selecting.}
}
\value{
  Invisibly returns the otolith data with supplementary quarterly commercial / survey catch.
  Also, writes this otolith data to binary \code{.rda} and ASCII \code{.csv} files.
  
  An ASCII file called \code{oto123-C(YYYY)-areas(0+0+0)--sex(1+2)-N999.csv} 
  (where \code{123} = species code, \code{C} = Commercial or \code{S} = Survey, \code{YYYY} = year, \code{0} = PMFC area code, 
  and \code{999} = no. of otoliths) writes out the random samples for each trip 
  (if commercial samples) / fishing event (if survey samples) in a format that simulates an otolith tray.
  
  The global list object \code{PBStool} contains: 
  \tabular{lll}{
    \code{module}   \tab \code{string}     \tab PBStools source code module that contains \code{requestAges}; \cr
    \code{call}     \tab \code{call}       \tab details of the last last call to \code{requestAges}; \cr
    \code{Sdat}     \tab \code{data.frame} \tab all sample data for \code{strSpp}; \cr
    \code{catch}    \tab \code{data.frame} \tab catches for \code{strSpp} (either commercial or survey); \cr
    \code{C}        \tab \code{vector}     \tab \code{strSpp} catch per quarter (commercial) or stratum (surveys); \cr
    \code{samp}     \tab \code{data.frame} \tab sample data relevant to the user's specification; \cr
    \code{describe} \tab \code{string}     \tab used to label output files; \cr
    \code{Opool}    \tab \code{list}       \tab pool of available otoliths for samples in \code{samp}; \cr
    \code{Nsamp}    \tab \code{vector}     \tab number of ages to sample from \code{Opool}; \cr
    \code{Osamp}    \tab \code{list}       \tab Otoliths chosen randomly from \code{Opool}.
  }
  The otoliths in \code{Opool} are identified by \code{SPECIMEN_SERIAL_NUMBER} and are taken from the 
  \code{GFBioSQL} table \code{B05_SPECIMEN}:
  \preformatted{
  SELECT
    B5.SAMPLE_ID AS SID, 
    B5.SPECIMEN_SERIAL_NUMBER AS SN
  FROM B05_SPECIMEN B5 
  WHERE 
    B5.SAMPLE_ID IN (@SIDS) AND 
    B5.AGEING_METHOD_CODE IS NULL AND 
    B5.SPECIMEN_SEX_CODE IN (@sex)
  }
}
\author{
  Rowan Haigh and Brian Krishka \cr
  Fisheries and Oceans Canada, Pacific Biological Station, Nanaimo BC
}
\seealso{
  \code{\link[PBStools]{calcVB}}, \code{\link[PBStools]{estOgive}}, 
  \code{\link[PBStools]{plotProp}}, \code{\link[PBStools]{weightBio}} \cr
  Available SQL queries: \code{\link[PBStools]{SQLcode}}
}
\keyword{manip}
\keyword{IO}

