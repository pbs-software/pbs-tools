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
buildCatch(
   dbdat,
   strSpp="396",
   orfSpp="TRF",
   major=c(1,3:9),
   fidout=c(1:5,10),
   useYR1=c(1996,2000,2007,2007,1986),
   useGFM=TRUE,
   useCA=TRUE,
   useUS=TRUE,
   useFF=TRUE,
   useSM=FALSE,
   useLS=TRUE,
   useAI=FALSE,
   useGM=FALSE,
   useBG=FALSE,
   refyrs = list(1997:2005, 1997:2005, 1997:2005, 1997:2005, 1997:2005),
   refarea=NULL,
   refgear=NULL,
   strat.gamma=FALSE,
   strat.delta=FALSE,
   depbin=100,
   disyrs = list(1954:1995, 1986:2005, 1986:2005, 1986:2005, 1986:2005), 
   sensitivity=NULL,
   reconstruct=TRUE,
   run.name=NULL,
   ascii.tables=TRUE,
   diagnostics=FALSE,
   saveinfo=TRUE,
   sql=FALSE,
   sql.only=FALSE,
   sql.force=FALSE,
   spath=.getSpath(),
   dpath=getwd(),
   eps=FALSE, png=FALSE, wmf=FALSE,
   uid=Sys.info()["user"], pwd=uid,
   ioenv=.GlobalEnv,
   hadley=FALSE,
   ...)
}
\arguments{
  \item{dbdat}{\code{list} -- list of data frames containing landing records from various DFO databases: \cr
    (1) \code{ph3dat} = \code{PacHarv3} (all fisheries) actually called \code{HARVEST_V2_0}
      on the \code{ORAPROD} Oracle server, \cr\cr
    If \code{useGM=TRUE}, then:\cr\cr
    (2) \code{gfmdat} = table \code{GF_MERGED_CATCH} from the \code{GFFOS} database on the \code{DFBCV9TWVASP001} SQL server,
    which contains merged catch data from six databases:\cr
    \code{GFBioSQL}, \code{GFCatch}, \code{PacHarvest}, \code{PacharvHL}, \code{PacHarvSable}, \code{GFFOS}.\cr\cr
    If \code{useGM=FALSE}, then (all on the \code{DFBCV9TWVASP001} SQL server):\cr\cr
    (2) \code{gfcdat} = \code{GFCatch} (trawl, trap, h&l), \cr
    (3) \code{phtdat} = \code{PacHarvest} (groundfish trawl), \cr
    (4) \code{phhdat} = \code{PacHarvHL} (Halibut bycatch from DMP validated landings), \cr
    (5) \code{phsdat} = \code{PacHarvSable} (Sablefish trap), \cr
    (6) \code{phhdat} = \code{PacHarvHL} (validated landings Sched II & ZN), \cr
    (7) \code{phfdat} = \code{PacHarvHL} (fisherlog records Sched II & ZN), \cr
    (8) \code{fosdat} = \code{GFFOS} (all fisheries).}
  \item{strSpp}{\code{character} -- string specifying the Hart species code for the rockfish to be reconstructed (\code{RRF}).}
  \item{orfSpp}{\code{character} -- field name of the denominator in the ratio of \code{RRF}
    to other rockfish (usually \code{ORF} but can be \code{TRF} if total rockfish is more appropriate).}
  \item{major}{\code{numeric} -- major PMFC area codes to see in plots; catch is always reconstructed for majors \code{c(1,3:9)}.}
  \item{fidout}{\code{numeric} -- fishery IDs for which an annual series barplot stacked by PMFC area is produced: \cr
    \code{1}  = groundfish trawl, \cr
    \code{2}  = Halibut longline, \cr
    \code{3}  = Sablefish trap, \cr
    \code{4}  = Dogfish-Lingcod, \cr
    \code{5}  = hook and line (H&L) rockfish, \cr
    \code{10} = all fisheries combined.}
  \item{useYR1}{\code{numeric} -- if specified, use these years (for each FID) to start accepting landings reported in the databases 
    instead of reconstructing landings from \code{gamma} (reference ratios of \code{RRF} to \code{ORF|TRF}).}
  \item{useGFM}{\code{logical} -- if \code{TRUE}, use the latest official \code{GF_MERGED_CATCH} table (compiled by Norm Olsen).}
  \item{useCA}{\code{logical} -- if \code{TRUE}, use \code{ORF|TRF} catch from the Canadian (CA) fleet.}
  \item{useUS}{\code{logical} -- if \code{TRUE}, use \code{ORF|TRF} catch from the American (US) fleet.}
  \item{useFF}{\code{logical} -- if \code{TRUE}, use \code{ORF|TRF} catch from the foreign (UR, JP, PO, etc.) fleet.}
  \item{useSM}{\code{logical} -- if \code{TRUE}, use catch data from seamounts.}
  \item{useLS}{\code{logical} -- if \code{TRUE}, use \code{ORF|TRF} catch from Langara Spit in \code{gamma} calculation.}
  \item{useAI}{\code{logical} -- if \code{TRUE}, assign catch from Anthony Island (PMFC 5E south of 52.33333 degrees N) to PMFC area 5C,
    primarily for POP and YMR. Only works for data from GFFOS merged catch in \code{gfmdat}.}
  \item{useGM}{\code{logical} -- if \code{TRUE}, use geometric mean to average annual ratios for \code{gamma} and 
    \code{delta} only. Not used if \code{strat.gamma}, \code{strat.delta}, or \code{useBG} are \code{TRUE}.}
  \item{useBG}{\code{logical} -- if \code{TRUE}, sample from the binomial-gamma distribution to estimate ratios \code{RRF/ORF} or code{RRF/TRF}.}
  \item{refyrs}{\code{numeric} -- reference years to use when calculating \code{gamma} (e.g., \code{RRF/ORF}).}
  \item{refarea}{\code{character} -- list (by fishery) of names of files containing reference
    areas to use when calculating \code{gamma}, \code{RRF/ORF|TRF} (e.g., \code{list(`1`="refA439.trawl.csv")}).}
  \item{refgear}{\code{numeric} -- list (by fishery) of reference gear types to use for calculating \code{gamma}
    (e.g., \code{list(`1`=1)}), where gears 1=bottom trawl, 2=trap, 3=midwater trawl, 4=h&l, 5=longline, 8=h&l/longline/trap.}
  \item{strat.gamma}{\code{logical} -- if \code{TRUE}, stratify the \code{RRF} numerator and
    \code{ORF|TRF} denominator by depth zone and weight by frequency of \code{RRF} landed > 0.}
  \item{strat.delta}{\code{logical} --if \code{TRUE}, stratify the discard numerator and 
    denominator by depth zone and weight by frequency of \code{RRF} discards > 0.}
  \item{depbin}{\code{integer} -- depth (m) bin to use for \code{strat.gamma} and \code{strat.delta}.}
  \item{disyrs}{\code{numeric} -- discard years, one set for each fishery (trawl, halibut, sable, dogling, hlrock).}
  \item{sensitivity}{\code{character} -- sensitivity name for tweaking decisions.}
  \item{reconstruct}{\code{logical} -- if \code{TRUE} (default), complete the reconstruction as previously,\cr
    if \code{FALSE}, terminate the code once the modern catch array has been compiled and saved
    (to a binary file \code{cat123mod.rda}, where \code{123} = species code).}
  \item{run.name}{\code{character} -- run name to keep track of various run configurations
    with \code{gamma}, \code{delta}, \code{refarea}, and \code{refgear}.}
  \item{ascii.tables}{\code{logical} -- if \code{TRUE}, create ASCII ouput tables and dump them into the subdirectory \code{./run.name/tables}.}
  \item{diagnostics}{\code{logical} -- if \code{TRUE} (default), create automatically-numbered
    diagnostic images files (prefaced with \code{pD000}, where \code{000} is the automatic number)
    to a subdirectory \code{./run.name/diags}.
    Subsequent calls to \code{buildCatch} remove all \code{pD000} files created by previous builds.}
  \item{saveinfo}{\code{logical} -- if \code{TRUE}, save various data objects created within the function
    to a list object called \code{PBStool} in the temporary environment \code{.PBStoolEnv};
    setting to \code{FALSE} may speed reconstruction.}
  \item{sql}{\code{logical} -- if \code{TRUE} query the databases, otherwise load catch records
    from binary files saved from a previous query run (saves time; also see \code{dpath}).}
  \item{sql.only}{\code{logical} -- if \code{TRUE}, only execute the queries to download catch data from remote databases.}
  \item{sql.force}{\code{logical} -- if \code{TRUE}, force the SQL query even if the data file \code{.rda} exists.}
  \item{spath}{\code{character} -- path to SQL code files; defaults to the \code{sql} 
    directory under \cr \code{system.file(package="PBStools")}.}
  \item{dpath}{\code{character} -- path to database (\code{.rda} files) already queried;
    useful when user wants to build alternative catch histories (in another directory) using data already queried.}
  \item{eps}{\code{logical} -- if \code{TRUE}, send the figures to \code{.eps} files.}
  \item{png}{\code{logical} -- if \code{TRUE}, send the figures to \code{.png} files.}
  \item{wmf}{\code{logical} -- if \code{TRUE}, send the figures to \code{.wmf} files.}
  \item{uid, pwd}{\code{character} -- user ID and password for Oracle DB account authentication (only used for PacHarv3 currently).}
  \item{ioenv}{\code{environment} -- input/output environment for function input data and output results.}
  \item{hadley}{\code{logical} -- if \code{TRUE}, use Hadley Wickham's wizardry in \code{'crossTab'}.}
  \item{...}{additional \emph{ad hoc} arguments to deal with industry issues.}
}
\details{
  For the time period 1918--1949, landings of other rockfish (\code{ORF}) and total rockfish (\code{TRF}) 
  are identical because catch agencies did not identify Pacific Ocean Perch (\code{POP}). 
  \code{POP} started showing up in catch records in 1950, but appear artificially low from 1950--1952. 
  Therefore, for the period 1918--1952 we predict \code{ORF} and \code{POP} using a linear regression of 
  \code{ORF} \emph{vs}. \code{TRF} landings from the 1953--1995 data (excluding an anomalous 1966 point):

  \deqn{O = 2^a T^b}{%
        O = 2^a T^b}

  where \eqn{a} = -0.8063372, \eqn{b} = 1.006260, \eqn{O} = \code{ORF}, \eqn{T} = \code{TRF}, and \code{POP = TRF - ORF}.
  
  The ratios calculated by \code{buildCatch}:\cr
  \eqn{\alpha} or \code{alpha} = proportion of \code{RRF} caught in a PMFC area for each fishery,\cr
  \eqn{\beta}  or \code{beta} = proportion of \code{RRF} caught in a PMFC area for H&L fisheries,\cr
  \eqn{\gamma} or \code{gamma} = ratio of \code{RRF} to a larger group (\code{TRF|ORF|POP}) from observer logs,\cr
  \eqn{\delta} or \code{delta} = ratio of discarded \code{RRF} to landings of a fishery-dependent spcecies|group from observer logs,\cr
  \eqn{\lambda} or \code{lambda} = proportion of early \code{ORF|TRF} catch by general gear type,\cr
  are applied to the historical data to estimate historical landings of the \code{RRF}
  species. For \code{POP} the historical trawl landings are known back to 1956 
  and so these are used instead of deriving estimates.

  Landings (metric tonnes) from the modern catch databases (detailed above) are used 
  to populate a 5-dimensional array where\cr
  \eqn{i} = calendar year,\cr
  \eqn{j} = PMFC major area code,\cr
  \eqn{k} = fishery ID,\cr
  \eqn{l} = species or catch group, and\cr
  \eqn{m} = the database name.\cr
  Then for each element \eqn{(i,j,k,l)}, the maximum landing across database \eqn{m} is taken.
  Some data sources overlap while others provide the only recorded landing for particular years.
  This methodology is utilized due to the non-contiguous nature of the data sources. 
}
\value{
  The final reconstructed catch array is invisibly returned.
  Additionally, if \code{saveinfo=TRUE}, various data objects internal to the
  function are saved to a global list object called \code{"PBStool"}.
  If \code{diagnostics=TRUE}, numerous plots of the various intermediary data 
  objects are stored as image files in a subdirectory called \code{"diags"}.
  If \code{ascii.tables=TRUE}, numerous \code{.csv} files are saved containing 
  individual-fishery landings, ratio tables, and the final catch reconstruction
  in a subdirectory called \code{"tables"}.
  Various data objects, including the database catch files, consolidated and 
  massaged, are saved as system binary files (\code{".rda"}). 
  The final reconstructed catch array is also flattened to a comma-delimited 
  text file \code{"Catch-History-123.csv"}, where \code{123} represents the RRF code.
}
\section{Summary of changes since 2011}{
  In previous stock assessments for POP (Edwards et al. 2014a,b), the authors documented two departures from the catch reconstruction algorithm introduced by Haigh & Yamanaka (2011). 
  The first dropped the use of data from the sales slip database PacHarv3 because catches were sometimes reported by large statistical areas that cannot be clearly mapped to PMFC areas. 
  In theory, \code{PacHarv3} should report the same catch as that in the \code{GFCatch} database (Rutherford 1999), but area inconsistencies cause catch inflation when certain large statistical areas cover multiple PMFC areas. 
  Therefore, only the \code{GFCatch} database for the trawl and trap records from 1954 to 1995 are used, rather than trying to mesh \code{GFCatch} and \code{PacHarv3}. 
  The point is somewhat moot as assessments since 2015 use a merged catch data table, \code{GF\_MERGED\_CATCH}, which is available in DFO's \code{GFFOS} database. 
  Data for the H&L fisheries from \code{PacHarv3} are still used as these do not appear in other databases. 
  The second departure was the inclusion of an additional data source for Japanese rockfish catch reported in Ketchen (1980).

  In 2014, the Yellowtail Rockfish (YTR) assessment (Starr et al. 2014) selected offshore areas that reflected the activity of the foreign fleets' impact on this species to calculate \eqn{\gamma} (\code{RRF/ORF}) and \eqn{\delta} ratios (\code{RRF/TAR}). 
  This option has not been used in other rockfish reconstructions.

  In the 2015 Yelloweye Rockfish (YYR) assessment (Yamanaka et al. 2018) the concept of depth-stratified \eqn{\gamma} and \eqn{\delta} ratios was introduced; however, this functionality has not been used since. 
  Also in the YYR assessment, rockfish catch from seamounts was removed, as well as an option to exclude rockfish catch from the foreign fleet and the experimental Langara Spit POP fishery.

  In the 2018 Redstripe Rockfish (RSR) assessment (Starr and Haigh, in press), gamma and delta ratios from reference years were calculated by taking the geometric mean across years instead of the previously used arithmetic mean. 
  This reduces the influence of single anomalously large annual ratios. 
  Also new in 2018 was the ability to estimate RRF (using \eqn{\gamma}) for landings later than 1996, should the user have reason to replace observed landings with estimated ones.
  For example, Widow Rockfish (WWR) observed landings by fishery were used starting in 1996 (trawl), 2000 (halibut), 2007 (sablefish), 2007 (dogfish/lingcod), and 1986 (h&l rockfish); prior to these years, landings were estimated using \eqn{\gamma}. 
  The user can also specify years by fishery when discard ratios (\eqn{\delta}) are to be applied; for WWR these years were 1954:1995 (trawl), 1986:2005 (halibut), 1986:2005 (sablefish), 1986:2005 (dogfish/lingcod), and 1986:2005 (h&l rockfish).
  As previously, years before the discard period assume no discarding, and years after the discard period assume that discards have been reported in the databases.

  In 2018, a reconstruction for Rougheye Rockfish (RER) changed the calculation of \eqn{\gamma} and \eqn{\delta} from including reference-year zero-value ratios to excluding them.
  The reason for this change was that scarcity of data, especially in observer records for the non-trawl fisheries, was more likely to be the reason for zero values that the absence of the reconstructed rockfish (RRF).
  The effect of excluding zeroes will increase both ratios using the non-stratified method.
  Depth-stratification calculations of \eqn{\gamma} and \eqn{\delta} have not been re-examined or altered in the current algorithm.

  In 2019, the WWR sock assessment found substantial amounts of WWR in GFBioSQL reported as foreign catch (specifically 349t in years 1982, 1987-1991, and 2000), which came from midwater gear off WCVI. 
  The reconstruction algorithm now assigns GFBio foreign catch to four of the five fisheries based on gear type -- bottom and midwater trawl gear assigned to trawl, longline gear assigned to halibut, trap and line-trap mix gear assigned to sablefish, h&l gear assigned to h&l rockfish -- assuming that the reconstruction uses foreign catch, which is optional. 
  These foreign catches occurred well after the foreign fleet activity between 1965 and the implementation of an exclusive economic zone in 1977.
}
\section{Additional information -- YTR 2014}{
  Rob Kronlund and Paul Starr met on Aug 12, 2014 with Brian Mose who made several important
  points regarding reconstruction results for Yellowtail Rockfish (YTR) from \code{buildCatch}:

  A) Midwater trawling for YTR did not really begin in the domestic fishery until the mid-1980s.
  This is evident in the rapid rise of YTR landings from the mid-1980s of the reconstruction.
  It is not known if the foreign fleet which fished rockfish from the early 1960s to 1977 used 
  mid-water gear, but it is thought to be unlikely.

  B) The foreign fleet fished the outside waters off Vancouver Island and Queen Charlotte Islands
  (now called Haida Gwaii)
  and did not enter very far into Queen Charlotte Sound. They also tended to fish deeper than
  the preferred depths for YTR, apparently preferring \code{POP} and allied species.

  Brian made several useful suggestions to improve the historical estimation method:

  A) Only include bottom trawl catches of YTR in the ratio estimation process, discarding all 
  midwater trawl catches of YTR as well as other rockfish species;

  B) Make the calculations based on the outside bottom trawl catches of rockfish, ignoring catches
  in the three Queen Charlotte Sound gullies (Goose, Mitchell and Moresby) and in Dixon Entrance.

  On the basis of these recommendations, Brian thought that very little YTR catches should come from
  the west coast of the Charlottes (WCHG) or from 5C which covers most the lower half of Hecate Strait.
  He thought that most of the YTR catches should come from the outer parts of 3D, 5A and 5B.

  Paul's calculations:\cr
  Proportion of YTR in the catch, based on the catch composition in each PMFC area
  for the years 1997-2005 calculated using (a) outside localities only and (b) all localities.

  \tabular{crrrr}{
  PMFC \tab \code{TRF} Outside \tab \code{TRF} All \tab \code{ORF} Outside \tab \code{ORF} All\cr
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
  \code{pjs}     \tab \code{logical} -- if \code{TRUE},\cr
                 \tab replace the first column of the matrix \code{gamma} with one of the columns above.\cr
  \code{outside} \tab \code{logical} -- if \code{TRUE}, \cr
                 \tab use the column above marked \sQuote{Outside}, otherwise use \sQuote{All}; \cr
                 \tab the denominator \code{TRF|ORF} is determined by the argument \code{orfSpp}.
  }
}
\section{Additional information -- YYR 2015}{
  On May 14th, 2015, the Yelloweye Rockfish (YYR) working group held a workshop on catch 
  reconstruction with members from the five fishery sectors.
  
  Changes to the catch reconstruction from this meeting and subsequent follow-ups include:
  \itemize{
    \item Merging of six DFO database sources into one catch table called \code{GF_MERGED_CATCH};
    \item Development of a depth-stratified \code{gamma} (\code{RRF} landed/\code{ORF} landed) and delta (\code{RRF} discarded/\code{TAR} landed);
    \item Optional exclusion of \code{ORF} landings by the foreign and Langara Spit experimental fisheries;
    \item Optional exclusion of seamount catch records.
  }
}
\section{Acronyms galore}{
  \tabular{rl}{
  \code{BC}   \tab British Columbia -- sixth province in Canada (joined July 20, 1871)\cr
  \code{CA}   \tab Canada -- referring to the nationality of the domestic fleet\cr
  \code{DFO}  \tab Fisheries and Oceans Canada -- a.k.a. Dept. Fisheries & Oceans\cr
  \code{DMP}  \tab Dockside Monitoring Program -- initiated for rockfish in 1995\cr
  \code{GF}   \tab Groundfish -- includes rockfish, flatfish, some pelagics, sharks, etc.\cr
  \code{FID}  \tab Fishery Identification -- 1=Trawl, 2=Halibut, 3=Sablefish, 4=Dogfish+Lingcod, 5=H&L Rockfish\cr
  \code{FOS}  \tab Fishery Operations System -- mega database housing all fish removals in BC\cr
  \code{H&L}  \tab Hook and Line -- usually referring to a fishery\cr
  \code{JP}   \tab Japan -- referring to a foreign fleet (1966-1977)\cr
  \code{ORF}  \tab Other Rockfish -- rockfish other than Pacific Ocean Perch\cr
  \code{PMFC} \tab Pacific Marine Fisheries Commission -- areas along BC coast used in stock assessment\cr
  \code{PO}   \tab Poland -- referring to a foreign fleet (1975-1976)\cr
  \code{POP}  \tab Pacific Ocean Perch (\emph{Sebastes alutus})\cr
  \code{QCS}  \tab Queen Charlotte Sound (PMFC 5ABC)\cr
  \code{RER}  \tab Rougheye Rockfish (\emph{Sebastes aleutianus})\cr
  \code{RRF}  \tab Reconstructed Rockfish -- rockfish selected for catch reconstruction\cr
  \code{RSR}  \tab Redstripe Rockfish (\emph{Sebastes proriger})\cr
  \code{SQL}  \tab Standardized Query Language -- used to extract records from relational databases\cr
  \code{TRF}  \tab Total Rockfish -- all species in \emph{Sebastes} and \emph{Sebastolobus}\cr
  \code{UR}   \tab Union of Soviet Socialist Republics (Russia) -- referring to a foreign fleet (1965-1972)\cr
  \code{US}   \tab United States of America -- referring to a foreign fleet (1930-1977)\cr
  \code{WCHG} \tab West Coast Haida Gwaii (PMFC 5E) -- formerly west coast Queen Charlotte Islands\cr
  \code{WCVI} \tab West Coast Vancouver Island (PMFC 3CD)\cr
  \code{YTR}  \tab Yellowtail Rockfish (\emph{Sebastes flavidus})\cr
  \code{YYR}  \tab Yelloweye Rockfish (\emph{Sebastes ruberrimus})
  }
}
\note{
  All catch databases are stored in SQL Server with the exception of \code{PacHarv3},
  which is stored in Oracle.
  Previously, \code{GFFOS} was only housed in Oracle, with a transition period where
  it was mirrored in SQL Server; it now occurs only in SQL Server on the server called
  \code{DFBCV9TWVASP001}.

  The function \code{buildCatch} has one argument for an Oracle database user ID and one for 
  its password. 
  If the user's authentication credentials are different for \emph{n} different Oracle databases, 
  specify \code{uid} and \code{pwd} as \emph{n}-element vectors.
  However, currently there is only one Oracle database used -- \code{PacHarv3}.

  When querying an Oracle DB from R, the user will likely need to run the 32-bit version of R,
  which should only need to be performed once per species to extract historical landings from \code{PacHarv3}.
  All queries to SQL Server can be run in the 64-bit version of R.

  The SQL server \code{DFBCV9TWVASP001} does not require a password if the server recognises 
  a user's DFO login credentials.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2019-10-02}
}
\references{
  Edwards, A.M., Haigh, R. and Starr, P.J. (2014a)
  \href{http://www.dfo-mpo.gc.ca/csas-sccs/publications/resdocs-docrech/2013/2013_092-eng.html}{Pacific Ocean Perch (\emph{Sebastes alutus}) stock assessment for the north and west coasts of Haida Gwaii, British Columbia}. 
  \emph{DFO Can. Sci. Advis. Sec. Res. Doc.} \bold{2013/092}: vi + 126 pp.

  Edwards, A.M., Haigh, R. and Starr, P.J. (2014b)
  \href{http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2013/2013_093-eng.html}{Pacific Ocean Perch (\emph{Sebastes alutus}) stock assessment for the west coast of Vancouver Island, British Columbia}.
  \emph{DFO Can. Sci. Advis. Sec. Res. Doc.} \bold{2013/093}: vi + 135 pp.

  Haigh, R. and Yamanaka, K.L. (2011)
  \href{http://www.dfo-mpo.gc.ca/Library/344242.pdf}{Catch history reconstruction for rockfish (\emph{Sebastes} spp.) caught in British Columbia coastal waters}.
  \emph{Canadian Technical Report of Fisheries and Aquatic Sciences} \bold{2943}: viii + 124 pp.

  Ketchen, K.S. (1980)
  \href{http://waves-vagues.dfo-mpo.gc.ca/Library/10079.pdf}{Reconstruction of Pacific Ocean Perch (\emph{Sebastes alutus}) stock history in Queen Charlotte sound}.
  Part I. Estimation of foreign catches, 1965--1976. 
  \emph{Can. Manuscr. Rep. Fish. Aquat. Sci.} \bold{1570}: iv + 46 pp.

  Rutherford, K.L. (1999)
  \href{http://www.dfo-mpo.gc.ca/Library/243214.pdf}{A brief history of GFCatch (1954--1995), the groundfish catch and effort database at the Pacific Biological Station}.
  \emph{Can. Tech. Rep. Fish. Aquat. Sci.} \bold{2299}: v + 66 pp.

  Starr, P.J., Kronlund, A.R., Olsen, N. and Rutherford, K. (2014, in revision)
  \href{http://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS/2015/2015_010-eng.html}{Yellowtail Rockfish (\emph{Sebastes flavidus}) stock assessment for the coast of British Columbia, Canada (working paper)}.
  \emph{DFO Can. Sci. Advis. Sec. Res. Doc.} \bold{2015/nnn}: x + 58 pp.

  Yamanaka, K.L., McAllister, M.M., Etienne, M.P., Edwards, A.M. and Haigh, R. (2018)
  \href{http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2018/2018_001-eng.html}{Assessment for the outside population of Yelloweye Rockfish (\emph{Sebastes ruberrimus}) for British Columbia, Canada in 2014}.
  \emph{DFO Can. Sci. Advis. Sec. Res. Doc.} \bold{2018/001}: ix + 150 pp.
}
\seealso{
In package \pkg{PBStools}:\cr
    \code{\link[PBStools]{calcPMR}},
    \code{\link[PBStools]{calcStockArea}},
    \code{\link[PBStools]{collectFigs}},
    \code{\link[PBStools]{crossTab}},
    \code{\link[PBStools]{getData}},
    \code{\link[PBStools]{getFile}},
    \code{\link[PBStools]{plotDiag}},
    \code{\link[PBStools]{plotRecon}},
    \code{\link[PBStools]{sampBG}},
    \code{\link[PBStools]{surveyCatch}},
    \code{\link[PBStools]{zapSeamounts}}, 
    \code{\link[PBStools]{SQLcode}}\cr\cr
In package \pkg{PBSdata}:\cr
    \code{\link[PBSdata]{orfhistory}},
    \code{\link[PBSdata]{rrfhistory}}\cr\cr
In package \pkg{PBSmodelling}:\cr
    \code{\link[PBSmodelling]{calcGM}},
    \code{\link[PBSmodelling]{packList}}
}
\keyword{manip}
\keyword{database}
\keyword{datagen}
\concept{M07_CatchRecon}

