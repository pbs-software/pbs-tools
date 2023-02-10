\name{weightBio}
\alias{weightBio}
\title{
  Weight Age Proportions by Catch or Density
}
\description{
  Weight age or length frequencies\eqn{\Vert}{|}proportions by: \cr
  \emph{commercial}: sample trip catch within quarters and quarterly catch within years, or \cr
  \emph{survey}: sample species density (kg/km\eqn{^2}) in stratum and stratum area (km\eqn{^2}) in survey.
}
\usage{
weightBio(adat, cdat, sunit="TID", sweight="catch", 
   ttype=NULL, stype=c(0,1,2,5:8,17), scat=NULL, ameth=c(2,3,17), sex=2:1, major=NULL,
   wSP=c(TRUE,TRUE), wN=TRUE, plus=60, accum=TRUE, Nmin=0, Amin=NULL, 
   ctype="C", per=90, SSID=NULL, tabs=TRUE, gear=NULL,
   plot=TRUE, ptype="bubb", size=0.05, powr=0.5, zfld="wp", 
   clrs=list( c(.colBlind["blue"],"cyan"),
              c(.colBlind["bluegreen"],"chartreuse")),
   cohorts=NULL, regimes=list(1900:1908, 1912:1915, 1923:1929, 1934:1943,
   1957:1960, 1976:1988, 1992:1998, 2002:2006, 2014:2017),
   layout="portrait", win=TRUE, eps=FALSE, pdf=FALSE, png=FALSE, wmf=FALSE,
   longside=10, outnam, outres=400, ioenv=.GlobalEnv, lang=c("e","f"), ...)
}
\arguments{
  \item{adat}{\code{data.frame} -- ageing information (fields returned by 
    \code{gfb_bio.sql} are described in the Details section). }
  \item{cdat}{\code{data.frame} -- catch information (fields returned by 
    \code{getCatch} are described in the Details section). }
  \item{sunit}{\code{character} -- sample unit (field name):\cr
    \code{TID} -- samples identified by trip ID (commercial);\cr
    \code{SID} -- samples identified by sample ID (surveys).}
  \item{sweight}{\code{character} -- sample weighting field (e.g., \code{catch} or \code{density}).}
  \item{ttype}{\code{numeric} -- trip type code where: \cr
    \code{1}  = non-observed domestic,\cr
    \code{2}  = research,\cr
    \code{3}  = charter,\cr
    \code{4}  = observed domestic,\cr
    \code{5}  = observed joint-venture,\cr
    \code{6}  = non-observed joint-venture,\cr
    \code{7}  = Polish commercial national,\cr
    \code{8}  = Russian commercial national,\cr
    \code{9}  = Russian commercial supplemental,\cr
    \code{10}  = Polish commercial supplemental,\cr
    \code{11}  = recreational,\cr
    \code{12}  = Japanese observed national,\cr
    \code{13}  = Japanese commercial,\cr
    \code{14}  = unknown foreign.}
  \item{stype}{\code{numeric} -- sample type code where: \cr
    \code{0}  = unknown,\cr
    \code{1}  = total catch,\cr
    \code{2}  = random,\cr
    \code{4}  = selected,\cr
    \code{5}  = stratified,\cr
    \code{6}  = random from randomly assigned set,\cr
    \code{7}  = random from set after randomly assigned set,\cr
    \code{8}  = random from set requested by vessel master,\cr
    \code{9}  = selected juveniles,\cr
    \code{10} = selected adults.}
  \item{scat}{\code{numeric} -- species category code where: \cr
    \code{0}  = unknown,\cr
    \code{1}  = unsorted,\cr
    \code{2}  = sorted (unk. criterion),\cr
    \code{3}  = keepers,\cr
    \code{4}  = discarded,\cr
    \code{5}  = remains,\cr
    \code{6}  = longline -- fish head only,\cr
    \code{7}  = longline -- whole fish and fish head only,\cr
    \code{8}  = longline/jig -- fish lost at rail/lost at surface.}
  \item{ameth}{\code{numeric} -- ageing method code (\code{ameth=3} denotes break and burn).}
  \item{sex}{\code{numeric} -- sex codes (1 = males, 2 = females).}
  \item{major}{\code{numeric} -- major PMFC area codes (1=4A, 3=3C, 4=3D, 5=5A, 6=5B, 7=5C, 8=5D, 9=5E).}
  \item{wSP}{\code{logical} -- vector of length two for weighting by (i) sample catch and (ii)
    period (e.g., quarterly) commercial catch.}
  \item{wN}{\code{logical} -- if \code{TRUE}, weight the age frequencies else weight the age proportions. }
  \item{plus}{\code{numeric} -- age of the plus group (accumulate ages >= \code{'plus'}).}
  \item{accum}{\code{logical} -- if \code{TRUE}, accumulate ages >= \code{plus} in age class \code{'plus'};
    if \code{FALSE}, discard ages > \code{'plus'}.}
  \item{Nmin}{\code{numeric} -- minimum number of specimens by year and sex in order to display bubbles.}
  \item{Amin}{\code{numeric} -- minimum age to display on the y-axis if \code{plot=TRUE}; 
    the default \code{NULL} means the function automatically finds the youngest age observed.}
  \item{ctype}{\code{character} -- catch type where \code{"C"} = commercial and \code{"S"} = survey.}
  \item{per}{\code{numeric} -- period length (days) within a year (e.g., 90 for quarterly periods).}
  \item{SSID}{\code{numeric} -- survey series ID (can be a restratification series).}
  \item{tabs}{\code{logical} -- if \code{TRUE}, create formatted tables for use in other applications.}
  \item{gear}{\code{numeric} -- gear code(s) used in GFBioSQL (e.g., 1=bottom trawl, 5=longline).}
  \item{plot}{\code{logical} -- if \code{TRUE}, plot the results as bubble plots.}
  \item{ptype}{\code{character} -- plot type where \code{"bubb"} = bubbles and \code{"bars"} = horizontal bars.}
  \item{size}{\code{numeric} -- specify size (inches) of largest bubble in all panels.}
  \item{powr}{\code{numeric} -- power to scale bubbles, where \code{powr=0.5} makes bubble area roughly proportional to the z-value.}
  \item{zfld}{\code{character} -- field name containing values for output (e.g., \code{"wp"} for weighted proportions).}
  \item{clrs}{\code{character} -- specify bubble colours indexed by sex code (\code{0} = black).}
  \item{cohorts}{\code{numeric} -- diagonal lines to plot indicating year-class cohorts (list of x- and y-values).}
  \item{regimes}{\code{numeric} -- diagonal periods to plot indicating regime periods (e.g., positive PDO periods).}
  \item{layout}{\code{character} -- string describing figure layout; one of: \cr
    \code{"portrait"} for vertically stacked plots,\cr
    \code{"landscape"} for plot placed horizontally side-by-side,\cr
    \code{"juxtapose"} for plots overlaid on each other (not implemented at present).}
  \item{win}{\code{logical} -- if \code{TRUE}, send figure to the R graphics device.}
  \item{eps}{\code{logical} -- if \code{TRUE}, send figure to an encapsulated postscript \code{.eps} file.}
  \item{pdf}{\code{logical} -- if \code{TRUE}, send figure to a postscript \code{.pdf} file.}
  \item{png}{\code{logical} -- if \code{TRUE}, send figure to a portable network graphics \code{.png} file.}
  \item{wmf}{\code{logical} -- if \code{TRUE}, send figure to a Windows metafile \code{.wmf} file.}
  \item{longside}{\code{numeric} -- dimension (inches) of the longest side of the figure.}
  \item{outnam}{\code{character} -- explicit output name to override an internally generated one.}
  \item{outres}{\code{numeric} -- output resolution (dots per inch) for raster-based graphics.}
  \item{ioenv}{\code{environment} -- input/output environment for function input data and output results.}
  \item{lang}{\code{character} -- a vector of letters that denote the language for output:
    currently only \code{"e"} (english) and \code{"f"} (french).}
  \item{...}{additional arguments to pass to the function \code{plotBubbles}.}
}
\details{
  \bold{Input Data}

  \code{adat} -- Age data (e.g., \code{age396} for Pacific Ocean Perch) is best
  derived from the query \code{gfb_bio.sql} using the command line calls:
  \preformatted{
  getData("gfb_bio.sql","GFBioSQL",strSpp="396",path=.getSpath())
  bio396 = PBSdat; age396 = bio396[bio396$age>0 & !is.na(bio396$age),]
  save("bio396","age396",file="bio396.rda") # save objects for future sessions
  }
  The fields returned by \code{gfb_bio.sql} include:
  \tabular{lrl}{
    \code{VID}      \tab integer   \tab Vessel identification number (ID) in GFBio database (GFB)\cr
    \code{TID}      \tab integer   \tab Trip ID in GFB\cr
    \code{FEID}     \tab integer   \tab Fishing Event ID in GFB\cr
    \code{CID}      \tab integer   \tab Catch record ID in GFB\cr
    \code{SID}      \tab integer   \tab Sample ID in GFB\cr
    \code{SPID}     \tab integer   \tab Specimen (fish) ID in GFB\cr
    \code{SVID}     \tab integer   \tab Survey ID (unique to each survey, usually annually)\cr
    \code{SSID}     \tab integer   \tab Survey Series ID describing a group of annual surveys in specific areas\cr
    \code{GC}       \tab integer   \tab Grouping code that describes a stratum in a survey series\cr
    \code{RC}       \tab integer   \tab Reason code -- purpose of fishing event\cr
    \code{block}    \tab character \tab Area descriptor for Hard-Bottom Longline surveys\cr
    \code{hail}     \tab integer   \tab Vessel hail-in number at port\cr
    \code{set}      \tab integer   \tab Primary set in FEID -- e.g. trawl tow, trap line, long line (\code{FE_MAJOR_LEVEL_ID})\cr
    \code{sset}     \tab integer   \tab Secondary set in FEID -- e.g. trap in trap line, skate in longline\cr
    \code{ssset}    \tab integer   \tab Tertiary set in FEID -- e.g. hooks in skate\cr
    \code{ttype}    \tab integer   \tab Trip type code -- describes purpose of trip\cr
    \code{stype}    \tab integer   \tab Sample type code -- describes how sample was collected\cr
    \code{ftype}    \tab ineteger  \tab Freezer trawl type -- 0=not freezer trawl vessel, 1=freezer trawl vessel\cr
    \code{date}     \tab POSIXt    \tab Date of sample if available, otherwise end date of trip\cr
    \code{year}     \tab integer   \tab Year of sample if available, otherwise end date of trip,\cr
    \code{spp}      \tab character \tab Species code -- for fish, page number that species appears in Hart (1973)\cr
    \code{sex}      \tab integer   \tab Sex of speciment - 0=not sexed, 1=male, 2=female, 3=observed but unkown\cr
    \code{mat}      \tab integer   \tab Maturity -- usually on a scale from 1-7 for rockfish\cr
    \code{oto}      \tab integer   \tab Otoliths collected -- 0=no, 1=yes\cr
    \code{fin}      \tab integer   \tab Fin rays collected -- 0=no, 1=yes\cr
    \code{scale}    \tab integer   \tab Fin scales collected -- 0=no,1=yes\cr
    \code{uage}     \tab integer   \tab Flag for unknown collection attribute code associated with aged specimen\cr
    \code{narc}     \tab numeric   \tab Not available reason code\cr
    \code{age}      \tab integer   \tab Age (years) of specimen\cr
    \code{ameth}    \tab integer   \tab Ageing method code\cr
    \code{len}      \tab numeric   \tab Length (cm) of specimen\cr
    \code{wt}       \tab numeric   \tab Weight (kg) of specimen\cr
    \code{scat}     \tab integer   \tab Species category code -- to determine unsorted vs.sorted\cr
    \code{cver}     \tab integer   \tab Catch verification code\cr
    \code{fdep}     \tab numeric   \tab Fishing depth (m) -- usually mean depth between start and end tow\cr
    \code{bwt}      \tab numeric   \tab Bottom water temperature (\eqn{^\circ}{deg}C),\cr
    \code{gear}     \tab integer   \tab Gear code\cr
    \code{use}      \tab integer   \tab Usability of data for CPUE or survey adundance calculations\cr
    \code{dist}     \tab numeric   \tab Distance (km) of trawl tow (length of tow capture area)\cr
    \code{door}     \tab numeric   \tab Doorspread (m) of tawl net (width of tow capture area)\cr
    \code{major}    \tab integer   \tab Pacific Marine Fisheries Commission (PMFC) major area code\cr
    \code{minor}    \tab integer   \tab PMFC minor area codes\cr
    \code{locality} \tab integer   \tab DFO locality codes -- fishing grounds\cr
    \code{X}        \tab numeric   \tab Longitude (\eqn{^\circ}{deg}W)\cr
    \code{Y}        \tab numeric   \tab Latitude (\eqn{^\circ}{deg}N)\cr
    \code{PMFC}     \tab character \tab Pacific Marine Fisheries Commission names\cr
    \code{PFMA}     \tab integer   \tab Pacific Fishery Management Area numbers\cr
    \code{PFMS}     \tab integer   \tab Pacific Fishery Management Subarea numbers\cr
    \code{GMA}      \tab character \tab Groundfish Management Areas (based on PFMA and PFMS combinations)\cr
    \code{catch}    \tab numeric   \tab Catch (kg) of species (spp) in fishing event (FEID)\cr
    \code{nfish}    \tab numeric   \tab Catch (# fish) of species (spp) in fishing event (FEID)\cr
    \code{density}  \tab numeric   \tab Density (t/km\eqn{^2}) of species (spp) in fishing event (FEID)\cr
    \code{area}     \tab integer   \tab Survey stratum area (km\eqn{^2}).
  }                                     
  \code{cdat} -- Catch data (e.g., \code{cat396}) is best derived from the 
  function \code{getCatch} using the command line calls:
  \preformatted{                        
  cat396 = getCatch("396", sql=TRUE)    
  Ccat396 = cat396$Ccat; Scat396 = cat396$Scat
  save("cat396","Ccat396","Scat396",file="cat396.rda") # save objects
  }
  The call to \code{getCatch} internally calls \code{getData}, which calls
  various queries:\cr
  \code{  gfb_catch_records.sql} -- GFBioSQL (survey catches); \cr
  \code{  gfc_catch_records.sql} -- GFCatch (commercial trawl and trap catch 1954--1995);\cr
  \code{  pht_catch_records.sql} -- PacHarvest (commercial trawl catch 1996--2007);\cr
  \code{  fos_catch_records.sql} -- GFFOS (commercial all-fisheries data 2006--present).

  Note that the \code{getCatch} call above returns a list object containing
  two data frames. 
  Each list element is called \code{Ccat} (for the commercial catch) and 
  \code{Scat} (for the survey catch). The input \code{cdat} to the function
  \code{weightBio} uses one or the other of these data frames, depending on the 
  argument \code{ctype} (catch type).

  The fields returned by \code{getCatch} for commercial catch are:
  \tabular{lrl}{
    \code{EID}      \tab integer   \tab \code{\link[PBSmapping]{EventData}} field,\cr
    \code{X}        \tab numeric   \tab longitude (\eqn{^\circ}{deg}W),\cr
    \code{Y}        \tab numeric   \tab latitude (\eqn{^\circ}{deg}N),\cr
    \code{FID}      \tab integer   \tab fishery identifier:\cr
                    \tab           \tab 1=trawl, 2=halibut, 3=sablefish, 4=dogfish/lingcod, 5=H&L rockfish,\cr
    \code{TID}      \tab integer   \tab trip identifier,\cr
    \code{FEID}     \tab integer   \tab fishing event identifier,\cr
    \code{hail}     \tab integer   \tab hail number used by vessels,\cr
    \code{set}      \tab integer   \tab set number within a trip/hail,\cr
    \code{log}      \tab integer   \tab log type code or data source,\cr
    \code{date}     \tab POSIXt    \tab comercial fishing event date,\cr
    \code{year}     \tab integer   \tab calendar year,\cr
    \code{major}    \tab integer   \tab PMFC major area code,\cr
    \code{minor}    \tab integer   \tab PMFC minor area code,\cr
    \code{locality} \tab integer   \tab fishing locality code,\cr
    \code{pfma}     \tab integer   \tab Pacific Fishery Management Areas,\cr
    \code{pfms}     \tab integer   \tab Pacific Fishery Management Subareas,\cr
    \code{depth}    \tab numeric   \tab fishing depth (m),\cr
    \code{vessel}   \tab numeric   \tab vessel registration number,\cr
    \code{gear}     \tab integer   \tab gear code,\cr
    \code{success}  \tab integer   \tab trawl success code,\cr
    \code{effort}   \tab numeric   \tab fishing event effort (min),\cr
    \code{catKg}    \tab numeric   \tab fishing event catch (kg),\cr
    \code{pdis}     \tab numeric   \tab proportion of species total catch discarded in fishing event,\cr
    \code{pcat}     \tab numeric   \tab proportion of species total catch to fishing event total catch.
  }
  The fields returned by \code{getCatch} for survey catch are:
  \tabular{lrl}{
    \code{TID}      \tab integer   \tab trip identifier,\cr
    \code{FEID}     \tab integer   \tab fishing event identifier,\cr
    \code{set}      \tab integer   \tab set number within a trip,\cr
    \code{CID}      \tab integer   \tab catch identifier,\cr
    \code{SSID}     \tab integer   \tab survey series identifier,\cr
    \code{SVID}     \tab integer   \tab survey identifier,\cr
    \code{OI}       \tab integer   \tab original index (1) or not (0) for survey series,\cr
    \code{GC}       \tab integer   \tab grouping code,\cr
    \code{block}    \tab character \tab block designator,\cr
    \code{date}     \tab POSIXt    \tab survey fishing event date,\cr
    \code{year}     \tab integer   \tab calendar year,\cr
    \code{major}    \tab integer   \tab PMFC major area code,\cr
    \code{minor}    \tab integer   \tab PMFC minor area code,\cr
    \code{locality} \tab integer   \tab fishing locality code,\cr
    \code{spp}      \tab character \tab species Hart code,\cr
    \code{wt}       \tab numeric   \tab species catch (kg) in fishing event,\cr
    \code{pcs}      \tab integer   \tab species catch (pieces) in fishing events,\cr
    \code{catKg}    \tab numeric   \tab species catch (kg) -- if \code{wt} not reported, use \code{pcs} converted to kg.
  }
  See examples below for guidance on how to call the function \code{weightBio}.
}
\note{
  The function uses values in the field specified by \code{sweight} to weight the 
  proportions-at-age (or frequencies-at-age). For example, if 
  \code{sweight="catch"} then the first weighting for commercial samples might 
  be by trip catch with a quarter year to yield a weighted proportion-at-age
  vector within each quarter. Similarly, the first weighting for a survey might
  be by sample catch within a stratum to yield a weighted proportion-at-age
  vector within each stratum. The second weighting would further weight these 
  period/stratum vectors by total catch of the species in each period/stratum 
  within a year/survey.
  
  The function is not designed to scale up observations based on areal 
  densities, though it can use this information to weight the proportions.
  For surveys in particular, if \code{sweight="density"}, then the first 
  weighting is by density of the species in each sample to derive a vector of
  weighted proportions for each stratum, and the second weighting is simply
  by the stratum area (km\eqn{^2}).
  
  Weighting by density is treated differently here to 'emulate' a survey 
  sample design. The code's algorithm usually takes data for the first weighting
  from the age object and data for the second weighting from the catch object.
  In the special case outlined in the previous paragraph, data for both 
  weightings come from the age object.
}
\value{
  An array with the four dimensions: \code{age}, \code{year}, 
  \code{sex}, and \code{np} = numbers/proportions. \cr
  The age array has two attributes attached: \cr
  \code{wpatab} = two-dimensional matrix of weighted proportions-at-age flattened 
    by sex for input to the Awatea/Coleraine population model. \cr
  \code{sumtab} = supplementary array with dimensions \code{year}, \code{per}
    = period (quarters), and \code{stat} = statistic, where statistic is one of the following: \cr
    \code{Nsid}  = no. samples,\cr
    \code{Ntid}  = no. trips (which roughly equates to number of samples in the non-observed fishery),\cr
    \code{Scat}  = sample unit catch (in our case the sample unit is the trip),\cr
    \code{Fcat}  = commercial fishery catch,\cr
    \code{Psamp} = trip catch/fishery catch.
}
\examples{\dontrun{
# Commercial age data
# -------------------
out = weightBio(age396, Ccat396, major=c(3:9), ttype=c(1,4), gear=1, 
   stype=c(1,2,6,7), sunit="TID", Nmin=0, plus=60, ameth=3, size=0.20, lwd=2)

# Survey age data
# ---------------
out = weightBio(age396, Scat396, ctype="S", sunit="SID", SSID=1, 
   stype=c(1,2,6,7), Nmin=0, plus=60, ameth=3, sweight="density", size=0.15)
}}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Regional Headquarters (RHQ), Vancouver BC\cr
  Last modified \code{Rd: 2023-02-08}

  \href{mailto:paul@starrfish.net}{Paul J. Starr}, Stock Assessment Scientist\cr
  Canadian Groundfish Research & Conservation Society (CGRCS), Nanaimo BC
}
\seealso{
  \code{\link[PBStools]{getCatch}}, \code{\link[PBStools]{getData}},
  \code{\link[PBStools]{plotProp}}, \code{\link[PBStools]{reportCatchAge}},
  \code{\link[PBStools]{requestAges}} \cr
  Available SQL queries: \code{\link[PBStools]{SQLcode}}
}
\keyword{datagen}
\concept{M02_Biology}

