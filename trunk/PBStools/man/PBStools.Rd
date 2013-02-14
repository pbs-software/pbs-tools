\name{PBStools}
\docType{package}
\alias{PBStools}
\alias{PBStools-package}
\title{PBS Tools}

\description{
  \pkg{PBStools} provides tools for stock assessments, 
  species-at-risk requirements, and stakeholder requests. 
  The package depends on four other R packages: \pkg{PBSmapping}, 
  \pkg{PBSmodelling}, \pkg{PBSdata}, and \pkg{RODBC}. 

  \pkg{PBStools} contains the following functions:
  \tabular{ll}{
  \bold{Utilities}      \tab \cr
  \code{biteData}       \tab Subset a data matrix/frame using a vector object.\cr
  \code{chewData}       \tab Remove sparse category records from data matrix/frame.\cr
  \code{collectFigs}    \tab Collect postscript figures into one document.\cr
  \code{confODBC}       \tab Configure an ODBC user data source name (DSN).\cr
  \code{convFY}         \tab Convert dates into fishing/fiscal years.\cr
  \code{convYM}         \tab Convert date limits into expanded year-months.\cr
  \code{convYP}         \tab Convert dates into binned year periods.\cr
  \code{createDSN}      \tab Create user DSNs for PBS groundfish databases.\cr
  \code{crossTab}       \tab Use package 'reshape' to summarize \eqn{z} using crosstab values \eqn{y}.\cr
  \code{dtget}          \tab Get/print objects from or put objects into temporary work environment (\code{.PBSdataEnv}).\cr
  \code{fitLogit}       \tab Fit binomial data using logit link function.\cr
  \code{flagIt}         \tab Label a coordinate \eqn{(a,b)} using a diagonal line.\cr
  \code{getData}        \tab Get data from a variety of sources.\cr
  \code{getFile}        \tab Get data from binaries or ASCII.\cr
  \code{getName}        \tab Get string names from literals or named objects.\cr
  \code{isThere}        \tab Checks whether an object is physically present in a specified environment.\cr
  \code{lenv}           \tab Get the local/parent/global environment.\cr
  \code{listTables}     \tab List tables in specified SQL/ORA database.\cr
  \code{makeLTH}        \tab Make a longtable header for Sweave.\cr
  \code{prime}          \tab Report the prime numbers given an integer vector.\cr
  \code{revStr}         \tab Reverse a string set.\cr
  \code{runModules}     \tab Run GUI modules included in PBS Fishery.\cr
  \code{scaleVec}       \tab Scale a vector to span a target minimum and maximum.\cr
  \code{showError}      \tab Display an error message on the current device.\cr
  \code{showMessage}    \tab Display an information message on the current device.\cr
  \code{spooler}        \tab Spool information specified by a list into a new field of an existing data frame.\cr
  \code{stdConc}        \tab Standardise a chemical concentration.\cr
  \code{toUpper}        \tab Capitalise first letters of words.\cr
  \code{ttget}          \tab Get/print objects from or put objects into temporary work environment (\code{.PBStoolEnv}).\cr
  \code{wrapText}       \tab Wrap, indent, and prefix a long text string.\cr
  \code{zapDupes}       \tab Remove records with duplicated indices from a data frame.\cr
  \tab \cr
  \bold{Biology}        \tab \cr
  \code{calcLenWt}      \tab Calculate length-weight relationship for a fish.\cr
  \code{calcSG}         \tab Calculate growth curve using Schnute growth model.\cr
  \code{calcVB}         \tab Calculate fits using a von Bertalanffy growth model.\cr
  \code{compCsum}       \tab Compare cumulative frequencies (e.g., of ages).\cr
  \code{estOgive}       \tab Estimate ogive curves for maturity.\cr
  \code{genPa}          \tab Generate proportions-at-age using catch curve composition.\cr
  \code{histMetric}     \tab Plot a matrix of histograms for a specified metric.\cr
  \code{histTail}       \tab Plot histogram with optional tail zoom.\cr
  \code{mapMaturity}    \tab Map species maturity by month.\cr
  \code{plotProp}       \tab \bold{GUI:} Create bubble plots showing proportions-at-age etc.\cr
  \code{predictRER}     \tab Predict Rougheye Rockfish from biological data.\cr
  \code{processBio}     \tab Process results from SQL query \code{gfb_bio.sql}.\cr
  \code{reportCatchAge} \tab \bold{GUI:} Plot results from Jon Schnute's catch-at-age ADMB report file.\cr
  \code{requestAges}    \tab Determine which otoliths to sample for ageing requests.\cr
  \code{simBSR}         \tab Simulate Blackspotted Rockfish biological data.\cr
  \code{simRER}         \tab Simulate Rougheye Rockfish biological data.\cr
  \code{sumBioTabs}     \tab Summarize frequency occurrence of biological samples.\cr
  \code{weightBio}      \tab Weight age/length frequencies/proportions by catch.\cr
  \tab \cr
  \bold{Fishery}        \tab \cr
  \code{calcRatio}      \tab Calculates ratios of numerator to denominator (e.g., \sQuote{discard}/\sQuote{catch}).\cr
  \code{dumpMod}        \tab Dump catch from modern sources used in catch reconstruction.\cr
  \code{dumpRat}        \tab Dump catch ratios calculated by a catch reconstruction.\cr
  \code{formatCatch}    \tab Format table of numeric catch as strings.\cr
  \code{getCatch}       \tab Get catch records for a species from various databases and combine.\cr
  \code{glimmer}        \tab Perform a standardised GLM analysis.\cr
  \code{makeCATtables}  \tab Make catch tables from commercial fishery data.\cr
  \code{plotCatch}      \tab Plot catch history as an annual barplot.\cr
  \code{plotConcur}     \tab Plot concurrent species in tows at depth.\cr
  \code{plotFOScatch}   \tab Plot monthly catch from FOS as barplots.\cr
  \code{plotRecon}      \tab Plot reconstructed catch using barplots stacked by PMFC area.\cr
  \code{runCCA}         \tab Catch-curve model based on Schnute and Haigh (2006).\cr
  \code{sumCatTabs}     \tab Summarize catch by year and PMFC from modern catch data used in catch reconstruction.\cr
  \code{trackBycat}     \tab Track annual fish group catches between depth limits.\cr
  \tab \cr
  \bold{Survey}         \tab \cr
  \code{bootBG}         \tab Bootstrap biomass using binomial-gamma population parameters.\cr
  \code{calcMoments}    \tab Calculate survey moments from raw data.\cr
  \code{calcPMR}        \tab Calculate \eqn{(p,\mu,\rho)} from a sample population.\cr
  \code{getBootRuns}    \tab Get Norm's survey bootstrap results.\cr
  \code{getPMR}         \tab Get \eqn{(p,\mu,\rho)} values for survey strata.\cr
  \code{makePMRtables}  \tab Make \eqn{(p,\mu,\rho)} tables for surveys.\cr
  \code{makeSSID}       \tab Make a data object of survey series information.\cr
  \code{sampBG}         \tab Sample from the binomial-gamma distribution.\cr
  \code{showAlpha}      \tab Show quantile confidence levels (\eqn{\alpha}) for bootstraps.\cr
  \code{showIndices}    \tab Show survey indices from bootstrap tables.\cr
  \code{simBGtrend}     \tab Simulate population projection based on prior binomial-gamma parameters.\cr
  \code{trend}          \tab \bold{GUI:} Create boxplots of annual survey data, trend lines, and bootstraps.\cr
  \tab \cr
  \bold{Spatial}        \tab \cr
  \code{calcHabitat}    \tab Calculate potential habitat using bathymetry limits.\cr
  \code{calcOccur}      \tab Calculate percent occurrence of EventData in PolySet.\cr
  \code{calcSRFA}       \tab Determine slope rockfish assessment areas or subareas.\cr
  \code{calcSurficial}  \tab Calculate intersection of surficial geology and bathymetry habitat.\cr
  \code{clarify}        \tab Summarize catch data into CLARA clusters.\cr
  \code{findHoles}      \tab Find holes and place them under correct parents.\cr
  \code{plotGMA}        \tab Plot the Groundfish Management Areas.\cr
  \code{preferDepth}    \tab \bold{GUI:} Plot depth distribution of species.\cr
  \code{prepClara}      \tab Prepare a data object for Clustering Large Applications.\cr
  \code{zapHoles}       \tab Zap (remove) holes from polygons.\cr
  \tab \cr
  \bold{Temporal}       \tab \cr
  \code{boxSeason}      \tab Display seasonal patterns using boxplots.\cr
  \code{calcMA}         \tab Calculate a moving average using a fixed period occurring every \eqn{x} units.\cr
  \code{imputeRate}     \tab \bold{GUI:} Impute the rate of return of an investment.\cr
  \code{plotDiversity}  \tab Plot diversity of phytoplankton samples.\cr
  \code{trackComp}      \tab Track composition of phytoplankton over time.\cr
  \tab \cr
  \bold{Catch Reconstruction}  \tab \cr
  \code{buildCatch}     \tab Build a catch history of BC rockfish 1918--present.\cr
  \code{plotData}       \tab Plot diagnostic data for catch reconstructions.\cr
  \code{plotRecon}      \tab Plot reconstructed annual catch series.
  }

  Other package resources:
  \tabular{ll}{
  \code{../library/PBStools/doc}   \tab includes a User's Guide describing all functions.\cr
  \code{../library/PBStools/ADMB}  \tab ADMB batch files and project files (\code{pop}, \code{vonB}).\cr
  \code{../library/PBStools/sql}   \tab SQL code files for querying remote databases.\cr
  \code{../library/PBStools/win}   \tab \pkg{PBSmodelling} \emph{window description files} for GUIs.
  }
}
\note{
  The temporary working list object \code{PBStool} (formerly \code{PBSfish}) is now
  stored in the temporary working environment \code{.PBStoolEnv} (see \code{ttget} 
  functions to access the object).
}
\seealso{
  \code{\link[PBStools]{getData}}, \code{\link[PBStools]{ttget}} \cr
  \code{\link[PBStools]{ServerParlance}} for differences between SQL Server and Oracle database organisation.\cr
  \code{\link[PBStools]{SQLcode}} for available SQL queries.\cr
}
\keyword{package}

