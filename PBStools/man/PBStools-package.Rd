\name{PBStools-package}
\alias{PBStools-package}
\alias{PBStools}
\docType{package}

\title{PBStools -- Tools for the Efficient Execution of Onerous Tasks}

\description{
  \pkg{PBStools} provides tools for stock assessments, 
  species-at-risk requirements, and stakeholder requests. 
  The package depends on four other R packages: \pkg{PBSmapping}, 
  \pkg{PBSmodelling}, \pkg{PBSdata}, and \pkg{RODBC}. 

  \pkg{PBStools} contains the following functions by module (M):
  \tabular{ll}{
  \bold{M01. Utilities}   \tab \cr
  \code{\link[PBStools]{addStrip}}      \tab Add a vertical colour strip as a legend;\cr
  \code{\link[PBStools]{biteData}}      \tab Subset a data matrix/frame using a vector object;\cr
  \code{\link[PBStools]{calcHM}}        \tab Calculate the harmonic mean of a vector of numbers;\cr
  \code{\link[PBStools]{changeLangOpts}}\tab Change the options that control the display of numbers based on language;\cr
  \code{\link[PBStools]{chewData}}      \tab Remove sparse category records from data matrix/frame;\cr
  \code{\link[PBStools]{clearFiles}}    \tab Check to see if file exists and keep a copy if it does before removing it;\cr
  \code{\link[PBStools]{confODBC}}      \tab Configure an ODBC user data source name (DSN);\cr
  \code{\link[PBStools]{convCT}}        \tab Convert a crossTab object to regular matrix or data frame;\cr
  \code{\link[PBStools]{convFY}}        \tab Convert dates into fishing/fiscal years;\cr
  \code{\link[PBStools]{convUTF}}       \tab Convert UTF-8 strings into Unicode characters;\cr
  \code{\link[PBStools]{convYM}}        \tab Convert date limits into expanded year-months;\cr
  \code{\link[PBStools]{convYP}}        \tab Convert dates into binned year periods;\cr
  \code{\link[PBStools]{countLines}}    \tab Count the number of lines in an ASCII file;\cr
  \code{\link[PBStools]{countVec}}      \tab Count number of definite vector elements (non NA) that exclude (or include) zero values;\cr
  \code{\link[PBStools]{createDSN}}     \tab Create user DSNs for PBS groundfish databases;\cr
  \code{\link[PBStools]{createFdir}}    \tab Create a subdirectory called `french' for storing figures with French text and labels;\cr
  \code{\link[PBStools]{crossTab}}      \tab Summarise values from a data table using one or more fields in the table;\cr
  \code{\link[PBStools]{dtget}}         \tab get|print|call objects from, or put objects into, temporary work environment (\code{.PBSdataEnv});\cr
  \code{\link[PBStools]{darkenRGB}}     \tab Programmatically darken the colour of given RGB values;\cr
  \code{\link[PBStools]{expand5C}}      \tab Transfer events from Moresby Gully in 5B and Flamingo Inlet/Anthony Is. in 5E to PMFC 5C;\cr
  \code{\link[PBStools]{extractPost}}   \tab Extract model posteriors from three model platforms (Awatea, SS3, iSCAM) for clients;\cr
  \code{\link[PBStools]{findPV}}        \tab Find nearest position in vector choice using a target point;\cr
  \code{\link[PBStools]{findRC}}        \tab Return no. (rows, columns) for multi-panel figures given no. figures to fit on one page;\cr
  \code{\link[PBStools]{fitLogit}}      \tab Fit binomial data using logit link function;\cr
  \code{\link[PBStools]{flagIt}}        \tab Label a coordinate \eqn{(a,b)} using a diagonal line;\cr
  \code{\link[PBStools]{gatherVals}}    \tab Gathers data from multiple columns into key-value pairs (replaces tidyr::gather);\cr
  \code{\link[PBStools]{getData}}       \tab Get data from a variety of sources;\cr
  \code{\link[PBStools]{getFile}}       \tab Get data from binaries or ASCII;\cr
  \code{\link[PBStools]{getName}}       \tab Get string names from literals or named objects;\cr
  \code{\link[PBStools]{getODBC}}       \tab Get a string vector of ODBC drivers on user's Windows system;\cr
  \code{\link[PBStools]{installPkgs}}   \tab Install specified packages if they are missing or if newer versions are available;\cr
  \code{\link[PBStools]{inWord}}        \tab Find morphemes (parts of words) in word and report T/F;\cr
  \code{\link[PBStools]{isThere}}       \tab Checks whether an object is physically present in a specified environment;\cr
  \code{\link[PBStools]{lenv}}          \tab Get the local/parent/global environment;\cr
  \code{\link[PBStools]{linguaFranca}}  \tab Translate English phrases to French in figures;\cr
  \code{\link[PBStools]{listTables}}    \tab List tables in specified SQL/ORA database;\cr
  \code{\link[PBStools]{prime}}         \tab Report the prime numbers given an integer vector;\cr
  \code{\link[PBStools]{quantBox}}      \tab Redefine boxplot to show quantiles;\cr
  \code{\link[PBStools]{readClog}}      \tab Read a \code{ChangeLog} file and convert it to an R list;\cr
  \code{\link[PBStools]{revStr}}        \tab Reverse a string set;\cr
  \code{\link[PBStools]{runModules}}    \tab Run GUI modules included in PBS Fishery;\cr
  \code{\link[PBStools]{scaleVec}}      \tab Scale a vector to span a target minimum and maximum;\cr
  \code{\link[PBStools]{showError}}     \tab Display an error message on the current device;\cr
  \code{\link[PBStools]{showMessage}}   \tab Display an information message on the current device;\cr
  \code{\link[PBStools]{spooler}}       \tab Spool information specified by a list into a new field of an existing data frame;\cr
  \code{\link[PBStools]{stdConc}}       \tab Standardise a chemical concentration;\cr
  \code{\link[PBStools]{subsetFile}}    \tab Subset an ASCII file every n rows (enrow);\cr
  \code{\link[PBStools]{testPch}}       \tab Display plotting symbols or octal strings;\cr
  \code{\link[PBStools]{toUpper}}       \tab Capitalise first letters of words;\cr
  \code{\link[PBStools]{ttget}}         \tab Get/print objects from or put objects into temporary work environment (\code{.PBStoolEnv});\cr
  \code{\link[PBStools]{wrapText}}      \tab Wrap, indent, and prefix a long text string;\cr
  \code{\link[PBStools]{zapDupes}}      \tab Remove records with duplicated indices from a data frame.\cr
  \tab \cr
  \bold{M02. Biology}     \tab \cr
  \code{\link[PBStools]{calcLW}}        \tab Calculate length-weight relationship for a fish;\cr
  \code{\link[PBStools]{calcSG}}        \tab Calculate growth curve using Schnute growth model;\cr
  \code{\link[PBStools]{calcVB}}        \tab Calculate fits using a von Bertalanffy growth model;\cr
  \code{\link[PBStools]{compCsum}}      \tab Compare cumulative frequencies (e.g., of ages);\cr
  \code{\link[PBStools]{estOgive}}      \tab Estimate ogive curves for maturity;\cr
  \code{\link[PBStools]{extractAges}}   \tab Extract records with positive age and qualify by the selected ageing method;\cr
  \code{\link[PBStools]{genPa}}         \tab Generate proportions-at-age using catch curve composition;\cr
  \code{\link[PBStools]{histMetric}}    \tab Plot a matrix of histograms for a specified metric;\cr
  \code{\link[PBStools]{histTail}}      \tab Plot histogram with optional tail zoom;\cr
  \code{\link[PBStools]{mapMaturity}}   \tab Map species maturity by month;\cr
  \code{\link[PBStools]{plotAdens}}     \tab Plot density of field (length) by age as waveforms;\cr
  \code{\link[PBStools]{plotProp}}      \tab \bold{GUI:} Create bubble plots showing proportions-at-age etc;\cr
  \code{\link[PBStools]{predictRER}}    \tab Predict Rougheye Rockfish from biological data;\cr
  \code{\link[PBStools]{reportCatchAge}}\tab \bold{GUI:} Plot results from Jon Schnute's catch-at-age ADMB report file;\cr
  \code{\link[PBStools]{requestAges}}   \tab Determine which otoliths to sample for ageing requests;\cr
  \code{\link[PBStools]{residVB}}       \tab Calculate and plot Studentised residuals as quantile plots;\cr
  \code{\link[PBStools]{simBSR}}        \tab Simulate Blackspotted Rockfish biological data;\cr
  \code{\link[PBStools]{simRER}}        \tab Simulate Rougheye Rockfish biological data;\cr
  \code{\link[PBStools]{sumBioTabs}}    \tab Summarize frequency occurrence of biological samples;\cr
  \code{\link[PBStools]{weightBio}}     \tab Weight age/length frequencies/proportions by catch.\cr
  \tab \cr
  \bold{M03. Fishery}     \tab \cr
  \code{\link[PBStools]{calcRatio}}     \tab Calculates ratios of numerator to denominator (e.g., \sQuote{discard}/\sQuote{catch});\cr
  \code{\link[PBStools]{dumpMod}}       \tab Dump catch from modern sources used in catch reconstruction;\cr
  \code{\link[PBStools]{dumpRat}}       \tab Dump catch ratios calculated by a catch reconstruction;\cr
  \code{\link[PBStools]{getCatch}}      \tab Get catch records for a species from various databases and combine;\cr
  \code{\link[PBStools]{glimmer}}       \tab Perform a standardised GLM analysis;\cr
  \code{\link[PBStools]{makeCATtables}} \tab Make catch tables from commercial fishery data;\cr
  \code{\link[PBStools]{plotCatch}}     \tab Plot catch history as an annual barplot;\cr
  \code{\link[PBStools]{plotFOScatch}}  \tab Plot monthly catch from FOS as barplots;\cr
  \code{\link[PBStools]{runCCA}}        \tab Catch-curve model based on Schnute and Haigh (2006);\cr
  \code{\link[PBStools]{sumCatTabs}}    \tab Summarize catch by year and PMFC from modern catch data used in catch reconstruction;\cr
  \code{\link[PBStools]{trackBycat}}    \tab Track annual fish group catches between depth limits.\cr
  \tab \cr
  \bold{M04. Survey}      \tab \cr
  \code{\link[PBStools]{bootBG}}        \tab Bootstrap biomass using binomial-gamma population parameters;\cr
  \code{\link[PBStools]{calcMoments}}   \tab Calculate survey moments from raw data;\cr
  \code{\link[PBStools]{calcPMR}}       \tab Calculate \eqn{(p,\mu,\rho)} from a sample population;\cr
  \code{\link[PBStools]{getBootRuns}}   \tab Get Norm's survey bootstrap results;\cr
  \code{\link[PBStools]{getPMR}}        \tab Get \eqn{(p,\mu,\rho)} values for survey strata;\cr
  \code{\link[PBStools]{makePMRtables}} \tab Make \eqn{(p,\mu,\rho)} tables for surveys;\cr
  \code{\link[PBStools]{makeSSID}}      \tab Make a data object of survey series information;\cr
  \code{\link[PBStools]{sampBG}}        \tab Sample from the binomial-gamma distribution;\cr
  \code{\link[PBStools]{showAlpha}}     \tab Show quantile confidence levels (\eqn{\alpha}) for bootstraps;\cr
  \code{\link[PBStools]{showIndices}}   \tab Show survey indices from bootstrap tables;\cr
  \code{\link[PBStools]{simBGtrend}}    \tab Simulate population projection based on prior binomial-gamma parameters;\cr
  \code{\link[PBStools]{trend}}         \tab \bold{GUI:} Create boxplots of annual survey data, trend lines, and bootstraps.\cr
  \tab \cr
  \bold{M05. Spatial}     \tab \cr
  \code{\link[PBStools]{calcHabitat}}   \tab Calculate potential habitat using bathymetry limits;\cr
  \code{\link[PBStools]{calcOccur}}     \tab Calculate percent occurrence of EventData in PolySet;\cr
  \code{\link[PBStools]{calcSRFA}}      \tab Determine slope rockfish assessment areas or subareas;\cr
  \code{\link[PBStools]{calcStockArea}} \tab Assign a stock area designation based on species HART code and PMFC major and/or minor areas;\cr
  \code{\link[PBStools]{calcWAParea}}   \tab Assign a stock area designation for Walleye Pollock using PMFC major and minor areas,\cr
                                        \tab a stratifying vector, and a weighting vector;\cr
  \code{\link[PBStools]{calcSurficial}} \tab Calculate intersection of surficial geology and bathymetry habitat;\cr
  \code{\link[PBStools]{clarify}}       \tab Summarize catch data into CLARA clusters;\cr
  \code{\link[PBStools]{findEP}}        \tab Find events in polys and and add poly info to events;\cr
  \code{\link[PBStools]{findHoles}}     \tab Find holes and place them under correct parents;\cr
  \code{\link[PBStools]{plotConcur}}    \tab Plot concurrent species in tows at depth;\cr
  \code{\link[PBStools]{plotEO}}        \tab Plot Extent of Occurrence for a species using a convex hull;\cr
  \code{\link[PBStools]{plotGMA}}       \tab Plot the Groundfish Management Areas;\cr
  \code{\link[PBStools]{plotHabitat}}   \tab Plot potential habitat using bathymetry output from \code{calcHabitat};\cr
  \code{\link[PBStools]{plotLocal}}     \tab Plot DFO fishing localities with the highest catch;\cr
  \code{\link[PBStools]{plotTernary}}   \tab Plot a ternary diagram for data amalgamated into 3 groups;\cr
  \code{\link[PBStools]{plotTertiary}}  \tab Composition plots within a polygonal space;\cr
  \code{\link[PBStools]{preferDepth}}   \tab \bold{GUI:} Plot depth distribution of species;\cr
  \code{\link[PBStools]{prepClara}}     \tab Prepare a data object for Clustering Large Applications;\cr
  \code{\link[PBStools]{zapHoles}}      \tab Zap (remove) holes from polygons.\cr
  \tab \cr
  \bold{M06. Assessment}  \tab \cr
  \code{\link[PBStools]{calcCVage}}     \tab Calculate CV of ages based on age or length data;\cr
  \code{\link[PBStools]{calcMA}}        \tab Calculate a moving average using a fixed period occurring every \code{x} units;\cr
  \code{\link[PBStools]{compAF}}        \tab Compare age frequencies using discrete or cumulative distribution plots;\cr
  \code{\link[PBStools]{compBmsy}}      \tab Compare biomass posteriors relative to \code{Bmsy} or \code{Bavg};\cr
  \code{\link[PBStools]{compLen}}       \tab Compare annual length distributions among surveys series or commercial gears;\cr
  \code{\link[PBStools]{compVB}}        \tab Compare fitted von B curves using parameters;\cr
  \code{\link[PBStools]{compVBpars}}    \tab Compare MCMC derived parameters from von Bertalanffy model;\cr
  \code{\link[PBStools]{createMA}}      \tab Create table of DFO management actions for Appendix A (catch);\cr
  \code{\link[PBStools]{makeAgeErr}}    \tab Make an ageing error matrix for Awatea;\cr
  \code{\link[PBStools]{plotAgeErr}}    \tab Plot ageing precision data from primary and secondary readers;\cr
  \code{\link[PBStools]{plotBTMW}}      \tab Plot bottom (BT) vs. midwater (MW) trawl catch;\cr
  \code{\link[PBStools]{plotMW}}        \tab Plot annual mean weights by stock and by PMFC area;\cr
  \code{\link[PBStools]{plotSnail}}     \tab Plot snail-trail plots for MCMC analysis;\cr
  \code{\link[PBStools]{processBio}}    \tab Process \code{PBSdat} created by call to SQL query \code{gfb_bio.sql} to create biology object;\cr
  \code{\link[PBStools]{processMap}}    \tab Process \code{PBSdat} created by call to SQL query \code{fos_map_density.sql} to create a map object;\cr
  \code{\link[PBStools]{quantAges}}     \tab Plot quantile boxes of age by year and/or area, including mean age over time;\cr
  \code{\link[PBStools]{splineCPUE}}    \tab Fit spline curves through CPUE data to determine CV process error;\cr
  \code{\link[PBStools]{tabAmeth}}      \tab Tabulate ageing error structures available in bio123;\cr
  \code{\link[PBStools]{tabOtos}}       \tab Tabulate otoliths available and aged.\cr
  \tab \cr
  \bold{M07. Catch Recon} \tab \cr
  \code{\link[PBStools]{buildCatch}}    \tab Build a catch history of BC rockfish 1918--present;\cr
  \code{\link[PBStools]{plotDiag}}      \tab Plot diagnostic data for catch reconstructions;\cr
  \code{\link[PBStools]{plotGREFS}}     \tab Plot gamma for reference years by fishery;\cr
  \code{\link[PBStools]{plotRecon}}     \tab Plot reconstructed catch using barplots stacked by PMFC area;\cr
  \code{\link[PBStools]{quickCat}}      \tab Subsets a large dataset derived from query \code{fos_mcatORF.sql};\cr
  \code{\link[PBStools]{surveyCatch}}   \tab Query the survey catch data and make summary tables;\cr
  \code{\link[PBStools]{zapSeamounts}}  \tab Remove seamount records using combinations of major, minor, and locality codes.\cr
  \tab \cr
  \bold{M08. LaTeX}       \tab \cr
  \code{\link[PBStools]{collectFigs}}   \tab Collect postscript figures into one document;\cr
  \code{\link[PBStools]{formatCatch}}   \tab Format table of numeric catch as strings;\cr
  \code{\link[PBStools]{makeLTH}}       \tab Make a longtable header for Sweave;\cr
  \code{\link[PBStools]{splitTab}}      \tab Split a long data table into side-by-side pieces for printing in LaTeX;\cr
  \code{\link[PBStools]{texArray}}      \tab Flatten and format an array for latex output;\cr
  \code{\link[PBStools]{texThatVec}}    \tab Convert a vector to a phrase \sQuote{x, y, and z}.\cr
  \tab \cr
  \bold{M09. PJSsurvey}   \tab \cr
  \code{\link[PBStools]{calcBiom}}      \tab Calculate swept-area biomass estimate and bootstrap within strata;\cr
  \code{\link[PBStools]{doSynoptic}}    \tab Get the synoptic file for specified survey and go through PJS machinations;\cr
  \code{\link[PBStools]{getLabels}}     \tab Read in data file and get labels;\cr
  \code{\link[PBStools]{getLabelends}}  \tab Get depth ranges from stratum names (methinks);\cr
  \code{\link[PBStools]{getMonth}}      \tab Extract month from dates vector;\cr
  \code{\link[PBStools]{getSpecies}}    \tab Get species codes from data;\cr
  \code{\link[PBStools]{getUsability}}  \tab Qualify data by usability code;\cr
  \code{\link[PBStools]{getVessel}}     \tab Get survey vessel codes;\cr
  \code{\link[PBStools]{keepAtts}}      \tab Keep user's attributes after performing an expression such as subsetting a file;\cr
  \code{\link[PBStools]{plotIndex}}     \tab Plot survey index series after bootstrapping;\cr
  \code{\link[PBStools]{prepGFsurv}}    \tab Prepare GF surveys using PJS codes;\cr
  \code{\link[PBStools]{prepGIGhis}}    \tab Prepare GIG Historical (formerly called BG Reed) survey data;\cr
  \code{\link[PBStools]{prepHSass}}     \tab Prepare Hecate Strait assemblage survey data;\cr
  \code{\link[PBStools]{prepHSsyn}}     \tab Prepare Hecate Strait synoptic survey data;\cr
  \code{\link[PBStools]{prepQCSsyn}}    \tab Prepare QCS synoptic survey data;\cr
  \code{\link[PBStools]{prepNMFStri}}   \tab Prepare US National Marine Fisheries Service triennial survey data from WCVI;\cr
  \code{\link[PBStools]{prepWCHGsyn}}   \tab Prepare WCHG (formerly WCQCI) synoptic survey data;\cr
  \code{\link[PBStools]{prepWCVIsyn}}   \tab Prepare west coast Vancouver Island synoptic survey data;\cr
  \code{\link[PBStools]{restratify}}    \tab Restratify survey by depth;\cr
  \code{\link[PBStools]{uniqtows}}      \tab Ensure unique tows occur for each survey, collapse records as needed.\cr
  \tab \cr
  \bold{M10. Financial}  \tab \cr
  \code{\link[PBStools]{imputeRate}}    \tab \bold{GUI:} Impute the rate of return of an investment.\cr
  \tab \cr
  \bold{Z01. Borrow}   \tab \cr
  \code{\link[PBStools]{.ss3.readdat}}      \tab Read SS3 data files (\code{r4ss::SS_readdat_3.30}).\cr
  }

  Other package resources:
  \tabular{ll}{
  \code{../library/PBStools/doc}   \tab includes a User's Guide describing all functions.\cr
  \code{../library/PBStools/ADMB}  \tab ADMB batch files and project files (\code{pop}, \code{vonB}).\cr
  \code{../library/PBStools/sql}   \tab SQL code files for querying remote databases.\cr
  \code{../library/PBStools/win}   \tab \pkg{PBSmodelling} \emph{window description files} for GUIs.
  }
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-26}\cr
}
\note{
  The working list object \code{PBStool} is stored in the 
  temporary working environment \code{.PBStoolEnv} (see \code{ttget} 
  functions to access the object).
}
\seealso{
  \code{\link[PBStools]{getData}}, \code{\link[PBStools]{ttget}} \cr
  \code{\link[PBStools]{ServerParlance}} for differences between SQL Server and Oracle database organisation.\cr
  \code{\link[PBStools]{SQLcode}} for available SQL queries.\cr
}
\keyword{package}
\concept{R_code}
