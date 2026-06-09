\name{PBStools-package}
\alias{PBStools-package}
\alias{PBStools}
\docType{package}

\title{PBStools -- Tools to Tackle Tricky Tasks}

\description{
  \pkg{PBStools} provides tools for stock assessments, 
  species-at-risk requirements, and stakeholder requests. 
  The package depends on four other R packages: \pkg{PBSmapping}, 
  \pkg{PBSmodelling}, \pkg{PBSdata}, and \pkg{RODBC}. 

  \pkg{PBStools} contains the following R-code modules:
  \tabular{cll}{
    \bold{M01} \tab Utility    \tab  utility functions\cr
    \bold{M02} \tab Biology    \tab  functions for biological processes\cr
    \bold{M03} \tab Fishery    \tab  functions for fisheries-related actions\cr
    \bold{M04} \tab Survey     \tab  functions for surveys-related actions\cr
    \bold{M05} \tab Spatial    \tab  spatial functions\cr
    \bold{M06} \tab Assessment \tab  functions for fisheries stock assessment\cr
    \bold{M07} \tab CatchRecon \tab  functions for rockfish catch reconstruction\cr
    \bold{M08} \tab LaTeX      \tab  functions to facilitate LaTeX integration\cr
    \bold{Z01} \tab Borrow     \tab  functions `borrowed' (taken) from other packages\cr
  }
  \pkg{PBStools} contains the following functions in alphabetical order:
  \tabular{lcl}{
    \bold{Function}  \tab \bold{Module}  \tab \bold{Description}\cr
    \code{\link[PBStools]{addStrip}}      \tab M01 \tab add a vertical color strip as a legend\cr
    \code{\link[PBStools]{adjustMajor}}   \tab M01 \tab adjust PMFC areas for POP|YMR for GMU\cr
    \code{\link[PBStools]{biteData}}      \tab M01 \tab subset a data matrix/frame using a vector object\cr
    \code{\link[PBStools]{bootBG}}        \tab M04 \tab bootstrap biomass using binomial-gamma population parameters\cr
    \code{\link[PBStools]{buildCatch}}    \tab M07 \tab build a catch history of BC rockfish 1918--present\cr
    \code{\link[PBStools]{calcCVage}}     \tab M06 \tab calculate CV of ages based on age or length data\cr
    \code{\link[PBStools]{calcHabitat}}   \tab M05 \tab calculate potential habitat using bathymetry limits\cr
    \code{\link[PBStools]{calcHBLL}}      \tab M04 \tab calculate swept-area biomass estimates for HBLL survey\cr
    \code{\link[PBStools]{calcHM}}        \tab M01 \tab calculate the harmonic mean of a vector of numbers\cr
    \code{\link[PBStools]{calcLW}}        \tab M02 \tab calculate length-weight relationship for a fish\cr
    \code{\link[PBStools]{calcMA}}        \tab M06 \tab calculate a moving average using a fixed period occurring every \code{x} units\cr
    \code{\link[PBStools]{calcMoments}}   \tab M04 \tab calculate survey moments from raw data\cr
    \code{\link[PBStools]{calcOccur}}     \tab M05 \tab calculate percent occurrence of EventData in PolySet\cr
    \code{\link[PBStools]{calcPMR}}       \tab M04 \tab calculate \eqn{(p,\mu,\rho)} from a sample population\cr
    \code{\link[PBStools]{calcRatio}}     \tab M03 \tab calculates ratios of numerator to denominator (e.g., discard/catch)\cr
    \code{\link[PBStools]{calcSG}}        \tab M02 \tab calculate growth curve using Schnute growth model\cr
    \code{\link[PBStools]{calcSRFA}}      \tab M05 \tab determine slope rockfish assessment areas or subareas\cr
    \code{\link[PBStools]{calcStockArea}} \tab M05 \tab assign stock area based on species code and PMFC areas\cr
    \code{\link[PBStools]{calcSurficial}} \tab M05 \tab calculate intersection of surficial geology and bathymetry habitat\cr
    \code{\link[PBStools]{calcVB}}        \tab M02 \tab calculate fits using a von Bertalanffy growth model\cr
    \code{\link[PBStools]{calcWAParea}}   \tab M05 \tab assign stock area for Walleye Pollock using PMFC areas\cr
    \code{\link[PBStools]{changeLangOpts}}\tab M01 \tab change the options that control the display of numbers based on language\cr
    \code{\link[PBStools]{chewData}}      \tab M01 \tab remove sparse category records from data matrix/frame\cr
    \code{\link[PBStools]{clarify}}       \tab M05 \tab summarize catch data into CLARA clusters\cr
    \code{\link[PBStools]{clearFiles}}    \tab M01 \tab check to see if file exists and keep a copy if it does before removing it\cr
    \code{\link[PBStools]{coalesce}}      \tab M01 \tab an R equivalent to SQL's coalesce\cr
    \code{\link[PBStools]{collateGBSP}}   \tab M03 \tab collate biological data collected by Groundfish Biological Sampling Program\cr
    \code{\link[PBStools]{collectFigs}}   \tab M08 \tab collect postscript figures into one document\cr
    \code{\link[PBStools]{compAF}}        \tab M06 \tab compare age frequencies using discrete or cumulative distribution plots\cr
    \code{\link[PBStools]{compBmsy}}      \tab M06 \tab compare biomass posteriors relative to \code{Bmsy} or \code{Bavg}\cr
    \code{\link[PBStools]{compCsum}}      \tab M02 \tab compare cumulative frequencies (e.g., of ages)\cr
    \code{\link[PBStools]{compLen}}       \tab M06 \tab compare annual length distributions among surveys series or commercial gears\cr
    \code{\link[PBStools]{compVBpars}}    \tab M06 \tab compare MCMC derived parameters from von Bertalanffy model\cr
    \code{\link[PBStools]{compVB}}        \tab M06 \tab compare fitted von B curves using parameters\cr
    \code{\link[PBStools]{confODBC}}      \tab M01 \tab configure an ODBC user data source name (DSN)\cr
    \code{\link[PBStools]{convCT}}        \tab M01 \tab convert a crossTab object to regular matrix or data frame\cr
    \code{\link[PBStools]{convFY}}        \tab M01 \tab convert dates into fishing/fiscal years\cr
    \code{\link[PBStools]{convUTF}}       \tab M01 \tab convert UTF-8 strings into Unicode characters\cr
    \code{\link[PBStools]{convYM}}        \tab M01 \tab convert date limits into expanded year-months\cr
    \code{\link[PBStools]{convYP}}        \tab M01 \tab convert dates into binned year periods\cr
    \code{\link[PBStools]{countLines}}    \tab M01 \tab count the number of lines in an ASCII file\cr
    \code{\link[PBStools]{countVec}}      \tab M01 \tab count number of definite vector elements that exclude zero values\cr
    \code{\link[PBStools]{createDSN}}     \tab M01 \tab create user DSNs for PBS groundfish databases\cr
    \code{\link[PBStools]{createFdir}}    \tab M01 \tab create a subdirectory called `french' for storing figures with French text and labels\cr
    \code{\link[PBStools]{createMA}}      \tab M06 \tab create table of DFO management actions for Appendix A (catch)\cr
    \code{\link[PBStools]{createTdir}}    \tab M01 \tab create subdirectories called `tables' and `data'\cr
    \code{\link[PBStools]{crossTab}}      \tab M01 \tab summarise values from a data table using one or more fields in the table\cr
    \code{\link[PBStools]{darkenRGB}}     \tab M01 \tab programmatically darken the colour of given RGB values\cr
    \code{\link[PBStools]{dtget}}         \tab M01 \tab get | call | list | print | put objects into temp work envir (\code{.PBSdataEnv})\cr
    \code{\link[PBStools]{dumpMod}}       \tab M03 \tab dump catch from modern sources used in catch reconstruction\cr
    \code{\link[PBStools]{dumpRat}}       \tab M03 \tab dump catch ratios calculated by a catch reconstruction\cr
    \code{\link[PBStools]{estOgive}}      \tab M02 \tab estimate ogive curves for maturity\cr
    \code{\link[PBStools]{expand5C}}      \tab M01 \tab transfer events from Moresby Gully in 5B and Anthony Is. in 5E to PMFC 5C\cr
    \code{\link[PBStools]{extractAges}}   \tab M02 \tab extract records with positive age and qualify by the selected ageing method\cr
    \code{\link[PBStools]{extractPost}}   \tab M01 \tab extract model posteriors from three model platforms (Awatea, SS3, iSCAM)\cr
    \code{\link[PBStools]{findEP}}        \tab M05 \tab find events in polys and and add poly info to events\cr
    \code{\link[PBStools]{findHoles}}     \tab M05 \tab find holes and place them under correct parents\cr
    \code{\link[PBStools]{findPV}}        \tab M01 \tab find nearest position in vector choice using a target point\cr
    \code{\link[PBStools]{findRC}}        \tab M01 \tab return (nrow, ncol) to set up multi-panel figure on one page\cr
    \code{\link[PBStools]{fitLogit}}      \tab M01 \tab fit binomial data using logit link function\cr
    \code{\link[PBStools]{flagIt}}        \tab M01 \tab label a coordinate \eqn{(a,b)} using a diagonal line\cr
    \code{\link[PBStools]{formatCatch}}   \tab M08 \tab format table of numeric catch as strings\cr
    \code{\link[PBStools]{gatherVals}}    \tab M01 \tab gather data from multiple columns into key-value pairs (cf. \code{tidyr::gather})\cr
    \code{\link[PBStools]{genPa}}         \tab M02 \tab generate proportions-at-age using catch curve composition\cr
    \code{\link[PBStools]{getBootRuns}}   \tab M04 \tab get Norm's survey bootstrap results\cr
    \code{\link[PBStools]{getCatch}}      \tab M03 \tab get catch records for a species from various databases and combine\cr
    \code{\link[PBStools]{getData}}       \tab M01 \tab get data from a variety of sources\cr
    \code{\link[PBStools]{getFile}}       \tab M01 \tab get data from binaries or ASCII\cr
    \code{\link[PBStools]{getGreek}}      \tab M01 \tab get Greek letters using unicode characters\cr
    \code{\link[PBStools]{getName}}       \tab M01 \tab get string names from literals or named objects\cr
    \code{\link[PBStools]{getODBC}}       \tab M01 \tab get a string vector of ODBC drivers on user's Windows system\cr
    \code{\link[PBStools]{getPMR}}        \tab M04 \tab get \eqn{(p,\mu,\rho)} values for survey strata\cr
    \code{\link[PBStools]{getPrey}}       \tab M02 \tab get prey items identified in stomach contents of fish\cr
    \code{\link[PBStools]{getVer}}        \tab M01 \tab get version of loaded|installed R packages in user's R environment\cr
    \code{\link[PBStools]{glimmer}}       \tab M03 \tab perform a standardised GLM analysis\cr
    \code{\link[PBStools]{histMetric}}    \tab M02 \tab plot a matrix of histograms for a specified metric\cr
    \code{\link[PBStools]{histTail}}      \tab M02 \tab plot histogram with optional tail zoom\cr
    \code{\link[PBStools]{installPkgs}}   \tab M01 \tab install specified packages if they are missing or if newer versions are available\cr
    \code{\link[PBStools]{inWord}}        \tab M01 \tab find morphemes (parts of words) in word and report T/F\cr
    \code{\link[PBStools]{isThere}}       \tab M01 \tab checks whether an object is physically present in a specified environment\cr
    \code{\link[PBStools]{lenv}}          \tab M01 \tab get the local (lenv), parent (penv), or global (genv) environment\cr
    \code{\link[PBStools]{linguaFranca}}  \tab M01 \tab translate English phrases to French in figures (see also eop|fop)\cr
    \code{\link[PBStools]{listTables}}    \tab M01 \tab list tables in specified SQL/ORA database\cr
    \code{\link[PBStools]{makeAgeErr}}    \tab M06 \tab make an ageing error matrix for Awatea\cr
    \code{\link[PBStools]{makeCATtables}} \tab M03 \tab make catch tables from commercial fishery data\cr
    \code{\link[PBStools]{makeLTH}}       \tab M08 \tab make a longtable header for Sweave\cr
    \code{\link[PBStools]{makePMRtables}} \tab M04 \tab make \eqn{(p,\mu,\rho)} tables for surveys\cr
    \code{\link[PBStools]{makeSSID}}      \tab M04 \tab make a data object of survey series information\cr
    \code{\link[PBStools]{mapMaturity}}   \tab M02 \tab map species maturity by month\cr
    \code{\link[PBStools]{plotAdens}}     \tab M02 \tab plot density of field (length) by age as waveforms\cr
    \code{\link[PBStools]{plotAgeErr}}    \tab M06 \tab plot ageing precision data from primary and secondary readers\cr
    \code{\link[PBStools]{plotBTMW}}      \tab M06 \tab plot bottom (BT) vs. midwater (MW) trawl catch\cr
    \code{\link[PBStools]{plotCatch}}     \tab M03 \tab plot catch history as an annual barplot\cr
    \code{\link[PBStools]{plotConcur}}    \tab M05 \tab plot concurrent species in tows at depth\cr
    \code{\link[PBStools]{plotDiag}}      \tab M07 \tab plot diagnostic data for catch reconstructions\cr
    \code{\link[PBStools]{plotEO}}        \tab M05 \tab plot Extent of Occurrence for a species using a convex hull\cr
    \code{\link[PBStools]{plotFOScatch}}  \tab M03 \tab plot monthly catch from FOS as barplots\cr
    \code{\link[PBStools]{plotGMA}}       \tab M05 \tab plot the Groundfish Management Areas\cr
    \code{\link[PBStools]{plotGREFS}}     \tab M07 \tab plot gamma for reference years by fishery\cr
    \code{\link[PBStools]{plotHabitat}}   \tab M05 \tab plot potential habitat using bathymetry output from \code{calcHabitat}\cr
    \code{\link[PBStools]{plotHBLL}}      \tab M04 \tab plot locations of the longline sets used in the HBLL surveys\cr
    \code{\link[PBStools]{plotLocal}}     \tab M05 \tab plot DFO fishing localities with the highest catch\cr
    \code{\link[PBStools]{plotMW}}        \tab M06 \tab plot annual mean weights by stock and by PMFC area\cr
    \code{\link[PBStools]{plotProp}}      \tab M02 \tab \bold{GUI:} Create bubble plots showing proportions-at-age etc\cr
    \code{\link[PBStools]{plotRecon}}     \tab M07 \tab plot reconstructed catch using barplots stacked by PMFC area\cr
    \code{\link[PBStools]{plotSnail}}     \tab M06 \tab plot snail-trail plots for MCMC analysis\cr
    \code{\link[PBStools]{plotTernary}}   \tab M05 \tab plot a ternary diagram for data amalgamated into 3 groups\cr
    \code{\link[PBStools]{plotTertiary}}  \tab M05 \tab composition plots within a polygonal space\cr
    \code{\link[PBStools]{predictRER}}    \tab M02 \tab predict Rougheye Rockfish from biological data\cr
    \code{\link[PBStools]{preferDepth}}   \tab M05 \tab \bold{GUI:} Plot depth distribution of species\cr
    \code{\link[PBStools]{prepClara}}     \tab M05 \tab prepare a data object for Clustering Large Applications\cr
    \code{\link[PBStools]{prime}}         \tab M01 \tab report the prime numbers given an integer vector\cr
    \code{\link[PBStools]{processBio}}    \tab M06 \tab process \code{PBSdat} created by call to SQL query \code{gfb_bio.sql}\cr
    \code{\link[PBStools]{processMap}}    \tab M06 \tab process \code{PBSdat} created by call to SQL query \code{fos_map_density.sql}\cr
    \code{\link[PBStools]{quantAges}}     \tab M06 \tab plot quantile boxes of age by year and/or area, including mean age over time\cr
    \code{\link[PBStools]{quantBox}}      \tab M01 \tab redefine boxplot to show quantiles\cr
    \code{\link[PBStools]{quickCat}}      \tab M07 \tab subsets a large dataset derived from query \code{fos_mcatORF.sql}\cr
    \code{\link[PBStools]{readClog}}      \tab M01 \tab read a \code{ChangeLog} file and convert it to an R list\cr
    \code{\link[PBStools]{reportCatchAge}}\tab M02 \tab \bold{GUI:} Plot results from Jon Schnute's catch-at-age ADMB report file\cr
    \code{\link[PBStools]{requestAges}}   \tab M02 \tab determine which otoliths to sample for ageing requests\cr
    \code{\link[PBStools]{residVB}}       \tab M02 \tab calculate and plot Studentised residuals as quantile plots\cr
    \code{\link[PBStools]{revStr}}        \tab M01 \tab reverse a string set\cr
    \code{\link[PBStools]{runCCA}}        \tab M03 \tab catch-curve model based on Schnute and Haigh (2006)\cr
    \code{\link[PBStools]{runModules}}    \tab M01 \tab run GUI modules included in PBS Fishery\cr
    \code{\link[PBStools]{sampBG}}        \tab M04 \tab sample from the binomial-gamma distribution\cr
    \code{\link[PBStools]{scaleVec}}      \tab M01 \tab scale a vector to span a target minimum and maximum\cr
    \code{\link[PBStools]{showAlpha}}     \tab M04 \tab show quantile confidence levels (\eqn{\alpha}) for bootstraps\cr
    \code{\link[PBStools]{showError}}     \tab M01 \tab display an error message on the current device\cr
    \code{\link[PBStools]{showIndices}}   \tab M04 \tab show survey indices from bootstrap tables\cr
    \code{\link[PBStools]{showMessage}}   \tab M01 \tab display an information message on the current device\cr
    \code{\link[PBStools]{simBGtrend}}    \tab M04 \tab simulate population projection based on prior binomial-gamma parameters\cr
    \code{\link[PBStools]{simBSR}}        \tab M02 \tab simulate Blackspotted Rockfish biological data\cr
    \code{\link[PBStools]{simRER}}        \tab M02 \tab simulate Rougheye Rockfish biological data\cr
    \code{\link[PBStools]{smoothPDO}}     \tab M07 \tab smooth environmental indices, often expressed as anomalies (e.g. PDO)\cr.
    \code{\link[PBStools]{splineCPUE}}    \tab M06 \tab fit spline curves through CPUE data to determine CV process error\cr
    \code{\link[PBStools]{splitTab}}      \tab M08 \tab split a long data table into side-by-side pieces for printing in \LaTeX\cr
    \code{\link[PBStools]{spooler}}       \tab M01 \tab spool information specified by a list into a new field of an existing data frame\cr
    \code{\link[PBStools]{stdConc}}       \tab M01 \tab standardise a chemical concentration\cr
    \code{\link[PBStools]{subsetFile}}    \tab M01 \tab subset an ASCII file every \emph{n} rows\cr
    \code{\link[PBStools]{sumBioTabs}}    \tab M02 \tab summarize frequency occurrence of biological samples\cr
    \code{\link[PBStools]{sumCatTabs}}    \tab M03 \tab summarize catch by year & PMFC from modern catch data (used in RCR)\cr
    \code{\link[PBStools]{surveyCatch}}   \tab M07 \tab query the survey catch data and make summary tables\cr
    \code{\link[PBStools]{tabAmeth}}      \tab M06 \tab tabulate ageing error structures available in bio123\cr
    \code{\link[PBStools]{tabMW}}         \tab M02 \tab tabulate mean weights (or some other morphometric)\cr
    \code{\link[PBStools]{tabOtos}}       \tab M06 \tab tabulate otoliths available and aged\cr
    \code{\link[PBStools]{tabSFW}}        \tab M03 \tab tabulate BC commercial fishery catch for Seafood Watch\cr
    \code{\link[PBStools]{testPch}}       \tab M01 \tab display plotting symbols or octal strings\cr
    \code{\link[PBStools]{texArray}}      \tab M08 \tab flatten and format an array for latex output\cr
    \code{\link[PBStools]{texThatVec}}    \tab M08 \tab convert a vector to a phrase \sQuote{x, y, and z}\cr
    \code{\link[PBStools]{texWrap}}       \tab M08 \tab wrap a TeX blob to enable its compilation for viewing\cr
    \code{\link[PBStools]{toUpper}}       \tab M01 \tab capitalise first letters of words\cr
    \code{\link[PBStools]{trackBycat}}    \tab M03 \tab track annual fish group catches between depth limits\cr
    \code{\link[PBStools]{trend}}         \tab M04 \tab \bold{GUI:} Create boxplots of annual survey data, trend lines, and bootstraps\cr
    \code{\link[PBStools]{ttget}}         \tab M01 \tab get | call | list | print | put objects into temp work envir (\code{.PBStoolEnv})\cr
    \code{\link[PBStools]{weightBio}}     \tab M02 \tab weight age/length frequencies/proportions by catch\cr
    \code{\link[PBStools]{wrapText}}      \tab M01 \tab wrap, indent, and prefix a long text string\cr
    \code{\link[PBStools]{zapDupes}}      \tab M01 \tab remove records with duplicated indices from a data frame\cr
    \code{\link[PBStools]{zapHoles}}      \tab M05 \tab zap (remove) holes from polygons\cr
    \code{\link[PBStools]{zapSeamounts}}  \tab M07 \tab remove seamount records using major, minor, and locality codes\cr
    \code{\link[PBStools]{.ss3.readdat}}  \tab Z01 \tab read SS3 data files (\code{r4ss::SS_readdat_3.30})\cr
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
  \emph{locus opus}: remote office, Vancouver BC\cr
  Last modified \code{Rd: 2026-06-04}\cr
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
\concept{modules}


