##==============================================================================
## Module 1: Utility
## -----------------
##  addStrip........Add a vertical colour strip as a legend.
##  biteData........Subsets a data matrix/frame using input vector.
##  calcHM..........Calculate the harmonic mean of a vector of numbers.
##  changeLangOpts..Change the options that control the display of numbers based on language.
##  chewData........Remove records that contribute little information to factor categories.
##  clearFiles......Check to see if file exists and keep a copy if it does before removing it.
##  confODBC........Set up an ODBC User Data Source Name (DSN).
##  convCT..........Convert a crossTab object to regular matrix or data frame.
##  convFY..........Convert dates into fishing years.
##  convUTF.........Convert UTF-8 strings into Unicode characters.
##  convYM..........Convert date limits into a vector of year-months (YYY-MM).
##  convYP..........Convert dates into year periods.
##  countLines......Count the number of lines in an ASCII file.
##  countVec........Count number of definite vector elements (non NA) that exclude (or include) zero values.
##  createDSN.......Create entire suite of DSNs for the groundfish databases.
##  createFdir......Create a subdirectory called `french' for storing figures with French text and labels.
##  crossTab........Summarise values from a data table using one or more fields in the table.
##  darkenRGB.......Programmatically darken the colour of given RGB values.
##  expand5C........Transfer events from Moresby Gully in 5B and Flamingo Inlet/Anthony Island in 5E to PMFC 5C.
##  extractPost.....Extract model posteriors from three model platforms (Awatea, SS3, iSCAM) for clients.
##  findPV..........Find nearest position in vector choice using a target point.
##  findRC..........Return no. (rows, columns) for multi-panel figures given no. figures to fit on one page.
##  fitLogit........Fit binomial data using logit link function.
##  flagIt..........Labels a coordinate using a diagonal line radiating from it.
##  gatherVals......Gathers data from multiple columns into key-value pairs (replaces tidyr::gather).
##  getData.........Get data from a variety of sources (mostly ODBC)
##  getFile.........Get a dataset (binary libraries, binary local, dumped data, comma-delimited text.
##  getName.........Get the names of the input object.
##  getODBC.........Get a string vector of ODBC drivers on user's Windows system.
##  installPkgs.....Install specified packages if they are missing or if newer versions are available..
##  inWord..........Find morphemes (parts of words) in word and report T/F
##  isThere.........Check to see if object physically exists in the specified environment.
##  lenv............Get the local/parent/global environment.
##  linguaFranca....Translate English phrases to French in figures.
##  listTables......List tables in specified SQL, ORA, or MDB database.
##  prime...........Report the prime numbers given an integer vector.
##  quantBox........Redefine boxplot to show quantiles.
##  readClog........Read a ChangeLog file and convert it to an R list.
##  revStr..........Reverse characters in a string.
##  runModules......Display a master GUI to display modules.
##  scaleVec........Scales a vector to span a target minimum and maximum.
##  showError.......Display error message on device surface.
##  spooler.........Spools list objects into fields of data frames.
##  stdConc.........Standardise a chemical concentration.
##  subsetFile......Subset an ASCII file every n rows (enrow).
##  testPch.........Display plotting symbols or octal strings.
##  toUpper.........Capitalise first letter of each word in phrase
##  ttget...........Provide wrappers for PBSmodelling functions tget/tcall/tprint/tput/lisp.
##  wrapText........Wrap, mark and indent a long text string.
##  zapDupes........Delete duplicated records based on specified index.
##
##-----Supplementary hidden functions-----
##  .chooseFQT......Choose a file/query/table from the user-specified path.
##  .flush.cat......Flush the cat down the console.
##  .grabPath.......Return the specified directory in the package tree.
##  .getApath.......Return the path for admb/examples/sql/win directories in PBStools.
##  .setApath.......Set the path for admb/examples/sql/win directories in PBStools.
##  .plotDev........Save plot on current devise using values from a GUI, if available.
##  .setCWD.........Return the current working directory and if win=TRUE, set the path variable in the active window.
##  .su.............Shortcut for sort(unique(x))
##===============================================================================

## NO: 2018-10-23 -- The SSC Networks team is working to change the network subnet for Nanaimo PBS site (Greg Remillard, SSC Windows Server team).
##                   Current GF server assigned a new IP address on the network to move it to the new subnet.
## RH: 2016-11-28 -- DFO phased out Windows 2003 servers; new server supports SQL Server 2008 and 2016
## RH: 2015-11-30 -- Virtualization of SVBCPBSGFIIS
.PBSserver = c(
  GFDB="10.114.52.8",
  DFBCV9TWVASP001="10.114.52.8",
  SVBCPBSGFIIS="199.60.94.98",
  PACPBSGFDB="199.60.95.200",
  GFDBtemp="PAC03450/GFDB",
  oldSVBCPBSGFIIS="199.60.95.134",
  oldDFBCV9TWVASP001="199.60.94.30"
)
.rgbBlind   = list(black=c(0,0,0),orange=c(230,159,0),skyblue=c(86,180,233),bluegreen=c(0,158,115),
	yellow=c(240,228,66),blue=c(0,114,178),vermillion=c(213,94,0),redpurple=c(204,121,167), white=c(255,255,255))
.colBlind   = sapply(.rgbBlind,function(x){rgb(x[1],x[2],x[3],maxColorValue=255)})
.colGnuplot = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628","#f781bf")


## addStrip-----------------------------2020-10-06
## Add a vertical colour strip as a legend.
## ---------------------------------------------RH
addStrip = function (x, y, col, lab, xwidth=0.01, yheight=0.3, ...) 
{
	#if (dev.cur()>1) { oldpar=par(no.readonly=TRUE); on.exit(par(oldpar)) }  ## screws up par()$mfg
	if (dev.cur()>1) { usr=par()$usr; on.exit(par(usr=usr)) }
	fenv = lenv()
	dots = list(...); unpackList(dots)
	if (!exists("border", envir=fenv)) border = "gainsboro"
	if (!exists("cex.txt", envir=fenv)) cex.txt = 0.9
	uxy <- par()$usr
	x1 <- uxy[1];  x2 <- uxy[2]
	y1 <- uxy[3];  y2 <- uxy[4]
	x0 <- x1 + x * (x2 - x1)
	y0 <- y1 + y * (y2 - y1)
	xw0 = xwidth * (x2-x1)
	yh0 = yheight * (y2-y1)
	if (par()$xlog){
		x0 <- 10^x0; xw0 = 10^xw0 }
	if (par()$ylog){
		y0 <- 10^y0; yh0 = 10^yh0 }
	xval = x0 + c(0,xw0)
	yval = seq(y0-yh0,y0,len=length(col)+1)
	ncol = length(col)
	xpol = c(x0,x0,xval[2],xval[2],NA)
	xpol = rep(xpol,ncol)
	ypol = numeric()
	for (i in 1:ncol)
		ypol = c(ypol, c(yval[i],rep(yval[i+1],2),yval[i],NA))
	polygon(xpol,ypol,border=border,col=col)
	text(xval[2]+0.25*xw0, yval[1:ncol]+diff(yval)/2,labels=lab,cex=cex.txt,adj=0)
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~addStrip


## biteData-----------------------------2008-11-10
## Subsets a data matrix/frame using input vector.
## ---------------------------------------------RH
biteData = function(dat,vec) {
	if (nrow(dat)==0 || is.null(vec)) return(dat)
	fld=as.character(substitute(vec))
	if (!any(fld==dimnames(dat)[[2]])) return(dat)
	expr=paste("bdat=dat[is.element(dat[,\"",fld,"\"],",deparse(unique(unlist(vec))),"),]",sep="")
	eval(parse(text=expr))
	return(bdat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~biteData


## calcHM-------------------------------2020-07-13
##  Calculate the harmonic mean of a vector of numbers.
##  Return the harmonic mean (function similar to calcGM).
##  Arguments:
##   x      -- Vector of numbers
##   offset -- Added value to validate zeroes
##   exzero -- If TRUE, exclude zeroes
## ---------------------------------------------RH
calcHM <- function (x, offset=0, exzero=TRUE) {
	x <- x[!is.na(x)]
	if (exzero) 
		x <- x[x > 0 & !is.na(x)]
	N <- length(x)
	if (N == 0) 
		return(0)
	x <- x + offset
	h <- N / sum(1/x)##; print(h)

	## https://stats.stackexchange.com/questions/7471/can-the-standard-deviation-be-calculated-for-harmonic-mean
	## accepted answer by mpiktas (but seems low when x values are high)
	sdhm1 = sqrt((mean(1/x))^(-4)*var(1/x)/length(x))
	## informal SD by Gil Wolff
	sdhm2 = sqrt( N/sum(((1/h)-(1/x))^2) )
	attr(h,"sd") = list (sdhm1=sdhm1, sdhm2=sdhm2)
#browser();return()
	return(h)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~calcHM


## changeLangOpts-----------------------2019-06-10
##  Change the options that control the display of
##  numbers based on language.
## ---------------------------------------------RH
changeLangOpts = function(L="e", stringsAsFactors=FALSE)
{
	## Assume defaults are english
	Eopts = list(OutDec=".", big.mark=",", stringsAsFactors=stringsAsFactors)
	Fopts = list(OutDec=",", big.mark=" ", stringsAsFactors=stringsAsFactors)
	tput(Eopts) ## store defaults in .PBSmodEnv
	options(switch(L, 'e'=Eopts, 'f'=Fopts))
#browser();return()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~changeLangOpts
eop = function(){changeLangOpts(L="e")}  ## create short command for switching to english options
fop = function(){changeLangOpts(L="f")}  ## create short command for switching to french  options


## chewData-----------------------------2009-01-13
## Remove records that contribute little information to factor categories.
## ---------------------------------------------RH
chewData=function(dat,fac,nmin=3,na.rm=TRUE) {
	if (nrow(dat) == 0 || is.null(fac)) return(dat)
	fld = as.character(substitute(fac))
	if (!any(fld == dimnames(dat)[[2]])) return(dat)
	ldat=split(dat[,fac],dat[,fac])
	ndat=sapply(ldat,function(x){length(x[!is.na(x)])})
	ndat=ndat[ndat>=nmin & !is.na(ndat)] ## get rid of factors with few occurrences
	if (na.rm) ndat=ndat[names(ndat)!=""]
	dat=dat[is.element(dat[,fac],names(ndat)),]
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~chewData


## clearFiles---------------------------2021-03-03
##  Check to see if file exists and keep a copy if it does before removing it.
## ---------------------------------------------RH
clearFiles = function(x, archive=TRUE, archdir="./archive", short=TRUE) {
	rubbish = 0
	for (i in x) {
		ii = basename(i)
		iii = strsplit(i,"/")[[1]]
		if (length(iii)==1) pp = "."
		else pp = paste0(iii[-length(iii)], collapse="/")
		if (file.exists(i)) {
			if (short)
				fstamp = gsub("[[:punct:]]","",substring(file.info(i)$mtime,3,10))
			else
				fstamp = paste0(sub("[[:space:]]","(",gsub("[[:punct:]]","",substring(file.info(i)$mtime,3,16))),")")
			ext    = tools::file_ext(ii)
			pre    = gsub(paste0("\\.",ext,"$"),"",ii)
			if (is.null(archdir) || is.na(archdir) || archdir %in% c("","."))
				arcdir = getwd()
			else
				arcdir = paste0(pp,"/",sub("./","",archdir))  ## compounds if overwrite 'archdir'
			adir   = paste0(sub("/$","",arcdir),"/")
			## Check to see if relative archive directory should be placed under relative file directory
			if (substring(adir,1,1)=="." && substring(pre,1,1)==".") {
				shards = strsplit(pre,"/")[[1]]
				adir   = paste0(paste0(shards[-length(shards)],collapse="/"),substring(adir,2))
				pre    = rev(shards)[1]
			}
#if (i==x[2]) {browser();return()}
			if (!dir.exists(adir))
				dir.create(adir)
			backup = paste0(adir, pre,"-",fstamp,".",ext)
#browser();return()
			if (!file.exists(backup))
				file.copy(from=i, to=backup, overwrite=FALSE, copy.date=TRUE)
			file.remove(i)
			rubbish = rubbish + 1
		}
	}
	invisible(paste0(rubbish, " files removed"))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~clearFiles


## coalesce-----------------------------2019-07-25
##  Emulate SQL's function 'COALESCE':
##  Accept a list of vectors of identical length and
##  return one vector with the first non-NA value.
## Author: Eric Vallabh Minikel (May 2, 2013)
## Source: https://www.cureffi.org/2013/05/02/r-equivalent-of-sql-coalesce/
## --------------------------------------------EVM
coalesce = function(...)
{
	## convert input arguments into a list of vectors
	input_list = list(...)
	# check that all input vectors are of same length
	vectorlength = length(input_list[[1]])
	for (j in 1:length(input_list)) {
		if(length(input_list[[j]]) != vectorlength) {
			stop(paste("Not all vectors are of same length. First vector length: ",vectorlength,". Vector #",j,"'s length: ",length(input_list[[j]]),sep=""))
		}
	}
	## create a result vector to fill with first non-NA values
	result = rep(NA,vectorlength)
	## fill with first non-NA value
	for (i in 1:length(result)) {
		for (j in 1:length(input_list)) {
			if(!is.na(input_list[[j]][i])) {
				result[i] = input_list[[j]][i]
				break
			}
		}
	}
	attr(result,"author") = "Eric Vallabh Minikel (May 2, 2013)"
	attr(result,"source") = "https://www.cureffi.org/2013/05/02/r-equivalent-of-sql-coalesce/"
	return(result)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~coalesce


## confODBC-----------------------------2010-06-02
## Set up an ODBC User Data Source Name (DSN)
## ---------------------------------------------RH
confODBC <- function(dsn="PacHarvest",server="GFDB",db="PacHarvest",
   driver="SQL Server",descr="",trusted=TRUE)
{
	## use forward slashes "/" for server otherwise the translation
	## is too dependent on the number of times "\" is escaped
	## getFile(".PBSserver",path=.getSpath(),tenv=penv())
	if (is.element(server,names(.PBSserver))) server <- .PBSserver[server]
	syntax <- paste("{CONFIGDSN \"",driver,"\" \"DSN=",dsn,
		"|Description=",descr,"|SERVER=",server,"|Trusted_Connection=",
		ifelse(trusted,"Yes","No"),"|Database=",db,"\"}",sep="")
	syntax=gsub("/","\\\\",syntax) # finally convert "/" to "\\"
	cmd <- paste("odbcconf.exe /a",syntax)
	system(cmd)
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~confODBC


## convCT-------------------------------2014-12-12
## Convert a crossTab object to regular matrix or data frame.
## Note: No longer necessary as crossTab does not use reshape.
## ---------------------------------------------RH
convCT = function(CT, fn=as.matrix, colAsRowName=TRUE)
{
	fnam = as.character(substitute(fn))
	if (!is.element(fnam,c("as.matrix","as.data.frame"))) return(CT)
	NT = fn(CT[,-1])
	class(NT) = sub("as\\.","",fnam)
	if (colAsRowName) dimnames(NT) = list(CT[,1],dimnames(CT)[[2]][-1])
	else  dimnames(NT) = list(dimnames(CT)[[1]],dimnames(CT)[[2]][-1])
	return(NT)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~convCT


## convFY-------------------------------2023-02-09
## Convert dates into fishing years.
## ---------------------------------------------RH
convFY = function(x,startM=4)
{
	#if (class(x)[1]=="character" && class(try(as.Date(x),silent=TRUE))=="Date" ) 
	if (inherits(x, "character") && inherits(try(as.Date(x),silent=TRUE), "Date") )
		x = as.Date(x)
	if (any(class(x)%in%c("POSIXct","POSIXt","Date")))
		cdate = format(x) # 10-character date "yyyy-mm-dd"
	else  return(rep("",length(x)))
	yrmo=substring(x,1,7) # year-month "yyyy-mm"
	yr=as.numeric(substring(yrmo,1,4)); mo=as.numeric(substring(yrmo,6,7))
	fyr=yr; sM=is.element(mo,startM:12); fyr[!sM]=fyr[!sM]-1; names(fyr)=yrmo
	return(fyr)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~convFY


## convUTF------------------------------2023-07-26
##  Convert UTF-8 strings into Unicode characters.
##  https://stackoverflow.com/questions/72591452/replace-double-by-single-backslash
##  Solution by G. Grothendieck
## ------------------------------------------GG|RH
convUTF = function(x, guide=FALSE)
{
	out = character()
	if (!missing(x)) {
		out = as.character(str2expression(sprintf('"%s"', x)))
	}
	## Show guide of unicode characters (\u{xxxx})
	if (missing(x) || guide) {
		collection = c(
			"1/2 one half (\u00BD) : \\u00BD",
			"a acute      (\u00E1) : \\u00E1",
			"a grave      (\u00E0) : \\u00E0",
			"c cedilla    (\u00E7) : \\u00E7",
			"e acute      (\u00E9) : \\u00E9",
			"e circumflex (\u00EA) : \\u00EA",
			"e umlaut     (\u00EB) : \\u00EB",
			"i acute      (\u00ED) : \\u00ED",
			"i circumflex (\u00EE) : \\u00EE",
			"n tilde      (\u00F1) : \\u00F1",
			"o acute      (\u00F3) : \\u00F3",
			"o circumflex (\u00F4) : \\u00F4",
			"u circumflex (\u00FB) : \\u00FB",
			"u umlaut     (\u00FC) : \\u00FC",
			"lower mu     (\u03BC) : \\u03BC (octal \\265)",
			"squared      (\u00B2) : \\u00B2 (octal \\262)",
			"endash       (\u2013) : \\u2013 (octal \\226)",
			"bullet       (\u2022) : \\u2022 (octal \\225)",
			"multiply     (\u00D7) : \\u00D7 (octal \\327)"
		)
		attr(out,"guide") = collection
		.flush.cat(paste0(collection, collapse="\n"), "\n")
	}
	return(out)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~convUTF


## convYM-------------------------------2023-02-09
## Convert date limits into a vector of year-months (YYY-MM).
## ---------------------------------------------RH
convYM = function(x)
{
	#if (class(x)[1]=="character" && class(try(as.Date(x),silent=TRUE))=="Date" ) 
	if (inherits(x, "character") && inherits(try(as.Date(x),silent=TRUE), "Date") )
		x = as.Date(x)
	if (any(class(x)%in%c("POSIXct","POSIXt","Date")))
		cdate = format(x) # 10-character date "yyyy-mm-dd"
	else  return(rep("",length(x)))
	yrmo=substring(x,1,7) # year-month "yyyy-mm"
	yr=as.numeric(substring(yrmo,1,4)); mo=as.numeric(substring(yrmo,6,7))
	yrs=range(yr); yrs=yrs[1]:yrs[2]; nyrs=length(yrs)
	X=paste(rep(yrs,each=12),rep(pad0(1:12,2),nyrs),sep="-")
	i=match(yrmo,X)
	xout=X[i[1]:i[2]]
	return(xout)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~convYM


## convYP-------------------------------2023-02-09
## Convert dates into year periods.
## ---------------------------------------------RH
convYP = function(x, ndays=90)
{
	yearperiod=function(dchar, ndays) { # date as character string
		yr=substring(dchar,1,4); YR=as.numeric(yr)
		ddate = as.numeric(as.Date(dchar))
		odate = as.numeric(as.Date(paste(yr,"-01-01",sep="")))
		ndate = ddate - odate + 0.5
		Ndays = 366/round(366/ndays)  # derive decimal days that will create even divisions
		brks  = seq(0,366,Ndays)      # Needs to be 366 because sometimes there are more than 365 days in year
		nper  = cut(ndate,breaks=brks,labels=FALSE,right=TRUE)
		out   = round(YR + (nper*Ndays)/366, 2) # end-point of bin
		#out=round(YR+pmin((nper*ndays-ndays/2)/366,1),2) # mid-point of bin
		names(out)=paste(yr,pad0(nper,nchar(ndays)),sep="-")
		attr(out,"Ndays")=Ndays; attr(out,"Nper")=366/Ndays
		attr(out,"Ylim")=range(YR,na.rm=TRUE)
		return(out)
	}
	#if (class(x)[1]=="character" && class(try(as.Date(x),silent=TRUE))=="Date" ) 
	if (inherits(x, "character") && inherits(try(as.Date(x),silent=TRUE), "Date") )
		x = as.Date(x)
	if (any(class(x)%in%c("POSIXct","POSIXt","Date")))
		cdate = format(x) # 10-character date "yyyy-mm-dd"
	else  return(rep("",length(x)))
	return(yearperiod(cdate,ndays))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~convYP


#countLines-----------------------------2013-05-07
# Count the number of lines in an ASCII file.
#-----------------------------------------------RH
countLines = function(fnam,os=.Platform$OS.type)
{
	if (os!="windows") stop("You're sh1t out of luck")
	if (!file.exists(fnam)) stop("File name specified does not exist.")
	cmd = paste("%SystemRoot%\\system32\\findstr /R /N \"^\" ",fnam," | %SystemRoot%\\system32\\find /C \":\"",sep="")
	Nrow = as.numeric(shell(cmd,intern=TRUE))
	return(Nrow)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~countLines


## countVec-----------------------------2021-02-03
## Count number of definite vector elements (non NA)
## that exclude (or include) zero values.
## ----------------------------------------------RH
countVec = function(x, exzero=TRUE)
{
	zNA = is.na(x)
	if (!exzero)  xx = x[!zNA]
	else {
		z0 = x>0 & !zNA
		xx = x[z0]
	}
	return(length(xx))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~countVec


## createDSN----------------------------2016-12-01
## Create entire suite of DSNs for the groundfish databases
## ---------------------------------------------RH
createDSN <- function(trusted=TRUE)
{
	today = Sys.Date()
	descr = paste0("Created for PBStools (",today,")")
	confODBC(dsn="GFBioSQL",    server="GFDB",db="GFBioSQL",     driver="SQL Server", descr=descr, trusted=trusted)
	confODBC(dsn="GFCatch",     server="GFDB",db="GFCatch",      driver="SQL Server", descr=descr, trusted=trusted)
	confODBC(dsn="GFCruise",    server="GFDB",db="GFCruise",     driver="SQL Server", descr=descr, trusted=trusted)
	confODBC(dsn="PacHarvest",  server="GFDB",db="PacHarvest",   driver="SQL Server", descr=descr, trusted=trusted)
	confODBC(dsn="PacHarvHL",   server="GFDB",db="PacHarvHL",    driver="SQL Server", descr=descr, trusted=trusted)
	confODBC(dsn="PacHarvSable",server="GFDB",db="PacHarvSable", driver="SQL Server", descr=descr, trusted=trusted)
	confODBC(dsn="GFFOS",       server="GFDB",db="GFFOS",        driver="SQL Server", descr=descr, trusted=trusted)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~createDSN


## createFdir---------------------------2018-07-23
## Create a subdirectory called `french' for
## storing figures with French text and labels.
## ---------------------------------------------RH
createFdir = function(lang, dir=".")
{
	## Create a subdirectory called `french' for French-language figures
	if (is.element("f",lang)) {
		mpd.dir.f = paste0(dir,"/french")
		if (!file.exists(mpd.dir.f))
		dir.create(mpd.dir.f)
	}
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~createFdir


## crossTab-----------------------------2024-02-29
## Summarize z using crosstab values y.
## Hadley and package 'reshape' deprecated.
## ---------------------------------------------RH
crossTab = function(x=PBSdat, y=c("year","major"), 
   z="landed", func=function(x){sum(x)/1000.}, 
   na.val=999, na.str="NA", hadley=FALSE, ...)
{
	if (hadley && !requireNamespace("reshape", quietly = TRUE)) stop("`reshape` package is required")
	flds=names(x)
	if (!all(is.element(setdiff(y,"year"),flds)))
		stop ("Not all specified 'z' in dataframe")
	if (is.element("year",y) && !is.element("year",names(x))) {
		if (is.element("date",flds)) x$year=convFY(x$date,1)
		else stop("Need 'date' field to calculate 'year'") }

	if (hadley) {
		Y=reshape::melt.data.frame(x,y,z)
		expr=paste("Z=reshape::cast(Y,", paste(paste(ifelse(length(y)==1,"~",""),y,sep=""),collapse="~"), ",func,...)",sep="")
		eval(parse(text=expr))
	} else {
		## Detect character summary dimensions and replace missing values with 'na.str'to character values (RH 190128)
		if (any(grepl("character",sapply(x[,y,drop=FALSE],class)))) {
			i = grep("character",sapply(x[,y,drop=FALSE],class))
			x[,y[i]] = as.data.frame(apply(x[,y[i],drop=FALSE],2,function(xx){
				xx = PBSmodelling:::.trimWhiteSpace(xx)  ## no longer exported from Namespace
				xx[is.na(xx) | xx==""] = PBSmodelling:::.trimWhiteSpace(as.character(na.str))
				return(xx)
			} ))
		}
		X = x[,unique(c(y,z))]
		#X = x[,c(y,z)]  ## if y & z have the same fields, only need to specify once, otherwise the duplicate field becomes 'fld.1'
		X[,y][is.na(X[,y])] = na.val

		## Need drop=FALSE when y is a single factor (in the non-R sense)
		xdim = sapply(X[,y,drop=FALSE],function(xx){length(.su(xx))})
		xnam = sapply(X[,y,drop=FALSE],function(xx){.su(xx)},simplify=FALSE)
		Z    = array(0, dim=xdim, dimnames=xnam )
		X$ID = apply(X[,y,drop=FALSE],1,function(x){paste0(PBSmodelling:::.trimWhiteSpace(x),collapse="|")}) ## sometimes paste adds whitespace depending on format of y-values.

		## vector summary of x by y using func (unless func returns more than one summary value)
		Zsum = sapply(split(X[,z],X$ID),func) #,simplify=FALSE)
		if (is.vector(Zsum)) {
			Zind = strsplit(names(Zsum),split="\\|"); names(Zind) = names(Zsum)
			expr = paste0("sapply(names(Zsum), function(i){ Z[", paste0("Zind[[i]][",1:length(xdim),"]",collapse=","),"] <<- Zsum[i] })")
		} else {
			Z = array(0, dim=c(xdim,nrow(Zsum)), dimnames=c(xnam,list(pars=rownames(Zsum))))
			Zind = strsplit(colnames(Zsum),split="\\|"); names(Zind) = colnames(Zsum)
			expr = paste0("sapply(colnames(Zsum), function(i){ Z[", paste0("Zind[[i]][",1:length(xdim),"]",collapse=","),",] <<- Zsum[,i] })")
		}
		eval(parse(text=expr))
	}
	return(Z)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~crossTab


## darkenRGB----------------------------2021-06-15
##  Programmatically darken the colour of existing RGB values.
##  https://stackoverflow.com/questions/30219738/is-there-a-way-to-programmatically-darken-the-color-given-rgb-values
## --------------------------------------Roland|RH
darkenRGB = function(cols, pct, add)
{
	##require(colorspace)
	xyz  = apply(col2rgb(cols),1,deparse)
#browser();return()
	mess = paste0("hexcols = do.call(rgb, args=list(",paste0(paste0(names(xyz),"=",xyz),collapse=","),", alpha=TRUE, maxColorValue=255))")
	eval(parse(text=mess))
	## Following shenanigans to avoid message generated by readhex C code
	## https://stackoverflow.com/questions/49694552/suppress-messages-from-underlying-c-function-in-r/49722545
	rubbish = invisible(capture.output(assign("hexcols", colorspace::readhex(file = textConnection(paste(hexcols, collapse = "\n")), class = "RGB"), envir=.PBStoolEnv), type="message"))
	ttget(hexcols)

	#transform to hue/lightness/saturation colorspace
	hexcols <- as(hexcols, "HLS")
	tmpcols = hexcols
	if (!missing(pct)) {
		#multiplicative decrease of lightness
		tmpcols@coords[, "L"] <- tmpcols@coords[, "L"] * (1-pct)
	} else if (!missing(add)) {
		#additive decrease of lightness
		tmpcols@coords[, "L"] <- pmax(0, tmpcols@coords[, "L"] - add)
	}
	#going via rgb seems to work better  
	tmpcols <- as(tmpcols, "RGB")
	hexcols <- colorspace::hex(tmpcols)
	return(hexcols)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~darkenRGB


## expand5C-----------------------------2023-05-11
##  Transfer events from Moresby Gully in 5B and 
##  Flamingo Inlet/Anthony Island in 5E to PMFC 5C.
##  Used only for POP and YMR (Yellowmouth)
## ---------------------------------------------RH
expand5C = function(dat)
{
	dat$major_adj = dat$major
	## Use a points in polygon routine (see bio.fields.xls for 5C polygon)
	poly5C = data.frame(PID=rep(1,6),POS=1:6,X=c(-131.5,-132,-131,-130,-130,-131.2), Y=c(52.33333333,52.33333333,51.5,51.8,52.16666667,52.16666667))
	poly5C = as.PolySet(poly5C, projection="LL", zone=9)
	hasXY  = dat$X<0 & !is.na(dat$X) & dat$Y>0 & !is.na(dat$Y)
	if (any(hasXY)) {
		if (!is.EventData(dat)){
			if (!"EID"%in%colnames(dat)) dat$EID = 1:nrow(dat)
			tmpidat = as.EventData(dat[hasXY,],projection="LL",zone=9)    ## RH 210120
		}
		e5C = .is.in(tmpidat, poly5C)
		if (nrow(e5C$e.in)>0) {
			tmpidat$major_adj[is.element(tmpidat$EID, e5C$e.in$EID)] = 7  ## 5C
			dat$major_adj[hasXY] = tmpidat$major_adj                      ## RH 210120
		}
	}
	## Capture Anthony Island/Flamingo Inlet data for records without X,Y coordinates
	zAI = dat$major==9 & dat$minor==34 & dat$locality%in%c(1,5)
	if (any(zAI))
		dat$major_adj[zAI] = 7  ## 5C
	## Capture SE/Outside Cape St. James data for records without X,Y coordinates
	zCSJ = dat$major==6 & dat$minor==8 & dat$locality%in%c(6,12)
	if (any(zCSJ))
		dat$major_adj[zCSJ] = 7  ## 5C
	## Switch majors
	dat$major_old = dat$major
	dat$major = dat$major_adj
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~expand5C


## extractPost--------------------------2024-02-15
##  Extract model posteriors from three model
##  platforms (Awatea, SS3, iSCAM) for clients.
## ---------------------------------------------RH
extractPost = function(spc="BOR", stock="CST", assYr=2021, path=getwd(),
   values="R", runs=1:3, rwts=rep(1,3), vers="", burnin=200, nmcmc=1200,
   model="Awatea", fleets="trawl", extra="forSomeone",
   proj=FALSE, projY1=2022, projY2=2023, catpol=1500)
{
	## Subfunctions ===============================
	##
	## qtab------------------------------2021-04-19
	## Quantile tabulation summary using decimal places
	## -----------------------------------------AME
	qtab = function(xx.MCMC, dig=0, quants3=tcall(quants3)) {  ## dig is number of dec places
		out = paste0( c( 
			prettyNum(round(quantile(xx.MCMC, quants3[2]), digits=dig), big.mark=options()$big.mark),
			" (", prettyNum(round(quantile(xx.MCMC, quants3[1]), digits=dig), big.mark=options()$big.mark),
			", ", prettyNum(round(quantile(xx.MCMC, quants3[3]), digits=dig), big.mark=options()$big.mark), ")"
		), collapse="")
		return(out)
	}
	##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~qtab

	## getRdev---------------------------2023-01-19
	## Adapted from Arni Magnusson's scape::importCol, specifically subfunction 'getR'
	## ---------------------------------------AM|RH
	getRdev <- function(dir, burnin=200) {
		R <- read.table(paste(dir, "resids.pst", sep = "/"), header=TRUE, fill=TRUE)
		R <- R[,grepl("^Rec",colnames(R))]
		R <- R[, -ncol(R)]
		colnames(R) = gsub("^Rec_","",colnames(R))
		#colnames(R) <- as.integer(colnames(R)) - 1  ## no apparent need for this adjustment
		R <- R[(burnin + 1):nrow(R),]
		type.convert(R, as.is = TRUE)
	}
	##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~getRdev
	##
	## End subfunctions ===========================

	## Main function ==============================
	if (model %in% c("Awatea")) {
	## Awatea -- variant of Coleraine
		for (v in values) {
			kdf = kdf2 = kdf3 = NULL
			is.mpd = is.mcmc = FALSE
			for (i in 1:length(runs)){
				ii   = runs[i]
				iii  = pad0(ii,2)
				jj   = rwts[i]
				jjj  = pad0(jj,2)
				run  = paste0(iii,".",jjj)
				ijmc = paste0(spc,"run",iii,"/MCMC.",iii,".",jjj)
				mcdir = paste0(path,"/",ijmc)
				mpdir = sub("/MCMC\\.","/MPD.",mcdir)
				if (proj) {
					## Specific to Dee's request for BOR
					pyr1 = as.character(projY1); pyr2 = as.character(projY2)
					load (paste0(mcdir,"/currentProj.rda"))
					catpols = as.numeric(names(currentProj$U))
					cpol    = findPV(catpol, catpols)
					uproj   = currentProj$U[[cpol]]
					umed    = median(uproj[,pyr1])
					load (paste0(mcdir,"/currentProj2.rda"))
					ratpols = as.numeric(names(currentProj2$U))
					rpol    = findPV(umed, ratpols)
					.flush.cat(paste0("Harvest rate for ", pyr1, " catch projections in Run ", iii, ".", jjj, "= ", ratpols[rpol]), "\n")
					yproj   = currentProj2$Y[[rpol]]
					yproj3   = currentProj2$Y[["0.06"]]  ## will have to run code before determining this number from object 'kdf'
					udat = uproj; cdat = yproj; cdat3 = yproj3
					run.sample = paste(iii, jjj, rownames(udat), sep=".")
					ijdat = data.frame('Run.Sample'=run.sample, udat)
					ijdat2 = data.frame('Run.Sample'=run.sample, cdat)
					ijdat3 = data.frame('Run.Sample'=run.sample, cdat3)
					colnames(ijdat) = colnames(ijdat2) = colnames(ijdat3) = sub("^X","",colnames(ijdat))
					kdf = rbind(kdf,ijdat)    ## projected harvest rates at catch policy 'catpol'
					kdf2 = rbind(kdf2,ijdat2) ## projected catches at harvest rate for run i rwt j
					kdf3 = rbind(kdf3,ijdat3) ## projected catches at median composite 'kdf' harvest rate (manual iteration)
				} else {
					if (v %in% c("B","R","U","VB","P","Rdev")){ ## MCMC
						is.mcmc = TRUE
						if (v %in% "Rdev"){
							kdat = getRdev(dir=paste0(path,"/",spc,"run",iii,"/MCMC.",iii,".",jjj), burnin=burnin)
						} else {
							if (!file.exists (paste0(mcdir,"/currentMCMC.rda"))) {
								## Need to re-run data collection and create .rda files
								.flush.cat(paste0("Collecting MCMC binaries for ", strSpp, " in ", assYr, "..."), "\n")
								so("redo.currentMCMC.r","awatea")
								redo.currentMCMC(strSpp=strSpp, assYr=assYr, stock=stock, mpdir=mpdir, mcdir=mcdir, mcsub=(burnin+1):nmcmc)
							}
							load (paste0(mcdir,"/currentMCMC.rda"))
							kdat = currentMCMC[[v]]
						}
						rnams = rownames(kdat)
						rnams = pad0(rnams,max(nchar(rnams))-1)
						run.sample = paste(iii, jjj, rnams, sep=".")
						ijdat = data.frame('Run.Sample'=run.sample, kdat)
						colnames(ijdat) = sub("^X","",colnames(ijdat))
						if (v %in% "P") {
							if (i==1) kdf = list()
							kdf[[run]] = ijdat
						} else { 
							if (i>1 && ncol(ijdat)!=ncol(kdf)) {  ## usually for parameters
								browser();return()
							}
							kdf = rbind(kdf,ijdat)
						}
					} else if (v %in% c("C")){
						is.mpd = TRUE
						mpd.file = list.files(mpdir,pattern="^Bmpd.+rda$")
						if (length(mpd.file)==0) {
							recon = TRUE
							run.dir = dirname(mpdir)
							mpd.res = list.files(run.dir, pattern=paste0("+.",iii,"\\.",jjj,"\\.res"))
							currentRes = importRes(paste0(run.dir,"/",mpd.res), Dev=TRUE, CPUE=TRUE, Survey=TRUE, CLc=TRUE, CLs=TRUE, CAs=TRUE, CAc=TRUE, extra=TRUE)
							save("currentRes",file=paste0(mpdir,"/currentRes.rda"))  ## useful to have when compiling the final results appendix
						} else {
							recon = FALSE
							load (paste0(mpdir,"/",mpd.file))
						}
						if (v %in% "C") {
							if (recon) {
								years = currentRes$B[,"Year"]
								Ct = currentRes$B[,-1][,grep("Y",names(currentRes$B[,-1])),drop=FALSE] ## needed in case Ngear > 1
								Ct = Ct[-nrow(Ct),,drop=FALSE]
								row.names(Ct) = years[-length(years)]
								kdat = Ct
							} else {
								kdat = Bmpd[[1]][[1]]$Ct
								if (is.vector(kdat)) {
									rnams = as.character(Bmpd[[1]][[1]]$years)
									rnams = rnams[-length(rnams)]
									kdat = as.data.frame(matrix(kdat, ncol=1, dimnames=list(rnams,"fishery")))
								}
							}
							if (is.null(dim(kdat)) || dim(kdat)[2]!=length(fleets)) {
								message("Fleet names do not match dimensions of catch")
								browser();return()
							}
							kdf  = kdat
							colnames(kdf) = fleets
							next
						} else if (v %in% "Rdev") {
							kdat = Bmpd[[1]][[1]]$logRecDev.mpd
							ijdat = cbind(NULL,kdat)
							colnames(ijdat) = paste0("R",iii,".",jjj)
							kdf  = cbind(kdf, ijdat)
						}
					}
				}
			}
			if (v %in% "P") {
				allnams = colnames(kdf[[which.max(sapply(kdf,ncol))]])
				if (all(sapply(kdf, function(x){all(colnames(x) %in% allnams)})) ) {
					ii = 0
					kls = kdf ## kdf is a list at this point
					#kdf = setNames(data.frame(matrix(ncol=length(allnams), nrow=0)), allnams)
					kdf = setNames(data.frame(matrix(NA, ncol=length(allnams), nrow=sum(sapply(kls,nrow)) )), allnams)
					for (i in 1:length(kls)) {
						idf = kls[[i]]
						ii  = 1:nrow(idf) + max(ii)
						kdf[ii, names(idf)] = idf
#if	(i==2){browser();return()}
					}
#br	owser();return()
				} else {
					browser();return()
				}
			}
			write.csv(kdf, paste0(spc,"-", stock,"-", assYr,"-", ifelse(is.mpd,"MPD","MCMC"),"(", paste0(v,collapse="."), ")-", extra, ".csv"), row.names=ifelse(is.mpd,TRUE,FALSE))
			out = list(kdf=kdf)
			if (proj) {
				## Harvest rates at constant catch policy (CCP) given by argument 'catpol'
				u2022   = quantile(kdf[,pyr1], c(0.05,0.5,0.95))
				q2022   = qtab(kdf[,pyr1], dig=2, quants3=c(0.05,0.5,0.95))
				.flush.cat(paste0("\nMedian (quantiles 0.05,0.95)  harvest rate in ", pyr1, " using the composite harvest rate object: \n", q2022), "\n\n")
		
				## Catches using multiple constant harvest rate policies (CHRPs); each specific to the component run of a composite
				c2023   = quantile(kdf2[,pyr2], c(0.05,0.5,0.95))
				q2023   = qtab(kdf2[,pyr2], dig=0, quants3=c(0.05,0.5,0.95))
				.flush.cat(paste0("Median (quantiles 0.05,0.95) catch in ", pyr2, " using the composite catch object from ", length(runs)," CHRPs:\n", q2023), "\n\n")
		
				rpol2    = findPV(median(kdf[,pyr1]), ratpols)
				.flush.cat(paste0("Harvest rate for ", pyr1, " catch projections using the median of the composite harvest rate object, CHRP=", ratpols[rpol2]), "\n")
				## Catches using one CHRP dtermined from median of HR composite; each specific to the component run of a composite
				c2023a  = quantile(kdf3[,pyr2], c(0.05,0.5,0.95))
				q2023a  = qtab(kdf3[,pyr2], dig=0, quants3=c(0.05,0.5,0.95))
				.flush.cat(paste0("Median (quantiles 0.05,0.95) catch in ", pyr2, " using the ", pyr1," CHRP=", ratpols[rpol2], ":\n", q2023a), "\n\n")
		
				## Write various kdf objects to CSV files
				row.names(kdf2) = 1:nrow(kdf2)
				write.csv(kdf2, paste0(spc,"-", stock,"-", assYr,"-", "MCMC(CHR3)-", extra, ".csv"), row.names=FALSE)
				row.names(kdf3) = 1:nrow(kdf3)
				write.csv(kdf3, paste0(spc,"-", stock,"-", assYr,"-", "MCMC(CHR1)-", extra, ".csv"), row.names=FALSE)
				out = c(out, list(kdf2=kdf2, kdf3=kdf3))
			}
		}
	} ## end Awatea

	else if (model %in% c("SS3")) {
		## Stock Synthesis 3 after using 'gatherMCMCs' function
		compo.files = list.files(path=path, pattern="^compo\\.[[:digit:]]+\\.rda")
		if (length(compo.files)>0) {
			mcfile = max(compo.files)
			load(paste0(path, "/", mcfile))  ## loads list object 'compo'
			unpackList(compo, scope="L")
		}
		for (v in values) {
			if (v %in% "C") {
				crun = as.numeric(substr(path, nchar(path)-1, nchar(path)))
				crwt = rwts[which(runs==crun)]
				cver = vers[which(runs==crun)]
				crrv = sub("\\.$","",paste0(c(pad0(crun,2),pad0(crwt,2),cver), collapse="."))
				crfile = paste0(path, "/MPD.", crrv, "/data.", pad0(crun,2), ".", pad0(crwt,2), ".ss")
				#crdat = r4ss::SS_readdat(crfile)
				#crdat = SS_readdat(crfile) ## function not imported from r4ss in NAMESPACE because r4ss has a sh!tload of dependencies
				#crdat = PBStools:::.ss3.readdat(crfile)
				crdat = .ss3.readdat(crfile)  ## .ss3* exported from Namespace
				crcat = crdat$catch
				crcat = crcat[crcat$year>0,]
				flcat = split(crcat$catch,crcat$fleet)
				catch = do.call("cbind", lapply(flcat, data.frame, stringsAsFactors=FALSE))
				rownames(catch) = .su(crcat$year)
				colnames(catch) = fleets
#browser();return()
				write.csv(catch, paste0(spc,"-", stock,"-", assYr,"-MPD(", v, ")-", extra, ".csv"), row.names=TRUE)
				out = crdat
			} else {
				if (v %in% c("B")) {
					if ( exists("xavgTS", inherits=FALSE) ) {
						for (aa in dimnames(xavgTS)$area) {
							write.csv(xavgTS[,,"B",aa], paste0(spc,"-", aa,"-", assYr,"-MCMC(", v, ")-", extra, ".csv"), row.names=TRUE)
						}
					} else {
						write.csv(avgTS[,,"Bt"], paste0(spc,"-", stock,"-", assYr,"-MCMC(", v, ")-", extra, ".csv"), row.names=TRUE)
					}
				}
				if (v %in% c("R")) {
					write.csv(avgTS[,,"Rt"], paste0(spc,"-", stock,"-", assYr,"-MCMC(", v, ")-", extra, ".csv"), row.names=TRUE)
				}
				if (v %in% c("U")) {
					write.csv(avgTS[,,"ut"], paste0(spc,"-", stock,"-", assYr,"-MCMC(", v, ")-", extra, ".csv"), row.names=TRUE)
				}
				if (v %in% c("P")) {
					write.csv(avgPA, paste0(spc,"-", stock,"-", assYr,"-MCMC(PA)-", extra, ".csv"), row.names=TRUE)
					write.csv(avgRP, paste0(spc,"-", stock,"-", assYr,"-MCMC(RP)-", extra, ".csv"), row.names=TRUE)
				}
				if (v %in% c("Rdev")) {
					write.csv(avgTS[,,"Rtdev"], paste0(spc,"-", stock,"-", assYr,"-MCMC(", v, ")-", extra, ".csv"), row.names=TRUE)
				}
				if (v %in% c("Bmsy")) {
					if ( exists("xavgRP", inherits=FALSE) ) {
						for (aa in dimnames(xavgRP)$area) {
							Bmsy = as.data.frame(matrix(xavgRP[,"Bmsy",aa], ncol=1, dimnames=list(rownames(xavgRP), "Bmsy")))
							write.csv(Bmsy, paste0(spc,"-", aa,"-", assYr,"-MCMC(", v, ")-", extra, ".csv"), row.names=TRUE)
						}
					} else {
						Bmsy = as.data.frame(matrix(avgRP[,"Bmsy"], ncol=1, dimnames=list(rownames(avgRP), "Bmsy")))
						write.csv(Bmsy, paste0(spc,"-", stock,"-", assYr,"-MCMC(", v, ")-", extra, ".csv"), row.names=TRUE)
					}
				}
				out = compo
			}
		}
	} ## end SS3
	else if (model %in% c("iSCAM")) {
		## iSCAM -- integrated Statistical Catch-age Model (Steve Martell)
		if (strSpp %in% c("SST")){
			rpath = "C:/Users/haighr/Files/GFish/PSARC/PSARC_2010s/PSARC15/SST/DDiff/rscripts-pbs"
			source(paste0(rpath,"/reptolist.r"), local=TRUE)
			spath = "assess"  ## stock path
			cpath = paste0(path,"/assess05")  ## central run (example run)
			inputs = dattoRlist(paste0(cpath,"/sstassess05dd.dat"))
		}
		if (strSpp %in% c("WAP")){
			rpath = "C:/Users/haighr/Files/GFish/PSARC/PSARC_2010s/PSARC17/WAP/DDiff/rscripts-pbs"
			source(paste0(rpath,"/reptolist.r"), local=TRUE)
			hrp = FALSE  ## assume no historical reference points unless otherwise detected
			## Need to get composites for Bavg and Bmin (not captured in iSCAM report files)
			if (any(grepl("Bavg|Bmin", values))) {
				fpath = "C:/Users/haighr/Files/GFish/PSARC/PSARC_2010s/PSARC17/WAP/Docs/RD/AppF_Results"
				load(paste0(fdir,"/A.SAR.rda"))
				hrp = TRUE  ## historical reference points
			}
			if (stock %in% c("North")) {
				spath = "Nassess"  ## stock path
				cpath = paste0(path,"/Nassess01")  ## central run (example run)
				inputs = dattoRlist(paste0(cpath,"/wapNassess01dd.dat"))
				if (hrp) {
					Bavg = A.SAR$WAPN$Bavg$modAvg
					Bmin = A.SAR$WAPN$Bmin$modAvg
					hrp.runs = gsub(spath,"",setdiff(names(A.SAR$WAPN$Bavg),"modAvg"))
				}
			}
			if (stock %in% c("South")) {
				spath = "Sassess"  ## stock path
				cpath = paste0(path,"/Sassess04")  ## central run (example run)
				inputs = dattoRlist(paste0(cpath,"/WAPSassess04dd.dat"))
				if (hrp) {
					Bavg = A.SAR$WAPS$Bavg$modAvg
					Bmin = A.SAR$WAPS$Bmin$modAvg
					hrp.runs = gsub(spath,"",setdiff(names(A.SAR$WAPS$Bavg),"modAvg"))
				}
			}
			if (hrp) {
				#z = match(runs,as.numeric(hrp.runs))
				#hrp.runs[order(runs[zz])]  ## too much hassle
				if (!all(runs==as.numeric(hrp.runs))) {
					mess = paste0("Your order choice of runs (", paste0(runs,collapse=", "), ")\n   does not match the model average order (", paste0(as.numeric(hrp.runs),collapse=", "), ")")
					stop(mess)
					browser();return()
				}
				hrp.samp = (burnin+1):nmcmc
				hrp.rnam = paste0("R", rep(hrp.runs, each=length(hrp.samp)), ".", rep(pad0(hrp.samp,4), length(hrp.runs)))
				HRP = data.frame(Bavg=Bavg, Bmin=Bmin)
				rownames(HRP) = hrp.rnam
				write.csv(HRP, paste0(spc,"-", stock,"-", assYr,"-MCMC(HRP)-", extra, ".csv"), row.names=TRUE)
				values = setdiff(values, c("Bavg","Bmin"))
			}
		}
		out    = inputs
		years  = inputs$`#first year of data`:inputs$`#last year of data`
		nyrs   = length(years)
		for (v in values) {
			if (v %in% "C"){
				catch = inputs[[grep("yr commercial",names(inputs))]]
				catch = catch[,apply(catch,2,sum)>0]
				kdat  = as.data.frame(matrix(catch[,-1], ncol=ncol(catch)-1, dimnames=list(catch[,1], fleets)))
				write.csv(kdat, paste0(spc,"-", stock,"-", assYr,"-MPD(", v, ")-", extra, ".csv"), row.names=TRUE)
#browser();return()
			} else {
				kdat = NULL
				for (i in runs) {
					ii = pad0(i,2)
					.flush.cat(paste0("Species=", strSpp, " Stock=", stock, " Value=", v, " Run", ii),"\n")
					ipath = paste0(path,"/", spath, ii)
					iprefix = switch (v, 'B'="sbt", 'R'="rt", 'Rdev'="recDevs", 'U'='ft', 'P'="iscamdelaydiff")
					idat = read.table(paste0(ipath,"/",iprefix,".mcmc"), header=ifelse(v %in% "P",TRUE,FALSE))
					if (v=="U") idat = 1 - exp(-idat)  ## convert instantaneous F to exploitation rate U
					if (v=="P") {
						idat = idat[(burnin+1):nmcmc, grep("Age\\.|Poor|Average|Good",colnames(idat),invert=TRUE,value=TRUE)]
					} else {
						iyrs = if (v %in% c("R")) years[ifelse(strSpp %in% "SST",-c(1:2),-1)] else years  ## why are recruits missing the first 2 years for SST?
						idat = idat[(burnin+1):nmcmc,1:length(iyrs)]
						colnames(idat) = iyrs
					}
					rownames(idat) = paste0("R",ii,".",pad0(rownames(idat),4))
#if(i==8) {browser();return()}
					if (!is.null(kdat) && (ncol(idat)!=ncol(kdat) || !all(colnames(idat)==colnames(kdat)))){
						if (ncol(idat)>ncol(kdat))
							inams = unique(colnames(idat), colnames(kdat))
						else
							inams = unique(colnames(kdat), colnames(idat))
						kdat.keep = kdat
						#kdat = setNames(data.frame(matrix(ncol=length(inams), nrow=0)), inams)  ## interesting but won't work
#browser();return()
						kdat = as.data.frame(array(NA, dim=c(nrow(kdat.keep)+nrow(idat),length(inams)), dimnames=list(c(rownames(kdat.keep),rownames(idat)),inams)))
						kdat[rownames(kdat.keep),colnames(kdat.keep)] = kdat.keep
						kdat[rownames(idat),colnames(idat)] = idat
					} else {
						kdat = rbind(kdat, idat)
					}
				}
				write.csv(kdat, paste0(spc,"-", stock,"-", assYr,"-MCMC(", v, ")-", extra, ".csv"), row.names=TRUE)
#browser();return()
			} ## end MCMC collection
		} ## end value loop
	} ## end iSCAM
	invisible(return(out))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~extractPost


## findPV-------------------------------2018-11-09
## Find nearest position in vector choice using a target point.
## source: ## https://stat.ethz.ch/pipermail/r-help/2008-July/167216.html
## ---------------------------------------------RH
findPV = function(p,v){
	## Using sapply allows multiple target points p
	sapply(p, function(x,v){
		 ## occasionally two vector points are equidistant to the target p
		which(abs(v-x)==min(abs(v-x)))[1]
	}, v=v)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~findPV


## findRC-------------------------------2018-11-08
## Return number of rows and columns for plotting
## multi-panel figures given number of figures (nf)
## to fit on one page.
## Similar to function PBSmodelling::.findSquare
## ---------------------------------------------RH
findRC = function (nf, orient="portrait") 
{
	sqn = sqrt(nf)
	m = ceiling(sqn)
	n = ceiling(nf/m)
	if (inWord("landscape", orient, prefix=TRUE))
		return(c(n, m))
	else
		return(c(m, n))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~findRC


#fitLogit-------------------------------2009-11-09
# Fit binomial data using logit link function
#-------------------------------------------ARK/RH
fitLogit = function(dat, yfld="pmat", xflds="age") { 
	# Calculate logit regression assuming binomial error
	dat = na.omit(dat[,c(yfld,xflds)])
	expr=paste("glmfit = glm(",yfld,"~",paste(xflds,collapse="+"),
		", family=binomial(link=\"logit\"), data=dat)",sep="")
	eval(parse(text=expr))
	return(glmfit) }


## flagIt-------------------------------2018-12-04
## Takes a coordinate (a,b) and labels it using a 
## diagonal line of radius r and angle A.
##  a,b : midpoint of a circle
##  A   : angle (degrees) to radiate out
##  r   : radius of the circle
##  n   : number of times to decrement the radius (max=5)
##  lab : optional label to precede the flagged coordinate
## ---------------------------------------------RH
flagIt = function(a, b, A=45, r=0.2, n=1, lab, ...)
{
	xlim = par()$usr[1:2]
	xdif = abs(diff(xlim))
	ylim = par()$usr[3:4]
	ydif = abs(diff(ylim))
	xpin = par()$pin[1]
	ypin = par()$pin[2]
	rads = pi*A/180
	xsig = sign(cos(rads))  ## sign of x (not used)
	ysig = sign(sin(rads))  ## sign of y (not used)
	
	r   = r - (n-1)*r/5     ## reduce when n increases
	x0  = a + r*cos(rads)
	y   = b + r*sin(rads)
	x   = x0 + (x0-a)*(xdif/ydif)*(ypin/xpin)  ## adjust for diffences in axis scale and plot dimensions (aspect ratio)
	xvec = c(a,x)
	yvec = c(b,y)
	lines(xvec,yvec,lty=3,col="grey20")
	text(x,y,paste0(ifelse(missing(lab),"",lab),"(",signif(a,3),", ",signif(b,3),")"),...)
	return(invisible(list(xvec=xvec,yvec=yvec,rads=rads,x0=x0,x=x,y=y)))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~flagIt


#gatherVals-----------------------------2018-03-14
# Gathers data from multiple columns into key-value pairs.
# Essentially a replacement function for tidyr::gather.
#-----------------------------------------------RH
gatherVals = function(x, columns){
	if (missing(columns)) {
		columns=1:ncol(x)  ## i.e. use all columns
	} else if (all(is.character(columns))) {
		## grab the column positions if user supplies column names
		columns = match(intersect(columns,colnames(x)),colnames(x))
	}
	xout = list(key=NULL, value=NULL)
	for (i in columns) {
		xout[["key"]] = c(xout[["key"]], rep(colnames(x)[i],nrow(x)))
		xout[["value"]] =c (xout[["value"]], x[,i])
	}
	xout = as.data.frame(xout)
	return(xout)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~gatherVals


## getData------------------------------2016-12-01
## Get data from a variety of sources.
## subQtrust -- if user has no DFO trusted credentials:
##   if (type=="SQL"), c(trusted, uid, pwd) copied to `subQtrust'
##   if (type=="ORA") user MUST supply `subQtrust' list.
## ---------------------------------------------RH
getData <-function(fqtName, dbName="PacHarvest", strSpp=NULL, server=NULL,
   type="SQL", path=getwd(), trusted=TRUE, uid="", pwd="",
   subQtrust = list(trusted=TRUE, uid="", pwd=""),
   noFactors=TRUE, noLogicals=TRUE, rownum=0, mindep=NULL, 
   maxdep=NULL, surveyid=NULL, survserid=NULL, fisheryid=NULL, 
   logtype=NULL, doors=NULL, speed=NULL, mnwt=NULL, tarSpp=NULL, 
   major=NULL, top=NULL, gear=NULL, dummy=NULL, senv=NULL, tenv=.GlobalEnv, ...)
{
	on.exit(odbcCloseAll())
	if (!is.null(getPBSoptions("showError")) && getPBSoptions("showError")) {
		setPBSoptions("showError", FALSE); frame()  }# clear any previous error messages
	if (missing(fqtName)) showError("Specify 'fqtName'")
	if (dbName=="") showError("Specify 'dbName'")
	# For people who have SQL Server accounts instead of trusted credentials:
	if (!trusted && type=="SQL")
		subQtrust = list( trusted=FALSE, uid=uid, pwd=pwd )

	timeF0=proc.time()[1:3]                ### start timing getData
	strQ=as.character(substitute(fqtName)) ### string name
	if (length(strQ)>1 && any(strQ=="paste"))
		strQ = fqtName
	pfenv=parent.frame(1)
	if (exists(strQ,where=pfenv) && length(get(strQ,envir=pfenv))==1 && is.character(get(strQ,envir=pfenv)))
		fqtName=get(strQ,envir=pfenv)     ## variable string name
	else fqtName=strQ
	envs=sys.frames()                    ## list all environments currently open
	if (type=="FILE") {
		expr=paste("getFile(\"",fqtName,"\",path=\"",path,"\",senv=senv,use.pkg=TRUE,try.all.frames=TRUE,tenv=penv()); ",sep="")
		expr=paste(expr,"PBSdat=",fqtName,";",sep="")
		expr=paste(expr," attr(PBSdat,\"fqt\")=\"",fqtName,"\"",sep="")
		eval(parse(text=expr)) }
	else if (type=="XLS") {
		eval(parse(text=paste("xlsName=paste(\"",path,"\",\"",dbName,"\",sep=\"/\")",sep="")))
		expr=paste("PBSdat=.getXLSdata(xlsTable=\"",xlsName,"\",qtName=\"",fqtName,"\"); ",
			"attr(PBSdat,\"xls\")=\"",xlsName,"\"; ",
			"attr(PBSdat,\"fqt\")=\"",fqtName,"\"",sep="")
		eval(parse(text=expr)) }
	else if (type=="DBF") {
		eval(parse(text=paste("dbfName=paste(\"",path,"\",\"",fqtName,".dbf\",sep=\"/\")",sep="")))
		if (nchar(fqtName)>8) {
			rtmp=tempdir(); rtmp=gsub("\\\\", "/", rtmp); 
			olddbfName=dbfName; oldfqtName=fqtName
			fqtName=substring(oldfqtName,1,8)
			dbfName=paste(rtmp,"/",fqtName,".dbf",sep="")
			file.copy(olddbfName,dbfName,overwrite=TRUE) }
		expr=paste("PBSdat=.getDBFdata(dbfTable=\"",dbfName,"\", qtName=\"",fqtName,"\"); ",
			"attr(PBSdat,\"dbf\")=\"",olddbfName,"\"; ",
			"attr(PBSdat,\"fqt\")=\"",oldfqtName,"\"",sep="")
		eval(parse(text=expr)) }
	else if (type=="MDB") {
		eval(parse(text=paste("mdbName=paste(\"",path,"\",\"",dbName,"\",sep=\"/\")",sep="")))
		expr=paste("PBSdat=.getMDBdata(mdbTable=\"",mdbName,"\",qtName=\"",fqtName,"\",rownum=",rownum,"); ",
			"attr(PBSdat,\"db\")=\"",mdbName,"\"; ",
			"attr(PBSdat,\"fqt\")=\"",fqtName,"\"",sep="")
		eval(parse(text=expr)) }
	else if (any(type==c("SQL","ORA","SQLX","ORAX"))) {
		## If a direct expression, execute it. If no species, grab a table.
		if (any(type==c("SQLX","ORAX")) || is.null(strSpp) || strSpp=="") {
			isExpr = ifelse(any(type==c("SQLX","ORAX")),TRUE,FALSE)
			expr=paste(c("datt=.getSQLdata(dbName=\"",dbName,"\"",
				",qtName=", ifelse(isExpr,"NULL",paste("\"",fqtName,"\"",sep="")),
				",strSQL=", ifelse(isExpr,paste("\"",fqtName,"\"",sep=""),"NULL"),
				",server=\"",server,"\",type=\"",substring(type,1,3),"\",rownum=",rownum,
				",trusted=",trusted,",uid=\"",uid,"\",pwd=\"",pwd,"\",...)"),collapse="")
#browser();return()
			timeQ0=proc.time()[1:3]  ## start timing SQL query
			eval(parse(text=expr)) 
			timeQ = round(proc.time()[1:3]-timeQ0,2) }
		else {
			data("species", package="PBSdata", envir=penv())
			suni = function(x) {sort(unique(x))}
			alfSpp=suni(species$code[species$fish])                 ## all fish species
			trfSpp=suni(species$code[species$rf])                   ## total rockfish species
			orfSpp=setdiff(trfSpp,"396")                            ## other rockfish species
			tffSpp=suni(species$code[species$ff])                   ## total flatfish species
			offSpp=setdiff(tffSpp,"602")                            ## other flatfish species
			carSpp=suni(species$code[is.element(species$taxon,2)])  ## cartilaginous species
			sssSpp=setdiff(carSpp,c("065","066"))                   ## shark and skate species
			invSpp=suni(species$code[species$invert])               ## invertebrates
			t01Spp=suni(species$code[is.element(species$taxon,1)])  ## taxon 1:  Agnatha (hagfish, lampreys)
			t02Spp=suni(species$code[is.element(species$taxon,2)])  ## taxon 2:  Chondrichthyes (sharks, rays, chimaeras)
			t03Spp=suni(species$code[is.element(species$taxon,3)])  ## taxon 3:  Clupeiformes (herring, anchovies)
			t04Spp=suni(species$code[is.element(species$taxon,4)])  ## taxon 4:  Salmonidae (salmon, trout)
			t05Spp=suni(species$code[is.element(species$taxon,5)])  ## taxon 5:  Osmeridae (smelts)
			t06Spp=suni(species$code[is.element(species$taxon,6)])  ## taxon 6:  Myctophidae (lanternfishes)
			t07Spp=suni(species$code[is.element(species$taxon,7)])  ## taxon 7:  Gadidae (codfishes)
			t08Spp=suni(species$code[is.element(species$taxon,8)])  ## taxon 8:  Macrouridae (grenadiers)
			t09Spp=suni(species$code[is.element(species$taxon,9)])  ## taxon 9:  Scombridae (mackerels, tunas)
			t10Spp=suni(species$code[is.element(species$taxon,10)]) ## taxon 10: Scorpaenidae (scorpionfishes)
			t11Spp=suni(species$code[is.element(species$taxon,11)]) ## taxon 11: Hexagrammidae (greenlings)
			t12Spp=suni(species$code[is.element(species$taxon,12)]) ## taxon 12: Pleuronectiformes (flounders, soles, halibut)
			t00Spp=intersect(alfSpp,suni(species$code[!is.element(species$taxon,1:12)])) ### taxon 00: All fish not part of a taxon group
			if (is.null(mnwt)) {
				if (type=="ORA") 
					mnwt=species[strSpp,"foswt",drop=FALSE]
				else 
					mnwt=species[strSpp,c("gfbcwt","gfbrwt"),drop=FALSE]
				mnwt=apply(mnwt,1,function(x){if(all(is.na(x))) 1.0 else max(x[!is.na(x)])}) 
				mnwt=mean(mnwt)} ## mean weight of one or more species in kg
			sppS <- paste("'",paste(strSpp,collapse="','"),"'",sep="")
			sppT <- paste("'",paste(tarSpp,collapse="','"),"'",sep="")
			sALF <- paste("'",paste(alfSpp,collapse="','"),"'",sep="")
			sTRF <- paste("'",paste(trfSpp,collapse="','"),"'",sep="")
			sORF <- paste("'",paste(orfSpp,collapse="','"),"'",sep="")
			sTFF <- paste("'",paste(tffSpp,collapse="','"),"'",sep="")
			sOFF <- paste("'",paste(offSpp,collapse="','"),"'",sep="")
			sCAR <- paste("'",paste(carSpp,collapse="','"),"'",sep="")
			sSSS <- paste("'",paste(sssSpp,collapse="','"),"'",sep="")
			sINV <- paste("'",paste(invSpp,collapse="','"),"'",sep="")
			sT01 <- paste("'",paste(t01Spp,collapse="','"),"'",sep="")
			sT02 <- paste("'",paste(t02Spp,collapse="','"),"'",sep="")
			sT03 <- paste("'",paste(t03Spp,collapse="','"),"'",sep="")
			sT04 <- paste("'",paste(t04Spp,collapse="','"),"'",sep="")
			sT05 <- paste("'",paste(t05Spp,collapse="','"),"'",sep="")
			sT06 <- paste("'",paste(t06Spp,collapse="','"),"'",sep="")
			sT07 <- paste("'",paste(t07Spp,collapse="','"),"'",sep="")
			sT08 <- paste("'",paste(t08Spp,collapse="','"),"'",sep="")
			sT09 <- paste("'",paste(t09Spp,collapse="','"),"'",sep="")
			sT10 <- paste("'",paste(t10Spp,collapse="','"),"'",sep="")
			sT11 <- paste("'",paste(t11Spp,collapse="','"),"'",sep="")
			sT12 <- paste("'",paste(t12Spp,collapse="','"),"'",sep="")
			sT00 <- paste("'",paste(t00Spp,collapse="','"),"'",sep="")
			## If Oracle SQL is called from SQL Server via OPENQUERY
			pppS <- paste("''",paste(strSpp,collapse="'',''"),"''",sep="")
			pppT <- paste("''",paste(tarSpp,collapse="'',''"),"''",sep="")
			pALF <- paste("''",paste(alfSpp,collapse="'',''"),"''",sep="")
			pTRF <- paste("''",paste(trfSpp,collapse="'',''"),"''",sep="")
			pORF <- paste("''",paste(orfSpp,collapse="'',''"),"''",sep="")
			pTFF <- paste("''",paste(tffSpp,collapse="'',''"),"''",sep="")
			pOFF <- paste("''",paste(offSpp,collapse="'',''"),"''",sep="")
			pCAR <- paste("''",paste(carSpp,collapse="'',''"),"''",sep="")
			pSSS <- paste("''",paste(sssSpp,collapse="'',''"),"''",sep="")
			pINV <- paste("''",paste(invSpp,collapse="'',''"),"''",sep="")
			pT01 <- paste("''",paste(t01Spp,collapse="'',''"),"''",sep="")
			pT02 <- paste("''",paste(t02Spp,collapse="'',''"),"''",sep="")
			pT03 <- paste("''",paste(t03Spp,collapse="'',''"),"''",sep="")
			pT04 <- paste("''",paste(t04Spp,collapse="'',''"),"''",sep="")
			pT05 <- paste("''",paste(t05Spp,collapse="'',''"),"''",sep="")
			pT06 <- paste("''",paste(t06Spp,collapse="'',''"),"''",sep="")
			pT07 <- paste("''",paste(t07Spp,collapse="'',''"),"''",sep="")
			pT08 <- paste("''",paste(t08Spp,collapse="'',''"),"''",sep="")
			pT09 <- paste("''",paste(t09Spp,collapse="'',''"),"''",sep="")
			pT10 <- paste("''",paste(t10Spp,collapse="'',''"),"''",sep="")
			pT11 <- paste("''",paste(t11Spp,collapse="'',''"),"''",sep="")
			pT12 <- paste("''",paste(t12Spp,collapse="'',''"),"''",sep="")
			pT00 <- paste("''",paste(t00Spp,collapse="'',''"),"''",sep="")
			##------------------------------------------------------------
			qnam <- paste(path,fqtName,sep="/")
			strQ <- readLines(qnam)
			strQ <- PBSmodelling:::.trimWhiteSpace(strQ)  ## no longer exported from Namespace
			##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
			## 2015-10-28 -- RH added code to allow insertion of other SQL files.
			if(any(grepl("@INSERT",strQ))) {
				in.ids   = grep("@INSERT",strQ)
				in.lines = strQ[in.ids]
				in.list  = strsplit(in.lines,split="'"); names(in.list)= in.ids
				in.names = sapply(in.list,function(x){x[grep("\\.sql$",x)]})
				k = 1:length(strQ)
				g = cut(k,breaks=c(0,in.ids,length(k)),labels=FALSE)
				g[in.ids] = NA
				keep = split(k,g)
				newQ = as.character()
				for (i in 1:length(in.ids)) {
					ii = in.ids[i]
					if (i==1 && ii>1) newQ = strQ[keep[[i]]]
					strI = readLines(paste0(path,"/",in.names[i]))
					if (length(strI)>0)  newQ = c(newQ, strI)
					if (ii<length(strQ)) newQ = c(newQ, strQ[keep[[i+1]]])
				}
				strQ = newQ
			}
			##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
			begC <- regexpr("--",strQ)
			isC  <- begC>0                               ## identify comments
			strQ[isC]=substring(strQ[isC],0,begC[isC]-1) ## strip comments
			strQ <- strQ[!strQ==""]                      ## strip blank lines
			strQ <- paste(strQ,collapse=" ")
			if (type=="ORA") 
				strQ <- gsub(pattern="@table",replacement=dbName,x=strQ)
			strQ <- gsub(pattern="@sppcode",replacement=sppS,x=strQ)
			strQ <- gsub(pattern="@tarcode",replacement=sppT,x=strQ)
			strQ <- gsub(pattern="@alfcode",replacement=sALF,x=strQ)
			strQ <- gsub(pattern="@trfcode",replacement=sTRF,x=strQ)
			strQ <- gsub(pattern="@orfcode",replacement=sORF,x=strQ)
			strQ <- gsub(pattern="@tffcode",replacement=sTFF,x=strQ)
			strQ <- gsub(pattern="@offcode",replacement=sOFF,x=strQ)
			strQ <- gsub(pattern="@carcode",replacement=sCAR,x=strQ)
			strQ <- gsub(pattern="@ssscode",replacement=sSSS,x=strQ)
			strQ <- gsub(pattern="@invcode",replacement=sINV,x=strQ)
			strQ <- gsub(pattern="@t01code",replacement=sT01,x=strQ)
			strQ <- gsub(pattern="@t02code",replacement=sT02,x=strQ)
			strQ <- gsub(pattern="@t03code",replacement=sT03,x=strQ)
			strQ <- gsub(pattern="@t04code",replacement=sT04,x=strQ)
			strQ <- gsub(pattern="@t05code",replacement=sT05,x=strQ)
			strQ <- gsub(pattern="@t06code",replacement=sT06,x=strQ)
			strQ <- gsub(pattern="@t07code",replacement=sT07,x=strQ)
			strQ <- gsub(pattern="@t08code",replacement=sT08,x=strQ)
			strQ <- gsub(pattern="@t09code",replacement=sT09,x=strQ)
			strQ <- gsub(pattern="@t10code",replacement=sT10,x=strQ)
			strQ <- gsub(pattern="@t11code",replacement=sT11,x=strQ)
			strQ <- gsub(pattern="@t12code",replacement=sT12,x=strQ)
			strQ <- gsub(pattern="@t00code",replacement=sT00,x=strQ)
			## If Oracle SQL is called from SQL Server via OPENQUERY
			strQ <- gsub(pattern="@~sppcode",replacement=pppS,x=strQ)
			strQ <- gsub(pattern="@~tarcode",replacement=pppT,x=strQ)
			strQ <- gsub(pattern="@~alfcode",replacement=pALF,x=strQ)
			strQ <- gsub(pattern="@~trfcode",replacement=pTRF,x=strQ)
			strQ <- gsub(pattern="@~orfcode",replacement=pORF,x=strQ)
			strQ <- gsub(pattern="@~tffcode",replacement=pTFF,x=strQ)
			strQ <- gsub(pattern="@~offcode",replacement=pOFF,x=strQ)
			strQ <- gsub(pattern="@~carcode",replacement=pCAR,x=strQ)
			strQ <- gsub(pattern="@~ssscode",replacement=pSSS,x=strQ)
			strQ <- gsub(pattern="@~invcode",replacement=pINV,x=strQ)
			strQ <- gsub(pattern="@~t01code",replacement=pT01,x=strQ)
			strQ <- gsub(pattern="@~t02code",replacement=pT02,x=strQ)
			strQ <- gsub(pattern="@~t03code",replacement=pT03,x=strQ)
			strQ <- gsub(pattern="@~t04code",replacement=pT04,x=strQ)
			strQ <- gsub(pattern="@~t05code",replacement=pT05,x=strQ)
			strQ <- gsub(pattern="@~t06code",replacement=pT06,x=strQ)
			strQ <- gsub(pattern="@~t07code",replacement=pT07,x=strQ)
			strQ <- gsub(pattern="@~t08code",replacement=pT08,x=strQ)
			strQ <- gsub(pattern="@~t09code",replacement=pT09,x=strQ)
			strQ <- gsub(pattern="@~t10code",replacement=pT10,x=strQ)
			strQ <- gsub(pattern="@~t11code",replacement=pT11,x=strQ)
			strQ <- gsub(pattern="@~t12code",replacement=pT12,x=strQ)
			strQ <- gsub(pattern="@~t00code",replacement=pT00,x=strQ)
			##-------------------------------------------------------
			strQ <- gsub(pattern="@mnwt",replacement=mnwt,x=strQ)
			strQ <- gsub(pattern="@mindep",replacement=ifelse(is.null(mindep),0,mindep),x=strQ)
			strQ <- gsub(pattern="@maxdep",replacement=ifelse(is.null(maxdep),1200,maxdep),x=strQ)
			if (is.null(survserid) && is.null(surveyid))
				strQ <- gsub(pattern="@originid",replacement="'Y'",x=strQ)
			else
				strQ <- gsub(pattern="@originid",replacement="'Y','N'",x=strQ)
#browser();return()
			if (is.null(surveyid)) {
				SQLdat = .getSQLdata(dbName="GFBioSQL", qtName=NULL, strSQL="select SURVEY_ID FROM SURVEY", 
					server=.PBSserver[1], type="SQL", trusted=subQtrust[["trusted"]], uid=subQtrust[["uid"]], pwd=subQtrust[["pwd"]])
				surveyid = sort(unique(SQLdat[[1]]))
			}
			strQ <- gsub(pattern="@surveyid",replacement=paste(surveyid,collapse=","),x=strQ)
			if (is.null(survserid)) {
				SQLdat = .getSQLdata(dbName="GFBioSQL", qtName=NULL, strSQL="select SURVEY_SERIES_ID FROM SURVEY", 
					server=.PBSserver[1], type="SQL", trusted=subQtrust[["trusted"]], uid=subQtrust[["uid"]], pwd=subQtrust[["pwd"]])
				survserid = sort(unique(SQLdat[[1]]))
			}
			strQ <- gsub(pattern="@survserid",replacement=paste(survserid,collapse=","),x=strQ)
			strQ <- gsub(pattern="@fisheryid",replacement=ifelse(is.null(fisheryid),
				paste(0:9,collapse=","), paste(fisheryid,collapse=",") ),x=strQ)
			#strQ <- gsub(pattern="@logtype",replacement=ifelse(is.null(logtype),"'FISHERLOG','OBSERVRLOG'",
			#	paste("'",logtype,"'",sep="")),x=strQ) ### in case we wish to mirror 'fisheryid'
			strQ <- gsub(pattern="@logtypeval",replacement=ifelse(is.null(logtype),"'FISHERLOG'",
				paste("'",logtype,"'",sep="")),x=strQ)
			strQ <- gsub(pattern="@doorsval",replacement=ifelse(is.null(doors),72,doors),x=strQ)
			strQ <- gsub(pattern="@speedval",replacement=ifelse(is.null(speed),95,speed),x=strQ)
			strQ <- gsub(pattern="@major",replacement=ifelse(is.null(major),
				paste(c(1,3:9),collapse=","), paste(major,collapse=",") ),x=strQ)
			strQ <- gsub(pattern="@top",replacement=ifelse(is.null(top),20,top),x=strQ)
			strQ <- gsub(pattern="@gear",replacement=ifelse(is.null(gear),
				paste(0:99,collapse=","), paste(gear,collapse=",") ),x=strQ)
			strQ <- gsub(pattern="@dummy",replacement=ifelse(is.null(dummy),"''",  ## default does not work for numeric
				ifelse(is.numeric(dummy),paste(dummy,collapse=","),
				ifelse(is.character(dummy),paste("'",paste(dummy,collapse="','"),"'",sep=""),"''"))),x=strQ)
			ttput(strQ)
#browser();return()
			expr <-paste("datt=.getSQLdata(dbName=\"",dbName,"\",strSQL=\"",strQ,
			"\",server=\"",server,"\",type=\"",type,"\",trusted=",trusted,
			",uid=\"",uid,"\",pwd=\"",pwd,"\",...)",sep="")
			timeQ0=proc.time()[1:3]  ### start timing SQL query
#browser();return()
			eval(parse(text=expr)) 
			timeQ = round(proc.time()[1:3]-timeQ0,2)
		}
		if (!is.data.frame(datt)) { 
			odbcCloseAll()
			showError(paste("SQL Query NOT executed",paste(datt[1:(length(datt)-1)],collapse="\n"),sep="\n\n"),cex=1) }
		PBSdat <- datt
		if (!is.null(strSpp)) { 
			attr(PBSdat,"spp") <- strSpp;  attr(PBSdat,"sql") <- strQ }
		attr(PBSdat,"db") <- dbName;  attr(PBSdat,"fqt") <- fqtName
	}
	else {
		assign("PBSdat",NULL,envir=tenv)
		showError(paste("No",type,"table available"))
	}
	if (noFactors) {
		z <- sapply(PBSdat,is.factor)
		if (any(z==TRUE)) {
			for (i in (1:length(z))[z]) PBSdat[,i] <- as.character(PBSdat[,i]) } }
	if (noLogicals) {
		z <- sapply(PBSdat,is.logical)
		if (any(z==TRUE)) {
			for (i in (1:length(z))[z]) PBSdat[,i] <- substring(as.character(PBSdat[,i]),1,1) } }
	assign("PBSdat",PBSdat,envir=tenv)
	timeF = round(proc.time()[1:3]-timeF0,2)
	if (type %in% c("SQL","ORA"))  FQtime = rbind(timeF,timeQ)
	else FQtime = timeF
	assign("FQtime",FQtime,envir=tenv)
	assign("sql",strQ,envir=tenv)
	junk=gc(verbose=FALSE) ### garbage collection (shunt messages to junk also)
	invisible(strQ)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~getData

## .getSQLdata--------------------------2021-01-14
##  Retrieves a data frame from SQL Server
## ---------------------------------------------RH
.getSQLdata <- function(dbName, qtName=NULL, strSQL=NULL,
     server=NULL, type="SQL", trusted=TRUE, uid="", pwd="", 
     rownum=0,...) {
	#if (!require(RODBC, quietly=TRUE)) stop("`RODBC` package is required")
	## Use forward slashes "/" for server otherwise the translation
	## is too dependent on the number of times "\" is escaped
	if (is.null(server) || server=="") {
		#getFile(".PBSserver", path = .getSpath(),tenv=penv())
		server = .PBSserver[1]; type="SQL" }
	driver= list(...)$driver
#browser();return()
	if (type=="SQL") driver=ifelse(is.null(driver),"SQL Server",driver)
	#else if (type=="ORA") driver="Oracle ODBC Driver" ## "Microsoft ODBC for Oracle"
	#else if (type=="ORA") driver=ifelse(is.null(driver),"Oracle in OraClient11g_home1",driver)
	else if (type=="ORA") driver=ifelse(is.null(driver),"Oracle in OraClient12Home1_32bit",driver) ## RH 210114
	else showError("Only 'SQL' and 'ORA' supported at present")
	syntax <- paste("Driver={",driver,"}",
		ifelse(type=="ORA" && substring(driver,1,1)=="O",";DBQ=",";Server="),server,
		";Database=",dbName,";Trusted_Connection=",ifelse(trusted,"Yes","No"),sep="")
	if (!trusted) syntax <- paste(syntax,";UID=",uid,";PWD=",pwd,sep="")
	if (type=="ORA") syntax = paste(syntax,";TLO=0;QTO=F",sep="")
	syntax=gsub("/","\\\\",syntax)  ## finally convert "/" to "\\"
	## does not work?: syntax2="Driver={Oracle in OraClient12Home1_32bit};Server=(DESCRIPTION=(ADDRESS=(PROTOCOL=TCP)(HOST=VSBCIOSXP75.ENT.DFO-MPO.CA)(PORT=1521))(CONNECT_DATA=(SERVICE_NAME=ORAPROD)));Database=HARVEST_V2_0;Trusted_Connection=No;UID=haighr;PWD=haighr;TLO=0;QTO=F"
#if (type=="ORA") {browser();return()}
	assign("cns",syntax,envir=.PBStoolEnv)
	cnn <- odbcDriverConnect(connection=syntax)
	if (is.null(qtName) && is.null(strSQL))
		showError("Must specify either 'qtName' or 'strSQL'")
	if (!is.null(qtName)) {
		## sqlFetch doesn't work for Oracle tables (thanks Norm)
		if (type=="ORA")
			query=paste("SELECT * FROM ", paste(dbName,qtName,sep="."),
				ifelse(rownum>0,paste(" WHERE ROWNUM <=",rownum),""),sep="")
		else
			query=paste("SELECT ",ifelse(rownum>0,paste("TOP",rownum),"")," * FROM ",qtName,sep="")
		dat <- sqlQuery(cnn, query, rows_at_time=1) }
	else { 
		arg.list = list(...)[!is.element(names(list(...)),"driver")]
		## seems you cannot just pass a list into ..., even if ... is rendered as a list by the function.
		if (length(arg.list)>0) { 
			arg.vec = sapply(names(arg.list),function(x){paste(x,"=",paste(deparse(arg.list[[x]]),collapse=""),sep="")}) ## deparse breaks lines
			args = paste(arg.vec,collapse=", ")
			expr = paste("dat = sqlQuery(cnn, strSQL, ",args,")",sep="")
#print(expr); #browser()
			eval(parse( text=expr ))
		} else {
			dat <- sqlQuery(cnn, strSQL) #, list(...)[!is.element(names(list(...)),"driver")] ) #...) #, believeNRows=ifelse(type=="ORA",FALSE,TRUE))
		}
		if (is.data.frame(dat) && nrow(dat)==0) 
			showMessage("No records returned. Maybe try again with 'rows_at_time=1'.",col="blue")
	}
	odbcClose(cnn)
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.getSQLdata

## .getMDBdata--------------------------2010-07-22
##  Retrieves a data frame from MDB query or table
## ---------------------------------------------RH
.getMDBdata <- function(mdbTable, qtName, rownum=0, ...)
{
	#if (!require(RODBC, quietly=TRUE)) stop("`RODBC` package is required")
	cnn <- odbcConnectAccess(access.file=mdbTable)
	query=paste("SELECT ",ifelse(rownum>0,paste("TOP",rownum),"")," * FROM ",qtName,sep="")
	dat <- sqlQuery(cnn, query, ...)
	odbcClose(cnn)
	return(dat)
}

## .getDBFdata--------------------------2010-07-22
##  Retrieves data from a DBF file
## ---------------------------------------------RH
.getDBFdata <- function(dbfTable, qtName, ...)
{
	if (nchar(qtName)>8) showError("Rename DBF file using 8 or less characters")
	#if (!require(RODBC, quietly=TRUE)) stop("`RODBC` package is required")
	cnn <- odbcConnectDbase(dbf.file=dbfTable)
	dat <- sqlQuery(cnn, paste("SELECT * FROM",qtName),...)
	odbcClose(cnn)
	return(dat)
}

## .getXLSdata--------------------------2013-01-30
##  Retrieves data from an XLS worksheet
##  Note: Data truncated to 255 characters with Excel ODBC driver
##  Fix : http://support.microsoft.com/kb/189897/en-us
##  regjump HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Jet\4.0\Engines\Excel
##  Set TypeGuessRows DWORD Value to 0 (scan all rows). OK for most small tables.
## ---------------------------------------------RH
.getXLSdata <- function(xlsTable, qtName, ...)
{
	#if (!require(RODBC, quietly=TRUE)) stop("`RODBC` package is required")
	cnn <- odbcConnectExcel(xls.file=xlsTable)
	dat <- sqlFetch(cnn, qtName, ...)
	odbcClose(cnn)
	return(dat)
}
##===================================getData group


#getFile--------------------------------2013-01-25
# If a user specifies a source environment (senv)
#  then function only looks there. Otherwise,
#  function searches sequentially through frames or
# Gets datasets searching:
#  1. binary libraries, 2. binary local (.rda), 
#  3. dumped data (.r), 4. comma-delimited text files (.csv,.txt)
#-----------------------------------------------RH
getFile <- function(..., list=character(0), path=getwd(), 
   senv=NULL, tenv=.GlobalEnv, use.pkg=FALSE, 
   reload=FALSE, try.all.frames=FALSE, use.all.packages=FALSE) {

	gfnam = c(as.character(substitute(list(...))[-1L]), list)
	if (is.null(gfnam)) return(invisible())
	if (!is.null(senv)) {
		gfnot = character()
		for (i in gfnam){
			if (isThere(i,envir=senv)) {
				dat = get(i,envir=senv)
				assign(i,dat,envir=tenv)
			} else
				#print(paste("object `",i,"` not found in specified environment",sep=""))
				gfnot = c(gfnot,i)
		}
		if (length(gfnot)==0) {
			junk = gc(verbose=FALSE)
			return(invisible()) }
		else gfnam = gfnot
	}
	fenv = parent.frame(1) # frame environment
	fraN=sys.nframe()
	if (try.all.frames) tryN=(fraN-1):0 else tryN=fraN-1 # parent frame / parent frame and back to global
	names(gfnam)=rep(-1,length(gfnam))
	for (i in gfnam) {
		zi=is.element(gfnam,i) # placement boolean
		for (j in tryN) {
			jenv=sys.frame(j)
			if (isThere(i,envir=jenv)) {
				ival=get(i,envir=jenv)
				if (length(ival)==1 && is.character(ival))
					gfnam[zi]=get(i,envir=fenv) # variable string name
				if (isThere(gfnam[zi],envir=jenv))
					names(gfnam)[zi]=j
				break
			} # exists
		}    # end j loop
	}       # end i loop
	if (use.pkg) {
		warn <- options()$warn; options(warn=-1)
		# available datsets from packages either loaded or installed (latter can be slow)
		Gdata <- sort(unique(data(package=.packages(all.available=use.all.packages))$results[,"Item"])) 
		options(warn=warn) }
	for (i in gfnam) {
		ii=as.numeric(names(gfnam)[match(i,gfnam)])
		if ( reload || !any(i==ls(envir=tenv)) ) { 
			if (!reload && ii >= 0) 
				eval(parse(text=paste("assign(\"",i,"\",get(\"",i,"\",envir=sys.frame(",ii,")),envir=tenv)",sep="")))
			else if (!reload && use.pkg && is.element(i,Gdata) )
				eval(parse(text = paste("data(", i, ", envir=tenv)", sep = "")))
			else {
				rda = paste(path,"/",i,".rda",sep=""); rnam = paste(path,"/",i,".r", sep = "")
				csv = paste(path,"/",i,".csv",sep=""); txt  = paste(path,"/",i,".txt", sep = "")
				if (file.exists(rda)) load(rda,envir=tenv)
				else if (file.exists(rnam)) sys.source(rnam,envir=tenv)
				else if (file.exists(csv) || file.exists(txt)) {
					if (file.exists(csv)) TXT=csv else TXT=txt
					expr=paste("temp=read.table(\"",TXT,"\",header=TRUE,sep=\",\",stringsAsFactors=FALSE)",sep="")
					expr=paste(expr,paste("assign(\"",i,"\",temp,envir=tenv)",sep=""),sep=";")
					eval(parse(text=expr)) } 
				else {
					envN=environmentName(tenv);  print(tenv)
					if (envN=="") envN=paste("local environment (frame",
						ifelse(length(tryN)>1,"s "," "),paste(tryN,collapse=","),")",sep="")
					mess=paste("'",i,"' cannot be found. Checked:",sep="")
					if (use.pkg) 
						mess=c(mess,"Binaries located in installed packages;\n")
					mess=c(mess,paste("Objects in ",envN,";",sep=""))
					mess=c(mess,paste("RDA, CSV, TXT files in\n",path))
					if(!use.pkg) 
						mess=c(mess,"\n\nTry looking for data in packages using arguments:\n",
							"'use.pkg=TRUE'", "(defaults to loaded packages)",
							"\nAdd 'use.all.packages=TRUE'","(looks in all packages installed in user's R library)") 
					showError(paste(mess,collapse="\n"),as.is=TRUE) 
				}
			}
		} # end if reload
	}    # end i loop
	junk=gc(verbose=FALSE) # garbage collection (shunt messages to junk also)
	invisible() }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~getFile


## getName------------------------------2009-05-11
##  Get the names of the input object.
##  If fnam exists as a list, it returns the names of the list.
##  If fnam exists as a string vector, it returns the strings.
##  If fnam does not exist, it simply returns itself.
## ---------------------------------------------RH
getName = function(fnam)
{
	snam = as.character(substitute(fnam))
	penv = parent.frame(1)
	if (any(snam==ls(envir=penv))) {
		temp=get(snam,envir=penv)
		if (is.list(temp)) {
			fnam=names(temp); type="list"; len=length(temp)
			if (is.null(fnam)) showError(paste("'",snam,"' is a list of ",len," objects with no names",sep=""))
			fnam=setdiff(fnam,"") }
		else if (all(sapply(temp,is.character)==TRUE)) {
			fnam = get(snam,envir=penv); type="vector"; len=length(fnam) }
		else  showError(paste("'",snam,"' is a non-character vector",sep=""))
	} else {
		fnam = snam; type="literal"; len=1 }
	attr(fnam,"type")=type; attr(fnam,"len")=len
	return(fnam)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~getName


#getODBC--------------------------------2016-04-12
# Get a string vector of ODBC drivers installed on user's Windows system.
# Code: Scripting Guy
# URL : http://blogs.technet.com/b/heyscriptingguy/archive/2005/07/07/how-can-i-get-a-list-of-the-odbc-drivers-that-are-installed-on-a-computer.aspx
#--------------------------------------------SG/RH
getODBC <- function(os=.Platform$OS.type, pattern=NULL, status="Installed") {
	if (os!="windows") {
		err="'getODBC' needs Windows OS to use Windows Scripting"
		cat(err,"\n"); return(invisible(err)) }
	tdir <- tempdir()
	fname <- paste(tdir, "\\getODBC.vbs", sep="")
	cat('Const HKEY_LOCAL_MACHINE = &H80000002\n', file=fname)
	cat('strComputer = "."\n', file=fname, append=TRUE)
	cat('Set objRegistry = GetObject("winmgmts:\\\\" & strComputer & "\\root\\default:StdRegProv")\n', file=fname, append=TRUE)
	cat('strKeyPath = "SOFTWARE\\ODBC\\ODBCINST.INI\\ODBC Drivers"\n', file=fname, append=TRUE)
	cat('objRegistry.EnumValues HKEY_LOCAL_MACHINE, strKeyPath, arrValueNames, arrValueTypes\n', file=fname, append=TRUE)
	cat('For i = 0 to UBound(arrValueNames)\n', file=fname, append=TRUE)
	cat('  strValueName = arrValueNames(i)\n', file=fname, append=TRUE)
	cat('  objRegistry.GetStringValue HKEY_LOCAL_MACHINE,strKeyPath,strValueName,strValue\n', file=fname, append=TRUE)
	cat('  Wscript.Echo arrValueNames(i) & " -- " & strValue\n', file=fname, append=TRUE)
	#cat('  Wscript.Echo arrValueNames(i)\n', file=fname, append=TRUE)
	cat('Next\n', file=fname, append=TRUE)
	odbcAll  = system(paste("cscript //NoLogo", fname), minimized=TRUE, intern=TRUE)
	odbcList = strsplit(odbcAll,split=" -- ")
	odbcStat = sapply(odbcList,function(x){x[1]})
	if (!is.null(status)) {
		isStatus = sapply(odbcList,function(x){x[2]==status})
		odbcStat = odbcStat[isStatus]
	}
	odbcOut = odbcStat
	if (!is.null(pattern))
		odbcOut = odbcStat[grep(pattern,odbcStat)]
	packList(c("odbcAll","odbcList","odbcStat","odbcOut"),target="PBStool",tenv=.PBStoolEnv)
	invisible(odbcOut) }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~getODBC


#installPkgs----------------------------2016-05-18
# Install specified packages if they are missing
# or if newer versions are available.
# Note: At some point the function should deal
#   with attached packages (use sessionInfo()).
#-----------------------------------------------RH
installPkgs <- function(pkg, repos=getOption("repos"), locdir=tempdir(), also.load=FALSE, ...)
{
	old.ver = rep("0",length(pkg)); names(old.ver) = pkg
	ins.pkg = pkg[(pkg %in% installed.packages()[, "Package"])]   ## installed
	mis.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]  ## missing
	if (is.null(repos)) {
		bins    = list.files(locdir,pattern="\\.zip$")
		if (length(bins)==0) stop("No zip files available")
		pvlist  = strsplit(sub("\\.zip$","",sub("_","+",bins)),split="\\+")
		new.ver = sapply(pvlist,function(x){v=x[2]; names(v)=x[1]; return(v)})
	} else {
		ava.pkg = available.packages() #type="binary")
		new.ver = ava.pkg[,"Version"]
	}
	new.pkg= names(new.ver)
	if (length(ins.pkg)>0) {
		ins.ver = sapply(ins.pkg, function(x){as.character(packageVersion(x))})
		old.ver[names(ins.ver)] = ins.ver
		int.ver = ins.ver[names(ins.ver)%in%names(new.ver)]  ## intersection -- installed intersects newly available
		int.pkg = names(int.ver)
		upd.pkg = int.pkg[(ins.ver[int.pkg] < new.ver[int.pkg])]
	} else
	upd.pkg = as.character()
	if (length(mis.pkg)>0)
		upd.pkg = c(upd.pkg, intersect(mis.pkg,new.pkg))
	if (length(upd.pkg)>0){
		if (is.null(repos))
			install.packages(paste0(locdir,"/",bins[sapply(upd.pkg,grep,bins)]), repos=NULL, ...)
		 else
			install.packages(upd.pkg, repos=repos, ...)
	} else
	cat("No new versions of requested packages installed\n")
	if (also.load)
		sapply(pkg, require, character.only = TRUE)
	invisible(new.ver)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~installPkgs


## inWord-------------------------------2018-11-13
## Find morphemes (parts of words) in word and report T/F
## Based on code supplied by user 'rawr' at:
## https://stackoverflow.com/questions/33483286/r-look-for-abbreviation-in-full-string
## ---------------------------------------------RH
inWord = Vectorize(function(word, morpheme, prefix=FALSE, suffix=FALSE)
{
	## Set prefix=TRUE to match beginning of word
	## Set suffix=TRUE to match ending of word
	mm <- strsplit(tolower(morpheme), "")[[1]]
	#grepl(paste0(mm, collapse = "[a-z]*?"), word)
	## Add this if you only want to consider letters in word
	expr = paste0(mm, collapse = sprintf("[%s]*?", tolower(word)))
	if (is.logical(prefix) && prefix)
		expr = paste0("^",expr)
	if (is.logical(suffix) && suffix)
		expr = paste0(expr,"$")
	grepl(expr, word)
}, vectorize.args = "morpheme")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#inWord


#isThere--------------------------------2009-06-18
# Check to see if object physically exists in the specified environment.
#-----------------------------------------------RH
isThere = function(x, envir=parent.frame()) {
	if (class(x)[1]!="character")
		showError(paste("isThere()\n'x' needs to be a character string"),as.is=TRUE)
	x %in% ls(envir=envir,all.names=TRUE) }


#lenv-----------------------------------2010-05-25
# Get the local/parent/global environment.
#-----------------------------------------------RH 
	lenv = function(){ sys.frame(sys.nframe()-1) } # local environment (probably the same as parent.frame())
	penv = function(){ parent.frame() }            # parent environment
	genv = function(){ .GlobalEnv }                # global environment


## linguaFranca-------------------------2024-02-14
## Translate English phrases to French (other languages possible)
## for use in plotting figures with French labels.
## Note that 'gsub' has a limit to its nesting depth.
## little     -- number of characters that defines a little word that is only translated when it appears by itself.
## strip      -- if TRUE, strip names off the output vector.
## localnames -- If TRUE, deal with locality names separately form the main function.
## ---------------------------------------------RH
linguaFranca = function(x, lang="e", little=4, strip=FALSE, localnames=FALSE)
{
	if (!is.element(lang, c("e","f")))
		stop("Only 'e' or 'f' supported for argument 'lang'")
	if (length(x)==1 && is.expression(x)) {
		if (lang=="e") return(x)
		else {
			is.expr=TRUE; x=as.character(x)}
		}
	else is.expr=FALSE # return(x)
	if (lang=="e" || is.null(x) || is.na(x) || x=="") return(x)
	## Need to sort so that longer strings are processed before shorter ones,
	##   otherwise, partial matching occurs and the translation is not correct.
	if (lang=="f") {
		##---------START LOCALITY NAMES---------
		## Deal with locality names separately form the main function when localnames=TRUE
		if (localnames) {
			locals = x
			locals = gsub("Below Middle Bank","Middle Bank (below)",locals)
			locals = gsub("Deep Big Bank","Big Bank (deep)",locals)
			localset = strsplit(locals,"/")
			locawords = lapply(localset,strsplit, split=" ")
			locowords = sapply(locawords,function(xx){
				locoset = sapply (xx, function(xxx){
					## Shift words to the front
					pattern = "[Oo]ff|[Ii]sland|[Cc]anyon|[Ss]ound|[Ss]pit|[Bb]ay|[Pp]oint|[Ss]pot|[Ii]nlet|[Dd]oughnut|[Bb]ank|[Ll]ake|[Ee]ntrance"
#browser();return()
					if (any(grepl(pattern,xxx))) {
						xrev = c(xxx[grep(pattern,xxx)],xxx[grep(pattern,xxx,invert=TRUE)])
						return(paste0(xrev,collapse=" "))
					}
					## Shift words to the back
					pattern = "[Nn]orth|[Ss]outh|[Ee]ast|[Ww]est|[S][E]|[S][W]|[Dd]eep|[Oo]utside"
					if (any(grepl(pattern,xxx))) {
						xrev = c(xxx[grep(pattern,xxx,invert=TRUE)],xxx[grep(pattern,xxx)])
						return(paste0(xrev,collapse=" "))
					}
					else return(paste0(xxx,collapse=" "))
				})
				return(paste0(locoset,collapse="/"))
			})
			## Locality names
			xloco = sapply(locowords, function(xx){
				gsub("fm", "br",
				gsub(" and ", " et ",
				gsub(" to ", eval(parse(text=deparse(" \u{00E0} "))),
				gsub("[Bb]ay", "Baie",
				gsub("[Bb]ig", "Grande",
				gsub("[S][W]", "S-o",
				gsub("[S][E]", "S-e",
				gsub("[Bb]ank", "Banque",
				gsub("[Cc]ape", "Cap",
				gsub("[Dd]eep", "Profonde",
				gsub("[Ee]ast", "Est",
				gsub("[Ll]ake", "Lac",
				gsub("[Ss]pot", "Place",
				gsub("[Ss]pit", "Broche",
				gsub("[Ww]est", "Ouest",
				gsub("\\(deep)", "(profonde)",
				gsub("[Nn]orth", "Nord",
				gsub("[Ss]outh", "Sud",
				gsub("[Ii]nlet", eval(parse(text=deparse("Entr\u{00E9}e"))),
				gsub("[Ss]ound", eval(parse(text=deparse("D\u{00E9}troit"))),
				gsub("\\(below)", "(dessous)",
				gsub("[Ii]sland", eval(parse(text=deparse("\u{00CE}le"))),
				gsub("[Ff]ather", eval(parse(text=deparse("P\u{00E8}re"))),
				gsub("[Mm]iddle", "moyen",
				gsub("[Ii]nshore", eval(parse(text=deparse("C\u{00F4}tier"))),
				gsub("[Oo]utside", "Dehors",
				gsub("[Dd]oughnut", "Beignet",
				gsub("\\(shallow)", "(peu profonde)",
				gsub("[Ee]ntrance", eval(parse(text=deparse("Entr\u{00E9}e"))),
				gsub("[Oo]ffshore", eval(parse(text=deparse("Extrac\u{00F4}tier"))),
				gsub("(\\s+)?[Oo]ff(\\s+)?", "\\1au large\\2",
				xx)))))))))))))))))))))))))))))))
			})
#browser();return()
			return(xloco)
		}
		##---------END LOCALITY NAMES---------

		## Start the regular functionality of linguaFranca
		## -----------------------------------------------
		xout    = rep("",length(x)); names(xout) = x
		nchars  = sapply(x,function(x0) { nchar(gsub("^([[:punct:]]+) |s$| ([[:digit:]]+)","",x0)) } )  ## remove 's' and digits to catch small-word plurals
		zlil    = nchars<=little; lilword = x[zlil]; xLpos=(1:length(x))[zlil]
		zbig    = nchars>little;  bigword = x[zbig]; xBpos=(1:length(x))[zbig]
		##---------START LITTLE SINGLE WORDS---------
		if (any(zlil)) {
			## les petits mots de bouche (stand-alone words)
			xlil = sapply(lilword, function(xx){
				gsub("[Aa]nd", "et",
				gsub("[Aa]vg", "moy",
				gsub("[Aa]ge", eval(parse(text=deparse("\u{00E2}ge"))),
				gsub("[Aa]ll", "tous",
				gsub("[Ll]ag", eval(parse(text=deparse("d\u{00E9}calage"))),
				#gsub("[Rr]un", eval(parse(text=deparse("Ex\u{00E9}"))),
				gsub("[Rr]un", "Sim", ## Paul Marchal
				gsub("[Ss]ex", "sexe",
				gsub("[Yy]ear", eval(parse(text=deparse("ann\u{00E9}e"))),
				gsub("[Mm]ale", eval(parse(text=deparse("m\u{00E2}le"))),
				gsub("[Gg]ear", eval(parse(text=deparse("\u{00E9}quipement"))),
				gsub("[Mm]ean", "moyenne",
				gsub("[Bb]oth", "tous les deux",
				gsub("[Tt]rap", "casier", #eval(parse(text=deparse("pi\u{00E8}ge"))),
				gsub("[Cc]om(m?)", "com",
				gsub("[Rr]es(e?)", "rec",
				gsub("[Ss]ur(v?)", "rel",
				xx))))))))))))))))
			})
			## dat abbreviations
			xlil = sapply(xlil, function(xx){
				gsub("[Jj]an", "jan",
				gsub("[Ff]eb", eval(parse(text=deparse("f\u{00E9}v"))),
				gsub("[Mm]ar", "mar",
				gsub("[Aa]pr", "avr",
				gsub("[Mm]ay", "mai",
				gsub("[Jj]un", "juin",
				gsub("[Jj]ul", "juil",
				gsub("[Aa]ug", eval(parse(text=deparse("ao\u{00FB}"))),
				gsub("[Ss]ep", "sep",
				gsub("[Oo]ct", "oct",
				gsub("[Nn]ov", "nov",
				gsub("[Dd]ec", eval(parse(text=deparse("d\u{00E9}c"))),
				xx))))))))))))
			})
			## species acronyms
			xlil = sapply(xlil, function(xx){
				gsub("[A][R][F]", "PGB",  ## Arrowtooth Flounder
				gsub("[B][O][R]", "SBO",  ## Bocaccio
				gsub("[B][S][R]", "STN",  ## Blackspotted
				gsub("[C][A][R]", "SCA",  ## Canary
				gsub("[C][P][R]", "SCU",  ## Copper
				gsub("[L][S][T]", eval(parse(text=deparse("SL\u{00C9}"))),  ## Longspine
				gsub("[P][O][P]", "SLM",  ## Pacific Ocean Perch
				gsub("[Q][B][R]", eval(parse(text=deparse("SD\u{00C9}"))),  ## Quillback
				gsub("[R][B][R]", "SBR",  ## Redbanded
				gsub("[R][E][R]", eval(parse(text=deparse("SO\u{00C9}"))),  ## Rougheye
				gsub("[R][O][L]", "FAL",  ## Rock Sole (fausse limande)
				gsub("[R][S][R]", "SRR",  ## Redstripe
				gsub("[S][G][R]", "SAR",  ## Silvergray
				gsub("[S][K][R]", "SBL",  ## Shortraker
				gsub("[S][S][T]", eval(parse(text=deparse("SC\u{00C9}"))),  ## Shortspine
				gsub("[W][A][P]", "GLA",  ## Walleye Pollock
				gsub("[W][W][R]", "SVV",  ## Widow
				gsub("[Y][M][R]", "SBJ",  ## Yellowmouth
				gsub("[Y][T][R]", "SQJ",  ## Yellowtail
				gsub("[Y][Y][R]", "SYJ",  ## Yelloweye
				gsub("[R][E][B][S]", eval(parse(text=deparse("SO\u{00C9}TN"))),  ## Rougheye/Blackspotted
				xx)))))))))))))))))))))
			})
			## Area acronyms
			xlil = sapply(xlil, function(xx){
				gsub("^[B][C]$", "C-B",
				gsub("[H][S]", "DH",
				gsub("[B][C][C]", "CBc",
				gsub("[B][C][N]", "CBn",
				gsub("[B][C][S]", "CBs",
				gsub("[C][S][T]", eval(parse(text=deparse("C\u{00D4}TE"))),
				gsub("[D][F][O]", "MPO",
				gsub("[P][B][S]", "SBP",
				gsub("[Q][C][S]", "BRC",
				gsub("[H][B][L][L]", "PFD", ## palangre \`{a} fond dur
				gsub("[N][M][F][S]", "SNPM", ## Service national des p\^{e}ches maritimes
				gsub("[W][C][H][G]", "COHG",
				gsub("[W][C][V][I]", "COIV",
				xx)))))))))))))
			})
			## ridiculous acronyms
			xlil = sapply(xlil, function(xx){
#browser();return()
				gsub("[B][T]", "CF",             ## bottom trawl   = chalut de fond
				gsub("[C][C]", "PC",             ## constant catch   = prise constante
				gsub("[M][W]", "CP",             ## midwater trawl = chalut p\'{e}lagique
				gsub("[H][R]", "TR",             ## harvest rate   = taux de r\'{e}colte (not taux d'exploitation)
				gsub("[D][F]", "DL",             ## degrees of freedom = degr\'{e}s de libert\'{e}
				gsub("[d][f]", "dl",             ## degrees of freedom = degr\'{e}s de libert\'{e}
				gsub("[S][A]", eval(parse(text=deparse("\u{00C9}S"))),  ## stock assessment  = \'{e}valuation des stocks
				gsub("[A][C][F]", "FAC",         ## fonction d'autocorr\'{e}lation
				gsub("[G][M][A]", "ZGPF",        ## les zones de gestion des poissons de fond 
				gsub("[G][M][U]", "GGPF",        ## le groupe de gestion du poisson de fond
				gsub("[L][R][P]", "PRL",         ## point de r\'{e}f\'{e}rence limite
				gsub("[M][P][D]", "MDP",         ## mode de distribution post\'{e}rieure
				gsub("[M][S][Y]", "RMD",         ## rendement maximal durable (no longer soutenu)
				gsub("[m][s][y]", "rmd",         ## rendement maximal durable (no longer soutenu)
				gsub("[R][S][S]", "SRC",         ## somme r\'{e}siduelle de carr\'{e}s
				gsub("[T][R][P]", "PRC",         ## point de r\'{e}f\'{e}rence cible
				gsub("[U][S][R]", "RSS",         ## r\'{e}f\'{e}rence de stock sup\'{e}rieure
				gsub("[I][P][H][C]", "CIFP",     ## Commission internationale du fl\'{e}tan du Pacifique
				gsub("[H][B][L][L]", eval(parse(text=deparse("P\u{00E0}FD"))), ## palangre \`{a} fond dur
				gsub("[M][C][M][C]", "MCCM",     ## Monte Carlo \`{a} cha\^{i}ne de Markov
				xx))))))))))))))))))))
			})
			xout[xLpos] = xlil
		}
#browser();return()
		##---------END LITTLE SINGLE WORDS----------

		##---------START BIG/MULTIPLE WORDS---------
		## words or phrases that appear in strings
		if (any(zbig)) {
			## Sensitivity run labels
			xsen = sapply(bigword, function(xx){
				gsub("split M ages\\(([0-9]+),([0-9]+)\\)", "diviser M entre \\1 et \\2 ans",
				gsub("AE([0-9]) no age error", "EV\\1 pas d'erreur de vieillissement",
				gsub("AE([0-9]) age reader CV", eval(parse(text=deparse("EV\\1 CV des lecteurs d'\u{00E2}ge"))),
				gsub("AE([0-9]) CASAL CV=0.1", "EV\\1 CASAL CV=0,1",
				gsub("reduce catch ([0-9]+)\\%", eval(parse(text=deparse("r\u{00E9}duire les prises de \\1%"))),
				gsub("increase catch ([0-9]+)\\%", "augmenter les prises de \\1%",
				gsub("sigmaR=([0-9])\\.([0-9]+)", "sigmaR=\\1,\\2",
				gsub("female dome select", eval(parse(text=deparse("s\u{00E9}lectivit\u{00E9} du d\u{00F4}me femelle"))),
				gsub("use AF HS WCHG", "utiliser les FA de DH et COHG",
				gsub("add HBLL survey(s)", eval(parse(text=deparse("ajouter relev\u{00E9}\\1 de P\u{00E0}FD"))),
				gsub("use Tweedie CPUE", "utiliser CPUE de Tweedie",
				gsub("remove comm CPUE", "supprimer la CPUE commerciale",
				gsub("use Francis reweight", eval(parse(text=deparse("utiliser la repond\u{00E9}ration de Francis"))),
				xx)))))))))))))
			})
			## rockfish species names
			xspp1 = sapply(xsen, function(xx){
				gsub("[Bb]ocaccio", eval(parse(text=deparse("s\u{00E9}baste bocace"))),
				gsub("[Tt]iger [Rr]ockfish", eval(parse(text=deparse("s\u{00E9}baste-tigre"))),
				gsub("[Ww]idow [Rr]ockfish", "veuve",
				gsub("[Cc]anary [Rr]ockfish", eval(parse(text=deparse("s\u{00E9}baste canari"))),
				gsub("[Cc]opper [Rr]ockfish", eval(parse(text=deparse("s\u{00E9}baste cuivr\u{00E9}"))),
				gsub("[Rr]ockfish [Ii]nside", eval(parse(text=deparse("s\u{00E9}baste \u{00E0} l'int\u{00E9}rieur"))),
				gsub("[Ii]nshore [Rr]ockfish", eval(parse(text=deparse("s\u{00E9}baste c\u{00F4}tier"))),
				gsub("[Rr]ockfish [Oo]utside", eval(parse(text=deparse("s\u{00E9}baste \u{00E0} l'ext\u{00E9}rieur"))),
				gsub("[Oo]ffshore [Rr]ockfish", eval(parse(text=deparse("s\u{00E9}baste extrac\u{00F4}tier"))),
				gsub("[Rr]ougheye [Rr]ockfish", eval(parse(text=deparse("s\u{00E9}baste \u{00E0} \u{0153}il \u{00E9}pineux"))),
				gsub("[Qq]uillback [Rr]ockfish", eval(parse(text=deparse("s\u{00E9}baste \u{00E0} dos \u{00E9}pineux"))),
				gsub("[Rr]edbanded [Rr]ockfish", eval(parse(text=deparse("s\u{00E9}baste \u{00E0} bandes rouges"))),
				gsub("[Rr]edstripe [Rr]ockfish", eval(parse(text=deparse("s\u{00E9}baste \u{00E0} raie rouge"))),
				gsub("[Rr]osethorn [Rr]ockfish", eval(parse(text=deparse("s\u{00E9}baste \u{00E0} l'\u{00E9}pine de rose"))),
				gsub("[Ss]harpchin [Rr]ockfish", eval(parse(text=deparse("s\u{00E9}baste \u{00E0} menton pointu"))),
				gsub("[Ss]plitnose [Rr]ockfish", eval(parse(text=deparse("s\u{00E9}baste \u{00E0} bec-de-li\u{00E8}vre"))),
				gsub("[Yy]elloweye [Rr]ockfish", eval(parse(text=deparse("s\u{00E9}baste aux yeux jaunes"))),
				gsub("[Ss]hortraker [Rr]ockfish", eval(parse(text=deparse("s\u{00E9}baste bor\u{00E9}al"))),
				gsub("[Yy]ellowtail [Rr]ockfish", eval(parse(text=deparse("s\u{00E9}baste \u{00E0} queue jaune"))),
				gsub("[Ll]ongspine [Tt]hornyhead", eval(parse(text=deparse("s\u{00E9}bastolobe \u{00E0} longues \u{00E9}pines"))),
				gsub("[Yy]ellowmouth [Rr]ockfish", eval(parse(text=deparse("s\u{00E9}baste \u{00E0} bouche jaune"))),
				gsub("[Bb]lackspotted [Rr]ockfish", eval(parse(text=deparse("s\u{00E9}baste \u{00E0} taches noires"))),
				gsub("[Ss]hortspine [Tt]hornyhead", eval(parse(text=deparse("s\u{00E9}bastolobe \u{00E0} courtes \u{00E9}pines"))),
				gsub("[Pp]acific [Oo]cean [Pp]erch", eval(parse(text=deparse("s\u{00E9}baste \u{00E0} longue m\u{00E2}choire"))),
				gsub("[Ss]ilvergr[ae]y [Rr]ockfish", eval(parse(text=deparse("s\u{00E9}baste argent\u{00E9}"))),
				gsub("[Rr]ougheye/[Bb]lackspotted [Rr]ockfish", eval(parse(text=deparse("s\u{00E9}baste \u{00E0} \u{0153}il \u{00E9}pineux/\u{00E0} taches noires"))),
				xx))))))))))))))))))))))))))
			})
			## non-rockfish fish names
			xspp2 = sapply(xspp1, function(xx){
				gsub("[Ss]kates", "les raies",
				gsub("[Ll]ingcod", "morue-lingue",
				gsub("[Ss]ablefish", eval(parse(text=deparse("morue charbonni\u{00E8}re"))),
				gsub("[Hh]erring(s)?", "des harengs",
				gsub("[Rr]ex [Ss]ole", "plie royale",
				gsub("[Bb]ig [Ss]kate", eval(parse(text=deparse("raie biocell\u{00E9}e"))),
				gsub("[Rr]ock [Ss]ole", "fausse limande",
				gsub("[Dd]over [Ss]ole", "limande-sole",
				gsub("[Ff]latfish(es)?", "les poissons plats",
				gsub("[Pp]acific [Cc]od", "morue du Pacifique",
				gsub("[Ee]nglish [Ss]ole", "carlottin anglais",
				gsub("[Pp]etrale [Ss]ole", "plie de Californie",
				gsub("[Pp]acific [Hh]ake", "merlu du Pacifique",
				gsub("[Aa]merican [Ss]had", "alose savoureuse",
				gsub("[Ff]lathead [Ss]ole", eval(parse(text=deparse("plie \u{00E0} t\u{00EA}te plate"))),
				gsub("[Ss]ixgill [Ss]hark", "requin griset",
				gsub("[Ss]piny [Dd]ogfish", "aiguillat commun",
				gsub("[Bb]lue [Ss]hark(s)?", "requin\\1 bleu\\1",
				gsub("[Cc]hinook [Ss]almon", "saumon quinnat",
				gsub("[Ll]ongnose [Ss]kate", "pocheteau long-nez",
				gsub("[Pp]acific [Hh]alibut", eval(parse(text=deparse("fl\u{00E9}tan du Pacifique"))),
				gsub("[Pp]acific [Hh]erring", "hareng du Pacifique",
				gsub("[Ss]andpaper [Ss]kate", "raie rugueuse",
				gsub("[Ss]potted [Rr]atfish", eval(parse(text=deparse("chim\u{00E8}re d'Am\u{00E9}rique"))),
				gsub("[Ww]alleye [Pp]ollock", "goberge de l'Alaska",
				gsub("[Uu]nknown [Ff]ish(es)?", "des poissons inconnus",
				gsub("[Aa]rrowtooth [Ff]lounder", eval(parse(text=deparse("plie \u{00E0} grande bouche"))),
				gsub("[Pp]acific [Ss]leeper [Ss]hark", "requin dormeur du Pacifique",
				xx))))))))))))))))))))))))))))
			})
			## non-fish names
			xspp3 = sapply(xspp2, function(xx){
				gsub("[Ss]quid(s)?", "des calmars",
				gsub("[Oo]ctopus(es)?", "des poulpes",
				gsub("[Jj]ellyfish(es)?", eval(parse(text=deparse("des m\u{00E9}duses"))),
				gsub("[Gg]astropod(s)?", eval(parse(text=deparse("gast\u{00E9}ropode\\1"))),
				gsub("[Bb]ox [Cc]rab(s)?", eval(parse(text=deparse("crabe \u{00E0} pattes trou\u{00E9}es"))),
				gsub("[Tt]rue [Cc]rab(s)?", "vrais crabe\\1",
				gsub("[Bb]asket [Ss]tar(s)?", "des ophiures",
				gsub("[Gg]orgonian [Cc]oral(s)?", "les gorgones",
				gsub("[Tt]anner [Cc]rab(s)?", "crabe des neiges du Pacifique",
				gsub("[Bb]lack-[Ff]ooted [Aa]lbatross", eval(parse(text=deparse("albatros \u{00E0} pied noir"))),
				gsub("[Kk]ing [Cc]rab(s)?|[L]ithodes", "les crabes royaux",
				gsub("[Aa]laskan [Kk]ing [Cc]rab(s)?", "crabe royal de l'Alaska",
				gsub("[Ss]carlet [Kk]ing [Cc]rab(s)?|[L]ithodes [Cc]ouesi", eval(parse(text=deparse("crabe royal \u{00E9}carlate"))),
				xx)))))))))))))
			})
#browser();return()
			## Stock Synthesis (SS3) favourites
			xss = sapply(xspp3, function(xx){
				gsub("[Ff]leet", "flotte",
				gsub("[Pp]rior", "prieur",
				gsub("[Tt]otal", "totale",  ## vraisemblance totale
				gsub("[Aa]ge[Ss]el", eval(parse(text=deparse("s\u{00E9}l.d'\u{00E2}ge"))),
				gsub("[Aa]ge [Dd]ata", eval(parse(text=deparse("donn\u{00E9}es d'\u{00E2}ge"))),
				gsub("[Ii]ndex [Dd]ata", eval(parse(text=deparse("donn\u{00E9}es d'indice"))),
				gsub("[N] (samp|adj)", eval(parse(text=deparse("N \u{00E9}ch"))),
				gsub("[Dd]bl[Nn] peak", "Ndbl de pointe",
				gsub("[Hh]armonic [Mm]ean", "moyenne harmonique",
				gsub("[Ii]nitial [Vv]alue", "valeur initiale",
				gsub("[Aa]bundance [Ii]ndex", "indice d'abondance",
				gsub("[Aa]rithmetic [Mm]ean", eval(parse(text=deparse("moyenne arithm\u{00E9}tique"))),
				gsub("[Pp]osterior [Mm]edian", eval(parse(text=deparse("m\u{00E9}diane post\u{00E9}rieure"))),
				gsub("[Aa]ge\\-([0-9]) fish", eval(parse(text=deparse("poisson d'\u{00E2}ge \\1"))),
				gsub("[Aa]ge\\-([0-9]) recruits", eval(parse(text=deparse("recrues d'\u{00E2}ge \\1 ans"))),
				gsub("[Dd]bl[Nn] ascend [Ss][Ee]", "Ndbl ascendante ES",
				gsub("([[:digit:]])[Mm]ale [Pp]eak", eval(parse(text=deparse("de pointe \\1m\u{00E2}le"))),  ## \\1 = first capture group in parentheses
				gsub("[Cc]hange in -log-likelihood", eval(parse(text=deparse("changement de log-vraisemblance n\u{00E9}gatif"))),
				gsub("[Oo]bserved [Ss]ample [Ss]ize", eval(parse(text=deparse("taille de l'\u{00E9}chantillon observ\u{00E9}"))),
				gsub("[Ee]ffective [Ss]ample [Ss]ize", eval(parse(text=deparse("taille effective de l'\u{00E9}chantillon"))),
				gsub("[Mm]ax(\\.|imum)? [Ll]ikelihood", "vraisemblance maximale",
				gsub("[Ss]tandard [Dd]eviation of [Aa]ge", eval(parse(text=deparse("\u{00E9}cart type de l'\u{00E2}ge"))),
				gsub("[Rr]ecruitment [Dd]eviation [Vv]ariance", eval(parse(text=deparse("variance de l'\u{00E9}cart de recrutement"))),
				gsub("[Mm]ale [Nn]atural [Mm]ortality(\\s+\\(M\\))?", eval(parse(text=deparse("mortalit\u{00E9} naturelle des m\u{00E2}les\\1"))),
				gsub("[Ff]emale [Nn]atural [Mm]ortality(\\s+\\(M\\))?", eval(parse(text=deparse("mortalit\u{00E9} naturelle des femelles\\1"))),
				gsub("[Aa]symptotic [Ss]tandard [Ee]rror [Ee]stimate", "estimation d'erreur standard asymptotique",
				xx))))))))))))))))))))))))))
			})
			## large unwieldy phrases (poo)
			xpoo = sapply(xss, function(xx){
				gsub("[Uu]nsorted [A][F]", eval(parse(text=deparse("F\u{00C2} non tri\u{00E9}es"))),
				gsub("[Yy]ear of [Bb]irth", eval(parse(text=deparse("ann\u{00E9}e de naissance"))),
				gsub("CPUE [Nn]ot [Uu]sed", eval(parse(text=deparse("CPUE non utilis\u{00E9}e"))),
				gsub("[M][P][D] [Ee]stimate", eval(parse(text=deparse("estimation de MDP"))),
				gsub("[Aa]ll [Gg]ear [Tt]ypes", eval(parse(text=deparse("tous les types d'\u{00E9}quipement"))),
				gsub("[Ll]ong-[Tt]erm [Mm]ean", eval(parse(text=deparse("moyenne \u{00E0} long terme"))),
				gsub("[Pp]arameter [Ee]stimate", eval(parse(text=deparse("estimation de param\u{00E8}tre"))),
				gsub("[Ll]ong-[Tt]erm [Mm]edian", eval(parse(text=deparse("m\u{00E9}diane \u{00E0} long terme"))),
				gsub("[Rr]elative to [Uu]nfished", eval(parse(text=deparse("par rapport \u{00E0} non exploit\u{00E9}"))),
				gsub("[Ww]eighted by [Cc]atch", eval(parse(text=deparse("pond\u{00E9}r\u{00E9} par les prises"))),
				gsub("[Mm]ax [Ff]ishing [Mm]ortality", eval(parse(text=deparse("mortalit\u{00E9} max par p\u{00EA}che"))),
				gsub("[Ff]ishing [Mm]ortality [Rr]ate", eval(parse(text=deparse("taux de mortalit\u{00E9} par p\u{00EA}che"))),
				gsub("[Mm]ean [Ff]ishing [Mm]ortality", eval(parse(text=deparse("mortalit\u{00E9} moyenne par p\u{00EA}che"))),
				gsub("[Nn]ormali[sz]ed [Rr]esiduals", eval(parse(text=deparse("r\u{00E9}sidus normalis\u{00E9}s"))),
				gsub("[Ss]tandardi[sz]ed [Rr]esiduals", eval(parse(text=deparse("r\u{00E9}sidus standardis\u{00E9}s"))),
				gsub("[Ss]tudenti[sz]ed [Rr]esiduals", eval(parse(text=deparse("r\u{00E9}sidus standardis\u{00E9}s"))),
				gsub("[Aa]ge [Rr]eader [Oo]bservations", eval(parse(text=deparse("les observations des lecteurs d'\u{00E2}ge"))),
				gsub("[Rr]esidual [Ss]um of [Ss]quares", eval(parse(text=deparse("somme r\u{00E9}siduelle des carr\u{00E9}s"))),
				gsub("[Aa]ll [Gg]roundfish [Ff]isheries", eval(parse(text=deparse("Toutes les p\u{00EA}ches de poisson de fond"))),
				gsub("[Mm]edian [Ff]ishing [Mm]ortality", eval(parse(text=deparse("mortalit\u{00E9} m\u{00E9}diane par p\u{00EA}che"))),
				gsub("[Aa]uto-[Cc]orrelation [Ff]unction of", eval(parse(text=deparse("Fonction d'auto-corr\u{00E9}lation de"))),
				gsub("[Ee]xploitation \\([Hh]arvest) [Rr]ate", "taux d'exploitation (r\u{00E9}colte)",
				gsub("[Ll]og [Rr]ecruitment [Dd]eviation(s)?", eval(parse(text=deparse("Log \u{00E9}carts de recrutement"))),
				gsub("[Ll]og [Ii]nitial [Aa]ge [Dd]eviation(s)?", eval(parse(text=deparse("Log des \u{00E9}carts d'\u{00E2}ge initiaux"))),
				gsub("[Aa]ll [Cc]ommercial [Gg]roundfish [Ff]isheries", eval(parse(text=deparse("Toutes les p\u{00EA}ches commerciales de poisson de fond"))),
				gsub("[Bb]iomass [Rr]elative to [Aa]verage [Bb]iomass", eval(parse(text=deparse("Biomasse par rapport \u{00E0} la biomasse moyenne"))),
				gsub("[Bb]iomass [Rr]elative to [Uu]nfished [Ee]quilibrium", eval(parse(text=deparse("Biomasse par rapport \u{00E0} l'\u{00E9}quilibre non exploit\u{00E9}"))),
				xx)))))))))))))))))))))))))))
			})
			## three words
			xthree = sapply(xpoo, function(xx){
				gsub("fit early mats", eval(parse(text=deparse("adapt\u{00E9} premi\u{00E8}res mats"))),
				gsub("[Aa]ge [Ee]rr ", eval(parse(text=deparse("err d'\u{00E2}ge "))),
				gsub("[Nn]o [Ss]urv [Aa]ge", eval(parse(text=deparse("pas d'\u{00E2}ge enq"))),
				gsub("[Ll]engths at [Aa]ge", eval(parse(text=deparse("les longueurs selon l'\u{00E2}ge"))),
				gsub("[Aa]nnual [Mm]ean [Ww]eight", "poids moyen annuel",
				gsub("[Dd]egrees [Oo]f [Ff]reedom", eval(parse(text=deparse("degr\u{00E9}s de libert\u{00E9}"))),
				gsub("[Cc][Vv] [Pp]rocess [Ee]rror", "erreur de processus de CV",
				gsub("[Nn]o [G][I][G]/[Tt]riennial", "pas GIG/triennale",
				gsub("([Tt]op|[Hh]ighest) [Cc]atch", eval(parse(text=deparse("prise la plus \u{00E9}lev\u{00E9}e"))),
				gsub("[Mm]ax [Ee]xploitation [Rr]ate", eval(parse(text=deparse("taux de r\u{00E9}colte maximal"))),
				gsub("[Ff]emale [Ss]pawning [Bb]iomass", "biomasse reproductrice femelles",
				gsub("[R][E][B][S] [Nn]orth [Cc]omposite", "composite du REBS nord",
				gsub("[R][E][B][S] [Ss]outh [Cc]omposite", "composite du REBS sud",
				gsub("[Hh]ake [Aa]coustic( [Ss]urvey)?(s)?", eval(parse(text=deparse("relev\u{00E9}\\2 acoustique du merlu"))),
				gsub("[Ss]pawner(s)? [Pp]er [Rr]ecruit(s)?", "biomasse reproductrice par recrue",
				xx)))))))))))))))
			})
#browser();return()
			## bigger double words
			xtwo.big = sapply(xthree, function(xx){
				gsub("[Bb][Oo][Tt][Tt][Oo][Mm] [Tt][Rr][Aa][Ww][Ll]", "chalut de fond",
				gsub("[Hh]arvest [Rr]ate", eval(parse(text=deparse("taux de r\u{00E9}colte"))),
				gsub("[Rr]educe [Cc]atch", eval(parse(text=deparse("r\u{00E9}duire les prises"))),
				gsub("[Ss]hrimp [Tt]rawl",  eval(parse(text=deparse("chalut \u{00E0} crevettes"))),
				gsub("[Uu]nknown [Tt]rawl", "chalut inconnu",
				gsub("[Mm][Ii][Dd][Ww][Aa][Tt][Ee][Rr] [Tt][Rr][Aa][Ww][Ll]", eval(parse(text=deparse("chalut p\u{00E9}lagique"))),
				gsub("[Cc]atch [Ss]trategy", eval(parse(text=deparse("strat\u{00E9}gie de prises"))),
				gsub("[Rr]elative [Vv]alue", "valeur relative",
				gsub("[Pp]rimary [Rr]eader", "technicien principal",
				gsub("[Ii]ncrease [Cc]atch", "augmenter les prises",
				gsub("[Dd]ecrease [Cc]atch", "diminuer les prises",
				gsub("[Rr]etained [Cc]atch", "prises retenues",
				gsub("[Oo]ther [Ff]isheries", eval(parse(text=deparse("autres p\u{00EA}cheries"))),
				gsub("[Pp]arameter [Vv]alue", eval(parse(text=deparse("valeur du param\u{00E8}tre"))),
				gsub("[Gg]roundfish [Tt]rawl",  eval(parse(text=deparse("chalut \u{00E0} poissons de fond"))),
				gsub("[Ss]econdary [Rr]eader", "technicien secondaire",
				gsub("[Cc]ommercial [Tt]rawl", eval(parse(text=deparse("p\u{00EA}che commerciale au chalut"))),
				gsub("[Rr]elative [Bb]iomass", "biomasse relative",
				gsub("[Ss]pawning [Bb]iomass", "biomasse reproductrice",
				#gsub("[Ss]ensitivity [Rr]uns", eval(parse(text=deparse("ex\u{00E9}cutions de sensibilit\u{00E9}"))),
				gsub("[Ss]ensitivity [Rr]un(s)", eval(parse(text=deparse("simulation\\1 de sensibilit\u{00E9}"))), ## Paul Marchal
				gsub("[Ee]xploitation [Rr]ate", eval(parse(text=deparse("taux de r\u{00E9}colte"))),
				gsub("[Ff]ishing [Mm]ortality", eval(parse(text=deparse("mortalit\u{00E9} par p\u{00EA}che"))),
				gsub("[Dd]erived [Qq]uantities", eval(parse(text=deparse("quantit\u{00E9}s d\u{00E9}riv\u{00E9}es"))),
				gsub("[Ss]pawning [Dd]epletion", eval(parse(text=deparse("\u{00E9}puisement des femelles reproductrices"))),
				gsub("[Vv]ulnerable [Bb]iomass", eval(parse(text=deparse("biomasse vuln\u{00E9}rable"))),
				gsub("[Rr]elative [Ff]requency", eval(parse(text=deparse("fr\u{00E9}quence relative"))),
				gsub("[Nn][Oo] [Dd][Aa][Tt][Aa]", eval(parse(text=deparse("pas de donn\u{00E9}es"))),
				gsub("[Cc]umulative [Ff]requency", eval(parse(text=deparse("fr\u{00E9}quence cumulative"))),
				gsub("[Cc]redibility [Ee]nvelope", eval(parse(text=deparse("enveloppe de cr\u{00E9}dibilit\u{00E9}"))),
				gsub("[Tt]heoretical [Qq]uantiles", eval(parse(text=deparse("quantiles th\u{00E9}oriques"))),
				gsub("[Pp]osterior [Dd]istribution", eval(parse(text=deparse("distribution post\u{00E9}rieure"))),
				gsub("[Rr]ecruitment [Dd]eviation(s)?", eval(parse(text=deparse("\u{00E9}cart\\1 de recrutement"))),
				xx))))))))))))))))))))))))))))))))
			})
			## smaller double words
#browser();return()
			xtwo.med = sapply(xtwo.big, function(xx){
				gsub("[Nn]o CPUE", "pas de CPUE",
				gsub("[Nn]o CVpro", "pas de CVpro",
				gsub("[Ii]n [Yy]ear", eval(parse(text=deparse("dans l'ann\u{00E9}e"))),
				gsub("CPUE [Ii]ndex", "indice de CPUE",
				gsub("[Ee]stimate M", "estimer M",
				gsub("[Mm]ax [Aa]ge", eval(parse(text=deparse("\u{00E2}ge max"))),
				gsub("[Mm]ean [Aa]ge", eval(parse(text=deparse("\u{00E2}ge moyen"))),
				gsub("[A]lt [Cc]atch", "prises alt",
				gsub("[Bb]ased [Oo]n", eval(parse(text=deparse("bas\u{00E9} sur"))),
				gsub("[Aa]ge(\\s+)?\\(y(?:ear|r)", eval(parse(text=deparse("\u{00E2}ge (ann\u{00E9}e"))),
				gsub("[Ee]nd [Yy]ear", eval(parse(text=deparse("ann\u{00E9}e de fin"))),
				gsub("[Pp]er [Yy]ear", eval(parse(text=deparse("par l'ann\u{00E9}e"))),
				gsub("[Bb]ase [Cc]ase|[Bb]ase [Rr]un", eval(parse(text=deparse("sc\u{00E9}nario de r\u{00E9}f\u{00E9}rence"))),
				#gsub("[Bb]ase [Rr]uns", eval(parse(text=deparse("ex\u{00E9}cutions des sc\u{00E9}narios de r\u{00E9}f\u{00E9}rence"))),
				gsub("[Bb]ase [Rr]un(s)?", eval(parse(text=deparse("simulation\\1 de r\u{00E9}f\u{00E9}rence"))),  ## Paul Marchal
				#gsub("[Bb]ase [Rr]un(s)?", eval(parse(text=deparse("sim\\1 de r\u{00E9}f"))),  ## Paul Marchal (shorten it for compo)
				gsub("[Bb]ut [Ff]ixed", "mais fixe",
				gsub("[Aa]ge [Cc]lass", eval(parse(text=deparse("classe d'\u{00E2}ge"))),
				gsub("min [B] [Yy]ear", eval(parse(text=deparse("ann\u{00E9}e de min B"))),
				gsub("[Ss]tart [Yy]ear", eval(parse(text=deparse("ann\u{00E9}e de d\u{00E9}but"))),
				gsub("[Cc]entral [Rr]un", eval(parse(text=deparse("ex\u{00E9}cution centrale"))),
				gsub("[Mm]ean [Ww]eight", "poids moyen",
				gsub("[Ss]urvey [Yy]ear", eval(parse(text=deparse("ann\u{00E9}e du relev\u{00E9}"))),
				gsub("[Ww]hole [Cc]atch", eval(parse(text=deparse("prise enti\u{00E8}re"))),
				gsub("[Tt]ail [Dd]etails", eval(parse(text=deparse("d\u{00E9}tails de la queue"))),
				gsub("[Oo]riginal von[Bb]", "vonB original",
				gsub("[Tt]otal [Bb]iomass", "biomasse totale",
				gsub("[Pp]redicted [Aa]ge", eval(parse(text=deparse("\u{00E2}ge pr\u{00E9}dit"))),
				gsub("[Ff]ishing [Ee]ffort", eval(parse(text=deparse("effort de p\u{00EA}che"))),
				gsub("[Aa]g(e)?(ing)? [Ee]rror", "erreur de vieillissement",
				gsub("[Tt]ow [Ll]ocation(s)?", "emplacement\\1 des traits de chalut",
				gsub("[Mm]ean [Aa]ge \\(year", eval(parse(text=deparse("\u{00E2}ge moyen (ann\u{00E9}e"))),
				gsub("[Pp]rojected [Cc]atch", eval(parse(text=deparse("prise projet\u{00E9}e"))),
				gsub("[Hh]al[fv](e)? [Cc]atch", eval(parse(text=deparse("moiti\u{00E9} prise"))),
				gsub("[Mm]ean\\([Cc][Pp][Uu][Ee])", "moyenne(cpue)",
				xx)))))))))))))))))))))))))))))))))
			})
			## single words 10 or more characters
			xone.big = sapply(xtwo.med, function(xx){
				gsub("[Dd]eveloped", eval(parse(text=deparse("d\u{00E9}velopp\u{00E9}"))),
				gsub("[Dd]eveloping", eval(parse(text=deparse("en d\u{00E9}veloppement"))),
				gsub("[Hh]istorical", "historique",
				gsub("[Pp]roportion", "proportion",
				gsub("[Cc]ommercial", "commercial",
				gsub("[Vv]ulnerable", eval(parse(text=deparse("vuln\u{00E9}rable"))),
				gsub("[Rr]ecruitment", "recrutement",
				gsub("[Ss]electivity", eval(parse(text=deparse("s\u{00E9}lectivit\u{00E9}"))),
				gsub("[Ee]xploitation", eval(parse(text=deparse("r\u{00E9}colte"))),
				xx)))))))))
			})
			## single words with 7-9 characters
			xone.med = sapply(xone.big, function(xx){
				gsub("[Cc]entral[^e]", "centrale",
				gsub("[Dd]ensity", eval(parse(text=deparse("densit\u{00E9}"))),
				gsub("[Hh]ealthy", "saine",
				gsub("[Ll]argest", "le plus grand",
				gsub("[Rr]emoved", eval(parse(text=deparse("retir\u{00E9}"))),
				gsub("[Rr]unning", eval(parse(text=deparse("en cours d'\u{00E9}x\u{00E9}cution"))),
				gsub("[Sc]enario", eval(parse(text=deparse("sc\u{00E9}nario"))),
				gsub("[Ss]hifted", eval(parse(text=deparse("d\u{00E9}cal\u{00E9}"))),
				gsub("[Cc]ritical", "critique",
				gsub("[Cc]autious", "prudence",
				gsub("[Oo]bserved", eval(parse(text=deparse("observ\u{00E9}"))),
				gsub("[Rr]ecruits", "recrues",
				gsub("[Rr]esearch", "recherche",
				gsub("[Mm]aturity", eval(parse(text=deparse("maturit\u{00E9}"))),
				gsub("[Ss]pawning", "frayant",
				gsub("[Ss]mallest", "le plus petit",
				gsub("[Uu]pweight", "augmenter le poid",
				gsub("[Dd]epletion", eval(parse(text=deparse("\u{00E9}puisement"))),
				gsub("[Pp]osterior", eval(parse(text=deparse("post\u{00E9}rieure"))),
				gsub("[Pp]redicted", eval(parse(text=deparse("pr\u{00E9}dit"))),
				gsub("[Rr]esiduals", eval(parse(text=deparse("r\u{00E9}sidus"))),
				gsub("[Ff]requency", eval(parse(text=deparse("la fr\u{00E9}quence"))),
				gsub("[Bb]iomass(e)?", "biomasse",
				gsub("[Ss][Yy][Nn][Oo][Pp][Tt][Ii][Cc]", "synoptique",
				gsub("[Tt][Rr][Ii][Ee][Nn][Nn][Ii][Aa][Ll]", "triennale",   ## survey is feminine: enqu\^{e}te triennale
				gsub("[Hh][Ii][Ss][Tt][Oo][Rr][Ii][Cc]([Aa][Ll])?", "historique",
				xx))))))))))))))))))))))))))
			})
			## single words up to 6 characters
			xone.small = sapply(xone.med, function(xx){
				gsub(" \\(y)", " (an.)",
				gsub("[Aa]dd", "ajouter",
				#gsub("[Rr]un", eval(parse(text=deparse("Ex\u{00E9}"))),
				gsub("[Rr]un", "Sim",  ## Paul Marchal
				gsub("[N]orth", "Nord",
				gsub("[N]orth", "Nord",
				gsub("[S]outh", "Sud",
				gsub(" [Ll]eft", eval(parse(text=deparse(" \u{00E0} gauche"))),
				gsub(" [Rr]ight", eval(parse(text=deparse(" \u{00E0} droite"))),
				gsub("^[Aa]ge ", eval(parse(text=deparse("\u{00E2}ge "))),
				gsub("^[Yy]ear ", eval(parse(text=deparse("ann\u{00E9}e "))),
				gsub("/[Cc]ell", "/cellule", ## need qualifier for Big Skate (raie biocell\'{e}e)?
				gsub("([^[:digit:]])?[Mm]ale", eval(parse(text=deparse("\\1m\u{00E2}le"))),
				gsub("[Cc]atch", "prises",
				gsub("[Dd]epth", "profondeur",
				gsub("[Ii]ndex", "indice",
				gsub("[Mm]onth", "mois",
				gsub("[Ss]cale", eval(parse(text=deparse("\u{00E9}chelle"))),
				gsub("[Ss]tart", "commencer",
				gsub("[Bb]ubble", "bulle",
				gsub("[Ee]vents", eval(parse(text=deparse("\u{00E9}v\u{00E9}nements"))),
				gsub("[Ff]emale", "femelle",
				gsub("[Ff]itted", eval(parse(text=deparse("ajust\u{00E9}"))),
				gsub("[Ll]ength", "longueur",
				gsub("[Mm]edian", eval(parse(text=deparse("m\u{00E9}diane"))),
				gsub("[Nn]arrow", eval(parse(text=deparse("\u{00E9}troit"))),
				gsub("[Rr]emove", "retirer",
				gsub("[Ss]ample", eval(parse(text=deparse("\u{00E9}chantillon"))),
				gsub("[Ss]eason", "saison",
				gsub("[Ss]eries", eval(parse(text=deparse("s\u{00E9}ries"))),
				gsub("[Vv]essel", "navire",
				gsub("[Ww]eight", "poids",
				gsub(" [Mm]ajor ", " zone ",
				gsub("[Ss]urv(ey)?", eval(parse(text=deparse("relev\u{00E9}"))),
				gsub("[Dd]rop(ped)?",  eval(parse(text=deparse("enlev\u{00E9}"))),
				gsub("[Oo][Tt][Hh][Ee][Rr]", "autre",
				xx)))))))))))))))))))))))))))))))))))
			})
#browser();return()
			## words describing fisheries
#browser();return()
			xfish = sapply(xone.small, function(xx){
				gsub("[Mm]ajor", "principal",
				gsub("[Mm]inor", "secondaire",
				gsub("[Ss]able", "morue",
				gsub("[Tt][Rr][Aa][Ww][Ll]", "chalut",
				gsub("[H][B][L][L]", eval(parse(text=deparse("P\u{00E0}FD"))),
				gsub("[Aa]ssess", eval(parse(text=deparse("\u{00E9}val"))),
				gsub("[Ss]almon", "saumon",
				gsub("[Ss]orted", eval(parse(text=deparse("tri\u{00E9}es"))),
				gsub("[Dd]ogfish", "aiguillat",
				gsub("[Hh]alibut", eval(parse(text=deparse("fl\u{00E9}tan"))),
				gsub("[Ll]anding", eval(parse(text=deparse("d\u{00E9}barquement"))),
				gsub("[Ss]urv\\:", "relev:",
				gsub("[Ss]urv\\_", "relev_",
				gsub("[Ll]ongline", "palangre",
				gsub("[Uu]nsorted", eval(parse(text=deparse("non tri\u{00E9}es"))),
				gsub("[Ss]teepness", "inclinaison de la pente",
				gsub("[Dd]og/[Ll]in", "aig/lin",
				gsub("[Tt]rip [Cc]ode", "code de voyage",
				gsub("from age readers", eval(parse(text=deparse("des lecteurs d'\u{00E2}ge"))),
				gsub("[S][B][F] [Tt]rap", "MC casier",
				gsub("[Hh](\\_)?[Ll]rock", eval(parse(text=deparse("HLs\u{00E9}b"))),
				gsub("[R][E][B][S] [Nn]orth", "REBS nord",
				gsub("[R][E][B][S] [Ss]outh", "REBS sud",
				gsub("[Hh]ook [\\&|Aa](nd)? [Ll]ine", eval(parse(text=deparse("hame\u{00E7}on et lignes"))),
				gsub("[Hh][Ll](\\_|\\.| )[Rr]ock(fish)?", eval(parse(text=deparse("HL.s\u{00E9}baste"))),
				gsub("[Ff][Ii][Ss][Hh]([Ee][Rr][Yy]|ing)", eval(parse(text=deparse("p\u{00EA}che"))),
				gsub("[Hh](\\&|\\.)[Ll](\\_|\\.| )[Rr]ock(fish)?", eval(parse(text=deparse("H&L s\u{00E9}baste"))),
				gsub("[Tt][Rr][Aa][Ww][Ll](\\s+|\\_)[Ff][Ii][Ss][Hh][Ee][Rr][Yy]", eval(parse(text=deparse("p\u{00EA}che\\1au\\1chalut"))),
				gsub("[Mm][Ii][Dd][Ww][Aa][Tt][Ee][Rr](\\s+|\\_)[Ff][Ii][Ss][Hh][Ee][Rr][Yy]", eval(parse(text=deparse("p\u{00EA}che\\1p\u{00E9}lagique"))),
				xx)))))))))))))))))))))))))))))
			})
			## single words describing biology
			xbio = sapply(xfish, function(xx){
				gsub("[Ss]pent", eval(parse(text=deparse("us\u{00E9}"))),
				gsub("[Mm]ature", "mature",
				gsub("[Ee]mbryos", "embryons",
				gsub("[Oo]tolith", "otolithe",
				gsub("[Rr]esting", "repos",
				gsub("[Ii]mmature", "immature",
				gsub("[Pp]ectoral", "pectorale",
				gsub("[Mm]aturing", eval(parse(text=deparse("\u{00E0} maturit\u{00E9}"))),
				gsub("[Ss]pecimen", eval(parse(text=deparse("sp\u{00E9}cimen"))),
				gsub("[Ff]ertili[sz]ed", eval(parse(text=deparse("fertilis\u{00E9}"))),
				xx))))))))))
			})
			## geographic words
			xgeo = sapply(xbio, function(xx){
				##gsub("[B][C]", eval(parse(text=deparse("C-B\u{2000}"))),
				#gsub("^[B][C]([[:space:]]+)?", eval(parse(text=deparse("C-B\u{2000}"))),
				#gsub("([[:space:]]+)?[B][C]([[:space:]]+)?", eval(parse(text=deparse("\\1C-B\\2"))),
				gsub("([[:space:]]+)[B][C]([[:space:]]+)", eval(parse(text=deparse("\\1C-B\\2"))),
				gsub("[H][S]", "DH",
				gsub("[Cc]oast", eval(parse(text=deparse("c\u{00F4}te"))),
				gsub("[G][I][G]", "CIG",
				gsub("[Q][C][S]", "BRC",
				gsub("[P][B][S]", "SBP",
				gsub("[H][B][L][L]", "PFD", ## palangre \`{a} fond dur
				gsub("[I][P][H][C]", "CIFP",     ## Commission internationale du fl\'{e}tan du Pacifique
				gsub("[N][M][F][S]", "SNPM",     ## Service national des p\^{e}ches maritimes
				gsub("[W][C][H][G]", "COHG",
				gsub("[W][C][V][I]", "COIV",
				gsub("[Aa]rea\\(km", "superficie(km",
				gsub("[Ss]ubarea(s)?", "sous-zone\\1",
				gsub("(\\s+)?[Ii]nside", eval(parse(text=deparse(" \u{00E0} l'int\u{00E9}rieur"))),
				gsub("(\\s+)?[Oo]utside", eval(parse(text=deparse(" \u{00E0} l'ext\u{00E9}rieur"))),
				gsub("[Hh]ecate [Ss]trait", eval(parse(text=deparse("d\u{00E9}troit d'Hecate"))),
				gsub("[Mm]oresby [Gg]ully", "canyon de Moresby",
				gsub("[P][M][F][C] [Aa]rea", "zone CPMP",
				gsub("[Bb]ritish [Cc]olumbia", "Colombie-Britannique",
				gsub("[Vv]ancouver [Ii]sland", eval(parse(text=deparse("\u{00CE}le de Vancouver"))),
				gsub("[Mm]itchell's [Gg]ully", "canyon de Mitchell",
				gsub("[Ee]ncountered [Aa]rea", "zone de rencontre",
				gsub("[Gg]oose [Ii]sland [Gg]ully", eval(parse(text=deparse("canyon de l'\u{00EE}le Goose"))),
				gsub("[Qq]ueen [Cc]harlotte [Ss]ound", "bassin de la Reine-Charlotte",
				gsub("[Qq]ueen [Cc]harlotte [Ss]trait", eval(parse(text=deparse("d\u{00E9}troit de la Reine-Charlotte"))),
				xx)))))))))))))))))))))))))
			})
			## DFO acronyms
			xacro = sapply(xgeo, function(xx){
				gsub("[A][E]", "EV",             ## ageing error = erreur de vieillissement
				gsub("[A][F]", "FA",             ## age frequency = fr\'{e}quence d'ages
				gsub("[B][T]", "CF",             ## bottom trawl   = chalut de fond
				gsub("[C][C]", "PC",             ## constant catch   = prise constante
				gsub("[M][W]", "CP",             ## midwater trawl = chalut p\'{e}lagique
				gsub("[H][R]", "TR",             ## harvest rate   = taux de r\'{e}colte (not taux d'exploitation)
				gsub("^[S][A]", eval(parse(text=deparse("\u{00C9}S"))),  ## stock assessment  = \'{e}valuation des stocks
				gsub("[G][M][A]", "ZGPF",        ## les zones de gestion des poissons de fond 
				gsub("[G][M][U]", "GGPF",        ## le groupe de gestion du poisson de fond
				gsub("[L][R][P]", "PRL",         ## point de r\'{e}f\'{e}rence limite
				gsub("[M][P][D]", "MDP",         ## mode de distribution post\'{e}rieure
				gsub("[M][S][Y]", "RMD",         ## rendement maximal durable (no longer soutenu)
				gsub("[m][s][y]", "rmd",         ## rendement maximal durable (no longer soutenu) (be careful of words ending in 'msy')
				gsub("[U][S][R]", "RSS",         ## r\'{e}f\'{e}rence de stock sup\'{e}rieure
				gsub("[T][R][P]", "PRC",         ## point de r\'{e}f\'{e}rence cible
				gsub("[M][C][M][C]", "MCCM",     ## Monte Carlo \`{a} cha\^{i}ne de Markov
				gsub("[P][M][F][C]", "CPMP",     ## Monte Carlo \`{a} cha\^{i}ne de Markov
				gsub("[R|S]\\+[S|R]", "R+R",     ## research + survey (relev\'{e})
				xx))))))))))))))))))
			})
			## species code3 acronyms
			xcode = sapply(xacro, function(xx){
				gsub("[A][R][F]", "PGB",  ## Arrowtooth Flounder
				gsub("[B][O][R]", "SBO",  ## Bocaccio
				gsub("[B][S][R]", "STN",  ## Blackspotted
				gsub("[C][A][R]", "SCA",  ## Canary
				gsub("[C][P][R]", "SCU",  ## Copper
				gsub("[L][S][T]", eval(parse(text=deparse("SL\u{00C9}"))),  ## Longspine
				gsub("[P][O][P]", "SLM",  ## Pacific Ocean Perch
				gsub("[Q][B][R]", eval(parse(text=deparse("SD\u{00C9}"))),  ## Quillback
				gsub("[R][B][R]", "SBR",  ## Redbanded
				gsub("[R][E][R]", eval(parse(text=deparse("SO\u{00C9}"))),  ## Rougheye
				gsub("[R][S][R]", "SRR",  ## Redstripe
				gsub("[S][G][R]", "SAR",  ## Silvergray
				gsub("[S][K][R]", "SBL",  ## Shortraker
				gsub("[S][S][T]", eval(parse(text=deparse("SC\u{00C9}"))),  ## Shortspine
				gsub("[W][A][P]", "GLA",  ## Walleye Pollock
				gsub("[W][W][R]", "SVV",  ## Widow
				gsub("[Y][M][R]", "SBJ",  ## Yellowmouth
				gsub("[Y][T][R]", "SQJ",  ## Yellowtail
				gsub("[Y][Y][R]", "SYJ",  ## Yelloweye
				gsub("[R][E][B][S]", eval(parse(text=deparse("SO\u{00C9}TN"))),  ## Rougheye/Blackspotted
				xx))))))))))))))))))))
			})
			## final swipe through
			xbig = sapply(xcode, function(xx){
				gsub("C\\+S","c+r",
				gsub(" of "," de ",
				gsub(" or "," ou ",
				gsub(" [\\&|Aa](nd)? "," et ",
				gsub(" by ", " par ",
				gsub(" in ", " en ",
				gsub("([.+ ])? to ([.+ ])?", eval(parse(text=deparse("\\1 \u{00E0} \\2"))),  ## (RH 230727)
				gsub(" but ", " mais ",
				gsub("^no ", "pas de ",
				gsub(" no ", " pas de ",
				gsub("\\(/y(r?))", "(/an)",
				gsub("2y(r?)", "2an",
				#gsub("R([[:digit:]]+)","E\\1",  ## Model run numbers, e.g. R75
				#gsub("R(\\d+)","E\\1",  ## Model run numbers, e.g. R75 (but R0 gets converted!)
				gsub("sigmaR( )?=( )?(0|1)\\.([1-9])","sigmaR\\1=\\2\\3,\\4",
				xx)))))))))))))
			})
#browser();return()
			xout[xBpos] = xbig
		}
		##---------END BIG/MULTIPLE WORDS---------
	}
#browser();return()
	rubbish = gc(verbose=FALSE)
	if (strip) xout = as.vector(xout)
	if (is.expr) xout= eval(parse(text=paste0("expression(",substitute(xout),")"))) #{browser();return()} #
	return(xout)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~linguaFranca


## listTables---------------------------2020-07-20
##  List tables in specified SQL, ORA, or MDB database.
## ---------------------------------------------RH
listTables <- function (dbName, pattern=NULL, path=getwd(), 
   server=NULL, type="SQL", ttype=NULL, trusted=TRUE, uid="", 
   pwd="", silent=FALSE, tenv=.GlobalEnv)
{
	on.exit(odbcCloseAll())
	if (is.null(server)) {
		#getFile(".PBSserver", path=.getSpath(), tenv=penv())
		server = .PBSserver[1]
	}
	if (type=="SQL") driver="SQL Server"
	else if (type=="ORA") driver="Oracle in OraClient11g_home1" #"Oracle ODBC Driver" # DFO standard
	else if (type=="MDB") mdbTable=paste(path,"/",dbName,".mdb",sep="")
	else showError("Only 'SQL', 'ORA', and 'MDB' supported at present")
	if (any(type==c("SQL","ORA"))) {
		syntax <- paste("Driver={",driver,"}", #;Server=",server, 
			ifelse(type=="ORA",";QTO=F;DBQ=",";Server="),server,
			";Database=", dbName, ";Trusted_Connection=", ifelse(trusted, 
			"Yes", "No"), sep = "")
		if (!trusted) syntax = paste(syntax, ";UID=", uid, ";PWD=", pwd, sep = "")
		cnn <- odbcDriverConnect(connection = syntax)
	} else if (type=="MDB") {
		cnn = odbcConnectAccess(access.file=mdbTable)
	}
	assign("cns",cnn,envir=tenv)
	if (type=="ORA") dat = sqlTables(cnn,schema=dbName)
	else             dat = sqlTables(cnn)
	assign("PBSdat",dat,envir=tenv)
	odbcClose(cnn)

	if (length(dat)==1 && dat==-1) {
		assign("PBSdat","No tables",envir=tenv)
		tabs = "No tables"
	} else {
		if (!is.null(ttype)) dat=dat[is.element(dat$TABLE_TYPE,ttype),]
		tabs = dat$TABLE_NAME
		if (!is.null(pattern)) tabs <- findPat(pattern,tabs)
		if (length(tabs)==0)   tabs <- "No matching tables"
#browser();return()
	}
	if (!silent) print(tabs)
	invisible(tabs)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~listTables


#prime----------------------------------2010-03-25
# Report the prime numbers given an integer vector
#-----------------------------------------------RH
prime = function(x=2:100)
{
	is.prime = function(xx) {
		if (xx<=1) return(FALSE)
		else if (is.element(xx,2:3)) return(TRUE)
		else !is.element(0,xx%%2:floor(sqrt(xx))) }
	if (!is.vector(x) || !is.numeric(x) || !all(x%%1==0))
		stop("'x' must be a vector of integers")
	z = sapply(x,is.prime) 
	if (any(z)) return(x[z])
	else return(NULL)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~prime


##quantBox------------------------------2016-03-24
## Redefine boxplot to show quantiles (RH 150910)
## http://r.789695.n4.nabble.com/Box-plot-with-5th-and-95th-percentiles-instead-of-1-5-IQR-problems-implementing-an-existing-solution-td3456123.html
##----------------------------------------------RH
quantBox = function (x, use.cols = TRUE, ...) ## taken from boxplot.matrix
{
	ttget(qboxplot)
	if (rev(class(x))[1]=="matrix") {
		groups <- if (use.cols) 
			split(x, rep.int(1L:ncol(x), rep.int(nrow(x), ncol(x))))
		else split(x, seq(nrow(x)))
		if (length(nam <- dimnames(x)[[1 + use.cols]])) 
		names(groups) <- nam
		qboxplot(groups, ...)
	}
	else qboxplot(x, ...)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~quantBox


## readClog-----------------------------2024-02-29
## Read a ChangeLog file and convert it to an R list.
## ---------------------------------------------RH
readClog = function(fnam)
{
	dat  = readLines(fnam)
	cdat = dat[grep("^$",dat,invert=TRUE)]    ## condense dat by removing blank lines
	cdat = cdat[grep("^#",cdat,invert=TRUE)]  ## get rid of comments
	ctit = cdat[1]                       ## title of ChangeLog
	laut = grep("^Authors", cdat)
	lend = grep("^-----", cdat)[1]       ## end of the header section
	auth = cdat[laut:(lend-1)]           ## authors of package

	lver = grep("^\\S",cdat)             ## lines starting with non-space
	lver = lver[lver>lend]
	#lmod = grep("^\\s\\s[*]",cdat)

	clog = list()
	tdat = PBSmodelling:::.trimWhiteSpace(cdat)         ## remove whitespace before and after text in each line
	tbrk = c(lver,length(tdat)+1)
	for (i in 1:length(lver)) {
		ii   = tdat[lver[i]]
		clog[[ii]] = list()
		idat = tdat[(tbrk[i]+1):(tbrk[i+1]-1)]
		imod = grep("^[*]",idat)
		idat[imod] = gsub("[*]\\s","",idat[imod])   ## get rid of asterisk
		#y = grep("^[-]",idat)
		#idat[imod[which(abs(imod-y[1])==min(abs(imod-y[1])))]]   ## to determine which modules have sub-bullets
		ibrk = c(imod,length(idat)+1)
		for (j in 1:length(imod)) {
			jj   = idat[imod[j]]
			jdat = idat[(ibrk[j]+1):(ibrk[j+1]-1)]
			jdat = gsub("^[-]\\s","  - ",jdat)
			clog[[ii]][[jj]] = c(clog[[ii]][[jj]],jdat)

			## A future update might create a list of lists for the actions (+) and subactions (-)
			#if (any(grepl("^[-]",jdat))) {
			#	jact = grep("^[+]",jdat)
			#	jbrk = c(jact,length(jdat)+1)
			#	for (k in 1:length(jact)) {
			#		kk   = jdat[jact[k]]
			#		clog[[ii]][[jj]] = c(clog[[ii]][[jj]],kk)
			#		nn = length(clog[[ii]][[jj]])
			#		kdat = jdat[(jbrk[k]+1):(jbrk[k+1]-1)]
			#		clog[[ii]][[jj]][[kk]] = list(kdat)
			#		OR
			#		kiss = list()
			#		kiss[[kk]] = kdat
			#		clog[[ii]][[jj]] = c(clog[[ii]][[jj]],kiss)
			#	}
			#} else {
			#clog[[ii]][[jj]] = c(clog[[ii]][[jj]],jdat)
			#}
		}
	}
	attr(clog,"title") = ctit
	attr(clog,"authors") = auth
	return(clog)
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~readClog


#revStr---------------------------------2008-11-26
# Reverse characters in a string (see 'strsplit()' in base package).
#------------------------------------------------R
revStr <- function(x)
	sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")


#runModules-----------------------------2012-07-10
# runModules: Display a master GUI to display modules
#-----------------------------------------------RH
runModules <- function () {
	.runModHelper <- function() {
		getWinVal(scope = "L");  act <- getWinAct()[1]
		if (!exists("act") || !exists("eN")) return()
		if (act == "quit") {
			closeWin(); return() }
		if (eN == 0) {
			#wtxt <- "No examples chosen"
			closeWin(name = c("window",paste("mod",pad0(1:20,2),sep=""))) }
		else {
			if (eN != 99)
				closeWin(name = c("window",paste("mod",pad0(1:20,2),sep="")))
			eval(parse(text=paste(act,"()",sep="")))
			wpath <- .getWpath()
			wnam <- paste(wpath,"/",act,"Win.txt", sep = "")
			#wtxt <- paste(readLines(wnam), collapse = "\n")
			wtxt <- wnam }
		setWinVal(list(wtxt = wtxt), winName = "runM")
	}
	.runModHelperQuit <- function() {
		closeWin(c("window",paste("mod",pad0(1:20,2),sep=""),"runM"))
		return()
	}
	assign(".runModHelper",.runModHelper,envir=.PBStoolEnv)
	assign(".runModHelperQuit",.runModHelperQuit,envir=.PBStoolEnv)
	wpath <- .getWpath()
	rtmp <- tempdir(); rtmp <- gsub("\\\\","/",rtmp)
	wnam <- paste(wpath,"runModulesWin.txt",sep="/")
	wtmp <- paste(rtmp,"runModulesWin.txt",sep="/")
	temp <- readLines(wnam)
	temp <- gsub("@wdf",wtmp,temp)
	writeLines(temp,con=wtmp)
	createWin(wtmp); 
	msg <- paste("runModulesWin.txt modified in ", rtmp, sep = "")
	setWinVal(list(wtxt = msg), winName = "runM")
}
#---------------------------------------runModules


#scaleVec-------------------------------2011-09-14
# Scales a vector to span a target minimum and maximum (see 'scalePar' & 'restorePar')
#-------------------------------------------JTS/RH
scaleVec = function(V, Tmin=0, Tmax=1) {
	if (Tmin>Tmax) stop("Tmin > Tmax")
	Pmin = min(V,na.rm=TRUE); Pmax=max(V,na.rm=TRUE)
	Sval = (V - Pmin)/(Pmax - Pmin)
	Sval = pmax(Sval, 0)
	Sval = pmin(Sval, 1)
	Snor = (2/pi) * asin(sqrt(Sval))                  # normalise to (0,1)
	Tval = Tmin + (Tmax - Tmin) * sin(pi * Snor/2)^2  # recast to lie between Tmin and Tmax
	return(Tval) }
#-----------------------------------------scaleVec


#showError------------------------------2014-10-07
# Display error message on device surface
#-----------------------------------------------RH
showMessage <- function(str, type="", as.is=FALSE, err=FALSE, ...) {
	par0 = par(no.readonly=TRUE); on.exit(par(par0))
	resetGraph(); expandGraph(mar=c(0,0,0,0),oma=c(0,0,0,0))
	if (!as.is) str <- paste(strwrap(str,50),collapse="\n")
	if (is.null(type) || type=="") msg <- str
	else if (type=="nodata") msg <- paste("No data qualified by:\n\n",str)
	else if (type=="nofields") msg <- paste("Following field(s) not available:\n\n",str,sep="")
	else if (type=="nofile") msg <- paste("No file exists:\n\n",str,sep="")
	else msg <- paste(type,str,sep=" : ")
	evalCall(addLabel,argu=list(x=.5,y=.5,txt=msg,cex=1.2,col="red"),...,checkpar=TRUE)
	if (err) stop("See display",call.=FALSE) 
	invisible()}

showError=function(str, type="", as.is=FALSE, err=TRUE, ...) {
	setPBSoptions("showError", TRUE)
	showMessage(str=str, type=type, as.is=as.is, err=err, ...) 
	invisible()}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~showError


#spooler--------------------------------2008-08-21
# Spools list objects into fields of data frames.
#-----------------------------------------------RH
spooler <- function(xlist,newfld="area",target){
	targ=as.character(substitute(target))
	if (!any(class(xlist)=="list"))
		showError(paste("Supply a list of vectors from names available:\n\n",
			paste(names(target),collapse="\n"),sep=""),as.is=TRUE) 
	ni=length(xlist)
	eval(parse(text=paste(targ,"[,\"",newfld,"\"]=rep(NA,nrow(",targ,"))",sep="")),envir=parent.frame(1))
	for (i in 1:ni) {
		fld=names(xlist)[i]; if (is.null(fld)) fld=as.character(substitute(xlist))
		ii=xlist[[i]]; nj=length(ii)
		if (!all(sapply(xlist,is.list)==TRUE)) {
			val=paste(ii,collapse="+")
			expr=paste(targ,"[,\"",newfld,"\"][is.element(",targ,"[,\"",fld,"\"],",deparse(ii),")]=\"",val,"\"",sep="")
			eval(parse(text=expr),envir=parent.frame(1)) }
		else {
			for (j in 1:nj) {
				jj=ii[[j]]
				val=paste(fld,paste(jj,collapse="+"),sep="-")
				expr=paste("z1=is.na(",targ,"[,\"",newfld,"\"]); ",sep="")
				expr=paste(expr,"z2=is.element(",targ,"[,\"",fld,"\"],",deparse(jj),"); ",sep="")
				expr=paste(expr,targ,"[z1&z2,\"",newfld,"\"]=\"",val,"\"",sep="")
				eval(parse(text=expr),envir=parent.frame(1))
}	}	} }


#stdConc--------------------------------2011-07-15
# Standardise chemical concentrations from various
# units to a common concentration and adjust for moisture.
#  namt   = numerator amount
#  nunit  = numerator unit
#  damt   = denominator amount
#  dunit  = denominator unit
#  nUout  = numerator unit desired
#  dUout  = denominator unit desired
#  fac    = multiplicative factor (e.g., 75 for 75 mg)
#-----------------------------------------------RH
stdConc = function(dat, nUout="mg", dUout="kg", fac=1)
{
	oldopts = options(); on.exit(options(oldopts))
	options(stringsAsFactors=FALSE)
	namt=as.numeric(dat[1]); nunit=dat[2]; 
	damt=as.numeric(dat[3]); dunit=dat[4]; 
	moist=as.numeric(dat[5])
	nonStd = FALSE

	# Assign scales for input numerators relative to g or ml
	if (nunit %in% "mcg")                      nSin = 0.000001
	else if (nunit %in% "mg")                  nSin = 0.001
	else if (nunit %in% "g")                   nSin = 1
	else if (nunit %in% c("kg","L"))           nSin = 1000
	else if (nunit %in% c("pct","pdw","ppm","ppt"))  nSin = 1
	else { nSin =1; nonStd = TRUE }

	# Assign scales for input denominators relative to g or ml
	if (dunit %in% "mcg")                      dSin = 0.000001
	else if (dunit %in% "mg")                  dSin = 0.001
	else if (dunit %in% "g")                   dSin = 1
	else if (dunit %in% c("kg","L","ppt"))     dSin = 1000
	else if (dunit %in% c("pct","pdw"))        dSin = 100
	else if (dunit %in% "ppm")                 dSin = 1000000
	else dSin = 1

	# Assign scales for output numerators relative to g or ml
	if (nUout %in% "mcg")                     nSout = 0.000001
	else if (nUout %in% "mg")                 nSout = 0.001
	else if (nUout %in% "g")                  nSout = 1
	else if (nUout %in% c("kg","L"))          nSout = 1000
	else if (nUout %in% c("pct","pdw","ppm","ppt")) nSout = 1
	else nSout = 1

	# Check for non-standard units in numerator
	if (nonStd) {
		nSout = 1; nUout = nunit }

	# Assign scales for output denominators relative to g or ml
	if (dUout %in% "mcg")                     dSout = 0.000001
	else if (dUout %in% "mg")                 dSout = 0.001
	else if (dUout %in% "g")                  dSout = 1
	else if (dUout %in% c("kg","L","ppt"))    dSout = 1000
	else if (dUout %in% c("pct","pdw"))       dSout = 100
	else if (dUout %in% "ppm")                dSout = 1000000
	else dSout = 1

	if (nunit %in% "pdw")  moist = 0   # if measuremennt already based on dry weight

	# Calculate standardised amount of chemical
	stdAmt = fac * (namt/(damt*(1-moist)))*((nSin/nSout)/(dSin/dSout))
	unit=paste(nUout,"/",dUout,sep="")
	if (fac!=1)
		dUout = paste(fac,dUout)
	if (stdAmt>0 & stdAmt < 1)        dig = 3
	else if (stdAmt>=1 & stdAmt<10)   dig = 2
	else if (stdAmt>=10 & stdAmt<100) dig = 1
	else dig = 0

	stdChr = format(stdAmt,nsmall=dig,big.mark=",",scientific=FALSE)
	return(data.frame(stdAmt=stdAmt,unit=unit))
}
#------------------------------------------stdConc


#subsetFile-----------------------------2013-05-07
# Subsets an ASCII file every n rows (enrow).
#-----------------------------------------------RH
subsetFile = function(fnam,enrow=30,header=TRUE,os=.Platform$OS.type)
{
	if (os!="windows") stop("You're sh1t out of luck")
	if (!file.exists(fnam)) stop("File name specified does not exist.")
	snam = paste("sub_",fnam,sep="")
	cmd = paste("sed -n ",ifelse(header,"-e 1p "," "),"-e '",enrow + as.numeric(header),"~",enrow,"p' ",fnam," > ",snam,sep="")
	shell(cmd)
	invisible()
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~subsetFile


## testPch------------------------------2023-07-25
## Display plotting symbols or octal strings.
##  Note (RH 230725):
##   Starting in R version 4.4.0, support for
##   pch and octal strings was reduced to poo.
## Arguments:
##  pch      - integer specifying symbols/octals
##  ncol     - number of columns to use
##  grid     - logical: if TRUE, display in a grid
##  newframe - if T, use a new graphics frame, if F, overlay
##  octal    - logical: if T, use octal (backslash) strings
## Transferred from PBSmodelling, where it was removed at the request of Prof Brian Ripley. (RH 231018)
## ---------------------------------------------RH
testPch <- function (pch=1:200, ncol=10, grid=TRUE, newframe=TRUE, octal=FALSE, ...)
{
	old.warn=options()$warn
	on.exit(options(warn=old.warn))
	options(warn=-1)

	if (!is.element(ncol,c(2,5,10))) stop("Set ncol to 2 or 5 or 10")
	if (!all(diff(pch)==1)) stop("pch vector must be a continuous increasing integer series")
	#if (!octal && (all(pch>255) | any(pch<0))) stop("pch must be in the range 0 - 255")
	#if (octal && (all(pch<41) | all(pch>377))) stop("pch must be in the range 41 - 377")
	if (newframe) {
		resetGraph(); frame(); }
	par0 <- par(no.readonly = TRUE)
	on.exit(par(par0))
	npch =length(pch)
	xlim = c(.5,ncol+.5)
	rlim = floor((pch[c(1,npch)]-1)/ncol); yval = rlim[1]:rlim[2]
	ylim = rev(rlim); ylim = ylim + c(.5,-.5)
	pchy = pch[is.element(pch,seq(0,1000,ncol))]
	if(length(pchy)<length(yval)) {
		pchy = c(pchy,floor((pchy[length(pchy)]+ncol)/ncol)*ncol)
	}
	ylab = pchy - ncol
	par(usr=c(xlim,ylim))
	if (grid) {
		abline(v=seq(.5,ncol+.5,1),h=seq(rlim[1]-.5,rlim[2]+.5,1),col="gainsboro")
	}
	old.warn=options()$warn
	options(warn=-1)
	for (i in pch) {
		y = floor((i - 1)/ncol)
		x = i - ncol * y
		if (octal) {
			#if (i<41 | i>377 | is.element(i,seq(9,379,10)) | is.element(i,c(90:99,190:199,290:299))) next
			#if (i<41 | i>377 | grepl("[89]",i) | is.element(i,c(177,201,220,235))) next   # Duncan Murdoch suggested grepl("[89]",i)
			cset = try(eval(parse(text=paste("\"\\", i, "\"", sep = ""))), silent=TRUE)
			if (inherits(cset,"try-error") || grepl("\\\\", deparse(cset)) ) next
			if (grepl("\\s|\\!|\\#|\\$|\\%|\\&|\\'|\\(|\\)|\\*|\\+|\\,|\\-|\\.|\\/[89]", deparse(cset)) ) next
			if (grepl("[0-7][89]", deparse(cset)) ) next
#.flush.cat(cset," : ", grepl("\\\\",deparse(cset)) , "\n")
#if(i==126){browser();return()}
			text(x, y, cset, cex=1.5, ...) 
		}
		else {
			#if (i>255 || is.element(i,c(27:31,128:255)) ) next
			points(x, y, pch = i, cex=1.5, ...)
		}
	}
	mtext(as.character(1:ncol), side=1, line=.5, at=(1:ncol), cex=1.3, col="blue")
	mtext(as.character(1:ncol), side=3, line=.4, at=(1:ncol), cex=1.3, col="blue")
	mtext(ylab, side=2, line=1, at=yval, cex=1.3, col="red",las=1)
	mtext(paste(ifelse(octal,"OCTAL STRINGS","PCH CHARACTERS"),"(",pch[1],"-",pch[npch],")"), side=3, line=2.2, cex=1.2)
	invisible(yval) }
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~testPch


#toUpper--------------------------------2012-09-19
# Function to capitalise first letters of words
#-----------------------------------------------RH
toUpper = function(x,exclude=c("&","and","exact","or","in","on","organic","pelagic","sp.","spp.","species","undetermined","unidentified",
   "birds","barnacles","crabs","eggs","fishes","larvae","matter","objects","remains","sample","shells","subfamily","tunicates","worms")) {
	strList = strsplit(x,split=" ")
	strL = sapply(strList,function(x){
		x = x[!is.element(x,"")]  # remove extra spaces
		z = !is.element(x,exclude)
		X = sapply(x[z],function(y){paste(toupper(substring(y,1,1)),substring(y,2),sep="")})
		x[z] = X
		return(x) },simplify=FALSE)
	strV = sapply(strL,paste,collapse=" ")
	#sapply(,function(x){})
	return(strV)
}
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^toUpper

#ttget----------------------------------2013-01-17
# Provide PBStools wrappers for PBSmodelling functions tget/tcall/tprint/tput/lisp
#-----------------------------------------------RH 
ttget   = function(...) {tget  (..., penv=parent.frame(), tenv=.PBStoolEnv)}
ttcall  = function(...) {tcall (..., penv=parent.frame(), tenv=.PBStoolEnv)}
ttprint = function(...) {tprint(..., penv=parent.frame(), tenv=.PBStoolEnv)}
ttput   = function(...) {tput  (..., penv=parent.frame(), tenv=.PBStoolEnv)}
tlisp   = function(...) {lisp  (..., pos =.PBStoolEnv)}

# functions called from window description files
.win.onClose  = function(){ ttcall(.onClose)() }
.win.runModHelperQuit = function(){ ttcall(.runModHelperQuit)() }

# Provide PBSdata wrappers for PBSmodelling functions tget/tcall/tprint/tput/lisp
#-----------------------------------------------RH 
dtget   = function(...) {tget  (..., penv=parent.frame(), tenv=.PBSdataEnv)}
dtcall  = function(...) {tcall (..., penv=parent.frame(), tenv=.PBSdataEnv)}
dtprint = function(...) {tprint(..., penv=parent.frame(), tenv=.PBSdataEnv)}
dtput   = function(...) {tput  (..., penv=parent.frame(), tenv=.PBSdataEnv)}
dlisp   = function(...) {lisp  (..., pos =.PBSdataEnv)}

#wrapText-------------------------------2008-09-17
# Wrap, mark and indent a long text string.
#-----------------------------------------------RH
wrapText=function(string,width=50,prefix=c("> ","+ "),exdent=3) {
	if (length(prefix)==1) prefix=c(prefix,prefix)
	wrap=strwrap(string,width=width,prefix=prefix[2],exdent=exdent)
	problems=c("$","^","*","(","+","|","{","[","\\","\"","?")
	wrap[1]=sub(paste(ifelse(any(substring(prefix[2],1,1)==problems),"\\",""),
		prefix[2],sep=""),prefix[1],wrap[1])
	wrap=paste(paste(wrap,collapse="\n"),"\n",sep="")
	return(wrap) }

#zapDupes-------------------------------2008-12-05
# Delete duplicated records based on specified index.
#-----------------------------------------------RH
zapDupes = function(dat, index) {
	if (!all(is.element(index,names(dat))==TRUE)) 
		showError ("Choose index fields that exist in data file.")
	atts=attributes(dat)[setdiff(names(attributes(dat)),c("names","row.names"))]
	# Get rid of lines duplicated by index
	idat=dat[,index,drop=FALSE]
	dat$index=apply(idat,1,function(x){paste(x,collapse="-")})
	dat=dat[order(dat$index),]
	zD=duplicated(dat$index)   # find duplicated indices
	dupes=dat$index[zD]
	if (length(dupes)==0) dupes="No duplicated index"
	dat=dat[!zD,]              # get the unique events (not duplicated)
	attr(dat,"dupes")=dupes
	attributes(dat)=c(attributes(dat),atts)
	return(dat) }

#-----Supplementary hidden functions-----

#.chooseFQT-----------------------------2007-09-20
# Choose a file/query/table from the user-specified path
#-----------------------------------------------RH
.chooseFQT <- function() {
	path <- getWinVal()$path
	sfiles <- list.files(path=path)
	chooseWinVal(sfiles,"fqtName",winname=tcall(.PBSmod)$.activeWin) }

#.flush.cat-----------------------------2011-09-30
# Flush the cat down the console
.flush.cat = function(...) { cat(...); flush.console(); invisible() }

#.grabPath------------------------------2010-06-02
# Return the specified directory in the package 
# tree; if win=TRUE, also set the 'path' variable 
# in the active window.
#-----------------------------------------------RH
.grabPath = function(where, pkg="PBStools", win=FALSE) {
	if (missing(where))
		stop("\nSpecify a folder in target package")
	pkgPath <- system.file(package = pkg)
	if (is.null(pkgPath) || pkgPath=="")
		stop(paste("\nPackage '",pkg,"' not on user's system",sep=""))
	pkgDir = paste(pkgPath,where,sep = "/")
	if (!file.exists(pkgDir))
		stop(paste("\n",pkgDir,"\ndoes not exist",sep=""))
	pkgList <- list(path=pkgDir)
	if (win) 
		try(setWinVal(pkgList, winName=tcall(.PBSmod)$.activeWin), silent=TRUE)
	invisible(pkgDir) }

# Shortcut functions:
.getApath = function(where="admb",    pkg="PBStools", win=FALSE) { .grabPath(where, pkg, win) }
.getEpath = function(where="examples",pkg="PBStools", win=FALSE) { .grabPath(where, pkg, win) }
.getSpath = function(where="sql",     pkg="PBStools", win=FALSE) { .grabPath(where, pkg, win) }
.getWpath = function(where="win",     pkg="PBStools", win=FALSE) { .grabPath(where, pkg, win) }

.setApath = function(where="admb",    pkg="PBStools", win=TRUE)  { .grabPath(where, pkg, win) }
.setEpath = function(where="examples",pkg="PBStools", win=TRUE)  { .grabPath(where, pkg, win) }
.setSpath = function(where="sql",     pkg="PBStools", win=TRUE)  { .grabPath(where, pkg, win) }
.setWpath = function(where="win",     pkg="PBStools", win=TRUE)  { .grabPath(where, pkg, win) }
#----------------------------------------.grabPath


## .plotDev-----------------------------2021-04-21
## Save plot on current devise using values from a GUI, if available.
## ---------------------------------------------RH
.plotDev = function(nam=NULL, act=NULL, lang=c("e"))
{
	if (is.null(nam)) {
		if (exists(".PBSmod",envir=.PBSmodEnv) && !all(substring(names(tcall(.PBSmod)),1,1)=="."))
			nam=getWinVal(scope="L")$plotname
		if (is.null(nam) & exists("PBStool",envir=.PBStoolEnv)) nam=ttcall(PBStool)$plotname
		if (is.null(nam)) nam="Rplot"
	}
	if (is.null(act)) {
		if (exists(".PBSmod",envir=.PBSmodEnv) && !all(substring(names(tcall(.PBSmod)),1,1)==".")) {
			act=getWinAct()[1]
			if (!any(act==c("wmf","emf","png","jpg","jpeg","bmp","tif","tiff","ps","eps","pdf")))
				act="png"
		}
		else act="png"
	}
	outnam = paste(nam,act,sep=".")
	createFdir(lang)
	fout.e = fout = outnam
	for (l in lang) {
		changeLangOpts(L=l)
		fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		morsels = list.files(switch(l,'e'=".",'f'="./french"), pattern=paste0("\\.",act))
		if (length(morsels)>0 && any(grepl(nam,morsels))) {
			bites = grep(nam,morsels,value=TRUE)
			nbite = length(bites)
			fout  = sub(paste0("\\.",act),paste0(nbite,".",act),fout)
		}
		savePlot(fout,type=act)
	}; eop()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.plotDev


#.setCWD--------------------------------2007-09-20
# Return the current working directory and if win=TRUE, 
# also set the path variable in the active window
#-----------------------------------------------RH
.setCWD = function(win=TRUE) {
	path = getwd()
	cwdList = list(path = path)
	if (win) setWinVal(cwdList,winName=tcall(.PBSmod)$.activeWin)
	invisible(path) }


#.su------------------------------------2011-09-12
# Shortcut for sort(unique(x))
#-----------------------------------------------RH
.su = function(x) { sort(unique(x)) }

