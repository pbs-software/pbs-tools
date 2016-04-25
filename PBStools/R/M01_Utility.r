#===============================================================================
# Module 1: Utility
# -----------------
#  addStrip........Add a vertical colour strip as a legend.
#  biteData........Subsets a data matrix/frame using input vector.
#  chewData........Remove records that contribute little information to factor categories.
#  confODBC........Set up an ODBC User Data Source Name (DSN).
#  convCT..........Convert a crossTab object to regular matrix or data frame.
#  convFY..........Convert dates into fishing years.
#  convYM..........Convert date limits into a vector of year-months (YYY-MM).
#  convYP..........Convert dates into year periods.
#  countLines......Count the number of lines in an ASCII file.
#  createDSN.......Create entire suite of DSNs for the groundfish databases.
#  crossTab........Use package 'reshape' to summarize z using crosstab values y.
#  fitLogit........Fit binomial data using logit link function.
#  flagIt..........Labels a coordinate using a diagonal line radiating from it.
#  getData.........Get data from a variety of sources.
#  getFile.........Get a dataset (binary libraries, binary local, dumped data, comma-delimited text.
#  getName.........Get the names of the input object.
#  getODBC.........Get a string vector of ODBC drivers on user's Windows system.
#  installPkgs     Install specified packages if they are missing or if newer versions are available.
#  isThere.........Check to see if object physically exists in the specified environment.
#  lenv............Get the local/parent/global environment.
#  listTables......List tables in specified SQL, ORA, or MDB database.
#  prime...........Report the prime numbers given an integer vector.
#  quantBox........Redefine boxplot to show quantiles.
#  revStr..........Reverse characters in a string.
#  runModules......Display a master GUI to display modules.
#  scaleVec........Scales a vector to span a target minimum and maximum.
#  showError.......Display error message on device surface.
#  spooler.........Spools list objects into fields of data frames.
#  stdConc.........Standardise a chemical concentration.
#  subsetFile......Subset an ASCII file every n rows (enrow).
#  toUpper.........Capitalise first letter of each word in phrase
#  ttget...........Provide wrappers for PBSmodelling functions tget/tcall/tprint/tput/lisp
#  wrapText........Wrap, mark and indent a long text string.
#  zapDupes........Delete duplicated records based on specified index.
#
#-----Supplementary hidden functions-----
#  .chooseFQT......Choose a file/query/table from the user-specified path.
#  .flush.cat......Flush the cat down the console.
#  .grabPath.......Return the specified directory in the package tree.
#  .getApath.......Return the path for admb/examples/sql/win directories in PBStools.
#  .setApath.......Set the path for admb/examples/sql/win directories in PBStools.
#  .plotDev........Save plot on current devise using values from a GUI, if available.
#  .setCWD.........Return the current working directory and if win=TRUE, set the path variable in the active window.
#  .su.............Shortcut for sort(unique(x))
#===============================================================================

### RH: 2015-11-30 -- Virtualization of SVBCPBSGFIIS
.PBSserver = c(
  GFDB="199.60.94.98",
  SVBCPBSGFIIS="199.60.94.98",
  PACPBSGFDB="199.60.95.200",
  GFDBtemp="PAC03450/GFDB",
  oldSVBCPBSGFIIS="199.60.95.134")

.rgbBlind = list(black=c(0,0,0),orange=c(230,159,0),skyblue=c(86,180,233),bluegreen=c(0,158,115),
	yellow=c(240,228,66),blue=c(0,114,178),vermillion=c(213,94,0),redpurple=c(204,121,167))
.colBlind = sapply(.rgbBlind,function(x){rgb(x[1],x[2],x[3],maxColorValue=255)})

#addStrip-------------------------------2015-12-01
# Add a vertical colour strip as a legend.
#-----------------------------------------------RH
addStrip = function (x, y, col, lab, xwidth=0.01, yheight=0.3, ...) 
{
	if (dev.cur()>1) { oldpar=par(no.readonly=TRUE); on.exit(par(oldpar)) }
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
	polygon(xpol,ypol,border="gray30",col=col)
	text(xval[2]+0.25*xw0, yval[1:ncol]+diff(yval)/2,labels=lab,cex=0.9,adj=0)
#browser();return()
	invisible()
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~addStrip

#biteData-------------------------------2008-11-10
# Subsets a data matrix/frame using input vector.
#-----------------------------------------------RH
biteData = function(dat,vec) {
	if (nrow(dat)==0 || is.null(vec)) return(dat)
	fld=as.character(substitute(vec))
	if (!any(fld==dimnames(dat)[[2]])) return(dat)
	expr=paste("bdat=dat[is.element(dat[,\"",fld,"\"],",deparse(unique(unlist(vec))),"),]",sep="")
	eval(parse(text=expr))
	return(bdat) }

#chewData-------------------------------2009-01-13
# Remove records that contribute little information to factor categories.
#-----------------------------------------------RH
chewData=function(dat,fac,nmin=3,na.rm=TRUE) {
	if (nrow(dat) == 0 || is.null(fac)) return(dat)
	fld = as.character(substitute(fac))
	if (!any(fld == dimnames(dat)[[2]])) return(dat)
	ldat=split(dat[,fac],dat[,fac])
	ndat=sapply(ldat,function(x){length(x[!is.na(x)])})
	ndat=ndat[ndat>=nmin & !is.na(ndat)] # get rid of factors with few occurrences
	if (na.rm) ndat=ndat[names(ndat)!=""]
	dat=dat[is.element(dat[,fac],names(ndat)),]
	return(dat) }


#confODBC-------------------------------2010-06-02
# Set up an ODBC User Data Source Name (DSN)
#-----------------------------------------------RH
confODBC <- function(dsn="PacHarvest",server="GFDB",db="PacHarvest",
                     driver="SQL Server",descr="",trusted=TRUE) {
	#use forward slashes "/" for server otherwise the translation
	#is too dependent on the number of times "\" is escaped
	#getFile(".PBSserver",path=.getSpath(),tenv=penv())
	if (is.element(server,names(.PBSserver))) server <- .PBSserver[server]
	syntax <- paste("{CONFIGDSN \"",driver,"\" \"DSN=",dsn,
		"|Description=",descr,"|SERVER=",server,"|Trusted_Connection=",
		ifelse(trusted,"Yes","No"),"|Database=",db,"\"}",sep="")
	syntax=gsub("/","\\\\",syntax) # finally convert "/" to "\\"
	cmd <- paste("odbcconf.exe /a",syntax)
	system(cmd)
	invisible() }

#convCT---------------------------------2014-12-12
# Convert a crossTab object to regular matrix or data frame.
#-----------------------------------------------RH
convCT = function(CT, fn=as.matrix, colAsRowName=TRUE) {
	fnam = as.character(substitute(fn))
	if (!is.element(fnam,c("as.matrix","as.data.frame"))) return(CT)
	NT = fn(CT[,-1])
	class(NT) = sub("as\\.","",fnam)
	if (colAsRowName) dimnames(NT) = list(CT[,1],dimnames(CT)[[2]][-1])
	else  dimnames(NT) = list(dimnames(CT)[[1]],dimnames(CT)[[2]][-1])
	return(NT)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~convCT

#convFY---------------------------------2011-12-05
# Convert dates into fishing years.
#-----------------------------------------------RH
convFY = function(x,startM=4) { # local function to get fishing years
	if (class(x)[1]=="character" && class(try(as.Date(x),silent=TRUE))=="Date" ) 
		x=as.Date(x)
	if (any(class(x)%in%c("POSIXct","POSIXt","Date")))
		cdate=format(x) # 10-character date "yyyy-mm-dd"
	else  return(rep("",length(x)))
	yrmo=substring(x,1,7) # year-month "yyyy-mm"
	yr=as.numeric(substring(yrmo,1,4)); mo=as.numeric(substring(yrmo,6,7))
	fyr=yr; sM=is.element(mo,startM:12); fyr[!sM]=fyr[!sM]-1; names(fyr)=yrmo
	return(fyr) }

#convYM---------------------------------2011-12-05
# Convert date limits into a vector of year-months (YYY-MM).
#-----------------------------------------------RH
convYM = function(x) { 
	if (class(x)[1]=="character" && class(try(as.Date(x),silent=TRUE))=="Date" ) 
		x=as.Date(x)
	if (any(class(x)%in%c("POSIXct","POSIXt","Date")))
		cdate=format(x) # 10-character date "yyyy-mm-dd"
	else  return(rep("",length(x)))
	yrmo=substring(x,1,7) # year-month "yyyy-mm"
	yr=as.numeric(substring(yrmo,1,4)); mo=as.numeric(substring(yrmo,6,7))
	yrs=range(yr); yrs=yrs[1]:yrs[2]; nyrs=length(yrs)
	X=paste(rep(yrs,each=12),rep(pad0(1:12,2),nyrs),sep="-")
	i=match(yrmo,X)
	xout=X[i[1]:i[2]]
	return(xout) }

#convYP---------------------------------2011-12-05
# Convert dates into year periods.
#-----------------------------------------------RH
convYP = function(x, ndays=90) {
	yearperiod=function(dchar, ndays){ # date as character string
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
		return(out) }
	if (class(x)[1]=="character" && class(try(as.Date(x),silent=TRUE))=="Date" ) 
		x=as.Date(x)
	if (any(class(x)%in%c("POSIXct","POSIXt","Date")))
		cdate=format(x) # 10-character date "yyyy-mm-dd"
	else  return(rep("",length(x)))
	return(yearperiod(cdate,ndays)) }

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

#createDSN------------------------------2009-02-26
# Create entire suite of DSNs for the groundfish databases
#-----------------------------------------------RH
createDSN <- function(trusted=TRUE) {
	confODBC(dsn="GFBioSQL",    server="GFDB",db="GFBioSQL",     driver="SQL Server",descr="Created for PBStools",trusted=trusted)
	confODBC(dsn="GFCatch",     server="GFDB",db="GFCatch",      driver="SQL Server",descr="Created for PBStools",trusted=trusted)
	confODBC(dsn="GFCruise",    server="GFDB",db="GFCruise",     driver="SQL Server",descr="Created for PBStools",trusted=trusted)
	confODBC(dsn="PacHarvest",  server="GFDB",db="PacHarvest",   driver="SQL Server",descr="Created for PBStools",trusted=trusted)
	confODBC(dsn="PacHarvHL",   server="GFDB",db="PacHarvHL",    driver="SQL Server",descr="Created for PBStools",trusted=trusted)
	confODBC(dsn="PacHarvSable",server="GFDB",db="PacHarvSable", driver="SQL Server",descr="Created for PBStools",trusted=trusted)
}

#crossTab-------------------------------2015-03-06
# Use package 'reshape' to summarize z using crosstab values y.
#-----------------------------------------------RH
crossTab = function(x=PBSdat, y=c("year","major"), 
   z="landed", func=function(x){sum(x)/1000.}, ...) {
	if (!requireNamespace("reshape", quietly = TRUE)) stop("`reshape` package is required")
	x=x;  flds=names(x)
	if (!all(is.element(setdiff(y,"year"),flds)))
		stop ("Not all specified 'z' in dataframe")
	if (is.element("year",y) && !is.element("year",names(x))) {
		if (is.element("date",flds)) x$year=convFY(x$date,1)
		else stop("Need 'date' field to calculate 'year'") }
	Y=reshape::melt.data.frame(x,y,z)
	expr=paste("Z=reshape::cast(Y,", paste(paste(ifelse(length(y)==1,"~",""),y,sep=""),collapse="~"), ",func,...)",sep="")
	eval(parse(text=expr))
	return(Z) }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~crossTab

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

#flagIt---------------------------------2012-06-13
# Takes a coordinate (a,b) and labels it using a 
# diagonal line of radius r and angle A.
#  a,b : midpoint of a circle
#  A   : angle (degrees) to radiate out
#  r   : radius of the circle
#  n   : number of times to decrement the radius (max=5)
#-----------------------------------------------RH
flagIt = function(a, b, A=45, r=0.2, n=1, ...){
	xlim = par()$usr[1:2]
	xdif = abs(diff(xlim))
	ylim = par()$usr[3:4]
	ydif = abs(diff(ylim))
	xpin = par()$pin[1]
	ypin = par()$pin[2]
	rads = pi*A/180
	xsig = sign(cos(rads))  # sign of x (not used)
	ysig = sign(sin(rads))  # sign of y (not used)
	
	r   = r - (n-1)*r/5     # reduce when n increases
	x0  = a + r*cos(rads)
	y   = b + r*sin(rads)
	x   = x0 + (x0-a)*(xdif/ydif)*(ypin/xpin)  # adjust for diffences in axis scale and plot dimensions (aspect ratio)
	xvec = c(a,x)
	yvec = c(b,y)
	lines(xvec,yvec,lty=3,col="grey20")
	text(x,y,paste("(",signif(a,3),", ",signif(b,3),")",sep=""),...)
	return(invisible(list(xvec=xvec,yvec=yvec,rads=rads,x0=x0,x=x,y=y)))
}

#getData--------------------------------2015-10-28
# Get data from a variety of sources.
# subQtrust -- if user has no DFO trusted credentials:
#   if (type=="SQL"), c(trusted, uid, pwd) copied to `subQtrust'
#   if (type=="ORA") user MUST supply `subQtrust' list.
#-----------------------------------------------RH
getData <-function(fqtName, dbName="PacHarvest", strSpp=NULL, server=NULL,
   type="SQL", path=getwd(), trusted=TRUE, uid="", pwd="",
   subQtrust = list(trusted=TRUE, uid="", pwd=""),
   noFactors=TRUE, noLogicals=TRUE, rownum=0, mindep=NULL, 
   maxdep=NULL, surveyid=NULL, survserid=NULL, fisheryid=NULL, 
   logtype=NULL, doors=NULL, speed=NULL, mnwt=NULL, tarSpp=NULL, 
   major=NULL, top=NULL, dummy=NULL, senv=NULL, tenv=.GlobalEnv, ...)
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
		fqtName=get(strQ,envir=pfenv)       ### variable string name
	else fqtName=strQ
	envs=sys.frames()                      ### list all environments currently open
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
		### If a direct expression, execute it. If no species, grab a table.
		if (any(type==c("SQLX","ORAX")) || is.null(strSpp) || strSpp=="") {
			isExpr = ifelse(any(type==c("SQLX","ORAX")),TRUE,FALSE)
			expr=paste(c("datt=.getSQLdata(dbName=\"",dbName,"\"",
				",qtName=", ifelse(isExpr,"NULL",paste("\"",fqtName,"\"",sep="")),
				",strSQL=", ifelse(isExpr,paste("\"",fqtName,"\"",sep=""),"NULL"),
				",server=\"",server,"\",type=\"",substring(type,1,3),"\",rownum=",rownum,
				",trusted=",trusted,",uid=\"",uid,"\",pwd=\"",pwd,"\",...)"),collapse="")
#browser();return()
			timeQ0=proc.time()[1:3]  ### start timing SQL query
			eval(parse(text=expr)) 
			timeQ = round(proc.time()[1:3]-timeQ0,2) }
		else {
			data(species,envir=penv())
			suni = function(x) {sort(unique(x))}
			alfSpp=suni(species$code[species$fish])                 ### all fish species
			trfSpp=suni(species$code[species$rf])                   ### total rockfish species
			orfSpp=setdiff(trfSpp,"396")                            ### other rockfish species
			tffSpp=suni(species$code[species$ff])                   ### total flatfish species
			offSpp=setdiff(tffSpp,"602")                            ### other flatfish species
			carSpp=suni(species$code[is.element(species$taxon,2)])  ### cartilaginous species
			sssSpp=setdiff(carSpp,c("065","066"))                   ### shark and skate species
			invSpp=suni(species$code[species$invert])               ### invertebrates
			t01Spp=suni(species$code[is.element(species$taxon,1)])  ### taxon 1:  Agnatha (hagfish, lampreys)
			t02Spp=suni(species$code[is.element(species$taxon,2)])  ### taxon 2:  Chondrichthyes (sharks, rays, chimaeras)
			t03Spp=suni(species$code[is.element(species$taxon,3)])  ### taxon 3:  Clupeiformes (herring, anchovies)
			t04Spp=suni(species$code[is.element(species$taxon,4)])  ### taxon 4:  Salmonidae (salmon, trout)
			t05Spp=suni(species$code[is.element(species$taxon,5)])  ### taxon 5:  Osmeridae (smelts)
			t06Spp=suni(species$code[is.element(species$taxon,6)])  ### taxon 6:  Myctophidae (lanternfishes)
			t07Spp=suni(species$code[is.element(species$taxon,7)])  ### taxon 7:  Gadidae (codfishes)
			t08Spp=suni(species$code[is.element(species$taxon,8)])  ### taxon 8:  Macrouridae (grenadiers)
			t09Spp=suni(species$code[is.element(species$taxon,9)])  ### taxon 9:  Scombridae (mackerels, tunas)
			t10Spp=suni(species$code[is.element(species$taxon,10)]) ### taxon 10: Scorpaenidae (scorpionfishes)
			t11Spp=suni(species$code[is.element(species$taxon,11)]) ### taxon 11: Hexagrammidae (greenlings)
			t12Spp=suni(species$code[is.element(species$taxon,12)]) ### taxon 12: Pleuronectiformes (flounders, soles, halibut)
			t00Spp=intersect(alfSpp,suni(species$code[!is.element(species$taxon,1:12)])) ### taxon 00: All fish not part of a taxon group
			if (is.null(mnwt)) {
				if (type=="ORA") 
					mnwt=species[strSpp,"foswt",drop=FALSE]
				else 
					mnwt=species[strSpp,c("gfbcwt","gfbrwt"),drop=FALSE]
				mnwt=apply(mnwt,1,function(x){if(all(is.na(x))) 1.0 else max(x[!is.na(x)])}) 
				mnwt=mean(mnwt)} ### mean weight of one or more species in kg
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
			# If Oracle SQL is called from SQL Server via OPENQUERY
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
			#-------------------------------------------------------------
			qnam <- paste(path,fqtName,sep="/")
			strQ <- readLines(qnam)
			strQ <- PBSmodelling::.trimWhiteSpace(strQ)
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
			isC  <- begC>0                               ### identify comments
			strQ[isC]=substring(strQ[isC],0,begC[isC]-1) ### strip comments
			strQ <- strQ[!strQ==""]                      ### strip blank lines
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
			# If Oracle SQL is called from SQL Server via OPENQUERY
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
			#--------------------------------------------------------
			strQ <- gsub(pattern="@mnwt",replacement=mnwt,x=strQ)
			strQ <- gsub(pattern="@mindep",replacement=ifelse(is.null(mindep),0,mindep),x=strQ)
			strQ <- gsub(pattern="@maxdep",replacement=ifelse(is.null(maxdep),1200,maxdep),x=strQ)
			if (is.null(survserid) && is.null(surveyid))
				strQ <- gsub(pattern="@originid",replacement="'Y'",x=strQ)
			else
				strQ <- gsub(pattern="@originid",replacement="'Y','N'",x=strQ)
			if (is.null(surveyid)) {
				SQLdat = .getSQLdata(dbName="GFBioSQL", qtName=NULL, strSQL="select SURVEY_ID FROM SURVEY", 
					server="SVBCPBSGFIIS", type="SQL", trusted=subQtrust[["trusted"]], uid=subQtrust[["uid"]], pwd=subQtrust[["pwd"]])
				surveyid = sort(unique(SQLdat[[1]]))
			}
			strQ <- gsub(pattern="@surveyid",replacement=paste(surveyid,collapse=","),x=strQ)
#browser();return()
			if (is.null(survserid)) {
				SQLdat = .getSQLdata(dbName="GFBioSQL", qtName=NULL, strSQL="select SURVEY_SERIES_ID FROM SURVEY", 
					server="SVBCPBSGFIIS", type="SQL", trusted=subQtrust[["trusted"]], uid=subQtrust[["uid"]], pwd=subQtrust[["pwd"]])
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
			strQ <- gsub(pattern="@dummy",replacement=ifelse(is.null(dummy),"''",
				ifelse(is.numeric(dummy),paste(dummy,collapse=","),
				ifelse(is.character(dummy),paste("'",paste(dummy,collapse="','"),"'",sep=""),"''"))),x=strQ)
			#assign("sql",strQ,envir=tenv)
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
	invisible(strQ) }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~getData

#.getSQLdata----------------------------2014-05-01
# Retrieves a data frame from SQL Server
#-----------------------------------------------RH
.getSQLdata <- function(dbName, qtName=NULL, strSQL=NULL,
     server=NULL, type="SQL", trusted=TRUE, uid="", pwd="", 
     rownum=0,...) {
	#if (!require(RODBC, quietly=TRUE)) stop("`RODBC` package is required")
	### Use forward slashes "/" for server otherwise the translation
	### is too dependent on the number of times "\" is escaped
	if (is.null(server) || server=="") {
		#getFile(".PBSserver", path = .getSpath(),tenv=penv())
		server = .PBSserver[1]; type="SQL" }
	driver= list(...)$driver
#browser();return()
	if (type=="SQL") driver=ifelse(is.null(driver),"SQL Server",driver)
	#else if (type=="ORA") driver="Oracle ODBC Driver" ### "Microsoft ODBC for Oracle"
	else if (type=="ORA") driver=ifelse(is.null(driver),"Oracle in OraClient11g_home1",driver)
	else showError("Only 'SQL' and 'ORA' supported at present")
	syntax <- paste("Driver={",driver,"}",
		ifelse(type=="ORA" && substring(driver,1,1)=="O",";DBQ=",";Server="),server,
		";Database=",dbName,";Trusted_Connection=",ifelse(trusted,"Yes","No"),sep="")
	if (!trusted) syntax <- paste(syntax,";UID=",uid,";PWD=",pwd,sep="")
	if (type=="ORA") syntax = paste(syntax,";TLO=0;QTO=F",sep="")
	syntax=gsub("/","\\\\",syntax) ### finally convert "/" to "\\"
	assign("cns",syntax,envir=.PBStoolEnv)
#if (type=="ORA") {browser();return()}
	cnn <- odbcDriverConnect(connection=syntax)
	if (is.null(qtName) && is.null(strSQL))
		showError("Must specify either 'qtName' or 'strSQL'")
	if (!is.null(qtName)) {
		### sqlFetch doesn't work for Oracle tables (thanks Norm)
		if (type=="ORA")
			query=paste("SELECT * FROM ", paste(dbName,qtName,sep="."),
				ifelse(rownum>0,paste(" WHERE ROWNUM <=",rownum),""),sep="")
		else
			query=paste("SELECT ",ifelse(rownum>0,paste("TOP",rownum),"")," * FROM ",qtName,sep="")
		dat <- sqlQuery(cnn, query, rows_at_time=1) }
	else { 
		arg.list = list(...)[!is.element(names(list(...)),"driver")]
		# seems you cannot just pass a list into ..., even if ... is rendered as a list by the function.
		if (length(arg.list)>0) { 
			arg.vec = sapply(names(arg.list),function(x){paste(x,"=",paste(deparse(arg.list[[x]]),collapse=""),sep="")}) # deparse breaks lines
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
	return(dat) }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.getSQLdata

#.getMDBdata----------------------------2010-07-22
# Retrieves a data frame from MDB query or table
#-----------------------------------------------RH
.getMDBdata <- function(mdbTable, qtName, rownum=0, ...) {
	#if (!require(RODBC, quietly=TRUE)) stop("`RODBC` package is required")
	cnn <- odbcConnectAccess(access.file=mdbTable)
	query=paste("SELECT ",ifelse(rownum>0,paste("TOP",rownum),"")," * FROM ",qtName,sep="")
	dat <- sqlQuery(cnn, query, ...)
	odbcClose(cnn)
	return(dat) };

#.getDBFdata----------------------------2010-07-22
# Retrieves data from a DBF file
#-----------------------------------------------RH
.getDBFdata <- function(dbfTable, qtName, ...) {
	if (nchar(qtName)>8) showError("Rename DBF file using 8 or less characters")
	#if (!require(RODBC, quietly=TRUE)) stop("`RODBC` package is required")
	cnn <- odbcConnectDbase(dbf.file=dbfTable)
	dat <- sqlQuery(cnn, paste("SELECT * FROM",qtName),...)
	odbcClose(cnn)
	return(dat) };

#.getXLSdata----------------------------2013-01-30
# Retrieves data from an XLS worksheet
# Note: Data truncated to 255 characters with Excel ODBC driver
# Fix : http://support.microsoft.com/kb/189897/en-us
# regjump HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Jet\4.0\Engines\Excel
# Set TypeGuessRows DWORD Value to 0 (scan all rows). OK for most small tables.
#-----------------------------------------------RH
.getXLSdata <- function(xlsTable, qtName, ...) {
	#if (!require(RODBC, quietly=TRUE)) stop("`RODBC` package is required")
	cnn <- odbcConnectExcel(xls.file=xlsTable)
	dat <- sqlFetch(cnn, qtName, ...)
	odbcClose(cnn)
	return(dat) };
#====================================getData group

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


#getName--------------------------------2009-05-11
# Get the names of the input object.
# If fnam exists as a list, it returns the names of the list.
# If fnam exists as a string vector, it returns the strings.
# If fnam does not exist, it simply returns itself.
#-----------------------------------------------RH
getName=function(fnam){
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
	return(fnam) }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~getName


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


#installPkgs----------------------------2016-04-13
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
			install.packages(paste0(locdir,"/",bins[sapply(upd.pkg,grep,bins)]), dependencies=TRUE, repos=NULL, ...)
		 else
			install.packages(upd.pkg, dependencies=TRUE, repos=repos, ...)
	} else
	cat("No new versions of requested packages installed\n")
	if (also.load)
		sapply(pkg, require, character.only = TRUE)
	invisible(new.ver)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~installPkgs


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

#listTables-----------------------------2013-01-17
# List tables in specified SQL, ORA, or MDB database.
#-----------------------------------------------RH
listTables <- function (dbName, pattern=NULL, path=getwd(), 
   server=NULL, type="SQL", ttype=NULL, trusted=TRUE, uid="", 
   pwd="", silent=FALSE, tenv=.GlobalEnv)
{
	if (is.null(server)) {
		#getFile(".PBSserver", path=.getSpath(), tenv=penv())
		server = .PBSserver[1] }
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
		cnn <- odbcDriverConnect(connection = syntax)  }
	else if (type=="MDB") {
		cnn = odbcConnectAccess(access.file=mdbTable) }
	assign("cns",cnn,envir=tenv)
	if (type=="ORA") dat = sqlTables(cnn,schema=dbName)
	else             dat = sqlTables(cnn)
	assign("PBSdat",dat,envir=tenv)
	odbcClose(cnn)

	if (!is.null(ttype)) dat=dat[is.element(dat$TABLE_TYPE,ttype),]
	tabs = dat$TABLE_NAME
	if (!is.null(pattern)) tabs <- findPat(pattern,tabs)
	if (!silent) print(tabs)
	invisible(tabs) }
#---------------------------------------listTables


#prime----------------------------------2010-03-25
# Report the prime numbers given an integer vector
#-----------------------------------------------RH
prime = function(x=2:100){
	is.prime = function(xx) {
		if (xx<=1) return(FALSE)
		else if (is.element(xx,2:3)) return(TRUE)
		else !is.element(0,xx%%2:floor(sqrt(xx))) }
	if (!is.vector(x) || !is.numeric(x) || !all(x%%1==0))
		stop("'x' must be a vector of integers")
	z = sapply(x,is.prime) 
	if (any(z)) return(x[z])
	else return(NULL) }


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
		try(setWinVal(pkgList, winName=tcall(.PBSmod)$.activeWin),silent=TRUE)
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

#.plotDev-------------------------------2008-09-19
# Save plot on current devise using values from a GUI, if available.
#-----------------------------------------------RH
.plotDev = function(nam=NULL,act=NULL){
	if (is.null(nam)) {
		if (exists(".PBSmod",envir=.PBSmodEnv) && !all(substring(names(tcall(.PBSmod)),1,1)=="."))
			nam=getWinVal(scope="L")$plotname
		if (is.null(nam) & exists("PBStool",envir=.PBStoolEnv)) nam=ttcall(PBStool)$plotname
		if (is.null(nam)) nam="Rplot" }
	if (is.null(act)) {
		if (exists(".PBSmod",envir=.PBSmodEnv) && !all(substring(names(tcall(.PBSmod)),1,1)==".")) {
			act=getWinAct()[1]
			if (!any(act==c("wmf","emf","png","jpg","jpeg","bmp","tif","tiff","ps","eps","pdf")))
				act="wmf" }
		else act="wmf" }
	savePlot(paste(nam,act,sep="."),type=act) }

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

