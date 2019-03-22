## =============================================================================
## Module 3: Fishery
## -----------------
##  calcRatio.......Calculates ratios of numerator to denominator (e.g., 'discard'/'catch').
##  dumpMod.........Dump catch from modern sources used in catch reconstruction.
##  dumpRat.........Dump catch ratios calculated by a catch reconstruction.
##  getCatch........Get catch records for a species from various databases and combine.
##  glimmer.........Performs an lm on a predefined dataset.
##  makeCATtables...Make CSV files containing catch from commercial fisheries.
##  plotCatch.......Plot catch history as annual barplot using specified catch fields.
##  plotFOScatch....Plot catch as monthly barplots from 'fos_catch.sql' query to GFFOS.
##  runCCA..........Catch-curve model based on Schnute and Haigh (2006).
##  sumCatTabs......Summarize catch by year & PMFC from modern catch data used in catch reconstruction.
##  trackBycat......Track annual fish group catches between depth limits.
## =============================================================================

## calcRatio----------------------------2013-07-19
##  Calculates ratios of the numerator field to the
##  denominator field (e.g., 'discard'/'catch').
##  nfld   - numerator field
##  dfld   - denominator field
##  nzero  - if T, keep 0-value numerators
##  dzero  - if T, keep 0-value denominators
##  major  - specific PMFC areas specified as numeric codes
##  startM - Month of year to start the year
## ---------------------------------------------RH
calcRatio <- function(dat, nfld, dfld, nzero=TRUE, dzero=TRUE, sumF=mean, 
     major=NULL, startM=1, plot=FALSE, ylim=NULL, wmf=FALSE, quiet=FALSE, ioenv=.GlobalEnv) {

	assign("PBStool",list(module="M03_Fishery",call=match.call(),args=args(calcRatio),ioenv=ioenv),envir=.PBStoolEnv)
	fnam=as.character(substitute(dat))
	expr = paste("getFile(\"",fnam,"\",senv=ioenv,try.all.frames=TRUE,tenv=penv()); dat=",fnam,sep="")
	eval(parse(text=expr))
	flds=names(dat)
	if (any(flds=="POP") && any(flds=="ORF")) {
		dat[["TRF"]]=dat[["POP"]]+dat[["ORF"]]                 # total rockfish
		flds=union(flds,"TRF") }
	if (missing(nfld) || is.null(nfld) || nfld=="")
		if (quiet) return(NULL) else showError("Specify a numerator field")
	if (missing(dfld) || is.null(dfld) || dfld=="") 
		if (quiet) return(NULL) else showError("Specify a denominator field")
	need=c("date","major",nfld,dfld); have=is.element(need,flds)
	if (!all(have))
		if (quiet) return(NULL) else showError(paste("'",paste(need[!have],collapse="' , '"),"'",sep=""),type="nofields")
	dat$fyear=convFY(dat$date,startM)                         # fishing year can start at whatever month specified
	zNA=is.na(dat[,nfld]) | is.na(dat[,dfld]); dat=dat[!zNA,] # get rid of NAs
	if (!nzero) dat=dat[dat[,nfld]>0,]                        # remove 0-value numerators if user specifies
	if (nrow(dat)==0) if (quiet) return(NULL) else showError(nfld,"nodata")
	if (!dzero) {
		dat=dat[dat[,dfld]>0,]                        # remove 0-value denominators if user specifies
		if (nrow(dat)==0) if (quiet) return(NULL) else showError(dfld,"nodata") }
	else {
		zn0=round(dat[,nfld],5)==0; zd0=round(dat[,dfld],5)==0
		dat[,dfld][!zn0&zd0]=dat[,nfld][!zn0&zd0]              # if num>0 and den=0, set den to num (discarded all)
		dat[,nfld][zn0&zd0]=0; dat[,dfld][zn0&zd0]=.0001       # if num=den=0, set num to 0 and den to .0001 (zero discard rate)
	}

	pmfc=c("UNK","4B","3B","3C","3D","5A","5B","5C","5D","5E",
		"AK","BC","4A"); names(pmfc)=c(0:11,"68")
	if (!is.null(major)) {
		dat=dat[is.element(dat$major,major),]
		if (nrow(dat)==0) if (quiet) return(NULL) else showError("major","nodata") }
	else {
		dat$major[is.na(dat$major) | dat$major==""]=0 }
	major=sort(unique(dat$major)) 
	majors=as.list(sort(unique(dat$major)))
	if (length(major)>1) {
		MAJOR=sort(unique(dat$major))
		majors=c(majors,list(MAJOR))
		shebang="TOT"; names(shebang)=paste(MAJOR,collapse="+")
		pmfc=c(pmfc,shebang) 
		major=c(major,"TOT") }
	nmaj=length(majors)
	fyears=sort(unique(dat$fyear)); nyrs=length(fyears)
	psum = array(NA,dim=c(nyrs,nmaj),dimnames=list(year=fyears,major=major))
	stored=character(0)
	dumbox=as.list(rep(NA,nyrs)); names(dumbox)=fyears        # dummy for boxplots
	for (i in 1:nmaj) {
		ii=majors[[i]]; iii=paste(ii,collapse="_",sep="")
		inam=paste(nfld,"_",dfld,"_",iii,sep="")
		idat=dat[is.element(dat$major,ii),]
		if (nrow(idat)==0) next 
		stored=c(stored,inam)
		dORF=dumbox
		pORF=split(idat[,nfld]/idat[,dfld],idat$fyear)         # list of ratios by year
		dORF[names(pORF)]=pORF
		attr(dORF,"pmfc")=pmfc[iii]
		expr=paste("\"",inam,"\"=dORF; packList(\"",inam,"\",\"PBStool\",tenv=.PBStoolEnv)",sep="")
		eval(parse(text=expr))
		sumV=sapply(pORF,sumF)                                 # summary value
		psum[names(sumV),ifelse(!any(iii==major),"TOT",iii)] = sumV
	}
	attr(psum,"sumF")=apply(psum,2,sumF,na.rm=TRUE)
	if (plot) {
		if (is.null(ylim)) ylim=c(0,max(apply(psum,2,extendrange,f=0.25)))
		nplt=length(stored)
		if (nplt>1) {rc=c(ceiling(nplt/2),2); if (wmf) rc=rev(rc)}
		if (wmf && .Platform$OS.type=="windows")
			do.call("win.metafile",list(filename=paste(nfld,"-",dfld,"-major",paste(major,collapse=""),".wmf",sep=""),width=8,height=2.5*rc[1]))
		else resetGraph()
		if (nplt==1) expandGraph(mfrow=c(1,1),mar=c(3,5,.5,.5),oma=c(0,0,0,0),las=1)
		else         expandGraph(mfrow=rc,mar=c(1.5,0,.5,.5),oma=c(2,5,0,0),las=1)
		for (i in stored) {
			pORF=ttcall(PBStool)[[i]]; ilab=attributes(pORF)$pmfc
			sumV=sapply(pORF,sumF)
			x=1:length(pORF)
			boxplot(pORF,boxwex=0.5,staplewex=0,range=0,lty=1,border="grey",
				col=ifelse(ilab=="TOT","gold","moccasin"),yaxt="n",ylim=ylim)
			nc=par()$mfg[2]
			axis(2,tck=.02,labels=ifelse(nc==1,TRUE,FALSE))
			points(x,sumV,pch=17,col="cornflowerblue",cex=1.2)
			points(x,sumV,pch=2,col="black",cex=1.2)
			text(x,sumV+ifelse(wmf,.05,.06)*diff(par()$usr[3:4]),
				show0(signif(sumV,2),2),col="blue",cex=1,srt=90,adj=c(0,0.4))
			addLabel(.92,.9,ilab,adj=1,cex=1.2,col=ifelse(length(ii)==1,"blue","darkblue"))
		}
		mtext("Year",side=1,outer=TRUE,line=.75,cex=1.2)
		mtext(paste("Ratio",nfld,"/",dfld),side=2,outer=TRUE,line=3.25,cex=1.2,las=0) }
	packList(c("pmfc","psum","stored"),"PBStool",tenv=.PBStoolEnv)
	if(plot&&wmf) dev.off()
	invisible(psum) }
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~calcRatio


#dumpMod--------------------------------2011-06-15
# Dump catch from modern sources used in catch reconstruction.
#-----------------------------------------------RH
dumpMod = function(dat,catch=c("landed","discard"),fid=1:5,strSpp="396",dbs=TRUE) {
	data(pmfc,envir=penv())
	gmu=pmfc$gmu; names(gmu)=pmfc$major
	data(species,envir=penv())
	spp = paste(species[strSpp,"name"]," (",species[strSpp,"latin"],")",sep="")
	DBS = c("PacHarv3","GFCatch","PacHarvest","PacHarvHL (halibut)","PacHarvSable",
		"PacHarvHL (DMP)","PacHarvHL (fisherlogs)","GFFOS")
	names(DBS)= c("ph3cat","gfccat","phtcat","phhcat","phscat","phvcat","phfcat","foscat")
	for (i in catch) {
		if (i=="total") ii = c("landed","discard") else ii = i
		fnam=paste(as.character(substitute(dat)),"-",i,"-",strSpp,".csv",sep="")
		cat(paste("Modern ",toupper(i)," Catch (t) of ",spp,"\n",sep=""),file=fnam)
		for (j in fid) {
			jj = as.character(j)
			cat(paste("\n",toupper(c("Trawl","Halibut","Sablefish","Dogfish-Lingcod","H&L Rockfish"))[j],"\n",sep=""),file=fnam,append=TRUE)
			if (dbs) k = dimnames(dat)$dbs
			else k = "merged"
			for (kk in k) {
				if (class(dat)=="array") {
					if (dbs) ktab = dat[,,jj,ii,kk] # catmod1
					else     ktab = dat[,,jj,ii]    # catmod
					if (i=="total") ktab = apply(ktab,1:2,sum) }
				if (class(dat)=="list") {
					jdat = dat[[jj]]                # ologs
					jtab = crossTab(jdat,c("year","major"),"discard")
					cnam = as.character(intersect(dimnames(jtab)[[2]],c(1,3:9)))
					ktab = as.matrix(jtab[,cnam])
					rnam = as.character(jtab[,"year"]) 
					dimnames(ktab) = list(year=rnam,major=cnam) }
				yrsum = apply(ktab,1,sum)
				if (all(round(yrsum,5)==0)) next
				names(yrsum) = dimnames(ktab)[[1]]
				yrsum = clipVector(yrsum,0)
				ktab = ktab[names(yrsum),,drop=FALSE]
				ktab = cbind(ktab,total=yrsum)
				if (dbs) cat(paste(c(DBS[kk],"\n"),collapse=""),file=fnam,append=TRUE)
				cat(paste(c("Year",gmu[setdiff(dimnames(ktab)[[2]],"total")],"Total"),collapse=",",sep=""),"\n",sep="",file=fnam,append=TRUE)
				yrs = as.numeric(dimnames(ktab)[[1]])
				mess=paste(apply(cbind(yrs,ktab),1,function(x){paste(x,collapse=",",sep="")}),collapse="\n",sep="")
				cat(mess,"\n",file=fnam,append=TRUE)
			}
		}
	}
	if (class(dat)=="array" && !dbs)
		write.csv(apply(dat[,,,c("landed","discard")],c(1,3),sum),
		paste(as.character(substitute(dat)),"-summary-",strSpp,".csv",sep=""))
}
#------------------------------------------dumpMod


#dumpRat--------------------------------2013-01-28
# Dump catch ratios calculated by a catch reconstruction.
#-----------------------------------------------RH
dumpRat = function(strSpp="396", rats=c("alpha","beta","gamma","delta","lambda"), ioenv=.GlobalEnv){
	expr = paste("getFile(PBStool",strSpp,",senv=ioenv,reload=TRUE,tenv=penv()); dat=PBStool",sep="")
	eval(parse(text=expr))
	#data(pmfc); gmu=pmfc$gmu; names(gmu)=pmfc$major
	data(species,envir=penv())
	spp = paste(species[strSpp,"name"]," (",species[strSpp,"latin"],")",sep="")
	code3 = species[strSpp,"code3"]
	RATS = c(paste("Proportion of",code3,"landed in Major ID for each FID"),
	paste("Proportion of H&L",code3,"catch in FIDs (2 4 5) for each Major ID"),
	paste("Ratio of",code3,"landed catch to that of ORF/TRF"),
	paste("Discard rate of",code3,"to landed catch of FID target"),
	"Gear ratios pre- and post-war for early rockfish catch")
	names(RATS)= c("alpha","beta","gamma","delta","lambda")
	fnam=paste("ratios-",strSpp,".csv",sep="")
	cat(paste("Ratio calculations for ",spp,"\n",sep=""),file=fnam)
	for (i in rats) {
		idat = dat[[i]]
		if (class(idat)=="matrix") jdim=1 else jdim=dim(idat)[3]
		cat(paste("\n",i,": ",RATS[i],"\n",sep=""),file=fnam,append=TRUE)
		if (jdim>1) cat(paste(names(dimnames(idat)),collapse="-"),"\n",file=fnam,append=TRUE)
		for (j in 1:jdim) {
			if (jdim==1) jdat = idat
			else {
				jdat = idat[,,j]
				cat(dimnames(idat)[[3]][j],"\n",file=fnam,append=TRUE) }
			mess = paste(names(dimnames(jdat)),collapse=" vs ")
			mess = paste(c(mess,dimnames(jdat)[[2]]),collapse=",")
			cat(mess,"\n",file=fnam,append=TRUE)
			jtab = cbind(as.numeric(dimnames(jdat)[[1]]),jdat)
			mess = paste(apply(jtab,1,function(x){paste(x,collapse=",")}),collapse="\n")
			cat(mess,"\n\n",file=fnam,append=TRUE)
		}
	}
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~dumpRat


## getCatch-----------------------------2018-12-31
##  Extract catch records for a species from various
##  databases and combine them into one commercial 
##  catch data frame and one survey catch data frame.
##  Queries: gfb_catch_records.sql
##           gfc_catch_records.sql
##           pht_catch_records.sql
##           fos_catch_records.sql
## ---------------------------------------------RH
getCatch = function(strSpp="396", dbs=c("gfb","gfc","pht","fos"),
   sql=FALSE, sqlpath=.getSpath(), proBio=FALSE, ioenv=.GlobalEnv)
   #uid=Sys.info()["user"], pwd=uid, ioenv=.GlobalEnv) 
{
	sysyr=as.numeric(substring(Sys.time(),1,4)) # maximum possible year
	if (sql) {
		DBS = c("gfb","gfc","pht","fos")
		if (!all(dbs%in%c("gfb","gfc","pht","fos")))
			showError(paste0("Not all 'dbs' are support by the code.\n\nChoose from ",deparse(DBS)),as.is=TRUE)
		for (i in dbs) {
			qnam = paste(i,"_catch_records.sql",sep="")
			if (i=="gfb")  qstr = "dbName=\"GFBioSQL\",as.is=c(rep(FALSE,15),TRUE,rep(FALSE,3))"  ## as.is for 'spp' field
			else if (i=="gfc") qstr = "dbName=\"GFCatch\""
			else if (i=="pht") qstr = "dbName=\"PacHarvest\""
			else if (i=="fos") qstr = "dbName=\"GFFOS\""
				#paste("dbName=\"GFFOS\",server=\"GFSH\",type=\"ORA\",trusted=FALSE,uid=\"",uid,"\",pwd=\"",pwd,"\"",sep="")
			else showMessage(paste("Database '",i,"' not currently supported.",sep=""))
			expr = paste("getData(\"",qnam,"\",strSpp=\"",strSpp,"\",",qstr,",path=\"",sqlpath,"\",tenv=penv()); ",sep="")
			expr = c(expr,paste("assign(\"cat",strSpp,i,".wB\",PBSdat,envir=.PBStoolEnv); ",sep=""))
			expr = c(expr,paste("save(\"cat",strSpp,i,".wB\",file=\"cat",strSpp,i,".wB.rda\",envir=.PBStoolEnv); ",sep=""))
			eval(parse(text=paste(expr,collapse="")))
		}
	} else {
		for (i in dbs) {
			expr=paste(c("getFile(\"cat",strSpp,i,".wB\", reload=TRUE, senv=ioenv, tenv=.PBStoolEnv)"),collapse="")
			eval(parse(text=expr))
		}
	}
	if (any(dbs=="gfb")) {
		Snam = paste("Scat",strSpp,".wB",sep="")  # Survey catch
		expr = paste(Snam,"=ttcall(cat",strSpp,"gfb.wB); ",sep="")
#browser();return()
		expr = c(expr,paste("save(\"",Snam,"\",file=\"",Snam,".rda\"); ",sep=""))
		if (all(dbs=="gfb"))
			expr = c(expr,paste("out=list(Scat=",Snam,"); ",sep=""))
		eval(parse(text=paste(expr,collapse=""))) 
	}
	dbs.merge = setdiff(dbs,"gfb")
	if (length(dbs.merge)>0) {
		tnames = paste("cat",strSpp,dbs.merge,".wB",sep="")
		FLDS=list()
		for (i in tnames) {
			
			expr=paste("flds=names(ttcall(",i,")); FLDS=c(FLDS,list(",i,"=flds))",sep="")
			eval(parse(text=expr))
		}
		Uflds = unique(as.vector(unlist(FLDS)))  # Union of all available fields
		uflds = sapply(FLDS,function(x,U){x[is.element(x,U)]},U=Uflds,simplify=FALSE) # fields in Uflds
		iflds = Uflds # find intersection of all fields
		for (i in names(uflds)) {
			iflds = intersect(iflds,uflds[[i]])
			if (length(iflds)==0) showError("No fields in common among the specified tables") }
		newtab=NULL
		for (i in tnames) {
			#itab = get(i, envir=.PBStoolEnv)[,iflds]
			eval(parse(text=paste0("itab = ttcall(",i,")[,iflds]")))
			db = substring(i,7,9)
			if (any(db==c("pht","fos"))) {
				mos = as.numeric(substring(itab[,"date"],6,7))
				if (db=="pht") itab = itab[is.element(itab[,"year"],1996:2006) | 
					(is.element(itab[,"year"],2007) & is.element(mos,1:3)),]
				if (db=="fos") {
					z1 = is.element(itab[,"FID"],1) &                               # Trawl
						(is.element(itab[,"year"],2008:sysyr) | 
						(is.element(itab[,"year"],2007) & is.element(mos,4:12)))
					z2 = is.element(itab[,"FID"],2:5) &                             # H&L + Trap
						(is.element(itab[,"year"],2007:sysyr) | 
						(is.element(itab[,"year"],2006) & is.element(mos,4:12)))
					itab = itab[z1 | z2,]
				}
			}
			newtab = rbind(newtab, itab)
		}
		newtab = newtab[!(is.element(newtab$FID,2:5) & !is.element(newtab$log,105)),] # exclude H&L catch not reported by fisherlogs
		row.names(newtab) = 1:nrow(newtab)
		if (is.element("EID",iflds)) newtab[,"EID"] = 1:nrow(newtab)
		#--- save results ---
		Cnam = paste("Ccat",strSpp,".wB",sep="")  # Commercial catch
		if (proBio) expr=paste(Cnam,"=processBio(newtab); ",sep="")
		else expr=paste(Cnam,"=newtab; ",sep="")
		expr = c(expr,paste("save(\"",Cnam,"\",file=\"",Cnam,".rda\"); ",sep=""))
		if (any(dbs=="gfb"))
			expr = c(expr,paste("out=list(Ccat=",Cnam,",Scat=",Snam,"); ",sep=""))
		else
			expr = c(expr,paste("out=list(Ccat=",Cnam,"); ",sep=""))
		eval(parse(text=paste(expr,collapse="")))
	}
	invisible(out)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~getCatch


#glimmer--------------------------------2014-05-06
# Performs an lm/aov on a predefined dataset
# Arguments:
#   file   = name of file or data object containing fields for GLM analysis:
#            (year, month, depth, latitude, vessel, effort, catch). 
#   facs   = string vector: factors (fields) other than those that describe area.
#   afld   = string scalar: factor describing spatial area, choose one of:
#            c("latitude","major","minor","locality","pfma","pfms","srfa","srfs","popa").
#   yrs    = numeric vector of years.
#   mos    = numeric vector of months.
#   mo1    = first month in year (e.g., 1=calendar year, 4=fishing year).
#   dbrks  = either a numeric vector of two or more cut points or a single number 
#           (greater than or equal to 2) giving the number of intervals into which depth is to be cut.
#   gear   = gear codes for trawl fishery (1=bottom, 3=midwater)
#   catch  = string scalar: field containing catch numbers.
#   areas  = vecor: specific area names/codes within 'afld'.
#   Vpro   = numeric scalar: minimum vessel's proportion of catch to total catch.
#   Uplot  = logical: if TRUE, calculate and plot CPUE index and effects on the CPUE index.
#                     if FALSE, plot the parameter coefficients.
#   Upanel = numeric scalar: proportion of space to plot index panel if Uplot=TRUE.
#   Ionly  = logical: if TRUE, plot only the first panel (index).
#   Imax   = numeric scalar: maximum index value for annual index plot.
#   crange = numeric vector: y-limits (min,max) for coefficient plots.
#   erange = numeric vector: y-limits (min,max) for factor effect plots.
#   facmin = numeric vector: minimum number of values before a category is used in the factor.
#            Separate minima can be specified for each factor.
#   effmax = Effort maximum; all records with effort greater than this are discarded.
#   vline  = numeric vector: x-values wherever user wants to draw a vertical line in the Index only plot.
#   wmf    = logical: if TRUE, send the plot to a WMF file.
#-----------------------------------------------RH
glimmer <- function(file, facs=c("year","month","depth","vessel"),
     afld="latitude", yrs=1996:2014, mos=1:12, mo1=1, dbrks=seq(100,500,100), gear=c(0,1,3), 
     catch="catch", areas=NULL, Vpro=0.03, Uplot=TRUE, Upanel=0.4, Ionly=FALSE, Imax=NULL,
     crange=c(-2,2), erange=c(0,5), facmin=10, effmax=1440, vline=NULL, wmf=FALSE, ioenv=.GlobalEnv) {

	### Set factor and order contrasts to sum
	csum = c("contr.sum","contr.sum"); names(csum)=c("factor","ordered")
	ocon = options()$contrasts ### save original contrasts
	options(contrasts=csum)
	AREAS=c("latitude","major","minor","locality","pfma","pfms","srfa","srfs","popa","pjsarea")
	facs=setdiff(union(facs,afld),"")
	facmin=rep(facmin,length(facs))[1:length(facs)]; names(facmin)=facs
	names(facmin)[names(facmin)%in%AREAS]="area"
	names(facmin)[names(facmin)%in%"depth"]="dzone"

	assign("PBStool",list(module="M03_Fishery",call=match.call(),ioenv=ioenv),envir=.PBStoolEnv)
	fnam=as.character(substitute(file))
	expr = paste("getData(\"",fnam,"\",type=\"FILE\",tenv=penv())",sep="")
	eval(parse(text=expr))
	file = PBSdat 
	spp = attributes(file)$spp
	flds <- names(file)
	need=union(facs,c("year","month","depth","effort",catch))
	if (!all(is.element(need,flds)==TRUE))
		showError(paste("File is missing:\nc(",paste(need[!is.element(need,flds)],collapse=","),")"))
	nyrs <- length(yrs); nmos <- length(mos)
	faclab <- paste(LETTERS[match(substring(facs,1,1),letters)],collapse="")

	file <- file[file$effort>0 & file$effort<=effmax & !is.na(file$effort),]
	file <- file[!is.na(file[,catch]),]
	file <- file[is.element(file$year,yrs),]
	file <- chewData(file,"year",facmin["year"])
	if(nrow(file)==0) showError("year","nodata")

	file <- file[is.element(file$month,mos),]
	file <- chewData(file,"month",facmin["month"])
	if(nrow(file)==0) showError("year:month","nodata")

	if (is.null(gear)) gear = .su(file$gear)
	else file = biteData(file,gear)
	if(nrow(file)==0) showError("year:month:gear","nodata")

	if (any(facs=="gear")) {
		z1 = file$year<=1995 & is.element(file$gear,c(1:5,9))  # GFCatch trawl
		if (any(z1)) file$gear[z1] = 1
		z2 = file$year<=1995 & is.element(file$gear,c(6:8))    # GFCatch midwater
		if (any(z2)) file$gear[z2] = 3
		file <- file[is.element(file$gear,gear),]
		file <- chewData(file,"gear",facmin["gear"])
		if(nrow(file)==0) showError("year:month:gear","nodata")
		if (length(gear)==1) 
			facs = setdiff(facs,"gear") 
		else 
			gear = sort(unique(file$gear)) 
	}
#browser();return()

	### Place vessel qualification after selecting years and months
	if (any(facs=="vessel")) {
		if (is.null(Vpro)) Vpro=0 ### minimum vessel's proportion of catch to total catch 
		if (!any(flds=="vessel")) showError(paste("File is missing:\nc(vessel)"))
		file=chewData(file,"vessel",facmin["vessel"])
		Vcat=rev(sort(sapply(split(file[,catch],file$vessel),sum)))
		Vpct=Vcat/sum(Vcat)
		Vtop=Vpct[Vpct>=Vpro]; vtop=names(Vtop)
		vid  = 1:length(vtop); names(vid)=vtop
		file = file[is.element(file$vessel,vtop),]
		if(nrow(file)==0) showError("year:month:vessel","nodata")
		file$vid = vid[as.character(file$vessel)]
	}
	if (any(facs=="depth")) {
		if (length(dbrks)==1)
			dbrks = seq(min(file$depth),max(file$depth),length.out=dbrks)
		file <- file[file$depth>=min(dbrks) & file$depth<max(dbrks) & !is.na(file$depth),]
		if(nrow(file)==0) showError("year:month:depth","nodata")
		file$dzone = cut(file$depth,breaks=dbrks,labels=FALSE,include.lowest=TRUE)
		file=chewData(file,"dzone",facmin["dzone"]) 
		if(nrow(file)==0) showError("year:month:depth","nodata")
		dzone <- sort(unique(file$dzone));  ndzone <- length(dzone)
		names(dzone) = round(dbrks[1:(length(dbrks)-1)])
#browser();return()
	}
	if (any(facs=="bycatch")) {
		quants = c(0.5,0.75,0.90,1)
		bcut = quantile(file$bycatch[file$bycatch>0],quants)
		bcut = c(0,bcut)
		#if(nrow(file)==0) showError("year:month:depth","nodata")
		file$bycat = cut(file$bycatch,breaks=bcut,include.lowest=TRUE,labels=FALSE)
		#file=chewData(file,"dzone",facmin["dzone"])
		bycat <- sort(unique(file$bycat));  nbycat <- length(bycat)
	}
	if (!is.null(afld) || afld!="") {
		if (afld=="latitude") {
			if (any(spp==c("440","396"))) { ### YMR, POP
				acut=c(48,50.2,51,51.6,52.8,54.8)
				file$area=cut(file$latitude,breaks=acut,labels=c("WCVI","Scott","QCS","MG","Dixon")) }
			else if (any(spp==c("410"))) { ### DBR
				acut=c(48,50.1,50.8,51.6,52.2,53.8,54.8)
				file$area=cut(file$latitude,breaks=acut,labels=c("WVI","NVI","QCS","MG","HS","Dixon")) }
			else if (any(spp==c("453"))) { ### LST
				acut=c(48,49.2,50.75,51.93333,53.08333,54.66667)
				file$area=cut(file$latitude,breaks=acut,labels=c("SWVI","NWVI","Tide","Flam","Renn")) }
			else file$area = ceiling(file$latitude/0.5)*0.5 }
		else  file$area <- file[,afld]
		if (afld=="pjsarea") {
			getFile(pjsa,use.pkg=TRUE,tenv=penv())
#browser();return()
			#file = file[is.element(file$major,3:9),]
			ntows=split(file$catch,file$pjsarea)
			ncats=sapply(ntows,function(x){length(x[x>0])})
			ncats=rev(sort(ncats))
			pjskeep = names(ncats[1:29])
			pjslump = names(ncats[30:length(ncats)])
			file$area[is.element(file$area,pjslump)] = 999
		}
		if (!is.null(areas)) file = file[is.element(file$area,areas),]
		if(nrow(file)==0) showError("year:month:area","nodata")
#		file=chewData(file,"area",facmin["area"])
		area <- sort(unique(file$area));  narea <- length(area)
	}
#browser();return()
	facmore=setdiff(names(facmin),c("year","month","vessel","depth","gear","area","bycatch"))
	if (length(facmore)>0) {
		for (i in facmore) {
			eval(parse(text=paste("file=chewData(file,\"",i,"\",facmin[\"",i,"\"])",sep="")))
			eval(parse(text=paste(i,"=sort(unique(file[,\"",i,"\"]))",sep=""))) }
	}
	file$catch = file[,catch] + 0  ### Add value if incl zero tows from qualified records
	file$U     = file$catch/(file$effort/60)  ### Assumes effort in minutes
	file       = file[is.finite(file$U) & file$U>0 & !is.na(file$U),]  ### Get rid of zero and infinite CPUE values
	file$lnU   = log2(file$U)
	
	yrs=sort(unique(file$year))

	dat=list()
	for (i in c(facs,"lnU")) {
		if (i==afld) dat[[i]] = file$area
		else if (i=="depth") dat[[i]] = file$dzone
		else if (i=="vessel") dat[[i]] = file$vid
		else if (i=="bycatch") dat[[i]] = file$bycat
		else dat[[i]]=file[,i]
	}
	dat=fdat=as.data.frame(dat)
#browser();return()

	for (i in facs)
		fdat[,i]  <- as.factor(fdat[,i])
	mod.lm=paste("lmres=lm(lnU~",paste(facs,collapse="+"),",data=fdat)",sep="")
	#mod.aov=paste("aovres=aov(lnU~",paste(facs,collapse="+"),",data=fdat)",sep="")
	eval(parse(text=mod.lm)); #eval(parse(text=mod.aov))

	coeffs <- lmres$coefficients
	if(any(is.na(coeffs))) showError(paste("Not enough data: dim=c(",
		paste(dim(dat),collapse=","),")...loosen your restrictions",sep=""))
	Ubase  <- coeffs[1]
	lmsum  <- summary(lmres,correlation=TRUE)
	stderr <- lmsum$coefficients[,"Std. Error"]
	correl <- lmsum$correlation

	nfac <- length(facs); nplot <- 0
	stuff=c("dat","coeffs","lmres","lmsum","aovres","facmin")
	packList(stuff,"PBStool",tenv=.PBStoolEnv)

	if (wmf && .Platform$OS.type=="windows")
		do.call("win.metafile",list(filename=paste(spp,"-GLM-",ifelse(Uplot,"Cpue-","Para-"),
		substring(faclab,1,ifelse(Ionly,1,nchar(faclab))),".wmf",sep=""),
		width=ifelse(Ionly,6.5,nfac*2.75),height=4))
	else resetGraph()
	expandGraph(mfrow=c(1,ifelse(Ionly,1,nfac)),mgp=c(2.5,.5,0),las=1)
	if (Uplot && Ionly)   par(mai=c(.75,.75,.1,.05), omi=c(0,0,0,0))
	fp1=Upanel; fp2=fp1+.04  ### width of first plot
	if (Uplot && !Ionly)  par(new=FALSE,fig=c(0,fp1,0,1),mai=c(.5,.5,.1,.05), omi=c(0,0,0,.05))
	if (!Uplot && Ionly)  par(mai=c(.75,0,.1,0), omi=c(0,.75,0,.05))
	if (!Uplot && !Ionly) par(mai=c(.5,0,.1,0), omi=c(0,.5,0,.05))

	for (i in 1:ifelse(Ionly,1,nfac)){
		fpwd=(1-fp2)/(nfac-1); figpos=c(fp2+(i-2)*fpwd,fp2+(i-1)*fpwd,0,1)
		if (Uplot & i>=2)  par(new=TRUE,fig=figpos,mai=c(.5,0,.1,0))
		ii    <- facs[i]
		nplot <- nplot + 1
		z     <- is.element(substring(names(coeffs),1,nchar(ii)),ii)
		rawcf <- coeffs[z]
		icont <- contr.sum(length(rawcf)+1)
		icoef <- icont %*% rawcf
		inam  <- dimnames(icoef)[[1]]
		icoef <- as.vector(icoef)

		iserr <- stderr[z]
		icorr <- correl[z,z]
		errZ  <- sqrt(iserr %*% icorr %*% iserr)
		iserr <- c(iserr,errZ)

		x  <- as.numeric(inam)
#if(ii=="gear") {browser();return()}
		if (ii=="year") {inam=yrs[x]; x=as.numeric(inam)}
		else if (ii=="month")    inam=c(month.abb,month.abb)[mos+(mo1-1)]
		else if (ii=="depth")    { dnam=rev(rev(round(dbrks))[-1]); inam=paste(dnam[x],"+",sep="") }
		else if (ii=="gear")     { gcode=c("Unknown","Bottom trawl","Midwater trawl"); names(gcode)=c(0,1,3);  inam=gcode[as.character(gear)] }
		else if (ii=="bycatch")  inam=quants[x]
		else if (ii%in%AREAS)    inam=area[x]
		else                     inam=inam[x]
		#else eval(parse(text=paste("inam=",ii,"[x]",sep="")))
		xl <- range(x)+c(-.25,.25)

		y95lo <- icoef + iserr * qnorm(0.025)  ### lower limit 
		y95hi <- icoef + iserr * qnorm(0.975)  ### upper limit
#if (ii=="depth") {browser();return()}


		if(ii=="year"){
			xfac <- as.numeric(inam)
			yrlm <- lm (icoef~xfac)
			beta <- yrlm$coeff[2]
			brR  = c(b = beta, r = 2^beta - 1., R = 2^(beta*(length(xfac)-1)) - 1. )
			packList(c("yrlm","brR"),"PBStool",tenv=.PBStoolEnv)
#browser();return()
			xreg <- seq(xfac[1],xfac[length(xfac)],length=100)
			yreg <- predict.lm(yrlm,newdata=data.frame(xfac=xreg))
		}

		if (Uplot) {
			if (ii=="year") Uadd=Ubase else Uadd=0
			icoef <- 2 ^ (icoef + Uadd)
			y95lo <- 2 ^ (y95lo + Uadd)
			y95hi <- 2 ^ (y95hi + Uadd)
			if (ii=="year") {
				yreg  <- 2 ^ (yreg  + Ubase)
				beta  <- 100 * (2^beta - 1) }
		}
		y <- icoef
		if (Uplot) {
			if(ii=="year") yl=c(0,ifelse(is.null(Imax),max(y95hi),Imax))
			else           yl=erange  }
		else yl=crange

		### Re-order the vessel IDs
		if (any(ii==c("vessel","pjsarea"))) {
			z     <- rev(order(y))
			x     <- 1:length(x)
			y     <- y[z]
			y95lo <- y95lo[z]
			y95hi <- y95hi[z]
			xl    <- range(x)  
			icoef=icoef[z]; iserr=iserr[z]; inam=inam[z]
		}

		### Output coefficients
#if (ii=="pjsarea") {browser();return()}
		if (ii=="pjsarea") {
			rnam = paste(inam,pjsa[as.character(inam),"name"])
			rnam = gsub("999 NA","999 LESSER CATCH AREAS COMBINED",rnam) }
		else rnam = inam
		out <- data.frame(icoef,iserr,row.names=rnam); names(out)=c(ii,"stderr")
		#eval(parse(text="PBStool[[ii]] <<- out"))
		ttget(PBStool); PBStool[[ii]] <- out; ttput(PBStool)
		#if (ii=="year" || !Uplot)
		write.csv(out,paste("cf-",ii,".csv",sep="")) ### output coefficients

		plot(x,y,type="n",xaxt="n",yaxt="n",xlab="",ylab="",ylim=yl,xlim=xl)
		if (Ionly && !is.null(vline)) abline(v=vline,lty=2,col="grey40")
		if (!Uplot) abline(h=0,col="grey")

		yrng=par()$usr[3:4]
		ybtck=pretty(yl); ybtck=ybtck[ybtck>yrng[1] & ybtck<yrng[2]]
		ystck=ybtck[1:(length(ybtck)-1)]+diff(ybtck)/2
		if(Uplot && ii=="year") 
			axis(2,at=ybtck,labels=TRUE,tck=-.02)
		else {
			if (Uplot && ii!="year") abline(h=1,col="grey")
			axis(2,at=ybtck,labels=ifelse(!Uplot&i==1 || Uplot&i<=2,TRUE,FALSE),tck=-.02) }
		axis(2,at=ystck,labels=FALSE,tck=-.01)
		axis(4,at=ybtck,labels=FALSE,tck=.02); axis(4,at=ystck,labels=FALSE,tck=.01)

		npts = length(x)
		small = npts>15 & ii!="year"
		zsd1  = rep(iserr>=1,each=3); zsd2 = rep(iserr>=2,each=3)
		ybars=as.vector(rbind(y95lo,y95hi,rep(NA,length(y)))); xbars=rep(x,each=3)
		lines(xbars,ybars,col="black")
		if (any(zsd1)) lines(xbars[zsd1],ybars[zsd1],col="darkgrey")
		if (any(zsd2)) lines(xbars[zsd2],ybars[zsd2],col="gainsboro")
		if (!small) lines(x,y,col="blue",lwd=2)
		points(x,y,pch=21,bg="gold",cex=ifelse(small,1,1.8))

		if(ii=="year"){
			up=beta>0
			lines(xreg,yreg,col=ifelse(up,"green","red"),lwd=2)
			rexp=paste("expression(italic(r)==",show0(signif(beta,2),1,add2int=TRUE),"~symbol(\"\\045\")~y^-1)",sep="")
			bexp=paste("expression(italic(b)==",round(beta,4),")",sep="")
			addLabel(.95,ifelse(up,.10,.90),eval(parse(text=ifelse(Uplot,rexp,bexp))),
				cex=1.2,adj=1,col=ifelse(up,"darkgreen","darkred"))
			axis(1,at=intersect(1900:2100,pretty(x,15)),cex=.6)
		}
		else if (npts>15) {
			#xshow = pretty(x,15); xshow = intersect(xshow,x)
			xshow = round(seq(1,npts,length=15))
			axis(1,at=xshow,labels=inam[xshow],cex=0.6) }
		else
			axis(1,at=x,labels=inam,cex=.6)

		mtext(ii,side=1,line=2,cex=ifelse(Ionly,1.2,1))
		if (Uplot & i==1)
			mtext("CPUE (kg/h)",side=2,line=2.5,cex=ifelse(Ionly,1.2,1),las=0)
		if(Uplot & i==2)
			mtext("Effect on CPUE",side=2,line=2.25,cex=1,las=0)
		if (i==1 && afld=="ltxa")
			addLabel(.94,.94,c("WCVI","Tidemarks","Rennell")[za],cex=.8,adj=1)
		if(!Ionly) addLabel(.08,.95,LETTERS[nplot],cex=1.5,adj=0)
	}
	if(!Uplot) mtext("Parameter Coefficients",outer=TRUE,side=2,line=2.5,cex=1.2,las=0)
	box()
	if (wmf) dev.off()
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~glimmer

#makeCATtables--------------------------2010-06-02
# Make CSV files containing catch from commercial fisheries
#-----------------------------------------------RH
makeCATtables <- function(strSpp, comm=1:7, path=.getSpath(), pout=FALSE) {
	fn <- paste("cattab-",strSpp,".csv",sep="")
	cat(paste("Catch Tables - ",strSpp,"\n",sep="",collapse=""),file=fn)
	oldOpt <- options("warn","windowsBuffered")
	options(warn=-1,windowsBuffered=FALSE)
	for (i in comm) {
		if (!is.element(i,1:7)) next
		head <- switch( i,"GFC","PHT","PHHL-Zn-vrec","PHHL-Zn-flog","PHHL-s2-vrec","PHHL-s2-flog","PHHL-halibut")
		cat(paste("Processing '",head,"'\n",sep="")); flush.console()
		switch( i,
			getData("gfc_catch_fyear.sql","GFCatch",strSpp=strSpp,path=path,tenv=penv()),
			getData("pht_catch_fyear.sql","PacHarvest",strSpp=strSpp,path=path,tenv=penv()),
			getData("phhl_vcatch_fyear.sql","PacHarvHL",strSpp=strSpp,path=path,tenv=penv(),fisheryid=5),
			getData("phhl_fcatch_fyear.sql","PacHarvHL",strSpp=strSpp,path=path,tenv=penv(),fisheryid=5),
			getData("phhl_vcatch_fyear.sql","PacHarvHL",strSpp=strSpp,path=path,tenv=penv(),fisheryid=4),
			getData("phhl_fcatch_fyear.sql","PacHarvHL",strSpp=strSpp,path=path,tenv=penv(),fisheryid=4),
			getData("phhl_hcatch_fyear.sql","PacHarvHL",strSpp=strSpp,path=path,tenv=penv()) )
		if (pout) { print(PBSdat);cat("\n") }; flush.console()
		cat(paste("\n",head,"\n",sep="",collapse=""),file=fn,append=TRUE)
		if (nrow(PBSdat)==0) {
			cat("NO DATA\n",file=fn,append=TRUE); next }
		temp <- PBSdat
		temp <- temp[apply(temp,1,function(x){!all(is.na(x[-1]))}),]
		if (nrow(temp)==0) {
			cat("NO DATA\n",file=fn,append=TRUE); next }
		temp$fyear[is.element(temp$fyear,19967)] <- 97
		temp$fyear[is.element(temp$fyear,9999)]  <- "UNK"
		write.table(temp,file=fn,sep=",", append=TRUE,row.names=FALSE,col.names=TRUE)
		} 
	cat(paste("Results in '",fn,"'\n",sep=""))
	options(oldOpt) }
#------------------------------------makeCATtables


#plotCatch------------------------------2013-01-28
# Plot catch history as annual barplot using specified catch fields
#-----------------------------------------------RH
plotCatch=function(dat="dbr.rem", flds=c("CAtrawl","UStrawl","TotalHL"),
   yrlim=NULL, wmf=FALSE, ioenv=.GlobalEnv, ...){
	assign("PBStool",list(module="M03_Fishery",call=match.call(),args=args(plotCatch),ioenv=ioenv),envir=.PBStoolEnv)
	fnam=as.character(substitute(dat))
	expr=paste("getFile(",fnam,",senv=ioenv,use.pkg=TRUE,try.all.frames=TRUE,tenv=penv()); dat=",fnam,sep="")
	eval(parse(text=expr))
	#load("dbr.rem.rda");dat=dbr.rem # for checking a more recent file
	if (!is.null(yrlim)) dat=dat[dat$Year>=yrlim[1] & dat$Year<=yrlim[2],]
	yrs=dat$Year; nyrs=length(yrs); xlim=c(0,nyrs); xlim=xlim+diff(xlim)*c(0,.07)
	spp=attributes(dat)$spp
	ylim=c(0,ifelse(spp=="440",2500,ifelse(spp=="410",165,1000)))
	height=t(dat[,flds]); height[is.na(height)]=0
	height = matrix(height,nrow=length(flds),byrow=FALSE,dimnames=list(flds,yrs))
	tcat=apply(height,2,sum,na.rm=TRUE)
	x0=1940; brks=seq(x0,2010,10); #x00=match(x0,yrs)
	decs=as.character(cut(yrs,brks,labels=paste(x0+10*0:(length(brks)-2),"s",sep="")))
	dcat=sapply(split(tcat,decs),mean)
	x1=brks[1:(length(brks)-1)]+1; d1=match(x1,yrs)-1; d2=d1+10 # histogram bars hit x-axis with their right side
	xd=as.vector(rbind(d1,d2,NA));
	xp=as.vector(rbind(d1,d1,d2,d2,NA)); 
	nch=max(nchar(round(dcat))); bak=ifelse(wmf,1.2,1.5)*nch; buf=(10-bak)*c(.5,.5)
	xb=as.vector(rbind(d1+buf[1],d1+buf[1],d2-buf[2],d2-buf[2],NA))
	yinc=diff(ylim)*ifelse(wmf,.015,.01)
	yd=as.vector(rbind(dcat,dcat,NA)); 
	yp=as.vector(rbind(dcat+yinc,dcat-yinc,dcat-yinc,dcat+yinc,NA))
	clrs=c("lightsalmon","lightskyblue1","darkorchid4")
	leg=c("CA Trawl","US Trawl","Zn H&L","Sched II","Halibut","Total H&L","Trawl + H&L")
	names(leg)=c("CAtrawl","UStrawl","ZnHL","ShedII","Halibut","TotalHL","Total")

	if (wmf && .Platform$OS.type=="windows")
		do.call("win.metafile",list(filename=paste(spp,"-Removals-",paste(flds,collapse="-"),".wmf",sep=""),width=8,height=6))
	else resetGraph()
	expandGraph(mfrow=c(1,1),mar=c(3,4,.5,.5),oma=c(0,0,0,0),las=1)

	barplot(height,space=0,col="white",border="white",xaxt="n",xaxs="i",ylim=ylim,xlim=xlim)
	if (any(spp=="410")) {
		abline(h=seq(25,150,25),col="grey")
		xleg="topright"; xjust=1 }
	else if (any(spp=="440")) {
		abline(h=seq(250,2250,250),col="grey")
		xleg="topleft"; xjust=0 }
	else {
		abline(h=pretty(ylim),col="grey")
		xleg="topleft"; xjust=0 }
	evalCall(legend,argu=list(x=xleg,fill=clrs,legend=leg[flds],bty="n",
		horiz=TRUE,xjust=xjust),...,checkpar=TRUE)
	evalCall(barplot,argu=list(height=height,space=0,col=clrs,border="grey20",
		xaxt="n",xaxs="i",ylim=ylim,xlim=xlim,add=TRUE),...,checkdef=TRUE,checkpar=TRUE)
	polygon(xb,yp,border="black",col="whitesmoke")
	polygon(xp,yp,border="black",col=0)#"lightgoldenrodyellow")
	text(d1+5,dcat,round(dcat),cex=.8,adj=c(0.5,0.4))
	xpos=seq(1,nyrs+5,5)
	axis(1,at=xpos-.5,labels=yrs[xpos],las=1,cex.axis=.9,mgp=c(0,.3,0),tcl=-0.3)
	mtext("Management Fishing Year",side=1,line=1.75,cex=1.1)
	mtext("Catch (t)",side=2,line=3,cex=1.2,las=0)
	if (wmf) dev.off()
	stuff=c("height","tcat","dcat")
	packList(stuff,"PBStool",tenv=.PBStoolEnv)
	invisible() }
#----------------------------------------plotCatch


#plotFOScatch---------------------------2010-06-02
# Plot catch as monthly barplots from 'fos_catch.sql' query to GFFOS
#-----------------------------------------------RH
plotFOScatch <- function(strSpp="453", majors=c(1,3:9), space=0.5,
   fplot="Groundfish Trawl", xlim=c("2007-04","2011-06"),
   fyrM1=4, uid=Sys.info()["user"], pwd=uid, wmf=FALSE, ...) {

	fyear=function(yrmo,startM=4) { # deprecated (see 'convFY')
		# local function to get fishing years
		yr=as.numeric(substring(yrmo,1,4)); mo=as.numeric(substring(yrmo,6,7))
		fyr=yr; sM=is.element(mo,startM:12); fyr[!sM]=fyr[!sM]-1; names(fyr)=yrmo
		return(fyr) }
	xvec=function(xlim) { # deprecated (see 'convYM')
		yr=as.numeric(substring(xlim,1,4)); mo=as.numeric(substring(xlim,6,7))
		yrs=range(yr); yrs=yrs[1]:yrs[2]; nyrs=length(yrs)
		X=paste(rep(yrs,each=12),rep(pad0(1:12,2),nyrs),sep="-")
		i=match(xlim,X)
		xout=X[i[1]:i[2]]
		return(xout) }

	assign("PBStool",list(module="M03_Fishery",call=match.call(),args=args(plotFOScatch),plotname="Rplot"),envir=.PBStoolEnv)
	if (!exists("PBSdat",where=1) || is.null(attributes(PBSdat)$spp) || 
		strSpp!=attributes(PBSdat)$spp || attributes(PBSdat)$fqt!="fos_catch.sql") {
		getData("fos_catch.sql",dbName="GFFOS",strSpp=strSpp,server="GFSH",type="ORA",
			path=.getSpath(),trusted=FALSE,uid=uid,pwd=pwd,tenv=penv())
		if (nrow(PBSdat)==0) {
			rm(PBSdat,envir=genv())
			showError("No data. Choose another species code.") }
		tmpdat=PBSdat
		names(tmpdat) = tolower(names(tmpdat)) 
		tmpdat$srfa = calcSRFA(tmpdat[,c("major","minor","locality")])
		assign("PBSdat",tmpdat,envir=genv()) }
	
	dat=PBSdat
	dat=dat[dat$year>1900 & !is.na(dat$year) & !is.na(dat$month) & !is.na(dat$major),]
	if (!is.null(majors)) dat=dat[is.element(dat$major,majors),]
	majors=sort(unique(dat$major)); nmajors=length(majors)
	dat$catch=dat$catch/1000 # convert from kg to t
	fid=sort(unique(dat$fid)); nfid=length(fid) # number of fisheries
	fishery=c("Groundfish Trawl","Halibut Longline","Sablefish Trap",
	"Dogfish & Lingcod","Rockfish H&L");  names(fishery)=1:5
	fishery=fishery[as.character(fid)]
	dat$x=paste(dat$year,pad0(dat$month,2),sep="-"); 
	x=sort(unique(dat$x)); nx=length(x)
	X=convYM(paste(range(x,xlim),"-15",sep="")); nX=length(X)
	momat=array(0,dim=c(nmajors,nX,nfid),dimnames=list(majors,X,fid))
	names(dimnames(momat))=c("major","yrmo","fid")

	for (i in fid) {
		ii=as.character(i); iii=fishery[ii] # fishery
		idat=dat[is.element(dat$fid,i),]
		if(nrow(idat)==0) next
		for (j in majors) {
			jj=as.character(j)
			jdat=idat[is.element(idat$major,j),]
			if(nrow(jdat)==0) next
			ijcat=sapply(split(jdat$catch,jdat$x),sum,na.rm=TRUE)
			momat[jj,names(ijcat),ii] = ijcat
		}
	}
	stuff=c("x","X","fishery","momat")
	packList(stuff,"PBStool",tenv=.PBStoolEnv)

	data(pmfc,spn,envir=penv())
	if (is.null(fplot)) fplot=fishery
	else fplot = findPat(fplot,fishery)
	nplot=length(fplot)
	
	if (nplot==0) showError(paste(c("Choose patterns from:\n",paste("    ",fishery)),collapse="\n"),as.is=TRUE,x=.1,y=.8,adj=0)
	if(is.null(xlim)) xplot=X else xplot=convYM(paste(xlim,"-15",sep=""))
	mplot=as.character(majors)
	pmat=momat[mplot,xplot,names(fplot),drop=FALSE]
	sums=apply(pmat,2:3,function(x){sum(x)})
	if (is.null(xlim)) {
		tsum=apply(sums,1,sum)        # total sum across fishery
		z=names(clipVector(tsum,0)) } # get only positive catch history
	else z=xplot

	fyrs=convFY(paste(z,"-15",sep=""),startM=fyrM1); fyr=sort(unique(fyrs))
	yrmat=array(0,dim=c(nmajors,length(fyr),nfid),dimnames=list(majors,fyr,fid))
	names(dimnames(yrmat))=c("major","yr","fid")
	for (i in fyr) {
		ii=names(fyrs[is.element(fyrs,i)]); iii=as.character(i)
		yrcat = apply(momat[,ii,,drop=FALSE],c(1,3), sum)
		for (j in fid) {
			jj=as.character(j); jjj=fishery[jj] # fishery
			yrmat[,iii,jj] = yrcat[,jj]
		}
	}
	fyrM12=fyrM1-1; if(fyrM12==0) fyrM12=12
	cnam=paste(strSpp,"-catFOSfyr.csv",sep="")
	cat(paste("Catch (t) by fishing year:  ",month.abb[fyrM1]," to ",month.abb[fyrM12],"\n\n",sep=""),file=cnam)
	for (i in dimnames(yrmat)[[3]]) {
		imat = yrmat[,,i,drop=FALSE]
		icat=t(apply(imat,c(1,2),sum))
		dimnames(icat)[[2]]=pmfc[dimnames(icat)[[2]],"gmu"]
		icat=icat[,order(dimnames(icat)[[2]]),drop=FALSE]
		cat(paste(fishery[i],"\n",sep=""),file=cnam,append=TRUE)
		warn=options()$warn; options(warn = -1)
		write.csv(icat,file=cnam,append=TRUE)
		options(warn=warn)
	}
	yrcat=apply(yrmat,c(2,3),sum)
	if (is.vector(yrcat)) yrcat=matrix(yrcat,nrow=1,dimnames=list(fyr,names(yrcat)))
	yrcat=as.data.frame(cbind(yrcat,Total=apply(yrcat,1,sum,na.rm=TRUE)))

	ylim=c(0,1.05*max(sums))
	clrs=paste("grey",round(seq(75,90,len=nmajors)),sep=""); names(clrs)=majors
	clrs[as.character(c(1,3:9))]=c("orchid1","blue","lightblue","yellow","orange","red","seagreen","lightgreen")
	sqn=sqrt(nplot)
	m=ceiling(sqn); n=ceiling(nplot/m)

	plotname=paste(strSpp,"-FOScatch.wmf",sep="")
	stuff=c("fplot","xplot","fyrs","fyrM1","fyrM12","sums","yrmat","yrcat","plotname")
	packList(stuff,"PBStool",tenv=.PBStoolEnv)
	print(yrcat)

	if (wmf && .Platform$OS.type=="windows")
		do.call("win.metafile",list(filename=plotname,height=8,width=8))
	else resetGraph()
	expandGraph(mfrow=c(m,n),mar=c(4,4,.5,.5),oma=c(0,0,1.5,0))
	for (i in fplot) {
		ii=names(fishery)[fishery%in%i]
		evalCall(barplot,argu=list(height=pmat[,z,ii],ylim=ylim,col=0,space=space,las=2,
			ylab="",cex.names=0.9),..., checkdef=TRUE, checkpar=TRUE)
		endpos=match(findPat(paste("-",pad0(fyrM12,2),sep=""),z),z)
		endyr=as.numeric(substring(z[endpos],1,4))-ifelse(fyrM12==12,0,1)
		if (length(fyr)>1) abline(v=(1+space)*endpos+(space/2),col="grey")
		evalCall(barplot,argu=list(height=pmat[,z,ii],ylim=ylim,col=clrs[mplot],space=space,las=2,
			cex.names=.9,add=TRUE),..., checkdef=TRUE, checkpar=TRUE)
		if (i==fplot[1]) {
			xleg=(1+space)*match(min(sums[,1]),sums[,1])[1]-space
			legend(xleg,ylim[2]*.95,fill=rev(clrs[mplot]),
				legend=rev(pmfc[mplot,]$gmu),bty="n",cex=.8,xjust=0.1) }
		text((1+space)*endpos,ylim[2],paste(endyr,"=",round(yrcat[as.character(endyr),ii]),"t"),
			col="red",cex=.9,adj=c(1.2,1.5))
		mtext(paste("Catch (t) in",i,"fishery"),side=2,line=2.5,cex=1.25/sqrt(m))
	}
	mtext(spn[strSpp],outer=TRUE,side=3,line=0.25,col="midnightblue",cex=1,adj=0.05)
	if(wmf) dev.off()
	invisible(dat) }
#-------------------------------------plotFOScatch

#runCCA---------------------------------2015-03-06
# Catch-curve model based on Schnute, J.T., and Haigh, R. 2006.
# Compositional analysis of catch curve data with an application to Sebastes maliger.
# ICES Journal of Marine Science.
# Code allows user to perform frequentist (NLM) and Bayesian (BRugs) analyses.
#-----------------------------------------------RH
runCCA = function(fnam="nage394", hnam=NULL, ioenv=.GlobalEnv, ...)
{
	assign("PBStool",list(module="M03_Fishery",call=match.call(),args=args(runCCA),ioenv=ioenv,plotname="CCAplot",dots=list(...)),envir=.PBStoolEnv)
	fnam=as.character(substitute(fnam))
	if (!is.character(fnam)) stop("Argument 'fnam' must be a string name of an R dataset")
	if (!requireNamespace("BRugs",        quietly=TRUE)) stop("`BRugs` package is required")

	path <- .getWpath() # win path
	rtmp <- tempdir(); rtmp <- gsub("\\\\","/",rtmp)
	wnam <- paste(path,"runCCAWin.txt",sep="/")
	wtmp <- paste(rtmp,"runCCAWin.txt",sep="/")
	temp <- readLines(wnam)
	temp <- gsub("@wdf",wtmp,temp)
	temp <- gsub("@fnam",fnam,temp)
	if (!is.null(hnam) && is.character(hnam))
		temp <- gsub("#import=",paste("import=\"",hnam,"\"",sep=""),temp)
	writeLines(temp,con=wtmp)
	createWin(wtmp)
	.runCCA.getData()
	invisible() }

.runCCA.model  = function(P) {
	Z=P[1]; alpha=P[2]; betak=P[3]; tau=P[4]; rho=P[5:9]
	unpackList(ttcall(PBStool)$FP,scope="L")
	a = k:B; Sa = exp(-Z*(a-k)); beta = rep(1,length(a)); Ra = rep(1,length(a))
	zk   = a < b0
	if (any(zk==TRUE))  beta[zk] = 1 - (1-betak) * ((b0-a[zk])/(b0-k))^alpha
	names(beta) = a
	zrho = match(bh,b5)
	if (length(zrho)>0) {
		rtemp = NULL
		for (i in zrho)
			rtemp = rbind( rtemp, rho[i] * exp(-.5*((a-b5[i])/tau)^2) )
		Ra = Ra + apply(rtemp,2,sum,na.rm=TRUE) }
	SBR = Sa * beta * Ra
	pa = SBR / sum(SBR); zA = a >= A; pA = sum(pa[zA]); names(pA) = A; paA  = c(pa[!zA],pA)
	if (round(sum(paA),5)!=1) stop("paA srcewy")
	return(paA) }

.runCCA.objFun = function(P) { # User's objective function
	Z=P[1]; alpha=P[2]; betak=P[3]; tau=P[4]; sigma=P[5]; n=P[6]
	rho1=P[7]; rho2=P[8]; rho3=P[9]; rho4=P[10]; rho5=P[11]; 
	unpackList(ttcall(PBStool)$FP,scope="L")
	Pcur=ttcall(PBStool)$Pcur
	a = as.numeric(names(pa.obs))
	pa = .runCCA.model(P=c(Z,alpha,betak,tau,rho1,rho2,rho3,rho4,rho5))
	if (Pcur==1) {
		out = -nspec * sum(pa.obs * log(pa))
		pi1 = pa; packList("pi1","PBStool",tenv=.PBStoolEnv) }
	if (any(Pcur==c(2,3))) {
		z = as.numeric(cut(a,acut))
		pi = sapply(split(pa,z),sum)
		if (Pcur==2) {
			g = length(pi.obs)
			out = sum( lgamma(n*pi) - n*pi*log(pi.obs) ) - lgamma(n)
			pi2=pi; packList("pi2","PBStool",tenv=.PBStoolEnv) }
		if (Pcur==3) {
			g = length(pi.obs); ytil = calcGM(pi.obs); ptil = calcGM(pi)
			out = (g-1) * log(sigma) + (1/(2*sigma^2)) * sum((log(pi.obs/ytil)-log(pi/ptil))^2)
			pi3=pi; packList("pi3", "PBStool",tenv=.PBStoolEnv) } }
	return(out) }

.runCCA.getData = function() {# Get user's data
	getWinVal(scope="L")
	ioenv = ttcall(PBStool)$ioenv
	expr=paste("getFile(",fnam,",senv=ioenv,try.all.frames=TRUE,use.pkg=TRUE,tenv=penv()); assign(\"Afile\",",fnam,")",sep="")
	eval(parse(text=expr))

	len  = 5 # number of years per line in message
	flds = dimnames(Afile)[[2]]; nflds = length(flds); fldc = paste(flds,",",sep="")
	temp = rep("",ceiling(nflds/len)*len); temp[1:nflds] = fldc
	temp = matrix(temp,nrow=len,byrow=FALSE);  temp = rbind(temp,rep("\n",ncol(temp)))
	temp = paste(as.vector(temp),collapse=""); temp = substring(temp,1,nchar(temp)-2)
	msg  = paste("Dim = ",paste(dim(Afile),collapse=" x "),",  Years:",sep="")
	msg  = paste(msg,temp,sep="\n")

	if (!any(flds==year)) showError("YEAR NOT AVAILABLE")
	afile = data.frame(age=as.numeric(dimnames(Afile)[[1]]),pa=Afile[,as.character(year)])
	attr(afile,"year") = year
	packList("afile","PBStool",tenv=.PBStoolEnv)

	ii = dimnames(afile)[[2]]
	x  = afile[,1]; y  = afile[,2]
	z  = cut(x,breaks=seq(0,ceiling(max(x)/10)*10,10),labels=FALSE); z=z-1 # decadal periods
	ydec = split(y,z); xdec = split(x,z)
	ymax = sapply(ydec,function(x){max(x)[1]})
	peak = array(0,dim=c(length(ydec),2),dimnames=list(decade=names(ydec),mode=c("x","y")))
	for (i in names(ydec)) {
		POS = grep(ymax[i],ydec[[i]])
		pos = POS[median(1:length(POS))]
		peak[i,c("x","y")] = c(xdec[[i]][pos], ydec[[i]][pos]) }
	B5 = peak[rev(order(peak[,"y"])),][1:5,]
	phi[c("m",paste("b",1:5,sep=""))] = c(5,sort(B5[,"x"]))
	updateGUI()
	.runCCA.plotData()
	packList("B5","PBStool",tenv=.PBStoolEnv)
	invisible() }
	
.runCCA.plotData = function() {# Plot user's data
	afile = ttcall(PBStool)$afile
	if (is.null(afile)) .runCCA.getData()
	getWinVal(scope="L")
	x  = afile[,1]; y  = afile[,2]
	ysum = sum(y); ymax = max(y); xmax = max(x)
	xmod = phi[paste("b",1:5,sep="")]; xmod = xmod[xmod>0]  #[1:phi["m"]]
	ymod = y[is.element(x,xmod)]

	plotname = paste("Data",fnam,year,sep="-")
	packList("plotname","PBStool",tenv=.PBStoolEnv)
	par(mfrow=c(1,1),mai=c(.6,.6,.1,.1),omi=c(0,0,0,0))
	plot(x,y,xlim=c(0,xmax),ylim=c(0,ymax),type="h",xlab="",ylab="",xaxt="n",cex=.7,las=1,adj=1,mgp=c(0,.4,0),tck=.02)
	lines(xmod,ymod,type="h",col="red",lwd=2)
	text(xmod,ymod+.02*ymax,xmod,col="red",cex=1)
	axis(1,at=seq(0,xmax,10),cex.axis=1,mgp=c(0,.5,0),tck=-.02)
	axis(1,at=1:xmax,labels=FALSE,tck=.01); axis(1,at=seq(0,xmax,5),labels=FALSE,tck=.02)
	addLabel(.95,.92,paste(fnam,"\nyear = ",attributes(afile)$year,sep="",collapse=""),cex=1.2,col="#400080",adj=1)
	addLabel(.95,.85,paste("n =",round(ysum,ifelse(ysum<1,3,0))),cex=1.2,col="blue",adj=1)
	mtext("Age",side=1,line=1.75,cex=1.5)
	mtext("Frequency",side=2,line=1.75,cex=1.5)
	invisible() }

.runCCA.setVals = function() { #Set user's settings
	getWinVal(scope="L")
	active = theta$active; names(active) = rownames(theta)
	b5s  = paste("b",1:5,sep=""); r5s = paste("rho",1:5,sep="")
	tnam = c("k","A","B","b0","eps","m")
	b5   = phi[b5s]; bh = b5[active[r5s]]
	dvec = phi[tnam]
	FP   = as.list(dvec) # file parameters
	m    = length(bh); FP[["m"]] = m; phi["m"] = m
	pnam = dimnames(theta)[[1]]
	FP   = c(FP,list(b5=b5,bh=bh,pnam=pnam))
	unpackList(FP,scope="L")

	afile = ttcall(PBStool)$afile
	if (is.null(afile)) .runCCA.getData()
	afile = afile[afile[,"age"]>=k & !is.na(afile[,"age"]) & !is.na(afile[,"pa"]),]
	pa    = rev(afile[,"pa"]); a = rev(afile[,"age"]); zz = cumsum(pa) > 0
	pa    = rev(pa[zz]); a = rev(a[zz]); names(pa) = a
	nspec = sum(pa); pa = pa/nspec
	FP    = c(FP,list(nspec=nspec,ages.raw=a,pa.raw=pa))
	amax  = max(a,na.rm=TRUE)
	FP    = c(FP,list(amax=amax))

	if (autoA) {
		zA    = rev(cumsum(rev(pa)) < eps)
		if (all(zA==FALSE)) A = amax
		else            A = a[zA][1] - 1
		if (!is.null(bh) && A<max(bh))
			stop("Age of plus class A lower than maximum stated anomaly age bh")
		FP[["A"]] = A; phi[2] = A; }; #x1 = pa[zz]; x2 = pa[length(x1)+1]

	zA = a >= A; pA = sum(pa[zA]); names(pA) = A; paA = c(pa[!zA],pA)
	pa.obs = paA; attr(pa.obs,"input") = c(k,A,B,b0,eps,bh)
	FP    = c(FP,list(pa.obs=pa.obs))

	# Grouping
	pi.obs = NULL; temp = 0
	for (i in 1:length(paA)) {
		temp = temp + paA[i]
		if (temp >= eps) {
			names(temp) = names(paA[i]); pi.obs = c(pi.obs,temp); temp = 0; } }
	g = length(pi.obs); i = 1:g; FP = c(FP,list(g=g))
	PA = array(NA,dim=c(length(paA),2),dimnames=list(names(paA),c("age","pa")))
	PA[names(paA),"age"] = as.numeric(names(paA)); PA[names(paA),"pa"]  = paA
	PI = array(NA,dim=c(g,2),dimnames=list(names(pi.obs),c("age","pi")))
	PI[names(pi.obs),"age"] = as.numeric(names(pi.obs)); PI[names(pi.obs),"pi"] = pi.obs
	acut = c(k-1,as.numeric(names(pi.obs))); acut[length(acut)] = amax
	FP = c(FP,list(acut=acut,pi.obs=pi.obs))

	idxM = c(TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE) & active
	idxD = c(TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE) & active
	idxL = c(TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE) & active
	FP   = c(FP,list(idxM=idxM,idxD=idxD,idxL=idxL))
	#pset = switch(MDL,idxM,idxD,idxL); pmon = pnam[pset]
	pset = c(TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE) & as.vector(phi>0) # Fix for Dirichlet
	pmon = pnam[pset]
	if (any(substring(pmon,1,3)=="rho")) {
		pmon = pmon[!is.element(substring(pmon,1,3),"rho")]
		pmon = c(pmon,"rho") }
	FP  = c(FP,list(pmon=pmon))
	updateGUI()
	packList("FP","PBStool",tenv=.PBStoolEnv)
	invisible() }

.runCCA.evalMod = function() { # Evaluate user's model
	FP = ttcall(PBStool)$FP
	if (is.null(FP)) .runCCA.setVals()
	getWinVal(scope="L")
	unpackList(FP,scope="L")
	suff = paste(fnam,year,sep="."); mods = (1:3)[modT]
	for (i in mods) {
		Pcur = i; # probability distribution: 1=multinomial, 2=dirichlet, 3=logistic-normal
		packList("Pcur","PBStool",tenv=.PBStoolEnv) # current probability distribution
		parvec = theta; parvec$active = switch(i,idxM,idxD,idxL)
		Fout = calcMin(parvec,.runCCA.objFun,method="nlm",steptol=1e-4,repN=25); print(Fout)
		expr=paste("assign(paste(\"Fout\",",i,",sep=\"\"),Fout); ",
			"assign(paste(paste(\"Fout\",",i,",sep=\"\"),\"",suff,"\",sep=\".\"),Fout); ",
			"packList(c(paste(\"Fout\",",i,",sep=\"\"),paste(paste(\"Fout\",",
			i,",sep=\"\"),\"",suff,"\",sep=\".\")),\"PBStool\",tenv=.PBStoolEnv)",sep="")
		eval(parse(text=expr)) # save results in PBStool
	}
	invisible() }

.runCCA.plotNLM = function() { # User's plot of NLM results
	FP = ttcall(PBStool)$FP
	if (is.null(FP)) { .runCCA.setVals(); .runCCA.evalMod() }
	getWinVal(scope="L")
	unpackList(FP,scope="L")
	resetGraph()
	if (!any(is.element(paste("Fout",(1:3)[modT],sep=""),names(ttcall(PBStool))))) {
		.runCCA.setVals(); .runCCA.evalMod() }

	# Data matrix for plotting
	mods = (1:3)[modT]; nmods = length(mods)
	suff = paste(fnam,year,sep=".")
	nliy = c(20,15)
	x = k:amax; xx = as.character(x); xa = k:A; xlim = c(k,A)+c(-1,1)
	pmat = array(NA,dim=c(length(x),10,nmods),
		dimnames=list(age=x,xy=c("x","y","ya","yi","pa","pi","cya","cyi","cpa","cpi"),mod=mods))

	for (i in mods) {
		ii = as.character(i)
		eval(parse(text=paste("Fout=ttcall(PBStool)$Fout",i,sep="")))
		pars = Fout$Pend
		y  = pa.raw; ya = pa.obs; yi = switch(i, pa.obs, pi.obs, pi.obs)
		a  = as.numeric(names(yi))
		pa   = .runCCA.model(P=pars[c("Z","alpha","betak","tau","rho1","rho2","rho3","rho4","rho5")])
		if (i!=1) {
			z  = as.numeric(cut(xa,acut));  z  = z[!is.na(z)]
			pi = sapply(split(pa,z),sum,na.rm=TRUE); names(pi) = a }
		else pi = pa
		cya = cumsum(ya); cyi  = cumsum(yi); cpa  = cumsum(pa); cpi  = cumsum(pi)

		pmat[xx,"x",ii]           = x
		pmat[names(y),  "y",  ii] = y
		pmat[names(ya), "ya", ii] = ya
		pmat[names(yi), "yi", ii] = yi
		pmat[names(pa), "pa", ii] = pa
		pmat[names(pi), "pi", ii] = pi
		pmat[names(cya),"cya",ii] = cya
		pmat[names(cyi),"cyi",ii] = cyi
		pmat[names(cpa),"cpa",ii] = cpa
		pmat[names(cpi),"cpi",ii] = cpi }
	expr=paste("assign(paste(\"pmat\",\"",suff,"\",sep=\".\"),pmat); ",
		"packList(paste(\"pmat\",\"",suff,"\",sep=\".\"),\"PBStool\",tenv=.PBStoolEnv)",sep="")
	eval(parse(text=expr))

	ymax  = max(apply(pmat,match("xy",names(dimnames(pmat))),max,na.rm=TRUE)[c("ya","yi")])
	ylim  = c(0,ymax);  xpos = seq(0,B,5);  zx = xpos>=k & xpos<=A
	ypos1 = seq(0,1,.01); zy1 = is.element(ypos1,seq(0,1,.02))
	ypos2 = seq(0,1,.05); zy2 = is.element(ypos2,seq(0,1,.1))

	# Plot the results
	plotname=paste(c("NLM.",suff,".mods",mods),collapse="")
	packList("plotname","PBStool",tenv=.PBStoolEnv)
	if (wmf && .Platform$OS.type=="windows")
		do.call("win.metafile",list(filename=paste(plotname,".wmf",sep=""),
		width=6.5,height=switch(nmods,5,8,9.5),pointsize=12))
	if (names(dev.cur())=="null device") { do.call("windows",list(width=6,height=8)); frame() }
	din = par()$din; xmarg = 1*c(0.5,0.6)
	ymarg = switch(nmods,max(1,par()$din[2]-4),2,1); ymarg = c(.6,.4)*ymarg
	ymarg = max(.8,par()$din[2]-4*nmods); ymarg = c(.7,.3)*ymarg
	par(mfrow=c(nmods,2),mai=c(0,0,0,0),omi=c(ymarg[1],xmarg[1],ymarg[2],xmarg[2]), mgp=c(0,.25,0))
	sz1 = (par()$fin[2]/nliy[1])/par()$csi; sz2 = (par()$fin[2]/nliy[2])/par()$csi

	for (i in mods) {
		ii = as.character(i); idx = switch(i,idxM,idxD,idxL)
		eval(parse(text=paste("Fout=ttcall(PBStool)$Fout",i,sep="")))
		pars = Fout$Pend[idx]; npars = length(pars)

		# PLOT 1 - Bars of pi
		za = !is.na(pmat[xx,"ya",ii]);  zi = !is.na(pmat[xx,"yi",ii])
		xa = pmat[xx,"x",ii][za];       xi = pmat[xx,"x",ii][zi]
		ya = pmat[xx,"ya",ii][za];      yi = pmat[xx,"yi",ii][zi]
		pa = pmat[xx,"pa",ii][za];      pi = pmat[xx,"pi",ii][zi]

		plot(0,0,type="n",xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
		axis(1,at=xpos,tck=.01,labels=FALSE)
		axis(1,at=xpos[zx],tck=.015,labels=(par()$mfg[1]==par()$mfg[3]),cex.axis=sz1)
		axis(2,at=ypos1,tck=.01,labels=FALSE)
		axis(2,at=ypos1[zy1],tck=.02,labels=TRUE,adj=1,cex.axis=sz1)
		abline(h=eps,col="gainsboro")
		lines(xa,pa,col=switch(i,"blue","forestgreen","red"),lwd=2)
		if (seepa) lines(xa,ya,type="h",lwd=1,col="darkorange")
		if (seepi) lines(xi+.25,yi,type="h",lwd=1,lty=3,col="orchid4")
		zbi = is.element(xa,bh)
		lines(xa[zbi],ya[zbi],type="h",lwd=2,col="black")#"#FF8000")
		mtext(paste(c("pa","pi")[c(seepa,seepi)],collapse="/"),side=2,cex=sz2,line=1.5*nmods^.2)
		addLabel(.95,.95,switch(i,"Multinomial","Dirichlet","Logistic-normal"),cex=sz2,adj=1,
			col=switch(i,"blue","forestgreen","red"))

		# PLOT 2 - Cumulative pi
		cyi = pmat[xx,"cyi",ii][zi];  cpi = pmat[xx,"cpi",ii][zi]
		cya = pmat[xx,"cya",ii][za];  cpa = pmat[xx,"cpa",ii][za]

		plot (xi,cpi,type="n",xlim=xlim,ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="")
		axis(1,at=xpos,tck=.01,labels=FALSE)
		axis(1,at=xpos[zx],tck=.015,labels=(par()$mfg[1]==par()$mfg[3]),cex.axis=sz1)
		axis(4,at=ypos2,tck=.01,labels=FALSE)
		axis(4,at=ypos2[zy2],tck=.02,labels=TRUE,adj=0,cex.axis=sz1)
		if (seepa) {
			lines(xa,cya,lwd=3,col="darkorange")
			lines(xa,cpa,lwd=2,col=switch(i,"blue","forestgreen","red")) }
		if (!seepa & seepi) {
			lines(xi,cyi,lwd=3,col="orchid4")
			lines(xi,cpi,lwd=2,col=switch(i,"blue","forestgreen","red")) }
		mtext(paste("Cumulative",c("pa","pi")[c(seepa,!seepa&seepi)]),side=4,line=1.5*nmods^.4,cex=sz2)

		Fmin = Fout$fminE; AIC = Fout$AIC; AICc = 2*Fmin + 2*sum(idx)*(g-1)/(g-sum(idx)-2)
		pval = as.character(signif(pars,3)); plab = names(pars)
		fval = as.character(signif(c(Fmin,AIC,AICc),3)); flab = c("fmin","AIC","AICc")
		y0p  = (1/nliy[1]) * (npars+4); y0f  = (1/nliy[1]) * (4)
		addLabel(.65,y0p,paste(plab,collapse="\n"),adj=c(1,1),col="blue",cex=sz1)
		addLabel(.70,y0p,paste(pval,collapse="\n"),adj=c(0,1),col="blue",cex=sz1)
		addLabel(.65,y0f,paste(flab,collapse="\n"),adj=c(1,1),col="red",cex=sz1)
		addLabel(.70,y0f,paste(fval,collapse="\n"),adj=c(0,1),col="red",cex=sz1) }
	mtext("Age",outer=TRUE,side=1,line=2,cex=sz2)
	if(wmf) dev.off() }

#-----WinBUGS-Model-------------------------------------------------------------

.runCCA.checkMon = function() { # Check user's monitor choices
	FP = ttcall(PBStool)$FP
	if (is.null(FP)) .runCCA.setVals()
	getWinVal(scope="L")
	unpackList(FP,scope="L")
	#Pset = c(TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE) #& as.vector(phi>0) # Fixed for Dirichlet
	Cset <- switch(case,
		c(TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE),
		c(TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE),
		c(TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE),
		c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE) )
	Pset <- switch(MDL,idxM,idxD,idxL) & Cset
	FP$Pset = Pset
	z = Pset-pset
	if(any(z==-1)) {
		showAlert(paste("Cannot monitor (",paste(pnam[is.element(z,-1)],collapse=","),")",sep=""))
		return(FALSE) }
	else {
		Pmon = pmon = pnam[pset]
		if (any(substring(pmon,1,3)=="rho")) {
			pmon = pmon[!is.element(substring(pmon,1,3),"rho")]
			pmon = c(pmon,"rho") }
		FP$Pmon = Pmon; FP$pmon = pmon
		packList("FP","PBStool",tenv=.PBStoolEnv)
		return(TRUE) }
	}

.runCCA.compileMod = function() { # Initialize and compile the WinBUGS model
	isOK = .runCCA.checkMon(); if (!isOK) stop("Reset monitors")
	getWinVal(scope="L")
	unpackList(ttcall(PBStool)$FP,scope="L")
	Bhist=NULL
	packList("Bhist","PBStool",tenv=.PBStoolEnv)
	suff = paste(fnam,year,sep=".")
	ai   = acut[2:length(acut)]; ai[length(ai)] = A; y = pi.obs
	dat  = switch(case,
				list(k=k,B=B,g=g,ai=ai,y=y),
				list(k=k,B=B,b0=b0,g=g,ai=ai,y=y),
				list(k=k,B=B,m=m,b=bh,g=g,ai=ai,y=y),
				list(k=k,B=B,b0=b0,m=m,b=bh,g=g,ai=ai,y=y) )
	BRugs::bugsData(dat,"CCAdat.txt")
	mnam = paste(.getEpath(),"/CCAmod-",c("M","D","L")[MDL],case,".txt",sep="") # model name in examples directory
	file.copy(mnam,"CCAmod.txt",overwrite=TRUE)
	BRugs::modelCheck("CCAmod.txt")   # check model syntax
	BRugs::modelData("CCAdat.txt")    # load current data
	BRugs::modelCompile(nc)           # compile with nc chains
	BRugs::modelGenInits()            # generate randoms inits
	BRugs::samplesSet(pmon)           # parameters to monitor
	setWinVal(list(clen=100,cthin=1,ctot=0,s1=1,s2=100,sthin=1,chn2=nc))
	par(ask=FALSE)
	invisible() }

.runCCA.updateMod = function() { # Update the model and save complete history in "CCAhist" (in "PBStool")
	isOK = .runCCA.checkMon(); if (!isOK) stop("Reset monitors")
	getWinVal(scope="L")
	Pmon=ttcall(PBStool)$FP$Pmon
	Batts=attributes(ttcall(PBStool)$Bhist)
	if ( !is.null(Batts$nc) && ( Batts$nc!=nc || 
	!all(is.element(Batts$Pmon,Pmon)) || !all(is.element(Pmon,Batts$Pmon)) ) )
		.runCCA.compileMod()
	BRugs::modelUpdate(clen,cthin)
	Shist = BRugs::samplesHistory("*",beg=0,plot=FALSE)
	names(Shist) = gsub("[]\\[]","",names(Shist))
	Bhist = as.data.frame(Shist)
	if (nc==1) names(Bhist) = paste(names(Bhist),1,sep=".")
	attr(Bhist,"nc") = nc
	attr(Bhist,"Pmon") = Pmon
	#nams  = names(Bhist); nams = sub("rho\\.","rho",nams)
	#if (nc==1) nams = paste(nams,1,sep=".")
	#nams = sub("\\.\\.",".",nams); names(Bhist) = nams
	CCAhist = cbind(X=1:nrow(Bhist),Bhist)
	ctot = dim(CCAhist)[1]    # total length so far
	setWinVal(list(ctot=ctot,s1=ctot-clen+1,s2=ctot))
	par(ask=FALSE)
	packList(c("Bhist","CCAhist"), "PBStool",tenv=.PBStoolEnv)
	invisible() }

.runCCA.plotTrace = function(file=ttcall(PBStool)$CCAhist,clrs=c("blue","red","green","magenta","navy")) {
	isOK = .runCCA.checkMon(); if (!isOK) stop("Reset monitors")
	getWinVal(scope="L")
	unpackList(ttcall(PBStool)$FP,scope="L")
	resetGraph()
	i1   = max(s1,1); i2 = min(s2,ctot)  # ensure valid range
	idx  = seq(i1,i2,by=sthin);  file = file[idx,];  nams = names(file)
	pmon = pnam[pset]; nr = length(pmon); chains = chn1:chn2
	plotname=paste("Trace(",paste(pmon,collapse="."),")-",fnam,"-",year,sep="")
	packList("plotname","PBStool",tenv=.PBStoolEnv)
	par(mfrow=c(nr,1), mar=c(1,3,0.5,0.5), oma=c(1,0,0,0), mgp=c(0,.25,0), ask=FALSE)
	for (i in pmon) {
		ii  = paste(i,chains,sep="."); iii = (1:length(nams))[is.element(nams,ii)]
		dot = regexpr("\\.",ii); j = as.numeric(substring(ii,dot+1))
		plotTrace(file[,c(1,iii)],clrs=clrs[j])
		addLabel(.97,.95,i,cex=1.2,col="#400080",adj=c(1,1),font=2) } 
	invisible() }

.runCCA.plotDens = function(file=ttcall(PBStool)$CCAhist,clrs=c("blue","red","green","magenta","navy")) {
	isOK = .runCCA.checkMon(); if (!isOK) stop("Reset monitors")
	getWinVal(scope="L")
	unpackList(ttcall(PBStool)$FP,scope="L")
	resetGraph()
	i1   = max(s1,1); i2 = min(s2,ctot)  # ensure valid range
	idx  = seq(i1,i2,by=sthin);  file = file[idx,];  nams = names(file)
	pmon = pnam[pset]; nP = length(pmon); chains = chn1:chn2
	plotname=paste("Dens(",paste(pmon,collapse="."),")-",fnam,"-",year,sep="")
	packList("plotname","PBStool",tenv=.PBStoolEnv)
	par(mfrow=.runCCA.getLayout(nP), mar=c(1,3.5,0.5,0.5), oma=c(1,0,0,0), mgp=c(0,.25,0), ask=FALSE)
	for (i in pmon) {
		ii  = paste(i,chains,sep="."); iii = (1:length(nams))[is.element(nams,ii)]
		dot = regexpr("\\.",ii); j = as.numeric(substring(ii,dot+1))
		plotDens(file[,c(iii)],clrs=clrs[j])
		addLabel(.97,.95,i,cex=1.2,col="#400080",adj=c(1,1),font=2) }
	invisible() }

.runCCA.plotACF = function(file=ttcall(PBStool)$CCAhist,clrs=c("blue","red","green","magenta","navy")) {
	isOK = .runCCA.checkMon(); if (!isOK) stop("Reset monitors")
	getWinVal(scope="L")
	unpackList(ttcall(PBStool)$FP,scope="L")
	resetGraph()
	i1   = max(s1,1); i2 = min(s2,ctot)  # ensure valid range
	idx  = seq(i1,i2,by=sthin);  file = file[idx,];  nams = names(file)
	pmon = pnam[pset]; nP = length(pmon); chains = chn1:chn2
	plotname=paste("ACF(",paste(pmon,collapse="."),")-",fnam,"-",year,sep="")
	packList("plotname","PBStool",tenv=.PBStoolEnv)
	par(mfrow=.runCCA.getLayout(nP), mar=c(1,3,0.5,0.5), oma=c(1,0,0,0), mgp=c(0,.25,0), ask=FALSE)
	for (i in pmon) {
		ii  = paste(i,chains,sep="."); iii = (1:length(nams))[is.element(nams,ii)]
		dot = regexpr("\\.",ii); j = as.numeric(substring(ii,dot+1))
		plotACF(file[,c(iii)],clrs=clrs[j],lwd=2)
		addLabel(.97,.95,i,cex=1.2,col="#400080",adj=c(1,1),font=2) }
	invisible() }

.runCCA.diag.panel = function(x, ...) { # Histograms for diagonal of pairs plor
	usr = par("usr"); on.exit(par(usr))
	h = hist(x, breaks="Sturges", plot=FALSE)
	breaks = h$breaks; nB = length(breaks)
	y = h$counts; y = y/sum(y)
	par(usr = c(usr[1:2], 0, max(y)*1.5) )
	rect(breaks[-nB], 0, breaks[-1], y, col="#FFD18F") 
	box(); invisible() }

.runCCA.plotPairs = function() { # Pairs plot
	isOK = .runCCA.checkMon(); if (!isOK) stop("Reset monitors")
	getWinVal(scope="L")
	unpackList(ttcall(PBStool)$FP,scope="L")
	resetGraph()
	i1   = max(s1,1); i2 = min(s2,ctot) # ensure valid range
	idx  = seq(i1,i2,by=sthin)
	file = ttcall(PBStool)$CCAhist[idx,]
	nams = names(file); dot = regexpr("\\.",nams)
	chains = chn1:chn2; nchn = length(chains)
	pmon = pnam[pset]
	z1   = is.element(substring(nams,dot+1),chains)
	z2   = is.element(substring(nams,1,dot-1),pmon)
	file = file[,z1&z2]
	PMON = paste(rep(pmon,each=nchn),rep(chains,length(pmon)),sep="."); file = file[,PMON]
	eval(parse(text=paste("Fout=ttcall(PBStool)$Fout",MDL,sep="")))
	if (!is.null(Fout)) {
		modes = Fout$Pend[pset]
		print("MODES:"); print(paste(names(modes),signif(modes,3),sep="="))
		modes = rep(modes,each=nchn) }
	else  modes = NULL

	plotname=paste("Pairs(",paste(pmon,collapse="."),")-",fnam,"-",year,sep="")
	packList("plotname","PBStool",tenv=.PBStoolEnv)
	par(ask=FALSE,mgp=c(0,.75,0))
	pairs(file, diag.panel=.runCCA.diag.panel, gap=0, cex.labels=1.2,
		panel=function(x,y,z=modes) {
			n   = length(x); nn = n-1
			xmn = mean(x,na.rm=TRUE); ymn = mean(y,na.rm=TRUE)
			points(x,y,pch=16,cex=0.6,col="darkgray")
			abline(h=ymn,v=xmn,col="blue",lty=3)
			points(xmn,ymn,col="cyan",pch=15,cex=1.2)
			points(xmn,ymn,col="blue",pch=0,cex=1.2)
			if (!is.null(modes)) {
				xmd = z[par()$mfg[2]]; ymd = z[par()$mfg[1]]
				points(xmd,ymd,col="red",pch=17,cex=1.2)
				points(xmd,ymd,col="black",pch=2,cex=1.2) } } )
	invisible() }

.runCCA.getChains = function(chains=BRugs::samplesGetFirstChain():BRugs::samplesGetLastChain()) { # amalgamates chains
	if (is.null(ttcall(PBStool)$CCAhist)) return(FALSE)
	getWinVal(scope="L")
	unpackList(ttcall(PBStool)$FP,scope="L")
	i1  = max(s1,1); i2 = min(s2,ctot) # ensure valid range
	idx = seq(i1,i2,by=sthin)
	Pfld = pnam[pset]; nP = length(Pfld); nch  = length(chains)
	pfld = paste(rep(Pfld,each=nch),chains,sep=".")
	file = ttcall(PBStool)$CCAhist[idx,pfld]
	temp = NULL
	for (i in chains) {
		junk = file[,paste(Pfld,i,sep="."),drop=FALSE];  names(junk) = Pfld
		temp = rbind(temp,junk) }
	CCAsub = temp
	packList("CCAsub","PBStool",tenv=.PBStoolEnv)
	return(TRUE) }

.runCCA.plotHist = function() { # Histograms of parameter posterior distributions
	isOK = .runCCA.checkMon(); if (!isOK) stop("Reset monitors")
	getWinVal(scope="L")
	unpackList(ttcall(PBStool)$FP,scope="L")
	unpackList(ttcall(PBStool)$dots,scope="L")
	chains = chn1:chn2; pmon = pnam[pset]; nP = length(pmon); nbin = 30
	mn = .runCCA.getLayout(nP)
	isOK = .runCCA.getChains(chains); if (!isOK) stop("Generate Updates")
	file = ttcall(PBStool)$CCAsub # generated by .runCCA.getChains()
	iclr = c("aquamarine",rep("sandybrown",2),"plum","royalblue","darkseagreen",rep("gold",5)); names(iclr) = pnam
	plotname=paste("Hist(",paste(pmon,collapse="."),")-",fnam,"-",year,sep="")
	packList("plotname","PBStool",tenv=.PBStoolEnv)
	resetGraph(); par(ask=FALSE)
	par(mfrow=mn,mar=c(1,3,.5,1),oma=c(1,0,0,0))
	for (i in pmon) {
		x    = file[,i]; xmn  = mean(x)
		q95  = quantile(x,c(.025,.975))
		x95  = c(rep(q95[1],2),NA,rep(q95[2],2))
		xf   = hist(x,nclass=nbin,plot=FALSE);  xx = xf$mids;  yy = xf$counts/sum(xf$counts)
		xoff = diff(xf$breaks)[1]/2;  nn = length(xx)
		xxx  = as.vector(rbind(xx-xoff,xx+xoff,xx+xoff,xx-xoff,rep(NA,nn)))
		yyy  = as.vector(rbind(rep(0,nn),rep(0,nn),yy,yy,rep(NA,nn)))
		if (i=="Z" && isThere("Zlim")) xlim = Zlim
		else  xlim = range(xxx,na.rm=TRUE)
		xdiff = diff(xlim); ylim = c(0,max(yy)); ydiff = diff(ylim)
		ylim = ylim + c(0,.25*ydiff); xlim = xlim + c(-1,1) * (mn[2]-1)*0.05*xdiff
		y75  = ylim[2]*0.85;  y75t = y75*1.05
		y95  = ylim[2]*0.95;  y95t = y95*1.05
		ycl  = c(0,y75,NA,0,y75)
		eval(parse(text=paste("mpd=ttcall(PBStool)$Fout",MDL,"$Pend[\"",i,"\"]",sep="")))

		plot(0,0,xlim=xlim,ylim=ylim,type="n",mgp=c(3.5,.5,0),xaxt="n",yaxt="n",
			axes=FALSE,xlab="",ylab="",cex.lab=1.5)
		axis(1,at=xx-xoff,pos=0,mgp=c(0,.5,0),tck=-.01)
		lines(xlim,rep(ylim[1],2))
		axis(2,at=pretty(ylim),cex.axis=1,adj=1,mgp=c(0,.6,0),las=1)
		lines(x95,ycl,col="blue",lty=2,lwd=1)              # 95% CI
		lines(rep(mpd,2),c(0,(y75+y95)/2),col="red",lwd=1) # MPD
		lines(rep(xmn,2),c(0,y95),col="blue",lwd=1)        # mean
		text(q95+.01*xdiff,rep(y75,2),signif(q95,2),adj=c(0,0),col="blue",font=1) # 95% CI
		text(mpd+.01*xdiff,(y75+y95)/2,signif(mpd,2),adj=c(0,0),col="red",font=1) # MPD
		text(xmn+.01*xdiff,y95,signif(xmn,2),adj=c(0,0),col="blue",font=2)        # mean
		polygon(xxx,yyy,col=as.vector(iclr[i]))
		addLabel(.97,.99,i,cex=1.2,col="#400080",adj=c(1,1),font=2)
	} 
	invisible() }

.runCCA.getLayout = function(nc) {
	if (nc <= 3) return(c(nc,1))
	else if (nc <=8) return(c(ceiling(nc/2),2))
	else {
		sqn = sqrt(nc); m = ceiling(sqn); n = ceiling(nc/m)
		return(c(m, n)) }
	}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~runCCA

#sumCatTabs-----------------------------2010-06-21
# Summarize catch by year and PMFC area from modern 
# catch input data used in catch reconstruction.
#-----------------------------------------------RH
sumCatTabs=function(dat, facs=list(c("year","major")), 
     cflds=c("landed","discard"), fnam ){
	if (missing(dat) || !is.array(dat)) {
		showAlert("Supply a 4-dimensional array\n( year x major x fid x catch )")
		stop("\nSupply a 4-dimensional array\n( year x major x fid x catch )") }
	datnam = as.character(substitute(dat))
	if (missing(fnam) || is.null(fnam) || fnam=="")
		fnam=paste("cattab-",datnam,".csv",sep="")
	FIDS = c("Trawl","Halibut","Sablefish","Dogfish & Lingcod","H&L Rockfish")
	fids = c("trawl","halibut","sable","dogling","hlrock")
	names(FIDS)=names(fids)=1:5
	pmfc = c("4B","4A","3C","3D","5A","5B","5C","5D","5E"); names(pmfc)=1:9
	dnam=dimnames(dat)
	cat("Catch Summary\n\n",file=fnam)
	for (i in dnam$fid) {
		cat(FIDS[i],"\n",file=fnam,append=TRUE)
		idat=dat[,,i,cflds]
		catch=apply(idat,1:2,sum,na.rm=TRUE)
		cat("year,",paste(pmfc[colnames(catch)],collapse=","),"\n",sep="",file=fnam,append=TRUE)
		apply(cbind(year=rownames(catch),catch),1,function(x){cat(paste(x,collapse=","),"\n",sep="",file=fnam,append=TRUE)})
		cat("\n\n",file=fnam,append=TRUE)
	}
	invisible() }
#---------------------------------------sumCatTabs

#trackBycat-----------------------------2013-01-28
# Track annual fish group catches between depth limits.
#-----------------------------------------------RH
trackBycat = function(strSpp="396", major=5:7, mindep=70, maxdep=441, 
     dbs=c("gfb","pht","fos"), trawl="bottom", spath=.getSpath(), 
     pyrs=1996:2010, rda=NULL, ioenv=.GlobalEnv) {

	par0 = par(no.readonly=TRUE); on.exit(par(par0))
	assign("PBStool",list(module="M03_Fishery",call=match.call(),ioenv=ioenv,plotname="Comp"),envir=.PBStoolEnv)
	fish=c("POP","rockfish","turbot","flatfish","hake","sharks","other"); nfish=length(fish)

	if (is.null(rda) || rda=="") {  # create the bycatch array using SQL queries
	ndbs = length(dbs)
	gear = rep(0,ndbs); names(gear) = dbs
	if (trawl=="bottom")  gear = sapply(gear,function(x){1})
	if (trawl=="midwater") {
		gear = sapply(gear,function(x){3}); gear["gfb"] = 6 }
	for (i in dbs) {
		if (i=="gfb") {
			fqtName = deparse("gfb_bycatch.sql"); dbName  = deparse("GFBioSQL")
			expr=paste("getData(",fqtName,",",dbName,sep="") }
		if (i=="pht") {
			fqtName = deparse("pht_bycatch.sql"); dbName  = deparse("PacHarvest")
			expr=paste("getData(",fqtName,",",dbName,sep="") }
		if (i=="fos") {
			fqtName = deparse("fos_bycatch.sql"); dbName  = deparse("GFFOS")
			expr=paste("getData(",fqtName,",",dbName,",type=\"ORA\",server=\"GFSH\",trusted=FALSE",
				",uid=\"haighr\",pwd=\"haighr357\"",sep="") }
		expr=paste(".flush.cat(\"Grabbing groups from ",toupper(i),"\\n\"); ",
			expr,",strSpp=",deparse(strSpp),",path=",deparse(spath),",major=",deparse(major),
			",mindep=",mindep,",maxdep=",maxdep,",dummy=",gear[i],",tenv=penv()); ",i,"dat=PBSdat",sep="")
		eval(parse(text=expr)) 
	}
	yrmin=3000; yrmax=0
	for (i in dbs) {
		idat = get(paste(i,"dat",sep=""))
		yrmin = min(yrmin, idat[["year"]])
		yrmax = max(yrmax, idat[["year"]]) }
	yrs = yrmin:yrmax; nyrs = length(yrs)

	bycatch = array(0,dim=c(nyrs,nfish,ndbs),dimnames=list(year=yrs,fish=fish,db=c("gfb","pht","fos")))
	for (i in dbs) {
		idat = get(paste(i,"dat",sep=""))
		bycatch[as.character(idat[,"year"]),fish,i] = as.matrix(idat[,fish])
	}
	clrs=c("red","orange","yellow","lightgreen","cyan","royalblue","magenta")
	attr(bycatch,"clrs") = clrs
	data(pmfc,envir=penv())
	save("bycatch",file=paste("bycatch-",paste(pmfc[as.character(major),"gmu"],collapse=""),"-",trawl,".rda",sep=""))
	} # end the section that checks for rda name
	else {
		getFile(rda,senv=ioenv,use.pkg=TRUE,tenv=penv())
		clrs=attributes(bycatch)$clrs }

	bartab = apply(bycatch,c(1,2),sum)         ### catch (t)
	amat=t(bartab);  amat=amat/1000.           ### absolute composition (kt)
	rmat=apply(amat,2,function(x){x/sum(x)})   ### relative composition

	plotname=paste("Comp-",strSpp,"-major(",paste(major,collapse=""),
		")-",trawl,"-tows",sep="")
	packList(c("bartab","amat","rmat","clrs","plotname"),"PBStool",tenv=.PBStoolEnv)

	expandGraph(mfrow=c(2,1),mar=c(2,4,1,1),oma=c(2,0,0,0),las=1)
	for (i in 1:2) {
		fnam = paste(switch(i,"Abs","Rel"),plotname,sep="")
		bmat = switch(i,amat,rmat)[,as.character(pyrs)]
		barplot(bmat,col=clrs,border="gainsboro",space=0,xlim=c(0,dim(bmat)[2] + 1.5),
			cex.axis=0.9,cex.names=0.9,legend.text=switch(i,TRUE,FALSE),
			args.legend=list(x="topright",bty="n",border="gainsboro",cex=0.9))
		lines(par()$usr[1:2],c(0,0),col=1) 
		mtext(switch(i,"Catch (kt)","Relative cacth"),side=2,line=2.5,cex=1.5,las=0)
		write.csv(t(bmat),file=paste(fnam,".csv",sep=""))
	}
	mtext("Year",outer=TRUE,side=1,line=0.5,cex=1.5)
	invisible(bycatch)
#browser();return()
}
#---------------------------------------trackBycat

