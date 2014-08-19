#===============================================================================
# Module 7: Catch Reconstruction
# ------------------------------
#  buildCatch   Build a catch history of BC rockfish 1918--present.
#  plotData     Plot diagnostic data for catch reconstructions.
#  plotRecon....Plot reconstructed catch using barplots stacked by PMFC area.
#===============================================================================

#buildCatch-----------------------------2014-08-18
# Catch reconstruction algorithm for BC rockfish.
# Use ratios of species catch to ORF catch for multiple fisheries.
# Matrix indices: i=year, j=major, k=fid, l='spp'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~RH
buildCatch=function(dbdat, strSpp="396", dfld="ORF", 
   major=c(1,3:9), refyrs=1997:2005, fidout=c(1:5,10), 
   saveinfo=TRUE, eps=FALSE, pix=FALSE, wmf=FALSE, 
   sql=FALSE, spath=.getSpath(), only.sql=FALSE,
   uid=Sys.info()["user"], pwd=uid, 
   reconstruct=TRUE, diagnostics=TRUE, ioenv=.GlobalEnv, ...)
{
	if (!only.sql) {
	### Global list object 'PBStool' stores results from the analysis
	assign("PBStool",list(module="M07_BuildCatch",call=match.call(),args=args(buildCatch),
		spp=strSpp,pD=1,eps=eps,wmf=wmf),envir=.PBStoolEnv)
	# pD = Counter for plotData diagnostics
	### Function to convert numbers to proportions
	pcalc=function(x){if (all(x==0)) rep(0,length(x)) else x/sum(x)}
	sysyr=as.numeric(substring(Sys.time(),1,4)) ### maximum possible year
	if (!file.exists("./CRdiag")) dir.create("CRdiag")
	else {
		diag.files = list.files("./CRdiag")
		if (length(diag.files)>0) 
			junk = file.remove(paste("./CRdiag/",diag.files,sep=""))
	}
	clrs.fishery=c("red","blue","orange","green","yellow"); names(clrs.fishery)=1:5

	### ------------------------------------------------------
	### 1. Compile the historical catches for POP, ORF and TRF
	### ------------------------------------------------------

	getFile(orfhistory,use.pkg=TRUE,tenv=penv()); dat=orfhistory ### catch in kg
	hisyrs=sort(unique(dat$year))
	HISYRS=hisyrs[1]:hisyrs[length(hisyrs)]
	majhis=sort(unique(dat$major))
	sou=sort(unique(dat$source))
	act=sort(unique(dat$action))
	fsh=sort(unique(dat$fishery))

	htab=array(0,dim=c(length(HISYRS),length(majhis),length(sou),length(act),length(fsh),3),
		dimnames=list(HISYRS,majhis,sou,act,fsh,c("POP","ORF","TRF")))
	names(dimnames(htab))=c("year","major","source","action","fishery","catch")

	for (a in act) {
		adat=dat[is.element(dat$action,a),]
		if(nrow(adat)==0) next
		for (b in fsh) {
			bdat=adat[is.element(adat$fishery,b),]
			if(nrow(bdat)==0) next
			for (i in sou) {
				ii=as.character(i)
				idat=bdat[is.element(bdat$source,i),]
				if(nrow(idat)==0) next
				for (j in majhis) {
					jj=as.character(j)
					jdat=idat[is.element(idat$major,j),]
					if(nrow(jdat)==0) next
					z=is.element(jdat$spp,"396")
					if (any(z)) {
						POP= sapply(split(jdat$catch[z],jdat$year[z]),sum,na.rm=TRUE)/1000
						htab[names(POP),jj,ii,a,b,"POP"]=POP }
					ORF= sapply(split(jdat$catch[!z],jdat$year[!z]),sum,na.rm=TRUE)/1000
					TRF= sapply(split(jdat$catch,jdat$year),sum,na.rm=TRUE)/1000
					htab[names(ORF),jj,ii,a,b,"ORF"]=ORF
					htab[names(TRF),jj,ii,a,b,"TRF"]=TRF
	} } } } ### close loops j,i,b,a

	### For the years 1918 to 1949, no records exist to delineate POP, ORF, and TRF.
	### In essence TRF = ORF for Dominion Bureau Stats and Stewart's US catch of BC rockfish
	### Therefore, use empirical data from 1951 to 1995 to estimate ORF from TRF (the difference is POP).
	sARF = apply(htab,c(1,6),sum)                                           ### sum of all rockfish
	zUNK = is.element(rownames(sARF),as.character(1918:1952))               ### unknown ORF and POP
	zOBS = is.element(rownames(sARF),as.character(c(1953:1965,1967:1995)))  ### exclude anomalous 1966 observation
	oTRF = sARF[zOBS,"TRF"]
	oORF = sARF[zOBS,"ORF"]
	lmo  = lm(log2(oORF)~log2(oTRF))
	alp  = lmo$coeff[1]; bet = lmo$coeff[2]
	htab[zUNK,,,,,"ORF"] = pmin(htab[zUNK,,,,,"TRF"],2^(alp+bet*log2(htab[zUNK,,,,,"TRF"])))  ### ORF cannot be bigger than TRF
	htab[zUNK,,,,,"POP"] = htab[zUNK,,,,,"TRF"] - htab[zUNK,,,,,"ORF"]
	
	if (diagnostics){
		mm = as.character(major)
		for (spp in dimnames(htab)$catch) {
			for (fish in dimnames(htab)$fishery) {
				plotData(apply(htab[,mm,,,fish,spp],c(1,3),sum),paste("htab ",spp," in ",fish,sep="")) }
			plotData(apply(htab[,mm,,,,spp],c(1,3),sum),paste("htab ",spp," in all fisheries",sep=""))
	}	}

	# Sabotage htab to remove PacHarv3 for trap and trawl between 1954 & 1995 (GFCatch provides best estimates, see Rutherford 1999)
	htab[as.character(1954:1995),,"PacHarv3","max",c("trap","trawl"),] = 0
	if (diagnostics)
		plotData(apply(htab[,mm,,,"trawl","POP"],c(1,3),sum),paste("htab sabotaged POP in trawl",sep=""))

	htabmax=htab[,,,"max",,]
	### historical maximum catch (POP,ORF,TRF) by year, major, gear (i.e. comparative):
	hismax=apply(htabmax,c(1:2,4:5),max,na.rm=TRUE) # collapse 'source' (dim 3)
	htabadd=htab[,,,"add",,]
	### historical unique catch (POP,ORF,TRF) by year, major, gear (i.e. additive):
	hisadd=apply(htabadd,c(1:2,4:5),sum,na.rm=TRUE) # collapse 'source' (dim 3)
	MAJ=dimnames(hismax)$major
	clrs.major=rep("gainsboro",length(MAJ)); names(clrs.major)=MAJ
	clrs.major[as.character(c(1,3:9))]=c("moccasin","blue","lightblue","yellow","orange","red","seagreen","lightgreen")

	if (saveinfo)
		packList(c("htab","htabmax","htabadd","hismax","hisadd"),"PBStool",tenv=.PBStoolEnv)
	### Historical used again on line 335

	### ------------------------------------------------
	### 2. Gather the RRF catches (landed and discarded)
	### ------------------------------------------------
#browser();return()

	if (missing(dbdat) && sql==FALSE) {
		mess=paste("Either provide a list object 'dbdat' or set 'sql=TRUE'.\n\n",
			"Ideally, 'dbdat' should contain data frames:\n", 
			"'ph3cat' = PacHarv3 database (all fisheries)\n",
			"'gfcdat' = GFCatch database (trawl, trap, h&l)\n",
			"'phtdat' = PacHarvest database (groundfish trawl)\n",
			"'phhdat' = PacHarvHL database (halibut bycatch from DMP validated landings)\n",
			"'phsdat' = PacHarvSable database (sablefish trap)\n",
			"'phvdat' = PacHarvHL database (validated landings Sched II & ZN)\n",
			"'phfdat' = PacHarvHL database (fisherlog records Sched II & ZN)\n",
			"'fosdat' = GFFOS database on the GFSH server (all fisheries)\n",
			"with fields:\nc( 'fid', 'date', 'major', 'landed', 'discard', 'POP', 'ORF', 'TAR' )",sep="")
		showError(mess,as.is=TRUE,x=0.05,adj=0,cex=1.2) }

	lenv=sys.frame(sys.nframe()) ### local environment
	cflds = c("landed","discard","POP","ORF","TAR")
	keep  = c("fid","date","major","minor",cflds)
	ufos  = c("POP","PAH","SBF","DOG","RFA","RFA","PAH","DOG","LIN")
	dbs   = c("ph3cat","gfccat","phtcat","phhcat","phscat","phvcat","phfcat","foscat")
	} # end skip if only.sql
#browser();return()

	if (sql) {
		if (isThere("PBSdat")) rm(PBSdat) ### remove from current environment
		uid=rep(uid,2)[1:2]; pwd=rep(pwd,2)[1:2]

		###-----Start querying the databases-----

		### NOTE: Currently the Oracle call to PacHarv3 creates an instability so that a second call to Oracle crashes.

		### PacHarv3 catch summary for fids 1:5 and 0 (unknown)
		getData("ph3_fcatORF.sql",dbName="HARVEST_V2_0",strSpp=strSpp,path=spath,
			server="ORAPROD",type="ORA",trusted=FALSE,uid=uid[1],pwd=pwd[1],tenv=penv())
			assign("ph3dat",PBSdat);  rm(PBSdat) ### just to be safe
			dimnames(ph3dat)[[1]]=1:nrow(ph3dat)
			save("ph3dat",file="ph3dat.rda")

		### GFCatch records for fids (1,3,5)
		getData("gfc_fcatORF.sql","GFCatch",strSpp=strSpp,path=spath,tenv=penv())
			assign("gfcdat",PBSdat); rm(PBSdat) ### just to be safe
			#gfcdat$year=as.numeric(substring(gfcdat$date,1,4))  ### doesn't appear to be used
			dimnames(gfcdat)[[1]]=1:nrow(gfcdat)
			save("gfcdat",file="gfcdat.rda")
		
		### PacHarvest records for fids (1)
		getData("pht_tcatORF.sql","PacHarvest",strSpp=strSpp,path=spath,tenv=penv())
			assign("phtdat",PBSdat); rm(PBSdat) ### just to be safe
			save("phtdat",file="phtdat.rda")

		### PacHarvHL halibut validation records for fids (2,7)
		getData("phhl_hcatORF.sql","PacHarvHL",strSpp=strSpp,path=spath,tenv=penv())   ### validated (DMP) catch
			assign("phhdat",PBSdat); rm(PBSdat) ### just to be safe
			save("phhdat",file="phhdat.rda")

		### PacHarvSable fisherlogs for fids (3)
		getData("phs_scatORF.sql","PacHarvSable",strSpp=strSpp,path=spath,fisheryid=3,logtype="FISHERLOG",tenv=penv())
			assign("phsdat",PBSdat); rm(PBSdat) ### just to be safe
			save("phsdat",file="phsdat.rda")

		### PacHarvHL validation records for fids (2,4,5)
		getData("phhl_vcatORF.sql","PacHarvHL",strSpp=strSpp,path=spath,tenv=penv())   ### validated (DMP) catch
			assign("phvdat",PBSdat); rm(PBSdat) ### just to be safe
			save("phvdat",file="phvdat.rda")

		### PacHarvHL fisherlog records for fids (4,5)
		getData("phhl_fcatORF.sql","PacHarvHL",strSpp=strSpp,path=spath,logtype="FISHERLOG",tenv=penv()) ### fisherlog catch
			assign("phfdat",PBSdat); rm(PBSdat) ### just to be safe
			save("phfdat",file="phfdat.rda")

		### FOS catch from all fisheries (fid=1:5) -- now use GFFOS on SQL Server, not Oracle
		getData("fos_vcatORF.sql", dbName="GFFOS", strSpp=strSpp, path=spath, tenv=penv())
			#server="GFSH",type="ORA",trusted=FALSE,uid=uid[2],pwd=pwd[2])
			assign("fosdat",PBSdat); rm(PBSdat) ### just to be safe
			dimnames(fosdat)[[1]]=1:nrow(fosdat)
			save("fosdat",file="fosdat.rda")

		### Wrap up the fisheries landings into a list object
		blob=list(ph3dat=ph3dat,gfcdat=gfcdat,phtdat=phtdat,phhdat=phhdat,phsdat=phsdat,phvdat=phvdat,phfdat=phfdat,fosdat=fosdat)
		eval(parse(text=paste("dbdat=\"cat",strSpp,"orf\"",sep="")))
		expr=paste(dbdat,"=blob; save(\"",dbdat,"\",file=\"",dbdat,".rda\")",sep="")
		eval(parse(text=expr))
		###-----Stop querying the databases-----
	}
	else {
		dbdat=as.character(substitute(dbdat)) ### database list object name
		expr=paste("getFile(",dbdat,",senv=ioenv,try.all.frames=TRUE,tenv=penv()); fnam=names(",dbdat,"); unpackList(",dbdat,")",sep="")
		eval(parse(text=expr)) 
	}
	if (only.sql) return()

	### Consolidate PacHarv3 records (fid=c(1:5))
	ph3cat = as.data.frame(t(apply(ph3dat,1,function(x){
		ufos=c("POP","PAH","SBF","DOG","RFA"); ufid=1:5; names(ufid)=ufos
		f = x["fid"]
		if (f==0) { 
			z = x[ufos]==max(x[ufos],na.rm=TRUE)
			utar = ufos[z][1]; fid = ufid[utar]; ucat = x[utar]
		}
		else { fid=f; ucat=x[ufos[f]] } 
		out = c(x["year"],fid,date=as.Date(paste(x["year"],"-07-01",sep="")),
			x[c("major","minor","landed","discard","POP","ORF")],ucat)
		return(out) } )))
	names(ph3cat) = c("year",keep)
	ph3cat$date = as.Date(paste(ph3cat$year,"-07-01",sep=""))
	ph3cat = ph3cat[,-1] ### get rid of 'year'
	save("ph3cat",file="ph3cat.rda")

	### Consolidate GFCatch records (fid=c(1,3,5))
	gfccat = gfcdat
	gfccat$TAR = rep(0,nrow(gfccat))
	for (i in 1:5) {
		ii = is.element(gfccat$fid,i)
		if (any(ii)) gfccat$TAR[ii] = gfccat[,ufos[i]][ii]
	}
	gfccat = gfccat[,keep]
	trash  = apply(gfccat[,cflds],1,function(x){all(x==0)})
	gfccat = gfccat[!trash,] ; dimnames(gfccat)[[1]] = 1:nrow(gfccat)
	save("gfccat",file="gfccat.rda")

	### Consolidate PacHarvest landings (fid=1)
	phtcat = phtdat[,keep]
	trash  = apply(phtcat[,cflds],1,function(x){all(x==0)})
	phtcat = phtcat[!trash,] ; dimnames(phtcat)[[1]] = 1:nrow(phtcat)
	save("phtcat",file="phtcat.rda")

	### Consolidate PacHarvHL halibut bycatch (fid=2)
	phhcat = phhdat
	phhcat$TAR = phhcat$PAH
	phhcat = phhcat[,keep]
	phhcat$fid = rep(2,nrow(phhcat))  ### because there are a few fid=7 (halibut + sablefish)
	trash  = apply(phhcat[,cflds],1,function(x){all(x==0)})
	phhcat = phhcat[!trash,] ; dimnames(phhcat)[[1]] = 1:nrow(phhcat)
	save("phhcat",file="phhcat.rda")

	### Consolidate PacHarvSable landings (fid=3)
	phscat = phsdat[,keep]
	trash  = apply(phscat[,cflds],1,function(x){all(x==0)})
	phscat = phscat[!trash,] ; dimnames(phscat)[[1]] = 1:nrow(phscat)
	save("phscat",file="phscat.rda")

	### Consolidate PacHarvHL validation landings (fid=c(2,4,5))
	phvcat = phvdat
	phvcat$TAR = rep(0,nrow(phvcat))
	for (i in 1:9) {
		ii = is.element(phvcat$fid,i)
		if (any(ii)) {
			phvcat$TAR[ii] = phvcat[,ufos[i]][ii]
			if (i==4) phvcat$TAR[ii] = phvcat$TAR[ii] + phvcat$LIN[ii] ### add lingcod to dogfish if Sched II
			if (i==6) phvcat$fid[ii] = 5                               ### Sablefish/ZN
			if (i==7) phvcat$fid[ii] = 2                               ### Sablefish/Halibut
			if (any(i==c(8,9))) phvcat$fid[ii] = 4                     ### Dogfish or lingcod
		}
	}
	phvcat = phvcat[,keep]
	trash  = apply(phvcat[,cflds],1,function(x){all(x==0)})
	phvcat = phvcat[!trash,] ; dimnames(phvcat)[[1]] = 1:nrow(phvcat)
	save("phvcat",file="phvcat.rda")

	### Consolidate PacHarvHL fisherlog records (fid=c(4,5))
	phfcat = phfdat[,keep]
	trash=apply(phfcat[,cflds],1,function(x){all(x==0)})
	phfcat=phfcat[!trash,]; dimnames(phfcat)[[1]]=1:nrow(phfcat)
	save("phfcat",file="phfcat.rda")

	### Consolidate GFFOS records (fid=1:5)
	z = fosdat$date >= as.POSIXct("2000-01-01") & fosdat$date <= Sys.time()  ### up to the current date
	#z = fosdat$date >= as.POSIXct("2000-01-01") & fosdat$date <= as.POSIXct("2010-06-30") ### for POP assessment
	foscat = fosdat[z,keep]
	trash=apply(foscat[,cflds],1,function(x){all(x==0)})
	foscat=foscat[!trash,]; dimnames(foscat)[[1]]=1:nrow(foscat)
	save("foscat",file="foscat.rda")

	modyrs = majmod = fid = NULL
	for (i in dbs) { 
		if(!isThere(i,envir=lenv)) next
		icat=get(i)
		modyrs=c(modyrs,unique(as.numeric(substring(icat$date,1,4))))
		majmod=c(majmod,unique(icat$major))
		fid=c(fid,unique(icat$fid)) }
	modyrs=sort(unique(modyrs)); majmod=sort(unique(majmod)); fid=sort(unique(fid))
	modyrs=modyrs[is.element(modyrs,1945:sysyr)]

	if (isThere("refyrs") && !is.null(refyrs) && length(intersect(refyrs,modyrs))==0)
		showError("refyrs","nodata") 
	MODYRS=modyrs[1]:modyrs[length(modyrs)]

	### Need to reconcile majors (remove concept of referrence majors)
	majmax=intersect(majhis,majmod)  ### maximum available overlap in majors from data
	if (is.null(major))
		MM=majmax
	else 
		MM=intersect(major, majmax)
	mm = as.character(MM)
	Cflds=c("landed","discard","POP","ORF","TRF","TAR")
#browser(); return()

	### Collect modern landings (t), including those in unknown areas
	catmod0=array(0,dim=c(length(MODYRS),length(majmod),length(fid),length(Cflds),length(dbs)),
		dimnames=list(year=MODYRS,major=majmod,fid=fid,catch=Cflds,dbs=dbs))
	for (a in dbs) {
		if(!isThere(a,envir=lenv)) next
		acat=get(a)
		acat$year = as.numeric(substring(acat$date,1,4))
		acat$TRF  = acat[["POP"]] + acat[["ORF"]]  ### total rockfish
		if (is.null(acat$discard)) acat$discard = rep(0,nrow(acat))
		for (k in fid) {
			kk=as.character(k)
			kdat=acat[is.element(acat$fid,k),]
			if(nrow(kdat)==0) next
			for (j in majmod) {
				jj=as.character(j)
				jdat=kdat[is.element(kdat$major,j),]
				if(nrow(jdat)==0) next
				landed=sapply(split(jdat$landed,jdat$year),sum,na.rm=TRUE)/1000.
				POP=  sapply(split(jdat$POP,jdat$year),sum,na.rm=TRUE)/1000.
				ORF=  sapply(split(jdat$ORF,jdat$year),sum,na.rm=TRUE)/1000.
				TRF=  sapply(split(jdat$TRF,jdat$year),sum,na.rm=TRUE)/1000.
				TAR=  sapply(split(jdat$TAR,jdat$year),sum,na.rm=TRUE)/1000.
				catmod0[names(landed),jj,kk,"landed",a] = landed
				catmod0[names(POP),jj,kk,"POP",a]       = POP
				catmod0[names(ORF),jj,kk,"ORF",a]       = ORF
				catmod0[names(TRF),jj,kk,"TRF",a]       = TRF
				catmod0[names(TAR),jj,kk,"TAR",a]       = TAR
				if (!is.null(jdat$discard)){
					discard=sapply(split(jdat$discard,jdat$year),sum,na.rm=TRUE)/1000.
					catmod0[names(discard),jj,kk,"discard",a] = discard }
	} } } ### close loops j & k & i

	if (diagnostics){
		for (spp in dimnames(catmod0)$catch) {
			for (db in dimnames(catmod0)$dbs) {
				plotData(apply(catmod0[,mm,,spp,db],c(1,3),sum),paste("catmod0 ",spp," in ",db,sep=""),col=clrs.fishery) }
			plotData(apply(catmod0[,mm,,spp,],c(1,3),sum),paste("catmod0 ",spp," in all databases",sep=""),col=clrs.fishery)
	}	}

	### Allocate catch (t) from unknown major (code=0) to user-specified majors (mm)
	catmod1=catmod0[,is.element(dimnames(catmod0)[[2]],mm),,,,drop=FALSE]
	for (aa in dimnames(catmod1)$dbs) {          ### databases
		for (kk in dimnames(catmod1)$fid) {       ### fishery IDs
			for (ll in dimnames(catmod1)$catch) {  ### catch categories
				#pmaj=apply(catmod1[,mm,kk,ll,aa],1,pcalc)  ### allocation doesn't work when all unknown
				#allo   = apply(pmaj,1,function(x,u){x*u},u=catmod0[,"0",kk,ll,aa])
				### Allocate based on mean proportions in each major observed over a number of years (>2)
				cattab = catmod1[,mm,kk,ll,aa]
				catobs = apply(cattab,1,function(x){length(x[x>0])})
				catyrs = names(catobs)[catobs>2]
				if (length(catyrs)==0) next
				pmaj   = apply(apply(cattab[catyrs,,drop=FALSE],1,pcalc),1,mean)
				allo   = t(t(catmod0[,"0",kk,ll,aa]))%*%pmaj; dimnames(allo)[[2]] = mm
				catmod1[,mm,kk,ll,aa] = catmod1[,mm,kk,ll,aa] + allo 
	} } }

	if (diagnostics){
		for (spp in dimnames(catmod1)$catch) {
			for (db in dimnames(catmod1)$dbs) {
				plotData(apply(catmod1[,mm,,spp,db],c(1,3),sum),paste("catmod1 ",spp," in ",db,sep=""),col=clrs.fishery) }
			plotData(apply(catmod1[,mm,,spp,],c(1,3),sum),paste("catmod1 ",spp," in all databases",sep=""),col=clrs.fishery)
	}	}

	### Merge modern landings from various databases (ie, collapse DB sources)
	catmod=array(0,dim=rev(rev(dim(catmod1))[-1]), dimnames=rev(rev(dimnames(catmod1))[-1]))
	ii = dimnames(catmod)$year
	jj = dimnames(catmod)$major
	ll = dimnames(catmod)$catch
	for (kk in dimnames(catmod)$fid) {  ### fishery IDs
		k = as.numeric(kk)
		dbmerge = switch(k,
			dbs[c(2,3,8)], dbs[c(1,4,6,8)], dbs[c(2,5,8)], dbs[c(1,6,7,8)], dbs[c(1,2,6,7,8)] ) # drop PacHarv3 for Trawl and Trap (see Rutherford 1999)
			#dbs[c(1,2,3,8)], dbs[c(1,4,6,8)], dbs[c(1,2,5,8)], dbs[c(1,6,7,8)], dbs[c(1,2,6,7,8)] )
		fcat = apply(catmod1[ii,jj,kk,ll,dbmerge,drop=FALSE],1:4,max)
		catmod[ii,jj,kk,ll]=fcat
		### adjust for quirks in DB transitions
		if (any(k==c(1,3))) {
			if (k==1) { iii="2007"; aaa=c("phtcat","foscat") }
			if (k==3) { iii="2006"; aaa=c("phscat","foscat") }
			qcat=apply(catmod1[iii,jj,kk,ll,aaa,drop=FALSE],1:4,sum) }
		if (any(k==c(2,4,5))) {
			iii = "2006"
			if (k==2) { aaa=c("phhcat","phvcat","foscat") }
			if (any(k==c(4,5))) { aaa=c("phvcat","phfcat","foscat") }
			qcat=apply(catmod1[iii,jj,kk,ll,aaa,drop=FALSE],1:4,function(x){ max(x[1:2]) + x[3] }) }
		catmod[iii,jj,kk,ll] = qcat 
	}
	### Make sure that TRF = ORF + POP
	TOPmod = apply(catmod[,,,c("POP","ORF","TRF")],1:3,function(x){
		if(round(x[1]+x[2],5)==round(x[3],5)) return(x)
		else {
			x[3] = max(x[3],x[1]+x[2]) # ensure TRF is the largest value
			x[1] = x[3] - x[2]         # assume ORF is now correct, POP is residual
			return(x) } })
	kk = dimnames(catmod)$fid
	for (ll in c("POP","ORF","TRF"))
		catmod[ii,jj,kk,ll] = TOPmod[ll,ii,jj,kk]
#browser();return()
	expr=paste("cat",strSpp,"mod=catmod; save(\"cat",strSpp,"mod\",file=\"cat",strSpp,"mod.rda\")",sep="")
	eval(parse(text=expr))

	if (diagnostics){
		for (spp in dimnames(catmod)$catch) {
			plotData(apply(catmod[,mm,,spp],c(1,2),sum),paste("catmod ",spp," by major",sep=""),col=clrs.fishery)
			plotData(apply(catmod[,mm,,spp],c(1,3),sum),paste("catmod ",spp," by fishery",sep=""),col=clrs.fishery)
	}	}
	
	if (isThere("refyrs") && !is.null(refyrs) ) 
		ctab = catmod[as.character(refyrs),,,,drop=FALSE] 
	else ctab=catmod

	### Get three historical time lines : trawl, trap, & H&L
	tmp0=hismax[,mm,,,drop=FALSE]
	allhis=array(0,dim=dim(tmp0),dimnames=dimnames(tmp0)) ### initialize the POP/ORF/TRF array
	for (l in dimnames(allhis)[[4]]) {
		for (k in fsh) {
			tmp1=hismax[,,k,l]
			tmp1=tmp1[,mm,drop=FALSE]  ### use only specified majors
			tmp2=hisadd[,,k,l]
			tmp2=tmp2[,mm,drop=FALSE]  ### use only specified majors
			allhis[,,k,l]=tmp1+tmp2
		}
	}
	if (diagnostics){
		for (spp in dimnames(allhis)$catch) {
			plotData(apply(allhis[,mm,,spp],c(1,2),sum),paste("allhis ",spp," by major",sep=""),col=clrs.major[mm])
			plotData(apply(allhis[,mm,,spp],c(1,3),sum),paste("allhis ",spp," by fishery",sep=""))
	}	}
	orfhis=allhis[,,,dfld]
	if (any(strSpp==c("396"))) rawhis=allhis[,,,"POP"]
	if (saveinfo)
		packList(c("catmod","catmod0","catmod1","ctab","MM","mm","allhis"),"PBStool",tenv=.PBStoolEnv)
	### Terminate here if all you want are the modern landings
	if (!reconstruct) return(list(catmod0=catmod0,catmod1=catmod1,catmod=catmod)) 
#browser();return()

	### -------------------
	### 3. Calculate ratios
	### -------------------

	### Catch reference summary tables
	cref=ctab[,mm,,,drop=FALSE]                ### catch reference
	catMF=apply(cref,2:4,sum,na.rm=TRUE)       ### total catch by major and fid
	catYM=apply(cref,c(1:2,4),sum,na.rm=TRUE)  ### total catch by year and major
	catYF=apply(cref,c(1,3:4),sum,na.rm=TRUE)  ### total catch by year and fid
	if (saveinfo)
		packList(c("cref","catYM","catMF","catYF"),"PBStool",tenv=.PBStoolEnv)

	### alpha - Proportion RRF caught in a major area for each fid
	alpha=apply(catMF[,,"landed"],2,function(x){
		if (all(x==0)) rep(0,length(x)) else x/sum(x)}) ### columns (fids) sum to 1
	dimnames(alpha)=dimnames(catMF)[1:2]
	if (diagnostics){
		plotData(alpha,"alpha (fishery in major)",col=clrs.fishery,type="bars")
		plotData(t(alpha),"alpha (major in fishery)",col=clrs.major[mm],type="bars")
	}

	### beta - Proportion RRF caught in H&L fisheries for each major
	dnam=intersect(c("2","4","5"),dimnames(alpha)[[2]]) ### dimnames for H&L
	beta=t(apply(catMF[,dnam,"landed",drop=FALSE],1,function(x){
		if (all(x==0)) rep(0,length(x)) else x/sum(x)})) ### columns (fids) sum to 1
	dimnames(beta)[[2]]=dnam; names(dimnames(beta)) = c("major","fid")
	if (diagnostics){
		plotData(beta,"beta (fishery in major)",col=clrs.fishery,type="bars")
		plotData(t(beta),"beta (major in fishery)",col=clrs.major[mm],type="bars")
	}

	### Ratio RRF catch to other catch (rtar)
	rtar=list() 
	### Use cref instead of catMF
	for (i in c("POP","ORF","TRF","TAR")) {
		z0  = cref[,,,"landed"]==0
		z1  = cref[,,,i]==0
		z2  = z0&z1
		rtmp = cref[,,,"landed"]/cref[,,,i]
#if(i=="TRF") {browser();return()}
		### order is important here (process equalities, zero-denominator, then zero-numerator)
		rtmp[z2]=0; rtmp[!z2&z1]=1; rtmp[!z2&!z1&z0]=0 
		rmat = apply(rtmp,2:3,mean)
		rtar[[i]] = rmat }
	### gamma - Ratio of RRF to a larger group (e.g., other rockfish)
	rfac=rtar[[dfld]]
	gamma=rfac[mm,,drop=FALSE]  ### use only specified majors

	# Special trawl calculations by Paul Starr for YTR based on Brian Mose's suggestions
	if (strSpp=="418" && !is.null(list(...)$pjs) && list(...)$pjs) {
		if (dfld=="ORF") {
			if (!is.null(list(...)$outside) && list(...)$outside)
				gamma[,1]=matrix(c(0,0.3139525,0.253063,0.0467262,0.0034517,0,0.4451063,0.0049684),ncol=1)
			else
				gamma[,1]=matrix(c(0,0.3138979,0.253063,0.2196372,0.2346488,0.1861062,0.4605869,0.0049684),ncol=1)
		}
		if (dfld=="TRF") {
			if (!is.null(list(...)$outside) && list(...)$outside)
				gamma[,1]=matrix(c(0,0.2596861,0.2291434,0.0388679,0.0009657,0,0.1670699,0.0029361),ncol=1)
			else
				gamma[,1]=matrix(c(0,0.2596484,0.2291434,0.1595966,0.1024332,0.1238941,0.239913,0.0029361),ncol=1)
		}
	}
	write.csv(gamma,file=paste0("gamma",dfld,".csv")) #;return(gamma)
#browser();return()
	if (diagnostics){
		plotData(gamma,"gamma (fishery in major)",col=clrs.fishery,type="bars")
		plotData(t(gamma),"gamma (major in fishery)",col=clrs.major[mm],type="bars")
	}

	### delta - Discard rate of landed species per ORF from observer logs
	assign("PBStool",ttget(PBStool)) ### remember global collection object because 'calcRatio' overwrites it
	drSpp=list(c("discard","landed"),c("discard","TAR"),c("discard","ORF"))
	drate=sapply(drSpp,function(x){paste(x,collapse=":")}) ### discard ratio description
	drN=length(drSpp)                                      ### number of discard rates
	dfac=array(0,dim=c(length(mm),length(fid),drN+1),
		dimnames=list(major=mm,fid=fid,rate=c(drate,"dr")))
	ologs=as.list(rep(NA,length(fid))); names(ologs)=fid
	for (k in fid) {
		kk=as.character(k); jj=dimnames(dfac)[[1]]; j=as.numeric(jj)
		if (k==1) {
			discat=phtcat[is.element(phtdat$log,1),]
			dyrs=1996:2006 }
		else if (any(k==c(2:5))) {
			if (any(k==c(2,4,5))) 
				getData("phhl_fcatORF.sql","PacHarvHL",strSpp=strSpp,
					path=spath,fisheryid=k,logtype="OBSERVRLOG",tenv=penv())
			if (k==3)
				getData("phs_scatORF.sql","PacHarvSable",strSpp=strSpp,
					path=spath,fisheryid=k,logtype="OBSERVRLOG",tenv=penv())
			discat=PBSdat; dyrs=2000:2004 }
		if (nrow(discat)==0) next
		ologs[[kk]] = discat
		for (d in 1:drN) { ### discard ratio combos 'drSpp'
			dd=drate[d]
			### if denominator is landed catch, allow zero-value denominators:
			dzero=nzero=ifelse(drSpp[[d]][2]=="landed",TRUE,FALSE)
#browser();return()
			DRAT=calcRatio(discat,drSpp[[d]][1],drSpp[[d]][2],major=j,dzero=dzero,quiet=TRUE)
			if (is.null(DRAT)) next
			drat=apply(DRAT[is.element(rownames(DRAT),as.character(dyrs)),,drop=FALSE],2,mean,na.rm=TRUE)
			drat=drat[intersect(names(drat),jj)]
			dfac[names(drat),kk,dd]=drat 
		}
		if (any(k==c(1,5)))
			dfac[jj,kk,"dr"]=dfac[jj,kk,"discard:landed"]
		if (any(k==c(2,3,4)))
			dfac[jj,kk,"dr"]=dfac[jj,kk,"discard:TAR"]
	}
	assign("PBStool",PBStool,envir=.PBStoolEnv); rm(PBStool) ### restore to .PBStoolEnv and remove local copy
	save("ologs",file=paste("ologs",strSpp,".rda",sep=""))   ### save observerlogs  with discard information
	dfac[is.na(dfac)] = 0; delta = dfac
	if (diagnostics){
		for (rate in dimnames(delta)$rate) {
			rr = sub(":","2",rate)
			plotData(delta[,,rate],paste("delta ",rr," (fishery in major)",sep=""),col=clrs.fishery,type="bars")
			plotData(t(delta[,,rate]),paste("delta ",rr," (major in fishery)",sep=""),col=clrs.major[mm],type="bars")
	}	}
	if (saveinfo)
		packList(c("alpha","beta","rtar","gamma","delta"),"PBStool",tenv=.PBStoolEnv)

	fidnam=c("trawl","halibut","sablefish","sched2","zn","sabzn","sabhal","dogfish","lingcod","combined")
	fshnam=c("trawl","h&l","trap",rep("h&l",6),"combined")  ### general category vector
#browser();return()

	### -------------------------------------------------
	### 4. Allocate the ancient rockfish catch by unknown
	###    gear type to RRF by fishery.
	### -------------------------------------------------

	### Get sector allocation for very early series from sales slip data (Obradovich)
	ancyrs=1918:1950; prewar=ancyrs[1]:1938; postwar=1939:rev(ancyrs)[1]
	gear=c("h&l","trap","trawl")
	epoch=c("prewar","postwar")
	cobra=htab[as.character(1951:1952),mm,"Obradovich","max",gear,"ORF"]
	major.gear=t(apply(apply(cobra,2:3,sum),1,function(x){if (all(x==0)) rep(0,length(x)) else x/sum(x)}))

	### lambda – Proportion of early catch by general gear type
	lambda=array(0,dim=c(dim(major.gear),2),dimnames=list(major=mm,gear=gear,epoch=epoch))
	lambda[,"h&l","prewar"]=0.9; lambda[,"trap","prewar"]=0; lambda[,"trawl","prewar"]=0.1
	lambda[rownames(major.gear),colnames(major.gear),"postwar"]=major.gear
	if (diagnostics){
		for (epo in dimnames(lambda)$epoch) {
			plotData(lambda[,,epo],paste("lambda ",epo," (gear in major)",sep=""),col=clrs.fishery,type="bars")
			plotData(t(lambda[,,epo]),paste("lambda ",epo," (major in gear)",sep=""),col=clrs.major[mm],type="bars")
	}	}

	ancient=array(0,dim=c(length(ancyrs),length(mm),length(fid)),
		dimnames=list(year=ancyrs,major=mm,fid=fid))

	oldies=sapply(epoch,function(x){get(x)},simplify=FALSE)
	for (i in names(oldies)) {
		oldyrs=oldies[[i]]
		ii=as.character(oldyrs); jj=mm
		L245=lambda[,"h&l",i] * beta  ### expand h&l contribution to fid (2,4,5)
		LAMBDA=cbind(lambda[,"trawl",i],L245[,"2"],lambda[,"trap",i],L245[,c("4","5")]) ### prop. combined ORF by major and fid
		dimnames(LAMBDA)[[2]]=fid
		gamma.lambda = gamma * LAMBDA ### prop. of combined ORF comprising RRF by PMFC and FID
		### Partition the 'combined' rockfish catch to fids
		for (k in fid) { 
			kk=as.character(k)
			decon=t(apply(orfhis[ii,jj,"combined"],1,function(x,gala){x*gala},gala=gamma.lambda[,kk]))
			ancient[ii,jj,kk]=ancient[ii,jj,kk] + decon
		}
	}
	if (diagnostics){
		plotData(apply(ancient,c(1,2),sum),"ancient in major",col=clrs.major[mm])
		plotData(apply(ancient,c(1,3),sum),"ancient in fishery",col=clrs.fishery)
	}
	if (saveinfo)
		packList(c("cobra","lambda","gamma.lambda","fidnam","fshnam","ancient"),"PBStool",tenv=.PBStoolEnv)

	### ----------------------------
	### 5. Reconstruct the RRF catch
	### ----------------------------

	ALLYRS=sort(union(HISYRS,MODYRS)); nyrs=length(ALLYRS)
	sppnew=array(0,dim=c(nyrs,length(mm),length(fid)+1),
		dimnames=list(year=ALLYRS,major=mm,fid=c(fid,10))) ### initilaize the catch array for the reconstruction

	### Add the ancient RRF
	sppnew[as.character(ancyrs),mm,as.character(fid)]=ancient

	### Allocation matrix from K to k
	BETA=cbind(rep(1,length(mm)),beta[,"2"],rep(1,length(mm)),beta[,c("4","5")])
	dimnames(BETA)[[2]]=fid
	beta.gamma = BETA * gamma  ### Expansion of RRF:ORF from K to k
	names(dimnames(beta.gamma)) = c("major","fid")
	if (diagnostics){
		plotData(beta.gamma,"beta.gamma (fishery in major)",col=clrs.fishery,type="bars")
		plotData(t(beta.gamma),"beta.gamma (major in fishery)",col=clrs.major[mm],type="bars")
	}
	
	for (k in fid) { 
		kk=as.character(k)
		### Reconstruct catch history form data with gear type K.
		### Years to estimate landings of 'strSpp' from ORF/TRF
		estYRS=HISYRS
		if (strSpp=="396" && k==1) {
			### POP trawl catch relatively well known back to 1956
			estYRS=HISYRS[1]:1955; useYRS=1956:rev(HISYRS)[1] } 
			#estYRS=HISYRS[1]:1976; useYRS=1977:rev(HISYRS)[1] } # if estimating foreign catch
		if (any(strSpp==c("442","396")) && any(k==c(4,5))) {
			### YYR H&L catch relatively well known back to 1982
			estYRS=HISYRS[1]:1981; useYRS=1982:rev(HISYRS)[1] } 
		ii=as.character(estYRS)
		recon=t(apply(orfhis[ii,,fshnam[k]],1,function(x,bega){x*bega},bega=beta.gamma[,kk]))

		### Combine estimated RRF landings with reported RRF landings
		if (strSpp=="396" && k==1) { 
			rawcat=rawhis[as.character(useYRS),,fshnam[k]]
			recon=rbind(recon,rawcat) }
		if (any(strSpp==c("442","396")) && any(k==c(4,5))) {
			rawcat=catmod[as.character(useYRS),jj,kk,"landed"]
			recon=rbind(recon,rawcat) }
		ii=dimnames(recon)[[1]]; jj=dimnames(recon)[[2]]
		sppnew[ii,jj,kk] = sppnew[ii,jj,kk] + recon[ii,jj,drop=FALSE]
		
		### Mesh modern RRF landings with reconstructed RRF landings
		modern = catmod[,jj,kk,"landed"]
		imod   = dimnames(modern)[[1]]
		zmod   = (modern - sppnew[imod,jj,kk]) > 0
		sppnew[imod,jj,kk][zmod] = modern[zmod]   ### use maximum values

		### Add in the RRF discards
		### discard rate - either RRF discard:RRF landed or RRF discard:TAR landed
		dr=delta[,kk,"dr"]
		### inone = years when RRF discards assumed reported in landings
		### icalc = years when RRF discards calculated by rates
		### idata = years when RRF discards reported as data
		discard.regimes = switch( k,
			list(inone=ALLYRS[1]:1953, icalc=1954:1995, idata=1996:ALLYRS[nyrs]),  ### trawl
			list(inone=ALLYRS[1]:1978, icalc=1979:2005, idata=2006:ALLYRS[nyrs]),  ### halibut
			list(inone=ALLYRS[1]:1985, icalc=1986:2005, idata=2006:ALLYRS[nyrs]),  ### sablefish
			list(inone=ALLYRS[1]:1985, icalc=1986:2005, idata=2006:ALLYRS[nyrs]),  ### schedule II
			list(inone=ALLYRS[1]:1985, icalc=1986:2005, idata=2006:ALLYRS[nyrs]))  ### ZN rockfish
		unpackList(sapply(discard.regimes,as.character))

		### Calculate/retrieve the RRF discards
		if (any(k==c(1,5))) {
			icalc=intersect(dimnames(sppnew)[[1]],icalc)
			kcalc=sppnew[,,kk] }
		if (any(k==c(2,3,4))) {
			icalc=intersect(dimnames(catmod)[[1]],icalc)
			kcalc=catmod[,,kk,"TAR"] }
		kcalc = kcalc[icalc,jj,drop=FALSE] 
		disC  = t(apply(kcalc,1,function(x,dr){x*dr},dr=dr)) 
		idata = intersect(dimnames(catmod)[[1]],idata)
		disD  = catmod[,,kk,"discard"]
		disD  = disD[idata,jj,drop=FALSE]
#if (k==2) {browser();return()}

		### Add in the RRF discards
		sppnew[icalc,jj,kk]  = sppnew[icalc,jj,kk]   + disC[icalc,jj]
		sppnew[idata,jj,kk]  = sppnew[idata,jj,kk]   + disD[idata,jj]
		if (diagnostics){
			plotData(recon,paste("recon for ",fidnam[k]," by major",sep=""),col=clrs.major[mm])
			plotData(modern,paste("modern for ",fidnam[k]," by major",sep=""),col=clrs.major[mm])
			plotData(disC,paste("disC for ",fidnam[k]," by major",sep=""),col=clrs.major[mm])
			plotData(disD,paste("disD for ",fidnam[k]," by major",sep=""),col=clrs.major[mm])
			plotData(sppnew[,,kk],paste("sppnew for ",fidnam[k]," by major",sep=""),col=clrs.major[mm])
		}
	}
	sppnew[,,"10"] = apply(sppnew[,,as.character(fid)],1:2,sum)  ### sum across fisheries
	if (diagnostics){
		plotData(sppnew[,,"10"],paste("sppnew for ",fidnam[10]," by major",sep=""),col=clrs.major[mm])
		collectFigs(path="./CRdiag",width=5,capskip=-20,is.fnum=TRUE)
	}
	expr=paste("cat",strSpp,"rec=sppnew; save(\"cat",strSpp,"rec\",file=\"cat",strSpp,"rec.rda\")",sep="")
	eval(parse(text=expr))

	if (saveinfo) packList(c("HISYRS","MODYRS","ALLYRS","inone","icalc","idata",
		"disC","disD","sppnew","beta.gamma"),"PBStool",tenv=.PBStoolEnv)
#browser(); return()

	###-----Plot results-----
	for (k in fidout) {
		yrs = as.numeric(dimnames(sppnew)$year)
		plotRecon(sppnew,strSpp=strSpp,major=major,fidout=k,years=yrs,eps=eps,pix=pix,wmf=wmf)
	}
	fidlab=c("Trawl","Halibut","Sablefish","Dogfish-Lingcod","H&L Rockfish","Sablefish + ZN",
		"Sablefish + Halibut","Dogfish","Lingcod","Combined Fisheries")
	data(pmfc,species,envir=penv())

	### ADMB catch data file for the combined fishery
	admdat = paste("admb-cat",strSpp,".dat",sep="")
	cat(paste("# Catch History",species[strSpp,"latin"],sep=" - "),"\n\n",sep="",file=admdat)
	mess = c(
		"# number of years of catch data 'NYearCat'\n", ALLYRS[nyrs]-ALLYRS[1]+1, "\n",
		"# start year 'tilde{t}' in model\n", ALLYRS[1], "\n",
		"# end year\n", ALLYRS[nyrs], "\n",
		"# number of areas\n", length(mm), "\n",
		"# areas (column headings of matrix), areas 5, 6 and 7 give QCS.\n",
		paste(mm,collapse="   "), "\n\n",
		"# catch history in tonnes (all fisheries combined)\n",
		"#  first column lists the years, each row gives the\n",
		"#  catch in corresponding area that year. Size is therefore\n",
		"#  (end year - start year + 1) * (number of areas + 1)\n\n")
	cat(paste(mess,collapse=""),file=admdat,append=TRUE)
	for (i in 10){
		ii = as.character(i)
		cat("# ",fidlab[i],"\n",sep="",file=admdat,append=TRUE)
		mess = cbind(year=as.numeric(rownames(sppnew)),
			apply(sppnew[,,ii],1,function(x){
			paste(show0(format(round(x,3),scientific=FALSE,width=12,justify="right"),3,add2int=TRUE),collapse="") } ) )
		cat(paste(apply(mess,1,paste,collapse=""),collapse="\n"),sep="",file=admdat,append=TRUE)
		cat("\n\n",file=admdat,append=TRUE) }

	### Output specified FID catch (fidout) as CSV
	onam=paste("Catch-History-",strSpp,".csv",sep="")  ### output file name
	cat(paste("Catch History",species[strSpp,"latin"],sep=" - "),"\n",sep="",file=onam)
	#warn=options()$warn; options(warn=-1)
	xlab=dimnames(sppnew)[[1]];  xpos=(1:length(xlab))-.5
	for (i in fidout){
		ii=as.character(i)
		cat(fidlab[i],"\n",sep="",file=onam,append=TRUE)
		cat("year,",paste(colnames(sppnew),collapse=","),"\n",sep="",file=onam,append=TRUE)
		apply(cbind(year=rownames(sppnew),sppnew[,,ii]),1,function(x){cat(paste(x,collapse=","),"\n",sep="",file=onam,append=TRUE)})
		cat("\n",file=onam,append=TRUE)
	}
	#options(warn=warn)
	if (saveinfo) {
		packList(c("plotname","clrs.major","clrs.fishery","fidlab"),"PBStool",tenv=.PBStoolEnv)
		ttget(PBStool)
		save("PBStool",file=paste("PBStool",strSpp,".rda",sep="")) }
	collectFigs(path=".",ext="eps",fout=paste("Catch-Recon-Summary-",strSpp,sep=""),width=6.75,pattern="Catch-History")
#browser();return()	
	invisible(sppnew) }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~buildCatch


#plotData-------------------------------2013-01-28
# Plot diagnostic data for catch reconstructions.
#-----------------------------------------------RH
plotData =function(x,description="something",
	col=c("red","coral","gold","green2","skyblue","blue","blueviolet","purple4"),...)
{
	if (dev.cur()>1) { oldpar=par(no.readonly=TRUE); on.exit(par(oldpar)) }
	dots=list(...)
	if (!is.matrix(x) & !is.data.frame(x)) {
		stop(cat(paste(deparse(substitute(x)), "is not a matrix or a data frame.\n")))
	}
	xlab = names(dimnames(x))[1]; zlab=names(dimnames(x))[2]; ylab=paste("Value for each ",zlab,sep="")
	xnam = dimnames(x)[[1]]
	xchr = strsplit(xnam,split="")
	if (all(sapply(xchr,function(x){all(x%in%c(as.character(0:9),"."))})))
		xval=as.numeric(xnam)
	else xval = 1:length(xnam)
	#xval = as.numeric(dimnames(x)[[1]]); 
	xlim=range(xval); ylim = range(x)
	if (all(x==0)) return(invisible())
	xx = x; xx[xx==0] = NA
	cnam = colnames(x); nc = length(cnam) # column names
	col =rep(col,nc)[1:nc]; names(col) = cnam
	pD = ttcall(PBStool)$pD
	eps= ttcall(PBStool)$eps
	wmf= ttcall(PBStool)$wmf
	if (!eps & !wmf) eps=TRUE
	plotname = paste("CRdiag/pD",pad0(pD,3),"-",gsub(" ","-",description),sep="")
#browser();return()
	if (eps) 
		postscript(file=paste(plotname,".eps",sep = ""), width=6.5,height=5,paper="special")
	else if (wmf)
		do.call("win.metafile",list(filename=paste(plotname,".wmf",sep = ""), width=6.5,height=5))
	expandGraph()
	if (!is.null(dots$type) && dots$type=="bars") {
		xpos = barplot(t(xx),beside=TRUE,col=col,ylim=ylim,xlab=xlab,ylab=ylab,space=c(0,1.5))
		xsum = apply(t(x),2,max,na.rm=TRUE); xadj=0.5
		xval = apply(xpos,2,median)
	} else {
		plot(0,0,type="n",xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab)
		sapply(1:nc,function(cpos,xtab,xval,clrs){
			cval = cnam[cpos]
			lines(xval,xtab[,cval],lwd=2,col=clrs[cval])
			}, xtab=xx, xval=xval,clrs=col)
		xsum = apply(x,1,max,na.rm=TRUE); xadj=0
	}
	xmin = is.element(xsum,min(xsum))
	zuse = apply(x,2,sum)>0  # which columns have data
	#legend("topleft",col=col[zuse],lwd=2,legend=cnam[zuse],inset=0.025,bty="n",title=zlab)
	legend(xval[xmin][1],par()$usr[4],col=col[zuse],lwd=2,legend=cnam[zuse],bty="n",title=zlab,xjust=xadj)
	if (eps|wmf) dev.off()
	#eval(parse(text="PBStool$pD <<- pD +1"))
	ttget(PBStool); PBStool$pD <- pD + 1; ttput(PBStool)  # increment number of diagnostic plot
#browser();return()
	invisible() }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotData


#plotRecon------------------------------2011-06-15
# Plot reconstructed catch using barplots stacked by PMFC area.
#-----------------------------------------------RH
plotRecon = function(dat=cat440rec, strSpp="440", major=c(1,3:9), fidout=10, 
   years=1918:2011, xlab=seq(1920,2010,5),
   eps=FALSE, pix=FALSE, wmf=FALSE, PIN=c(10,5))
{
	if (dev.cur()>1) { oldpar=par(no.readonly=TRUE); on.exit(par(oldpar)) }
	fshnam=c("trawl","h&l","trap",rep("h&l",6),"combined") #general category vector
	fidnam=c("trawl","halibut","sablefish","sched2","zn","sabzn","sabhal","dogfish","lingcod","combined")
	fidlab=c("Trawl","Halibut","Sablefish","Dogfish-Lingcod","H&L Rockfish","Sablefish + ZN",
		"Sablefish + Halibut","Dogfish","Lingcod","Combined Fisheries")
	yy  = as.character(years)
	dat = dat[yy,,,drop=FALSE]
	MAJ = as.character(1:10); mm=as.character(major)
	clrs = rep("gainsboro",length(MAJ)); names(clrs)=MAJ
	clrs[as.character(c(1,3:9))]=c("moccasin","blue","lightblue","yellow","orange","red","seagreen","lightgreen")
	mclrs=clrs[mm]
	data(pmfc,species,envir=penv())
	XLAB=dimnames(dat)[[1]];  xpos=(1:length(XLAB))-.5; zlab=is.element(XLAB,xlab)
	for (i in fidout){
		ii=as.character(i)
		plotname=paste(strSpp,"-Catch-History-",fidnam[i],"-years(",min(years),"-",max(years),")-major(",paste(major,collapse=""),")",sep="")
		if (eps)       postscript(file=paste(plotname,".eps",sep=""),width=PIN[1],height=PIN[2],fonts="mono",paper="special")
		else if (pix)  png(filename=paste(plotname,".png",sep=""),width=round(100*PIN[1]),height=round(100*PIN[2]))
		else if (wmf)  do.call("win.metafile",list(filename=paste(plotname,".wmf",sep=""),width=PIN[1],height=PIN[2]))
		else  resetGraph()
		expandGraph(mar=c(3,3,.5,.5))
		barplot(t(dat[,mm,ii]),col=0,space=0,xaxt="n",yaxt="n",xaxs="i")
		yaxp=par()$yaxp; yint=yaxp[2]/yaxp[3]; hlin=seq(yint,yaxp[2],yint)
		segments(rep(0,length(hlin)),hlin,rep(par()$usr[2],length(hlin)),hlin,col="gainsboro")
		barplot(t(dat[,mm,ii]),col=mclrs,space=0,cex.names=.8,mgp=c(1.5,.5,0),xaxt="n",xaxs="i",add=TRUE)
		axis(1,at=xpos[zlab],labels=XLAB[zlab],tick=FALSE,las=3,mgp=c(0,.2,0),cex.axis=.8,hadj=1)
		legend("topleft",fill=rev(mclrs),legend=paste("PMFC",rev(pmfc[mm,"gmu"])),
			bty="n", cex=0.8, xjust=0, inset=c(.04,.20))
		addLabel(.05,.95,species[strSpp,"latin"],font=3,cex=1,col="#400080",adj=0)
		addLabel(.05,.90,fidlab[i],cex=1.2,col="#400080",adj=0)
		mtext("Year",side=1,line=1.75,cex=1.2)
		mtext("Catch (t)",side=2,line=2,cex=1.3)
		if (eps|pix|wmf) dev.off()
	}
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotRecon


#surveyCatch----------------------------2014-07-03
# Query GFBioSQL for survey catch and summarise
# catches by year and PMFC area.
#-----------------------------------------------RH
surveyCatch = function(strSpp="396", spath=.getSpath())
{
	if (isThere("PBSdat")) rm(PBSdat) ### remove from current environment
	### GFBioSQL catch summary for surveys
	getData("gfb_rcatORF.sql",dbName="GFBioSQL",strSpp=strSpp,path=spath,,tenv=penv())
	assign("gfbdat",PBSdat);  rm(PBSdat) ### just to be safe
	gfbtab = crossTab(gfbdat,c("year","major"),"catKg") # summarise catch (t)
	gfbcat = as.data.frame(gfbtab[,-1])
	row.names(gfbcat) = gfbtab[,1]
	data(pmfc,package="PBSdata",envir=penv())
	names(gfbcat) = pmfc[names(gfbcat),"gmu"]
	save("gfbdat",file="gfbdat.rda")
	save("gfbcat",file="gfbcat.rda")
	write.csv(gfbcat,file=paste("Catch-Survey-",strSpp,".csv",sep=""))
	invisible(gfbcat)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~surveyCatch

#===============================================================================
#5RF
# Splitnose (412), Greenstriped (414), Redstripe (439), Harlequin (446), Sharpchin (450)
#x=buildCatch(sql=T,strSpp="414",wmf=T,pwd=c(pwd1,pwd2))
#x=buildCatch(cat412orf,strSpp="412",wmf=T,pwd=c(pwd1,pwd2))

# LST - Longspine thornyhead
#x=buildCatch(sql=T,strSpp="453",wmf=T,pwd=c(pwd1,pwd2))
#x=buildCatch(cat453orf,strSpp="453",wmf=T)

# POP - Pacific Ocean Perch
#x=buildCatch(sql=TRUE,strSpp="396",dfld="TRF",wmf=TRUE,pwd=c(pwd1,pwd2))
#x=buildCatch(cat396orf,strSpp="396",dfld="TRF",wmf=TRUE)
#x=buildCatch(cat396orf,strSpp="396",dfld="TRF",wmf=TRUE) #---not good

# QBR - Quilback rockfish
#x=buildCatch(sql=T,strSpp="424",dfld="ORF",wmf=T,pwd=c(pwd1,pwd2))
#x=buildCatch(cat424orf,strSpp="424",dfld="ORF",wmf=T)

# RBR - Redbanded Rockfish
#x=buildCatch(sql=TRUE,strSpp="401",dfld="ORF",only.sql=TRUE)
#x=buildCatch(cat401orf,strSpp="401",dfld="ORF",eps=TRUE)
#y=surveyCatch(strSpp="401")

# RER - Rougheye rockfish
#x=buildCatch(sql=T,strSpp="394",wmf=T,pwd=c(pwd1,pwd2))
#x=buildCatch(cat439orf,strSpp="394",wmf=T)

# SGR - Silvergrey Rockfish
#x=buildCatch(sql=TRUE,strSpp="405",dfld="ORF",wmf=FALSE,pwd=c(pwd1,pwd2),only.sql=TRUE)
#x=buildCatch(cat405orf,strSpp="405",dfld="TRF",eps=TRUE)

# YMR - Yellowmouth Rockfish
#x=buildCatch(sql=T,strSpp="440",dfld="TRF",wmf=T,pwd=c(pwd1,pwd2))
#x=buildCatch(cat440orf,strSpp="440",dfld="TRF",wmf=T)

# YTR - Yellowtail Rockfish
#x=buildCatch(sql=TRUE,strSpp="418",dfld="ORF",only.sql=TRUE)
#x=buildCatch(cat418orf,strSpp="418",dfld="ORF",eps=TRUE ,pjs=TRUE,outside=TRUE)
#y=surveyCatch(strSpp="418")

# YYR - Yelloweye Rockfish
#x=buildCatch(sql=TRUE,strSpp="442",dfld="ORF",only.sql=TRUE)
#x=buildCatch(cat442orf,strSpp="442",dfld="ORF",eps=TRUE)
#y=surveyCatch(strSpp="442")

