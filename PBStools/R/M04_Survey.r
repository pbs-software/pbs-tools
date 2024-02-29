##==============================================================================
## Module 4: Survey
## ----------------
##  bootBG..........Bootstraps binomial-gamma variates from (p, mu, rho) for each stratum.
##  calcMoments.....Calculate survey strata population moments from raw survey data.
##  calcPMR.........Calculate pmr parameter values from a sample.
##  getBootRuns.....Get Norm's survey bootstrap results.
##  getPMR..........Get pmr values from survey data in GFBioSQL.
##  makePMRtables...Make CSV files containg pmr values for survey strata.
##  makeSSID........Make a data object of survey series information.
##  sampBG..........Sample from the binomial-gamma distribution.
##  showAlpha.......Show quantile confidence levels (alpha) for bootstrapped biomass.
##  showIndices.....Show survey indices from Norm's bootstrap tables.
##  simBGtrend......Simulate a population projection based on prior binomial-gamma parameters.
##  trend...........Simple trend analysis for annnual IPHC indices.
##==============================================================================

#bootBG---------------------------------2015-03-06
# Bootstraps binomial-gamma variates from (p, mu, rho) for each stratum.
# Arguments:
#  dat : Data file containing p, mu, rho, A, k for each stratum h, where
#    h = strata ID (alpha and/or numeric)
#    p = proportion of zero measurements
#   mu = mean of nonzero measurements
#  rho = CV of nonzero measurements
#    A = habitat area (km.sq)
#    n = number of old tows performed to derive (p,mu,rho)
#    k = preference of new tows
#    K : Total tow budget for the new survey
#    S : Number of samples for simulation
#    R : Number of bootstraps for simulation
#   ci : Confidence intervals for simulation 
#        e.g., ci=c(.90,.95) gives confidence limits (.025,.05,.95,.975)
# lims : Bootstrap confidence limit types to compute
# allo : Tow allocation scheme 0=existing, 1=optimal
#-----------------------------------------------RH
bootBG <- function (dat="pop.pmr.qcss", K=100, S=100, R=500, SID=NULL, 
	ci=seq(0.1,0.9,0.1), lims=c("emp","norm","basic","perc","bca"), allo=0, ioenv=.GlobalEnv) {
	#Subfunctions--------------
	cmatrix=function(x,cc) {
		dnam=dimnames(x); rnam=dnam[[1]]; cnam=dnam[[2]]
		if (all(is.numeric(cc))) cnam=cnam[cc]
		else cnam=cnam[match(cc,cnam)]
		matrix(x[,cc],nrow=dim(x)[1],dimnames=list(rnam,cnam)) }
	mysum=function(x,f){
		z=!is.na(x)
		sum(f[z]*x[z])}
	#--------------------------
	assign("PBStool",list(module="M04_Survey",call=match.call(),args=args(bootBG),ioenv=ioenv),envir=.PBStoolEnv)

	if (!requireNamespace("boot", quietly=TRUE)) stop("`boot` package is required")
	snam=as.character(substitute(dat))
	if (length(snam)==0 || snam=="") showError("Supply a file name or \n run 'getPMR' to create the file.")

	txt=paste("getFile(\"",snam,"\",senv=ioenv,use.pkg=TRUE,try.all.frames=TRUE,tenv=penv()); dat=",snam,sep="")
	eval(parse(text=txt))
	flds=names(dat)
	if (any(flds=="SID") && !is.null(SID)) dat=dat[is.element(dat$SID,SID),]
	m=nrow(dat) # number of strata
	hhh=dat$h; 
	if(any(duplicated(sort(hhh)))) showError("Strata ID not unique")
	h=order(hhh); hh=dimnames(dat)[[1]]=as.character(hhh)  # strata ID
	names(h)=hh
	dat$k=dat$k/sum(dat$k) # standardize k to sum to 1 (treat k as a preferential weight rather than a cost)
	z0=dat$p==1
	ins <- c("h","p","mu","rho","A","k")  # parameter inputs
	mom <- c("nu","delta","sigma","B","n","V","CV","n*","V*","CV*")  # moment estimates calculated (except 'n')
	qint <- round(ci,3);  nqnt=2*length(qint)
	clim=(cbind(-qint,qint)+1)/2; qnt=sort(as.vector(clim))
	LIMS=c("emp","normal","basic","percent","bca")
	names(LIMS)=c("emp","norm","basic","perc","bca") # generally available

	Sord  <- ceiling(log10(S))
	if (Sord==log10(S)) Sord=Sord+1
	BS    <- paste("B",pad0(1:S,Sord),sep="")
	dS    <- paste("d",pad0(1:S,Sord),sep="")

	# create array of parameters and moment estimates by stratum
	x <- array(NA,dim=c(m,length(ins)+length(mom)),dimnames=list(hh,c(ins,mom)))
	names(dimnames(x)) <- c("h","stat")
	# create array of sampled densities by stratum
	y <- array(NA,dim=c(m,S),dimnames=list(hh,dS)); names(dimnames(y)) <- c("h","dS")
	# create array of sampled biomass by stratum
	z <- array(NA,dim=c(m,S),dimnames=list(hh,BS)); names(dimnames(z)) <- c("h","BS")
	# create matrix of B-quantiles by stratum
	Bqnt <- array(NA,dim=c(length(BS),nqnt+4,length(lims)),dimnames=list(BS,c(qnt,"Bsamp","Qsamp","Btrue","Qtrue"),lims))
	names(dimnames(Bqnt)) <- c("B","q","lim")
	add=attributes(dat)[c("spp","survey","year","fqt")]
	attributes(Bqnt)=c(attributes(Bqnt),add)
	attr(Bqnt,"SID")=SID

	x[hh,c(ins,"n")]  <- as.matrix(dat[hh,c(ins,"n")],nrow=m) # copy input parameters to output array
	x[hh,"nu"] <- 1/(x[hh,"rho"]^2)   # calculate nu from rho

	x[,"delta"]=(1-x[,"p"])*x[,"mu"]                                                #(T4.1) Density
	x[,"sigma"]=sqrt((1-x[,"p"])*(1+x[,"nu"]*x[,"p"])*(x[,"mu"]^2/x[,"nu"]))        #(T4.2)
	x[,"B"]    =(1-x[,"p"])*x[,"mu"]*x[,"A"]                                        #(11)
	x[,"B"][z0]=0 # when p=1
	x[,"V"]    =(1/x[,"n"])*(x[,"rho"]^2+x[,"p"])*(1-x[,"p"])*x[,"mu"]^2*x[,"A"]^2  #(12)
	x[,"CV"]   =sqrt((x[,"rho"]^2+x[,"p"])/(x[,"n"]*(1-x[,"p"])))                   #(13)
	X          =sum(x[,"A"]*x[,"sigma"]*sqrt(x[,"k"]),na.rm=TRUE)                      #(T4.9)
	x[,"n*"]   =switch(allo+1,
		pmax(1,round((K/sum(x[,"n"]))*x[,"n"])),
		pmax(1,round(((K*x[,"A"]*x[,"sigma"])/(X*sqrt(x[,"k"])))*x[,"k"])))          #(T4.10) modified to treat k as a preferential weight)
	K=sum(x[,"n*"]) # to account for rounding
	x[,"V*"]   =(x[,"A"]^2*x[,"sigma"]^2)/x[,"n*"]                                  #(T4.7)
	x[,"CV*"]  =sqrt(x[,"V*"])/x[,"B"]                                              #(13)

	if (S>9) cat("\n",paste(S,"samples: \n"))
	Btrue <- sum(x[,"B"],na.rm=TRUE)
	A     <- x[hh,"A"]; names(A)=h
	n     <- x[hh,"n*"]; names(n)=h # allocation used for simulation
	Bboot.all <- as.list(1:S); names(Bboot.all) <- BS

	for (s in 1:S) {
		if (round(s%%10)==0) { cat((s-9):s,"\n"); flush.console() }
		ds    <- paste("d",pad0(s,Sord),sep="")
		Bs    <- paste("B",pad0(s,Sord),sep="")
		bgsamp=apply(x,1,function(xx){
			sampBG(xx["n*"],xx["p"],xx["mu"],xx["rho"]) })  #(T3.1)
		bgmean=sapply(bgsamp,mean)                          #(T4.4)
		y[names(bgmean),ds]=bgmean
		group=rep(h[names(bgmean)],sapply(bgsamp,length))
		z[,Bs] <- x[,"A"]*y[,ds]                            #(T4.5)
		zvars=unlist(bgsamp)
		names(zvars)=index=as.character(group)

		Bhi   <- zvars[is.element(names(zvars),index)] * (A[index] / n[index]) # factor out n before summing
		Bboot <- boot::boot(data=Bhi,statistic=mysum,R=R,stype="f",strata=group)
		Bboot.all[[Bs]] = Bboot # collect the bootstrap results
		if (all(diff(Bhi)==0)) {
			for (i in lims) {
				Bqnt[Bs,as.character(qnt),i]=Bhi[1]
				Bqnt[Bs,"Bsamp",i] <- R*Bhi[1]
				Bqnt[Bs,"Btrue",i] <- Btrue
				Bqnt[Bs,"Qtrue",i] <- ifelse(Btrue<Bhi[1],0,1) }
		} else {
			L=GT0(boot::empinf(data=Bhi,statistic=mysum,stype="f",strata=group))
			Bconf <- boot::boot.ci(boot.out=Bboot,conf=qint,type=setdiff(lims,"emp"),L=L)
			for (i in lims) {
				if (i=="emp") {
					Blims=quantile(Bboot$t,qnt)
					Bqnt[Bs,as.character(qnt),i]=Blims
				} else {
					bconf=Bconf[[LIMS[i]]]
					if (i=="norm") cc=2:3 else cc=4:5
					Blims=cbind(t(t(bconf[,1])),clim,cmatrix(bconf,cc))
					Bqnt[Bs,as.character(Blims[,2]),i]=Blims[,4]
					Bqnt[Bs,as.character(Blims[,3]),i]=Blims[,5]  }
				Bqnt[Bs,"Bsamp",i] <- sum(z[,Bs],na.rm=TRUE)
				Bqnt[Bs,"Qsamp",i] <- approx(Bqnt[Bs,1:nqnt,i],qnt,Bqnt[Bs,"Bsamp",i],yleft=5e-5,yright=1,rule=2,ties="ordered")$y
				Bqnt[Bs,"Btrue",i] <- Btrue
				Bqnt[Bs,"Qtrue",i] <- approx(Bqnt[Bs,1:nqnt,i],qnt,Btrue,yleft=.001,yright=1,rule=2,ties="ordered")$y
			} }
	} # end S loop
	Best <- apply(cmatrix(z,BS),2,sum) # B estimated from random binomial gamma
	bgtab=x; dStab=y; BStab=z; Bboot=Bboot.all; Bobs=Btrue
	packList(c("dat","bgtab","dStab","BStab","bgsamp","Bboot","Bqnt","Best","Bobs","group"),"PBStool",tenv=.PBStoolEnv)
	invisible() }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~bootBG

#calcMoments----------------------------2010-06-02
# Calculate survey strata population moments,
# including relative biomass, from raw survey data.
#--------------------------------------------NO/RH
calcMoments = function(strSpp="396", survID=c(1,2,3,121,167)) {
	#Subfunctions------------------------
	calcBio=function(dat,i,sa,Bonly=TRUE) {
		if (length(i)==1) i = 1:nrow(dat)
		dat <- dat[i,] # Resample the data frame
		# Mean biomass (kg/km2) per stratum h
		h=sa$GROUPING_CODE; hh=as.character(h)
		mu = numeric(nrow(sa)); names(mu)=h  #initialize to 0
		Ah=sa$AREA_KM2; names(Ah)=h; A=sum(Ah)
		mb = tapply(dat$DENSITY_KGPM2 * 1000, list(dat$GROUPING_CODE), mean)
		mu[names(mb)] = mb
		# Expand mean biomass to the survey area
		Bh = mu[hh] * Ah[hh]; B = sum(Bh)
		if (Bonly) return(sum(B))

		Vh = numeric(nrow(sa)); names(Vh)=h; Nh=Vh #initialize to 0
		x = tapply(dat$GROUPING_CODE, dat$GROUPING_CODE, length) 
		Nh[names(x)] = x; N=sum(Nh)
		# Variance of biomass per stratum
		bv <- tapply(dat$DENSITY_KGPM2 * 1000, dat$GROUPING_CODE, var)
		bv <- bv[!is.na(bv)] # Have to remove NAs resulting when only 1 obs/stratum
		Vh[names(bv)] = bv; V=sum(Vh)
		# Unbiased estimate of the variance
		#Vhat = sum((Ah/A)^2 * ((Ah - Nh)/Ah) * (Vh/Nh))
		CVh=sqrt(Vh)/Bh; CV=sqrt(V)/B
		Bmat=data.frame(B=c(Bh,B),N=c(Nh,N),V=c(Vh,V),sig=sqrt(c(Vh,V)),CV=c(CVh,CV))
		row.names(Bmat)[nrow(Bmat)]="Total"
		noTows=Bmat$N==0
		if (any(noTows==TRUE)) Bmat[noTows,c("B","V","sig","CV")]=NA
		noB=Bmat$B==0
		if (any(noB==TRUE)) { Bmat[noB,"CV"]=NA; Bmat["Total","CV"]=sum(Bmat$sig[!noB])/sum(Bmat$B[!noB]) }
		Bmat$CV=Bmat$CV*100
		return(Bmat) }
	#------------------------Subfunctions
	assign("PBStool",list(module="M04_Survey",call=match.call(),args=args(calcMoments)),envir=.PBStoolEnv)
	getData("BOOT_DEFAULTS","GFBioSQL",tenv=penv())
	doors=PBSdat$doorspread; names(doors)=PBSdat$SURVEY_ID
	speed=PBSdat$speed;      names(speed)=PBSdat$SURVEY_ID
	moments=list()
	for (i in survID) {
		ii=as.character(i)
		getData("gfb_survey_stratum.sql","GFBioSQL","xxx",path=.getSpath(),surveyid=i,tenv=penv());  sa=PBSdat
		getData("gfb_survey_catch.sql","GFBioSQL",strSpp,path=.getSpath(),surveyid=i,
			doors=doors[ii],speed=speed[ii],tenv=penv()); dat=PBSdat
		moments[[ii]]=calcBio(dat,i,sa,Bonly=FALSE)
	}
	packList(c("doors","speed","moments"),"PBStool",tenv=.PBStoolEnv)
	invisible(moments)
}
#--------------------------------------calcMoments


## calcPMR------------------------------2018-03-26
## Calculate pmr parameter values from a sample.
## ---------------------------------------------RH
calcPMR <- function(x, na.value=NULL)
{
	if(is.null(na.value)) x=x[!is.na(x)] # get rid of NAs
	else x[is.na(x)]=na.value
	z0 = x==0 | is.na(x) | !is.finite(x)
	n=length(x); n0=length(x[z0]); n1=n-n0; p=n0/n
	#if (p==1) return(list(p=p,mu=NA,rho=NA))
	if (p==1) pmr=c(n,p,NA,NA)
	else {
		mu = mean(x[!z0]); 
		if (n1==1) rho=1/sqrt(n)
		else rho=sd(x[!z0])/mean(x[!z0])
		#return(list(p=p,mu=mu,rho=rho))
		pmr=c(n,p,mu,rho) 
	}
	names(pmr)=c("n","p","mu","rho")
	return(pmr)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~calcPMR


#getBootRuns----------------------------2013-11-25
# Get Norm's survey bootstrap results.
#-----------------------------------------------RH
getBootRuns=function(strSpp) {
	getData("gfb_boot.sql","GFBioSQL",strSpp=strSpp,path=.getSpath(),tenv=penv())
	surveys=PBSdat[order(PBSdat$date,PBSdat$bootID),]
	ttput(PBSdat)
	return(surveys) }

#getPMR---------------------------------2010-06-02
# Get pmr values from survey data in GFBioSQL
# using the SQL query 'gfb_pmr.sql'.
#-----------------------------------------------RH
getPMR =  function(strSpp="396",survID=c(1,2,3,121,167)){
	assign("PBStool",list(module="M04_Survey",call=match.call(),args=args(getPMR)),envir=.PBStoolEnv)
	sTime=proc.time()
	pmrTab = NULL
	cat(paste("pmr for species ",strSpp,"\n-------------------\nSurvey ID: ",sep=""))
	for (i in survID) {
		cat(paste(i,ifelse(i==survID[length(survID)],"",", "),sep="")); flush.console()
		getData("gfb_pmr.sql","GFBioSQL",strSpp,path=.getSpath(),surveyid=i,tenv=penv())
		pmrTmp=data.frame(SID=rep(i,nrow(PBSdat)),PBSdat[,c("h","p","mu","rho","A")],n=PBSdat$Nh,k=rep(1,nrow(PBSdat)))
		pmrTab=rbind(pmrTab,pmrTmp) }
	h=sort(unique(pmrTab$h))
	SURVEYS=getBootRuns(strSpp)
	surveys=SURVEYS[is.element(SURVEYS$survID,survID),]
	attributes(pmrTab)=c(attributes(pmrTab),list(fqt="gfb_pmr.sql",strSpp=strSpp,
		h=h,surveys.all=SURVEYS,surveys.selected=surveys))
	packList(c("pmrTab","sTime"),"PBStool",tenv=.PBStoolEnv)
	sTime=proc.time()-sTime
	cat(paste("\nDone and done in ",show0(round(sTime[3],1),1,add2int=TRUE)," sec\n",sep=""))
	return(pmrTab) }

#makePMRtables--------------------------2010-06-02
# Make CSV files containg pmr values for survey strata
#-----------------------------------------------RH
makePMRtables <- function(strSpp, surveys=list(qcss=c(1,2,3,121,167),
     wcvis=c(16,70,126), wqcis=c(79,122,129)), 
     qName="gfb_pmr.sql", path=.getSpath()) {
	for (i in names(surveys)) {
		cat(paste("Processing '",i,"'\n",sep="")); flush.console()
		sid <- surveys[[i]]
		for (j in sid) {
			cat(paste("SID ",j,"\n",sep="")); flush.console()
			first <- j==sid[1]
			getData(qName,"GFBioSQL",strSpp=strSpp,path=path,surveyid=j,tenv=penv())
			write.table(PBSdat,paste("pmr-",i,".csv",sep=""),sep=",",
				append=ifelse(first,FALSE,TRUE),row.names=FALSE,col.names=ifelse(first,TRUE,FALSE))
		} } }


#makeSSID-------------------------------2013-01-23
# Make a data object of survey series information
# called `ssid` for inclusion to `PBSdata`.
#-----------------------------------------------RH
makeSSID = function() {
	getData("SURVEY","GFBioSQL")
	z = is.element(PBSdat$SURVEY_SERIES_ID,0)
	PBSdat$SURVEY_SERIES_ID[z]= 1000 + PBSdat$SURVEY_ID[z]
	surv = desc = split(PBSdat$SURVEY_DESC, PBSdat$SURVEY_SERIES_ID)
	nyrs = sapply(surv,length)
	zmor = nyrs > 1
	word = sapply(surv[zmor],function(x){strsplit(x,split=" ")})
	year = sapply(word,function(w){sapply(w,function(x){x[is.element(x,1900:2100)]})})
	yrng = sapply(year,function(x){xx=as.numeric(x); paste("(",min(xx),"-",max(xx),")",sep="")}) # year range
	dlst = sapply(word,function(w){sapply(w,function(x){x[!is.element(x,1900:2100)]},simplify=FALSE)})
	dmor = sapply(dlst,function(d){
		for (i in 1:length(d)) if (i==1) dtmp=d[[i]] else dtmp=intersect(dtmp,d[[i]])
		paste(dtmp,collapse=" ") } )
	dmor = paste(dmor,yrng,sep=" ")
	desc[zmor] = dmor
	desc = unlist(desc)
	SSID = names(desc)
	ssid = as.numeric(SSID)
	ORIG = sapply(split(PBSdat$ORIGINAL_IND, PBSdat$SURVEY_SERIES_ID),function(x){x[1]})
	TRAW = sapply(split(PBSdat$TRAWL_IND, PBSdat$SURVEY_SERIES_ID),function(x){x[1]})
	orig = as.logical(as.numeric(gsub("N",0,gsub("Y",1,ORIG))))
	traw = as.logical(as.numeric(gsub("N",0,gsub("Y",1,TRAW))))
	gfb.survey.series = data.frame(ssid=ssid, desc=desc, nsurv=nyrs, original=orig, trawl=traw, row.names=SSID)
	ssid = list(gfb=gfb.survey.series)
	save("ssid",file="ssid.rda")
	invisible(ssid)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~makeSSID


#sampBG---------------------------------2008-09-30
# Sample from the binomial-gamma distribution.
#-----------------------------------------------RH
sampBG <- function (n, p=0.25, mu=1, rho=0.5) {
	# Obtain z-variates from binomial gamma distribution
	# Note: -------------------------------------
	# nu = 1/(rho^2) squared reciprocal CV of non-zeroes
	# mu=shape/rate, mu^2/nu = shape/rate^2
	# Therefore, shape=nu, rate=nu/mu
	bvec  <- rbinom(n,1,1-p)              # binomial vector
	if (p==1 || all(bvec==0)) return(bvec)
	nu=1/rho^2; numu=nu/mu
	gvec  <- rgamma(n,shape=nu,rate=numu) # gamma vector
	bgvec <- bvec * gvec                  # binomial gamma vector
	return(bgvec) }

#showAlpha------------------------------2013-01-18
# Show quantile confidence levels (alpha) for bootstrapped biomass.
#-----------------------------------------------RH
showAlpha <- function(lims=c("emp","bca")) {
	if (!exists("PBStool",envir=.PBStoolEnv) || is.null(ttcall(PBStool)$Bboot) || is.null(ttcall(PBStool)$bgtab)) bootBG()
	unpackList(ttcall(PBStool),scope="L")
	m    <- dim(bgtab)[1]
	h    <- dimnames(bgtab)$h
	S    <- length(Bboot)
	R    <- Bboot[[1]]$R
	n    <- bgtab[,"n*"]
	K    <- sum(n)
	B    <- sum(bgtab[,"B"])
	A    <- bgtab[,"A"]
	pmr  <- bgtab[,c("p","mu","rho")]
	nlims <- length(lims); lint=1/nlims

	pnam=setdiff(dimnames(Bqnt)$q,c("Bsamp","Qsamp","Btrue","Qtrue"))
	plev=as.numeric(pnam)
	names(plev) <- pnam
	np   <- length(plev)

	Sord  <- ceiling(log10(S))
	if (Sord==log10(S)) Sord <- Sord + 1
	SS    <- paste("S",pad0(1:S,Sord),sep="")
	BS    <- paste("B",pad0(1:S,Sord),sep="")
	dS    <- paste("d",pad0(1:S,Sord),sep="")
	names(BS) <- SS

	# PLOTORAMA =====================================
	pane <- 0
	resetGraph()

	# Plot 1 - Histogram of Bhats
	figpos <- c(0,.4,.667,1)
	par(fig=figpos, mar=c(4,3,.5,0), new=ifelse(pane==0,FALSE,TRUE),las=1)
	Nbin=20; Bmin=min(Best); Bmax=max(Best); targ=(Bmax-Bmin)/Nbin
	base=sort(c(1,2,5)%*%t(10^(-5:5)))
	Bbin=base[targ<base][1]
	xlim=c(floor(Bmin/Bbin)*Bbin, ceiling(Bmax/Bbin)*Bbin)
	brks=seq(xlim[1],xlim[2],Bbin)
	xy=hist(Best,breaks=brks,plot=FALSE)
	y=xy$counts/sum(xy$counts); 
	x=1:length(xy$counts); xval=xy$breaks[1:max(x)]+Bbin/2
	X=seq(1,max(x),4); Xval=xval[X] # big ticks
	barplot(y,space=0,col="moccasin",border="grey20",xaxt="n",xlab="",ylab="",cex.axis=.7,tcl=-0.25,mgp=c(0,.3,0))
	axis(1,at=X-.5,labels=Xval,pos=0,cex.axis=.7,tcl=-0.2,mgp=c(0,.1,0))
	Xobs=approx(xval,x-.5,Bobs,rule=2,ties="ordered")
	abline(v=Xobs,lty=2,col="blue")
	addLabel(.95,1,paste("K =",K),cex=.8,col="red",adj=c(1,1))
	blab=paste("expression(E*group(\"[\",widehat(B),\"]\")==",format(round(Bobs),big.mark="*\",\"*"),"~~t)",sep="")
	addLabel(.95,.93,eval(parse(text=blab)),cex=.8,col="blue",adj=c(1,1))
	pane=pane+1
	addLabel(.1,0.93,paste("(",letters[pane],")",sep=""),col=1,adj=0,cex=.8)
	mlab=paste("expression(Estimated~~widehat(B)~~group(\"(\",t,\")\"))",sep="")
	#mtext("Estimated B (t)",side=1,line=1,cex=.8)
	mtext(eval(parse(text=mlab)),side=1,line=1.2,cex=.8)
	mtext("Probability",side=2,line=2,cex=.9,las=0)

	# Plot 2 - Quantiles of Standard Normal
	figpos <- c(0.4,.7,.667,1)
	par(fig=figpos,mar=c(4,2,.1,.1),new=ifelse(pane==0,FALSE,TRUE),las=1)
	x=sort(Best); nx=length(x)
	y=(1:nx)/nx
	plot(x,y,type="n",xlab="",ylab="",xaxt="n",yaxt="n",xlim=range(x))
	lines(x,y,col="blue",lwd=2)
	abline(v=Bobs,lty=8,col="darkblue")
	#axis(1,at=xval,labels=FALSE,tcl=-.1)
	axis(1,at=Xval,tcl=-.2,cex.axis=.7,mgp=c(0,.1,0))
	axis(2,at=seq(0,1,.1),labels=FALSE,tcl=-.1)
	axis(2,at=seq(0,1,.2),cex.axis=.7,tcl=-.2,mgp=c(0,.3,0))
	pane=pane+1
	addLabel(.1,0.93,paste("(",letters[pane],")",sep=""),col=1,adj=0,cex=.8)
	mtext(eval(parse(text=mlab)),side=1,line=1.2,cex=.8)
	mtext("Cumulative Probability",side=2,line=1.5,cex=.8,las=0)

	# Plot 3 - Quantiles of Standard Normal
	figpos <- c(0.7,1,.667,1)
	par(fig=figpos,mar=c(4,2,.1,.1),new=ifelse(pane==0,FALSE,TRUE),las=1)
	y <- sort(qqnorm(Best,plot=FALSE)$x)
	plot(x,y,type="n",xlab="",ylab="",xaxt="n",yaxt="n",xlim=range(x))
	lines(x,y,col="blue",lwd=2); abline(v=Bobs,lty=8,col="darkblue")
	#axis(1,at=xval,labels=FALSE,tcl=-.1)
	axis(1,at=Xval,tcl=-.2,cex.axis=.7,mgp=c(0,.1,0))
	axis(2,at=seq(-3,3,.5),labels=FALSE,tcl=-.1)
	axis(2,at=seq(-3,3,1),cex.axis=.7,tcl=-.2,mgp=c(0,.3,0))
	pane=pane+1
	addLabel(.1,0.93,paste("(",letters[pane],")",sep=""),col=1,adj=0,cex=.8)
	mtext(eval(parse(text=mlab)),side=1,line=1.2,cex=.8)
	mtext("Normal Quantile",side=2,line=1,cex=0.8,las=0)

	nl=0; 
	LIMS=c("empirical","normal\napproximation","basic\nbootstrap","bootstrap\npercentile",
		"bias-corrected\naccelerated")
	names(LIMS)=c("emp","norm","basic","perc","bca") # generally available
	brks <- seq(0,1,.05); nbrks=length(brks)
	ymax=apply(Bqnt[,"Qtrue",lims],2,function(x,b){
		xy=hist(x,breaks=b,plot=FALSE); y=xy$counts/sum(xy$counts); max(y) },b=brks)
	yl=c(0,max(ymax))
	for (i in lims) {
		nl <- nl + 1
		# Histogram of B as quantile of predicteds
		figpos <- c(0+(nl-1)*lint,0+nl*lint,.333,.667)
		par(fig=figpos, mar=c(3,3,.5,.5), new=ifelse(pane==0,FALSE,TRUE))
		Qs   <- Bqnt[,"Qtrue",i]
		xy   <- hist(Qs,breaks=brks,plot=FALSE)
		y=xy$counts/sum(xy$counts);
		xmid <- barplot(xy$counts/S,space=0,ylim=yl,xlab="",ylab="",tcl=-.2,
			col="lightblue",border="grey60",cex.axis=.7,mgp=c(0,.3,0),adj=1,xaxt="n")
		xbrk=c(0,xmid+.5); big=seq(1,nbrks,4); Xbrk=xbrk[big]; Xlab=xy$breaks[big]
		axis(1,at=Xbrk,labels=Xlab,pos=0,tcl=-.2,cex.axis=.7,mgp=c(0,.1,0))
		pane=pane+1
		addLabel(0.07,0.93,paste("(",letters[pane],")",sep=""),col=1,adj=0,cex=.8)
		addLabel(.5,.95,LIMS[i],col="grey40",cex=.8)
		#if (i==lims[1])  
		mtext("Probability",side=2,line=2,cex=0.9,las=0)
		mlab="expression(alpha~~Interval)"
		mtext(eval(parse(text=mlab)),side=1,line=1,cex=.8)
	}
	nl=0
	for (i in lims) {
		nl=nl+1
		# Observed alpha vs. True alpha
		figpos <- c(0+(nl-1)*lint,0+nl*lint,0,.333)
		par(fig=figpos, mar=c(2.5,3,0,.5), new=ifelse(pane==0,FALSE,TRUE))
		xcount <- rep(0, np)
		names(xcount) <- pnam
		for(j in pnam) xcount[j]=sum(B <= Bqnt[,j,i])
		pobs <- xcount/S
		plot(plev,pobs,type="n",xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="")
		abline(h=.5,v=.5,lty=8,col="grey"); abline(0,1,col="grey30")
	   pcex <- 0.8
	   points(plev,pobs,pch=15,col="yellow",cex=pcex)
	   points(plev,pobs,pch=0,col=1,cex=pcex)
	   y50=approx(plev,pobs,.5,rule=2,ties="ordered")$y
	   points(0.5,y50,pch=15,col="red",cex=pcex+.1)
	   points(0.5,y50,pch=0,col=1,cex=pcex+.1)
		axis(1,at=brks,labels=FALSE,tcl=-.1)
		axis(1,at=brks[big],labels=TRUE,tcl=-.2,cex.axis=.7,mgp=c(0,.1,0))
		axis(2,at=brks,labels=FALSE,tcl=-.1)
		axis(2,at=brks[big],labels=TRUE,tcl=-.2,cex.axis=.7,mgp=c(0,.3,0),adj=1)
		pane=pane+1
		addLabel(0.07,0.93,paste("(",letters[pane],")",sep=""),col=1,adj=0,cex=.8)
		mlab="expression(True~~alpha)"
		mtext(eval(parse(text=mlab)),side=1,line=1,cex=.8)
		mlab="expression(Observed~~alpha)"
		mtext(eval(parse(text=mlab)),side=2,line=2,cex=.9,las=0)
	}
	packList(c("plev","xy","xcount","pobs"),"PBStool",tenv=.PBStoolEnv)
	invisible() }
#----------------------------------------showAlpha


## showIndices--------------------------2018-06-28
## Show survey indices from Norm's bootstrap tables
## ---------------------------------------------RH
showIndices =  function(strSpp="396", serID=1, survID=NULL, bootID, 
   tenv=.PBStoolEnv, quiet=TRUE, addN=TRUE, addT=TRUE, 
   outnam="relAbund", png=FALSE, pngres=400, PIN=c(7,7))
{
	assign("PBStool",list(module="M04_Survey",call=match.call(),args=args(showIndices)),envir=tenv)
	data("spn", package="PBSdata", envir=penv())
	surveys  = getBootRuns(strSpp)
	survBoot = surveys[,c("serID","survID","bootID","runDesc")]
#browser();return()

	# Selected survey
	if (!is.null(serID)) {
		sppBoot=surveys[is.element(surveys$serID,serID),]
		if (nrow(sppBoot)==0) {showMessage(paste0("No spp '",strSpp,"' in survey series ID ", serID)); return(NULL)}
	}
	if (!is.null(survID)) {
		sppBoot=surveys[is.element(surveys$survID,survID),]
		if (nrow(sppBoot)==0) {showMessage(paste0("No spp '",strSpp,"' in survey ID ", survID)); return(NULL)}
	}
	if (!missing(bootID)) {
		if (bootID %in% c("last","first")) {
			runBoot=substring(sppBoot$runDate,1,10); names(runBoot)=sppBoot$bootID
			booties = split(runBoot,sppBoot$survID)
			if (bootID=="last") zuse = sapply(booties,function(x){match(max(x),x)[1]})
			if (bootID=="first") zuse = sapply(booties,function(x){match(min(x),x)[1]})
			bootID = sapply(names(booties),function(x,y,z){as.numeric(names(y[[x]])[z[x]])},y=booties,z=zuse)
		}
#browser();return()
		oneI = sapply(bootID,function(x){grep(T,is.element(sppBoot$bootID,x))[1]})
		sppBoot=sppBoot[oneI,]
		#sppBoot=sppBoot[is.element(sppBoot$bootID,bootID),] ## returns multiple records (debug later)
	}
	if (!missing(outnam)){
		fnam = paste0(outnam,".csv")
		admBoot = sppBoot[,c("serID","year","biomass","bootRE")]
		admBoot$biomass = admBoot$biomass/1000. ## convert to tonnes
		names(admBoot) = c("series","year","value","CV")
		write.csv(admBoot,fnam,row.names=FALSE)
	}
#browser();return()
	if (nrow(sppBoot)==0) {
		if (quiet) {plot(0,0,type="n",axes=FALSE,xlab="",ylab=""); return("nada") }
		else {showMessage(paste("No index for species '",strSpp,"'",sep="")); return(NULL)}
	}
	noDesc = is.na(sppBoot$runDesc)
	if (any(noDesc)) {
		for (i in grep(TRUE,noDesc)) {
			getData(paste("SELECT SURVEY_SERIES_DESC FROM SURVEY_SERIES WHERE SURVEY_SERIES_ID IN (",sppBoot$serID[i],")",sep=""),type="SQLX",dbName="GFBioSQL",strSpp=strSpp,tenv=penv())
			sppBoot$runDesc[i] = PBSdat
		}
	}

	# Function to group and flatten x by some factor, usually y
	flatten=function(x,f,off=0) {
		xx=split(x,f); ff=split(f,f)
		#cc=sapply(xx,function(x){1:length(x)},simplify=FALSE) # for colour positions
		plump=function(pillow,off=0) {
			plist=sapply(pillow,function(i){
				if (length(i)>1 && off>0) 
					i=seq(i[1]-off/2,i[1]+off/2,len=length(i))
				c(i,NA)},simplify=FALSE)
			px=as.vector(unlist(plist))
			return(px) }
		x=plump(ff,off=off); y=plump(xx)#; clr=plump(cc)
		return(list(x=x,y=y)) }

	# x-y data and limits
	x=sppBoot$year; y=sppBoot$biomass/1000; xoff=.025*diff(range(x))
	xy   = flatten(y,x,off=xoff)
	xlim = extendrange(xy$x,f=0.05)
	cil  = flatten(sppBoot$bootLoCI/1000,x,off=xoff)
	cih  = flatten(sppBoot$bootHiCI/1000,x,off=xoff)
	ylim = extendrange(c(xy$y,cil$y,cih$y),f=0.1)
	xci  = as.vector(rbind(cil$x,cih$x,NA))
	yci  = as.vector(rbind(cil$y,cih$y,NA))
	
	ymax = max(sppBoot$bootHiCI/1000)
	ysca = ifelse(ymax>2000,1000,1)
	yuni = ifelse(ymax>2000,"1000t","t")

	# Log-linear fit
	if (length(unique(x))>1) {
		isFit=TRUE
		logy=log2(y)
		lmfit=lm(logy~x); lmcoef=lmfit$coefficients
		a=lmcoef[1]; b=lmcoef[2]; r=2^b - 1
		xfit=seq(min(x),max(x),length=100)
		logyfit=a + b*xfit; yfit=2^logyfit; neg=b<0 }
	else {
		isFit=FALSE; xlim=xlim+c(-1,1) }

	#Legend label
	tlab=split(sppBoot$runDesc,x)
	llab=character(0)
	for(i in names(tlab)){llab=c(llab,gsub(paste(i," ",sep=""),"",tlab[[i]]))}
	llab[is.element(llab,c("",NA))] = "No description"
	ulab=unique(llab); nlab=length(ulab)
	clr=flatten(match(llab,ulab),x,off=xoff)
	#bg=rep(c("gold","green","coral","cyan","magenta","moccasin"),nlab)[1:nlab]
	#fg=rep(c("black","darkgreen","darkred","darkblue","magenta4","brown"),nlab)[1:nlab]
	bg=c("gold","green","coral","cyan","magenta","moccasin")
	fg=c("black","darkgreen","darkred","darkblue","magenta4","brown")
	if (nlab>length(bg)) {
		newclrs = setdiff(colors(),c(bg,"black",paste(c("gray","grey"),rep(1:99,each=2),sep="")))
		bg=c(bg,sample(newclrs,(nlab-length(bg))))
		newclrs = setdiff(newclrs,bg)
		fg=c(fg,sample(newclrs,(nlab-length(fg))))
	} else {
		bg = bg[1:nlab]; fg = fg[1:nlab]
	}
#browser();return()

	# Plot the results
	#resetGraph()
	if (png) png(paste0(outnam,".png"),width=PIN[1], height=PIN[2], units="in", res=pngres)
	expandGraph(mar=c(2.75,3.5,0.5,1),las=1)
	plot(xy,xlim=xlim,ylim=ylim,type="n",xlab="",ylab="",yaxt="n")

	axis(2, at=pretty(c(0,par()$usr[4])), labels=pretty(c(0,par()$usr[4]))/ysca)
	if (diff(xlim)<25)
		axis(1,at=seq(min(x),max(x),1),labels=FALSE,tcl=-.2)
	else
		axis(1,at=pretty(x,n=10),labels=FALSE,tcl=-.01)
	abline(h=pretty(ylim,6)[-1],col="gainsboro")
	lines(xci,yci,col="cornflowerblue",lwd=2)
	points(xy,pch=21,cex=ifelse(png,1.2,1.5),col=fg[clr$y],bg=bg[clr$y])
	if (addN)
		text(x,sppBoot$bootLoCI/1000,sppBoot$numPosSets,col="navyblue",adj=c(.5,1.5))
	if (isFit && addT) {
		lines(xfit,yfit,col=ifelse(neg,"red","green4"),lwd=2)
		addLabel(ifelse(neg,.95,.05),ifelse(neg,.05,.05),paste(round(r*100,1),"%/y"),
			adj=ifelse(neg,1,0),col=ifelse(neg,"red","green4"),cex=1) }
	addLabel(.95,.99,spn[strSpp],cex=0.9,adj=c(1,1),col="slategrey")
	if (isFit && neg) right=TRUE else right=FALSE
	addLegend(ifelse(right,0.99,.03), 0.97, legend=ulab, pch=21, col=fg[1:nlab], pt.bg=bg[1:nlab],
		bty="n", xjust=ifelse(right,1,0), cex=0.8, pt.cex=1.2)
	mtext(paste0("Relative Biomass (",yuni,")"),side=2,line=2.5,cex=1.2,las=0)
	mtext("Survey Year",side=1,line=1.5,cex=1.2)
	box()
	if (png) dev.off()
	packList(c("surveys","sppBoot","survBoot","xy","cil","cih","clr","llab","ulab"),"PBStool",tenv=tenv)
	names(y)=x
	if (!quiet) print(sppBoot)
	invisible(sppBoot)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~showIndices


#simBGtrend-----------------------------2013-01-28
# Simulate a population projection based on prior binomial-gamma parameters.
#-----------------------------------------------RH
simBGtrend <- function(pmr=pop.pmr.qcss, Npred=15, genT=NULL,
   Nsim=1, yrs=NULL, alpha=1, ioenv=.GlobalEnv) {
	#Subfunctions--------------
	wval <- function(y,alpha=1){ 
		#weighted y based on distance x; sum values for current weighted y
		if (is.null(names(y))) x=1:length(y)
		else {x=as.numeric(names(y)); x=x-min(x)+1 }
		d=exp(alpha*x) # distance effect: as alpha increases, latter observations get more weight, alpha=0 renders equal weights
		w=d/sum(d)     # proportional weights
		return(sum(w*y,na.rm=TRUE))}
	#--------------------------
	assign("PBStool",list(module="M04_Survey",call=match.call(),args=args(simBGtrend),ioenv=ioenv),envir=.PBStoolEnv)
	#if (scope=="L") env <- parent.frame(1) else env <- .PBStoolEnv
	snam=as.character(substitute(pmr))
	txt=paste("getFile(\"",snam,"\",senv=ioenv,use.pkg=TRUE,try.all.frames=TRUE,tenv=penv()); pmr=",snam,sep="")
	eval(parse(text=txt))
	if (is.null(yrs)) {
		survey=attributes(pmr)$survey; yrs=survey$year }
	if (is.null(yrs)) showError(paste("Assign",length(unique(pmr$SID)),"years to argument 'yrs'"))
	nyr=length(yrs); 
	if (!is.null(genT)) Npred=(3*genT)-nyr
	Nyr=nyr+Npred;  YRS=c(yrs,(yrs[nyr]+1):(yrs[nyr]+Npred))
	SIDs=survey$SID;  nSID=length(SIDs)
	h=attributes(pmr)$h
	if (is.null(h)) h=sort(unique(pmr$h))
	hh=as.character(h); m=length(h)

	pmrhist=array(NA,dim=c(m,3,Nyr,Nsim),dimnames=list(h,c("p","mu","rho"),YRS,1:Nsim))
	names(dimnames(pmrhist))=c("h","pmr","yr","sim")
	Brows=c("year",c("Blo","Bhi","Bsamp","Qsamp","Btrue","Qtrue")); 
	Btrend=array(NA,dim=c(Nyr,length(Brows),Nsim),dimnames=list(YRS,Brows,1:Nsim))
	names(dimnames(Btrend))=c("yr","B","sim")
	brR=array(NA,dim=c(Nsim,4),dimnames=list(1:Nsim,c("sim","b","r","R")))
	names(dimnames(brR))=c("sim","brR")

	cat("\n",paste(Nsim,"Samples: \n"))
	for (s in 1:Nsim) {
		cat(paste(s,ifelse(s==Nsim || round(s%%10)==0,"\n",", "),sep="")); flush.console()
		for (i in SIDs) {
			yr=yrs[is.element(SIDs,i)]; yy=as.character(yr)
			#bootBG(pmr,K=224,S=1,R=500,SID=i,ci=.95,lims=c("emp","bca"),scope="L") # must be local to be seen
			bootBG(pmr,K=224,S=1,R=500,SID=i,ci=.95,lims=c("emp","bca"))
			unpackList(ttcall(PBStool),scope="L")
			pmrhist[hh,c("p","mu","rho"),yy,s]=bgtab[hh,c("p","mu","rho")]
			iqnt=c(year=yr,Bqnt[,,"bca"])
			Btrend[yy,Brows,s]=iqnt }
		for (i in 1:Npred) {
			yr=yrs[nyr]+i; yy=as.character(yr); 
			yyy=as.character(YRS[1:(nyr+i)])
			SID=SIDs[nSID]+i
			samppmr=(t(sapply(bgsamp,calcPMR,simplify=TRUE)))
			pmrhist[hh,c("p","mu","rho"),yy,s]=samppmr[hh,c("p","mu","rho")]
			nextpmr=apply(pmrhist[hh,,yyy,s],1:2,wval,alpha=alpha)
			newpmr=data.frame(SID=rep(SID,m),h=h,
				p=unlist(nextpmr[,"p"]),mu=unlist(nextpmr[,"mu"]),rho=unlist(nextpmr[,"rho"]),
				bgtab[,c("A","n","k")])
			#bootBG(newpmr,K=224,S=1,R=500,SID=SID,ci=.95,lims=c("emp","bca"),scope="L") # must be local to be seen
			bootBG(newpmr,K=224,S=1,R=500,SID=SID,ci=.95,lims=c("emp","bca"))
			unpackList(ttcall(PBStool),scope="L")
			pmrhist[hh,c("p","mu","rho"),yy,s]=nextpmr[hh,c("p","mu","rho")]
			iqnt=c(year=yr,Bqnt[,,"bca"])
			Btrend[yy,Brows,s]=iqnt }
		Bt=as.data.frame(Btrend[,Brows,s])
		lmfit=lm(log2(Bsamp+0.001)~year,data=Bt); b=lmfit$coeff[[2]]
		r=2^b-1; R=2^(b*(Nyr-1))-1
		brR[s,]=c(s,b,r,R)
	} # end Nsim loop
	brR=as.data.frame(brR)

	Bmean=apply(Btrend,1:2,mean,na.rm=TRUE); Bmean=as.data.frame(Bmean)
	Bseen=Bmean[as.character(yrs),]
	packList(c("pmrhist","Btrend","Bmean","Bseen","brR"),"PBStool",tenv=.PBStoolEnv)

	# Plot
	x=Bmean$year; ytru=Bmean$Btrue; ylo=Bmean$Blo; yhi=Bmean$Bhi
	x0=Bseen$year; y0tru=Bseen$Btrue; y0lo=Bseen$Blo; y0hi=Bseen$Bhi
	ylim=c(0,max(ytru,ylo,yhi))

	resetGraph()
	expandGraph(mar=c(3,3,1,0.5))
	plot(x,ytru,ylim=ylim,type="n",xlab="Year",ylab="Biomass Index")
	polygon(c(x,rev(x)),c(ylo,rev(yhi)),col="grey95",border=FALSE)
	lines(x,ytru,col="green",lwd=2)
	lines(c(x,NA,x),c(ylo,NA,yhi),col="cornflowerblue",lwd=2)
	polygon(c(x0,rev(x0)),c(y0lo,rev(y0hi)),col="grey90",border=FALSE)
	lines(x0,y0tru,col="forestgreen",lwd=2)
	lines(c(x0,NA,x0),c(y0lo,NA,y0hi),col="blue",lwd=2)
	points(x0,y0tru,pch=15,col="gold"); points(x0,y0tru,pch=0,col="black")
	invisible() }
#---------------------------------------simBGtrend


#trend----------------------------------2013-04-03
# Simple trend analysis for annnual IPHC indices
#-----------------------------------------------RH
trend <- function(strSpp="442", fqtName="gfb_iphc.sql",
   dbName="GFBioSQL", spath=NULL, type="SQL", ioenv=.GlobalEnv, hnam=NULL)
{
	warn <- options()$warn; options(warn=-1)
	data("spn", package="PBSdata", envir=ioenv)
	if (exists("PBSdat",envir=ioenv)) rm(PBSdat,pos=ioenv)
	assign("PBStool",list(module="M04_Survey",call=match.call(),args=args(trend),ioenv=ioenv,plotname="Rplot"),envir=.PBStoolEnv)

	wpath <- .getWpath()
	if (is.null(spath)) spath <- .getSpath()
	rtmp <- tempdir(); rtmp <- gsub("\\\\","/",rtmp)
	wnam <- paste(wpath,"trendWin.txt",sep="/")
	wtmp <- paste(rtmp,"trendWin.txt",sep="/")
	temp <- readLines(wnam)
	temp <- gsub("@wdf",wtmp,temp)
	temp <- gsub("@sppcode",strSpp,temp)
	temp <- gsub("@fqtName",fqtName,temp)
	temp <- gsub("@dbName",dbName,temp)
	temp <- gsub("@path",spath,temp)
	temp <- gsub("@type",type,temp)
	if (!is.null(hnam) && is.character(hnam))
		temp <- gsub("#import=",paste("import=\"",hnam,"\"",sep=""),temp)
	temp <- gsub("guest",Sys.info()["user"],temp)
	writeLines(temp,con=wtmp)
	createWin(wtmp); options(warn=warn)
	invisible() }

#.trend.getSQLspp-----------------------2010-10-20
# Get IPHC data for a selected species
#-----------------------------------------------RH
.trend.getSQLspp <- function(spp=NULL){
	getWinVal(scope="L",winName="window"); act <- getWinAct()[1]
	if (!is.null(act) && act=="getdata")
		getdata <- TRUE else getdata <- FALSE
	if (path==""){
		path <- getwd(); setWinVal(list(path=path),winName="window") }
	if (is.null(spp)) {
		spp  <- eval(parse(text=paste("c(\"",gsub(",","\",\"",strSpp),"\")",sep="")));
		if (any(spp=="") || length(spp)>1) showError("Choose 1 species") }
	ioenv = ttcall(PBStool)$ioenv
	if (exists("PBSdat",envir=ioenv)) tget(PBSdat,tenv=ioenv)
	if (!exists("PBSdat",envir=ioenv) || spp!=attributes(PBSdat)$spp || 
		fqtName!=attributes(PBSdat)$fqt || getdata)
	{
		expr=paste("getData(fqtName=\"",fqtName,"\",dbName=\"",dbName,"\",strSpp=\"",
			spp,"\",type=\"",type,"\",path=\"",path,"\",trusted=",trusted,
			",uid=\"",uid,"\",pwd=\"",pwd,"\",tenv=penv())",sep="")
		eval(parse(text=expr))
	}
	if (nrow(PBSdat)==0 || sum(PBSdat$pcs)==0) showError(paste("Species =",spp),type="nodata")
	if (is.null(attributes(PBSdat)$spp) || attributes(PBSdat)$spp!=spp) {
		spp <- attributes(PBSdat)$spp
		setWinVal(list(strSpp=spp),winName="window") } 
	setWinVal(list(fnam="PBSdat"),winName="window")
	tput(PBSdat,tenv=ioenv)
	invisible() }

#.trend.trendy--------------------------2013-04-04
# Main engine of the trend algorithm.
#-----------------------------------------------RH
.trend.trendy <- function() {
	getWinVal(scope="L",winName="window")
	ioenv = ttcall(PBStool)$ioenv
	spp  <- eval(parse(text=paste("c(\"",gsub(",","\",\"",strSpp),"\")",sep="")));
	if (any(spp=="") || length(spp)>1) showError("Choose 1 species")
	if (fnam=="empty" || (fnam=="PBSdat" && attributes(tcall(PBSdat,tenv=ioenv))$spp!=spp)) {
		.trend.getSQLspp(spp)
		fnam = getWinVal()$fnam
		tget(PBSdat,tenv=ioenv) }
	else if (fnam %in% c("iphc.rbr","iphc.rer","iphc.yyr"))
		eval(parse(text=paste("data(\"",fnam,"\", package=\"PBSdata\", envir=penv())",sep=""))) 
	else 
		eval(parse(text=paste("getFile(",fnam,",senv=ioenv,tenv=penv())",sep=""))) 
	eval(parse(text=paste("dat=",fnam,sep="")))
	if (is.null(attributes(dat)$spp))
		attr(dat,"spp") = spp
	else if( attributes(dat)$spp!=spp) {
		spp=attributes(dat)$spp
		setWinVal(list(strSpp=spp),winName="window") 
	}
	year=eval(parse(text=paste("c(",strYear,")",sep="")))
	if (is.null(year)) YEAR <- sort(unique(dat$year))

	if (zstn) { # Exclude stations with no history of catch
		PCS <- sapply(split(dat$pcs,dat$station),sum)
		stn <- names(PCS[PCS>0])
		packList("stn","PBStool",tenv=.PBStoolEnv)
		dat <- dat[is.element(dat$station,stn),]
		if (nrow(dat)==0) showError(paste("Stations catching",spp),type="nodata") }

	if (strYear=="") yrUse=YEAR else yrUse=year
	xlim <- range(yrUse,na.rm=TRUE); nset <- nrow(dat)
	yrs <- seq(xlim[1],xlim[2],1)
	tlist <- as.list(rep(NA,length(yrs))); names(tlist)=yrs # create an empty list for boxplots
	p=rep(NA,length(yrs)); names(p)=yrs; n=p # initialize p- and n-vectors

	dat <- dat[is.element(dat$year,yrUse),]
	if (nrow(dat)==0) showError(paste("year =",paste(yrUse,collapse=", ")),type="nodata") 
	packList("dat","PBStool",tenv=.PBStoolEnv)

	# Generate trend in data along with boxplots and proportions zero
	sfunc <- get(func); # summary function
	.trend.funky();           # generate transformation functions in the global environment
	getWinVal(c("zero","tran"),scope="L",winName="window");
	one <- ifelse(tran=="",0,1); xfld <- "year";

	X <- dat[,xfld]; Y <- dat[,yfld]; #z0 <- Y==0 | is.na(Y);
	Alist <- split(Y,X)
	packList("Alist","PBStool",tenv=.PBStoolEnv)
	alist <- sapply(Alist,function(x){if(length(x[x>0&!is.na(x)])>0) x[x>0&!is.na(x)] else NULL },simplify=FALSE)
	packList("alist","PBStool",tenv=.PBStoolEnv)
	pA <- sapply(Alist,function(x){length(x[x==0 & !is.na(x)])/length(x)});
	pB=pA; if(!zero) pB[pB==1]=NA
	Alist=sapply(Alist,function(x,a=max(0,aVal,na.rm=TRUE)){x+a},simplify=FALSE)
	alist=sapply(alist,function(x,a=max(0,aVal,na.rm=TRUE)){x+a},simplify=FALSE)

	if (zero) ttlist <- sapply(Alist,ttcall(PBStool)$ftran,simplify=FALSE)
	else ttlist <- sapply(alist,ttcall(PBStool)$ftran,simplify=FALSE)
	if (any(sapply(ttlist,function(x){any(is.infinite(x))})==TRUE)) 
		showError("Add small value to observations for sensible log-transformation")
	tlist[names(ttlist)] = ttlist
	z0=sapply(tlist,function(x){all(is.na(x))}); z1=!z0
	tlist[z0] <- 0;
	p[names(pB)]=pB; #p=p[z1]
	nA <- sapply(ttlist,function(x){length(!is.na(x))})
	nB=nA; if(!zero) nB[nB==0]=NA
	n[names(nB)]=nB

	y <- sapply(tlist,sfunc,na.rm=TRUE,simplify=TRUE);
	x <- as.numeric(names(y)); 
	xpos <- 1:length(x); N <- max(x)-min(x)+1;
#browser();return()
	ylim <- c( min(y[z1],sapply(tlist[z1],min)), max(y[z1],sapply(tlist[z1],max)) );
	if (diff(ylim)==0) ylim <- ylim + c(-1,1); # in case all values are the same
	tlist[z0] <- NA;
	packList("tlist","PBStool",tenv=.PBStoolEnv)

	# Lower area for p
	pex <- ifelse(zsho,0.25,0); psp <- 1-1/(1+pex); # proportion of boxplot area
	ylim <- ylim + c(-(pex+0.05)*diff(ylim),0);
	blim <- c(ylim[1],ylim[1]+psp*diff(ylim)); db <- diff(blim);

	oname = paste("trend",spp,yfld,sep="-")
	ofile = c(TRUE,ofile); names(ofile)[1] = "gdev"
	for (i in 1:length(ofile)){
		ii = ofile[i];  if (!ii) next; iii=names(ii)
		switch(iii,
			"eps"  = { postscript(file=paste(oname,"eps",sep="."),width=6.5,height=7,paper="special") },
			"pdf"  = { pdf(file=paste(oname,"pdf",sep="."),width=6.5,height=7,paper="special") },
			"png"  = { png(filename=paste(oname,"png",sep="."),width=6.5*150,height=7*150,pointsize=18) },
			"wmf"  = { do.call("win.metafile",list(filename=paste(oname,"wmf",sep="."),width=6.5,height=7)) },
			"gdev" = { resetGraph() }
		)
		expandGraph(mfrow=c(1,1),mar=c(4,4,4,3),omi=c(0,0,0,0),mgp=c(2.5,.5,0));
		out <- boxplot(tlist,ylim=ylim,las=1,col=Cbox,range=0,boxwex=0.5,staplewex=0,lty=1);
		packList("out","PBStool",tenv=.PBStoolEnv)
		lines(xpos[z1],y[z1],col=Clin,lwd=3);
		points(xpos[z1],y[z1],col=Cpoi,pch=15,cex=1.2); points(xpos[z1],y[z1],col="black",pch=0,cex=1.2);
		mtext(tcall(spn,tenv=ioenv)[spp],side=3,line=2,cex=1.5,las=0);
		mtext("Year",side=1,line=2,cex=1.5); mtext(paste(tran,yfld),side=2,line=2.5,cex=1.5);
		if (zsho) {
			pp <- p*db + ylim[1]; qq <- seq(0,1,.25); hh <- qq*db + ylim[1];
			abline(h=hh,col="grey40",lty=3);
			if (zfill) {
				eval(parse(text=paste(c("fillBars = ",gsub("lines","polygon",deparse(drawBars))))))
				fillBars(xpos,pp,base=ylim[1],col=lucent(Cbar,0.1),border=FALSE)
			}
			drawBars(xpos,pp,base=ylim[1],col=Cbar,lwd=2);
			mtext("prop Zero",side=2,line=2.5,cex=1,adj=0.10,col=Cbar,las=0)
			mtext(show0(qq,2),side=4,at=hh,las=1,cex=.8,col=Cbar,line=.5,adj=0)
			text(xpos,pp,show0(round(p,2),2),col=Cbar,cex=.8,adj=c(.5,-.5))
			#nout=nout[z1]; n[names(nout)]=nout
			text(xpos,(ylim[1]+par()$usr[3])/2,n,col=Cnum,cex=0.8)
			text(.4*(1+par()$usr[1]),(ylim[1]+par()$usr[3])/2,"n",col=Cnum,cex=0.9)
			packList(c("n","p","xpos"),"PBStool",tenv=.PBStoolEnv) }
		lmY <- lm(y[z1]~x[z1]); b <- lmY$coeff[2]
		r   <- ttcall(PBStool)$btran(b)-one
		R   <- ttcall(PBStool)$btran(b*(N-1))-one
		yp  <- predict.lm(lmY); lines(xpos[z1],yp,col=Ctrd,lwd=3);
		msg <- paste("( b= ",show0(round(b,4),4),",  r= ",show0(round(r,4),4),",  R= ",show0(round(R,4),4),")",sep="");
		mtext(msg,side=3,line=.5,cex=1);
		if (iii!="gdev") dev.off()
	}
	invisible() }

#.trend.funky---------------------------2010-10-20
# Transformation function interpreter.
#-----------------------------------------------RH
.trend.funky <- function() {
	# Generate forward and backward transformation functions
	getWinVal(scope="L",winName="window");
	if (!any(tran==c("log2","log","log10"))) {
		ftran <- function(x) {return (x)}
		btran <- function(x) {return (x)}
		packList(c("ftran","btran"),"PBStool",tenv=.PBStoolEnv)
		setWinVal(list(tran="nada"),winName="window"); }
	else {
		ftran <- get(tran)
		zbase <- match(tran,c("log2","log","log10"));
		tbase <- switch(zbase,2,exp(1),10);
		fexp  <- paste("btran = function(x,t=",tbase,"){t^x}");
		eval(parse(text=fexp)); 
		packList(c("ftran","btran"),"PBStool",tenv=.PBStoolEnv)
		#setWinVal(list(zero=FALSE),winName="window")
		}
	invisible() }

#.trend.booty---------------------------2013-04-03
# Calculate and plot bootstraps.
#-----------------------------------------------RH
.trend.booty <- function() {
	alist <- ttcall(PBStool)$tlist
	if (is.null(alist)) showError("Generate a TREND first")
	# Bootstrap the indices and generate multiple trends
	getWinVal(scope="L",winName="window");
	one <- ifelse(tran=="",0,1);
	z0 <- sapply(alist,function(x){all(is.na(x))}); z1 <- !z0;
	alist <- alist[z1];
	tboot <- sapply(alist,sample,size=Nboot,replace=TRUE);
	x     <- as.numeric(dimnames(tboot)[[2]]);
	brR   <- function(y,x,one) {
				N   <- max(x)-min(x)+1;
				lmY <- lm(y~x); b <- lmY$coeff[2];
				r   <- ttcall(PBStool)$btran(b)-one
				R   <- ttcall(PBStool)$btran(b*(N-1))-one
				return(c(b,r,R)) };
	bboot <- t(apply(tboot,1,brR,x=x,one=one));
	dimnames(bboot)[[2]] <- c("b","r","R");
	bout  <- as.data.frame(bboot);

	oname = paste("boot",attributes(ttcall(PBStool)$dat)$spp,yfld,sep="-")
	ofile = c(TRUE,ofile); names(ofile)[1] = "gdev"
	for (i in 1:length(ofile)){
		ii = ofile[i];  if (!ii) next; iii=names(ii)
		switch(iii,
			"eps"  = { postscript(file=paste(oname,"eps",sep="."),width=6.5,height=7,paper="special") },
			"pdf"  = { pdf(file=paste(oname,"pdf",sep="."),width=6.5,height=7,paper="special") },
			"png"  = { png(filename=paste(oname,"png",sep="."),width=6.5*150,height=7*150,pointsize=18) },
			"wmf"  = { do.call("win.metafile",list(filename=paste(oname,"wmf",sep="."),width=6.5,height=7)) },
			"gdev" = { resetGraph() }
		)
		expandGraph(mfrow=c(2,1),mar=c(3,4,.5,.5),omi=c(0,0,0,0),mgp=c(2.5,.5,0))
		for (i in c("b","r")){
			ii<- match(i,c("b","r","R"));
			x <- sort(bout[,i]);
			n <- length(x);
			y <- (1:n)/n;
			q <- quantile(x,c(.025,.5,.975));
			plot(x,y,ylim=c(0,1),type="l",lwd=2,col="#400080",las=1,
				xlab="",ylab=paste("Cum % Freq of ",i),cex.lab=1.2);
			abline(v=q,lty=c(3,2,3),col=c("red","green","red"),lwd=c(1,2,1));
			text(q,c(.95,.875,.80),show0(round(q,3),3),cex=1.2,adj=c(-.1,0),col=c("red","darkgreen","red"));
			addLabel(.98,.15,i,cex=2,col="#400080",adj=1);
			mtext(switch(ii,"Slope","Annual Rate","Accumulated Change"),side=1,line=1.75,cex=1);
			box() }
		if(iii!="gdev") dev.off()
	}
	invisible() }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~trend


