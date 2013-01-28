#===============================================================================
# Module 6: Temporal
# ------------------
#  boxSeason.......Show seasonal data using boxplots.
#  calcMA..........Calculate a moving average using a fixed period occurring every x units.
#  imputeRate......Impute rate of return from an investment with irregular contributions/withdrawals.
#  plotDiversity...Plot diversity as barplots and overlay species richness.
#  trackComp.......Track the composition of phytoplankton groups
#===============================================================================

#boxSeason------------------------------2013-01-28
# Show seasonal data using boxplots.
#-----------------------------------------------RH
boxSeason <- function (fqtName="Ex01_Sample_Info", dbName="Examples.mdb",
     type="MDB", path=getwd(), fld="S", brks="M", pdf=FALSE, wmf=FALSE, ioenv=.GlobalEnv) {

	par0 <- par(no.readonly = TRUE)
	on.exit(par(par0))

	fnam=paste("Season-",fld,"x",brks,sep="")
	assign("PBStool",list(module="M06_Temporal",call=match.call(),ioenv=ioenv,plotname=fnam),envir=.PBStoolEnv)
	if (type=="FILE") {
		eval(parse(text=paste("getFile(",fqtName,",senv=ioenv,try.all.frames=TRUE,tenv=penv()); dat=",fqtName,sep=""))) }
	else {
		getData(fqtName,dbName,type=type,path=path,tenv=penv()) 
		dat=PBSdat }
	ramp <- colorRamp(c("yellow","red","darkblue"))

	if (any(names(dat)=="YMD")) dat$date=as.POSIXct(dat$YMD)
	xrng <- range(dat$date)
	yr <- sort(unique(as.numeric(substring(dat$date,1,4))))
	yr = yr[1]:yr[length(yr)]; nyr <- length(yr)
	if (brks=="Q") { # Quarter
		moda0="01-01"; vlin=4
		moda=c("03-31","06-30","09-30","12-31"); 
		labs=c("Q1","Q2","Q3","Q4") }
	if (brks=="S") { # Season
		moda0="10-31"; vlin=4
		moda=c("02-28","05-31","08-31","10-31"); 
		labs=c("SP","SU","FA","WI") }
	if (brks=="B") { # Bimonthly
		moda0="01-01"; vlin=6
		moda=c("02-28","04-30","06-30","08-31","10-31","12-31"); 
		labs=c("JF","MA","MJ","JA","SO","ND") }
	if (brks=="M") { # Monthly
		moda0="01-01"; vlin=12
		moda=c("01-31","02-28","03-31","04-30","05-31","06-30","07-31","08-31","09-30","10-31","11-30","12-31"); 
		labs=c("Ja","Fe","Ma","Ap","My","Jn","Jl","Au","Se","Oc","No","De") }
	nmoda=length(moda)
	bcol=rgb(ramp(seq(0,1,len=nmoda)),maxColorValue=255)
	brks=paste(rep(yr,each=nmoda),rep(moda,nyr),sep="-")
	labs=paste(rep(yr,each=nmoda),rep(labs,nyr),sep="-")
	brks=c(paste(yr[1]-ifelse(moda0>moda[1],1,0),moda0,sep="-"),brks)

	dat$per <- cut(dat$date,as.POSIXct(brks),labels=labs)
	bag     <- split(dat[,fld],dat$per); nbag <- length(bag)
	xy      <- boxplot(bag,plot=FALSE)
	zna     <- apply(xy$stats,2,function(x){all(is.na(x))})
	ztrunc  <- clipVector(zna,"TRUE")
	ii      <- as.numeric(names(ztrunc)) # index of time periods which show data
#browser();return()
	ylim=c(0, max(sapply(bag[ii],function(x){if(length(x)>0) max(x) else 0 })))
	stuff=c("dat","brks","labs","bag","ii","bcol")
	packList(stuff,"PBStool",tenv=.PBStoolEnv)

	if (pdf) pdf(file=paste(fnam,".pdf",sep=""),width=11,height=8.5)
	else if (wmf) win.metafile(paste(fnam,".wmf",sep=""),width=11,height=8.5)
	else resetGraph()
	expandGraph(mar=c(6,5,2,1.5),mgp=c(2.75,.75,0))
	boxplot(bag[ii],las=2,range=0,lty=1,boxwex=0.5,staplewex=0,
		col=rep(bcol,length(ii))[ii],cex.lab=1.2,
		ylim=ylim, xlab=if(length(ii)==1) labs[!zna] else "",  # potentially problematic
		ylab=ifelse(fld=="S","Number of Species",ifelse(fld=="H","Shannon-Wiener Diversity Index","Evenness of Species")))
	zv=is.element(ii,seq(vlin,nbag,vlin))
	iv=(1:length(zv))[zv]+.5 # possible indices where vertical lines are drawn
	abline(v=iv,col="slategrey",lty=8)
	text(iv,par()$usr[4],yr[1:length(iv)],cex=1,adj=c(1.5,1.5))
	if (pdf|wmf) dev.off()
	invisible() }
#----------------------------------------boxSeason

#calcMA---------------------------------2011-05-05
# Calculate a moving average using a fixed
# period occurring every x units.
#-----------------------------------------------RH
calcMA = function(x,y,y2,period=270,every=10) {
	xclass = class(x); yclass = class(y)  # get all classes of x & y
	xallow = c("numeric","integer","ts","POSIXct","POSIXlt","Date")
	if (!any(is.element(xclass,xallow))) return(NULL)
	xclass = xclass[is.element(xclass,xallow)][1]
	yclass = rev(yclass)[1]
	# Make 'as.' functions to predefine empty vectors for filling in moving averages of x & y.
	eval(parse(text=paste("xfun=as.",xclass,"; yfun=as.",yclass,sep="")))
	if (!missing(y2) && length(y)==length(y2)){      # use y2 observations if any y-values missing
		z=is.na(y); if(any(z)) y[z]=y2[z] }
	z1=!is.na(x) & !is.na(y); x=x[z1]; y=y[z1]       # remove NAs
	z2=order(x);            ; x=x[z2]; y=y[z2]       # enforce ordering of time series
	n=length(x)
	xdiff=as.numeric(diff(x))
	xval=c(0,cumsum(xdiff))
	nmean=ceiling((max(xval)-period)/every)
	xma=rep(xfun(NA),nmean); yma=rep(yfun(NA),nmean); zma=rep(as.numeric(NA),nmean)
	# Slide the moving average backwards from the last observation
	x1=xval[n]; x0=x1-period+1
	for (i in 1:nmean) {
		z = xval>=x0 & xval<=x1
		xma[i]=rev(x[z])[1]              # grab the end value
		yma[i]=mean(y[z],na.rm=TRUE)
		zma[i]=rev(xval[z])[1]           # grab the end value
		x1=x1-every; x0=x1-period+1
	}
	ma=data.frame(x=xma,y=yma,z=zma)
	ma=ma[order(ma$x),]; rownames(ma)=1:nrow(ma) # Reverse the backwards accumulation
	attr(ma,"dat")=data.frame(x=x,y=y,z=xval)
	return(ma) }
#-------------------------------------------calcMA

#imputeRate-----------------------------2010-10-20
# Impute the rate of return from an investment 
# with irregular contributions/withdrawals.
#-----------------------------------------------RH
imputeRate <- function(qtName="Ex03_Portfolio", dbName="Examples", AID=1, pathN=2, hnam=NULL) {

	if (!require(PBSmodelling)) stop("`PBSmodelling` package is required")
	if (!require(stats)) stop("`stats` package is required")
	assign("PBStool",list(module="M06_Temporal",call=match.call(),args=args(imputeRate),plotname="impute"),envir=.PBStoolEnv)
	wdir <- paste(system.file(package="PBStools"),"/win",sep="")
	sdir <- switch(pathN,getwd(),.getSpath())
	tdir <- tempdir(); tdir <- gsub("\\\\","/",tdir)
	wdf  <- paste("imputeRate","Win.txt",sep="") # window description file
	doc  <- paste("imputeRate","Doc.pdf",sep="") # documentation
	wnam <- paste(wdir,wdf,sep="/")
	wtmp <- paste(tdir,wdf,sep="/")
	dtmp <- paste(wdir,doc,sep="/")
	temp <- readLines(wnam)
	temp <- gsub("@wdf",wtmp,temp)
	temp <- gsub("@doc",dtmp,temp) # not used
	temp <- gsub("@dbName",dbName,temp)
	temp <- gsub("@qtName",qtName,temp)
	temp <- gsub("@AID",AID,temp)
	temp <- switch(pathN,
		gsub("cwd value=1","cwd value=1 selected=TRUE",temp),
		gsub("sql value=2","sql value=2 selected=TRUE",temp) )
	writeLines(temp,con=wtmp)
	.onClose=function() { ttget(PBStool); save("PBStool",file="PBStool.rda") }
	assign(".onClose",.onClose,envir=.PBStoolEnv)
	createWin(wtmp);  .imputeRate.impIR()
	invisible() }

#.imputeRate.impIR----------------------2011-06-13
# Impute the rate of return from an investment.
#-----------------------------------------------RH
.imputeRate.impIR <- function() {
	IRRfun <- function(P){
		# See XIRR function in Excel
		r=as.numeric(P[1])
		unpackList(ttcall(PBStool)["ddat"],scope="L")
		N=nrow(ddat)
		Ci=c(ddat$value[1],ddat$cont[2:N]) # initial value (includes 1st contributuon) + additioanl contributions
		#Clast=Ci[N]; Ci[N]=0
		Vlast=ddat$value[N]                # final value
		#f=GT0(mean(abs(Ci)))               # scale factor for contributions
		f=1
		Ci=Ci/f
		Di=c(ddat$date)
		ri = (1-r)^(as.numeric(Di-Di[1])/365)  # Discounts
		ri[Ci<0] = 1                           # Do not discount removals
		DCi=Ci/ri                              # NPV
		Vcont=sum(DCi)
		Vlast=Vlast/f
		Yobs  = scalePar(matrix(c(Vlast,0,1.5*max(Vlast,Vcont),TRUE),nrow=1))
		Ypred = scalePar(matrix(c(Vcont,0,1.5*max(Vlast,Vcont),TRUE),nrow=1))
		fval = (Yobs-Ypred)^2
		#fval = (Vlast - Vcont)^2
#		print(round(c(r,fval,Vcont,Vlast),5))
		IRRmin=c(r,fval,f*Vcont,f*Vlast); names(IRRmin)=c("r","fval","Vcont","Vlast")
		#packList("IRRmin","PBStool",tenv=.PBStoolEnv)
		#eval(parse(text="PBStool$IRRmin <<- IRRmin"))  # weird results for SANN
		ttget(PBStool); PBStool$IRRmin <- IRRmin; ttput(PBStool)  # weird results for SANN
		return(fval) }
	#----------------
	getWinVal(scope="L")
	unpackList(ttcall(PBStool),scope="L")
	act=getWinAct()[1]
	cenv=environment(); penv=parent.frame(1); genv=.PBStoolEnv
	if (!isThere("dat",envir=cenv) ) {#|| (act=="impute" && attributes(dat)$fqt!=qtName)) {
		.imputeRate.getIR()
		unpackList(ttcall(PBStool),scope="L") }
	assign("PBStool",PBStool[c("module","func","plotname","dat","DLIM","PLIM")],envir=.PBStoolEnv)
	adat=dat[is.element(dat$AID,AID),] # account data
	if (nrow(adat)==0) showError("account ID","nodata")
	if (autoD) dlim=range(adat$date,na.rm=TRUE) else dlim=c(d0,d1)
	d0=dlim[1]; d1=dlim[2]
	ddat=adat[adat$date>=dlim[1] & adat$dat<=dlim[2] & !is.na(adat$date),] # date-delimited account data
	if (nrow(ddat)==0) showError("date limits","nodata")
	plim=range(ddat$period,na.rm=TRUE); v0=plim[1]; v1=plim[2]
	pyr=365/median(as.vector(diff(ddat$date)))
	setWinVal(list(d0=d0,d1=d1,v0=v0,v1=v1,pyr=round(pyr,1)))

	x <- ddat$period; y <- ddat$value; names(x)=names(y)=as.character(ddat$date)
	# Indexing with z redundant since 'ddat' is now a subet of 'adat'
	z <- is.element(x,v0:v1); zL <-sum(z);
	z0 <- is.element(x,v0); z1 <- is.element(x,v1);
	xobs <- x[z]; yobs <-y[z]; 
	cobs <- ddat$cont[z]; cobs[1]=yobs[1] # initial value already includes 1st contribution
	names(cobs)=as.character(ddat$date[z])
	Vstart <- yobs[1]; Vend <- yobs[zL]; Vadj=Vend-cobs[zL]
	packList(c("adat","ddat","Vstart","Vend","Vadj","x","y","z0","z1","z","zL","xobs","yobs","cobs"),"PBStool",tenv=.PBStoolEnv)
#browser();return()

	Obag <- calcMin(pvec=parVec,func=IRRfun,method=method,trace=trace,maxit=maxit,reltol=reltol,steptol=steptol,repN=repN);
	fmin <- PBSmin$fmin; np <- sum(parVec[,4]); ng <- zL;
	AICc = 2*fmin + 2*np * (ng/(ng-np-1))
	packList("AICc","PBSmin",tenv=.PBStoolEnv)
	Pend <- PBSmin$end  # Pend is annualized if IRRfun()
	ftime <- PBSmin$time;
	rates=c(Pend/pyr,Pend)  # period and annual rates

	Pfig <- signif(rates*100,3)
	if (sim==2) sim <- 1
	unpackList(Obag,scope="L");
	Gbag <- list(Git=iters, Gev=evals, Gct=round(cpuTime,nd), Get=round(elapTime,nd),
		Gf1=round(fminS,nd), Gf2=round(fminE,nd), Gaic=round(AIC,nd), Gaicc=round(PBSmin$AICc,nd),
		Gv1=c(Pstart,((1+Pstart)^pyr)-1), Gv2=round(rates,nd), Gmess=message, sim=sim);
	setWinVal(Gbag)
	packList(c("Obag","Gbag","Pend","rates","Pfig","ftime"),"PBStool",tenv=.PBStoolEnv)
	.imputeRate.plotIR()
	invisible() }

#.imputeRate.trajIR---------------------2011-06-10
# Calculate the value trajectory.
#-----------------------------------------------RH
.imputeRate.trajIR=function(x,cw,y0,r){ 
	# x=periods, cw=contributions/withdrawals, y0=starting value, r=period rate
	cw[1]=y0 # initial value already includes 1st contribution
	ri = (1-r)^(as.numeric(x-x[1])/365)
	ri[cw<0] = 1
	ypred=cw/ri  # NPV
	y=cumsum(ypred)
	return(y) }

#.imputeRate.getIR----------------------2010-10-20
# Get the master data set to impute rate.
#-----------------------------------------------RH
.imputeRate.getIR <- function(){
	act <- getWinAct()[1]; getWinVal(scope="L")
	if (!is.null(act) && act=="sim") dat=ttcall(PBStool)$dat 
	else {getData(qtName,dbName,type="MDB",path=switch(pathN,getwd(),.getSpath()),tenv=penv()); dat <- PBSdat }
	if (!is.data.frame(dat)) showError("Specify a valid SQL table/query or Simulate data")
	dat$year=as.numeric(substring(dat$date,1,4)); dat$month=as.numeric(substring(dat$date,6,7))
	dat$date <- as.Date(dat$date); dat=dat[order(dat$AID,as.numeric(dat$date)),]
	plist=sapply(split(dat$AID,dat$AID),function(x){1:length(x)},simplify=FALSE)
	dat$period=as.vector(unlist(plist))
	if (autoD) {
		DLIM=range(dat$date,na.rm=TRUE)
		PLIM=range(dat$period[is.element(dat$AID,AID)],na.rm=TRUE)
		setWinVal(list(d0=DLIM[1],d1=DLIM[2],v0=PLIM[1],v1=PLIM[2])) }
	dat$cont[is.na(dat$cont)] <- 0; 
	packList(c("dat","DLIM","PLIM"),"PBStool",tenv=.PBStoolEnv)
	invisible() }

#.imputeRate.plotIR---------------------2011-06-10
# Plot the results of the imputed rate.
#-----------------------------------------------RH
.imputeRate.plotIR=function() {
	getWinVal(scope="L")
	unpackList(ttcall(PBStool),scope="L")
	cenv=environment(); penv=parent.frame(1); genv=.PBStoolEnv
	if (!isThere("ddat",envir=cenv)) {.imputeRate.impIR(); return()}
	resetGraph(); expandGraph(mar=c(5,2.5,1.5,1.5))
	clrs <- c("red","blue","green4","purple4","cornflowerblue","darkorange3","darkolivegreen");
	clr  <- clrs[match(method,c("nlminb","nlm","Nelder-Mead","BFGS","CG","L-BFGS-B","SANN"))];
	oclr <- c("grey40","grey90")                 # observed: lines, points
	mclr <- c("forestgreen","red","darkorange"); # contributions: positive, negative, cumulative
	ycum <- .imputeRate.trajIR(x=as.Date(names(xobs)),cw=cobs,y0=Vstart,r=rates[2])
	ccum <- cumsum(cobs)
	xlim <- range(x) + c(-.5,.5);
	yglob<- c(ddat$value,ycum,ccum,cobs,0)
	ylim = extendrange(yglob,f=0.01) #range(yglob,na.rm=TRUE)
	#print(paste("Vend =",Vend," Vest=",round(Vest,2)," Vit =",round(ycum[zL],2)));
	print(round(IRRmin,5))

	plot(x,y,cex.axis=1,mgp=c(2.5,0.5,0),xlim=xlim,ylim=ylim,xlab="", ylab="",type="n",xaxt="n",yaxt="n",bty="n")
	zax=intersect(x,pretty(x,15))
	zay=intersect(floor(ylim[1]):ceiling(ylim[2]),pretty(yglob,10))
	zpos=cobs>0 & !is.na(cobs); zneg=cobs<0 & !is.na(cobs)
	cadd=abs(min(0,min(cobs)))
	base=0; rcobs=cobs+base
	zac=pretty(cobs,4)

	abline(h=base,col="gainsboro")
	axis(1,at=zax,labels=names(x)[is.element(x,zax)],las=3,cex.axis=.8,tck=.005,mgp=c(0,.2,0))
	axis(2,at=zay,labels=format(zay,big.mark=","),las=0,cex.axis=.8,tck=.005,mgp=c(0,.2,0))
	axis(4,at=zac+base,labels=zac,las=0,cex.axis=.7,tck=.005,mgp=c(0,0,0))
	
	expr = c("dBars =",deparse(drawBars)); expr[length(expr)]= "invisible(xy) }"
	eval(parse(text=expr))
	
	if (any(zpos==TRUE)){
		xy = dBars(xobs[zpos],rcobs[zpos],width=1,col=mclr[1],base=base,lwd=1)
		polygon(xy,col="lightgreen",border="grey") }
	if (any(zneg==TRUE)) {
		xy = dBars(xobs[zneg],rcobs[zneg],width=1,col=mclr[2],base=base,lwd=1)
		polygon(xy,col="pink",border="grey") }
	lines(x,y,col=oclr[1],lwd=1); points(x,y,pch=21,bg=oclr[2],cex=1);
	lines(xobs,ccum,col=mclr[3],lwd=2); lines(xobs,ycum,col=clr,lwd=2);

	mtext("Date",side=1,line=3.75,cex=1.2); mtext("Value",side=2,line=1.5,cex=1.2);
	ydif=diff(ylim); ypin=par()$pin[2]
	yyy = ifelse(Vstart < (0.80*ydif+ylim[1]),.98,.50)
	addLabel(.05,yyy,paste("Method =",method),cex=1.2,adj=0,col=clr);
	addLabel(.05,yyy-.02*(6/ypin),paste(paste(c("Period rate","Annual rate")," = ",Pfig,"%",sep=""),collapse="\n"),adj=c(0,1),cex=0.8);
	addLabel(.05,yyy-.08*(6/ypin),paste("Timing =",paste(round(ftime[1],2),collapse=", "),"sec"),adj=0,cex=0.7,col="grey35");
	addLegend(.04,yyy-.09*(6/ypin),legend=c("Observed","Contributions","Growth"),bty="n",cex=.8,
		lty=1,lwd=3,col=c(oclr[1],mclr[3],clr));
	clab=paste("Initial value = ",format(round(Vstart),big.mark=","),
		"    Contributions = ",format(round(sum(ddat$cont[z][-1])),big.mark=","),
		"    Final value = ",format(round(Vend),big.mark=","),sep="")
	addLabel(.99,.01,clab,adj=c(1,0),cex=0.7,col=mclr[3]);
	packList(c("ycum","ccum","clr"),"PBStool",tenv=.PBStoolEnv)
	packList(c("xlim","ylim","yyy","zax","zay","zac","rcobs","zpos","zneg","base"),"PBStool",tenv=.PBStoolEnv)
	box(bty="L"); invisible() }

#.imputeRate.resetIR--------------------2010-10-20
# Reset/reinitialize the GUI.
#-----------------------------------------------RH
.imputeRate.resetIR=function(){
	act <- getWinAct()[1]; if (is.null(act)) return()
	getWinVal(scope="L")
	unpackList(ttcall(PBStool),scope="L")
	basic=list(Git=0,Gev=0,Gct=0,Get=0,Gf1=0,Gf2=0,Gaic=0,Gaicc=0,Gv1=c(0,0),Gv2=c(0,0))
	if (act=="reSet") {
		pvec=data.frame(val=0.02,min=-3,max=3,active=TRUE,row.names="rate")
		reSet=c(basic,list(parVec=pvec,d0=DLIM[1],d1=DLIM[2],v0=PLIM[1],v1=PLIM[2],
			Gmess="------------\nparVec reset with initial starting values.\n------------"))
		setWinVal(reSet)
		packList("reSet","PBStool",tenv=.PBStoolEnv) }
	else if (act=="reInit") {
		pvec=parVec; pvec[,1]=Gv2[1]
		reInit=c(basic,list(parVec=pvec,
			Gmess="------------\nparVec reset with last set of estimated parameters.\n------------"))
		setWinVal(reInit)
		packList("reInit","PBStool",tenv=.PBStoolEnv) }
	invisible() }

#.imputeRate.simIR----------------------2010-10-20
# Simulate a data set for imputing.
#-----------------------------------------------RH
.imputeRate.simIR <- function() {
	rpareto <- function(n,xm,k) {
		y = runif(n); x = xm*(1-y)^(-1/k); return(x) }
	getWinVal(scope="L"); #unpackList(ttcall(PBStool),scope="L")
	assign("PBStool",PBStool[c("module","func","plotname")],envir=.PBStoolEnv)
	if (k!=0 && k<1) { resetGraph();
		setWinVal(list(k=20,sim=1))
		showError("Pareto distribution parameter k:  Volatility decreases as k increases;  Choose k>=1  Try k=20") }
	val <- rep(start,nper); cont <- round(runif(nper,10,100)); cont[1] <- 0;
	up <- min(max(up,0),1); updown <- rbinom(nper,1,up); updown[is.element(updown,0)] <- -1;
	cont <- cont * updown;
	for (i in 2:nper) {
		#val[i] <- (val[i-1]*(1+rate) + cont[i]) * exp(rnorm(1,mean=-sigma^2/2,sd=sigma))
		parrot = ifelse(k==0,1,rpareto(1,xm=1,k=k)) # pareto noise
		val[i] <- max(0, (val[i-1]+cont[i])); val[i]=val[i]*(1+rate)*parrot
		if (val[i]<=0 && cont[i]<=0) cont[i]=-val[i-1]
	}
	dat=data.frame(AID=rep(AID,nper),date=seq(as.Date("2009/1/1"),by="month",length.out=nper),
		value=val,cont=cont)
	attr(dat,"fqt") = "Simulation"
	packList("dat","PBStool",tenv=.PBStoolEnv)
	.imputeRate.getIR(); .imputeRate.impIR()
	invisible()}
#---------------------------------------imputeRate

#plotDiversity--------------------------2013-01-28
# Plot diversity as barplots and overlay species richness.
#-----------------------------------------------RH
plotDiversity <- function (fqtName="Ex01_Sample_Info",dbName="Examples.mdb",
     type="MDB",path=getwd(),bars="H", pnts="S", xnames=c("SID","Batch"),
     xint=40,clrs=c("skyblue","gold","blue","green4"), addlowess=TRUE, f=0.2, 
     pdf=FALSE, wmf=FALSE, ioenv=.GlobalEnv) {

	par0 <- par(no.readonly = TRUE)
	on.exit(par(par0))

	fnam=paste("Diversity-",bars,"x",pnts,sep="")
	assign("PBStool",list(module="M06_Temporal",call=match.call(),ioenv=ioenv,plotname=fnam),envir=.PBStoolEnv)
	if (type=="FILE") {
		eval(parse(text=paste("getFile(",fqtName,",senv=ioenv,try.all.frames=TRUE,tenv=penv()); dat=",fqtName,sep=""))) }
	else {
		getData(fqtName,dbName,type=type,path=path,tenv=penv()) 
		dat=PBSdat }
	index=c("Shannon-Wiener diversity H", "Species richness S", "Species evenness E")
	names(index)=c("H","S","E")

	ypick=function(y) {
		ylim=extendrange(y,f=0.05); ylim[1]=0
		if (ylim[2]<1) fac=10^(-floor(log10(ylim[2]))) else fac=1
		ylim=ylim*fac
		ypos=ceiling(ylim[1]):floor(ylim[2]); ny=length(ypos)
		ypos=ypos[seq(1,ny,round(ny/min(6,ny)))]
		dfy=diff(ypos)[1]
		yint=diff(pretty(ypos[1]:dfy))[1]
		ytck=seq(ylim[1],ypos[length(ypos)],yint) #diff(ypos)[1]/4	)
		list(ylim=ylim/fac,ypos=ypos/fac,ytck=ytck/fac)
	}

	if (any(names(dat)=="YMD")) dat$date=as.POSIXct(dat$YMD)
	xrng <- range(dat$date)
	#yr <- sort(unique(as.numeric(substring(dat$YMD,1,4)))); nyr <- length(yr)
	xnams=apply(dat[,xnames,drop=FALSE],1,paste,collapse="-")
	nx=length(xnams)
	znams=seq(1,nx,round(nx/min(xint,nx)))
	xshow=rep("",nx); xshow[znams]=xnams[znams]; xchar=max(nchar(xshow))
	unpackList(ypick(dat[,bars]),scope="L")
	
	#if (eps) postscript(file=paste(fnam,".eps",sep=""),width=11,height=8.5,paper="special")
	if (pdf) pdf(file=paste(fnam,".pdf",sep=""),width=11,height=8.5)
	else if (wmf) win.metafile(paste(fnam,".wmf",sep=""),width=11,height=8.5)
	else resetGraph()
	expandGraph(mar=c(max(3.5,ceiling(xchar^.7)),3.75,1,3.75),mgp=c(2.75,.75,0))
	x=barplot(height=dat[,bars],names.arg=xshow,space=0,col=clrs[1],cex.names=0.7,
		las=2,mgp=c(0,0.2,0),tck=.01,yaxt="n",xaxs="i",ylim=ylim)
	if (addlowess) lines(lowess(x,dat[,bars],f=f),col=clrs[3],lwd=2)
	axis(2,at=ytck,tck=-.005,labels=FALSE); axis(2,at=ypos,las=1)
	mtext(ifelse(any(bars==c("H","S","E")),index[bars],bars),side=2,line=2,cex=1.2)
	mtext("Sample identification",side=1,line=par()$mar[1]-1.5,cex=1.2)
	xlim=par()$usr[1:2]

	par(new=TRUE)
	unpackList(ypick(dat[,pnts]),scope="L")
	plot(x,dat[,pnts],type="n",xlim=xlim,xaxs="i",ylim=ylim,yaxs="i",axes=FALSE,xlab="",ylab="")
	abline(h=ypos[ypos>0],col="grey",lty=ifelse(pdf,1,3))
	if (addlowess) lines(lowess(x,dat[,pnts],f=f),col=clrs[4],lwd=2)
	points(x,dat[,pnts],pch=21,bg=clrs[2])
	axis(4,at=ytck,tck=-.005,labels=FALSE); axis(4,at=ypos,las=1)
#browser();return()
	mtext(ifelse(any(pnts==c("H","S","E")),index[pnts],pnts),side=4,line=1.75,cex=1.2)

	legend("topleft",pch=c(22,21),pt.bg=clrs[1:2],legend=c(bars,pnts),
		text.width=diff(par()$usr[1:2])*ifelse(addlowess,.10,.05),bg="aliceblue")
		if (addlowess) addLegend(.06,1,lty=c(1,1),col=clrs[3:4],legend=c("",""),bty="n")
	box()
	if (pdf|wmf) dev.off()
	packList(c("dat","x","xnams","xshow"),"PBStool",tenv=.PBStoolEnv)
	invisible() }
#------------------------------------plotDiversity

#trackComp------------------------------2013-01-28
# Track the composition of phytoplankton groups
#-----------------------------------------------RH
trackComp = function(fqtName=c("Ex01_Sample_Info","Ex02_Species_Abundance"),
     dbName="Examples.mdb", type="MDB", path=getwd(), ndays=15, groups=NULL, 
     dlim=c("2007-01-01",substring(Sys.time(),1,19)),
     clrs=c("green","dodgerblue","orange","yellow","red","grey"),
     pdf=FALSE, wmf=FALSE, ioenv=.GlobalEnv, ...) {

	par0 <- par(no.readonly = TRUE)
	on.exit(par(par0))
	dots=list(...)
	fnam=paste("Comp",paste(dots$co,collapse="-"),"-Every-",ndays,"days",sep="")
	assign("PBStool",list(module="M06_Temporal",call=match.call(),ioenv=ioenv,plotname=fnam),envir=.PBStoolEnv)
	if (type=="FILE") {
		eval(parse(text=paste("getFile(",fqtName[1],",senv=ioenv,try.all.frames=TRUE,tenv=penv()); dat=",fqtName[1],sep=""))) }
	else {
		eval(parse(text=paste("getData(\"",fqtName[1],"\",\"",dbName,"\",type=\"",type,"\",path=\"",path,"\",tenv=penv())",sep="")))
		dat=PBSdat }
	if (any(names(dat)=="YMD")) dat$date=as.POSIXct(dat$YMD)
	flds=names(dat)
	up=is.element(flds,c("PID","SID","EID","POS","YMD"))
	flds[!up]=tolower(flds[!up])
	names(dat)=flds
	dlim = as.POSIXct(dlim)
#browser();return()
	dat  = dat[dat$date>=dlim[1] & dat$date<=dlim[2] & !is.na(dat$date),]
	if (nrow(dat)==0) showError("Choose different date limits")
	if (any(flds=="dominant")) dat=dat[dat$dominant!="Zilcho" & dat$dominant!="" & !is.na(dat$dominant),]
	if (length(dots)>0) {
		unpackList(dots,scope="L")
		for (i in dots)
			eval(parse(text=paste("dat=biteData(dat,",i,")",sep="")))
	}
	yper=convYP(dat$date,ndays)
	dat$x=yper; dat$lab=names(yper)
	
	#Group proportions
	if (type=="FILE") {
		eval(parse(text=paste("getFile(",fqtName[2],",senv=ioenv,try.all.frames=TRUE,tenv=penv()); gdat=",fqtName[2],sep=""))) }
	else {
		eval(parse(text=paste("getData(\"",fqtName[2],"\",\"",dbName,"\",type=\"",type,"\",path=\"",path,"\",tenv=penv())",sep="")))
		gdat=PBSdat }
	gdat = gdat[is.element(gdat$SID,sort(unique(dat$SID))),]
	if (is.null(groups))
		groups=c("Skeletonema","Thalassiosira","Chaetoceros","Other Diatoms","Phytoflagellates","Grazers")
	px=sort(sapply(split(yper,names(yper)),unique))
	plob=list(x=px, lab=names(px)) # plot object
	for (i in groups) {
		idat=gdat[is.element(gdat$Group,i),]
		gA=idat$gA; names(gA)=idat$SID
		dat[,i] = gA[as.character(dat$SID)] 
		dat[is.na(dat[,i]),i]=0 
		plob[[i]]=sapply(split(dat[,i],dat$x),mean)  }
	tmp=as.data.frame(plob[groups])
	dfpic = data.frame(x=plob$x,base=rep(0,length(plob$x)),t(apply(tmp,1,cumsum)))
#browser();return()
	
	if (pdf) pdf(file=paste(fnam,".pdf",sep=""),width=11,height=8.5)
	else if (wmf) win.metafile(paste(fnam,".wmf",sep=""),width=11,height=8.5)
	else resetGraph()
	expandGraph(mar=c(4.5,3.5,0.5,1.25),oma=c(0,0,0,0),las=1,fig=c(0,1,0,.95))
	plot(dfpic$x,dfpic$base,ylim=c(0,100),xlab="",ylab="",type="l",xaxs="i",yaxs="i",xaxt="n")
	dfgroups=gsub(" ",".",groups)
	for(i in 1:length(dfgroups)) {
		ii=dfgroups[i];  iii=ifelse(i==1,"base",dfgroups[i-1])
		xpol=c(dfpic$x,rev(dfpic$x))
		ypol=c(dfpic[,ii],rev(dfpic[,iii]))
		polygon(xpol,ypol,col=clrs[i],border=clrs[i])
	}
	abline(h=seq(10,90,10),v=sort(unique(round(dfpic$x))),col="moccasin",lty=3)
	axis(1,at=dfpic$x,labels=row.names(dfpic),las=2,tck=.01,mgp=c(0,.2,0),cex.axis=.8)
	mtext(paste("Year-period (interval =",ndays,"days)"),side=1,las=1,line=3,cex=1)
	mtext("Relative Composition (%)",side=2,las=0,line=2,cex=1.2)
	box(); 
	packList(c("dat","gdat","plob","dfpic"),"PBStool",tenv=.PBStoolEnv)
	#Legend
	#par(new=TRUE,mar=c(0,2,1.25,0),fig=c(0,1,.95,1))
	par(new=TRUE,mar=c(0,1,0.1,0),fig=c(0,1,.95,1))
	plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
	addLegend(.05,.5,fill=clrs,legend=groups,horiz=TRUE,bty="n",yjust=.5)
	if (pdf|wmf) dev.off()
	invisible() }
#----------------------------------------trackComp

