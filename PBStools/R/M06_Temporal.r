#===============================================================================
# Module 6: Temporal
# ------------------
#  calcMA..........Calculate a moving average using a fixed period occurring every x units.
#  imputeRate......Impute rate of return from an investment with irregular contributions/withdrawals.
#===============================================================================


#calcMA---------------------------------2011-05-05
# Calculate a moving average using a fixed
# period occurring every x units.
#-----------------------------------------------RH
calcMA = function(x,y,y2,period=270,every=10)
{
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
	return(ma)
}
#-------------------------------------------calcMA


#imputeRate-----------------------------2010-10-20
# Impute the rate of return from an investment 
# with irregular contributions/withdrawals.
#-----------------------------------------------RH
imputeRate <- function(qtName="Ex03_Portfolio", dbName="Examples", AID=1, pathN=2, hnam=NULL)
{
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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~imputeRate

#.imputeRate.impIR----------------------2013-02-01
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
	ttget(PBStool)
	unpackList(PBStool,scope="L")
	act=getWinAct()[1]
	cenv=environment(); penv=parent.frame(1); genv=.PBStoolEnv
	if (!isThere("dat",envir=cenv) ) {#|| (act=="impute" && attributes(dat)$fqt!=qtName)) {
		.imputeRate.getIR()
		ttget(PBStool)
		unpackList(PBStool,scope="L") }
	assign("PBStool",PBStool[c("module","call","args","plotname","dat","DLIM","PLIM")],envir=.PBStoolEnv)
	adat=dat[is.element(dat$AID,AID),] # account data
	if (nrow(adat)==0) showError("account ID","nodata")
	if (autoD) dlim=range(adat$date,na.rm=TRUE) else dlim=c(d0,d1)
	d0=dlim[1]; d1=dlim[2]
	ddat=adat[adat$date>=dlim[1] & adat$dat<=dlim[2] & !is.na(adat$date),] # date-delimited account data
	if (nrow(ddat)==0) showError("date limits","nodata")
	plim=range(ddat$period,na.rm=TRUE); v0=plim[1]; v1=plim[2]
	pyr = 365./median(as.vector(diff(ddat$date)))
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

	Obag <- calcMin(pvec=parVec,func=IRRfun,method=method,trace=trace,maxit=maxit,reltol=reltol,steptol=steptol,repN=repN)
	tget(PBSmin)
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
		Gf1=round(fminS,nd), Gf2=round(fminE,nd), Gaic=round(AIC,nd), Gaicc=round(AICc,nd),
		Gv1=c(Pstart,((1+Pstart)^pyr)-1), Gv2=round(rates,nd), Gmess=message, sim=sim);
	tput(PBSmin)
	setWinVal(Gbag)
	packList(c("Obag","Gbag","Pend","rates","Pfig","ftime"),"PBStool",tenv=.PBStoolEnv)
	.imputeRate.plotIR()
	invisible() }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.imputeRate.impIR

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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.imputeRate.trajIR

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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.imputeRate.getIR

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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.imputeRate.plotIR

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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.imputeRate.resetIR

#.imputeRate.simIR----------------------2013-02-01
# Simulate a data set for imputing.
#-----------------------------------------------RH
.imputeRate.simIR <- function() {
	rpareto <- function(n,xm,k) {
		y = runif(n); x = xm*(1-y)^(-1/k); return(x) }
	getWinVal(scope="L")
	ttget(PBStool)
	assign("PBStool",PBStool[c("module","call","args","plotname")],envir=.PBStoolEnv)
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
	invisible()
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.imputeRate.simR

#=======================================imputeRate

