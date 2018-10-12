## =============================================================================
## Module 6: Assessment
## --------------------
##  calcMA..........Calculate a moving average using a fixed period occurring every x units.
##  compAF..........Compare age frequencies using discrete or cumulative distribution plots.
##  compBmsy........Compare biomass posteriors relative to Bmsy or Bavg.
##  imputeRate......Impute rate of return from an investment with irregular contributions/withdrawals.
##  plotAgeErr......Plot ageing precision data from primary and secondary readers.
##  plotMW..........Plot annual mean weights by stock and by PMFC area.
##  quantAges.......Plot quantile boxes of age by year and/or area, including mean age over time.
## =============================================================================


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


## compAF-------------------------------2018-07-17
## Compare age frequencies using discrete or
## cumulative distribution plots.
## ---------------------------------------------RH
compAF=function(x, year=2003, sex=2, amax=40, pfld="wp",
   png=FALSE, outnam, clrs=c("red","black"), ltys=1, 
   type="cumul", lang=c("e","f"))
{
	if (length(x)==0) stop("Supply a named list for x")
	std   = function(x){x/sum(x)}
	ncomp = length(x)
	nsex  = length(sex)
	ntype = length(type)
	years = sapply(x,function(xx){
		noto=apply(xx[,,,"n",drop=FALSE],2,sum,na.rm=TRUE)
		yrs = as.numeric(names(noto[noto>0]))
		return(yrs)
	} )
	year  = intersect(year,.su(unlist(years)))
	nyear = length(year)
	col   = lucent(rep(clrs,ncomp)[1:ncomp],0.5)
	lty   = rep(ltys,ncomp)[1:ncomp]

	fout = fout.e = outnam
	for (l in lang) {
		if (l=="f") fout = paste0("./french/",fout.e)  ## could repeat for other languages
		if (png) png(file=paste0(fout,".png"), units="in", res=600, width=10, height=7.5)
		if (ntype==1 && nsex==1) {
			rc = .findSquare(nyear)
			np = 0  ## keep track of # plots
			par(mfrow=rc, mar=c(0,0,0,0), oma=c(4,4,0.5,0.5), mgp=c(1.6,0.5,0))
		} else {
			rc = c(ntype,nsex)
			par(mfcol=rc, mar=c(0,0,0,0), oma=c(3.5,3.5,0.5,0.5), mgp=c(1.6,0.5,0))
		}
	
		xnam = names(x)
		for (y in year) {
			for (s in sex) {
				yy = as.character(y)
				ss = as.character(s)
				xvec = list()
				for (i in 1:length(x)) {
					ii = names(x)[i]
					xmat = x[[ii]][,,ss,pfld,drop=FALSE]
					if (!is.element(yy,colnames(xmat))) xvec[[ii]] = NA # next
					else xvec[[ii]] = std(x[[i]][,yy,ss,pfld])
				}
				nvec   = length(xvec)
				nord   = match(names(xvec),xnam)
				notos  = sapply(x,function(xx){
					if(!is.element(yy,dimnames(xx)$year)) 0 
					else sum(xx[,yy,ss,"n"])
				})
				legtxt = paste0(names(notos)," ",round(notos)," otos")
				ylim = c(0,max(sapply(xvec,max),na.rm=TRUE))
	
				if ("discr" %in% type) {
					plot(0,0, xlim=c(1,amax), ylim=ylim, type="n", xlab="", ylab="", xaxt="n", yaxt="n")
					sapply(nord, function(n){
						if (!all(is.na(xvec[[n]])))
							lines(1:length(xvec[[n]]), xvec[[n]], col=col[n], lty=lty[n], lwd=ifelse(n==1,3,2)) } )
					addLabel(0.95, 0.95, paste0(yy, " - ", linguaFranca(switch(s,"Male","Female"),l)), adj=1)
				}
				if ("cumul" %in% type) {
					plot(0,0, xlim=c(1,amax), ylim=c(0,1), type="n", xlab="", ylab="", xaxt="n", yaxt="n")
					np = np + 1
					if (all(notos==0))
						addLabel(0.5, 0.5, linguaFranca("NO DATA",l), col="red", cex=1.2)
					else 
						abline(h=seq(0.1,0.9,0.1), v=seq(5,amax-5,5), col=lucent("grey",0.5))
	#if (y==2014) {browser();return()}
					sapply(nord, function(n){
						if (!all(is.na(xvec[[n]]))) {
							lines(1:length(xvec[[n]]), cumsum(xvec[[n]]), col=col[n], lty=lty[n], lwd=ifelse(n==1,3,2))
						}
					} )
					addLabel(0.05, 0.95, paste0(yy, ifelse(np==1, linguaFranca(switch(s," - Male"," - Female"),l), "")), adj=c(0,1), cex=1.2)
					if (par()$mfg[2]==1) {
						axis(2, at=seq(0,1,0.1), tcl=-0.25, labels=FALSE)
						axis(2, at=seq(0.2,1,0.2), labels=TRUE, cex.axis=1.1, las=1)
					}
					if (np > nyear-rc[2]) {
						axis(1, at=seq(0,amax,5), tcl=-0.25, labels=FALSE)
						axis(1, at=seq(10,amax,10), labels=TRUE, cex.axis=1.1, las=1)
					}
				}
				if (type=="cumul")
					addLegend(0.975,0.05, bty="n", lty=lty, seg.len=1, col=col, legend=linguaFranca(gsub("_"," ",legtxt),l), yjust=0, xjust=1, lwd=2, cex=0.9)
				else
					addLegend(0.025,0.975,bty="n", lty=lty, seg.len=1, col=col, legend=linguaFranca(gsub("_"," ",legtxt),l), yjust=1, xjust=0, lwd=2, cex=0.9)
			} ## end s (sex) loop
		} ## end y (year) loop
		mtext (linguaFranca("Age",l), side=1, outer=TRUE, line=2.5, cex=1.5)
		mtext (linguaFranca(paste0(ifelse(type=="cumul","Cumulative ",""), "Frequency"),l), side=2, outer=TRUE, line=2.5, cex=1.25)
	#browser();return()
		if(png) dev.off()
	} ## end l (lang) loop
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~compAF


## compBmsy-----------------------------2018-08-02
## Compare biomass posteriors relative to Bmsy or Bavg
## ---------------------------------------------RH
compBmsy = function(Bspp, spp="POP", Mnams=c("Est M","Fix M"),
   ratios=c(0.4,0.8), oratios=NULL, t.yr=2018,
   quants=c(0.05,0.25,0.5,0.75,0.95),
   zones = c("Critical","Cautious","Healthy"),
   figgy=list(win=T), pngres=400, width=12, height=9, 
   rcol=c("red","green4","blue"),    ## line cols
   ocol = c("#D55E00", "#009E73", "#56B4E9", "#F0E442"), ## dot cols for colour-blind humans (vermillion, bluegreen, skyblue, yellow)
   lcol = c("red","darkorange","green4"),
   spplabs=TRUE, left.space=NULL, top.space=2, fout=NULL, 
   offset=c(-0.1,0.1), calcRat=TRUE, refpt="MSY", param=NULL, 
   boxlim=NULL, lang=c("e","f"), ...)
{
	oldpar = par(no.readonly=TRUE); oldpso = grDevices::ps.options()
	ciao = function() {
		par(oldpar)
		mess = paste("grDevices::ps.options(",paste(paste(names(oldpso),sapply(oldpso,deparse),sep="="),collapse=","),")",sep="")
		eval(parse(text=mess))
		gc(verbose=FALSE) }
	#on.exit(ciao())

	spp = unique(spp)
	spp = spp[is.element(spp,names(Bspp))]
	if (length(spp)==0) 
		stop (paste("Input list 'Bspp' contains no species labelled: (\"",paste(spp,collapse="\",\""),"\")",sep=""))
	Nmods  = sapply(Bspp,length); SPP = names(Nmods); nmods=sum(Nmods[spp])
	Nrats  = length(ratios)   ## reference point ratios
	Norats = length(oratios) ## other ratios (shorter lines)
	Bmsy   = list()
	for (i in spp) {
		iBspp = Bspp[[i]]
		if (is.null(Mnams)) Mnams = names(iBspp)
		names(iBspp) = if (spplabs) paste(i,"\n",Mnams,sep="") else Mnams
		if (is.null(param) && calcRat) {
			iBmsy = sapply(iBspp,function(x) { x[["Bt.MCMC"]] / x[["Bmsy.MCMC"]] }, simplify=FALSE)
		} else if (!is.null(param)) {
			iBmsy = sapply(iBspp,function(x){x[["P.MCMC"]][[param]]},simplify=FALSE)
			if (all(sapply(iBmsy,is.null))) stop("This parameter does not exist")
		} else {
			iBmsy = iBspp
		}
		for (j in names(iBmsy)) {
			Bmsy[[j]] = iBmsy[[j]]
		}
	}
	Bmsy = Bmsy[rev(names(Bmsy))]

	if (is.null(boxlim))
		ylim = c(ifelse(is.null(param),0,min(sapply(Bmsy,quantile,0.0))),max(sapply(Bmsy,quantile,0.96)))
	else
		ylim = boxlim

	dots=list(...)
	unpackList(dots)
#browser();return()
	if (!is.null(dots$medcol)) medcol=rev(medcol)
	if (!is.null(dots$boxfill)) boxfill=rev(boxfill)

	#if (figgy) figout = c("eps","pdf","png","wmf") else figout="win"   ## don't need all these formats any more...
	figout = sapply(figgy,function(x){x})
	figout = names(figout)[figout]
#browser();return()

	if (is.null(fout))
		fout = fout.e = paste("CompBmsy-",paste(spp,collapse="+"),"-(",paste(gsub(" ","",Mnams),collapse=","),")",sep="")
	else
		fout.e = fout

	for (l in lang) {
		if (l=="f") fout = paste0("./french/",fout.e)  ## could repeat for other languages
		for (f in figout) {
			if (f=="eps"){    grDevices::ps.options(horizontal = FALSE)
			                  postscript(file=paste(fout,".eps",sep=""),width=width*1.25,height=height,fonts="mono",paper="special") }
			if (f=="pdf"){    grDevices::ps.options(horizontal = TRUE)
			                  pdf(file=paste(fout,".pdf",sep=""),width=width*1.25,height=height*1.25,fonts="mono") }
			else if (f=="png") png(paste(fout,".png",sep=""), units="in", res=pngres, width=width, height=height)
			else if (.Platform$OS.type=="windows" && f=="wmf") 
				do.call("win.metafile",list(filename=paste(fout,".wmf",sep=""), width=width*1.25, height=height*1.25))
			if (is.null(left.space))
				left.space = (max(nchar(names(Bmsy)))-ifelse(spplabs,nchar(spp),0))^0.9
			par(mar=c(4,left.space,0.5,0.5),cex=ifelse(f%in%c("png","eps"),1,1.2),mgp=c(1.6,0.6,0))
			quantBox(Bmsy, horizontal=TRUE, las=1, xlim=c(0.5,nmods+top.space), ylim=ylim, cex.axis=1.2, yaxs="i", outline=FALSE,
				pars=list(boxwex=boxwidth,medlwd=2,whisklty=1),quants=quants,names=FALSE)
	
			if (Nrats>0)
				abline(v=ratios,col=rep(rcol,Nrats)[1:Nrats],lty=2,lwd=2)
	
			#	segments(v=ratios,col=rep(c("red","green4","blue"),Nrats)[1:Nrats],lty=2,lwd=2)
			quantBox(Bmsy, horizontal=TRUE, las=1, xlim=c(0.5,nmods+1), ylim=ylim, cex.axis=1.2, yaxs="i", outline=FALSE, names=linguaFranca(names(Bmsy),l), pars=list(boxwex=boxwidth, medlwd=2, whisklty=1, medcol=medcol, boxfill=boxfill, ...), add=TRUE, quants=quants)
#browser();return()
			if (length(Bmsy)==1)  ## for some reason, a label is not added when there is only 1 boxplot.
				axis(2, at=1, labels=linguaFranca(names(Bmsy),l), las=1, cex.axis=1.2)
	
			if (Norats>0){
				orange = attributes(oratios)$range
				if (!is.null(orange)){
					if (all(sapply(orange,is.vector)))
						orange = sapply(orange, function(x){ as.matrix(x,ncol=1) }, simplify=FALSE) ## this line not not tested
					sapply(1:ncol(oratios), function(x){
						xy = cbind(orange$low[,x], orange$high[,x], rep(NA,nrow(oratios)))
						yy = as.vector(t(xy))
						xx = rep(rev(1:nrow(oratios)),each=3) + offset[x]  ## currently assumes only two HRPs
						lines(yy, xx, col=ocol[x], lty=1, lwd=1.5)
	#browser();return()
					})
				}
				if (is.vector(oratios))
					oratios =  as.matrix(oratios, ncol=1)
				sapply(1:ncol(oratios), function(x) {
					xx = oratios[,x]
					points(rev(xx), (1:length(xx)) + offset[x], pch=21, col="black", bg=ocol[x], cex=ifelse(nrow(oratios)>4,1,1.2))
				})
				#xrat = rbind(matrix(rep(orats,each=2),ncol=Norats))
				#yrat = rbind(matrix(rep(c(0,nmods),Norats),ncol=Norats))
				#ocol = rep(c("blue","purple","navy"),Norats)[1:Norats]
				#junk = sapply(1:Norats,
				#	function(i,x,y,col,lty){lines(x[,i], y[,i], col=col[i], lty=lty[i], lwd=2)},
				#	x=xrat, y=yrat, col=ocol, lty=rep(4,Norats)
				#)
			}

#browser();return()
			if (!is.null(ratios) && !is.null(zones)) {
				#y2 = par()$usr[4] - 0.2*diff(par()$usr[3:4])
				#text(c(0.2,0.6,1.2),rep(y2,3),c("Critical","Cautious","Healthy"),col=c("red","darkorange","green4"),font=2,cex=1.1,srt=90,adj=c(0,0.5))
				y2 = par()$usr[4] - 0.02*diff(par()$usr[3:4])
				y2h = par()$usr[4] - 0.04*diff(par()$usr[3:4])
				xpos.zones = c(0,ratios[1:2]) + diff(c(0,ratios[1:2],par()$usr[2]))/2  ## only use the first two ref pts; others are for illustration
				if (is.null(param)) {
					text(xpos.zones[1:2], rep(y2,3)[1:2], labels=linguaFranca(zones[1:2],l), col=lcol[1:2], font=2, cex=1.1, srt=90, adj=c(1,0.5))
					text(xpos.zones[3],   rep(y2h,3)[3],  labels=linguaFranca(zones[3],l),   col=lcol[3],   font=2, cex=1.1, srt=0,  adj=c(0.5,1))
				}
			}
			if (!is.null(ratios)) {
				#text(c(ratios,oratios),par()$usr[3],labels=show0(round(c(ratios,oratios),2),2),adj=c(1.1,-.5),col=c("red","green4",ocol))
				text(c(ratios),par()$usr[3],labels=show0(round(ratios,2),2),adj=c(1.1,-.5),col=rcol)
			}
			if (is.null(param)) {
				mess = paste0("mtext(expression(italic(B)[italic(t)]/italic(B)[",linguaFranca(refpt,l),"]),side=1,line=2.5,cex=1.5)")
			} else {
				mess = sapply(strsplit(param,"_"),function(x){if(length(x)==1) x else paste0("italic(",x[1],")[",x[2],"]")})
				mess = paste0("mtext(expression(",mess,"),side=1,line=2.5,cex=2)")
			}
			eval(parse(text=mess))
			if (f!="win") dev.off()
		} ## end f (figout) loop
	} ## end l (lang) loop
	invisible(Bmsy)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~compBmsy


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

#.imputeRate.impIR----------------------2017-12-10
# Impute the rate of return from an investment.
# Algorithm uses the NFV in the sense that the
# final value today is the future value as seen
# from the first time period.
# This means that the fit is to the final point
# and the estimated trajectory is more like a 
# NPV in reverse, i.e., the net present value of
# the historical asset.
#-----------------------------------------------RH
.imputeRate.impIR <- function() {
	IRRfun <- function(P){
		## See XIRR function in Excel
		## NPV -- https://www.investopedia.com/terms/n/npv.asp
		## NFV -- https://www.investopedia.com/terms/f/futurevalue.asp
		r=as.numeric(P[1])
		unpackList(ttcall(PBStool)["ddat"],scope="L")
		N=nrow(ddat)
		Ci=c(ddat$value[1],ddat$cont[2:N]) ## initial value (includes 1st contributuon) + additioanl contributions
		#Clast=Ci[N]; Ci[N]=0
		Vlast=ddat$value[N]                ## final value
		#sfac=GT0(mean(abs(Ci)))           ## scale factor for contributions
		sfac=1
		Ci=Ci/sfac
		Di=c(ddat$date)
		
		#ri = (1-r)^(as.numeric(Di-Di[1])/365)      ## Discounts
		#ri = (1+r)^(as.numeric(Di-Di[1])/365)      ## Discounts   ### need to work backwads rev(Di)[1]-Di
		ri = (1+r)^(as.numeric(rev(Di)[1]-Di)/365)  ## Growth as NPV -- need to work backwads rev(Di)[1]-Di
		#ri = 1/((1+r)^(as.numeric(Di-Di[1])/365))  ## Discounts
		#ri[Ci<0] = 1      ## Do not discount removals, BUT with current NLL and fval, need to discount withdrawals also.
		#DCi=Ci/ri         ## NPV
		ri[is.na(ri)] = 0  ## in case of ridiculous estimates
		FCi = Ci * ri      ## NFV (so that today's value is the target)
		
		N = nrow(ddat)
#browser();return()
		LL1 = -log(sum(sqrt((FCi - ddat$value)^2)))             ## NLL for fit to individual observations
		LL2 = log(abs(diff(c(sum(FCi),rev(ddat$value)[1]))^2))  ## Penalty for not fitting the final point
		#LL2 = -log(sum(sqrt((sum(FCi)-rev(ddat$value)[1])^2)))
		fval = LL1 + LL2
if(is.na(LL1)) {browser();return()}
		#return(fval)
.flush.cat(as.character(c(LL1,LL2,fval)), "\n")
		Vcont=sum(FCi)
		Vlast=Vlast/sfac
		#Vstd = c(Vcont,Vlast)/mean(c(Vcont,Vlast))
		#fval = diff(Vstd)^2

#browser();return()
		#Yobs  = scalePar(matrix(c(Vlast,0,1.5*max(Vlast,Vcont),TRUE),nrow=1))
		#Ypred = scalePar(matrix(c(Vcont,0,1.5*max(Vlast,Vcont),TRUE),nrow=1))
		#fval = (Yobs-Ypred)^2
		#fval = (Vlast - Vcont)^2
#print(round(c(r,fval,Vcont,Vlast),5))
		IRRmin=c(r,fval,sfac*Vcont,sfac*Vlast); names(IRRmin)=c("r","fval","Vcont","Vlast")
#print(round(IRRmin,2))
#browser();return()
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
	pyr = 365./median(as.vector(diff(ddat$date))) ## divide year by ave. days in month
	setWinVal(list(d0=d0,d1=d1,v0=v0,v1=v1,pyr=round(pyr,1)))

	x <- ddat$period; y <- ddat$value; names(x)=names(y)=as.character(ddat$date)
	## Indexing with z redundant since 'ddat' is now a subet of 'adat'
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
#browser();return()
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

#.imputeRate.trajIR---------------------2017-12-10
# Calculate the value trajectory.
#-----------------------------------------------RH
.imputeRate.trajIR=function(x,cw,y0,r){ 
	# x=periods, cw=contributions/withdrawals, y0=starting value, r=period rate
	cw[1]=y0 # initial value already includes 1st contribution
	#ri = (1-r)^(as.numeric(x-x[1])/365)
	#ri = (1+r)^(as.numeric(x-x[1])/365)
	ri = (1+r)^(as.numeric(rev(x)[1]-x)/365)
	#ri = 1/((1+r)^(as.numeric(x-x[1])/365))
	#ri[cw<0] = 1  ## with current NLL and fval, need to discount withdrawals also
	#ypred=cw/ri   ## NPV
	ypred=cw*ri    ## NFV
	y=cumsum(ypred)
#browser();return()
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

#.imputeRate.plotIR---------------------2017-12-10
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
#browser();return()
	zay=intersect(floor(ylim[1]):ceiling(ylim[2]),pretty(yglob,10))
	zpos=cobs>0 & !is.na(cobs); zneg=cobs<0 & !is.na(cobs)
	#cadd=abs(min(0,min(cobs)))
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
	lines(xobs,ccum,col=mclr[3],lwd=2); 
	lines(xobs,ycum,col=clr,lwd=2);  ## this seems offset high (perhaps subtract cobs[1]?)

	mtext("Date",side=1,line=3.75,cex=1.2); mtext("Value",side=2,line=1.5,cex=1.2);
	ydif=diff(ylim); ypin=par()$pin[2]
	yyy = ifelse(Vstart < (0.80*ydif+ylim[1]),.98,.50)
	addLabel(.05,yyy,paste("Method =",method),cex=1.2,adj=0,col=clr);
	addLabel(.05,yyy-.02*(6/ypin),paste(paste(c("Period rate","Annual rate")," = ",Pfig,"%",sep=""),collapse="\n"),adj=c(0,1),cex=0.8);
	addLabel(.05,yyy-.08*(6/ypin),paste("Timing =",paste(round(ftime[1],2),collapse=", "),"sec"),adj=0,cex=0.7,col="grey35");
	addLegend(.04,yyy-.09*(6/ypin),legend=c("Observed","Contributions","NPV"),bty="n",cex=.8,
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


## plotAgeErr---------------------------2018-07-24
## Plot ageing precision data
##   (modified from code by Sean Anderson, PBS)
## Arguments:
##   dat       - data frame created by call to `gfb_age_precision.sql'
##   nsamp     - number of fish to randomly sample for plot
##   xlim|ylim - x|y limits for plot
##   jitter    - maximum value to to randomly sample from uniform for visualization (done separately for x & y);
##               same jitter values are used for the ages estimated by primary and precision (secondary) readers for the same fish.
##   seed      - seed value, if numeric value, set the random seed so that the same rows
##               are sampled each time and the same jitter values are generated;
##               if `NULL', different fish will be sampled each time the function is run.
##   png       - logical: if TRUE, send figure to a PNG output file
##   pngres    - resolution (pixels/inch) for PNG figure
##   PIN       - output size (inches) specifying width and height for PNG figure.
## ------------------------------------------SA/RH

plotAgeErr = function(dat, nsamp, xlim=NULL, ylim=NULL, jitter=0.25, seed=42, 
   png=FALSE, pngres=400, PIN=c(8,8), lang=c("e","f"))
{
	opar = par(no.readonly=T); on.exit(par(opar))
	strSpp = as.character(.su(dat$spp))
	sppNam = toUpper(tolower(.su(dat$spn)))

	## Create a subdirectory called `french' for French-language figures
	createFdir(lang)

	## remove non-specified ageing methods
	dat = dat[!is.na(dat$ameth),]

	## remove specimen IDs for which there is no precision reading
	pSPID = dat$SPID[is.element(dat$atype,3)]
	dat = dat[is.element(dat$SPID,pSPID),]

	## remove records where specimens were read by only one employee
	one = crossTab(dat,"SPID","EMID",function(x){length(unique(x))}) < 2
	bad = names(one[one])
	dat = dat[!is.element(dat$SPID,bad),]

	## The data can be very ragged so process the specimens individually
	oflds  = c("spp","SPID","year") #,"atype","ARID","EMID","ameth","age","amin","amax")
	aflds  = c("atype","ARID","EMID","ameth","age","amin","amax")
	RAGOUT = as.data.frame(array(0,dim=c(1,length(oflds)+2*length(aflds)),dimnames=list("0",c(oflds, paste0(rep(c("r1_","r2_"),each=length(aflds)),aflds)))))
	ttput(RAGOUT)  ## send it to the PBStools environment
	ragin  = split(dat,dat$SPID)
	ragout = sapply(ragin, function(df,ofld,afld){
		ttget(RAGOUT)
		rout  = numeric()
		r1    = .su(df$atype)[1]
		r2    = .su(df$atype)[2]
		read1 = df[is.element(df$atype,r1),]
		read2 = df[is.element(df$atype,r2),]
		for (i in 1:nrow(read1)) {
			irec = read1[i,,drop=FALSE]
			for (j in 1:nrow(read2)) {
				jrec = read2[j,,drop=FALSE]
				if (irec$EMID==jrec$EMID) next
				RAGOUT = rbind(RAGOUT, as.vector(cbind(irec[,ofld],irec[,afld],jrec[,afld]), mode="numeric"), make.row.names=FALSE)
#browser();return()
			}
		}
		ttput(RAGOUT)
		return(as.matrix(rout))
	}, ofld=oflds, afld=aflds, simplify=F)
	ttget(RAGOUT)
	ragout = RAGOUT[-1,]  ## remove artificially initiated first row of zeroes (stoopid R)
	ttput(ragout)         ## send it to the PBStools environment

	dat.base = dat; dat = ragout
	if (!is.null(seed)) set.seed(seed)
	if (!missing(nsamp) && nsamp < nrow(dat)) {
		dat <- dat[sample(1:nrow(dat), nsamp),]
	}
	## Jitter x and y separately
	xjit <- stats::runif(nrow(dat), -jitter, jitter)
	yjit <- stats::runif(nrow(dat), -jitter, jitter)
	dat$r1_age  <- dat$r1_age  + xjit
	dat$r2_age  <- dat$r2_age  + yjit
	dat$r1_amin <- dat$r1_amin + xjit
	dat$r1_amax <- dat$r1_amax + xjit
	dat$r2_amin <- dat$r2_amin + yjit
	dat$r2_amax <- dat$r2_amax + yjit

	if (is.null(xlim))
		xlim = range(dat[,c("r1_amin","r1_amax")])
	if (is.null(ylim))
		ylim = range(dat[,c("r2_amin","r2_amax")])
	
	fout = fout.e = paste0("AgeErr",strSpp)
	for (l in lang) {
		if (l=="f") fout = paste0("./french/",fout.e)  ## could repeat for other languages
		if (png) png(paste0(fout,".png"), units="in", res=pngres, width=PIN[1], height=PIN[2])
		par(mfrow=c(1,1), mar=c(3.5,3.5,0.5,0.75), oma=c(0,0,0,0), mgp=c(2,0.5,0))
		plot(dat$r1_age, dat$r2_age, xlim=xlim, ylim=ylim, type="n", xlab="", ylab="", cex.axis=1.2, las=1)
		axis(1, at=1:xlim[2], labels=F, tcl=-0.2)
		axis(2, at=1:ylim[2], labels=F, tcl=-0.2)
		abline(0,1,col=lucent("green4",0.5),lwd=2)
		## This seems to take a long time:
		segments(x0=dat$r1_amin, y0=dat$r2_age, x1=dat$r1_amax, y1=dat$r2_age, col=lucent("grey50",0.5),lwd=2)
		segments(x0=dat$r1_age, y0=dat$r2_amin, x1=dat$r1_age, y1=dat$r2_amax, col=lucent("grey50",0.5),lwd=2)
#browser();return()
		points(dat$r1_age, dat$r2_age, pch=21, cex=0.8, col=lucent("black",0.5), bg=lucent("cyan",0.5))
		mtext(linguaFranca("Age (y) by Primary Reader",l), side=1, line=2, cex=1.5)
		mtext(linguaFranca("Age (y) by Secondary Reader",l), side=2, line=2, cex=1.5)
		addLabel(0.05, 0.95, linguaFranca(sppNam,l), cex=1.5, col="blue", adj=c(0,1))
		if (png) dev.off()
	} ## end l (lang) loop
	junk = gc(verbose=FALSE)
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotAgeErr


## plotMW-------------------------------2018-08-09
## Plot mean weight of stocks and individual areas
## that occur in the bigger stock areas.
## ---------------------------------------------RH
plotMW = function(dat, xlim, ylim, outnam="Mean-Weight-Compare",
   outcast, png=FALSE, lang=c("e","f"))
{
	## Create a subdirectory called `french' for French-language figures
	createFdir(lang)

	if (missing(xlim)) xlim=range(dat[,1])
	if (missing(ylim)) ylim=range(dat[,-1], na.rm=T)
	x = xlim[1]:xlim[2]
	stocks = names(dat[,-1])
	#scol   = c("green","green4","gold","orange","red","cyan","blue","red","blue")
	if (missing(outcast)){
		scol   = c("orange","salmon","coral","hotpink","plum","cyan","blue","red","blue")
		spch   = c(1:6,8,25,24)
		slty   = c(rep(5,7),rep(1,2))
		slwd   = c(rep(1,7),rep(3,2))
		scex   = c(rep(1.2,7),rep(1.5,2))
		sord   = paste0("s",c("3C","3D","5A","5B","5C","5D","5E","5ABC3CD","5DE"))
	} else {
		unpackList(outcast)
	}
	names(scol) = names(spch) = names(slty) = names(slwd) = names(scex) = sord
#browser();return()

	fout = fout.e = outnam
	for (l in lang) {
		if (l=="f") fout = paste0("./french/",fout.e)  ## could repeat for other languages
		if (png) png(paste0(fout,".png"), units="in", res=400, width=8, height=6)
		par(mfrow=c(1,1), mar=c(3.5,3.8,0.5,0.5), oma=c(0,0,0,0), mgp=c(2.25,0.5,0))
		plot(0, xlim=xlim, ylim=ylim, type="n", xlab=linguaFranca("Year",l), ylab=linguaFranca("Mean Weight (kg)",l), cex.axis=1.2, cex.lab=1.5)
		z = is.element(dat$year,x)
		for (i in sord) {
			if (!is.element(i,stocks)) next
			lines(x,dat[z,i], col=scol[i], lty=slty[i], lwd=slwd[i])
			points(x,dat[z,i], pch=spch[i], col=scol[i], bg="ghostwhite", cex=scex[i], lwd=slwd[i])
		}
		ii = gsub("^[s]","",stocks)
#browser();return
		addLegend(0.05,0.97, pch=spch[ii], col=scol[ii], lty=slty[ii], lwd=slwd[ii], pt.cex=scex[ii], pt.bg="ghostwhite", seg.len=5, legend=ii, bty="n")
		if (png) dev.off()
	} ## end l (lang) loop
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotMW


## quantAges----------------------------2018-07-17
## Plot quantile boxes of age by year and/or area,
## including mean age over time.
## Suggested by PJS to detect changes in age.
## -----------------------------------------PJS/RH
quantAges =function(bioDat, dfld="age", afld="major", tfld="year", 
   type="time", major=3:9, ylim=c(0,35), strSpp="RSR",
   outnam="Quant-Age", png=FALSE, lang=c("e","f"))  # types: time, area
{
	bioDat = bioDat[bioDat[,dfld]>0 & !is.na(bioDat[,dfld]),]
	if (afld=="major")
		bioDat = bioDat[is.element(bioDat[,afld], major),]
	if (dfld=="wt") dlab = paste0(strSpp, " Weight (kg)")
	else if (dfld=="len") dlab = paste0(strSpp, " Length (cm)")
	else dlab = paste0(strSpp, " Age (years)")
	bioDat$ctype=rep("U",nrow(bioDat))
	bioDat$ctype[is.element(bioDat$ttype,c(1,4:10,12:14))]="C"  ## Note: TRIP_SUB_TYPE=11 in GFBio is RECREATIONAL
	bioDat$ctype[is.element(bioDat$ttype,c(2,3))]="S"

	#mcol  = .colBlind[c("skyblue","blue")]
	#fcol  = .colBlind[c("vermillion","redpurple")]
	mcol   = c("cyan","blue","darkblue")
	fcol   = c("pink","red","darkred")
	boxwex = 0.3
	Qage   = list()

	if (type=="time") {
		abioDat = split(bioDat, bioDat[,afld])
		years   = 1990:2016; nyrs = length(years)
		yearbox = as.list(rep(NA,nyrs)); names(yearbox) = years

		fout = fout.e = outnam
		for (l in lang) {
			if (l=="f") fout = paste0("./french/",fout.e)  ## could repeat for other languages
			if (png) png(paste0(fout,".png"), width=8, height=8, units="in", res=400)
			par(mfcol=c(length(abioDat),2), mar=c(0,2,0,0), oma=c(4,2,1,1), mgp=c(2,0.5,0))

			for (j in 1:2){
				jj = c("C","S")[j]
				jjj = c("Commercial","Survey")[j]

				for (a in length(abioDat):1) {
					aa   = names(abioDat)[a]
					adat = abioDat[[a]]
					adat = adat[is.element(adat$ctype,jj),]

					males = is.element(adat$sex,1)
					msex = split(adat[,dfld][males],adat[,tfld][males])  ## may include ealry years
					zsex = is.element(names(msex),years)
					Msex = yearbox
					Msex[names(msex[zsex])] = msex[zsex]
					Qage[[jj]][[aa]][["M"]] = Msex

					females = is.element(adat$sex,2)
					fsex = split(adat[,dfld][females],adat[,tfld][females])
					zsex = is.element(names(fsex),years)
					Fsex = yearbox
					Fsex[names(fsex[zsex])] = fsex[zsex]
					Qage[[jj]][[aa]][["F"]] = Fsex

					quantBox(yearbox, outline=F, ylim=c(0,35), xaxt="n")
					quantBox(Msex, xaxt="n", yaxt="n", outline=F, boxcol="grey30", boxfill=mcol[1], medcol=mcol[2], whisklty=1, whiskcol="gainsboro", add=T, boxwex=boxwex, at=(1:nyrs)+(boxwex/2))
					quantBox(Fsex, xaxt="n", yaxt="n", outline=F, boxcol="grey30", boxfill=fcol[1], medcol=fcol[2], whisklty=1, whiskcol="gainsboro", add=T, boxwex=boxwex, at=(1:nyrs)-(boxwex/2))

					lines((1:nyrs)-(boxwex/2),sapply(Fsex,mean),col=fcol[2],lwd=2)
					lines((1:nyrs)+(boxwex/2),sapply(Msex,mean),col=mcol[2],lwd=2)

					axis(1, at=1:nyrs, labels=FALSE, tcl=0.25)
					if (a==1) axis(1, at=seq(1,nyrs,5), labels=seq(years[1],rev(years)[1],5), cex.axis=1.2, tcl=0.5)
					jfranc = ifelse(l=="f" && jj=="S","R",jj)
					addLabel(0.05, 0.95, linguaFranca(paste0(jfranc," - Major ",aa),l), adj=c(0,1), cex=1)
				}
			}
			mtext(linguaFranca("Year",l), side=1, outer=TRUE, line=2.5, cex=1.5)
			mtext(linguaFranca("RSR Age (years)",l), side=2, outer=TRUE, line=0.25, cex=1.5, las=3)
			if (png) dev.off()
		} ## end l (lang) loop
	} ## end if time

	if (type=="area") {
		pmfc = c("3C","3D","5A","5B","5C","5D","5E"); names(pmfc) = 3:9
		jbioDat = split(bioDat, bioDat[,"ctype"])
		areas   = .su(bioDat[,afld]); nareas = length(areas)
		areabox = as.list(rep(NA,nareas)); names(areabox) = areas

		fout = fout.e = outnam
		for (l in lang) {
			if (l=="f") fout = paste0("./french/",fout.e)  ## could repeat for other languages
			if (png) png(paste0(fout,".png"), width=8, height=8, units="in", res=400)
			par(mfcol=c(length(jbioDat),1), mar=c(0,2,0,0), oma=c(4,2,1,1), mgp=c(2,0.5,0))

			for (j in 1:length(jbioDat)) {
				jj   = names(jbioDat)[j]
				jjj = c("Commercial","Survey")[j]
				jdat = jbioDat[[j]]

				males = is.element(jdat$sex,1)
				msex = split(jdat[,dfld][males],jdat[,afld][males])  ## may include ealry years
				zsex = is.element(names(msex),areas)
				Msex = areabox
				Msex[names(msex[zsex])] = msex[zsex]
				Qage[[jj]][["M"]] = Msex

				females = is.element(jdat$sex,2)
				fsex = split(jdat[,dfld][females],jdat[,afld][females])
				zsex = is.element(names(fsex),areas)
				Fsex = areabox
				Fsex[names(fsex[zsex])] = fsex[zsex]
				Qage[[jj]][["F"]] = Fsex
				if (is.null(ylim))
					Ylim = c(min(sapply(Qage[[jj]],function(x){sapply(x,function(xx){quantile(xx,0.05,na.rm=T)})}),na.rm=T),
						max(sapply(Qage[[jj]],function(x){sapply(x,function(xx){quantile(xx,0.95,na.rm=T)})}),na.rm=T))
				else Ylim = ylim
#browser();return()

				quantBox(areabox, outline=F, ylim=Ylim, xaxt="n")
				quantBox(Msex, xaxt="n", yaxt="n", outline=F, boxcol="grey30", boxfill=mcol[1], medcol=mcol[2], whisklty=1, whiskcol="gainsboro", add=T, boxwex=boxwex, at=(1:nareas)+(boxwex/2))
				quantBox(Fsex, xaxt="n", yaxt="n", outline=F, boxcol="grey30", boxfill=fcol[1], medcol=fcol[2], whisklty=1, whiskcol="gainsboro", add=T, boxwex=boxwex, at=(1:nareas)-(boxwex/2))

				#lines((1:nareas)-(boxwex/2),sapply(Fsex,mean),col=fcol[2],lwd=2)
				#lines((1:nareas)+(boxwex/2),sapply(Msex,mean),col=mcol[2],lwd=2)

				axis(1, at=1:nareas, labels=FALSE, tcl=0.25)
				if (j==2) axis(1, at=1:nareas, labels=pmfc[as.character(areas)], cex.axis=1.2, tcl=0.5)
#browser();return()
				addLabel(0.05, 0.95, linguaFranca(paste0(jjj),l), adj=c(0,1), cex=1)
			}
			mtext(linguaFranca("PMFC Area",l), side=1, outer=TRUE, line=2.5, cex=1.5)
			mtext(linguaFranca(dlab,l), side=2, outer=TRUE, line=0.25, cex=1.5, las=3)
			if (png) dev.off()
		} ## end l (lang) loop
	} ## end if area
	return(Qage)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~quantAges