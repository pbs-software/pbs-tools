## =============================================================================
## Module 6: Assessment
## --------------------
##  calcCVage.......Calculate CV of ages based on age or length data.
##  calcMA..........Calculate a moving average using a fixed period occurring every x units.
##  compAF..........Compare age frequencies using discrete or cumulative distribution plots.
##  compBmsy........Compare biomass posteriors relative to Bmsy or Bavg.
##  compLen.........Compare annual length distributions among surveys series or commercial gears.
##  compVB..........Compare fitted von B curves using parameters.
##  compVBpars......Compare MCMC derived parameters from von Bertalanffy model.
##  createMA........Create table of DFO management actions for Appendix A (catch).
##  imputeRate......Impute rate of return from an investment with irregular contributions/withdrawals.
##  makeAgeErr......Make an ageing error matrix for Awatea.
##  plotAgeErr......Plot ageing precision data from primary and secondary readers.
##  plotBTMW........Plot bottom (BT) vs. midwater (MW) trawl catch.
##  plotMW..........Plot annual mean weights by stock and by PMFC area.
##  plotSnail.......Plot snail-trail plots for MCMC analysis.
##  processBio......Process PBSdat created by call to 'gfb_bio.sql' query.
##  processMap......Process PBSdat created by call to `fos_map_density.sql' to create a map object for stock assessment.
##  quantAges.......Plot quantile boxes of age by year and/or area, including mean age over time.
##  splineCPUE......Fit spline curves through CPUE data to determine CV process error.
##  tabAmeth........Tabulate ageing error structures available in bio123.
##  tabOtos.........Tabulate otoliths available and aged.
## =============================================================================


## calcCVage----------------------------2021-03-16
##  Calculate CV of ages based on age or length data.
##  Runs SQL query 'gfb_ages_read.sql' at least once.
## ---------------------------------------------RH
calcCVage = function(dat, read_true=1, read_obs=NULL, cvtype="age", Amax=50,
   min.ages=1:6, min.cv=0.25, smooth=FALSE,
   plot=TRUE, png=FALSE, pngres=400, PIN=c(8,6), outnam, lang=c("e","f"))
{
	readT = paste0("read", read_true)
	dat   = dat[dat[,readT] > 0 & !is.na(dat[,readT]),]
	if (cvtype=="age") {
		if (is.null(read_obs))
			readObs = setdiff(colnames(dat)[grep("^read",colnames(dat))], .su(c("read0",readT)))  ## exclude 'Unknown' also (RH 220502)
		else
			readObs = paste0("read", read_obs)
	}
	else
		readObs = "len"
	eval(parse(text=paste0("alist = split(dat, dat$read", read_true, ")")))
	#Amax = max(as.numeric(names(alist)))
	Atab = array(0,dim=c(Amax,ifelse(smooth,6,5)), dimnames=list(age=1:Amax, stat=c("N","MU","SD","CV","SDa","CVsm")[1:ifelse(smooth,6,5)]))

	for (i in 1:length(alist)){
		ii = names(alist)[i]
		Atrue = as.numeric(ii)
		if (Atrue>Amax) next
		if (ii=="0") next
		adat = alist[[i]]
		if (Atrue==Amax && any(as.numeric(names(alist))>Amax)) {
			for (j in (i+1):length(alist))
				adat = rbind(adat,alist[[j]])
		}
		zpos = adat[,readObs,drop=FALSE]>0
		Atab[ii,"N"] = sum(zpos)
		if (sum(zpos)==0) next
		Atab[ii,"MU"] = mean(adat[,readObs][zpos])
		if (sum(zpos)==1) {
			Atab[ii,"SD"] = Atab[ii,"CV"] = 0
		} else {
			Atab[ii,"SD"] = sd(adat[,readObs][zpos])
			Atab[ii,"CV"] = Atab[ii,"SD"] / Atab[ii,"MU"]
		}
	}
	## Override specific ages with minimum acceptable CVs
	exCV = is.element(rownames(Atab),min.ages)
	if (any(exCV))
		Atab[exCV,"CV"] = pmax(Atab[exCV,"CV"],min.cv)

	cv0 = is.na(Atab[,"CV"]) | (is.element(Atab[,"CV"],0) & Atab[,"N"] <= 1)
	while(any(cv0)) {
		cva = (1:nrow(Atab))[cv0]
		cvb = rbind(Atab[pmax(1,cva-1),"CV"], Atab[pmin(nrow(Atab),cva+1),"CV"])
		cvc = apply(cvb,2,mean) #calcGM)  ## geometric mean cannot handle 0 values sensibly
		Atab[cv0,"CV"] = cvc
		#cv0 = is.element(Atab[,"CV"],0) | is.na(Atab[,"CV"])
		cv0 = is.na(Atab[,"CV"]) | (is.element(Atab[,"CV"],0) & Atab[,"N"] <= 1) ## believe CV=0 if n>1
	}
#browser();return()

	xsmoo = as.numeric(rownames(Atab))
	xoff  = mean(diff(xsmoo))/2
	sd0   = is.element(Atab[,"SD"],0)
	Atab[,"SDa"] = xsmoo * Atab[,"CV"]  ## calculated SD by age from derived CVs
	if (smooth) {
		xsmoo = as.numeric(rownames(Atab))
		ysmoo = GT0(loess.smooth(xsmoo, xsmoo * Atab[,"CV"], evaluation=nrow(Atab), span=1/2)$y)
		#ysmoo = loess.smooth(xsmoo, log(GT0(xsmoo * Atab[,"CV"])), evaluation=nrow(Atab), span=2/3)$y; ysmoo = exp(ysmoo)
		Atab[,"CVsm"] = ysmoo
	}
#browser();return()
	names.not = names.arg = rep("",nrow(Atab))
	names.out = seq(5,Amax,5)
	names.arg[match(names.out,rownames(Atab))] = names.out

	if (plot) {
		if (missing(outnam))
			outnam = paste0("CV",cvtype)
		fout.e = outnam
		for (l in lang) {  ## could switch to other languages if available in 'linguaFranca'.
			changeLangOpts(L=l)
			fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
			if (png){ 
				createFdir(l)
				clearFiles(paste0(fout,c(".csv",".png")))
				png(file=paste0(fout,".png"), units="in", res=pngres, width=PIN[1], height=PIN[2])
			}
			if (smooth) {
				par(fig=c(0,1,0.4,1), mar=c(3.5,3,1,1), new=FALSE)
				plot( xsmoo, xsmoo * Atab[,"CV"], type="n", xlab=linguaFranca("Age (y)",l), ylab="", cex.axis=1.2, cex.lab=1.5, mgp=c(2,0.5,0), las=1)
				## Add shading for ages with no observed SD
				if (any(sd0)) {
					x0  = xsmoo[sd0]
					xvert = data.frame(v1=x0[c(TRUE,diff(x0)>1)], v2=x0[!c(diff(x0)==1,FALSE)])
					apply(xvert, 1, function(x) {
						if (diff(x)==0) x = x + c(-xoff/2,xoff/2)
						y = par()$usr[3:4]
						polygon(x=x[c(1,1,2,2)], y=y[c(1,2,2,1)], col="gainsboro", border="gainsboro")
					})	
					#abline(v=x0, lwd=5, col=lucent(switch(cvtype, 'age'="red", 'len'="blue"),0.5))
				}
				axis(1, at=seq(5, max(xsmoo), 5), tick=TRUE, labels=FALSE, tcl=-0.25)
				mtext(linguaFranca("Standard deviation of age",l), side=2, line=1.5, cex=1.5)
				lines(xsmoo, xsmoo * Atab[,"CV"], lwd=2, lty=2, col=switch(cvtype, 'age'="brown", 'len'="green4"))
				lines(xsmoo, ysmoo, lwd=2, col=switch(cvtype, 'age'="red", 'len'="blue"))
				box()
#browser();return()

				par(fig=c(0,1,0,0.4), mar=c(3,4,0.5,0), new=TRUE)
				barplot(Atab[,"CV"], col=switch(cvtype, 'age'="moccasin", 'len'="olivedrab1"), space=0, xaxt="n", xlab="", ylab="", cex.axis=1.2, cex.lab=1.4, names.arg=names.not)
				axis(1, at=names.out-0.5, labels=names.out, line=NA, pos=0, tick=FALSE, padj=0, cex=1.2, mgp=c(2,0.2,0))
				mtext(linguaFranca("Age (y)",l), side=1, line=1.5, cex=1.5)
				mtext(switch(l, 'e'="Coefficient of Variation", 'f'="coefficient de variation"), side=2, line=2.5, cex=1.5)
				addLegend(0.9,0.9, linguaFranca(paste0("based on ", switch(cvtype, 'age'="age precision checks", 'len'="lengths-at-age")),l), adj=c(1,0), bty="n")
				if (any(sd0)) {
					x0  = xsmoo[sd0] - xoff
					points(x0, rep(0,length(x0)), pch=15, cex=0.8, col=lucent(switch(cvtype, 'age'="red", 'len'="blue"),0.75))
				}
			} else {
				expandGraph(mfrow=c(1,1), mar=c(3,3,1,1))
				barplot(Atab[,"CV"], col=switch(cvtype, 'age'="moccasin", 'len'="olivedrab1"), space=0, xlab=linguaFranca("Age (y)",l), ylab=switch(l, 'e'="Coefficient of Variation", 'f'="coefficient de variation"), cex.axis=1.2, cex.lab=1.5, names.arg=names.not)
				axis(1, at=names.out-0.5, labels=names.out, line=NA, pos=0, tick=FALSE, padj=0, cex=1.2, mgp=c(2,0.2,0))
				addLegend(0.9,0.9, linguaFranca(paste0("based on ", switch(cvtype, 'age'="age precision checks", 'len'="lengths-at-age")),l), adj=c(1,0), bty="n")
			}
			if(png) dev.off()
		}; eop()
	}
	write.csv(Atab,file=paste0(outnam,".csv"))
	return(Atab)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~calcCVage


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


## compAF-------------------------------2020-04-07
## Compare age frequencies using discrete or
## cumulative distribution plots.
## ---------------------------------------------RH
compAF=function(x, year=2003, sex=2, amax=40, pfld="wp",
   png=FALSE, pngres=400, PIN=c(10,7.5),
   outnam, clrs=c("red","black"), ltys=1, 
   type="cumul", fac="len", lang=c("e","f"))
{
	if (length(x)==0) stop("Supply a named list for x")
	std   = function(x){x/sum(x)}
	ncomp = length(x)
	nsex  = length(sex)
	ntype = length(type)
#browser();return()
	years = sapply(x,function(xx){
		noto=apply(xx[,,,"n",drop=FALSE],2,sum,na.rm=TRUE)
		yrs = as.numeric(names(noto[noto>0]))
		return(yrs)
	} )
	year  = intersect(year,.su(unlist(years)))
	nyear = length(year)
	col   = lucent(rep(clrs,ncomp)[1:ncomp],0.5)
	lty   = rep(ltys,ncomp)[1:ncomp]

	createFdir(lang)
	fout.e = outnam
	for (l in lang) {
		changeLangOpts(L=l)
		fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		if (png) {
			clearFiles(paste0(fout,".png"))
			png(file=paste0(fout,".png"), units="in", res=pngres, width=PIN[1], height=PIN[2])
		}
#browser();return()
		if (ntype==1 && nsex==1) {
			rc = .findSquare(nyear)
			np = 0  ## keep track of # plots
			par(mfrow=rc, mar=c(0,0,0,0), oma=c(4,4,0.5,0.5), mgp=c(1.6,0.5,0))
		} else {
			rc = c(ntype,nsex)
			par(mfcol=rc, mar=c(0,0,0,0), oma=c(3.5,3.5,0.5,0.5), mgp=c(1.6,0.5,0))
		}
	
		xnam = names(x)
		ydiff = diff(year); udiff=unique(ydiff); ymode=udiff[which.max(tabulate(match(ydiff, udiff)))]
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
				#legtxt = paste0(names(notos)," ",round(notos), switch(fac, 'age'=" otos", 'len'=" lens"))
				legtxt = paste0(names(notos)," ",round(notos), " obs")
				ylim = c(0,max(sapply(xvec,max),na.rm=TRUE))
#browser();return()
				#yyy  = if (ymode<=1) yy else paste0(y-ymode+1,"-",y)
				yyy  = if (ymode<=1) yy else paste0(y,"-",y+ymode-1)

				if ("discr" %in% type) {
					plot(0,0, xlim=c(1,amax), ylim=ylim, type="n", xlab="", ylab="", xaxt="n", yaxt="n")
					sapply(nord, function(n){
						if (!all(is.na(xvec[[n]])))
							#lines(1:length(xvec[[n]]), xvec[[n]], col=col[n], lty=lty[n], lwd=ifelse(n==1,3,2)) } )
							lines(1:length(xvec[[n]]), xvec[[n]], col=col[n], lty=lty[n], lwd=2) } )
					addLabel(0.95, 0.95, paste0(yyy, " - ", linguaFranca(switch(s,"Male","Female"),l)), adj=1)
				}
				if ("cumul" %in% type) {
					plot(0,0, xlim=c(1,amax), ylim=c(0,1), type="n", xlab="", ylab="", xaxt="n", yaxt="n")
					np = np + 1
					if (all(notos==0))
						addLabel(0.5, 0.5, linguaFranca("NO DATA",l), col="red", cex=1.2)
					else 
						#abline(h=seq(0.1,0.9,0.1), v=seq(5,amax-5,5), col=lucent("grey",0.4))
						abline(h=seq(0,1,0.1), v=seq(0,amax,5), col=lucent("grey",0.4))
					sapply(nord, function(n){
						if (!all(is.na(xvec[[n]]))) {
							#lines(1:length(xvec[[n]]), cumsum(xvec[[n]]), col=col[n], lty=lty[n], lwd=ifelse(n==1,3,2))
							lines(1:length(xvec[[n]]), cumsum(xvec[[n]]), col=col[n], lty=lty[n], lwd=2)
						}
					} )
					addLabel(0.05, 0.95, paste0(yyy, ifelse(np==1, linguaFranca(switch(s," - Male"," - Female"),l), "")), adj=c(0,1), cex=1.2)
					if (par()$mfg[2]==1) {
						axis(2, at=seq(0,1,0.1), tcl=-0.25, labels=FALSE)
						axis(2, at=seq(0.2,1,0.2), labels=TRUE, cex.axis=1.1, las=1)
					}
					if (np > nyear-rc[2]) {
						axis(1, at=seq(0,amax,5), tcl=-0.25, labels=FALSE)
						axis(1, at=seq(10,amax,10), labels=TRUE, cex.axis=1.1, las=1)
					}
				}
#browser();return()
				zleg = grep(" 0 ",legtxt,invert=TRUE)
				if (type=="cumul")
					addLegend(0.025,0.8, bty="n", lty=lty[zleg], seg.len=3, col=col[zleg], legend=linguaFranca(gsub("_"," ",legtxt[zleg]),l), yjust=1, xjust=0, lwd=2, cex=0.9)
				else
					addLegend(0.025,0.975,bty="n", lty=lty, seg.len=1, col=col, legend=linguaFranca(gsub("_"," ",legtxt),l), yjust=1, xjust=0, lwd=2, cex=0.9)
			} ## end s (sex) loop
		} ## end y (year) loop
		#mtext (linguaFranca("Age",l), side=1, outer=TRUE, line=2.5, cex=1.5)
		mtext (linguaFranca(switch(fac, 'age'="Age", 'len'="Length (cm)"),l), side=1, outer=TRUE, line=2.5, cex=1.5)
		mtext (linguaFranca(paste0(ifelse(type=="cumul","Cumulative ",""), "Frequency"),l), side=2, outer=TRUE, line=2.5, cex=1.25, las=0)
		if(png) dev.off()
	}; eop()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~compAF


## compBmsy-----------------------------2021-09-16
## Compare biomass posteriors relative to Bmsy or Bavg
## ---------------------------------------------RH
compBmsy = function(Bspp, spp="POP", Mnams=c("Est M","Fix M"),
   ratios=c(0.4,0.8), oratios=NULL, t.yr=2018,
   quants=c(0.05,0.25,0.5,0.75,0.95),
   zones = c("Critical","Cautious","Healthy"),
   figgy=list(win=TRUE), pngres=400, width=12, height=9, 
   rcol=c("red","green4","blue"), rlty=rep(2,3), rlwd=rep(2,3), ## ratio lines: col, lty, lwd
   ocol = c("#D55E00", "#009E73", "#56B4E9", "#F0E442"),   ## dot cols for colour-blind humans (vermillion, bluegreen, skyblue, yellow)
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
	if(!is.null(left.space)) left.space = rep(left.space,2)[1:2]

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
		ylim = c(ifelse(is.null(param),0,min(sapply(Bmsy,quantile,0.0))), max(sapply(Bmsy,quantile,0.96),1.1))
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

	for (l in lang) {  ## could switch to other languages if available in 'linguaFranca'.
		changeLangOpts(L=l)
		fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		for (f in figout) {
			if (f=="eps"){    grDevices::ps.options(horizontal = FALSE)
			                  postscript(file=paste(fout,".eps",sep=""),width=width*1.25,height=height,fonts="mono",paper="special") }
			if (f=="pdf"){    grDevices::ps.options(horizontal = TRUE)
			                  pdf(file=paste(fout,".pdf",sep=""),width=width*1.25,height=height*1.25,fonts="mono") }
			else if (f=="png") png(paste(fout,".png",sep=""), units="in", res=pngres, width=width, height=height)
			else if (.Platform$OS.type=="windows" && f=="wmf") 
				do.call("win.metafile",list(filename=paste(fout,".wmf",sep=""), width=width*1.25, height=height*1.25))
			if (is.null(left.space))
				left.space = rep((max(nchar(names(Bmsy)))-ifelse(spplabs,nchar(spp),0))^0.9,2) ## just to be consistent with english vs. french
			len.lab = max(sapply(names(Bmsy),nchar))                      ## RH 200420
			cex.lab = ifelse(len.lab>20, 0.9, ifelse(len.lab>15, 1, 1.2)) ## RH 200420
			par(mar=c(4, switch(l,'e'=left.space[1],'f'=left.space[2]), 0.5, 0.5), cex=ifelse(f%in%c("png","eps"),1,1.2), mgp=c(1.6,0.6,0))
			quantBox(Bmsy, horizontal=TRUE, las=1, xlim=c(0.5,nmods+top.space), ylim=ylim, cex.axis=1.2, yaxs="i", outline=FALSE,
				pars=list(boxwex=boxwidth,medlwd=2,whisklty=1), quants=quants, names=FALSE)
			if (Nrats>0)
				abline(v=ratios,col=rep(rcol,Nrats)[1:Nrats],lty=rlty,lwd=rlwd)
	
			## segments(v=ratios,col=rep(c("red","green4","blue"),Nrats)[1:Nrats],lty=2,lwd=2)
			quantBox(Bmsy, horizontal=TRUE, las=1, xlim=c(0.5,nmods+1), ylim=ylim, cex.axis=1.2, yaxs="i", outline=FALSE, names=FALSE, pars=list(boxwex=boxwidth, medlwd=2, whisklty=1, medcol=medcol, boxfill=boxfill, ...), add=TRUE, quants=quants)
			## if (length(Bmsy)==1)  ## for some reason, a label is not added when there is only 1 boxplot.
			##	axis(2, at=1, labels=linguaFranca(names(Bmsy),l), las=1, cex.axis=1.2)
#browser();return()
			axis(2, at=1:length(Bmsy), labels=linguaFranca(names(Bmsy),l), las=1, cex.axis=cex.lab) ## RH 200420

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
				mess = paste0("mtext(expression(italic(B)[", t.yr, "]/italic(B)[",linguaFranca(refpt,l),"]),side=1,line=2.5,cex=1.5)")
			} else {
				mess = sapply(strsplit(param,"_"),function(x){if(length(x)==1) x else paste0("italic(",x[1],")[",x[2],"]")})
				mess = paste0("mtext(expression(",mess,"),side=1,line=2.5,cex=2)")
			}
			eval(parse(text=mess))
			if (f!="win") dev.off()
		} ## end f (figout) loop
	}; eop()
	invisible(Bmsy)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~compBmsy


## compLen------------------------------2020-06-16
##  Compare lengths (or ages) among groups by sex.
##  For example, compare annual length distributions
##  among surveys series or commercial gears.
## -----------------------------------------PJS/RH
compLen = function(dat, strSpp, fld="len", lbin=1, sex=c(2,1),
   gfld="SSID", gval=list(16,1,4), yrs, ttype, stype, scat, exlax,
   strat=FALSE, boot=FALSE, R=10, bxpsep=0.2, bxpcol="black", 
   ylim=NULL, legpos=c(0.025,0.4), stock.name, outnam,
   png=FALSE, pngres=400, PIN=c(8,8), lang=c("e","f"))
{
	datnam = as.character(substitute(dat))
	## Match colours to gval early on
	bxpcol = rep(bxpcol,length(gval))[1:length(gval)]; names(bxpcol) = names(gval)
	gval.o = gval
	gval.u = unique(unlist(gval))
	dat  = dat[is.element(dat[,gfld],gval.u) & dat[,fld]>0 & !is.na(dat[,fld]),]
	if (!missing(yrs))
		dat = dat[is.element(dat$year, yrs),]
	if (!missing(ttype))
		dat = dat[is.element(dat$ttype, ttype),]
	if (!missing(stype))
		dat = dat[is.element(dat$stype, stype),]
	if (!missing(scat))
		dat = dat[is.element(dat$scat, scat),]
	if (!is.null(sex))
		dat = dat[is.element(dat$sex, sex),]
	
	if (nrow(dat)==0) stop("No records for this species/stock")

	gval = sapply(gval, function(x){xout = intersect(x, .su(dat[,gfld])); if(length(xout)==0) NA else xout}, simplify=FALSE)
	gval = gval[!is.na(gval)]
	if (is.null(names(gval)))
		names(gval) = sapply(gval,paste0,collapse="+")
	bxpcol = bxpcol[names(gval)] ## retain intended colours

	#dat$group = rep(NA,nrow(dat))  ## problem when gvals overlap between groups
	#dat$group[is.element(dat[,gfld],gval[[i]])] = i
	dat.group = qtmp = list()
	for (i in names(gval)){
		dat$group = rep(NA,nrow(dat))
		dat$group[is.element(dat[,gfld],gval[[i]])] = i
		dat.group[[i]] = dat[,"group",drop=FALSE]
		qtmp[[i]] = sapply(split(dat[,fld],paste(dat$group,dat$year,sep="-")),quantile,c(0.05,0.95))
	}
	xlim = if (missing(yrs)) range(dat$year) else range(yrs)
	if (is.null(ylim)) {
		ylim.group = sapply(qtmp,function(x){c(min(x[1,]),max(x[2,]))})
		ylim = c(min(ylim.group[1,]),max(ylim.group[2,]))
	}
	Qbox = as.list(rep(NA,diff(xlim)+1)); names(Qbox)=xlim[1]:xlim[2]
	Lbin = .su(ceiling(.su(dat[,fld])/lbin)*lbin)

	if (gfld=="SSID.off") {
		#ssnames = c("QCS Synoptic", "WCVI Synoptic", "WCHG Synoptic", "HS Synoptic", "IPHC Longline", "Shrimp Trawl")
		#names(ssnames) = names(bxpcol) = c(1,4,16,3,14,670)
		ssnames = names(gval)
		gnames  = sapply(gval,paste0,collapse="|")
		for (i in 1:length(ssnames)) {
			if (any(grepl(gnames[i],names(gval))))
				names(ssnames)[i] = names(bxpcol)[i] = names(gval)[grep(gnames[i],names(gval))]
			else
				names(ssnames)[i] = names(bxpcol)[i] = names(gval)[i]
			#	ssnames = ssnames[grep(gnames[i],names(gval),invert=TRUE)]
		}
		ssnames[!is.na(names(ssnames))]
		bxpcol = bxpcol[names(ssnames)]

		#ssnames = names(ssid)
		#names(ssnames) = names(bxpcol) = sapply(ssid,paste0,collapse="|")
	} else if (gfld=="major.old") {
		if (any(strSpp %in% "417")) {
			ssnames = c("BCS", "BCC", "BCN")
			names(ssnames)[1] = names(bxpcol)[1] = names(gval)[grep("3|4",names(gval))]
			names(ssnames)[2] = names(bxpcol)[2] = names(gval)[grep("5|6|7|8",names(gval))]
			names(ssnames)[3] = names(bxpcol)[3] = names(gval)[grep("9",names(gval))]
		} else if (any(strSpp %in% "435")) {
			#ssnames = c("3CD", "5AB", "5CDE")
			ssnames = c("South", "North")
			names(ssnames)[1] = names(bxpcol)[1] = names(gval)[grep("3|4",names(gval))]
			#names(ssnames)[2] = names(bxpcol)[2] = names(gval)[grep("5|6",names(gval))]
			#names(ssnames)[3] = names(bxpcol)[3] = names(gval)[grep("7|8|9",names(gval))]
			names(ssnames)[2] = names(bxpcol)[2] = names(gval)[grep("5|6|7|8|9",names(gval))]
		}
	} else if (gfld=="gear.off") {
		#ssnames = c("BT", "MW", "LL")
		#gnames  = c("1|8","6","5")
		ssnames = names(gval)
		gnames  = sapply(gval,paste0,collapse="|")
		for (i in 1:length(ssnames)) {
			if (any(grepl(gnames[i],names(gval))))
				names(ssnames)[i] = names(bxpcol)[i] = names(gval)[grep(gnames[i],names(gval))]
			else
				names(ssnames)[i] = names(bxpcol)[i] = names(gval)[i]
			#	ssnames = ssnames[grep(gnames[i],names(gval),invert=TRUE)]
		}
		ssnames[!is.na(names(ssnames))]
		bxpcol = bxpcol[names(ssnames)]
	}
	gnames  = sapply(gval,paste0,collapse="|")
	ssnames = names(gval)
	loca    = lenv()
	data("species", package="PBSdata", envir=loca)
	if (missing(stock.name)) { ## RH 200616
		if (grepl("BSR|RER|HYB|REBS", strSpp))
			spp3 = strSpp
		else 
			spp3 = paste0(species[strSpp, "code3"],collapse="|")
	} else {
		spp3 = stock.name ## prone to breaking but use for now (RH 200616)
	}

	#spp3    = if(all(strSpp=="REBS")) "REBS" else if(all(strSpp=="HYB")) "HYB" else paste0(species[strSpp, "code3"],collapse="|")
	createFdir(lang)

	out  = if (gfld=="SSID") "-Surv" else paste0("-tt(",paste0(.su(dat$ttype),collapse=""),")")
	poo  = if (missing(exlax)) "" else paste0("-(",exlax,")")
	goo  = if (gfld=="gear")  paste0(paste0(names(gnames),"=",gsub("\\|","+",gnames)),collapse="_") else gfld
	foo  = paste0(spp3,"-(", datnam, ")", out , poo, "-g(", goo, ")", ifelse(strat,"-(strat_","-(obs_"),fld,")")
	fout.e = if (missing(outnam)) foo else outnam

	for (l in lang) {  ## could switch to other languages if available in 'linguaFranca'.
		changeLangOpts(L=l)
		fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		if (png){
			clearFiles(paste0(fout,".png"))
			png(paste0(fout,".png"), units="in", res=pngres, width=PIN[1], height=PIN[2])
		}
		par (mfrow=c(2,1), mar=c(1.5,3.5,0.5,0.5), oma=c(1.5,0,0,0), mgp=c(1.5,0.5,0), las=1)
		for (s in sex) {
			#sdat = dat[is.element(dat$sex,s),]
			#Idat = split(sdat,sdat$group) ## cannot do this when groups use common records
			Idat = list()
			for (i in names(gval)){
				dat$group = dat.group[[i]][,1]
				sdat = dat[is.element(dat$sex,s),]
				Idat[[i]] = sdat[is.element(sdat$group,i),]
			}
			Idat = Idat[intersect(names(gval),names(Idat))]  ## order them as per user input
			if (strat && !boot) {
				resetGraph();expandGraph()
				#plotBubbles(Yprop,dnam=TRUE,hide0=TRUE,prettyAxis=TRUE,siz=0.1,ylim=ylim,cpro=TRUE)
				#points(as.numeric(names(Ymean)),Ymean, pch=21,col="red",bg="pink",cex=1.5)
				#plot(0,0,type="n",xlim=xlim,ylim=ylim,xlab="",ylab="", mgp=c(1.5,0.5,0))
			} else {
				quantBox(sapply(Qbox,function(x){NA},simplify=FALSE), type="n", ylim=ylim, xlab="", ylab="")
				abline(v=seq(0.5,par()$usr[2],1),col="grey90"); box()
			}
			sapply(1:length(Idat), function(i) {  ## loop through index 
				ii  = names(Idat)[i]
				idat = Idat[[i]]
				ival  = split(idat[,fld],idat$year)
				if (strat){
					yrs  = .su(idat$year)
					Ymat = array(0,dim=c(length(Lbin),length(yrs)), dimnames=list(len=Lbin,yr=yrs))
					idat$lbin = ceiling(idat[,fld]/lbin)*lbin
					for (y in yrs) {
#.flush.cat(y,"\n")
						yy = as.character(y)
						ydat = idat[is.element(idat$year,y),]
						jdat = split(ydat,ydat$GC)  ## split by Grouping Code (stratum)

						dGC  = sapply(jdat,function(x){sapply(split(x$density,x$SID),mean)},simplify=FALSE)  ## density by SID in each GC
						pGC  = sapply(dGC,function(x){x/sum(x)},simplify=FALSE)                              ## proportion density by SID in each GC
						vGC  = unlist(pGC)
						ydat$pGC =rep(0,nrow(ydat))
						ydat$pGC = vGC[paste(ydat$GC,ydat$SID,sep=".")]

						aGC  = sapply(jdat,function(x){.su(x$area)})     ## area by SID in each GC
						paGC = aGC/sum(aGC)
						ydat$paGC =rep(0,nrow(ydat))
						ydat$paGC = paGC[as.character(ydat$GC)]

						ydat$pbin = apply(ydat[,c("pGC","paGC")],1,prod)
						ydat$pbin = ydat$pbin/sum(ydat$pbin)
						ydat$plbin = apply(ydat[,c("pbin","lbin")],1,prod)
						Lest = sum(ydat$plbin)  ## need to bootstrap this procedure

						Lboot = function(Ydat,R) {
							Lsamp = numeric()
							for (r in 1:R) {
								jdat = split(Ydat,Ydat$GC)  ## split by Grouping Code (stratum)
								jdat = sapply(jdat,function(x){x[sample(1:nrow(x),nrow(x),replace=TRUE),]},simplify=FALSE)
								ydat = do.call(rbind, jdat)

								dGC  = sapply(jdat,function(x){sapply(split(x$density,x$SID),mean)},simplify=FALSE)  ## density by SID in each GC
								pGC  = sapply(dGC,function(x){x/sum(x)},simplify=FALSE)                              ## proportion density by SID in each GC
								vGC  = unlist(pGC)
								ydat$pGC =rep(0,nrow(ydat))
								ydat$pGC = vGC[paste(ydat$GC,ydat$SID,sep=".")]

								aGC  = sapply(jdat,function(x){.su(x$area)})     ## area by SID in each GC
								paGC = aGC/sum(aGC)
								ydat$paGC = rep(0,nrow(ydat))
								ydat$paGC = paGC[as.character(ydat$GC)]

								ydat$pbin = apply(ydat[,c("pGC","paGC")],1,prod)
								ydat$pbin = ydat$pbin/sum(ydat$pbin)
								ydat$plbin = apply(ydat[,c("pbin","lbin")],1,prod)
								Lsamp[r] = sum(ydat$plbin)  ## need to bootstrap this procedure
							}
							return(Lsamp)
						}
						Gmat = array(0,dim=c(length(Lbin),length(pGC)), dimnames=list(len=Lbin,GC=names(pGC)))
						for (k in 1:length(pGC)) {
							kk   = names(pGC)[k]
							pk   = pGC[[kk]]
							kdat = jdat[[kk]]
							kmat = crossTab(kdat,c("lbin","SID"),"lbin",length)
							pkmat = kmat%*%pk
							Gmat[rownames(pkmat),kk] = pkmat
						}
						aGC = sapply(jdat,function(x){.su(x$area)})     ## area by SID in each GC
						paGC = aGC/sum(aGC)
						pymat = Gmat%*%paGC
						Ymat[rownames(pymat),yy] = pymat
						if (boot)
							ival[[yy]] = Lboot(ydat[,c("GC","SID","density","area","lbin")], R=R)
						else
							ival = NULL
					}
					Yprop = apply(Ymat,2,function(x){x/sum(x)})
					YSE   = apply(Yprop,2,function(x){sqrt(x*(1-x)/length(x))})
					Yvals = apply(Ymat,2,function(x,a){(x/sum(x))*a},a=Lbin)
					Ymean = apply(Yvals,2,function(x){sum(x)})
				} else {
					##ival  = split(idat[,fld],idat$year) ## redundant?
					Ymean = sapply(ival,mean,na.rm=TRUE)
				}
				if (!is.null(ival)) {
					attr(ival,"Ymean") = Ymean
					qbox   = Qbox
					qbox[names(ival)] = ival
					#wbxp  = (bxpsep*2)^(2)  ## reverse calcs in function bxp (sort of)  ## used for WWR 2019
					wbxp   = ifelse(length(gval)==1, 0.5, 0.90/length(gval))
					midout = ((wbxp*length(gval)/2)-wbxp/2) * c(-1,1)
					xoff   = seq(midout[1],midout[2],length.out=length(gval))
					pars   = list(boxwex=wbxp, whisklty=1, boxcol="gainsboro", boxfill=lucent(bxpcol[ii],0.5), medcol="black", medlwd=2)
					xpos   = (1:length(qbox)) + xoff[i]
					qxy    = quantBox(qbox, outline=FALSE, pars=pars, add=TRUE, xaxt="n", at=xpos)
					imean  = match(names(Ymean),names(Qbox))
					#points(xpos[imean],Ymean,pch=21,col=bxpcol[i],bg="white",cex=0.8)
				} else {
				}
			}) ## end i (index) loop
			mtext(linguaFranca(ifelse(fld %in% c("len"), "Length (cm)", "Age (y)"),l), side=2, line=2.25, cex=1.5, las=0)
			yleg = ifelse(fld=="age" && gfld=="SSID", 0.9, 0.05)
			addLabel(0.025, yleg, linguaFranca(paste0(spp3, " ",switch(s,"Males","Females")),l), cex=1.2, adj=c(0,0))
			if (par()$mfg[1]==1) {
				leglab = gsub("_"," ",ssnames)
				#leglab = gsub("_"," ",ssnames[names(gval)])
				addLegend(legpos[1], legpos[2], bty="n", fill=lucent(bxpcol[names(gval)],0.5), border="gainsboro", legend=linguaFranca(leglab,l), xjust=0, yjust=1)
				if (gfld %in% c("gear","major") && !is.element(strSpp, c("REBS","BSR","RER"))){
					#addLabel(0.975, 0.95, txt=linguaFranca( sub("bioDat","",datnam),l), cex=1.2, adj=c(1,1))
					data("species",package="PBSdata")
					addLabel(0.975, 0.95, txt=linguaFranca(toUpper(species[strSpp,"name"]),l), cex=1.2, adj=c(1,1))
				}
			}
		} ## end s (sex) loop
		mtext(linguaFranca("Year",l), side=1, outer=TRUE, line=0.5, cex=1.5)
		if (png) dev.off()
	}; eop()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~compLen


## compVB-------------------------------2020-03-30
## Compare fitted von B curves using parameters.
## ---------------------------------------------RH
compVB = function(dat, index, A=1:40, subset=list(sex=c("Male","Female")), 
   col=c("blue","green4","red"), lty=c(1,2,3), ymax,
   outnam="compVB-RSR", png=FALSE, pngres=400, lang=c("e","f"), ...)
{
	VBfun <- function(P, a=1:40) {
		Linf <- P[1]
		K    <- P[2]
		t0   <- P[3]
		pred <- Linf * (1 - exp(-K * (a - t0)))
		return(pred)
	}
	## Create a subdirectory called `french' for French-language figures
	createFdir(lang)

	## Use index to subset a potentially large list object
	if (!missing(index))
		dat = dat[index]

	## Group
	if (any(names(subset)=="sex")) {
		iii    = subset[["sex"]]
		nsex   = length(iii)
		stocks = names(dat)
		nstock = length(stocks)
		nss    = nstock * nsex
		#scols  = rep(col,nstock)[1:nstock]; names(scols) = stocks
		#sltys  = rep(lty,nstock)[1:nstock]; names(sltys) = stocks
#browser();return()
		scols  = rep(col,nss)[1:nss]; names(scols) = paste0(rep(stocks,nsex),".",rep(iii,each=nstock))
		sltys  = rep(lty,nss)[1:nss]; names(sltys) = paste0(rep(stocks,nsex),".",rep(iii,each=nstock))
		xlim = range(A)
		xlim = xlim + c(-1,1)
		if (missing(ymax)) {
			ylim = c(0, max(sapply(dat, function(x){apply(x,2,max)[2]})) )
			ylim[2] =extendrange(ylim)[2]
		} else {
			ylim = c(0,ymax)
		}
		fout = fout.e = outnam
		for (l in lang) {
			changeLangOpts(L=l)
			fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
			if (png) png(paste0(fout,".png"), units="in", res=pngres, width=8, height=6)
			par(mfrow=c(1,1), mar=c(3.25,3.5,0.5,0.5), oma=c(0,0,0,0), mgp=c(2,0.5,0))
			plot(NA, xlim=xlim, ylim=ylim, xaxs="i", yaxs="i", las=1, cex.axis=1.2, cex.lab=1.5, xlab=linguaFranca("Age (years)",l), ylab=linguaFranca("Predicted Length (cm)",l))
			axis(1, at=A, tcl=-0.25, labels=FALSE)
			abline(h=seq(0,ymax,2), v=c(0,A,A+1), col="whitesmoke", lwd=0.5)
			for (i in iii) {
				for (j in stocks) {
					ij = paste(j,i,sep=".")
#browser();return()
					lty = sltys[ij]
					col = scols[ij]
					vb = VBfun(dat[[j]][i,2:4], a=A)
					lines(A, vb, col=col, lty=lty, ...) ##lwd=3
#if (i=="Male") {browser();return()}
				}
			}
			lcol = rep(scols,nstock)
			llty = rep(sltys,nstock)
			legtxt = paste0(rep(stocks,length(iii))," -- ",rep(iii,each=nstock))
			if (all(grepl("\\.mcmc",legtxt)))
				legtxt = gsub("\\."," ", gsub("\\.mcmc|\\.err","",legtxt))
			else
				legtxt = gsub("\\."," ", gsub("\\.err","",legtxt))
			addLegend(0.9,0.05, lty=llty, col=lcol, xjust=1, yjust=0, text.col=lcol, lwd=3, seg.len=3.5, bty="n",
				legend=linguaFranca(legtxt,l), cex=1.25)
			box(col="slategray")
			if (png) dev.off()
		}; eop()
	} ## end if subset=="sex"
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~compVB


## compVBpars---------------------------2021-02-02
##  Compare MCMC derived parameters from von Bertalanffy model
## ---------------------------------------------RH
compVBpars = function(bfiles, prefix="vbstan.barf.", 
   pnams=c("linf","k","t0"), redo.QVB=TRUE, quants=c(0.05,0.25,0.5,0.75,0.95),
   bcol=c("orange", "green4"), bgrp=c("female","male"), 
   outnam, png=FALSE, pngres=400, PIN=c(8,8), lang=c("e","f"))
{
	## Compare quantile plots for each parameter
	bnams = gsub("\\.rda$","",gsub(prefix,"",bfiles))
	if (!file.exists("QVB.rda") || redo.QVB) {
		#QVB   = array(NA, dim=c(nmcmc, length(pnams), length(bfiles)), dimnames=list(1:nmcmc, pnams, bnams))
		for (i in 1:length(bfiles)) {
			kk = bnams[i]
			if (!file.exists(bfiles[i]))
				stop(paste0("File '", bfiles[i], "' does not exist."))
			load(bfiles[i])        ## loads as object 'barf'
			ibarf = barf$p[pnams]  ## p = random effects fit
			idat  = as.data.frame(ibarf)
			nmcmc = nrow(idat)
			if (i==1) {
				QVB = array(NA, dim=c(dim(idat),length(bfiles)), dimnames=list(1:nmcmc, pnams, bnams))
				ii = as.character(1:nmcmc); jj = pnams
			}
			QVB[ii,jj,kk] = as.matrix(idat[ii,jj])
		}
		save("QVB", file="QVB.rda")
	} else {
		load("QVB.rda")
	}
	if (!all(bnams %in% dimnames(QVB)[[3]]))
		stop ("You need to re-create QVB, set 'redo.QVB=TRUE'")
	QVB = QVB[,,bnams,drop=FALSE]
	pvb.med = apply(QVB[,1,],2,median)
	pvb.ord = order(pvb.med)
	col.ord = rep("white", length(pvb.ord))
	z1      = grep(paste0("\\b",bgrp[1],"\\b"),names(pvb.med)[pvb.ord])
	col.ord[z1] = bcol[1]
	z2      = grep(paste0("\\b",bgrp[2],"\\b"),names(pvb.med)[pvb.ord])
	col.ord[z2] = bcol[2]

	colgrey = "grey60"
	bpars = list(boxwex=0.5, boxcol=colgrey, boxfill=lucent((col.ord),0.50), medcol=(col.ord), whisklty=1, whiskcol=colgrey, staplecol=colgrey)

	if (missing(outnam))
		outnam = paste0(prefix, ".pars.compare")
	fout.e = outnam
	for (l in lang) {  ## could switch to other languages if available in 'linguaFranca'.
		changeLangOpts(L=l)
		fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		if (png) png(paste0(fout,".png"), units="in", res=pngres, width=PIN[1], height=PIN[2])
		expandGraph(mfrow=c(3,1),mar=c(3.5,0,1,1), oma=c(0,12,0,0))

		for (p in 1:length(pnams)) {
			pp  = p[1]
			pvb = QVB[,pp,]
			pvb.med = apply(pvb,2,median)

			ppp = c(expression(italic(L)[infinity]),expression(italic(K)),expression(italic(t)[0]),expression(italic(sigma)))[p]
			pvb.plot = pvb[,pvb.ord]; attr(pvb.plot,"class")="matrix"    ## some weird new shit with subsetting and quantBox (RH 210202)

			quantBox(pvb.plot, horizontal=TRUE, outline=FALSE, pars=bpars, yaxt="n", cex.axis=1.2)
			mtext(ppp, side=1, line=2.25, cex=1.5)
			if (par()$mfg[2]==1) {
				ylab = gsub("\\."," ", gsub("surv\\.","", gsub("bsr","BSR",gsub("rer","RER",gsub("len\\.","",colnames(pvb)[pvb.ord])))))
				## Modify the names of the stocks and genetic species (probably should make ylab an argument to the function):
				ylab = gsub("394G","RER",gsub("425G","BSR",gsub("RER","REBS south",gsub("BSR","REBS north",ylab))))
				mtext (linguaFranca(ylab,l), at=1:length(ylab), side=2, las=1, line=0.5, adj=1, cex=1)
			}
		}
		if (png) dev.off()
	}; eop()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~compVBpars


## createMA-----------------------------2023-04-20
##  Create table of DFO management actions and quotas
##  for Catch Appendix A.
## ---------------------------------------------RH
createMA =function(yrs=1979:2023, strSpp="POP", addletters=TRUE)
{
	dfo.action = dfo.quota = as.list(rep(NA,length(yrs)))
	names(dfo.action) = names(dfo.quota) = yrs
	for (i in yrs) {
		ii =as.character(i)
		dfo.action[[ii]] = list()
		dfo.quota[[ii]]  = list()
	}
	##-----1979--------------------------
	dfo.action[["1979"]][["PAH"]] = "PAH: Started limited vessel entry for Halibut fleet."
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["1979"]][["POP"]][["TRW"]][["3C"]]  = 50
	dfo.quota[["1979"]][["POP"]][["TRW"]][["5AB"]] = 2000
	dfo.quota[["1979"]][["POP"]][["TRW"]][["5E"]]  = 600
	dfo.quota[["1979"]][["POP"]][["TRW"]][["CST"]] = 2650

	##-----1980--------------------------
	dfo.action[["1980"]][["POP"]] = "POP: Started experimental over-harvesting of SW Vancouver Island POP stock."
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["1980"]][["POP"]][["TRW"]][["3C"]]  = 600
	dfo.quota[["1980"]][["POP"]][["TRW"]][["5AB"]] = 2200
	dfo.quota[["1980"]][["POP"]][["TRW"]][["5E"]]  = 800
	dfo.quota[["1980"]][["POP"]][["TRW"]][["CST"]] = 3600

	##-----1981--------------------------
	dfo.action[["1981"]][["SBF"]] = "SBF: Started limited vessel entry for Sablefish fleet."
	dfo.action[["1981"]][["WAP"]] = "WAP: Pollock TAC (1981-1994): only 4B=Areas 13-18, 29"
	dfo.action[["1981"]][["CAR"]] = "SRF: Shelf rockfish aggregate is [CAR] for 3D."
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["1981"]][["CAR"]][["TRW"]][["3D"]] = 350
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["1981"]][["POP"]][["TRW"]][["3C"]]  = 500
	dfo.quota[["1981"]][["POP"]][["TRW"]][["5AB"]] = 1500
	dfo.quota[["1981"]][["POP"]][["TRW"]][["5CD"]] = 1800
	dfo.quota[["1981"]][["POP"]][["TRW"]][["5E"]]  = 800
	dfo.quota[["1981"]][["POP"]][["TRW"]][["CST"]] = 4600

	##-----1982--------------------------
	dfo.action[["1982"]][["CAR"]] = "SRF: Shelf rockfish aggregates are [CAR] for 3D, [CAR+SGR] for 5AB."
	##~~~~~Canary|Silvergrey~~~~~~~~~~~~~
	dfo.quota[["1982"]][["CAR"]][["TRW"]][["3D"]] = 350
	dfo.quota[["1982"]][["CAR|SGR"]][["TRW"]][["5AB"]] = 1100
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["1982"]][["POP"]][["TRW"]][["3C"]]  = 500
	dfo.quota[["1982"]][["POP"]][["TRW"]][["3D"]]  = 250
	dfo.quota[["1982"]][["POP"]][["TRW"]][["5AB"]] = 1000
	dfo.quota[["1982"]][["POP"]][["TRW"]][["5CD"]] = 2000
	dfo.quota[["1982"]][["POP"]][["TRW"]][["5E"]]  = 800
	dfo.quota[["1982"]][["POP"]][["TRW"]][["CST"]] = 4550

	##-----1983--------------------------
	dfo.action[["1983"]][["POP"]] = "POP: Started experimental unlimited harvesting of Langara Spit POP stock (5EN)."
	dfo.action[["1983"]][["CAR|SGR|YTR"]] = "SRF: Shelf rockfish aggregates are [CAR+SGR+YTR] for 3D; [CAR+SGR] for 5AB; [CAR+YTR] for 5CD."
	##~~~~~Canary|Silvergrey|Yellowtail~~
	dfo.quota[["1983"]][["CAR|SGR|YTR"]][["TRW"]][["3D"]] = 600
	dfo.quota[["1983"]][["CAR|SGR"]][["TRW"]][["5AB"]] = 1100
	dfo.quota[["1983"]][["CAR|YTR"]][["TRW"]][["5CD"]] = 200
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["1983"]][["POP"]][["TRW"]][["3C"]]  = 500
	dfo.quota[["1983"]][["POP"]][["TRW"]][["3D"]]  = 250
	dfo.quota[["1983"]][["POP"]][["TRW"]][["5AB"]] = 1000
	dfo.quota[["1983"]][["POP"]][["TRW"]][["5CD"]] = 2000
	dfo.quota[["1983"]][["POP"]][["TRW"]][["CST"]] = 3750

	##-----1984--------------------------
	dfo.action[["1984"]][["POP"]] = "POP: Ended experimental over-harvesting of SW Vancouver Island POP stock."
	dfo.action[["1984"]][["CAR|SGR|YTR"]] = "SRF: Shelf rockfish aggregates are [CAR+SGR+YTR] for 3C, 3D, 5E; [CAR+SGR] for 5AB; [CAR+YTR] for 5CD."
	##~~~~~Canary|Silvergrey|Yellowtail~~
	dfo.quota[["1984"]][["CAR|SGR|YTR"]][["TRW"]][["3C"]] = 200
	dfo.quota[["1984"]][["CAR|SGR|YTR"]][["TRW"]][["3D"]] = 500 + 500
	dfo.quota[["1984"]][["CAR|SGR"]][["TRW"]][["5AB"]] = 1100
	dfo.quota[["1984"]][["CAR|YTR"]][["TRW"]][["5CD"]] = 450
	dfo.quota[["1984"]][["CAR|SGR|YTR"]][["TRW"]][["5E"]] = 950
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["1984"]][["POP"]][["TRW"]][["3C"]]  = 500
	dfo.quota[["1984"]][["POP"]][["TRW"]][["3D"]]  = 250
	dfo.quota[["1984"]][["POP"]][["TRW"]][["5AB"]] = 800
	dfo.quota[["1984"]][["POP"]][["TRW"]][["5CD"]] = 2000
	dfo.quota[["1984"]][["POP"]][["TRW"]][["CST"]] = 3550

	##-----1985--------------------------
	dfo.action[["1985"]][["CAR|SGR|YTR"]] = "SRF: Shelf rockfish aggregates are [CAR+SGR+YTR] for 3C, 3D, 5E; [CAR+SGR] for 5AB; [CAR] for 5CD."
	##~~~~~Canary|Silvergrey|Yellowtail~~
	dfo.quota[["1985"]][["CAR|SGR|YTR"]][["TRW"]][["3C"]] = 300
	dfo.quota[["1985"]][["CAR|SGR|YTR"]][["TRW"]][["3D"]] = 500 + 500
	dfo.quota[["1985"]][["CAR|SGR"]][["TRW"]][["5AB"]] = 1100
	dfo.quota[["1985"]][["CAR"]][["TRW"]][["5CD"]] = 450
	dfo.quota[["1985"]][["CAR|SGR|YTR"]][["TRW"]][["5E"]] = 950
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["1985"]][["POP"]][["TRW"]][["3C"]]  = 300
	dfo.quota[["1985"]][["POP"]][["TRW"]][["3D"]]  = 350
	dfo.quota[["1985"]][["POP"]][["TRW"]][["5AB"]] = 850
	dfo.quota[["1985"]][["POP"]][["TRW"]][["5CD"]] = 2000
	dfo.quota[["1985"]][["POP"]][["TRW"]][["CST"]] = 3500

	##-----1986--------------------------
	dfo.action[["1986"]][["POP|YMR|RER"]] = "SRF: Slope rockfish (POP, YMR, RER) coastwide quota = 5000t."
	dfo.action[["1986"]][["CAR|SGR|YTR"]] = "SRF: Shelf rockfish aggregates are [CAR+SGR+YTR] for 3C, 3D, 5E; [CAR+SGR] for 5AB; [CAR] for 5CD; coastwide quota = 4100t; 1986 quotas revised in 1988 MP to exclude YTR."
	##~~~~~Canary|Silvergrey~~~~~~~~~~~~~
	dfo.quota[["1986"]][["CAR|SGR"]][["TRW"]][["3C"]] = 250
	dfo.quota[["1986"]][["CAR|SGR"]][["TRW"]][["3D"]] = 800
	dfo.quota[["1986"]][["CAR|SGR"]][["TRW"]][["5AB"]] = 1100
	dfo.quota[["1986"]][["CAR"]][["TRW"]][["5CD"]] = 300
	dfo.quota[["1986"]][["CAR|SGR"]][["TRW"]][["5E"]] = 750
	dfo.quota[["1986"]][["CAR|SGR"]][["TRW"]][["CST"]] = 4100
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["1986"]][["POP"]][["TRW"]][["3C"]]  = 100
	dfo.quota[["1986"]][["POP"]][["TRW"]][["3D"]]  = 350
	dfo.quota[["1986"]][["POP"]][["TRW"]][["5AB"]] = 500
	dfo.quota[["1986"]][["POP"]][["TRW"]][["5CD"]] = 2000
	dfo.quota[["1986"]][["POP"]][["TRW"]][["CST"]] = 2950
	##~~~~~Slope Rockfish~~~~~~~~~~~~~~~~
	#dfo.quota[["1986"]][["POP|YMR|RER"]][["ZNH"]][["CST"]] = 5000
	dfo.quota[["1986"]][["YMR|RER"]][["TRW"]][["CST"]] = 5000

	##-----1987--------------------------
	dfo.action[["1987"]][["CAR|SGR"]] = "SRF: Shelf rockfish aggregates are [CAR+SGR] for 3C, 3D, 5AB, 5E; [CAR] for 5CD."
	##~~~~~Canary|Silvergrey~~~~~~~~~~~~~
	dfo.quota[["1987"]][["CAR|SGR"]][["TRW"]][["3C"]]  = 250
	dfo.quota[["1987"]][["CAR|SGR"]][["TRW"]][["3D"]]  = 800
	dfo.quota[["1987"]][["CAR|SGR"]][["TRW"]][["5AB"]] = 1100
	dfo.quota[["1987"]][["CAR"]][["TRW"]][["5CD"]]     = 300
	dfo.quota[["1987"]][["CAR|SGR"]][["TRW"]][["5E"]]  = 750
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["1987"]][["POP"]][["TRW"]][["3C"]]  = 100
	dfo.quota[["1987"]][["POP"]][["TRW"]][["3D"]]  = 350
	dfo.quota[["1987"]][["POP"]][["TRW"]][["5AB"]] = 500
	dfo.quota[["1987"]][["POP"]][["TRW"]][["5CD"]] = 2000
	dfo.quota[["1987"]][["POP"]][["TRW"]][["CST"]] = 2950

	##-----1988--------------------------
	dfo.action[["1988"]][["YMR"]] = "YMR: The quota for Yellowmouth Rockfish only applies to areas 127, 108, 109, 110, 111 and 130-1. Evidence from surveys and from commercial fishery suggests a common stock from the mouth of Queen Charlotte Sound and possibly to Cape Cook."
	dfo.action[["1988"]][["CAR|SGR"]] = "SRF: Shelf rockfish aggregates are [CAR+SGR] for 3C, 3D, 5AB, 5E; [CAR] for 5CD."
	##~~~~~Canary|Silvergrey~~~~~~~~~~~~~
	dfo.quota[["1988"]][["CAR|SGR"]][["TRW"]][["3C"]]  = 300
	dfo.quota[["1988"]][["CAR|SGR"]][["TRW"]][["3D"]]  = 800
	dfo.quota[["1988"]][["CAR|SGR"]][["TRW"]][["5AB"]] = 1100
	dfo.quota[["1988"]][["CAR"]][["TRW"]][["5CD"]]     = 300
	dfo.quota[["1988"]][["CAR|SGR"]][["TRW"]][["5E"]]  = 750
	dfo.quota[["1988"]][["CAR|SGR"]][["TRW"]][["CST"]] = 3850
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["1988"]][["POP"]][["TRW"]][["3C"]]  = 100
	dfo.quota[["1988"]][["POP"]][["TRW"]][["3D"]]  = 350
	dfo.quota[["1988"]][["POP"]][["TRW"]][["5AB"]] = 700
	dfo.quota[["1988"]][["POP"]][["TRW"]][["5CD"]] = 3000
	dfo.quota[["1988"]][["POP"]][["TRW"]][["CST"]] = 4150
	##~~~~~Slope Rockfish~~~~~~~~~~~~~~~~
	#dfo.quota[["1988"]][["POP|YMR"]][["ZNH"]][["CST"]] = 6075
	dfo.quota[["1988"]][["YMR"]][["TRW"]][["CST"]] = 6075

	##-----1989--------------------------
	dfo.action[["1989"]][["POP|YMR|CAR|SGR"]] = paste0(strSpp,": In 1989, quota rockfish comprising Pacific Ocean Perch, Yellowmouth Rockfish, Canary Rockfish and Silvergray Rockfish, will be managed on a coastwide basis.")
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["1989"]][["CAR"]][["TRW"]][["3CD"]] = 600
	dfo.quota[["1989"]][["CAR"]][["TRW"]][["5AB"]] = 425
	dfo.quota[["1989"]][["CAR"]][["TRW"]][["5CD"]] = 300
	dfo.quota[["1989"]][["CAR"]][["TRW"]][["5E"]]  = 500
	dfo.quota[["1989"]][["CAR"]][["TRW"]][["CST"]] = 1575
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["1989"]][["POP"]][["TRW"]][["3C"]]  = 150
	dfo.quota[["1989"]][["POP"]][["TRW"]][["3D"]]  = 400
	dfo.quota[["1989"]][["POP"]][["TRW"]][["5AB"]] = 850
	dfo.quota[["1989"]][["POP"]][["TRW"]][["5CD"]] = 3000
	dfo.quota[["1989"]][["POP"]][["TRW"]][["5E"]]  = 400
	dfo.quota[["1989"]][["POP"]][["TRW"]][["CST"]] = 4800
	##~~~~~Rougheye Rockfish~~~~~~~~~~~~~
	dfo.quota[["1989"]][["RER"]][["TRW"]][["5E"]]  = 250
	##~~~~~Silvergray Rockfish~~~~~~~~~~~
	dfo.quota[["1989"]][["SGR"]][["TRW"]][["3CD"]] = 500
	dfo.quota[["1989"]][["SGR"]][["TRW"]][["5AB"]] = 850
	dfo.quota[["1989"]][["SGR"]][["TRW"]][["5CD"]] = 650
	dfo.quota[["1989"]][["SGR"]][["TRW"]][["5E"]]  = 250
	dfo.quota[["1989"]][["SGR"]][["TRW"]][["CST"]] = 2125
	##~~~~~Yellowmouth Rockfish~~~~~~~~~~
	dfo.quota[["1989"]][["YMR"]][["TRW"]][["5AB"]] = 500
	dfo.quota[["1989"]][["YMR"]][["TRW"]][["5CD"]] = 350
	dfo.quota[["1989"]][["YMR"]][["TRW"]][["5E"]]  = 600
	dfo.quota[["1989"]][["YMR"]][["TRW"]][["CST"]] = 1450
	##~~~~~Yellowtail Rockfish~~~~~~~~~~
	dfo.quota[["1989"]][["YTR"]][["TRW"]][["CST"]] = 3500

	##-----1990--------------------------
	dfo.action[["1990"]][["PAH|SBF"]] = paste0(strSpp,": Started \\emph{Individual Vessel Quotas} (IVQ) systems for Halibut and Sablefish.")
	dfo.action[["1990"]][["CAR|SGR"]] = paste0(strSpp,": Only one half of the 1990 5E South area quotas (250t of CAR and 125t of SGR) have been included in the overall coastwide quotas, due to past underharvesting. Should the area quotas allocated be attained, additional quotas of 250t for CAR and 125t for SGR may be added to the coastwide quotas at a later date.")
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["1990"]][["CAR"]][["TRW"]][["5E"]]  = 250
	dfo.quota[["1990"]][["CAR"]][["TRW"]][["CST"]] = 1475
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["1990"]][["POP"]][["TRW"]][["3C"]]  = 150
	dfo.quota[["1990"]][["POP"]][["TRW"]][["3D"]]  = 400
	dfo.quota[["1990"]][["POP"]][["TRW"]][["5AB"]] = 850
	dfo.quota[["1990"]][["POP"]][["TRW"]][["5CD"]] = 2450
	dfo.quota[["1990"]][["POP"]][["TRW"]][["5E"]]  = 400
	dfo.quota[["1990"]][["POP"]][["TRW"]][["CST"]] = 4250
	##~~~~~Silvergray Rockfish~~~~~~~~~~~
	dfo.quota[["1990"]][["SGR"]][["TRW"]][["5E"]]  = 125
	dfo.quota[["1990"]][["SGR"]][["TRW"]][["CST"]] = 1900
	##~~~~~Yellowmouth Rockfish~~~~~~~~~~
	dfo.quota[["1990"]][["YMR"]][["TRW"]][["CST"]] = 1380
	##~~~~~Yellowtail Rockfish~~~~~~~~~~
	dfo.quota[["1990"]][["YTR"]][["TRW"]][["CST"]] = 4800

	##-----1991--------------------------
	dfo.action[["1991"]][["PAH"]] = "PAH: Started \\emph{Dockside Monitoring Program} (DMP) for the Halibut fleet."
	dfo.action[["1991"]][["QBR|YYR"]] = paste0(strSpp,": Started limited vessel entry for \\emph{Hook and Line} (H\\&L) fleet inside.")
	dfo.action[["1991"]][["CAR|SGR"]] = paste0(strSpp,": For 5E South, 125t of CAR and 125t of SGR have been included in the overall coastwide quotas, due to past underharvesting. Should the allocated area quotas be attained, further tonnages may be added to the applicable coastwide quotas at a later date.")
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["1991"]][["CAR"]][["TRW"]][["5E"]]  = 125
	dfo.quota[["1991"]][["CAR"]][["TRW"]][["CST"]] = 1350
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["1991"]][["POP"]][["TRW"]][["3C"]]  = 0
	dfo.quota[["1991"]][["POP"]][["TRW"]][["3D"]]  = 400
	dfo.quota[["1991"]][["POP"]][["TRW"]][["5AB"]] = 850
	dfo.quota[["1991"]][["POP"]][["TRW"]][["5CD"]] = 2150
	dfo.quota[["1991"]][["POP"]][["TRW"]][["5E"]]  = 400
	dfo.quota[["1991"]][["POP"]][["TRW"]][["CST"]] = 3800
	##~~~~~Silvergray Rockfish~~~~~~~~~~~
	dfo.quota[["1991"]][["SGR"]][["TRW"]][["5E"]]  = 125
	dfo.quota[["1991"]][["SGR"]][["TRW"]][["CST"]] = 1575
	##~~~~~Yellowmouth Rockfish~~~~~~~~~~
	dfo.quota[["1991"]][["YMR"]][["TRW"]][["CST"]] = 1380

	##-----1992--------------------------
	dfo.action[["1992"]][["QBR|YYR"]] = paste0(strSpp,": Started limited vessel entry for H\\&L fleet outside.")
	dfo.action[["1992"]][["CAR|SGR"]] = paste0(strSpp,": For 5E South, 50t of CAR and 125t of SGR have been included in the overall coastwide quotas, due to past underharvesting. Should the allocated area quotas be attained, further tonnages may be added to the applicable coastwide quotas at a later date.")
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["1992"]][["CAR"]][["TRW"]][["5E"]]  = 50
	dfo.quota[["1992"]][["CAR"]][["TRW"]][["CST"]] = 1275
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["1992"]][["POP"]][["TRW"]][["3C"]]  = 0
	dfo.quota[["1992"]][["POP"]][["TRW"]][["3D"]]  = 400
	dfo.quota[["1992"]][["POP"]][["TRW"]][["5AB"]] = 850
	dfo.quota[["1992"]][["POP"]][["TRW"]][["5CD"]] = 2400
	dfo.quota[["1992"]][["POP"]][["TRW"]][["5E"]]  = 400
	dfo.quota[["1992"]][["POP"]][["TRW"]][["CST"]] = 4050
	##~~~~~Silvergray Rockfish~~~~~~~~~~~
	dfo.quota[["1992"]][["SGR"]][["TRW"]][["5E"]]  = 125
	dfo.quota[["1992"]][["SGR"]][["TRW"]][["CST"]] = 1575
	##~~~~~Yellowmouth Rockfish~~~~~~~~~~
	dfo.quota[["1992"]][["YMR"]][["TRW"]][["CST"]] = 1380
	##~~~~~Yellowtail Rockfish~~~~~~~~~~
	dfo.quota[["1992"]][["YTR"]][["TRW"]][["CST"]] = 4800

	##-----1993--------------------------
	dfo.action[["1993"]][["POPa"]] = "POP: Stopped experimental fishing of Langara Spit POP stock."
	dfo.action[["1993"]][["POPb"]] = "POP: Closed POP fishery in PMFC area 5EN (Langara Spit)."
	dfo.action[["1993"]][["RER|REBS"]] = "RER: Trip limits for trawl specified for the first time."
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["1993"]][["CAR"]][["TRW"]][["CST"]] = 850
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["1993"]][["POP"]][["TRW"]][["3C"]]  = 150
	dfo.quota[["1993"]][["POP"]][["TRW"]][["3D"]]  = 400
	dfo.quota[["1993"]][["POP"]][["TRW"]][["5AB"]] = 850
	dfo.quota[["1993"]][["POP"]][["TRW"]][["5CD"]] = 2400
	dfo.quota[["1993"]][["POP"]][["TRW"]][["5E"]]  = 400
	dfo.quota[["1993"]][["POP"]][["TRW"]][["CST"]] = 4200
	##~~~~~Redstripe Rockfish~~~~~~~~~~~~
	dfo.quota[["1993"]][["RSR"]][["TRW"]][["CST"]] = 2200
	##~~~~~Silvergray Rockfish~~~~~~~~~~~
	dfo.quota[["1993"]][["SGR"]][["TRW"]][["CST"]] = 1255
	##~~~~~Widow Rockfish~~~~~~~~~~~~~~~~
	dfo.quota[["1993"]][["WWR"]][["TRW"]][["CST"]] = 1800
	##~~~~~Yellowmouth Rockfish~~~~~~~~~~
	dfo.quota[["1993"]][["YMR"]][["TRW"]][["CST"]] = 1380
	##~~~~~Yellowtail Rockfish~~~~~~~~~~
	dfo.quota[["1993"]][["YTR"]][["TRW"]][["CST"]] = 4700

	##-----1994--------------------------
	dfo.action[["1994"]][["@@@"]] = "TWL: Started a dockside monitoring program (DMP) for the Trawl fleet."
	dfo.action[["1994"]][["POP|YMR|RER|CAR|SGR|YTR|RSR|WWR|SKR|SST|LST|REBS"]] = paste0(strSpp,": As a means of both reducing at-sea discarding and simplifying the harvesting regime, rockfish aggregation was implemented. Through consultation with GTAC, the following aggregates were identified: Agg~1=~POP, YMR, RER, CAR, SGR, YTR; Agg~2=~RSR, WWR; Agg~3=~SKR, SST, LST; Agg~4=~ORF.")
	##~~~~~Aggregates~~~~~~~~~~~~~~~~~~~~
	dfo.quota[["1994"]][["POP|YMR|RER|CAR|SGR|YTR"]][["TRW"]][["CST"]] = 12574
	dfo.quota[["1994"]][["RSR|WWR"]][["TRW"]][["CST"]] = 4000
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["1994"]][["POP"]][["TRW"]][["3C"]]  = 1173
	dfo.quota[["1994"]][["POP"]][["TRW"]][["3D"]]  = 207
	dfo.quota[["1994"]][["POP"]][["TRW"]][["5AB"]] = 2177
	dfo.quota[["1994"]][["POP"]][["TRW"]][["5CD"]] = 1107
	dfo.quota[["1994"]][["POP"]][["TRW"]][["5E"]]  = 253
	dfo.quota[["1994"]][["POP"]][["TRW"]][["CST"]] = 4917

	##-----1995--------------------------
	dfo.action[["1995"]][["BOR|RBR|RSR|SST"]] = paste0(strSpp,": Implemented catch limits (monthly) on rockfish aggregates for H\\&L.")
	dfo.action[["1995"]][["WAP"]] = "WAP: Pollock TAC areas: 5CDE=5CD; 5AB=Area 12; 4B=Areas 13-18, 29."
	dfo.action[["1995"]][["CAR|SGR|YTR|WWR|RER|POP|YMR|RSR|SKR|SST|LST|REBS"]] = paste0(strSpp,": Trawl aggregates established in 1994 changed: Agg~1=~CAR, SGR, YTR, WWR, RER; Agg~2=~POP, YMR, RSR; Agg~3=~SKR, SST, LST; Agg~4=~ORF.")
	##~~~~~Aggregates~~~~~~~~~~~~~~~~~~~~
	dfo.quota[["1995"]][["CAR|SGR|YTR|WWR|RER"]][["TRW"]][["CST"]] = 9716
	dfo.quota[["1995"]][["POP|YMR|RSR"]][["TRW"]][["CST"]] = 7320
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["1995"]][["POP"]][["TRW"]][["3C"]]  = 548
	dfo.quota[["1995"]][["POP"]][["TRW"]][["3D"]]  = 72
	dfo.quota[["1995"]][["POP"]][["TRW"]][["5AB"]] = 1892
	dfo.quota[["1995"]][["POP"]][["TRW"]][["5CD"]] = 1178
	dfo.quota[["1995"]][["POP"]][["TRW"]][["5E"]]  = 544
	dfo.quota[["1995"]][["POP"]][["TRW"]][["CST"]] = 4234

	##-----1996--------------------------
	dfo.action[["1996"]][["@@@"]] = "TWL: Started 100\\% onboard observer program for offshore Trawl fleet."
	dfo.action[["1996"]][["BOR|RBR|RSR|SST"]] = paste0(strSpp,": Started DMP for H\\&L fleet.")
	dfo.action[["1996"]][["YTR|WWR|CAR|SGR|POP|YMR|RER|SKR|RSR|SCR|SST|LST|REBS"]] = paste0(strSpp,": Rockfish aggregation will continue on a limited basis in 1996: Agg~1=~YTR, WWR; Agg~2=~CAR, SGR; Agg~3=~POP, YMR; Agg~4=~RER, SKR; Agg~5=~RSR, SCR; Agg~6=~ORF incl. SST, LST")
	dfo.action[["1996"]][["WAP"]] = "WAP: Pollock TAC areas: 5CDE=5CD; 5AB=Areas 11,12; 4B=Areas 13-18, 29"
	##~~~~~Aggregates~~~~~~~~~~~~~~~~~~~~
	dfo.quota[["1996"]][["YTR|WWR"]][["TRW"]][["CST"]] = 7734
	dfo.quota[["1996"]][["CAR|SGR"]][["TRW"]][["CST"]] = 2085
	dfo.quota[["1996"]][["POP|YMR"]][["TRW"]][["CST"]] = 6884
	dfo.quota[["1996"]][["RER|SKR"]][["TRW"]][["CST"]] = 1311
	dfo.quota[["1996"]][["LST|SST"]][["TRW"]][["CST"]] = 752
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["1996"]][["CAR"]][["ZNH"]][["CST"]] = 738
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["1996"]][["POP"]][["TRW"]][["3C"]]  = 491
	dfo.quota[["1996"]][["POP"]][["TRW"]][["3D"]]  = 164
	dfo.quota[["1996"]][["POP"]][["TRW"]][["5AB"]] = 1500
	dfo.quota[["1996"]][["POP"]][["TRW"]][["5CD"]] = 4003
	dfo.quota[["1996"]][["POP"]][["TRW"]][["5E"]]  = 726
	dfo.quota[["1996"]][["POP"]][["TRW"]][["CST"]] = 6884

	##-----1997--------------------------
	dfo.action[["1997"]][["@@@"]] = "TWL: Started IVQ system for Trawl Total Allowable Catch (TAC) species (April 1, 1997)"
	dfo.action[["1997"]][["BOR|RBR"]] = paste0(strSpp,": Implemented catch limits (15,000 lbs per trip) on combined non-TAC rockfish for the Trawl fleet.")
	dfo.action[["1997"]][["POP|YMR"]] = paste0(strSpp,": Permanent boundary adjustment -- Pacific Ocean Perch and Yellowmouth Rockfish caught within Subarea 102-3 and those portions of Subareas 142-1, 130-3 and 130-2 found southerly and easterly of a straight line commencing at 52$^\\circ$20$'$00$''$N 131$^\\circ$36$'$00$''$W thence to 52$^\\circ$20$'$00$''$N 132$^\\circ$00$'$00$''$W thence to 51$^\\circ$30$'$00$''$N 131$^\\circ$00$'$00$''$W and easterly and northerly of a straight line commencing at 51$^\\circ$30$'$00$''$N 131$^\\circ$00$'$00$''$W thence to 51$^\\circ$39$'$20$''$N 130$^\\circ$30$'$30$''$W will be deducted from the vessel's 5CD IVQ for those two species.")
	dfo.action[["1997"]][["QBR|CPR|CHR|TIR|CAR|SGR|RER|SKR|SST|LST|POP|YMR|RSR|YTR|BKR|WWR|REBS"]] = "H\\&L: All H\\&L rockfish, with the exception of YYR, shall be managed under the following rockfish aggregates: Agg~1=~QBR, CPR; Agg~2=~CHR, TIR; Agg~3=~CAR, SGR; Agg~4=~RER, SKR, SST, LST; Agg~5=~POP, YMR, RSR; Agg~6=~YTR, BKR, WWR; Agg~7=~ORF excluding YYR."
	dfo.action[["1996"]][["CAR"]] = "CAR: groundfish equivalent price (GFE) relative to POP = 1.19"
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["1997"]][["CAR"]][["TRW"]][["3CD"]]  = 503
	dfo.quota[["1997"]][["CAR"]][["TRW"]][["5AB"]]  = 345
	dfo.quota[["1997"]][["CAR"]][["TRW"]][["5CDE"]] = 81
	dfo.quota[["1997"]][["CAR"]][["TRW"]][["CST"]]  = 929
	dfo.quota[["1997"]][["CAR"]][["ZNH"]][["CST"]]  = 906
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["1997"]][["POP"]][["TRW"]][["3C"]]  = 431
	dfo.quota[["1997"]][["POP"]][["TRW"]][["3D"]]  = 230
	dfo.quota[["1997"]][["POP"]][["TRW"]][["5AB"]] = 2358
	dfo.quota[["1997"]][["POP"]][["TRW"]][["5CD"]] = 2818
	dfo.quota[["1997"]][["POP"]][["TRW"]][["5E"]]  = 644
	dfo.quota[["1997"]][["POP"]][["TRW"]][["CST"]] = 6481

	##-----1998--------------------------
	dfo.action[["1998"]][["YYR"]] = "H\\&L: Aggregate 4 -- Option A: a quantity of Aggregates 2 to 5 and 7 combined not to exceed 100\\% of the total of Aggregate 1 per landing; an overage of Aggregate 1 and 6 up to a maximum of 10\\% per fishing period which shall be deducted from the vessel's succeeding fishing period limit. Option B: a quantity of Aggregates 2 to 7 combined not to exceed 100\\% of the Yelloweye rockfish per landing. Option C: 20,000 pounds of Aggregate 4 per fishing period; an overage for each of the Aggregates 3 to 5 and, Aggregates 6 and 7 combined, up to a maximum of 20\\% per fishing period which shall be deducted from the vessel's succeeding fishing period limit."
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["1998"]][["CAR"]][["TRW"]][["3CD"]] = 503
	dfo.quota[["1998"]][["CAR"]][["TRW"]][["5AB"]] = 345
	dfo.quota[["1998"]][["CAR"]][["TRW"]][["5CDE"]] = 81
	dfo.quota[["1998"]][["CAR"]][["TRW"]][["CST"]] = 929
	dfo.quota[["1998"]][["CAR"]][["ZNH"]][["CST"]] = 74
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["1998"]][["POP"]][["TRW"]][["3C"]]  = 300
	dfo.quota[["1998"]][["POP"]][["TRW"]][["3D"]]  = 230
	dfo.quota[["1998"]][["POP"]][["TRW"]][["5AB"]] = 2070
	dfo.quota[["1998"]][["POP"]][["TRW"]][["5CD"]] = 2817
	dfo.quota[["1998"]][["POP"]][["TRW"]][["5E"]]  = 730
	dfo.quota[["1998"]][["POP"]][["TRW"]][["CST"]] = 6147

	##-----1999--------------------------
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["1999"]][["CAR"]][["TRW"]][["3CD"]] = 499
	dfo.quota[["1999"]][["CAR"]][["TRW"]][["5AB"]] = 342
	dfo.quota[["1999"]][["CAR"]][["TRW"]][["5CDE"]] = 80
	dfo.quota[["1999"]][["CAR"]][["TRW"]][["CST"]] = 921
	dfo.quota[["1999"]][["CAR"]][["ZNH"]][["CST"]] = 76
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["1999"]][["POP"]][["TRW"]][["3C"]]  = 300
	dfo.quota[["1999"]][["POP"]][["TRW"]][["3D"]]  = 230
	dfo.quota[["1999"]][["POP"]][["TRW"]][["5AB"]] = 2070
	dfo.quota[["1999"]][["POP"]][["TRW"]][["5CD"]] = 2817
	dfo.quota[["1999"]][["POP"]][["TRW"]][["5E"]]  = 730
	dfo.quota[["1999"]][["POP"]][["TRW"]][["CST"]] = 6147

	##-----2000--------------------------
	dfo.action[["2000"]][["PAH|RBR"]] = "PAH: Implemented catch limits (20,000 lbs per trip) on rockfish aggregates for the Halibut option D fleet."
	dfo.action[["2000"]][["PAH|RBR|SST|BSR|RER|REBS"]] = "H\\&L: Implemented formal allocation of rockfish species between Halibut and H\\&L sectors."
	dfo.action[["2000"]][["@@@"]] = "ALL: Formal discussions between the hook and line rockfish (ZN), halibut and trawl sectors were initiated in 2000 to establish individual rockfish species allocations between the sectors to replace the 92/8 split. Allocation arrangements were agreed to for rockfish species that are not currently under TAC. Splits agreed upon for these rockfish will be implemented in the future when or if TACs are set for those species."
	dfo.action[["2000"]][["LST|SST"]] = "TWL: DFO cut LST TAC off WCVI to 404~t and set a conditional TAC of 425~t for an exploratory fishery north of 230$^\\circ$ true from Lookout Island."
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2000"]][["CAR"]][["TRW"]][["3CD"]] = 555
	dfo.quota[["2000"]][["CAR"]][["TRW"]][["5AB"]] = 277
	dfo.quota[["2000"]][["CAR"]][["TRW"]][["5CD"]] = 106
	dfo.quota[["2000"]][["CAR"]][["TRW"]][["5E"]] =  159
	dfo.quota[["2000"]][["CAR"]][["TRW"]][["CST"]] = 1097
	dfo.quota[["2000"]][["CAR"]][["ZNH"]][["CST"]] = 92.5
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2000"]][["POP"]][["TRW"]][["3C"]]  = 300
	dfo.quota[["2000"]][["POP"]][["TRW"]][["3D"]]  = 230
	dfo.quota[["2000"]][["POP"]][["TRW"]][["5AB"]] = 2070
	dfo.quota[["2000"]][["POP"]][["TRW"]][["5CD"]] = 2818
	dfo.quota[["2000"]][["POP"]][["TRW"]][["5E"]]  = 730
	dfo.quota[["2000"]][["POP"]][["TRW"]][["CST"]] = 6148

	##-----2001--------------------------
	dfo.action[["2001"]][["@@@"]] = "ALL: An agreement reached amongst the commercial groundfish industry has established the allocation of the rockfish species between the commercial Groundfish Trawl and Groundfish Hook and Line sectors."
	dfo.action[["2001"]][["BOR"]] = "BOR: PSARC (now CSAP) concerned that the decline of abundance indices for Bocaccio from the West Coast Vancouver Island (WCVI) Shrimp survey data and, in particular, the U.S. Triennial survey data reflected a serious decline. A detailed review of all survey indices was recommended to assess trends in Bocaccio abundance."
	dfo.action[["2001"]][["BSR|RER|REBS"]] = "RER: Set commercial allocations among sectors (ongoing to 2019): Trawl 55.8\\%, H\\&L 41.17\\%, Halibut 3.03\\%."
	dfo.action[["2001"]][["CAR"]] = "CAR: Sector allocations: T=87.7\\%, HL=12.3\\%"
	dfo.action[["2001"]][["POP"]] = "POP: TAC reduction (3y) for POP -- DFO reduced the 5CD POP TAC by 300 tonnes for research use as payment for the Hecate Strait Pacific Cod charter for each of the next three fishing seasons."
	dfo.action[["2001"]][["SGR"]] = "SGR: TAC reduction (3y) for SGR -- DFO has adopted conservative F=M harvest strategy in establishing the Silvergrey Rockfish TAC for all areas except 5AB. In 5AB the TAC will be stepped downward by 60 tonnes annually for each of the 2001/2002, 2003/2004 and 2003/2004 seasons to achieve this harvest strategy."
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2001"]][["CAR"]][["TRW"]][["3CD"]] = 529
	dfo.quota[["2001"]][["CAR"]][["TRW"]][["5AB"]] = 265
	dfo.quota[["2001"]][["CAR"]][["TRW"]][["5CD"]] = 101
	dfo.quota[["2001"]][["CAR"]][["TRW"]][["5E"]] =  151
	dfo.quota[["2001"]][["CAR"]][["TRW"]][["CST"]] = 1046
	dfo.quota[["2001"]][["CAR"]][["ZNH"]][["CST"]] = 140
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2001"]][["POP"]][["TRW"]][["3C"]]  = 300
	dfo.quota[["2001"]][["POP"]][["TRW"]][["3D"]]  = 230
	dfo.quota[["2001"]][["POP"]][["TRW"]][["5AB"]] = 2070
	dfo.quota[["2001"]][["POP"]][["TRW"]][["5CD"]] = 2818
	dfo.quota[["2001"]][["POP"]][["TRW"]][["5E"]]  = 730
	dfo.quota[["2001"]][["POP"]][["TRW"]][["CST"]] = 6148

	##-----2002--------------------------
	dfo.action[["2002"]][["QBR|YYR|CPR|CHR|TIR"]] = "H\\&L: Established the inshore rockfish conservation strategy."
	dfo.action[["2002"]][["@@@"]] = "TWL: Closed areas to preserve four hexactinellid (glassy) sponge reefs."
	dfo.action[["2002"]][["LST|SST"]] = "TWL: Managers created 5 LST management zones coastwide (WCVI, Triangle, Tidemarks, Flamingo, Rennell); zones north of WCVI were designated ``experimental''."
	dfo.action[["2002"]][["BOR"]] = "BOR: Status of Bocaccio was designated as `Threatened' by the Committee on the Status of Endangered Wildlife in Canada (COSEWIC) in November 2002. The designation was based on a new status report that indicated a combination of low recruitment and high fishing mortality had resulted in severe declines and low spawning abundance of this species. As the Species at Risk Act (SARA) was not yet in place, there was no legal designation for Bocaccio. Protection under SARA would only come in the event that this species was listed, by regulation, under the Act."
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2002"]][["CAR"]][["TRW"]][["3CD"]] = 529
	dfo.quota[["2002"]][["CAR"]][["TRW"]][["5AB"]] = 265
	dfo.quota[["2002"]][["CAR"]][["TRW"]][["5CD"]] = 101
	dfo.quota[["2002"]][["CAR"]][["TRW"]][["5E"]] =  151
	dfo.quota[["2002"]][["CAR"]][["TRW"]][["CST"]] = 1046
	dfo.quota[["2002"]][["CAR"]][["ZNH"]][["CST"]] = 140
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2002"]][["POP"]][["TRW"]][["3C"]]  = 300
	dfo.quota[["2002"]][["POP"]][["TRW"]][["3D"]]  = 230
	dfo.quota[["2002"]][["POP"]][["TRW"]][["5AB"]] = 2070
	dfo.quota[["2002"]][["POP"]][["TRW"]][["5CD"]] = 2518
	dfo.quota[["2002"]][["POP"]][["TRW"]][["5E"]]  = 730
	dfo.quota[["2002"]][["POP"]][["TRW"]][["CST"]] = 5848

	##-----2003--------------------------
	dfo.action[["2003"]][["BOR|CAR|LST|YMR|YYR|BSR|RER|REBS"]] = paste0(strSpp,": Species at Risk Act (SARA) came into force in 2003.")
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2003"]][["CAR"]][["TRW"]][["3CD"]] = 529
	dfo.quota[["2003"]][["CAR"]][["TRW"]][["5AB"]] = 265
	dfo.quota[["2003"]][["CAR"]][["TRW"]][["5CD"]] = 101
	dfo.quota[["2003"]][["CAR"]][["TRW"]][["5E"]] =  151
	dfo.quota[["2003"]][["CAR"]][["TRW"]][["CST"]] = 1046
	dfo.quota[["2003"]][["CAR"]][["ZNH"]][["CST"]] = 140
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2003"]][["POP"]][["TRW"]][["3C"]]  = 300
	dfo.quota[["2003"]][["POP"]][["TRW"]][["3D"]]  = 230
	dfo.quota[["2003"]][["POP"]][["TRW"]][["5AB"]] = 2070
	dfo.quota[["2003"]][["POP"]][["TRW"]][["5CD"]] = 2818
	dfo.quota[["2003"]][["POP"]][["TRW"]][["5E"]]  = 730
	dfo.quota[["2003"]][["POP"]][["TRW"]][["CST"]] = 6148

	##-----2004--------------------------
	dfo.action[["2004"]][["BOR"]] = "BOR: DFO reviewed management measures in the groundfish fisheries to assess the impacts on listed species under SARA. Voluntary program for the trawl fleet was developed and implemented in 2004 in which groundfish trawl vessels directed the proceeds of all landed Bocaccio Rockfish for research and management purposes. Ongoing to 2019."
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2004"]][["CAR"]][["TRW"]][["3CD"]] = 529
	dfo.quota[["2004"]][["CAR"]][["TRW"]][["5AB"]] = 265
	dfo.quota[["2004"]][["CAR"]][["TRW"]][["5CD"]] = 101
	dfo.quota[["2004"]][["CAR"]][["TRW"]][["5E"]] =  151
	dfo.quota[["2004"]][["CAR"]][["TRW"]][["CST"]] = 1046
	dfo.quota[["2004"]][["CAR"]][["ZNH"]][["CST"]] = 140
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2004"]][["POP"]][["TRW"]][["3C"]]  = 300
	dfo.quota[["2004"]][["POP"]][["TRW"]][["3D"]]  = 230
	dfo.quota[["2004"]][["POP"]][["TRW"]][["5AB"]] = 2070
	dfo.quota[["2004"]][["POP"]][["TRW"]][["5CD"]] = 2818
	dfo.quota[["2004"]][["POP"]][["TRW"]][["5E"]]  = 730
	dfo.quota[["2004"]][["POP"]][["TRW"]][["CST"]] = 6148

	##-----2005--------------------------
	dfo.action[["2005"]][["BORa"]] = "BOR: DFO consulted with First Nations, stakeholders, and the Canadian public on Bocaccio COSEWIC designation for 1.5 years and planned recommendations for further action to be presented to the Minister of Environment and Governor in Council (Cabinet) in spring 2005. A final listing decision by Governor in Council was expected in October 2005."
	dfo.action[["2005"]][["BORb"]] = "BOR: As a proactive measure, industry reduced the harvest of Bocaccio, beginning in 2004, and resulted in a reduction of the Bocaccio catch by over 50\\% percent. Subsequently, measures to avoid Bocaccio were taken in the fishing years 2005/06 through 2019/20."
	dfo.action[["2005"]][["BORc"]] = "BOR: The Government of Canada announced in November 2005 that Bocaccio be sent back to COSEWIC for further information or consideration."
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2005"]][["CAR"]][["TRW"]][["3CD"]] = 529
	dfo.quota[["2005"]][["CAR"]][["TRW"]][["5AB"]] = 265
	dfo.quota[["2005"]][["CAR"]][["TRW"]][["5CD"]] = 101
	dfo.quota[["2005"]][["CAR"]][["TRW"]][["5E"]] =  151
	dfo.quota[["2005"]][["CAR"]][["TRW"]][["CST"]] = 1046
	dfo.quota[["2005"]][["CAR"]][["ZNH"]][["CST"]] = 140
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2005"]][["POP"]][["TRW"]][["3C"]]  = 300
	dfo.quota[["2005"]][["POP"]][["TRW"]][["3D"]]  = 230
	dfo.quota[["2005"]][["POP"]][["TRW"]][["5AB"]] = 2070
	dfo.quota[["2005"]][["POP"]][["TRW"]][["5CD"]] = 2818
	dfo.quota[["2005"]][["POP"]][["TRW"]][["5E"]]  = 730
	dfo.quota[["2005"]][["POP"]][["TRW"]][["CST"]] = 6148

	##-----2006--------------------------
	dfo.action[["2006"]][["@@@"]] = "ALL: Introduced an Integrated Fisheries Management Plan (IFMP) for all directed groundfish fisheries."
	dfo.action[["2006"]][["@@@a"]] = "H\\&L: Implemented 100\\% at-sea electronic monitoring and 100\\% dockside monitoring for all groundfish H\\&L fisheries."
	dfo.action[["2006"]][["BOR|RBR|RSR|SST|WWR"]] = "H\\&L: Implemented mandatory retention of rockfish for H\\&L."
	dfo.action[["2006"]][["CAR"]] = "CAR: Sector allocations: T=87.7\\%, ZN=11.77\\%, L=0.53\\%"
	dfo.action[["2006"]][["POP"]] = "POP: TAC reduction for POP -- DFO reduced the 5CD POP TAC by 700 tonnes for use in possible research programs."
	dfo.action[["2006"]][["QBR|YYR|CPR|CHR|TIR"]] = "H\\&L: To support rockfish research the Groundfish Hook and Line Sub Committee (GHLSC) agreed to set aside 5\\% of the ZN allocations for research purposes."
	dfo.action[["2006"]][["SST"]] = "H\\&L: Annual non-directed species caps by fishery -- Shortspine Thornyhead (Dogfish = 0.05\\% Dogfish IVQ, Outside ZN = 1881 lbs., Halibut = 8000 lbs., Sablefish = 10,512 lbs.)"
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2006"]][["CAR"]][["ALL"]][["3CD"]] = 604
	dfo.quota[["2006"]][["CAR"]][["ALL"]][["5AB"]] = 302
	dfo.quota[["2006"]][["CAR"]][["ALL"]][["5CD"]] = 115
	dfo.quota[["2006"]][["CAR"]][["ALL"]][["5E"]] =  173
	dfo.quota[["2006"]][["CAR"]][["ALL"]][["CST"]] = 1193
	dfo.quota[["2006"]][["CAR"]][["TRW"]][["3CD"]] = 529
	dfo.quota[["2006"]][["CAR"]][["TRW"]][["5AB"]] = 265
	dfo.quota[["2006"]][["CAR"]][["TRW"]][["5CD"]] = 101
	dfo.quota[["2006"]][["CAR"]][["TRW"]][["5E"]] =  151
	dfo.quota[["2006"]][["CAR"]][["TRW"]][["CST"]] = 1046
	dfo.quota[["2006"]][["CAR"]][["ZNH"]][["3CD"]] = 74
	dfo.quota[["2006"]][["CAR"]][["ZNH"]][["5AB"]] = 37
	dfo.quota[["2006"]][["CAR"]][["ZNH"]][["5CD"]] = 14
	dfo.quota[["2006"]][["CAR"]][["ZNH"]][["5E"]] =  21
	dfo.quota[["2006"]][["CAR"]][["ZNH"]][["CST"]] = 147
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2006"]][["POP"]][["TRW"]][["3C"]]  = 300
	dfo.quota[["2006"]][["POP"]][["TRW"]][["3D"]]  = 230
	dfo.quota[["2006"]][["POP"]][["TRW"]][["5AB"]] = 2070
	dfo.quota[["2006"]][["POP"]][["TRW"]][["5CD"]] = 2118
	dfo.quota[["2006"]][["POP"]][["TRW"]][["5E"]]  = 730
	dfo.quota[["2006"]][["POP"]][["TRW"]][["CST"]] = 5448

	##-----2007--------------------------
	dfo.action[["2007"]][["BOR"]] = "BOR: COSEWIC reconfirmed Bocaccio's Threatened designation, and the species re-entered the SARA listing process in 2007."
	dfo.action[["2007"]][["CAR"]] = "CAR: Research allocation: H\\&L=7.0t"
	dfo.action[["2007"]][["SST|BSR|RER|REBS"]] = paste0(strSpp,": Amendment to Halibut IVQ cap for SST and RER -- reallocations can only occur in blocks up to 4000 lbs or until the vessel species cap is met. Once the first 4000 lbs has been caught additional IVQ can be reallocated onto the licence up to 4000 lbs. This can continue until the vessel species cap is met.")
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2007"]][["CAR"]] = dfo.quota[["2006"]][["CAR"]]
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2007"]][["POP"]][["TRW"]][["3C"]]  = 300
	dfo.quota[["2007"]][["POP"]][["TRW"]][["3D"]]  = 230
	dfo.quota[["2007"]][["POP"]][["TRW"]][["5AB"]] = 2070
	dfo.quota[["2007"]][["POP"]][["TRW"]][["5CD"]] = 2118
	dfo.quota[["2007"]][["POP"]][["TRW"]][["5E"]]  = 730
	dfo.quota[["2007"]][["POP"]][["TRW"]][["CST"]] = 5448

	##-----2008--------------------------
	dfo.action[["2008"]][["CARa"]] = "CAR: Stock status reviewed in Nov 2007; stock declined from original biomass but decline likely arrested; uncertain if recent catch levels will ensure rebuild; TAC for Canary reduced from 1193t to 912t coastwide."
	dfo.action[["2008"]][["CARb"]] = "CAR: Research allocation: H\\&L=7.0t"
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2008"]][["CAR"]][["ALL"]][["3CD"]] = 507
	dfo.quota[["2008"]][["CAR"]][["ALL"]][["5AB"]] = 258
	dfo.quota[["2008"]][["CAR"]][["ALL"]][["5CD"]] = 101
	dfo.quota[["2008"]][["CAR"]][["ALL"]][["5E"]] =  46
	dfo.quota[["2008"]][["CAR"]][["ALL"]][["CST"]] = 912
	dfo.quota[["2008"]][["CAR"]][["TRW"]][["3CD"]] = 450
	dfo.quota[["2008"]][["CAR"]][["TRW"]][["5AB"]] = 230
	dfo.quota[["2008"]][["CAR"]][["TRW"]][["5CD"]] = 90
	dfo.quota[["2008"]][["CAR"]][["TRW"]][["5E"]] =  30
	dfo.quota[["2008"]][["CAR"]][["TRW"]][["CST"]] = 800
	dfo.quota[["2008"]][["CAR"]][["ZNH"]][["3CD"]] = 57
	dfo.quota[["2008"]][["CAR"]][["ZNH"]][["5AB"]] = 28
	dfo.quota[["2008"]][["CAR"]][["ZNH"]][["5CD"]] = 11
	dfo.quota[["2008"]][["CAR"]][["ZNH"]][["5E"]] =  16
	dfo.quota[["2008"]][["CAR"]][["ZNH"]][["CST"]] = 112
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2008"]][["POP"]][["TRW"]][["3C"]]  = 300
	dfo.quota[["2008"]][["POP"]][["TRW"]][["3D"]]  = 230
	dfo.quota[["2008"]][["POP"]][["TRW"]][["5AB"]] = 2070
	dfo.quota[["2008"]][["POP"]][["TRW"]][["5CD"]] = 2118
	dfo.quota[["2008"]][["POP"]][["TRW"]][["5E"]]  = 730
	dfo.quota[["2008"]][["POP"]][["TRW"]][["CST"]] = 5448

	##-----2009--------------------------
	dfo.action[["2009"]][["CARa"]] = "CAR: TAC for Canary further reduced from 912t to 679t coastwide."
	dfo.action[["2009"]][["CARb"]] = "CAR: COSEWIC-designated marine species in Pacific region under consideration for listing under Schedule I of SARA: Canary as 'Threatened'."
	dfo.action[["2009"]][["CARc"]] = "CAR: Research allocation: H\\&L=4.0t"
	dfo.action[["2009"]][["LST|RER|BSR|REBS"]] = paste0(strSpp,": Listed as species of `Special Concern' under SARA; management plan required.")
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2009"]][["CAR"]][["ALL"]][["3CD"]] = 419
	dfo.quota[["2009"]][["CAR"]][["ALL"]][["5AB"]] = 178
	dfo.quota[["2009"]][["CAR"]][["ALL"]][["5CD"]] = 55
	dfo.quota[["2009"]][["CAR"]][["ALL"]][["5E"]] =  26
	dfo.quota[["2009"]][["CAR"]][["ALL"]][["CST"]] = 679
	dfo.quota[["2009"]][["CAR"]][["TRW"]][["3CD"]] = 400
	dfo.quota[["2009"]][["CAR"]][["TRW"]][["5AB"]] = 145
	dfo.quota[["2009"]][["CAR"]][["TRW"]][["5CD"]] = 40
	dfo.quota[["2009"]][["CAR"]][["TRW"]][["5E"]] =  10
	dfo.quota[["2009"]][["CAR"]][["TRW"]][["CST"]] = 595
	dfo.quota[["2009"]][["CAR"]][["ZNH"]][["3CD"]] = 19
	dfo.quota[["2009"]][["CAR"]][["ZNH"]][["5AB"]] = 33
	dfo.quota[["2009"]][["CAR"]][["ZNH"]][["5CD"]] = 15
	dfo.quota[["2009"]][["CAR"]][["ZNH"]][["5E"]] =  16
	dfo.quota[["2009"]][["CAR"]][["ZNH"]][["CST"]] = 84
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2009"]][["POP"]][["TRW"]][["3C"]]  = 300
	dfo.quota[["2009"]][["POP"]][["TRW"]][["3D"]]  = 230
	dfo.quota[["2009"]][["POP"]][["TRW"]][["5AB"]] = 2070
	dfo.quota[["2009"]][["POP"]][["TRW"]][["5CD"]] = 2118
	dfo.quota[["2009"]][["POP"]][["TRW"]][["5E"]]  = 730
	dfo.quota[["2009"]][["POP"]][["TRW"]][["CST"]] = 5448

	##-----2010--------------------------
	dfo.action[["2010"]][["CARa"]] = "CAR: Stock status reviewed in Dec 2009; stock declined from unfished equilibrium biomass but decline likely arrested; TAC for Canary increased to 900t coastwide; stock expected to rebuild and remain at levels consistent with DFO's Precautionary Approach."
	dfo.action[["2010"]][["CARb"]] = "CAR: Research allocation: H\\&L=6.0t"
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2010"]][["CAR"]][["ALL"]][["3CD"]] = 625
	dfo.quota[["2010"]][["CAR"]][["ALL"]][["5AB"]] = 183
	dfo.quota[["2010"]][["CAR"]][["ALL"]][["5CD"]] = 66
	dfo.quota[["2010"]][["CAR"]][["ALL"]][["5E"]] =  26
	dfo.quota[["2010"]][["CAR"]][["ALL"]][["CST"]] = 900
	dfo.quota[["2010"]][["CAR"]][["TRW"]][["3CD"]] = 569
	dfo.quota[["2010"]][["CAR"]][["TRW"]][["5AB"]] = 155
	dfo.quota[["2010"]][["CAR"]][["TRW"]][["5CD"]] = 55
	dfo.quota[["2010"]][["CAR"]][["TRW"]][["5E"]] =  10
	dfo.quota[["2010"]][["CAR"]][["TRW"]][["CST"]] = 789
	dfo.quota[["2010"]][["CAR"]][["ZNH"]][["3CD"]] = 26
	dfo.quota[["2010"]][["CAR"]][["ZNH"]][["5AB"]] = 43
	dfo.quota[["2010"]][["CAR"]][["ZNH"]][["5CD"]] = 21
	dfo.quota[["2010"]][["CAR"]][["ZNH"]][["5E"]] =  21
	dfo.quota[["2010"]][["CAR"]][["ZNH"]][["CST"]] = 111
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2010"]][["POP"]][["TRW"]][["3C"]]  = 300
	dfo.quota[["2010"]][["POP"]][["TRW"]][["3D"]]  = 230
	dfo.quota[["2010"]][["POP"]][["TRW"]][["5AB"]] = 2070
	dfo.quota[["2010"]][["POP"]][["TRW"]][["5CD"]] = 2118
	dfo.quota[["2010"]][["POP"]][["TRW"]][["5E"]]  = 730
	dfo.quota[["2010"]][["POP"]][["TRW"]][["CST"]] = 5448

	##-----2011--------------------------
	dfo.action[["2011"]][["CAR"]] = "CAR: Research allocation: H\\&L=6.0t"
	dfo.action[["2011"]][["RBR"]] = "RBR: TAC implementation for RBR -- 1,300,000 lbs has been set for Redbanded Rockfish coastwide (50\\% allocated to trawl, 37.5\\% allocated to rockfish outside and 12.5\\% allocated to halibut) and harvesters are now responsible for this mortality."
	dfo.action[["2011"]][["POP"]] = "POP: TAC adjustment (3y) for POP -- combined 5ABCD POP TAC reduction to 3413\\,t will be achieved over a three year period through an annual reduction of 258\\,t. The expected catch level will be 68\\% of TAC."
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2011"]][["CAR"]][["ALL"]][["3CD"]] = 529
	dfo.quota[["2011"]][["CAR"]][["ALL"]][["5AB"]] = 240
	dfo.quota[["2011"]][["CAR"]][["ALL"]][["5CD"]] = 100
	dfo.quota[["2011"]][["CAR"]][["ALL"]][["5E"]] =  31
	dfo.quota[["2011"]][["CAR"]][["ALL"]][["CST"]] = 900
	dfo.quota[["2011"]][["CAR"]][["TRW"]][["3CD"]] = 503
	dfo.quota[["2011"]][["CAR"]][["TRW"]][["5AB"]] = 197
	dfo.quota[["2011"]][["CAR"]][["TRW"]][["5CD"]] = 79
	dfo.quota[["2011"]][["CAR"]][["TRW"]][["5E"]] =  10
	dfo.quota[["2011"]][["CAR"]][["TRW"]][["CST"]] = 789
	dfo.quota[["2011"]][["CAR"]][["ZNH"]][["3CD"]] = 26
	dfo.quota[["2011"]][["CAR"]][["ZNH"]][["5AB"]] = 43
	dfo.quota[["2011"]][["CAR"]][["ZNH"]][["5CD"]] = 21
	dfo.quota[["2011"]][["CAR"]][["ZNH"]][["5E"]] =  21
	dfo.quota[["2011"]][["CAR"]][["ZNH"]][["CST"]] = 111
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2011"]][["POP"]][["TRW"]][["3C"]]  = 300
	dfo.quota[["2011"]][["POP"]][["TRW"]][["3D"]]  = 230
	dfo.quota[["2011"]][["POP"]][["TRW"]][["5AB"]] = 1942
	dfo.quota[["2011"]][["POP"]][["TRW"]][["5CD"]] = 1987
	dfo.quota[["2011"]][["POP"]][["TRW"]][["5E"]]  = 730
	dfo.quota[["2011"]][["POP"]][["TRW"]][["CST"]] = 5189

	##-----2012--------------------------
	dfo.action[["2012"]][["@@@"]] = "TWL: Froze the footprint of where groundfish bottom trawl activities can occur (all vessels under the authority of a valid Category T commercial groundfish trawl license selecting Option A as identified in the IFMP)."
	dfo.action[["2009"]][["LST|RER|BSR|REBS"]] = paste0(strSpp,": Management plan published, with goal to maintain sustainable populations of LST and REBS within each species' known range in Canadian Pacific waters.")
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2012"]][["CAR"]] = dfo.quota[["2011"]][["CAR"]]
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2012"]][["POP"]][["TRW"]][["3C"]]  = 300
	dfo.quota[["2012"]][["POP"]][["TRW"]][["3D"]]  = 230
	dfo.quota[["2012"]][["POP"]][["TRW"]][["5AB"]] = 1814
	dfo.quota[["2012"]][["POP"]][["TRW"]][["5CD"]] = 1856
	dfo.quota[["2012"]][["POP"]][["TRW"]][["5E"]]  = 730
	dfo.quota[["2012"]][["POP"]][["TRW"]][["CST"]] = 4930

	##-----2013--------------------------
	dfo.action[["2013"]][["@@@"]] = "TWL: To support groundfish research, the groundfish trawl industry agreed to the trawl TAC offsets to account for unavoidable mortality incurred during the joint DFO-Industry groundfish multi-species surveys in 2013."
	dfo.action[["2013"]][["BORa"]] = "BOR: COSEWIC had previously designated Bocaccio as Threatened in November 2002. Its status was re-examined and designated Endangered in November 2013."
	dfo.action[["2013"]][["BORb"]] = "BOR: DFO formulated a plan for stepped reductions from current Bocaccio catch levels of approximately 137 tonnes (inclusive of trawl, groundfish hook and line, salmon troll, and recreational sectors) to a target level of 75 tonnes over 3 years (2013/14 to 2015/16). This plan accounted for First Nations' priority access for food, social, and ceremonial purposes. DFO worked with fishing interests to develop measures that would reduce Bocaccio catch and enable stock rebuilding over the long term."
	dfo.action[["2013"]][["BORc"]] = "BOR: Annual Trawl \\emph{Mortality Cap} (MC) for Bocaccio was initially set at 150 tonnes. The IVQ carryover/underage limit was set to 15\\% of each vessels' Bocaccio holdings (in effect until 2019/20 fishery year)."
	dfo.action[["2013"]][["BORd"]] = "BOR: All H\\&L groundfish fisheries subject to Bocaccio trip limits based on landings of directed species. For example, Halibut directed trips could land up to 200 pounds of Bocaccio when 15,000 pounds or less of Halibut was landed, 300 pounds of Bocaccio when 30,000 pounds of Halibut was landed and 400 pounds of Bocaccio when greater than 30,000 pounds of Halibut was landed. The Dogfish, Lingcod, ZN Rockfish, and Sablefish fisheries were subject to similar trip limits for Bocaccio. These trip limits remained in effect until 2015/16."
	dfo.action[["2013"]][["CAR"]] = "CAR: Research allocations: Trawl=2.1t, H\\&L=6.0t"
	dfo.action[["2013"]][["POP"]] = "POP: New species-area groups have been created for Pacific Ocean Perch for 3CD, 5AB, 5C and 5DE."
	dfo.action[["2013"]][["POPa"]] = "POP: Combine 5ABCD TACs reduction to 3413~mt is to be achieved over a three year period through an annual reduction of 258 mt. 2013/14 is the third year of this three year period. The expected catch level is to be 68\\% of TAC. TAC is subject to annual review."
	dfo.action[["2013"]][["POPb"]] = "POP: Pacific Ocean Perch within Subarea 127-1 and that portion of Subareas 127-2 found northerly and westerly of 50$^\\circ$06$'$00$''$N will be deducted from the vessel's Pacific Ocean Perch rockfish 5A/B IVQ."
	dfo.action[["2013"]][["POPc"]] = "POP: Research allocations (trawl): 5AB=22.6t"
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2013"]][["CAR"]] = dfo.quota[["2011"]][["CAR"]]
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2013"]][["POP"]][["TRW"]][["3CD"]] = 750
	dfo.quota[["2013"]][["POP"]][["TRW"]][["5AB"]] = 1664
	dfo.quota[["2013"]][["POP"]][["TRW"]][["5C"]]  = 1555
	dfo.quota[["2013"]][["POP"]][["TRW"]][["5DE"]] = 1200
	dfo.quota[["2013"]][["POP"]][["TRW"]][["CST"]] = 5169

	##-----2014--------------------------
	dfo.action[["2014"]][["CAR"]] = "CAR: Research allocation: H\\&L=6.0t"
	dfo.action[["2014"]][["POP"]] = "POP: Research allocations (trawl): 5DE=49.4t"
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2014"]][["CAR"]] = dfo.quota[["2011"]][["CAR"]]
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2014"]][["POP"]][["TRW"]][["3CD"]] = 750
	dfo.quota[["2014"]][["POP"]][["TRW"]][["5AB"]] = 1687
	dfo.quota[["2014"]][["POP"]][["TRW"]][["5C"]]  = 1544
	dfo.quota[["2014"]][["POP"]][["TRW"]][["5DE"]] = 1200
	dfo.quota[["2014"]][["POP"]][["TRW"]][["CST"]] = 5192

	##-----2015--------------------------
	dfo.action[["2015"]][["@@@"]] = "ALL: Research allocations were specified starting in 2015 to account for the mortalities associated with survey catches to be covered by TACs."
	dfo.action[["2015"]][["BORa"]] = "BOR: DFO Groundfish Management Unit refined the generalised primary objective for Bocaccio to specify that the aim was to also: \\emph{Achieve rebuilding throughout the species' range and grow out of the critical zone ($B > 0.4 B_{MSY}$) within three generations, with a 65\\% probability of success}. To support and monitor progress towards the objective, milestones were also established: \\emph{Achieve a positive stock trajectory trend in each 5-year interval, such that the biomass at the end of each 5-year period is greater than the biomass at the beginning of the same 5-year period. Between major assessments, progress towards this goal will be monitored by annually reviewing fishery-dependent and fishery-independent indices of stock trajectory}."
	dfo.action[["2015"]][["BORb"]] = "BOR: To reduce Bocaccio mortality in the groundfish H\\&L fisheries new trip limits were introduced. For example, Halibut directed trips could land 100 pounds plus 1\\% of the amount of Halibut landed in excess of 10,000 pounds to a maximum of 600 pounds of Bocaccio. The Dogfish, Lingcod, ZN Rockfish, and Sablefish directed fisheries were subject to the same trip limits. These trip limits remained in effect during the 2019/20 fishing year."
	dfo.action[["2015"]][["BORc"]] = "BOR: Bocaccio trawl MC reduced to 110~t coastwide."
	dfo.action[["2015"]][["CAR"]] = "CAR: Research allocations: Trawl=2.7t, Longline=6.0t, Total=8.7t"
	dfo.action[["2015"]][["POP"]] = "POP: Research allocations (trawl): 5AB=16.4t, 5C=0.6t, Total=17t"
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2015"]][["CAR"]][["TRW"]][["3CD"]] = 503
	dfo.quota[["2015"]][["CAR"]][["TRW"]][["5AB"]] = 197
	dfo.quota[["2015"]][["CAR"]][["TRW"]][["5CD"]] = 79
	dfo.quota[["2015"]][["CAR"]][["TRW"]][["5E"]] =  10
	dfo.quota[["2015"]][["CAR"]][["TRW"]][["CST"]] = 789
	dfo.quota[["2015"]][["CAR"]][["ZNH"]][["3CD"]] = 24
	dfo.quota[["2015"]][["CAR"]][["ZNH"]][["5AB"]] = 42
	dfo.quota[["2015"]][["CAR"]][["ZNH"]][["5CD"]] = 19
	dfo.quota[["2015"]][["CAR"]][["ZNH"]][["5E"]] =  20
	dfo.quota[["2015"]][["CAR"]][["ZNH"]][["CST"]] = 116
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2015"]][["POP"]][["TRW"]][["3CD"]] = 750
	dfo.quota[["2015"]][["POP"]][["TRW"]][["5AB"]] = 1687
	dfo.quota[["2015"]][["POP"]][["TRW"]][["5C"]]  = 1544
	dfo.quota[["2015"]][["POP"]][["TRW"]][["5DE"]] = 1200
	dfo.quota[["2015"]][["POP"]][["TRW"]][["CST"]] = 5192

	##-----2016--------------------------
	dfo.action[["2016"]][["BOR"]] = "BOR: Bocaccio trawl MC reduced to 80~t coastwide. Bocaccio remains a quota species in the trawl fishery, but not in the hook and line fisheries."
	dfo.action[["2016"]][["CAR"]] = "CAR: Research allocations: Trawl=3.3t, Longline=6.0t, Total=9.3t"
	dfo.action[["2016"]][["POP"]] = "POP: Research allocations (trawl): 3CD=15.3t, 5DE=41.8t, Total=57.1t"
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2016"]][["CAR"]] = dfo.quota[["2015"]][["CAR"]]
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2016"]][["POP"]][["TRW"]][["3CD"]] = 750
	dfo.quota[["2016"]][["POP"]][["TRW"]][["5AB"]] = 1687
	dfo.quota[["2016"]][["POP"]][["TRW"]][["5C"]]  = 1544
	dfo.quota[["2016"]][["POP"]][["TRW"]][["5DE"]] = 1200
	dfo.quota[["2016"]][["POP"]][["TRW"]][["CST"]] = 5192

	##-----2017--------------------------
	dfo.action[["2017"]][["CAR"]] = "CAR: Research allocations: Trawl=2.3t, Longline=6.0t, Total=8.3t"
	dfo.action[["2017"]][["POP"]] = "POP: Research allocations (trawl): 5AB=17.1t, 5C=0.8t, Total=17.9t"
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2017"]][["CAR"]][["TRW"]][["3CD"]] = 615
	dfo.quota[["2017"]][["CAR"]][["TRW"]][["5AB"]] = 241
	dfo.quota[["2017"]][["CAR"]][["TRW"]][["5CD"]] = 97
	dfo.quota[["2017"]][["CAR"]][["TRW"]][["5E"]] =  12
	dfo.quota[["2017"]][["CAR"]][["TRW"]][["CST"]] = 965
	dfo.quota[["2017"]][["CAR"]][["ZNH"]][["3CD"]] = 31
	dfo.quota[["2017"]][["CAR"]][["ZNH"]][["5AB"]] = 53
	dfo.quota[["2017"]][["CAR"]][["ZNH"]][["5CD"]] = 25
	dfo.quota[["2017"]][["CAR"]][["ZNH"]][["5E"]] =  26
	dfo.quota[["2017"]][["CAR"]][["ZNH"]][["CST"]] = 135
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2017"]][["POP"]][["TRW"]][["3CD"]] = 750
	dfo.quota[["2017"]][["POP"]][["TRW"]][["5AB"]] = 1687
	dfo.quota[["2017"]][["POP"]][["TRW"]][["5C"]]  = 1544
	dfo.quota[["2017"]][["POP"]][["TRW"]][["5DE"]] = 1200
	dfo.quota[["2017"]][["POP"]][["TRW"]][["CST"]] = 5192

	##-----2018--------------------------
	dfo.action[["2018"]][["CAR"]] = "CAR: Research allocations: Trawl=4.1t, Longline=6.4t, Total=10.5t"
	dfo.action[["2018"]][["POP"]] = "POP: Research allocations (trawl): 3CD=32t, 5DE=41.8t, Total=73.8t"
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2018"]][["CAR"]] = dfo.quota[["2017"]][["CAR"]]
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2018"]][["POP"]][["TRW"]][["3CD"]] = 750
	dfo.quota[["2018"]][["POP"]][["TRW"]][["5AB"]] = 1687
	dfo.quota[["2018"]][["POP"]][["TRW"]][["5C"]]  = 1544
	dfo.quota[["2018"]][["POP"]][["TRW"]][["5DE"]] = 1200
	dfo.quota[["2018"]][["POP"]][["TRW"]][["CST"]] = 5192

	##-----2019--------------------------
	dfo.action[["2019"]][["CAR"]] = "CAR: Research allocations: Trawl=2.0t, Longline=1.2t, Total=3.2t"
	dfo.action[["2019"]][["POP"]] = "POP: Research allocations (trawl): 5AB=20.8t, 5C=1.0t, Total=21.8t"
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2019"]][["CAR"]] = dfo.quota[["2017"]][["CAR"]]
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2019"]][["POP"]][["TRW"]][["3CD"]] = 750
	dfo.quota[["2019"]][["POP"]][["TRW"]][["5AB"]] = 1687
	dfo.quota[["2019"]][["POP"]][["TRW"]][["5C"]]  = 1544
	dfo.quota[["2019"]][["POP"]][["TRW"]][["5DE"]] = 1200
	dfo.quota[["2019"]][["POP"]][["TRW"]][["CST"]] = 5192

	##-----2020--------------------------
	dfo.action[["2020"]][["CAR"]] = "CAR: Research allocations: Trawl=6.2t, Longline=6.5t, Total=12.7t"
	dfo.action[["2020"]][["POP"]] = "POP: Research allocations (trawl): 3CD=12.8t, 5E=87.1t, Total=99.9t"
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2020"]][["CAR"]] = dfo.quota[["2017"]][["CAR"]]
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2020"]][["POP"]][["TRW"]][["3CD"]] = 750
	dfo.quota[["2020"]][["POP"]][["TRW"]][["5AB"]] = 1687
	dfo.quota[["2020"]][["POP"]][["TRW"]][["5C"]]  = 1555
	dfo.quota[["2020"]][["POP"]][["TRW"]][["5DE"]] = 1200
	dfo.quota[["2020"]][["POP"]][["TRW"]][["CST"]] = 5192

	##-----2021--------------------------
	dfo.action[["2021"]][["CAR"]] = "CAR: Research allocations: Trawl=1.8t, Longline=6.5t, Total=8.3t"
	dfo.action[["2021"]][["POP"]] = "POP: Research allocations (trawl): 5AB=19.4t, 5CD=1.5t, Total=20.8t"
	##~~~~~Canary Rockfish~~~~~~~~~~~~~~~
	dfo.quota[["2021"]][["CAR"]] = dfo.quota[["2017"]][["CAR"]]
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2021"]][["POP"]][["TRW"]][["3CD"]] = 750
	dfo.quota[["2021"]][["POP"]][["TRW"]][["5AB"]] = 1687
	dfo.quota[["2021"]][["POP"]][["TRW"]][["5C"]]  = 1555
	dfo.quota[["2021"]][["POP"]][["TRW"]][["5DE"]] = 1200
	dfo.quota[["2021"]][["POP"]][["TRW"]][["CST"]] = 5192

	##-----2022--------------------------
	dfo.action[["2022"]][["POP"]] = "POP: Research allocations (trawl): 3CD=9.8t, 5E=106.5t, Total=116.3t"
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2022"]][["POP"]][["TRW"]][["3CD"]] = 750
	dfo.quota[["2022"]][["POP"]][["TRW"]][["5AB"]] = 1687
	dfo.quota[["2022"]][["POP"]][["TRW"]][["5C"]]  = 1555
	dfo.quota[["2022"]][["POP"]][["TRW"]][["5DE"]] = 1200
	dfo.quota[["2022"]][["POP"]][["TRW"]][["CST"]] = 5192

	##-----2023--------------------------
	dfo.action[["2023"]][["POP"]] = "POP: Research allocations (trawl): 5AB=21.8t, 5CD=1.5t, Total=23.3t"
	##~~~~~Pacific Ocean Perch~~~~~~~~~~~
	dfo.quota[["2023"]][["POP"]][["TRW"]][["3CD"]] = 750
	dfo.quota[["2023"]][["POP"]][["TRW"]][["5AB"]] = 1687
	dfo.quota[["2023"]][["POP"]][["TRW"]][["5C"]]  = 1555
	dfo.quota[["2023"]][["POP"]][["TRW"]][["5DE"]] = 1200
	dfo.quota[["2023"]][["POP"]][["TRW"]][["CST"]] = 5192

	ttput(dfo.action); ttput(dfo.quota)

	## Management Quotas ## assume list structure [[year]][[spp]][[gear]][[area]]
	qout = lapply(dfo.quota,function(x,spp){
		if (any(grepl(paste0("@@@|",spp),names(x)))) {
			#print(names(x))
			poo = x[grep(paste0("@@@|",spp),names(x))]
			goo = lapply(poo,function(x){lapply(x,function(y){sapply(y,sum,na.rm=TRUE)})})
			QOO = as.numeric()
			for (g in c("ALL","TRW","ZNH")){
				qoo = as.numeric()
				for (i in 1:length(goo)){
					igoo = goo[[i]]
#browser();return()
					if (!any(g%in%names(igoo))) next
					qoo = c(qoo, igoo[[g]])
#browser();return()
				}
				if (length(qoo)>0) {
					#names(qoo) = paste0(names(qoo),".",substring(g,1,1))
					names(qoo) = paste0(g,".",names(qoo))
					QOO = c(QOO, qoo)
				}
#if(g=="TRW") {browser();return()}
			}
			return(QOO)
		}
	}, spp=strSpp)
#browser();return()
	qout  = qout[!sapply(qout,is.null)]
	years = .su(names(qout))
	areas = .su(unlist(lapply(qout,names)))

	quotas = array(NA, dim=c(length(years),length(areas)), dimnames=list(year=years, area=areas))
	for (i in 1:length(qout)) {
		ii = names(qout)[i]
		qval = qout[[ii]]
		quotas[ii,names(qval)] = qval
	}
	write.csv(quotas, file=paste0("dfo.mgmt.quotas.", strSpp, ".csv"))

#browser();return()


	## Management Actions
	aout = lapply(dfo.action,function(x,spp){
		if (any(grepl(paste0("@@@|",spp),names(x)))) {
			#print(names(x))
			poo = as.vector(unlist(x[grep(paste0("@@@|",spp),names(x))]))
			return(poo)
		}
	}, spp=strSpp)

	aout = aout[!sapply(aout,is.null)]
	rout = lapply(names(aout),function(i){
		df = data.frame(
			year=rep(as.numeric(i),length(i)),
			comment = as.vector(aout[[i]]))
	})
	sout = do.call("rbind", lapply(rout, data.frame, stringsAsFactors=FALSE))
	if (addletters){
		sout = data.frame(code=c(letters,LETTERS)[1:nrow(sout)],sout)
		code.year = sapply(split(sout$code,sout$year),paste0,collapse=",")
		write.csv(code.year,"code.year.csv")
	}
	## Get rid of Latex controls
	comm = sout$comment
	comm = gsub("\\$\\^\\\\circ\\$", eval(parse(text=deparse("\u{00B0}"))), comm)
	comm = gsub("\\$\\'\\$", eval(parse(text=deparse("\u{2032}"))), comm)
	comm = gsub("\\$\\'\\'\\$", eval(parse(text=deparse("\u{2032}\u{2032}"))), comm)
	comm = gsub("\\$\\'\\'\\$", eval(parse(text=deparse("\u{2032}\u{2032}"))), comm)
	comm = gsub("\\\\%", eval(parse(text=deparse("\u{0025}"))), comm)
	comm = gsub("\\\\&", eval(parse(text=deparse("\u{0026}"))), comm)
	comm = gsub("\\~", " ", comm)
	comm = gsub("\\\\emph\\{","", gsub("}","", comm))
#browser();retrun()
	sout$comment = comm
	write.csv(sout, file=paste0("dfo.mgmt.actions.", strSpp, ".csv"), row.names=FALSE)


	return(sout)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~createMA


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


## makeAgeErr---------------------------2020-05-24
## Make an ageing error matrix for Awatea.
##  (first used for YMR in 2011)
## type = "simple" : symmetric matrix
## type = "dnorm"  : normal distribution from quantiles 0.01 to 0.99
## type = "cverr"  : use a vector of CV error-at-age
## type = "observe": frequency of ages observed per age class
## ---------------------------------------------RH
makeAgeErr = function(type="simple", strSpp, sql=FALSE,
   Amax=45, ondiag=0.8, offdiag=0.1, corner=0.9, Ndiff=5,
   CV, Noff=1, less=0, more=0, png=FALSE, ptype="bubble", lang=c("e","f"))
{
	make.errtab = function(errmat, Ndiff) {
		Amax = dim(errmat)[1]
		errtab = array(0,dim=c(Amax,1+Ndiff*2),dimnames=list(obs=1:Amax, diff=-Ndiff:Ndiff))
		for (i in 1:Amax) {
			ii = as.character(i)
#browser();return()
			avec = age.err[[ii]]
			names(avec)=as.numeric(names(avec))-i
			adiff = as.numeric(names(avec))
			avec[as.character(-Ndiff)] = sum(avec[adiff<=-Ndiff])
			avec[as.character(Ndiff)] = sum(avec[adiff>=Ndiff])
			Avec = avec[intersect(dimnames(errtab)[[2]],names(avec))]
			errtab[ii,names(Avec)] = Avec
		}
	}
	if (missing(strSpp))
			showError("Supply a species Hart code for in|output files (and GFB query if relevant)")

	if (type=="simple") {
		errmat = diag(ondiag,Amax,Amax)
		for (i in 1:(Amax-1))
			errmat[i,i+1] = offdiag
		for (j in 1:(Amax-1))
			errmat[j+1,j] = offdiag
		errmat[1,1] = errmat[Amax,Amax] = corner
		snam = paste0("aerr.mat.simp.",strSpp,".dat")
		write.table(errmat, file=snam, sep="\t", row.names=FALSE, col.names=FALSE)
		packList(c("errmat","Amax", "ondiag", "offdiag", "corner"), target="obj.makeAgeErr", tenv=.PBStoolEnv)
		return(errmat)

	} else if(type %in% c("cverr","dnorm")) {

		if (type=="cverr" && (missing(CV) || length(CV)!=Amax))
			stop("You must supply a vector of CVs by age up to Amax")
		errmat = diag(1:Amax,Amax,Amax)
		dimnames(errmat) = list(incorrect=1:Amax, true=1:Amax)
		if (type=="cverr") {
			SD = (1:Amax) * CV
			packList(c("CV", "SD"), target="obj.makeAgeErr", tenv=.PBStoolEnv)
		}
		errmat[Amax,Amax] = 0 ## initialize for accumulation

		collect=list()
		collect[["zscores"]] = list()
		for (a in 1:Amax) {
			aa = a + seq(-Noff,Noff,1)
			if (type=="cverr") {
				if (SD[a]==0) next
				#p = dnorm(a + seq(-Noff,Noff,1) * SD[a], mean=a, sd=SD[a])
				#p = dnorm(a + seq(-Noff,Noff,1) , mean=a, sd=SD[a] * 2) ## increase SD arbitrarily
				zscores = dnorm(aa , mean=a, sd=SD[a])
			} else {
				zscores = dnorm(seq(qnorm(0.01), qnorm(0.99), len=length(aa)))
			}
			collect[["zscores"]][[a]] = zscores
			pp = zscores/sum(zscores)
			zz = aa>0 & aa<Amax
			errmat[a,aa[zz]] = pp[zz]
			zzz = aa>=Amax
#browser();return()
			if (any(zzz)) errmat[a,Amax] = errmat[a,Amax] + sum(pp[zzz])
#if (a==49) {browser();return()}  ## NEED TO FIX THIS
		}
#browser();return()
		#for (i in 1:Noff) {
		#	diag(errmat[1:(Amax-i), (1+i):Amax]) = SD[1:(Amax-i)]  ## e.g. diag(errmat[1:49,2:50])=out[1:49,"SD"]
		#	diag(errmat[(1+i):Amax, 1:(Amax-i)]) = SD[(1+i):Amax]  ## e.g. diag(errmat[2:50,1:49])=out[2:50,"SD"]
		#}
		errmat = t(apply(errmat,1,function(x){x/sum(x)}))
		if (type=="cverr")
			spread = paste0("(",paste0(CV[c(5,Amax)],collapse="-"),")")
		else
			spread = "(0.01-0.99)" ## need to modify if this becomes dynamic

		onam = paste0("errmat.",ptype,".type=",type,spread,".noff=",Noff,".png")
		snam = paste0("aerr(",strSpp,").mat.type=",type,spread,".noff=",Noff,".dat")
		fnam = gsub("dat$","csv", snam)

		if (strSpp=="435" && fnam!="aerr(435).mat.type=dnorm(0.01-0.99).noff=1.csv") {
			tight = read.csv("aerr(435).mat.type=dnorm(0.01-0.99).noff=1.csv")
			tight4 = tight[1:4,]
			tight4[is.na(tight4)] = 0
			errmat[1:4,] = as.matrix(tight4[,-1]) ## remove 'age' column and convert to matrix for compatability
		}
		fout  = fout.e = onam
		if (png) createFdir(lang)

		for (l in lang) {
			fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
#browser();return()

			changeLangOpts(L=l)
			if (png) {
				clearFiles(fout)
				png(fout, units="in", res=400, width=8, height=8)
			}
			expandGraph(mfrow=c(1,1), mar=c(3,3,1,1), oma=c(0,0,0,0))
			if (ptype=="bubble") {
				#plotBubbles(errmat, dnam=TRUE, prettyAxis=TRUE, frange=0.01, size=0.1)
				errbat = errmat
				rownames(errbat) = -(1:Amax)
				plotBubbles(errbat, dnam=TRUE, xaxt="n", yaxt="n", frange=0.01, size=0.1, hide0=TRUE)
				axis(1, at=seq(5,Amax,5), cex=1.2)
				axis(2, at=seq(-5,-Amax,-5), labels=seq(5,Amax,5), cex=1.2, las=1)
				mtext(switch(l, 'e'="True Age", 'f'=eval(parse(text=deparse("le vrai \u{00E2}ge")))), side=1, line=1.75, cex=1.5)
				mtext(switch(l, 'e'="Misclassified Age", 'f'=eval(parse(text=deparse("l'\u{00E2}ge mal class\u{00E9}")))), side=2, line=1.75, cex=1.5)
#browser();return()
			}
			if (ptype=="bars") {
				plot(0,0,type="n", xlim=c(1,Amax), ylim=c(1,Amax), xlab="", ylab="", xaxt="n", yaxt="n")
				tckpos = seq(1, Amax, 5); tcklab = Amax-(tckpos)+1
				axis(1, at=seq(5,Amax,5), labels=TRUE, cex.axis=1.2)
				axis(2, at=tckpos, labels=tcklab, cex.axis=1.2)
				for (i in 1:Amax){
					ii = Amax-i+1
					y = errmat[i,]
					y = y[y>0]
					x = as.numeric(names(y))
	#if (i==15) {browser();return()}
					bcol = lucent(ifelse(i%%2, "blue", "green3"),0.5)
					fcol = lucent(ifelse(i%%2, "cyan", "green"),0.5)
					drawBars(x, (y+ii), lwd=2, col=bcol, fill=fcol,width=1, base=ii)
				}
			}
			if (png) dev.off()
		} ; eop()
#return()
		#errtab = make.errtab(errmat, Ndiff=Noff)

		clearFiles(c(snam,fnam))
		write.table(errmat, file=snam, sep="\t", row.names=FALSE, col.names=FALSE)
		cat(paste(c("age",dimnames(errmat)[[2]]),sep="",collapse=","),"\n",file=fnam)
		mess = paste(row.names(errmat),
			apply(errmat,1,function(x){
				z=x>0;xstr=x;xstr[!z]="";return(paste(xstr,sep="",collapse=","))}),sep=",")
#browser();return()
		cat(paste(mess,sep="",collapse="\n"),"\n",file=fnam,append=TRUE)

		packList("errmat", target="obj.makeAgeErr", tenv=.PBStoolEnv)
		return(errmat)

	} else {

		if (sql){
			getData("gfb_age_precision.sql", dbName="GFBioSQL", strSpp=strSpp, path=.getSpath())
			mess = paste0("aerr", strSpp, " = edat = PBSdat; ", "save(\"aerr", strSpp, "\", file=\"aerr", strSpp, ".rda\")")
			eval(parse(text=mess))
		} else {
			mess = paste0("getFile(aerr", strSpp, ", reload=TRUE); edat = aerr", strSpp)
			eval(parse(text=mess))
		}
		if (strSpp %in% ("435")) { ## get rid of obvious error in SPID 10536208
			edat = edat[edat$amax <1000 & !is.na(edat$amax),]

			## ========================================
			## Temporary fix for young-age BOR (191030)
			z1 = is.element(edat$spp,"435") & is.element(edat$year,2017) & is.element(edat$age,1)
			z2 = is.element(edat$spp,"435") & is.element(edat$year,2019) & is.element(edat$age,2)
			edat$amin[z1] = 2; edat$amax[z1] = pmax(edat$amax[z1],2)
			edat$amin[z2] = 4; edat$amax[z2] = pmax(edat$amax[z2],4)
			## ========================================
		}

		agelst = apply(edat[,c("amin","amax")],1,function(x){x[1]:x[2]})
		names(agelst) = edat$age

		bigage = max(edat$age)
		ages = as.list(1:bigage); names(ages) = 1:bigage
		for (i in 1:bigage) {
			ii = as.character(i)
			iii = is.element(names(agelst),ii)
			if (any(iii)) {
#if(i==15){browser();return()}
			ages[[ii]] = as.vector(unlist(agelst[iii])) }
		}
		f = function(x, n){ x / 2^(1:n)} ## halving function (https://stackoverflow.com/questions/53872440/divide-a-number-and-each-successive-answer-x-times-in-r)

#par(mfcol=c(10,5),mar=c(0,0,0,0),oma=c(1,1,1,1))

		expand.alen=function(x, alen, less=2, more=10)
		{
			amore = c(max(1,(min(x)-less)):(max(x)+more))
			pmore = rep(1,length(amore))
			names(pmore) = amore
			pmore[names(alen)] = alen
			return(pmore)
		}

		age.err = sapply(1:length(ages),function(a){ 
			atrue = as.numeric(names(ages)[a])
			x = ages[[a]]
			if (length(x)==1) {
				alen=1; names(alen) = x
				if (less>0 || more>0)
					alen = expand.alen(x, alen)
				alen = alen/sum(alen) 
#browser();return()
			} else {
				alen=sapply(split(x,x),length)
				if (less>0 || more>0)
					alen = expand.alen(x, alen)
#browser();return()
#if (atrue==15){browser();return()}
				alen=alen/sum(alen) 
				z = as.numeric(names(alen)) >= Amax
				## Consolidate plus groups using probs >=Amax into Amax
				if (any(z)) {
					aplus = sum(alen[z]); names(aplus) = Amax
					alen = c(alen[!z],aplus)
				}
			}
#plot(as.numeric(names(alen)),alen,xlim=c(1,Amax),type="h",lwd=2,xaxt="n",yaxt="n",xlab="",ylab=""); abline(v=c(2,4),col="red",lty=5)
#if (atrue==50) {browser();return()}
			return(alen)
		} ,simplify=FALSE)
		names(age.err) = names(ages)

		errmat = array(0,dim=c(Amax,Amax),dimnames=list(incorrect=1:Amax,true=1:Amax))
		for (i in 1:Amax) {
			ii = as.character(i)
			avec = age.err[[ii]]
			#errmat[names(avec),ii] = avec  ## columns added to 1 but rows should add to 1
			errmat[ii,names(avec)] = avec
		}

		errtab = array(0,dim=c(Amax,1+Ndiff*2),dimnames=list(obs=1:Amax, diff=-Ndiff:Ndiff))
		for (i in 1:Amax) {
			ii = as.character(i)
#if (ii=="139"){browser();return()}
			avec = age.err[[ii]]
			if (is.null(avec)) next
			names(avec)=as.numeric(names(avec))-i
			adiff = as.numeric(names(avec))
			avec[as.character(-Ndiff)] = sum(avec[adiff<=-Ndiff])
			avec[as.character(Ndiff)] = sum(avec[adiff>=Ndiff])
			Avec = avec[intersect(dimnames(errtab)[[2]],names(avec))]
			errtab[ii,names(Avec)] = Avec
		}


		noto = sapply(split(edat$SPID,edat$age),length)
		Noto = rep(0,Amax); names(Noto) = 1:Amax
		nn = intersect(names(Noto),names(noto))
		Noto[nn] = noto[nn]
		Noto[as.character(Amax)] = sum(noto[as.numeric(names(noto))>=Amax])
#browser();return()
		attr(errmat,"Noto") = Noto
		attr(errtab,"Noto") = Noto

		save(list=c("errmat","errtab"), file=paste0("aerr.mat.",strSpp,".rda"))

		fnam = paste0("aerr.mat.obs.",strSpp,".csv")
		cat(paste(c("age",dimnames(errmat)[[2]]),sep="",collapse=","),"\n",file=fnam)
		mess = paste(row.names(errmat),
			apply(errmat,1,function(x){
				z=x>0;xstr=x;xstr[!z]="";return(paste(xstr,sep="",collapse=","))}),sep=",")
		cat(paste(mess,sep="",collapse="\n"),"\n",file=fnam,append=TRUE)

		dnam = gsub("csv","dat",fnam)
		write.table(errmat, file=dnam, sep="\t", row.names=FALSE, col.names=FALSE)

		tnam = gsub("obs","tab",fnam)
		cat(paste(c("age","noto",dimnames(errtab)[[2]]),sep="",collapse=","),"\n",file=tnam)
		mess = paste(row.names(errtab),Noto,
			apply(errtab,1,function(x){
				z=x>0;xstr=x;xstr[!z]="";return(paste(xstr,sep="",collapse=","))}),sep=",")
		cat(paste(mess,sep="",collapse="\n"), "\n", file=tnam, append=TRUE)

		packList(c("agelst","bigage","ages","age.err","errmat","errtab","noto","Noto"), target="obj.makeAgeErr", tenv=.PBStoolEnv)
		return(list(errmat=errmat, errtab=errtab))
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~makeAgeErr


## plotAgeErr---------------------------2021-12-02
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
	opar = par(no.readonly=TRUE); on.exit(par(opar))
	strSpp = as.character(.su(dat$spp))
	sppNam = toUpper(tolower(.su(dat$spn)))
	if (grepl("/", sppNam)) {
		for (i in 1:26)
			sppNam = sub(paste0("/",letters[i]), paste0("/",LETTERS[i]), sppNam)
	}
#browser();return()

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
#browser();return()
		for (i in 1:nrow(read1)) {
			irec = read1[i,,drop=FALSE]
			for (j in 1:nrow(read2)) {
				jrec = read2[j,,drop=FALSE]
				if (irec$EMID==jrec$EMID) next
				RAGOUT = rbind(RAGOUT, as.vector(cbind(irec[,ofld],irec[,afld],jrec[,afld]), mode="numeric"), make.row.names=FALSE)
			}
		}
		ttput(RAGOUT)
		return(as.matrix(rout))
	}, ofld=oflds, afld=aflds, simplify=FALSE)
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
#browser();return()

	if (is.null(xlim))
		xlim = range(dat[,c("r1_amin","r1_amax")])
	if (is.null(ylim))
		ylim = range(dat[,c("r2_amin","r2_amax")])
	
	fout.e = paste0("AgeErr",strSpp)
	for (l in lang) {
		changeLangOpts(L=l)
		fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		if (png) {
			clearFiles(fout,".png")
			png(paste0(fout,".png"), units="in", res=pngres, width=PIN[1], height=PIN[2])
		}
		par(mfrow=c(1,1), mar=c(3.5,3.5,0.5,0.75), oma=c(0,0,0,0), mgp=c(2,0.5,0))
		plot(dat$r1_age, dat$r2_age, xlim=xlim, ylim=ylim, type="n", xlab="", ylab="", cex.axis=1.2, las=1)
		axis(1, at=seq(0,xlim[2],ifelse(xlim[2]<60,1,5)), labels=FALSE, tcl=-0.2)
		axis(2, at=seq(0,ylim[2],ifelse(ylim[2]<60,1,5)), labels=FALSE, tcl=-0.2)
		abline(0,1,col=lucent("green4",0.5),lwd=2)
		## Drawing segments takes a long time, especially when sending to PNG device:
		segments(x0=dat$r1_amin, y0=dat$r2_age, x1=dat$r1_amax, y1=dat$r2_age, col=lucent("grey50",0.5),lwd=2)
		segments(x0=dat$r1_age, y0=dat$r2_amin, x1=dat$r1_age, y1=dat$r2_amax, col=lucent("grey50",0.5),lwd=2)
		points(dat$r1_age, dat$r2_age, pch=21, cex=0.8, col=lucent("black",0.5), bg=lucent("cyan",0.5))
		mtext(linguaFranca("Age (y) by Primary Reader",l), side=1, line=2, cex=1.5)
		mtext(linguaFranca("Age (y) by Secondary Reader",l), side=2, line=2, cex=1.5)
		addLabel(0.05, 0.95, linguaFranca(sppNam,l), cex=1.5, col="blue", adj=c(0,1))
		if (png) dev.off()
	}; eop()
	junk = gc(verbose=FALSE)
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotAgeErr


## plotBTMW-----------------------------2023-04-26
##  Plot for bottom (BT) vs. midwater (MW) trawl catch
##  WHEN MC.GEAR IN ('BOTTOM TRAWL','UNKNOWN TRAWL') THEN 1
##  WHEN MC.GEAR IN ('TRAP') THEN 2
##  WHEN MC.GEAR IN ('MIDWATER TRAWL') THEN 3
##  WHEN MC.GEAR IN ('HOOK AND LINE') THEN 4
##  WHEN MC.GEAR IN ('LONGLINE') THEN 5
##  WHEN MC.GEAR IN ('LONGLINE OR HOOK AND LINE','TRAP OR LONGLINE OR HOOK AND LINE') THEN 8
## ---------------------------------------------RH
plotBTMW = function(dat, strSpp="417", years=1996:2018, major=list('CST'=3:9), 
   glist=list(gear=c(1:4)), removeSM=FALSE, ymax,
   png=FALSE, pngres=400, PIN=c(8,6) , lang=c("e","f"))
{
	flds = colnames(dat)
#	if (!is.null(ttcall(dat.btmw)))  ## remove this rrequirement for now (just shortens processing but lessens dynamic refresh)
#		dat = ttcall(dat.btmw)
#	else {
		if (removeSM) ## seamounts
			dat    = zapSeamounts(dat)
		else if (!is.element("seamount",flds))
			dat = zapSeamounts(dat, only.mark=TRUE)
		if (!is.element("year",flds))
			dat$year = as.numeric(substring(dat$date,1,4))
		dat = dat[is.element(dat$year, years),]

		#if (strSpp %in% c("396","POP","440","YMR"))
		#	dat = dat[is.element(dat$major_adj, unique(unlist(major))),]  ## use major_adj when summamrising YMR catdat for scaling
		#else
		dat = dat[is.element(dat$major, unique(unlist(major))),]
		if (!is.element("catKg",flds))
			dat$catKg = dat$landed + dat$discard
		dat = dat[dat$catKg > 0,]
		dat = calcStockArea(strSpp, dat)  ## uses expand5C but only if it has not been applied previously
		## Fiddle for POP 2023:
		if (strSpp %in% c("396","POP"))
			dat$stock[is.element(dat$stock,c("5AB","5C"))] = "5ABC"

		dat.btmw=dat; ttput(dat.btmw)
#	}
	gcodes = list(
		gear   = c('0'="Unknown", '1'="Bottom_Trawl", '2'="Trap", '3'="Midwater_Trawl", '4'="Hook_&_Line", '5'="Longline", '8'="Mixed_H&L"),
		fid    = c('0'="Unknown", '1'="Trawl", '2'="Halibut", '3'="Sablefish", '4'="Dogfish/Lingcod", '5'="H&L_Rockfish", '8'="GF_Longline", '9'="Foreign"),
		sector = c('0'="UNKNOWN", '1'="FOREIGN", '2'="GROUNDFISH LONGLINE", '3'="GROUNDFISH TRAWL", '4'="HALIBUT", '5'="HALIBUT AND SABLEFISH", '6'="K/L", '7'="K/ZN", '8'="LINGCOD", '9'="ROCKFISH INSIDE",'10'="ROCKFISH OUTSIDE", '11'="SABLEFISH", '12'="SCHEDULE II", '13'="SPINY DOGFISH", '14'="ZN"),
		fishery = c('0'="Unknown", '1'="BSR_trawl", '2'="BSR_other", '3'="RER_trawl", '4'="RER_other", '5'="HYB_trawl", '6'="HYB_other"), ## REBS only
		stock = switch(strSpp,
			'440'=c('UNK'="UNKNOWN", '3C'="3C", '3D5AB'="3D5AB", '5CD'="5CD", '5E'="5E"), ## YMR
			'437'=c('UNK'="UNKNOWN", '3CD'="3CD", '5AB'="5AB", '5CD'="5CD", '5E'="5E"), ## CAR
			'396'=c('UNK'="UNKNOWN", '5ABC'="5ABC", '3CD'="3CD", '5DE'="5DE") ## POP
		)
	)
#browser();return()

	gtabs  = list()
	if (png) createFdir(lang)

	for (g in 1:length(glist)) {
		gval  = glist[[g]]
		gnam  = names(glist)[g]
		if (!is.element(gnam,flds)) next
		gcode = gleg = gcodes[[gnam]]
		if (gnam=="sector")
			gleg = gsub("/l","/L",gsub("Zn|zn","ZN",gsub("Ii","II",toUpper(tolower(gcode)))))
		ncode = length(gcode)
		if (gnam %in% c("sector","fishery")) {  ## convert values in gdat to codes
			zcode = as.numeric(names(gcode))
			names(zcode) = as.vector(gcode)
			zdat = dat
			zdat[,gnam] = zcode[zdat[,gnam]]
		} else {
			zdat = dat
		}
		gdat  = zdat[is.element(zdat[,gnam], gval),]

		if (gnam=="stock")
			gtab  = crossTab(gdat, c("year",gnam), "catKg")
		else
			gtab  = crossTab(gdat, c("year",gnam,"stock"), "catKg")
		narea = dim(gtab)[3]
		if(is.na(narea)) narea = 1

		#glty  = rep(c(6,1,5,2,3,4,3),ncode)[1:ncode]
		glty  = rep(1,ncode)
		#glty = major
		#for (i in 1:narea){
		#	ilty = rep(i,ncode); names(ilty) = names(gcode)
		#	glty[[i]] = ilty
		#}
		gpch  = rep(c(4,21,22,24,25,8,6,5,3,2,1,7,9:16),ncode)[1:ncode]
		gcol  = rep(c("black","blue","green4","red","darkgoldenrod1","purple","coral","navy",.colBlind[-1],.colGnuplot[1:5]),ncode)[1:ncode]
		gbg   = rep(c(NA,"cyan","green","mistyrose1","yellow2",rep(NA,15)),ncode)[1:ncode]
		names(glty) = names(gcol) = names(gpch) = names(gbg) = names(gcode)
#browser();return()

		xlim   = range(years)
		xval   = as.numeric(rownames(gtab))
		#ylim   = c(0, ifelse(missing(ymax)||is.null(ymax), max(gtab)*1.1, ymax))
		onam   = paste0("Catch.", strSpp, ".m(", paste0(major,collapse=""), ")", gnam)
		onam   = paste0("Catch.", strSpp, ".", paste0(names(major),collapse="."), ".", gnam)
		fout.e = onam

		for (l in lang) {
			fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
			changeLangOpts(L=l)
			if (png) png (filename=paste0(fout,".png"), width=PIN[1], height=PIN[2], units="in", res=pngres)
			#expandGraph(mfrow=c(1,narea), mar=c(3,3,1.2,0.5), mgp=c(1.6,0.5,0))
			expandGraph(mfrow=c(1,narea), mar=c(1.5,2,1,1), oma=c(1.5,2,0,0), mgp=c(1.6,0.5,0))
			for (a in 1:narea) {
				aa = names(major)[a]
				atab = gtab
				if (narea>1) atab = gtab[,,aa]

				ylim = c(0, ifelse(missing(ymax)||is.null(ymax), max(atab)*1.1, ymax))
				plot(0,0, xlim=xlim, ylim=ylim, type="n", xlab="", ylab="", cex.axis=1.2)
				axis(1, at=years, labels=FALSE, tcl=-0.25)
				for (i in 1:ncol(atab)) {
					ii = colnames(atab)[i]
					lines(xval, atab[,ii], lty=glty[ii], col=gcol[ii], lwd=2 )
					points(xval, atab[,ii], pch=gpch[ii], col=gcol[ii], bg=gbg[ii],cex=1.5)
				}
				iii = intersect(as.character(gval),colnames(gtab)) ## use names instead of numeric indices
				ypos = if (strSpp %in% c("417") || (strSpp %in% "437" && gnam %in% c("sector","fid")))  0.50 else 0.99
				if (a==1)
					addLegend(0.99, ypos, lty=glty[iii], seg.len=3, lwd=2, col=gcol[iii], pch=gpch[iii], pt.bg=gbg[iii], pt.cex=1.75, legend=linguaFranca(gsub("_"," ",gleg[iii]),l), cex=1.2, xjust=1, yjust=1, bty="n")
				addLabel(0.05, 0.95, txt=aa, adj=c(0,1), cex=1.5, col=switch(a,"blue","green4","red","purple"), font=2)

				## Write the tables to CSV
				colnames(atab) = gcode[colnames(atab)]
				abad = paste0(grep(aa,names(major),invert=TRUE,value=TRUE),collapse="|")
				anam = gsub("\\.+",".",gsub(abad,"",onam))
#browser();return()
				write.csv(atab, paste0(anam,".csv"))
				gtabs[[gnam]][[aa]] = atab
			}
			mtext(linguaFranca("Year",l), outer=TRUE, side=1, line=0.5, cex=1.5)
			mtext(linguaFranca("Catch (t)",l), outer=TRUE, side=2, line=0.25, cex=1.5)
			if (png) dev.off()
		}; eop()
#browser(); return()
	}
	ttput(gtabs)
	invisible(return(gtabs))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotBTMW


## plotMW-------------------------------2019-10-17
## Plot mean weight of stocks and individual areas
## that occur in the bigger stock areas.
## ---------------------------------------------RH
plotMW = function(dat, xlim, ylim, outnam="Mean-Weight-Compare",
   outcast, png=FALSE, lang=c("e","f"))
{
	## Create a subdirectory called `french' for French-language figures
	createFdir(lang)

	if (missing(xlim)) xlim=range(dat[,1])
	if (missing(ylim)) ylim=range(dat[,-1], na.rm=TRUE)
	x = xlim[1]:xlim[2]
	#x = x[is.element(x,dat$year)]
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
		changeLangOpts(L=l)
		fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		if (png) png(paste0(fout,".png"), units="in", res=400, width=8, height=6)
		par(mfrow=c(1,1), mar=c(3.5,3.8,0.5,0.5), oma=c(0,0,0,0), mgp=c(2.25,0.5,0))
		plot(0, xlim=xlim, ylim=ylim, type="n", xlab=linguaFranca("Year",l), ylab=linguaFranca("Mean Weight (kg)",l), cex.axis=1.2, cex.lab=1.5, las=1)
		z = is.element(x,dat$year)
		for (i in sord) {
			if (!is.element(i,stocks)) next
			y    = rep(0, length(x))
			y[z] = dat[,i]
			y[y==0] = NA
#browser();return
			lines(x, y, col=scol[i], lty=slty[i], lwd=slwd[i])
			points(x, y, pch=spch[i], col=scol[i], bg="ghostwhite", cex=scex[i], lwd=slwd[i])
		}
		ii = gsub("^[s]","",stocks)
		addLegend(0.05,0.97, pch=spch[ii], col=scol[ii], lty=slty[ii], lwd=slwd[ii], pt.cex=scex[ii], pt.bg="ghostwhite", seg.len=5, legend=linguaFranca(ii,l), bty="n")
		if (png) dev.off()
	}; eop()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotMW


## plotSnail----------------------------2023-04-19
## Plot snail-trail plots for MCMC analysis.
##  AME: replacing "2010" with as.character(currYear - 1)
##  RH: added assYrs = years past with estimated Bcurr
##      from previous assessment(s), e.g., 5ABC QCS c(2011, 2017)
##  RH 210727: Moved function to PBStools from PBSawatea
##  RH 230418: Modified code to accept multi-area quantiles from 'load_extra_mcmc.r'
## -----------------------------------------AME/RH
plotSnail=function (BoverBmsy, UoverUmsy, model="SS", yrs=1935:2022,
   p=c(0.05,0.95), xLim=NULL, yLim=NULL, Lwd=1.5, ngear=1,
   Cnames, assYrs=NULL, outs=FALSE,         ## outs = outliers
   ptypes="win", pngres=400, PIN=c(8,6), outnam, lang="e")
{
	if (missing(Cnames))
		Cnames  = tcall("PBStools")$Cnames        ## names of commercial gear
	if (is.null(Cnames)) {
		if(is.list(UoverUmsy) && length(UoverUmsy)<=5)
			Cnames = names(UoverUmsy)
		else
			Cnames = "Some fishery"
	}
	ngear = length(Cnames)
	narea = 1
	areas = "BC coastwide"
	isQ   = FALSE
	if(is.list(BoverBmsy) && length(BoverBmsy)<=5) {
		narea = length(BoverBmsy)
		areas = names(BoverBmsy)
		isQ   = TRUE  ## qantiles supplied insted of raw numbers
	}
	if (model=="SS" && narea==1){
		ngear = 1  ## SS does not seem to report separate F for each fishery (unless create separate Report files in MCMC eval)
		#Cnames = paste0(Cnames, collapse="+")
		#Cnames = paste(unique(unlist(strsplit(Cnames, " "))),collapse="")  ## doesn't really matter
		Cnames = paste0(sapply(strsplit(Cnames,split="\\s+"),function(x){x[1]}),collapse="+")
	}
	if (narea==1) {
		## BU -- B = spawning biomass, U = harvest rate (or exploitation rate)
		BUlist = as.list(0:ngear); names(BUlist)=c("Spawning Biomass",Cnames[1:ngear])
		colnames(BoverBmsy) = sub("^.+_","",colnames(BoverBmsy))
		colnames(UoverUmsy) = sub("^.+_","",colnames(UoverUmsy))
		## Awatea -- previous conversation with PJS: we both agree that B2021/Bmsy should be paired with u2020/Umsy
		## SS3    -- currYear (e.g. 2023) = beginning of current year for SSB and middle of 'previous' year for mid-year quantities like F.
		##           B2023 = SSB at end of 2022 or beginning of 2023
		##           F2023 = fishing mortality for 2022 (e.g. mid-year) after catches for 2022 have been applied.
	} else {
		BUlist = as.list(c(1:narea,1:ngear)); names(BUlist)=c(paste0("SSB_",areas), paste0("HR_",Cnames))
	}
	currYear = rev(yrs)[1]
	if (model=="SS") {
		if (isQ) {
			for (a in areas)
				BUlist[[paste0("SSB_",a)]] = BoverBmsy[[a]][,as.character(yrs)]
			for (u in Cnames)
				BUlist[[paste0("HR_",u)]] = UoverUmsy[[u]][,as.character(yrs)]
		} else {
			BoverBmsy = BoverBmsy[,colnames(BoverBmsy) %in% yrs]
			UoverUmsy = UoverUmsy[,colnames(UoverUmsy) %in% yrs]
			is.good.B = apply(BoverBmsy,1,function(x){all(is.finite(unlist(x)))})
			is.good.U = apply(UoverUmsy,1,function(x){all(is.finite(unlist(x)))})
			is.good   = is.good.B & is.good.U
			is.bad    = !is.good
			BoverBmsy = BoverBmsy[is.good,]
			UoverUmsy = UoverUmsy[is.good,]
			BUlist    = list(SSB=BoverBmsy, HR=UoverUmsy)
#browser();return()
		}
	} else { ## Awatea
		BUlist[[1]] = BoverBmsy[,colnames(BoverBmsy) %in% c(yrs,yrs[length(yrs)]+1)]
		BUlist[[1]] = BUlist[[1]][,-1]  ## remove first year
		## Separate u if multiple gears (designed for Awatea, not sure how SS displays multiple exploitation by gear)
		## SS does not seem to report multiple exploitation rates
		for (g in 1:ngear) {
			if (any(grepl("_",names(UoverUmsy)))) {
				gfile = UoverUmsy[,grep(paste0("_",g),names(UoverUmsy))]
				names(gfile) = substring(names(gfile),1,4)
			} else if (is.list(UoverUmsy)) {
				gfile = UoverUmsy[[g]]
			} else {
				gfile = UoverUmsy
			}
			if ( model %in% c("AW","Awatea") || ncol(BUlist[[1]])!=ncol(BUlist[[g+1]]) )
				BUlist[[g+1]] = gfile[,1:ncol(BUlist[[1]])] ## SS has same lengths of B and u
		}
	}
	# Calculate medians to be plotted
	if (isQ) {
		BUmed = lapply(BUlist, function(x) { x["50%",] })
	} else {
		BUmed    = lapply(BUlist,function(x){apply(x,2,median,na.rm=TRUE)})  # median each year
	}

	colPal   = colorRampPalette(c("grey90", "grey30"))
	if (narea==1) {
		colDots  = list( colorRampPalette(c("slategray2", "slategray4")), colorRampPalette(c("thistle2", "thistle4")), colorRampPalette(c("rosybrown2", "rosybrown4")) )  ## (RH 210727)
		colSlime = rep(c("slategray3","thistle","rosybrown"),ngear)[1:ngear]  ## RH 200528|230418
		colStart = rep(c("green","yellowgreen","limegreen"),ngear)[1:ngear]
		colStop  = rep(c("cyan","thistle","pink"),ngear)[1:ngear]  #,"cyan"
		colLim   = rep(c("blue2","purple","red"),ngear)[1:ngear]
		colAss   = rep(c("gold","orange","yellow"),ngear)[1:ngear]
	} else {
		## Colours chosen for areas 5ABC, 3CD, and 5DE for POP 2023 assessment
		colDots  = list( colorRampPalette(c("powderblue", "blue")), colorRampPalette(c("palegreen", "green3")), colorRampPalette(c("lightpink", "red")) )  ## (RH 210727)
		colSlime = rep(c("powderblue","palegreen","lightpink"),ngear)[1:ngear]  ## RH 200528|230418
		colStart = rep(c("aliceblue","honeydew","mistyrose"),ngear)[1:ngear]
		colStop  = rep(c("cyan","greenyellow","pink"),ngear)[1:ngear]  #,"cyan"
		colLim   = rep(c("blue","green4","red"),ngear)[1:ngear]
		colAss   = rep(c("gold","orange","yellow"),ngear)[1:ngear]
	}
	nB = length(BUmed[[1]])

	if (is.null(xLim)) {
		if (isQ)
			xLim = c(0, max(sapply(BUlist[grep("SSB",names(BUlist))],function(x){max(x[,nB])}), sapply(BUmed[grep("SSB",names(BUmed))],function(x){max(x)}) ))
		else
			xLim=c(0, max(c(BUmed[[1]], rev(apply(BUlist[[1]], 2, quantile, probs=ifelse(outs,1,p[2]), na.rm=TRUE))[1], 1)))
	}
	if (is.null(yLim)) {
		if (isQ) {
			yLim = c(0, max(sapply(BUlist[grep("HR",names(BUlist))],function(x){max(x[,nB])}), sapply(BUmed[grep("HR",names(BUmed))],function(x){max(x)}) ))
		} else {
			yUps = sapply(BUlist[(1:ngear)+1], function(x,p){
				#is.good = apply(x,1,function(x){all(is.finite(x))})
				apply(x, 2, quantile, p=p, na.rm=TRUE)}, p=ifelse(outs,1,p[2]))  ## (RH 200624)
			yLim=c(0, max(c(sapply(BUmed[(1:ngear)+1], max, na.rm=TRUE), max(yUps[nrow(yUps),],na.rm=TRUE), 1)))    ## (RH 200624)
		}
#browser();return()
	}
	P = list(p=p)
	if (outs)
		P = list (o=c(0,1), p=p)

	if (missing(outnam))
		outnam = "plotSnail"
	fout.e = outnam
	for (l in lang) {  ## could switch to other languages if available in 'linguaFranca'.
		createFdir(lang, dir=".")
		changeLangOpts(L=l)
		fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		if ("eps" %in% ptypes){
			clearFiles(paste0(fout,".eps"))
			postscript(paste0(fout,".eps"), width=PIN[1], height=PIN[2], horizontal=FALSE, paper="special")
		} else if ("png" %in% ptypes){
			clearFiles(paste0(fout,".png"))
			png(paste0(fout,".png"), units="in", res=pngres, width=PIN[1], height=PIN[2])
		} else if ("wmf" %in% ptypes){
			clearFiles(paste0(fout,".wmf"))
			do.call("win.metafile", list(filename=paste0(fout,".wmf"), width=PIN[1], height=PIN[2]))
		}
		expandGraph(mfrow=c(1,1), mar= c(3.5,3.5,1,1), mgp=c(2,0.5,0))

		xlab = switch(l, 'e'=expression(paste(italic(B[t])/italic(B)[MSY])),   'f'=expression(paste(italic(B[t])/italic(B)[RMD])) )
		ylab = if (model %in% c("AW","Awatea","SS"))
			switch(l, 'e'=expression(paste(italic(u[t-1])/italic(u)[MSY])), 'f'=expression(paste(italic(u[t-1])/italic(u)[RMD])) )
		else
			switch(l, 'e'=expression(paste(italic(u[t])/italic(u)[MSY])), 'f'=expression(paste(italic(u[t])/italic(u)[RMD])) )

		plot(0,0, xlim=xLim, ylim=yLim, type="n", xlab=xlab, ylab=ylab, cex.lab=1.25, cex.axis=1.0, las=1)
		abline(h=1, col=c("grey20"), lwd=2, lty=3)
		abline(v=c(0.4,0.8), col=c("red","green4"), lwd=2, lty=2)
		for (i in ngear:1) {
			ii = ifelse(narea==1,1,i)
			iii = i + narea
			lines(BUmed[[ii]], BUmed[[iii]], col=colSlime[i], lwd=Lwd)
			points(BUmed[[ii]], BUmed[[iii]], type="p", pch=19, col=colDots[[i]](nB)) ## (RH 210727)
		}
#browser();return()
		## Use three loops to plot the trace, end-year line segments, end-year points (RH 200624)
		for (i in ngear:1) {
			ii = ifelse(narea==1,1,i)
			iii = i + narea
			xend = rev(BUmed[[ii]])[1]
			yend = rev(BUmed[[iii]])[1]
			xoff = ifelse(as.logical(i%%2),-1,1) * diff(par()$usr[1:2])*0.0015 ## shift final xpos +/- some small amount (RH 200624)
			for (j in 1:length(P)) {
				q = P[[j]]
				lty = ifelse(j==1 && outs, 3, 1)
				lwd = ifelse(j==1 && outs, 1, 2)
				## Plot horizontal (BtB0) quantile range
				if (isQ) {
					qq   = paste0(q*100,"%")
					xqlo = BUlist[[ii]][qq[1],nB]
					xqhi = BUlist[[ii]][qq[2],nB]
				} else {
					xqlo = quantile(BUlist[[ii]][,nB], q[1], na.rm=TRUE)
					xqhi = quantile(BUlist[[ii]][,nB], q[2], na.rm=TRUE)
				}
				#segments(xqlo, yend, xqhi, yend, col=lucent(colLim[i],0.5), lty=lty, lwd=2)
				segments(xqlo, yend, xqhi, yend, col=colLim[i], lty=lty, lwd=2)
				## Plot vertical (UtU0) quantile range
				if (isQ) {
					qq   = paste0(q*100,"%")
					yqlo = BUlist[[iii]][qq[1],nB]
					yqhi = BUlist[[iii]][qq[2],nB]
				} else {
					yqlo = quantile(BUlist[[i+1]][,nB], q[1], na.rm=TRUE)
					yqhi = quantile(BUlist[[i+1]][,nB], q[2], na.rm=TRUE)
				}
				#segments(xend+xoff, yqlo, xend+xoff, yqhi, col=lucent(colLim[i],0.5), lty=lty, lwd=lwd)
				segments(xend+xoff, yqlo, xend+xoff, yqhi, col=colLim[i], lty=lty, lwd=lwd)
			}
		}
		for (i in ngear:1) {
			ii = ifelse(narea==1,1,i)
			iii = i + narea
			points(BUmed[[ii]][1], BUmed[[iii]][1], pch=21, col=1, bg=colStart[i], cex=1.2)  ## similar in 1935 for all areas because we use prop B0 to allocate MSY
			xend = rev(BUmed[[ii]])[1]
			yend = rev(BUmed[[iii]])[1]
			xoff = ifelse(as.logical(i%%2),-1,1) * diff(par()$usr[1:2])*0.0015 ## shift final xpos +/- some small amount (RH 200624)
			if (!is.null(assYrs))
				points(BUmed[[ii]][as.character(assYrs)], BUmed[[iii]][as.character(assYrs)], pch=21, col=1, bg=colAss[i], cex=1.2)
			points(xend+xoff, yend, pch=21, col=colLim[i], bg=colStop[i], cex=1.5, lwd=2)
		}
		if (ngear>1)
			addLegend(0.95, 0.95, legend=linguaFranca(Cnames,l), lty=1, lwd=Lwd, col=colLim, seg.len=4, xjust=1, bty="n", cex=1)
		if (model=="SS" && !isQ && sum(is.bad)>0)
			addLabel(0.95, 0.90, txt=linguaFranca(paste0("dropped ",sum(is.bad)," samples"),l), adj=c(1,0), cex=1)
		box()
#browser();return()
		if (any(ptypes %in% c("eps","png","wmf"))) dev.off()
	}; eop()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotSnail


## processBio---------------------------2020-05-24
## Process results from 'gfb_bio.sql' query.
##  zflds = new fields to move to end of file to save PJS 
##          having to reorder his database all the time
## ---------------------------------------------RH
processBio = function(dat=PBSdat, strSpp, addsrfa=TRUE, 
   addsrfs=TRUE, addpopa=TRUE, addstock=TRUE, addsort=TRUE,
   useSM=FALSE, maxrows=5e4, zflds=c("ssrc","AC","FOSTID"))
{
	f=function(x){format(x,scientific=FALSE,big.mark=",")}
	if (!useSM)
		dat = zapSeamounts(dat)  ## remove seamount data
	atts=attributes(dat)[setdiff(names(attributes(dat)),c("names","row.names","class"))] # extra attributes to retain
	N=nrow(dat); nloop=ceiling(nrow(dat)/maxrows)
	DAT=NULL
	for (i in 1:nloop) {
		n0=(i-1)*maxrows+1; n1=min(i*maxrows,N)
		cat(paste("processing rows",f(n0),"to",f(n1),"of",f(N)),"\n"); flush.console()
		idat=dat[n0:n1,]
		idat$EID=n0:n1
		if (addsrfa)
			idat$srfa=calcSRFA(idat$major,idat$minor,idat$locality)
		if (addsrfs)
			idat$srfs=calcSRFA(idat$major,idat$minor,idat$locality,subarea=TRUE)
		if (addpopa){
			if (i==1) data("popa", package="PBSdata", envir=penv())
			datpopa=rep("",nrow(idat))
			events=idat[!is.na(idat$X) & !is.na(idat$Y),c("EID","X","Y")]
			if (nrow(events)>0) {
				events=as.EventData(events)
				locs=findPolys(events,popa,maxRows=maxrows)
				locs=zapDupes(locs,"EID")
				pdata=attributes(popa)$PolyData; #pdata$label=substring(pdata$label,5)
				pvec=pdata$label[locs$PID]; names(pvec)=locs$EID
				names(datpopa)=idat$EID
				datpopa[names(pvec)]=pvec
			}
			idat$popa=datpopa
		}
		if (addstock || "major_adj" %in% zflds) {
			if (missing(strSpp))
				stop("Must specify a species to determine stock allocation")
			#idat$stock = calcStockArea(strSpp, major=idat$major, minor=idat$minor)
			stockSpp = if (strSpp %in% c("REBS","RER","BSR","394","425")) "394" else strSpp
			idat = calcStockArea(stockSpp, dat=idat)
#browser();return()
		}
		if (addsort) {
			idat$sort = rep("Z",nrow(idat))

			## Unsorted ---------
			zunso  = is.element(idat$scat,1) & (is.element(idat$ssrc,1) | is.na(idat$ssrc))
			zunso  = zunso | (is.element(idat$scat,0) &  (is.element(idat$ssrc,1) & !is.na(idat$ssrc)))

			## Keepers ----------
			zkeep  = is.element(idat$scat,1) & (is.element(idat$ssrc,2) & !is.na(idat$ssrc))
			zkeep  = zkeep | (is.element(idat$scat,3) & (is.element(idat$ssrc,2) | is.na(idat$ssrc)))
			zkeep  = zkeep | (is.element(idat$scat,0) &  (is.element(idat$ssrc,2) & !is.na(idat$ssrc)))

			## Discards ---------
			zdisc  = is.element(idat$scat,1) & (is.element(idat$ssrc,3) & !is.na(idat$ssrc))
			zdisc  = zdisc | (is.element(idat$scat,4) & (is.element(idat$ssrc,3) | is.na(idat$ssrc)))
			zdisc  = zdisc | (is.element(idat$scat,0) &  (is.element(idat$ssrc,3) & !is.na(idat$ssrc)))

			idat$sort[zunso] = "U"
			idat$sort[zkeep] = "K"
			idat$sort[zdisc] = "D"
			zflds = c("sort", setdiff(zflds,"sort"))

			## Testing results of sorting:
			## test = paste(idat$sort,idat$scat,idat$ssrc,sep="-")
			## table(test) # e.g., BOR:
			## D-1-3  K-1-2  K-3-2 K-3-NA  U-0-1  U-1-1 U-1-NA Z-0-NA  Z-3-1 
			##   479    175   1216    983      6   5044   1100      3     12 
		}
		if (strSpp=="REBS") {
			idat$sppG = idat$spp
			idat$gene = rep(0,nrow(idat))
			gBSR = grepl("18",idat$exist); idat$sppG[gBSR]  = "425"; idat$gene[gBSR] = 1
			gRER = grepl("19",idat$exist); idat$sppG[gRER]  = "394"; idat$gene[gRER] = 1
			gHY1 = grepl("20",idat$exist); idat$sppG[gHY1]  = "HY1"; idat$gene[gHY1] = 1
			gHY2 = grepl("21",idat$exist); idat$sppG[gHY2]  = "HY2"; idat$gene[gHY2] = 1
			gFAIL= grepl("22",idat$exist); idat$sppG[gFAIL] = "FAIL";idat$gene[gFAIL] = 1
			idat$stock = rep("",nrow(idat))
			idat$stock[is.element(idat$major,3:6)] = "RER"
			idat$stock[is.element(idat$major,8:9)] = "BSR"
			idat$stock[is.element(idat$major,7)]   = "HYB"
		}
		aflds=c("EID","X","Y")
#browser();return()
		idat=idat[,c(aflds,setdiff(names(idat),c(aflds,zflds)), zflds)]
#if(i==2) {browser();return()}
		DAT=rbind(DAT,idat)
	} # end nloop
	if (!any(is.na(DAT$X)) && !any(is.na(DAT$Y))) DAT=as.EventData(DAT)
	attributes(DAT)=c(attributes(DAT),atts)
	attr(DAT, "version") = Sys.time()
	return(DAT)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~processBio


## processMap---------------------------2020-05-24
## Process PBSdat created by call to `fos_map_density.sql'
## to create a map object for stock assessment.
## ---------------------------------------------RH
processMap = function(dat=PBSdat, strSpp, prefix="map", useSM=FALSE)
{
	if (!useSM)
		dat = zapSeamounts(dat)  ## remove seamount data
	## Replace zero-effort fields when catch exists
	z0 = round(dat$eff,5) <= 0 & round(dat[,strSpp],5) > 0
	z1 = round(dat$eff,5) > 0 & round(dat[,strSpp],5) > 0
	gear = .su(dat$gear)
	ttput(dat)
	for (g in gear) {
		zg   = is.element(dat$gear,g) & !is.element(dat$PMFC,"00")
		zg1  = zg & z1
		adat = split(dat[zg1,], dat$PMFC[zg1])
		lapply (adat, function(x) {
			ttget(dat)
			za     = is.element(dat$PMFC, unique(x$PMFC))  ## index PMFC area in greater dat
			if (sum(z0&zg&za) == 0) return()
			x$leff = log10(x$eff)
			x$lcat = log10(x[,strSpp])
			x$zdep = ceiling(x$fdep/50)*50 ## 50m zones
			xmod = lm(leff ~ lcat + zdep + locality + cfv, data=x)
			xnew = dat[z0&zg&za,]
			xnew$lcat = log10(xnew[,strSpp])
			xnew$zdep = ceiling(xnew$fdep/50)*50 ## 50m zones
			xnew$leff = predict.lm(xmod, newdata=xnew)
			xnew$eff  = 10^xnew$leff
			dat$eff[z0&zg&za] = xnew$eff
#print(dim(x))
			ttput(dat)
		})
		#aeff = split(dat$eff[zg], dat$PMFC[zg])      ## gear effort list by PMFC area
		#acat = split(dat[,strSpp][zg], dat$PMFC[zg]) ## gear effort list by PMFC area
		#geff = mean(sapply(aeff,calcPMR)["mu",])     ## mean gear effort of the non-zero efforts
		#dat$eff[z0&zg] = geff                        ## replace zero effort with mean effort if catch is positive
	}
	ttget(dat)
	dat = calcStockArea(strSpp, dat=dat)
#browser();return()
	fidpos = match("fid",names(dat))
	dat  = as.EventData(data.frame(EID=1:nrow(dat),dat[,-fidpos],fid=dat[,fidpos], check.names=FALSE, stringsAsFactors=FALSE), projection="LL")
	mess = paste0(prefix,strSpp," = dat; save (\"", prefix, strSpp, "\",file=\"", prefix, strSpp, ".rda\")")
	eval(parse(text=mess))
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~processMap


## quantAges----------------------------2019-10-19
## Plot quantile boxes of age by year and/or area,
## including mean age over time.
## Suggested by PJS to detect changes in age.
## -----------------------------------------PJS/RH
quantAges =function(bioDat, dfld="age", afld="major", tfld="year", 
   type="time", major=3:9, years=1990:2018, ylim=c(0,35), strSpp="RSR",
   outnam="Quant-Age", png=FALSE, pngres=400, PIN=c(8,8),
   scat, lang=c("e","f"))  # types: time, area
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
	nyrs = length(years)
	bioDat = bioDat[is.element(bioDat$year,years),]
	if (!missing(scat))
		bioDat = bioDat[is.element(bioDat$scat,scat),]

	Nscat = crossTab(bioDat,c("major","gear","scat"),"age",function(x){countVec(x)})
	for (k in dimnames(Nscat)$scat)
		write.csv(Nscat[,,k],paste0("Nscat",k,".csv"))
	Mscat = crossTab(bioDat,c("major","gear","scat"),"age",function(x){mean(x)})
	for (k in dimnames(Mscat)$scat)
		write.csv(Mscat[,,k],paste0("Mscat",k,".csv"))
#browser();return()

	#mcol  = .colBlind[c("skyblue","blue")]
	#fcol  = .colBlind[c("vermillion","redpurple")]
	if (strSpp %in% c("RSR","WWR")){
		mcol   = c("cyan","blue","darkblue")
		fcol   = c("pink","red","darkred")
	} else {
		mcol   = c("springgreen","darkgreen","green3")
		fcol   = c("gold","orange4","goldenrod")
	}
	boxwex = ifelse(type=="area",0.25,0.35)
	Qage   = list()

	if (type=="time") {
		abioDat = split(bioDat, bioDat[,afld])
		yearbox = as.list(rep(NA,nyrs)); names(yearbox) = years

		fout = fout.e = outnam
		for (l in lang) {  ## could switch to other languages if available in 'linguaFranca'.
			changeLangOpts(L=l)
			fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
			if (png) png(paste0(fout,".png"), width=PIN[1], height=PIN[2], units="in", res=pngres)
			par(mfcol=c(length(abioDat),2), mar=c(0,2,0,0), oma=c(4,2,1,1), mgp=c(2,0.5,0))

			for (j in 1:2){
				jj = c("C","S")[j]
				jjj = c("Commercial","Survey")[j]

				for (a in length(abioDat):1) {
					aa   = names(abioDat)[a]
					adat = abioDat[[a]]
#browser();return()
					adat = adat[is.element(adat$ctype,jj),]
					if (nrow(adat)==0) {
						quantBox(yearbox, outline=FALSE, ylim=ylim, xaxt="n")
					}
					else {
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

						quantBox(yearbox, outline=FALSE, ylim=ylim, xaxt="n")
						quantBox(Msex, xaxt="n", yaxt="n", outline=FALSE, boxcol="grey30", boxfill=mcol[1], medlwd=1, medcol=mcol[2], whisklty=1, whiskcol="gainsboro", add=TRUE, boxwex=boxwex, at=(1:nyrs)+(boxwex/2))
						quantBox(Fsex, xaxt="n", yaxt="n", outline=FALSE, boxcol="grey30", boxfill=fcol[1], medlwd=1, medcol=fcol[2], whisklty=1, whiskcol="gainsboro", add=TRUE, boxwex=boxwex, at=(1:nyrs)-(boxwex/2))

						#lines((1:nyrs)-(boxwex/2),sapply(Fsex,mean),col=fcol[2],lwd=2)
						#lines((1:nyrs)+(boxwex/2),sapply(Msex,mean),col=mcol[2],lwd=2)
					}
					axis(1, at=1:nyrs, labels=FALSE, tcl=0.25)
					if (a==1) axis(1, at=seq(1,nyrs,5), labels=seq(years[1],rev(years)[1],5), cex.axis=1.2, tcl=0.5)
					jfranc = ifelse(l=="f" && jj=="S","R",jj)
					addLabel(0.05, 0.95, linguaFranca(paste0(jfranc," - Major ",aa),l), adj=c(0,1), cex=1)
				}
			}
			mtext(linguaFranca("Year",l), side=1, outer=TRUE, line=2.5, cex=1.5)
			mtext(linguaFranca(dlab,l), side=2, outer=TRUE, line=0.25, cex=1.5, las=3)
			if (png) dev.off()
		}; eop()
	} ## end if time

	if (type=="area") {
		pmfc = c("3C","3D","5A","5B","5C","5D","5E"); names(pmfc) = 3:9
		jbioDat = split(bioDat, bioDat[,"ctype"])
		areas   = .su(bioDat[,afld]); nareas = length(areas)
		areabox = as.list(rep(NA,nareas)); names(areabox) = areas

		fout = fout.e = outnam
		for (l in lang) {  ## could switch to other languages if available in 'linguaFranca'.
			changeLangOpts(L=l)
			fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
			if (png) png(paste0(fout,".png"), width=PIN[1], height=PIN[2], units="in", res=pngres)
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
				nMsex = sapply(Msex,countVec)


				females = is.element(jdat$sex,2)
				fsex = split(jdat[,dfld][females],jdat[,afld][females])
				zsex = is.element(names(fsex),areas)
				Fsex = areabox
				Fsex[names(fsex[zsex])] = fsex[zsex]
				Qage[[jj]][["F"]] = Fsex
				nFsex = sapply(Fsex,countVec)

				if (is.null(ylim))
					Ylim = c(min(sapply(Qage[[jj]],function(x){sapply(x,function(xx){quantile(xx,0.05,na.rm=TRUE)})}),na.rm=TRUE),
						max(sapply(Qage[[jj]],function(x){sapply(x,function(xx){quantile(xx,0.95,na.rm=TRUE)})}),na.rm=TRUE))
				else Ylim = ylim

				quantBox(areabox, outline=FALSE, ylim=Ylim, xaxt="n")
				quantBox(Msex, xaxt="n", yaxt="n", outline=FALSE, boxcol="grey30", boxfill=mcol[1], medcol=mcol[2], whisklty=1, whiskcol="gainsboro", add=TRUE, boxwex=boxwex, at=(1:nareas)+(boxwex/2), varwidth=TRUE)
				text((1:nareas)+(boxwex/2), 0, nMsex, col=mcol[3], cex=0.8, font=2)
				quantBox(Fsex, xaxt="n", yaxt="n", outline=FALSE, boxcol="grey30", boxfill=fcol[1], medcol=fcol[2], whisklty=1, whiskcol="gainsboro", add=TRUE, boxwex=boxwex, at=(1:nareas)-(boxwex/2), varwidth=TRUE)
				text((1:nareas)-(boxwex/2), 0, nFsex, col=fcol[3], cex=0.8, font=2)

				#lines((1:nareas)-(boxwex/2),sapply(Fsex,mean),col=fcol[2],lwd=2)
				#lines((1:nareas)+(boxwex/2),sapply(Msex,mean),col=mcol[2],lwd=2)

				axis(1, at=1:nareas, labels=FALSE, tcl=0.25)
				if (j==2) axis(1, at=1:nareas, labels=pmfc[as.character(areas)], cex.axis=1.2, tcl=0.5)
				addLabel(0.05, 0.95, linguaFranca(paste0(jjj),l), adj=c(0,1), cex=1)
			}
			mtext(linguaFranca("PMFC Area",l), side=1, outer=TRUE, line=2.5, cex=1.5)
#browser();return()
			mtext(linguaFranca(dlab,l), side=2, outer=TRUE, line=0.25, cex=1.5, las=3)
			if (png) dev.off()
		}; eop()
	} ## end if area
	return(Qage)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~quantAges


## splineCPUE---------------------------2022-04-28
## Fit spline curves through CPUE data to determine 
## optimal degrees of freedom (balance between rigorously
## fitting indices while not removing the majority of the signal)
## and to calculate CV process error from RSS at optimal DF.
## ---------------------------------------------RH
splineCPUE = function(dat, ndf=50, strSpp="ZZZ", ufld="cpue",
   png=FALSE, pngres=400, PIN=c(8,8), lang=c("e","f"), ...)
{
	colnames(dat) = sub("[Yy]ear(s)?","year",colnames(dat))
	colnames(dat) = sub(ufld,"index",colnames(dat))
	#ndf = max(ndf,nrow(dat))
	## Collect residual sum of squares by degrees of freedom
	DF  = seq(2,nrow(dat),length.out=ndf-1)
	#DF  = seq(0,1,length.out=100)
	RSS = rep(NA,length(DF))#; names(RSS) = DF
	for (i in 1:length(DF)){
		ii = DF[i]
		RSS[i] = smooth.spline(dat[,"year"], dat[,"index"], df=ii, all.knots=TRUE)$pen.crit
	}
	dRSS   = c(0,diff(RSS))
	df.opt = DF[findPV(min(dRSS),dRSS)]
	rho_k  = RSS[findPV(min(dRSS),dRSS)]

	createFdir(lang)
	fout.e = paste0(toupper(ufld), "res-CVpro-", strSpp)
	if (!is.null(list(...)$extra))
		fout.e =  paste0(fout.e,list(...)$extra)
#browser();return()

	for (l in lang) {
		changeLangOpts(L=l)
		fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		if (png) {
			clearFiles(paste0(fout,".png"))
			png(paste0(fout,".png"), units="in", res=pngres, width=PIN[1], height=PIN[2])
		}
		expandGraph(mfrow=c(2,2), mar=c(2.75,2.75,0.5,0.5), cex=1)
	
		plot(DF, RSS, type="n", xlab=linguaFranca("degrees of freedom",l), ylab=linguaFranca("RSS",l))
		addLabel(0.5, 0.95, linguaFranca("Residual Sum of Squares",l), adj=0.5)
		abline(v=df.opt, col="green4", lty=3)
		lines(DF, RSS, col="red", lwd=2)
	
		plot(DF, dRSS, type="n", xlab=linguaFranca("degrees of freedom",l), ylab=paste0("d",linguaFranca("RSS",l)))
		if (strSpp %in% c('YMR')) {
			xpos=0.95; ypos=0.5; xadj=1
		} else {
			xpos=0.5; ypos=0.95; xadj=0.5
		}
		addLabel(xpos, ypos, switch(l, 'e'="Change in RSS (~slope)", 'f'="changement dans SRC (~pente)"), adj=xadj)
		abline(v=df.opt, col="green4", lty=3)
		lines(DF, dRSS, col="blue", lwd=2)
		if (ndf<=50)
			points(DF,dRSS, pch=21, cex=.8, col="blue", bg="yellow")
		CVpro = sqrt(RSS[findPV(min(dRSS),dRSS)]/(nrow(dat)-2))/mean(dat[,"index"])
	
		##  Plot the data and the 'optimal' fit
		
		#plot(index ~ year, data = dat, pch=21, col="green4", bg="green", cex=1.1, xlab=linguaFranca("Year",l), ylab=linguaFranca(ufld,l)) #, main = "data(index) & smoothing splines")
		plot(index ~ year, data = dat, type="n", xlab=linguaFranca("Year",l), ylab=linguaFranca(ufld,l)) 
		lines(index ~ year, data = dat, lty=1, col="slategrey")
		points(index ~ year, data = dat, pch=21, col="green4", bg="green")
		index.spl <- with(dat, smooth.spline(year, index, all.knots=TRUE))
		lines(index.spl, col="blue", lty=2, lwd=2)
	
		index.df <- smooth.spline(dat[,"year"], dat[,"index"], df=df.opt, all.knots=TRUE)

		df.spl  = index.spl$df
		rho.spl = RSS[findPV(df.spl,DF)]
		cp.spl   = sqrt(rho.spl/(nrow(dat)-2))/mean(dat[,"index"])

		lines(index.df, col="red", lty=1, lwd=2)
		if (strSpp %in% c('BSR','WWR','YMR') || ufld %in% c("PDO")) {
			xpos=0.95; ypos=0.99; xadj=1
		} else {
			xpos=0.02; ypos=0.2; xadj=0
		}
		addLegend(xpos, ypos, legend=c(paste0(switch(l, 'e'="fitted df (nu) = ", 'f'=eval(parse(text=deparse("degr\u{00E9}s de libert\u{00E9} ajuste\u{00E9}s (nu) = ")))), signif(index.spl$df,4), ", cp=", signif(cp.spl,4)), paste0(switch(l, 'e'="with nu=", 'f'="avec nu="), signif(df.opt,4), ", rho=", signif(rho_k,4), ", cp=", signif(CVpro,4))), col = c("blue","red"), lty = 2:1, bty="n", bg="transparent", adj=0, xjust=xadj, cex=0.8)
		## Residual (Tukey Anscombe) plot:
		ylim = if(ufld=="PDO") NULL else c(-0.35,0.45)
		plot(residuals(index.df) ~ fitted(index.df), pch=21, col="red", bg="pink", cex=1.1, xlab=linguaFranca("Fitted Index",l), ylab=linguaFranca("Index residuals",l))
		abline(h = 0, col = "red")
		## consistency check:
		stopifnot(all.equal(dat[,"index"], fitted(index.df) + residuals(index.df)))
		## The chosen inner knots in original x-scale :
		with(index.df$fit, min + range * knot[-c(1:3, nk+1 +1:3)]) # == unique(index$year)

		if (png) dev.off()
	}; eop()
	return(list(DF=df.opt, RSS=RSS[findPV(min(dRSS),dRSS)], CVpro=CVpro))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~splineCPUE


## tabAmeth-----------------------------2023-05-04
##  Tabulate ageing error structures available in bio123.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~RH
tabAmeth = function(dat)
{
	dat  = dat[dat$age>0 & !is.na(dat$age),]
	tdat = split(dat, dat$ttype)
	names(tdat) = sapply(names(tdat), function(i){switch(as.numeric(i),"Non-obs domestic","Research","Charter","Obs domestic","Obs J-V")})

	tlist = lapply (tdat, function(x) {
		x$sexnam = rep("U",nrow(x))
		x$sexnam[is.element(x$sex,2)] = "F"
		x$sexnam[is.element(x$sex,1)] = "M"
		mess = list()
		for (i in .su(x$ameth)) {
			ii = switch(as.character(i), '0'="unknown method", '1'="surface read", '2'="thin section", '3'="break & burn", '4'="burnt & ts", 
				'5'="unknown otolith", '6'="dorsal fin xs", '7'="pectoral fin", '8'="pelvic fin", '9'="unknown fin", '10'="scale",
				'11'="dorsal spine", '12'="vertebrae", '13'="length", '14'="pre-opercular", '15'="anal fin", '16'="surface & B/B", '17'="break & bake")
			idat = x[is.element(x$ameth,i),]
			nspec = crossTab(idat,"sexnam","age",countVec)
			nsamp = crossTab(idat,"sexnam","SID",function(j){length(.su(j))})
			#mess[[ii]]  = paste0(names(nspec), ": ", nspec," (",nsamp,")")
			imess =  paste0(nspec," (",nsamp,")")
			names(imess) = names(nspec)
			if (is.null(ii)) {browser();return()}
			.flush.cat(ii,"\n")
			mess[[ii]] = imess
		}
		return(mess)
	})
	ss = names(unlist(sapply(tlist,names)))
	s0 = grep("[[:digit:]]$",ss,invert=TRUE)
	if (any(s0)) ss[s0] = paste0(ss[s0],"1")
	tt = sapply(tlist,length)
	out = array("---",dim=c(sum(tt),6), dimnames=list(ss, c("ttype","activity","ameth","F","M","U")))
	for (i in names(tlist)) {
		ii = ifelse(i %in% c("Research","Charter"),"survey","commercial")
		ilist = tlist[[i]]
		nn = 0
		for (j in names(ilist)) {
			nn = nn + 1
			ri = paste0(i,nn)
			jvec = ilist[[j]]
			out[ri,1:3] = c(i,ii,j)
			out[ri,names(jvec)] = jvec
		}
	}
	write.csv(out, "ameth.summary.csv", row.names=FALSE)
#browser();return()
	tlist$years = lapply(tdat,function(x){table(x$year)})
	return(list(tlist=tlist, out=out))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tabAmeth


## tabOtos------------------------------2023-04-24
##  Tabulate otoliths available and aged.
## ---------------------------------------------RH
tabOtos = function(strSpp="123", fpath=getwd())
{
	clearFiles( c(paste0(c(rep("otos",4),rep("aged",4)),"-",c("SSID","ttype","gear","major"),".csv"), paste0("oto",strSpp,".rda")) )

	mess = paste0("getFile(bio", strSpp, ", path=\"",fpath,"\"); oto", strSpp, " = bio", strSpp, "[is.element(bio", strSpp, "$oto,1),]; ttput(oto", strSpp, "); save(\"oto", strSpp, "\", file=\"oto", strSpp, ".rda\")")

#browser();return()
	#eval(parse(text=paste0(mess,collapse="")))

	## Table of otoliths collected year and factor
	mess = c(mess, paste0("write.csv(crossTab(oto", strSpp, ", c(\"year\",\"SSID\"),  \"oto\", countVec), file=\"otos-SSID.csv\")"))
	mess = c(mess, paste0("write.csv(crossTab(oto", strSpp, ", c(\"year\",\"ttype\"), \"oto\", countVec), file=\"otos-ttype.csv\")"))
	mess = c(mess, paste0("write.csv(crossTab(oto", strSpp, ", c(\"year\",\"gear\"),  \"oto\", countVec), file=\"otos-gear.csv\")"))
	mess = c(mess, paste0("write.csv(crossTab(oto", strSpp, ", c(\"year\",\"major\"), \"oto\", countVec), file=\"otos-major.csv\")"))

## Table of otoliths aged by year and factor
	mess = c(mess, paste0("write.csv(crossTab(oto", strSpp, ", c(\"year\",\"SSID\"),  \"age\", countVec), file=\"aged-SSID.csv\")"))
	mess = c(mess, paste0("write.csv(crossTab(oto", strSpp, ", c(\"year\",\"ttype\"), \"age\", countVec), file=\"aged-ttype.csv\")"))
	mess = c(mess, paste0("write.csv(crossTab(oto", strSpp, ", c(\"year\",\"gear\"),  \"age\", countVec), file=\"aged-gear.csv\")"))
	mess = c(mess, paste0("write.csv(crossTab(oto", strSpp, ", c(\"year\",\"major\"), \"age\", countVec), file=\"aged-major.csv\")"))

	if (strSpp %in% c("396")) {
		mess = c(mess, paste0("otoComm = oto", strSpp, "[is.element(oto", strSpp, "$ttype, c(1,4)),]"))
		mess = c(mess, paste0("write.csv(crossTab(otoComm, c(\"year\",\"major_adj\"), \"oto\", countVec), file=\"otos-major-comm.csv\")"))
		mess = c(mess, paste0("write.csv(crossTab(otoComm, c(\"year\",\"major_adj\"), \"age\", countVec), file=\"aged-major-comm.csv\")"))
	}

#browser();return()
	eval(parse(text=paste0(mess,collapse="; ")))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tabOtos

