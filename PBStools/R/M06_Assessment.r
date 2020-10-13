## =============================================================================
## Module 6: Assessment
## --------------------
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
##  processBio......Process PBSdat created by call to 'gfb_bio.sql' query.
##  processMap......Process PBSdat created by call to `fos_map_density.sql' to create a map object for stock assessment.
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

	fout = fout.e = outnam
	for (l in lang) {
		changeLangOpts(L=l)
		if (l=="f") fout = paste0("./french/",fout.e)  ## could repeat for other languages
		if (png) png(file=paste0(fout,".png"), units="in", res=pngres, width=PIN[1], height=PIN[2])
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
				legtxt = paste0(names(notos)," ",round(notos), switch(fac, 'age'=" otos", 'len'=" lens"))
				ylim = c(0,max(sapply(xvec,max),na.rm=TRUE))
	
				if ("discr" %in% type) {
					plot(0,0, xlim=c(1,amax), ylim=ylim, type="n", xlab="", ylab="", xaxt="n", yaxt="n")
					sapply(nord, function(n){
						if (!all(is.na(xvec[[n]])))
							#lines(1:length(xvec[[n]]), xvec[[n]], col=col[n], lty=lty[n], lwd=ifelse(n==1,3,2)) } )
							lines(1:length(xvec[[n]]), xvec[[n]], col=col[n], lty=lty[n], lwd=2) } )
					addLabel(0.95, 0.95, paste0(yy, " - ", linguaFranca(switch(s,"Male","Female"),l)), adj=1)
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
		#mtext (linguaFranca("Age",l), side=1, outer=TRUE, line=2.5, cex=1.5)
		mtext (linguaFranca(switch(fac, 'age'="Age", 'len'="Length (cm)"),l), side=1, outer=TRUE, line=2.5, cex=1.5)
		mtext (linguaFranca(paste0(ifelse(type=="cumul","Cumulative ",""), "Frequency"),l), side=2, outer=TRUE, line=2.5, cex=1.25, las=0)
		if(png) dev.off()
	}; eop()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~compAF


## compBmsy-----------------------------2020-04-20
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
				left.space = (max(nchar(names(Bmsy)))-ifelse(spplabs,nchar(spp),0))^0.9
			len.lab = max(sapply(names(Bmsy),nchar))                      ## RH 200420
			cex.lab = ifelse(len.lab>20, 0.9, ifelse(len.lab>15, 1, 1.2)) ## RH 200420
			par(mar=c(4,left.space + ifelse(l=="f",2,0), 0.5, 0.5),cex=ifelse(f%in%c("png","eps"),1,1.2),mgp=c(1.6,0.6,0))
			quantBox(Bmsy, horizontal=TRUE, las=1, xlim=c(0.5,nmods+top.space), ylim=ylim, cex.axis=1.2, yaxs="i", outline=FALSE,
				pars=list(boxwex=boxwidth,medlwd=2,whisklty=1), quants=quants, names=FALSE)
			if (Nrats>0)
				abline(v=ratios,col=rep(rcol,Nrats)[1:Nrats],lty=2,lwd=2)
	
			## segments(v=ratios,col=rep(c("red","green4","blue"),Nrats)[1:Nrats],lty=2,lwd=2)
			quantBox(Bmsy, horizontal=TRUE, las=1, xlim=c(0.5,nmods+1), ylim=ylim, cex.axis=1.2, yaxs="i", outline=FALSE, names=FALSE, pars=list(boxwex=boxwidth, medlwd=2, whisklty=1, medcol=medcol, boxfill=boxfill, ...), add=TRUE, quants=quants)
			## if (length(Bmsy)==1)  ## for some reason, a label is not added when there is only 1 boxplot.
			##	axis(2, at=1, labels=linguaFranca(names(Bmsy),l), las=1, cex.axis=1.2)
			axis(2, at=1:length(Bmsy), labels=linguaFranca(names(Bmsy),l), las=1, cex.axis=cex.lab) ## RH 200420
#browser();return()

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
#browser();return()
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
#browser();return()
	loca    = lenv()
	data(species, package="PBSdata", envir=loca)
#browser();return()
	if (missing(stock.name)) { ## RH 200616
		if (grepl("BSR|RER|HYB|REBS", strSpp))
			spp3 = strSpp
		else 
			spp3 = paste0(species[strSpp, "code3"],collapse="|")
	} else {
		spp3 = stock.name ## prone to breaking but use for now (RH 200616)
	}

	#spp3    = if(all(strSpp=="REBS")) "REBS" else if(all(strSpp=="HYB")) "HYB" else paste0(species[strSpp, "code3"],collapse="|")

	out  = if (gfld=="SSID") "-Surv" else paste0("-tt(",paste0(.su(dat$ttype),collapse=""),")")
	poo  = if (missing(exlax)) "" else paste0("-(",exlax,")")
	goo  = if (gfld=="gear")  paste0(paste0(names(gnames),"=",gsub("\\|","+",gnames)),collapse="_") else gfld
	foo  = paste0(spp3,"-(", datnam, ")", out , poo, "-g(", goo, ")", ifelse(strat,"-(strat_","-(obs_"),fld,")")
	fout = fout.e = if (missing(outnam)) foo else outnam
#browser();return()

	for (l in lang) {  ## could switch to other languages if available in 'linguaFranca'.
		changeLangOpts(L=l)
		fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		if (png) png(paste0(fout,".png"), units="in", res=pngres, width=PIN[1], height=PIN[2])
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
#browser();return()
			Idat = Idat[intersect(names(gval),names(Idat))]  ## order them as per user input
			if (strat && !boot) {
				resetGraph();expandGraph()
				#plotBubbles(Yprop,dnam=T,hide0=T,prettyAxis=T,siz=0.1,ylim=ylim,cpro=T)
				#points(as.numeric(names(Ymean)),Ymean, pch=21,col="red",bg="pink",cex=1.5)
				#plot(0,0,type="n",xlim=xlim,ylim=ylim,xlab="",ylab="", mgp=c(1.5,0.5,0))
			} else {
				quantBox(sapply(Qbox,function(x){NA},simplify=F), type="n", ylim=ylim, xlab="", ylab="")
				abline(v=seq(0.5,par()$usr[2],1),col="grey95")
			}
			sapply(1:length(Idat), function(i) {  ## loop through index 
				ii  = names(Idat)[i]
#browser();return()
#if (i==5) {browser();return()}
				idat = Idat[[i]]
				ival  = split(idat[,fld],idat$year)
				if (strat){
					yrs  = .su(idat$year)
					Ymat = array(0,dim=c(length(Lbin),length(yrs)), dimnames=list(len=Lbin,yr=yrs))
					idat$lbin = ceiling(idat[,fld]/lbin)*lbin
					for (y in yrs) {
	.flush.cat(y,"\n")
						yy = as.character(y)
						ydat = idat[is.element(idat$year,y),]
						jdat = split(ydat,ydat$GC)  ## split by Grouping Code (stratum)
	
						dGC  = sapply(jdat,function(x){sapply(split(x$density,x$SID),mean)},simplify=F)  ## density by SID in each GC
						pGC  = sapply(dGC,function(x){x/sum(x)},simplify=F)                              ## proportion density by SID in each GC
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
								jdat = sapply(jdat,function(x){x[sample(1:nrow(x),nrow(x),replace=T),]},simplify=F)
								ydat = do.call(rbind, jdat)
	
								dGC  = sapply(jdat,function(x){sapply(split(x$density,x$SID),mean)},simplify=F)  ## density by SID in each GC
								pGC  = sapply(dGC,function(x){x/sum(x)},simplify=F)                              ## proportion density by SID in each GC
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
					Ymean = sapply(ival,mean,na.rm=T)
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
#browser();return()
			}) ## end i (index) loop
			mtext(linguaFranca(ifelse(fld %in% c("len"), "Length (cm)", "Age (y)"),l), side=2, line=2.25, cex=1.5, las=0)
			yleg = ifelse(fld=="age" && gfld=="SSID", 0.9, 0.05)
			addLabel(0.025, yleg, linguaFranca(paste0(spp3, " ",switch(s,"Males","Females")),l), cex=1.2, adj=c(0,0))
			if (par()$mfg[1]==1) {
				leglab = gsub("_"," ",ssnames)
				#leglab = gsub("_"," ",ssnames[names(gval)])
#browser();return()
				addLegend(legpos[1], legpos[2], bty="n", fill=lucent(bxpcol[names(gval)],0.5), border="gainsboro", legend=linguaFranca(leglab,l), xjust=0, yjust=1)
				if (gfld %in% c("gear","major"))
					addLabel(0.975, 0.95, txt=linguaFranca( sub("bioDat","",datnam),l), cex=1.2, adj=c(1,1))
			}
		} ## end s (sex) loop
		mtext(linguaFranca("Year",l), side=1, outer=TRUE, line=0.5, cex=1.5)
		if (png) dev.off()
	}; eop()
#browser();return()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~compLen


## compVB-------------------------------2020-03-30
## Compare fitted von B curves using parameters.
## ---------------------------------------------RH
compVB = function(dat, index, A=1:40, subset=list(sex=c("Male","Female")), 
   col=c("blue","green4","red"), lty=c(1,2,3), ymax,
   outnam="compVB-RSR", png=FALSE, pngres=400, lang=c("e","f"))
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
					lines(A, vb, lwd=3, col=col, lty=lty)
#if (i=="Male") {browser();return()}
				}
			}
			lcol = rep(scols,nstock)
			llty = rep(sltys,nstock)
			legtxt = paste0(rep(stocks,length(iii))," -- ",rep(iii,each=nstock))
			legtxt = gsub("\\."," ", gsub("\\.mcmc|\\.err","",legtxt))
			addLegend(0.9,0.05, lty=llty, col=lcol, xjust=1, yjust=0, text.col=lcol, lwd=3, seg.len=3.5, bty="n",
				legend=linguaFranca(legtxt,l), cex=1.25)
			box(col="slategray")
			if (png) dev.off()
		}; eop()
	} ## end if subset=="sex"
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~compVB


## compVBpars---------------------------2020-06-16
##  Compare MCMC derived parameters from von Bertalanffy model
## ---------------------------------------------RH
compVBpars = function(bfiles, prefix="vbstan.barf.", 
   pnams=c("linf","k","t0"), nmcmc=4000, redo.QVB=TRUE,
   quants=c(0.05,0.25,0.5,0.75,0.95), bcol =c("orange", "green4"),
   outnam, png=FALSE, pngres=400, PIN=c(8,8), lang=c("e","f"))
{
	## Compare quantile plots for each parameter
	bnams = gsub("\\.rda$","",gsub(prefix,"",bfiles))
	if (!file.exists("QVB.rda") || redo.QVB) {
		QVB   = array(NA, dim=c(nmcmc, length(pnams), length(bfiles)), dimnames=list(1:nmcmc, pnams, bnams))
		ii = as.character(1:nmcmc); jj = pnams
		for (i in 1:length(bfiles)) {
			kk = bnams[i]
			ifile = load(bfiles[i])
			idat  = array(unlist(barf$p[pnams]), dim=c(nmcmc,length(pnams)), dimnames=list(1:nmcmc, pnams))
			QVB[ii,jj,kk] = idat[ii,jj]
		}
		save("QVB", file="QVB.rda")
	} else {
		load("QVB.rda")
	}
	QVB = QVB[,,bnams,drop=FALSE]
	pvb.med = apply(QVB[,1,],2,median)
	pvb.ord = order(pvb.med)
	col.ord = rep("white", length(pvb.ord))
	z.bsr   = grep("bsr|425",names(pvb.med)[pvb.ord])
	col.ord[z.bsr] = bcol[1]
	z.rer   = grep("rer|394",names(pvb.med)[pvb.ord])
	col.ord[z.rer] = bcol[2]

	colgrey = "grey60"
	bpars = list(boxwex=0.5, boxcol=colgrey, boxfill=lucent((col.ord),0.50), medcol=(col.ord), whisklty=1, whiskcol=colgrey, staplecol=colgrey)

	if (missing(outnam))
		outnam = paste0(prefix, ".pars.compare")
	fout.e = outnam
	for (l in lang) {  ## could switch to other languages if available in 'linguaFranca'.
		changeLangOpts(L=l)
		fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
#browser();return()
		if (png) png(paste0(fout,".png"), units="in", res=pngres, width=PIN[1], height=PIN[2])
		expandGraph(mfrow=c(3,1),mar=c(3.5,0,1,1), oma=c(0,12,0,0))

		for (p in 1:length(pnams)) {
			pp  = p[1]
			pvb = QVB[,pp,]
			pvb.med = apply(pvb,2,median)
	
			if (p==0) pvb.ord = order(pvb.med) ## disable for now
	
			if (p==0) { ## disable for now
				pvb.nam = names(pvb.med)
				pvb.new = c(sort(pvb.nam[grep("\\.female",pvb.nam)]), sort(pvb.nam[grep("\\.male",pvb.nam)]))
				## Highly specific
				t1=rep(c(rep("len.surv.",4),rep("len.syn.",2)),2)
				t2=rep(c("425G.","394G.",rep(c("bsr.","rer."),2)),2)
				t3=rep(c("female","male"), each=6)
				pvb.new = paste0(t1,t2,t3)
				pvb.ord = match(rev(pvb.new),pvb.nam)
			}
			#pdat.qnt = quantile(pdat, quants)
	
			ppp = c(expression(italic(L)[infinity]),expression(italic(K)),expression(italic(t)[0]),expression(italic(sigma)))[p]
			pvb.rev = pvb
			quantBox(pvb[,pvb.ord], horizontal=T, outline=F, pars=bpars, yaxt="n", cex.axis=1.2)
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
#browser();return()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~compVBpars


## createMA-----------------------------2020-03-24
##  Create table of DFO management actions for
##  Catch Appendix A tailored to species using 'strSppp'.
## ---------------------------------------------RH
createMA =function(yrs=1979:2019, strSpp="POP", addletters=TRUE)
{
	dfo.acts = as.list(rep(NA,length(yrs)))
	names(dfo.acts)=yrs
	for (i in yrs) {
		ii =as.character(i)
		dfo.acts[[ii]] = list()
	}
	##-----1979--------------------------
	dfo.acts[["1979"]][["PAH"]] = "PAH: Started limited vessel entry for Halibut fleet."
	##-----1980--------------------------
	dfo.acts[["1980"]][["POP"]] = "POP: Started experimental over-harvesting of SW Vancouver Island POP stock."
	##-----1981--------------------------
	dfo.acts[["1981"]][["SBF"]] = "SBF: Started limited vessel entry for Sablefish fleet."
	dfo.acts[["1981"]][["WAP"]] = "WAP: Pollock TAC (1981-1994): only 4B=Areas 13-18, 29"
	##-----1983--------------------------
	dfo.acts[["1983"]][["POP"]] = "POP: Started experimental unlimited harvesting of Langara Spit POP stock (5EN)."
	##-----1984--------------------------
	dfo.acts[["1984"]][["POP"]] = "POP: Ended experimental over-harvesting of SW Vancouver Island POP stock."
	##-----1990--------------------------
	dfo.acts[["1990"]][["PAH|SBF"]] = paste0(strSpp,": Started \\emph{Individual Vessel Quotas} (IVQ) systems for Halibut and Sablefish.")
	##-----1991--------------------------
	dfo.acts[["1991"]][["PAH"]] = "PAH: Started \\emph{Dockside Monitoring Program} (DMP) for the Halibut fleet."
	dfo.acts[["1991"]][["QBR|YYR"]] = "H\\&L: Started limited vessel entry for \\emph{Hook and Line} (H\\&L) fleet inside."
	##-----1992--------------------------
	dfo.acts[["1992"]][["QBR|YYR"]] = "H\\&L: Started limited vessel entry for H\\&L fleet outside."
	##-----1993--------------------------
	dfo.acts[["1993"]][["POPa"]] = "POP: Stopped experimental fishing of Langara Spit POP stock."
	dfo.acts[["1993"]][["POPb"]] = "POP: Closed POP fishery in PMFC area 5EN (Langara Spit)."
	dfo.acts[["1993"]][["RER|REBS"]] = "RER: Trip limits for trawl specified for the first time."
	##-----1994--------------------------
	dfo.acts[["1994"]][["@@@"]] = "TWL: Started a dockside monitoring program (DMP) for the Trawl fleet."
	dfo.acts[["1994"]][["POP|YMR|RER|CAR|SGR|YTR|RSR|WWR|SKR|SST|LST|REBS"]] = "TWL: As a means of both reducing at-sea discarding and simplifying the harvesting regime, rockfish aggregation was implemented. Through consultation with GTAC, the following aggregates were identified: Agg~1=~POP, YMR, RER, CAR, SGR, YTR; Agg~2=~RSR, WWR; Agg~3=~SKR, SST, LST; Agg~4=~ORF."
	##-----1995--------------------------
	dfo.acts[["1995"]][["BOR|RBR|RSR|SST"]] = "H\\&L: Implemented catch limits (monthly) on rockfish aggregates for H\\&L."
	dfo.acts[["1995"]][["WAP"]] = "WAP: Pollock TAC areas: 5CDE=5CD; 5AB=Area 12; 4B=Areas 13-18, 29."
	dfo.acts[["1995"]][["CAR|SGR|YTR|WWR|RER|POP|YMR|RSR|SKR|SST|LST|REBS"]] = "TWL: Trawl aggregates established in 1994 changed: Agg~1=~CAR, SGR, YTR, WWR, RER; Agg~2=~POP, YMR, RSR; Agg~3=~SKR, SST, LST; Agg~4=~ORF."
	##-----1996--------------------------
	dfo.acts[["1996"]][["@@@"]] = "TWL: Started 100\\% onboard observer program for offshore Trawl fleet."
	dfo.acts[["1996"]][["BOR|RBR|RSR|SST"]] = "H\\&L: Started DMP for H\\&L fleet."
	dfo.acts[["1996"]][["YTR|WWR|CAR|SGR|POP|YMR|RER|SKR|RSR|SCR|SST|LST|REBS"]] = "H\\&L: Rockfish aggregation will continue on a limited basis in 1996: Agg~1=~YTR, WWR; Agg~2=~CAR, SGR; Agg~3=~POP, YMR; Agg~4=~RER, SKR; Agg~5=~RSR, SCR; Agg~6=~ORF incl. SST, LST"
	dfo.acts[["1996"]][["WAP"]] = "WAP: Pollock TAC areas: 5CDE=5CD; 5AB=Areas 11,12; 4B=Areas 13-18, 29"
	##-----1997--------------------------
	dfo.acts[["1997"]][["@@@"]] = "TWL: Started IVQ system for Trawl \\emph{Total Allowable Catch} (TAC) species (April 1, 1997)"
	dfo.acts[["1997"]][["BOR|RBR"]] = paste0(strSpp,": Implemented catch limits (15,000 lbs per trip) on combined non-TAC rockfish for the Trawl fleet.")
	dfo.acts[["1997"]][["POP|YMR"]] = paste0(strSpp,": Permanent boundary adjustment -- Pacific Ocean Perch and Yellowmouth Rockfish caught within Subarea 102-3 and those portions of Subareas 142-1, 130-3 and 130-2 found southerly and easterly of a straight line commencing at 52$^\\circ$20$'$00$''$N 131$^\\circ$36$'$00$''$W thence to 52$^\\circ$20$'$00$''$N 132$^\\circ$00$'$00$''$W thence to 51$^\\circ$30$'$00$''$N 131$^\\circ$00$'$00$''$W and easterly and northerly of a straight line commencing at 51$^\\circ$30$'$00$''$N 131$^\\circ$00$'$00$''$W thence to 51$^\\circ$39$'$20$''$N 130$^\\circ$30$'$30$''$W will be deducted from the vessel's 5CD IVQ for those two species.")
	dfo.acts[["1997"]][["QBR|CPR|CHR|TIR|CAR|SGR|RER|SKR|SST|LST|POP|YMR|RSR|YTR|BKR|WWR|REBS"]] = "H\\&L: All H\\&L rockfish, with the exception of YYR, shall be managed under the following rockfish aggregates: Agg~1=~QBR, CPR; Agg~2=~CHR, TIR; Agg~3=~CAR, SGR; Agg~4=~RER, SKR, SST, LST; Agg~5=~POP, YMR, RSR; Agg~6=~YTR, BKR, WWR; Agg~7=~ORF excluding YYR."
	##-----1998--------------------------
	dfo.acts[["1998"]][["YYR"]] = "H\\&L: Aggregate 4 -- Option A: a quantity of Aggregates 2 to 5 and 7 combined not to exceed 100\\% of the total of Aggregate 1 per landing; an overage of Aggregate 1 and 6 up to a maximum of 10\\% per fishing period which shall be deducted from the vessel's succeeding fishing period limit. Option B: a quantity of Aggregates 2 to 7 combined not to exceed 100\\% of the Yelloweye rockfish per landing. Option C: 20,000 pounds of Aggregate 4 per fishing period; an overage for each of the Aggregates 3 to 5 and, Aggregates 6 and 7 combined, up to a maximum of 20\\% per fishing period which shall be deducted from the vessel's succeeding fishing period limit."
	##-----2000--------------------------
	dfo.acts[["2000"]][["PAH|RBR"]] = "PAH: Implemented catch limits (20,000 lbs per trip) on rockfish aggregates for the Halibut option D fleet."
	dfo.acts[["2000"]][["PAH|RBR|SST|BSR|RER|REBS"]] = "H\\&L: Implemented formal allocation of rockfish species between Halibut and H\\&L sectors."
	dfo.acts[["2000"]][["@@@"]] = "ALL: Formal discussions between the hook and line rockfish (ZN), halibut and trawl sectors were initiated in 2000 to establish individual rockfish species allocations between the sectors to replace the 92/8 split. Allocation arrangements were agreed to for rockfish species that are not currently under TAC. Splits agreed upon for these rockfish will be implemented in the future when or if TACs are set for those species."
	dfo.acts[["2000"]][["LST|SST"]] = "TWL: DFO cut LST TAC off WCVI to 404~t and set a conditional TAC of 425~t for an exploratory fishery north of 230$^\\circ$ true from Lookout Island."
	##-----2001--------------------------
	dfo.acts[["2001"]][["SGR"]] = "SGR: TAC reduction (3y) for SGR -- DFO has adopted conservative F=M harvest strategy in establishing the Silvergrey Rockfish TAC for all areas except 5AB. In 5AB the TAC will be stepped downward by 60 tonnes annually for each of the 2001/2002, 2003/2004 and 2003/2004 seasons to achieve this harvest strategy."
	dfo.acts[["2001"]][["POP"]] = "POP: TAC reduction (3y) for POP -- DFO reduced the 5CD POP TAC by 300 tonnes for research use as payment for the Hecate Strait Pacific Cod charter for each of the next three fishing seasons."
	dfo.acts[["2001"]][["BOR"]] = "BOR: PSARC (now CSAP) concerned that the decline of abundance indices for Bocaccio from the West Coast Vancouver Island (WCVI) Shrimp survey data and, in particular, the U.S. Triennial survey data reflected a serious decline. A detailed review of all survey indices was recommended to assess trends in Bocaccio abundance."
	dfo.acts[["2001"]][["BSR|RER|REBS"]] = "RER: Set commercial allocations among sectors (ongoing to 2019): Trawl 55.8\\%, H\\&L 41.17\\%, Halibut 3.03\\%."
	##-----2002--------------------------
	dfo.acts[["2002"]][["QBR|YYR|CPR|CHR|TIR"]] = "H\\&L: Established the inshore rockfish conservation strategy."
	dfo.acts[["2002"]][["@@@"]] = "TWL: Closed areas to preserve four hexactinellid (glassy) sponge reefs."
	dfo.acts[["2002"]][["LST|SST"]] = "TWL: Managers created 5 LST management zones coastwide (WCVI, Triangle, Tidemarks, Flamingo, Rennell); zones north of WCVI were designated ``experimental''."
	dfo.acts[["2002"]][["BOR"]] = "BOR: Status of Bocaccio was designated as `Threatened' by the Committee on the Status of Endangered Wildlife in Canada (COSEWIC) in November 2002. The designation was based on a new status report that indicated a combination of low recruitment and high fishing mortality had resulted in severe declines and low spawning abundance of this species. As the Species at Risk Act (SARA) was not yet in place, there was no legal designation for Bocaccio. Protection under SARA would only come in the event that this species was listed, by regulation, under the Act."
	##-----2003--------------------------
	dfo.acts[["2003"]][["BOR|CAR|LST|YMR|YYR|BSR|RER|REBS"]] = paste0(strSpp,": Species at Risk Act (SARA) came into force in 2003.")
	##-----2004--------------------------
	dfo.acts[["2004"]][["BOR"]] = "BOR: DFO reviewed management measures in the groundfish fisheries to assess the impacts on listed species under SARA. Voluntary program for the trawl fleet was developed and implemented in 2004 in which groundfish trawl vessels directed the proceeds of all landed Bocaccio Rockfish for research and management purposes. Ongoing to 2019."
	##-----2005--------------------------
	dfo.acts[["2005"]][["BORa"]] = "BOR: DFO consulted with First Nations, stakeholders, and the Canadian public on Bocaccio COSEWIC designation for 1.5 years and planned recommendations for further action to be presented to the Minister of Environment and Governor in Council (Cabinet) in spring 2005. A final listing decision by Governor in Council was expected in October 2005."
	dfo.acts[["2005"]][["BORb"]] = "BOR: As a proactive measure, industry reduced the harvest of Bocaccio, beginning in 2004, and resulted in a reduction of the Bocaccio catch by over 50\\% percent. Subsequently, measures to avoid Bocaccio were taken in the fishing years 2005/06 through 2019/20."
	dfo.acts[["2005"]][["BORc"]] = "BOR: The Government of Canada announced in November 2005 that Bocaccio be sent back to COSEWIC for further information or consideration."
	##-----2006--------------------------
	dfo.acts[["2006"]][["@@@"]] = "ALL: Introduced an \\emph{Integrated Fisheries Management Plan} (IFMP) for all directed groundfish fisheries."
	dfo.acts[["2006"]][["@@@a"]] = "H\\&L: Implemented 100\\% at-sea electronic monitoring and 100\\% dockside monitoring for all groundfish H\\&L fisheries."
	dfo.acts[["2006"]][["POP"]] = "POP: TAC reduction for POP -- DFO reduced the 5CD POP TAC by 700 tonnes for use in possible research programs."
	dfo.acts[["2006"]][["BOR|RBR|RSR|SST|WWR"]] = "H\\&L: Implemented mandatory retention of rockfish for H\\&L."
	dfo.acts[["2006"]][["SST"]] = "H\\&L: Annual non-directed species caps by fishery -- Shortspine Thornyhead (Dogfish = 0.05\\% Dogfish IVQ, Outside ZN = 1881 lbs., Halibut = 8000 lbs., Sablefish = 10,512 lbs.)"
	dfo.acts[["2006"]][["QBR|YYR|CPR|CHR|TIR"]] = "H\\&L: To support rockfish research the Groundfish Hook and Line Sub Committee (GHLSC) agreed to set aside 5\\% of the ZN allocations for research purposes."
	##-----2007--------------------------
	dfo.acts[["2007"]][["SST|BSR|RER|REBS"]] = paste0(strSpp,": Amendment to Halibut IVQ cap for SST and RER -- reallocations can only occur in blocks up to 4000 lbs or until the vessel species cap is met. Once the first 4000 lbs has been caught additional IVQ can be reallocated onto the licence up to 4000 lbs. This can continue until the vessel species cap is met.")
	dfo.acts[["2007"]][["BOR"]] = "BOR: COSEWIC reconfirmed Bocaccio's Threatened designation, and the species re-entered the SARA listing process in 2007."
	##-----2009--------------------------
	dfo.acts[["2009"]][["LST|RER|BSR|REBS"]] = paste0(strSpp,": Listed as species of `Special Concern' under SARA; management plan required.")
	##-----2011--------------------------
	dfo.acts[["2011"]][["POP"]] = "POP: TAC adjustment (3y) for POP -- combined 5ABCD POP TAC reduction to 3413\\,t will be achieved over a three year period through an annual reduction of 258\\,t. The expected catch level will be 68\\% of TAC."
	dfo.acts[["2011"]][["RBR"]] = "RBR: TAC implementation for RBR -- 1,300,000 lbs has been set for Redbanded Rockfish coastwide (50\\% allocated to trawl, 37.5\\% allocated to rockfish outside and 12.5\\% allocated to halibut) and harvesters are now responsible for this mortality."
	##-----2012--------------------------
	dfo.acts[["2012"]][["@@@"]] = "TWL: Froze the footprint of where groundfish bottom trawl activities can occur (all vessels under the authority of a valid Category T commercial groundfish trawl license selecting Option A as identified in the IFMP)."
	dfo.acts[["2009"]][["LST|RER|BSR|REBS"]] = paste0(strSpp,": Management plan published, with goal to maintain sustainable populations of LST and REBS within each species' known range in Canadian Pacific waters.")
	##-----2013--------------------------
	dfo.acts[["2013"]][["@@@"]] = "TWL: To support groundfish research, the groundfish trawl industry agreed to the trawl TAC offsets to account for unavoidable mortality incurred during the joint DFO-Industry groundfish multi-species surveys in 2013."
	dfo.acts[["2013"]][["POP"]] = "POP: New species-area groups have been created for Pacific Ocean Perch for 3CD, 5AB, 5C and 5DE."
	dfo.acts[["2013"]][["POPa"]] = "POP: Combine 5ABCD TACs reduction to 3413~mt is to be achieved over a three year period through an annual reduction of 258 mt. 2013/14 is the third year of this three year period. The expected catch level is to be 68\\% of TAC. TAC is subject to annual review."
	dfo.acts[["2013"]][["POPb"]] = "POP: Pacific Ocean Perch within Subarea 127-1 and that portion of Subareas 127-2 found northerly and westerly of 50$^\\circ$06$'$00$''$N will be deducted from the vessel's Pacific Ocean Perch rockfish 5A/B IVQ."
	dfo.acts[["2013"]][["BORa"]] = "BOR: COSEWIC had previously designated Bocaccio as Threatened in November 2002. Its status was re-examined and designated Endangered in November 2013."
	dfo.acts[["2013"]][["BORb"]] = "BOR: DFO formulated a plan for stepped reductions from current Bocaccio catch levels of approximately 137 tonnes (inclusive of trawl, groundfish hook and line, salmon troll, and recreational sectors) to a target level of 75 tonnes over 3 years (2013/14 to 2015/16). This plan accounted for First Nations' priority access for food, social, and ceremonial purposes. DFO worked with fishing interests to develop measures that would reduce Bocaccio catch and enable stock rebuilding over the long term."
	dfo.acts[["2013"]][["BORc"]] = "BOR: Annual Trawl \\emph{Mortality Cap} (MC) for Bocaccio was initially set at 150 tonnes. The IVQ carryover/underage limit was set to 15\\% of each vessels' Bocaccio holdings (in effect until 2019/20 fishery year)."
	dfo.acts[["2013"]][["BORd"]] = "BOR: All H\\&L groundfish fisheries subject to Bocaccio trip limits based on landings of directed species. For example, Halibut directed trips could land up to 200 pounds of Bocaccio when 15,000 pounds or less of Halibut was landed, 300 pounds of Bocaccio when 30,000 pounds of Halibut was landed and 400 pounds of Bocaccio when greater than 30,000 pounds of Halibut was landed. The Dogfish, Lingcod, ZN Rockfish, and Sablefish fisheries were subject to similar trip limits for Bocaccio. These trip limits remained in effect until 2015/16."
	##-----2015--------------------------
	dfo.acts[["2015"]][["@@@"]] = "ALL: Research allocations were specified starting in 2015 to account for the mortalities associated with survey catches to be covered by TACs."
	dfo.acts[["2015"]][["BORa"]] = "BOR: DFO Groundfish Management Unit refined the generalised primary objective for Bocaccio to specify that the aim was to also: \\emph{Achieve rebuilding throughout the species' range and grow out of the critical zone ($B > 0.4 B_{MSY}$) within three generations, with a 65\\% probability of success}. To support and monitor progress towards the objective, milestones were also established: \\emph{Achieve a positive stock trajectory trend in each 5-year interval, such that the biomass at the end of each 5-year period is greater than the biomass at the beginning of the same 5-year period. Between major assessments, progress towards this goal will be monitored by annually reviewing fishery-dependent and fishery-independent indices of stock trajectory}."
	dfo.acts[["2015"]][["BORb"]] = "BOR: To reduce Bocaccio mortality in the groundfish H\\&L fisheries new trip limits were introduced. For example, Halibut directed trips could land 100 pounds plus 1\\% of the amount of Halibut landed in excess of 10,000 pounds to a maximum of 600 pounds of Bocaccio. The Dogfish, Lingcod, ZN Rockfish, and Sablefish directed fisheries were subject to the same trip limits. These trip limits remained in effect during the 2019/20 fishing year."
	dfo.acts[["2015"]][["BORc"]] = "BOR: Bocaccio trawl MC reduced to 110~t coastwide."
	##-----2016--------------------------
	dfo.acts[["2016"]][["BORc"]] = "BOR: Bocaccio trawl MC reduced to 80~t coastwide. Bocaccio remains a quota species in the trawl fishery, but not in the hook and line fisheries."
	##-----2019--------------------------

	## Implementation
	out = lapply(dfo.acts,function(x,spp){
		if (any(grepl(paste0("@@@|",spp),names(x)))) {
			#print(names(x))
			poo = as.vector(unlist(x[grep(paste0("@@@|",spp),names(x))]))
			return(poo)
		}
	}, spp=strSpp)
	out = out[!sapply(out,is.null)]
	rout = lapply(names(out),function(i){
		df = data.frame(
			year=rep(as.numeric(i),length(i)),
			comment = as.vector(out[[i]]))
	})
	sout = do.call("rbind", lapply(rout, data.frame, stringsAsFactors=FALSE))
	if (addletters){
		sout = data.frame(code=c(letters,LETTERS)[1:nrow(sout)],sout)
		code.year = sapply(split(sout$code,sout$year),paste0,collapse=",")
		write.csv(code.year,"code.year.csv")
	}
	## Get rid of Latex controls
	sout$comment = gsub("emph\\{","",gsub("}","",gsub("~","",gsub("\\\\","",sout$comment))))
#browser();retrun()
	write.csv(sout, file=paste0("dfo.mgmt.acts.", strSpp, ".csv"), row.names=F)
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
   CV, Noff=1, less=0, more=0, png=FALSE, ptype="bubble")
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

		if (strSpp=="435") {
			tight = read.csv("aerr(435).mat.type=dnorm(0.01-0.99).noff=1.csv")
			tight4 = tight[1:4,]
			tight4[is.na(tight4)] = 0
			errmat[1:4,] = as.matrix(tight4[,-1]) ## remove 'age' column and convert to matrix for compatability
		}
		onam   = paste0("errmat.",ptype,".type=",type,spread,".noff=",Noff,".png")
#browser(); return()

		if (png) png(onam, units="in", res=400, width=8, height=8)
		expandGraph(mfrow=c(1,1), mar=c(3,3,1,1), oma=c(0,0,0,0))
		if (ptype=="bubble") {
			#plotBubbles(errmat, dnam=T, prettyAxis=T, frange=0.01, size=0.1)
			errbat = errmat
			rownames(errbat) = -(1:Amax)
			plotBubbles(errbat, dnam=T, xaxt="n", yaxt="n", frange=0.01, size=0.1)
			axis(1, at=seq(5,Amax,5), cex=1.2)
			axis(2, at=seq(-5,-Amax,-5), labels=seq(5,Amax,5), cex=1.2, las=1)
			mtext("Real Age", side=1, line=1.75, cex=1.5)
			mtext("Incorrect Age", side=2, line=1.75, cex=1.5)
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
#return()
#browser();return()
		#errtab = make.errtab(errmat, Ndiff=Noff)

		snam = paste0("aerr(",strSpp,").mat.type=",type,spread,".noff=",Noff,".dat")
		write.table(errmat, file=snam, sep="\t", row.names=FALSE, col.names=FALSE)

		fnam = gsub("dat$","csv", snam)
		cat(paste(c("age",dimnames(errmat)[[2]]),sep="",collapse=","),"\n",file=fnam)
		mess = paste(row.names(errmat),
			apply(errmat,1,function(x){
				z=x>0;xstr=x;xstr[!z]="";return(paste(xstr,sep="",collapse=","))}),sep=",")
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


## plotAgeErr---------------------------2020-03-23
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
		changeLangOpts(L=l)
		if (l=="f") fout = paste0("./french/",fout.e)  ## could repeat for other languages
		if (png) png(paste0(fout,".png"), units="in", res=pngres, width=PIN[1], height=PIN[2])
		par(mfrow=c(1,1), mar=c(3.5,3.5,0.5,0.75), oma=c(0,0,0,0), mgp=c(2,0.5,0))
		plot(dat$r1_age, dat$r2_age, xlim=xlim, ylim=ylim, type="n", xlab="", ylab="", cex.axis=1.2, las=1)
		axis(1, at=seq(0,xlim[2],ifelse(xlim[2]<80,1,5)), labels=F, tcl=-0.2)
		axis(2, at=seq(0,ylim[2],ifelse(ylim[2]<80,1,5)), labels=F, tcl=-0.2)
		abline(0,1,col=lucent("green4",0.5),lwd=2)
		## Drawing segments takes a long time, especially when sending to PNG device:
		segments(x0=dat$r1_amin, y0=dat$r2_age, x1=dat$r1_amax, y1=dat$r2_age, col=lucent("grey50",0.5),lwd=2)
		segments(x0=dat$r1_age, y0=dat$r2_amin, x1=dat$r1_age, y1=dat$r2_amax, col=lucent("grey50",0.5),lwd=2)
#browser();return()
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


## plotBTMW-----------------------------2020-08-07
##  Plot for bottom (BT) vs. midwater (MW) trawl catch
##  WHEN MC.GEAR IN ('BOTTOM TRAWL','UNKNOWN TRAWL') THEN 1
##  WHEN MC.GEAR IN ('TRAP') THEN 2
##  WHEN MC.GEAR IN ('MIDWATER TRAWL') THEN 3
##  WHEN MC.GEAR IN ('HOOK AND LINE') THEN 4
##  WHEN MC.GEAR IN ('LONGLINE') THEN 5
##  WHEN MC.GEAR IN ('LONGLINE OR HOOK AND LINE','TRAP OR LONGLINE OR HOOK AND LINE') THEN 8
## ---------------------------------------------RH
plotBTMW = function(dat, strSpp="417", years=1996:2018, major=3:9, 
   glist=list(gear=c(1:4)), removeSM=FALSE, ymax,
   png=FALSE, pngres=400, PIN=c(8,6) , lang=c("e","f"))
{
	flds = colnames(dat)
	if (removeSM) ## seamounts
		dat    = zapSeamounts(dat)
	else if (!is.element("seamount",flds))
		dat = zapSeamounts(dat, only.mark=TRUE)
	if (!is.element("year",flds))
		dat$year = as.numeric(substring(dat$date,1,4))
	dat = dat[is.element(dat$year, years),]
	dat = dat[is.element(dat$major, major),]
	if (!is.element("catKg",flds))
		dat$catKg = dat$landed + dat$discard
	dat = dat[dat$catKg > 0,]

	gcodes = list(
		gear   = c('0'="Unknown", '1'="Bottom_Trawl", '2'="Trap", '3'="Midwater_Trawl", '4'="Hook_&_Line", '5'="Longline", '8'="Mixed_H&L"),
		fid    = c('0'="Unknown", '1'="Trawl", '2'="Halibut", '3'="Sablefish", '4'="Dogfish/Lingcod", '5'="H&L_Rockfish", '8'="GF_Longline", '9'="Foreign"),
		sector = c('0'="UNKNOWN", '1'="FOREIGN", '2'="GROUNDFISH LONGLINE", '3'="GROUNDFISH TRAWL", '4'="HALIBUT", '5'="HALIBUT AND SABLEFISH", '6'="K/L", '7'="K/ZN", '8'="LINGCOD", '9'="ROCKFISH INSIDE",'10'="ROCKFISH OUTSIDE", '11'="SABLEFISH", '12'="SCHEDULE II", '13'="SPINY DOGFISH", '14'="ZN"),
		fishery = c('0'="Unknown", '1'="BSR_trawl", '2'="BSR_other", '3'="RER_trawl", '4'="RER_other", '5'="HYB_trawl", '6'="HYB_other")
	)

	gtabs  = list()
	if (png) createFdir(lang)

	for (g in 1:length(glist)) {
		gval  = glist[[g]]
		gnam  = names(glist)[g]
		if (!is.element(gnam,flds)) next
		gcode = gcodes[[gnam]]
		ncode = length(gcode)
		if (gnam %in% c("sector","fishery")) {  ## convert values in gdat to codes
			zcode = as.numeric(names(gcode))
			names(zcode) = as.vector(gcode)
			zdat = dat
			zdat[,gnam] = zcode[zdat[,gnam]]
		} else {
			zdat = dat
		}
#browser();return()
		gdat  = zdat[is.element(zdat[,gnam], gval),]

		#glty  = rep(c(6,1,5,2,3,4,3),ncode)[1:ncode]
		glty  = rep(1,ncode)
		gpch  = rep(c(4,21,8,24,22,25,6,5,3,2,1,7,9:16),ncode)[1:ncode]
		gcol  = rep(c("black","blue","purple","red","green4","darkgoldenrod1","coral","navy",.colBlind[-1],.colGnuplot[1:5]),ncode)[1:ncode]
		gbg   = rep(c(NA,"cyan",NA,"mistyrose1","green","yellow2",rep(NA,14)),ncode)[1:ncode]
		names(glty) = names(gcol) = names(gpch) = names(gbg) = names(gcode)

		#gtab  = ttcall(gtabs)[[gnam]]
		#if (is.null(gtab) || !all(years %in% rownames(gtab)) || removeSM){
		#	gtab  = crossTab(gdat, c("year",gnam), "catKg")
		#} else {
		#	colnames(gtab) = names(gcode)[match(colnames(gtab),gcode)]  ## reverse engineer colnames
		#	gtab = gtab[as.character(years),]
		#}
		gtab  = crossTab(gdat, c("year",gnam), "catKg")

		xlim  = range(years)
		xval  = as.numeric(rownames(gtab))
		ylim  = c(0, ifelse(missing(ymax), max(gtab)*1.2, ymax))
		onam  = paste0("Catch_", strSpp, "_", gnam)
		fout  = fout.e = onam

		for (l in lang) {
			fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
			changeLangOpts(L=l)
			if (png) png (filename=paste0(fout,".png"), width=PIN[1], height=PIN[2], units="in", res=pngres)
			expandGraph(mar=c(3,3,1.2,0.5), mgp=c(1.6,0.5,0))
			plot(0,0, xlim=xlim, ylim=ylim, type="n", xlab="", ylab="", cex.axis=1.2)
			axis(1, at=years, labels=FALSE, tcl=-0.25)
			for (i in 1:ncol(gtab)) {
				ii = colnames(gtab)[i]
#browser();return()
				lines(xval, gtab[,ii], lty=glty[ii], col=gcol[ii], lwd=3 )
				points(xval, gtab[,ii], pch=gpch[ii], col=gcol[ii], bg=gbg[ii],cex=1.5)
			}
			iii = intersect(as.character(gval),colnames(gtab)) ## use names instead of numeric indices
#browser(); return()
			addLegend(0.99, ifelse(strSpp %in% c("417"), 0.50,0.99), lty=glty[iii], seg.len=3, lwd=2, col=gcol[iii], pch=gpch[iii], pt.bg=gbg[iii], pt.cex=1.75, legend=linguaFranca(gsub("_"," ",gcode[iii]),l), cex=1.2, xjust=1, yjust=1, bty="n")
			mtext(linguaFranca("Year",l),side=1,line=1.75,cex=1.5)
			mtext(linguaFranca("Catch (t)",l),side=2,line=1.75,cex=1.5)
			if (png) dev.off()
		}; eop()

		colnames(gtab) = gcode[colnames(gtab)]
		write.csv(gtab, paste0(onam,".csv"))
		gtabs[[gnam]] = gtab
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
	if (missing(ylim)) ylim=range(dat[,-1], na.rm=T)
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
			if (i==1) data(popa,envir=penv())
			datpopa=rep("",nrow(idat))
			events=idat[!is.na(idat$X) & !is.na(idat$Y),c("EID","X","Y")]
			if (nrow(events)>0) {
				events=as.EventData(events)
				locs=findPolys(events,popa,maxRows=maxrows)
				locs=zapDupes(locs,"EID")
				pdata=attributes(popa)$PolyData; #pdata$label=substring(pdata$label,5)
				pvec=pdata$label[locs$PID]; names(pvec)=locs$EID
				names(datpopa)=idat$EID
				datpopa[names(pvec)]=pvec }
			idat$popa=datpopa
		}
		if (addstock) {
			if (missing(strSpp))
				stop("Must specify a species to determine stock allocation")
			#idat$stock = calcStockArea(strSpp, major=idat$major, minor=idat$minor)
			stockSpp = if (strSpp %in% c("REBS","RER","BSR","394","425")) "394" else strSpp
			idat = calcStockArea(stockSpp, dat=idat)
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
#browser();return()
	fidpos = match("fid",names(dat))
	dat  = as.EventData(data.frame(EID=1:nrow(dat),dat[,-fidpos],fid=dat[,fidpos], check.names=F, stringsAsFactors=F), projection="LL")
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
						quantBox(yearbox, outline=F, ylim=ylim, xaxt="n")
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

						quantBox(yearbox, outline=F, ylim=ylim, xaxt="n")
						quantBox(Msex, xaxt="n", yaxt="n", outline=F, boxcol="grey30", boxfill=mcol[1], medlwd=1, medcol=mcol[2], whisklty=1, whiskcol="gainsboro", add=T, boxwex=boxwex, at=(1:nyrs)+(boxwex/2))
						quantBox(Fsex, xaxt="n", yaxt="n", outline=F, boxcol="grey30", boxfill=fcol[1], medlwd=1, medcol=fcol[2], whisklty=1, whiskcol="gainsboro", add=T, boxwex=boxwex, at=(1:nyrs)-(boxwex/2))

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
					Ylim = c(min(sapply(Qage[[jj]],function(x){sapply(x,function(xx){quantile(xx,0.05,na.rm=T)})}),na.rm=T),
						max(sapply(Qage[[jj]],function(x){sapply(x,function(xx){quantile(xx,0.95,na.rm=T)})}),na.rm=T))
				else Ylim = ylim

				quantBox(areabox, outline=F, ylim=Ylim, xaxt="n")
				quantBox(Msex, xaxt="n", yaxt="n", outline=F, boxcol="grey30", boxfill=mcol[1], medcol=mcol[2], whisklty=1, whiskcol="gainsboro", add=T, boxwex=boxwex, at=(1:nareas)+(boxwex/2), varwidth=T)
				text((1:nareas)+(boxwex/2), 0, nMsex, col=mcol[3], cex=0.8, font=2)
				quantBox(Fsex, xaxt="n", yaxt="n", outline=F, boxcol="grey30", boxfill=fcol[1], medcol=fcol[2], whisklty=1, whiskcol="gainsboro", add=T, boxwex=boxwex, at=(1:nareas)-(boxwex/2), varwidth=T)
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
