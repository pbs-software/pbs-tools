##==============================================================================
## Module 2: Biology
## -----------------
##  calcLW..........Calculate length-weight relationship for a fish.
##  calcSG..........Calculate growth curve using Schnute growth model.
##  calcVB..........Calculate von Bertalanffy growth curve.
##  compCsum........Compare cumulative sum curves.
##  estOgive........Creates ogives of some metric (e.g., % maturity at age).
##  extractAges.....Extract records with positive age and qualify by the selected ageing method.
##  genPa...........Generate proportions-at-age using catch curve composition.
##  histMetric......Create a matrix of histograms for a specified metric.
##  histTail........Create a histogram showing tail details.
##  mapMaturity.....Plot maturity chart of stages by month.
##  plotAdens.......Plot density of field (length) by age as waveforms.
##  plotProp........Plot proportion-at-age from GFBio specimen data.
##  predictRER......Predict Rougheye Rockfish from biological data.
##  reportCatchAge..Report analyses from catch-at-age report.
##  requestAges.....Determine which otoliths to sample for ageing requests.
##  residVB.........Calculate and plot Studentised residuals as quantile plots.
##  simBSR..........Simulate Blackspotted Rockfish biological data.
##  simRER..........Simulate Rougheye Rockfish biological data.
##  sumBioTabs......Summarize frequency occurrence of biological samples.
##  weightBio.......Weight age/length frequencies/proportions by catch.
##==============================================================================


## calcLW-------------------------------2024-10-24
## Calculate length-weight relationship for a fish.
## Formerly called calcLenWt.
## A bit clumsy when you want just tables (see calcVB)
##   but much better output format.
## ---------------------------------------------RH
calcLW <- function(dat=pop.age, strSpp="396", xfld="len", yfld="wt", 
   areas=list(major=3:9), ttype=list(Research=c(2,3)), 
   sex=list(Females=2,Males=1), stype=NULL, gear=NULL, 
   rm.studs=NULL, tabs=TRUE, append=FALSE, plotit=TRUE,
   ptype="png", outnam, lang=c("e","f")) 
{
	## Start Subfunctions---------------------------
	## Device set-ups should match those in `calcVB'
	## ---------------------------------------------
	## Setup the EPS device for import to word, half page with 2 plotit side by side
	createEPS <- function(plotName,rc=c(2,2), width=3.5, height=3) { ## width & height for each panel
		plotName <- paste(plotName,"eps",sep=".")
		clearFiles(plotName)
		postscript(plotName, width=width*rc[2], height=height*rc[1]+0.75, paper="special", horizontal=FALSE, family="NimbusSan")
		par(mfrow=rc, mar=c(2.6,2.5,1,0.5), oma=c(0,0,0,0), mgp=c(1.5,0.5,0), cex=1.2) }

	## Setup the JPG device for import to word, half page with 2 plotit side by side
	createJPG <- function(plotName,rc=c(2,2), width=3.5, height=3) { ## width & height for each panel
		plotName <- paste(plotName,"jpg",sep=".")
		clearFiles(plotName)
		jpeg(plotName, quality=100, res=200, width=width*rc[2]*200, height=(height*rc[1]+0.75)*200, pointsize=12)
		par(mfrow=rc, mar=c(2.6,2.5,1,0.5), oma=c(0,0,0,0), mgp=c(1.5,0.5,0), cex=1.2) }

	## Setup the PDF device for import to word, half page with 2 plotit side by side
	createPDF <- function(plotName,rc=c(2,2), width=3.5, height=3) { ## width & height for each panel
		plotName <- paste(plotName,"pdf",sep=".")
		clearFiles(plotName)
		pdf(plotName, width=width*rc[2], height=height*rc[1]+0.75, paper="special")
		par(mfrow=rc, mar=c(2.6,2.5,1,0.5), oma=c(0,0,0,0), mgp=c(1.5,0.5,0), cex=1.2) }

	## Setup the PNG device for import to word, half page with 2 plotit side by side
	createPNG <- function(plotName,rc=c(2,2), width=3.5, height=3) { ## width & height for each panel
		plotName <- paste(plotName,"png",sep=".")
		clearFiles(plotName)
		png(plotName, units="in", res=200, width=width*rc[2], height=(height*rc[1]+0.75), pointsize=12)
		par(mfrow=rc, mar=c(2.6,2.5,1,0.5), oma=c(0,0,0,0), mgp=c(1.5,0.5,0), cex=1) }

	## Setup the TIFF device for import to word, half page with 2 plotit side by side
	createTIF <- function(plotName,rc=c(2,2), width=3.5, height=3) { ## width & height for each panel
		plotName <- paste(plotName,"tif",sep=".")
		clearFiles(plotName)
		tiff(plotName, units="px", res=200, width=width*rc[2]*200, height=(height*rc[1]+0.75)*200, pointsize=12)
		par(mfrow=rc, mar=c(2.6,2.5,1,0.5), oma=c(0,0,0,0), mgp=c(1.5,0.5,0), cex=1.2) }

	## Setup the WMF device for import to word, half page with 2 plotit side by side
	createWMF <- function(plotName,rc=c(1,2), width=3.5, height=3) { ## width & height for each panel
		plotName <- paste(plotName,"wmf",sep=".")
		clearFiles(plotName)
		if(.Platform$OS.type=="windows")
			do.call("win.metafile",list(filename=plotName, width=width*rc[2], height=height*rc[1]+0.75))
		par(mfrow=rc, mar=c(2.6,2.5,1,0.5), oma=c(0,0,0,0), mgp=c(1.5,0.5,0), cex=1.2) }
	##----------------------------------End Subfunctions

	assign("LW",list(module="M02_Biology",call=match.call(),args=args(calcLW)),envir=.PBStoolEnv)
	inObj = as.character(substitute(dat))

	## Create a subdirectory called `french' for French-language figures
	if (plotit) createFdir(lang)

	dat0 = dat  # save for later
	#dat <- dat[dat$len>=1 & !is.na(dat$len) & dat$wt>0 & !is.na(dat$wt) & !is.na(dat$sex),]
	dat <- dat[dat[,xfld]>0 & !is.na(dat[,xfld]) & dat[,yfld]>0 & !is.na(dat[,yfld]) & !is.na(dat$sex),]
	if (!is.null(ttype)) dat <- dat[is.element(dat$ttype,.su(unlist(ttype))),]
	else {
		ttype =  paste(.su(dat$ttype),collapse="|")
		dat$ttype = ttype
	}
	if (!is.null(stype)) dat <- dat[is.element(dat$stype,stype),]
	if (!is.null(gear))  dat <- dat[is.element(dat$gear,gear),]
	if (is.null(areas)) { areas <- list(coast="BC"); dat$coast <- rep("BC",nrow(dat)) }
	anams = sapply(areas,function(x){paste(paste(x,collapse="|",sep=""),sep="")})
	anams = paste(names(anams),anams,sep="_")
	tnams = sapply(ttype,function(x){paste(paste(x,collapse="|",sep=""),sep="")})
	tnams = paste(names(tnams),tnams,sep="_")
	ylim <- range(dat[,yfld],na.rm=TRUE); ylim[1] = 0
	xlim <- range(dat[,xfld],na.rm=TRUE); xlim[1] = 0
	out <- array(NA,dim=c(length(sex),10,length(ttype),length(areas)),
		dimnames=list(names(sex),c("n","a","SEa","b","SEb","w","SDw","wmin","wmax","wpred"),
		tnams,anams))
	names(dimnames(out)) <- c("sex","par","ttype","area")
	xout = array(NA,dim=c(2,length(ttype),length(areas),2),
		dimnames=list(range=c("min","max"),ttype=tnams,area=anams,year=c("year","date" )))
	plotNames = as.character()
	if (!plotit) lang = "e"  ## only need to go through fitting once for tables

	for (l in lang) {
		changeLangOpts(L=l)
		for (a1 in 1:length(areas)) {
			an = names(areas)[a1]; ar = areas[[a1]]
			adat <- dat[is.element(dat[,an],ar),]
	#browser();return()
			if (nrow(adat)==0) next
			ylim[2] <- max(adat[,yfld],na.rm=TRUE)
			akeyname  = paste0(ar,collapse="|")
			#arName <- paste(an,akeyname,sep="_")
			amat = paste(an,akeyname,sep="_")
			for (t1 in 1:length(ttype)) {
				tt   = ttype[[t1]]; ttt = names(ttype)[t1]
				tdat <- adat[is.element(adat$ttype,tt),]
				## Manually set to get a list of SSIDs for stock assessment table (RH 240617)
				collect.ssid = FALSE
				if (collect.ssid) {
					if ("SSID" %in% colnames(tdat))
						ssids = texThatVec(.su(tdat$SSID))
					else {
						load("ssid.svid.rda")
						ssids = texThatVec(.su(ssid.svid[names(table(tdat$SVID))]))
					}
					.flush.cat("Area=",ar,"; Ttype=",tt,": ", gsub("\\s+|and","",ssids), "\n\n")
					browser();return() 
				}
				if (nrow(tdat)==0) next
				ylim[2] <- max(tdat[,yfld],na.rm=TRUE)
				tmat = paste(ttt,paste0(tt,collapse="|"),sep="_")
				#ttName <- paste(paste(tt,collapse="|",sep=""),sep="")
				if (is.null(ttt)) ttt = tmat
				xout[,tmat,amat,"year"] = range(tdat$year,na.rm=TRUE)
				xout[,tmat,amat,"date"] = range(substring(tdat$date,1,10),na.rm=TRUE)
				if (missing(outnam)) {
					plotName <- gsub("_","(",gsub("\\|","",amat))
					plotName <- paste("LenWt-",strSpp,"-",plotName,")-tt(",gsub("\\|","",tmat),")",sep="")
					plotName <- paste0(plotName,"-data(",inObj,")")
					plotNames = c(plotNames,plotName)
				} else plotName = outnam
				fout.e = plotName
				#fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
				fout = switch(l, 'e' = paste0("./english/",fout.e), 'f' = paste0("./french/",fout.e) )
	
				if (length(sex)>3)
					rc = rev(.findSquare(length(sex)))  ## .findSquare exported from PBSmodelling namespace once more (RH 241106)
				else
					rc = c(1,length(sex))
				if (plotit && ptype!="win")
					eval(parse(text=paste0("create", toupper(ptype), "(\"", fout, "\",rc=", deparse(rc),")")))
				else
					par(mfrow=rc,cex=2.0,mar=c(3.5,3,1.5,.1),oma=c(0,0,0,0),mgp=c(2,0.5,0),cex=1)
				#else par(mfrow=c(2,2),cex=2.0,mar=c(3.5,3,1.5,.1),oma=c(0,0,0,0),mgp=c(2,.5,0),cex=1)

				for (i in 1:length(sex)) {
					ii   = sex[[i]]; iii = names(sex)[i]
					idat = tdat[is.element(tdat$sex,ii),];
					n1   = nrow(idat); #out[[iii]] <- idat
					if (n1==0) {
						if (iii=="Unknown") {
							next
						} else {
							## retrofit for now; better to calculate all fits and then plot and/or table the results (RH 241118)
							if (plotit) {
								plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
								addLabel(0.5, 0.5, linguaFranca("NO DATA",l))
							}
							next
						}
					}
					if (n1>1) {
						#fit1 = lm(log(wt)~log(len), data=idat)
						fit1 = eval(parse(text=paste0("lm(log(",yfld,")~log(",xfld,"), data=idat)")))
						if (!is.null(rm.studs) && is.numeric(rm.studs)) {
							res.stud = rstudent(fit1)
							if (length(rm.studs)==1) rm.studs = rep(rm.studs,2) * c(-1,1)
							rm.studs = sort(rm.studs)
							keep = res.stud >= rm.studs[1] & res.stud <= rm.studs[2]
							removed = length(res.stud) - sum(keep)
							idat0 = idat; fit0 = fit1; n0 = n1
							idat = idat[keep,]
							xlim <- range(idat[,xfld],na.rm=TRUE); xlim[1] = 0
							ylim <- range(idat[,yfld],na.rm=TRUE);  ylim[1] = 0
							n1   = nrow(idat)
							#fit1 = lm(log(wt)~log(len), data=idat)
							fit1 = eval(parse(text=paste0("lm(log(",yfld,")~log(",xfld,"), data=idat)")))
						}
						a <- fit1$coef[1]; aa <- format(signif(a,5), scientific=FALSE)
						b <- fit1$coef[2]; bb <- format(signif(b,5), big.mark=options()$big.mark, scientific=FALSE)
						se <- summary(fit1)$coefficients[,"Std. Error"]
						Wt <- (exp(a) * (0:xlim[2])^b) 
					} ## end if n1>1
					else {a=aa=b=bb=se=NA}
					w = idat[,yfld]
					wdat <- dat0[is.element(dat0[,an],ar) & is.element(dat0$ttype,tt) & is.element(dat0$sex,ii),]
					wdat <- wdat[wdat[,xfld]>=1 & !is.na(wdat[,xfld]),]
					wdat[,yfld] = (exp(a) * wdat[,xfld]^b)
					ovec <- c(n=n1,a=as.vector(a),SEa=as.vector(se[1]),b=as.vector(b),SEb=as.vector(se[2]),
						w=mean(w),SDw=sd(w),wmin=min(w),wmax=max(w),wpred=mean(wdat[,yfld]))
					out[iii,,tmat,amat] <- ovec

					if (plotit) {
						if (iii!="Unknown") {
							xaxs=yaxs="r"
							if (is.element(strSpp, c("228"))){ xlim=c(0,75); ylim=c(0,4.5); xaxs=yaxs="i" } ## WAP 
							if (is.element(strSpp, c("439"))){ xlim=c(0,50); ylim=c(0,1.3); xaxs=yaxs="i" } ## RSR
							if (is.element(strSpp, c("417","440"))){ xlim=c(0,60); ylim=c(0,3.5); xaxs=yaxs="i" } ## WWR|YMR
							if (is.element(strSpp, c("394","425"))){ xlim=c(0,90); ylim=c(0,12); xaxs=yaxs="i" } ## REBS (RER, BSR)
							if (is.element(strSpp, c("396"))){ xlim=c(0,60); ylim=c(0,2.5); xaxs=yaxs="i" } ## POP
							if (is.element(strSpp, c("418"))){ xlim=c(0,65); ylim=c(0,3.5); xaxs=yaxs="i" } ## YTR
							if (is.element(strSpp, c("405"))){ xlim=c(0,80); ylim=c(0,6); xaxs=yaxs="i" } ## SGR
							ylab = ifelse(yfld=="age", "   Age (y)", "   Weight (kg)")
							plot(jitter(idat[,xfld],0), jitter(idat[,yfld],0), pch=ifelse(grepl("mw$",inObj),18,20), cex=ifelse(plotit,0.5,0.8), col=switch(ttt, Research="dodgerblue", Commercial="orangered","orange"), xlab=linguaFranca("     Length (cm)",l), ylab=linguaFranca(ylab,l), main=linguaFranca(iii,l), xlim=xlim, ylim=ylim, bty="l", xaxs=xaxs, yaxs=yaxs)
							#col=.colBlind[switch(ttt,Research="bluegreen",Commercial="redpurple","orange")], #mgp=c(1.75,.5,0),
							axis(side=1, at=seq(5,  xlim[2],5),  tcl=-0.25, labels=FALSE)
							axis(side=1, at=seq(10, xlim[2],10), tcl=-0.5,  labels=FALSE)
							if (n1>1) {
								Wt <- (exp(a) * (0:xlim[2])^b) 
								lines(0:xlim[2], Wt, col=.colBlind["black"], lwd=ifelse(plotit,2,3))
							}
							mw <- format(round(mean(idat[,yfld],na.rm=TRUE),2), big.mark=options()$big.mark)
							xleft   = 0.075; ytop = 0.90
							cexlab  = ifelse(plotit,0.7,1)
							akeycoast = c("5CDE|5AB|3CD", "5DE|5ABC|3CD")
							arTitle = amat
							#if(any(grepl(akeyname, akeycoast))) arTitle = gsub(akeyname,"outer coast",amat,fixed=TRUE)
							#atTitle = paste0(gsub("_"," (",arTitle),") - trip type (",tmat,")",sep="")
#browser();r	eturn()
							arTitle = paste(substring(amat,1,gregexpr("_",amat)[[1]]-1),
								texThatVec(as.numeric(regmatches(amat,gregexpr("[[:digit:]]+",amat))[[1]])), sep=":" )
							atTitle = paste(substring(tmat,1,gregexpr("_",tmat)[[1]]-1),
								texThatVec(as.numeric(regmatches(tmat,gregexpr("[[:digit:]]+",tmat))[[1]])), sep=":" )
							artTitle = paste(arTitle, atTitle, sep="; ")
							artTitle = gsub(", ",",",gsub(" and ", "&", artTitle))
							if (i==floor(median(1:length(sex)))) {
								addLabel(0.5, 0.96, linguaFranca(artTitle,l), adj=c(.5,0), cex=cexlab+0.1)
							}
							if (yfld=="age")
								addLabel(xleft,ytop-0.10,bquote(bolditalic(bar(A)) == .(mw)),adj=0,cex=0.8)
							else
								addLabel(xleft,ytop-0.10,bquote(bolditalic(bar(W)) == .(mw)),adj=0,cex=0.8)
							addLabel(xleft,ytop-0.15,bquote(bolditalic(n)==.(format(n1, big.mark=options()$big.mark))),adj=0,cex=0.8)
							if (n1>1){
								addLabel(xleft,ytop-0.20,bquote(bold(log(alpha))==.(aa)),adj=0,cex=0.8)
								addLabel(xleft,ytop-0.26,bquote(bolditalic(beta)==.(bb)),adj=0,cex=0.8)
							}
							if (isThere("removed"))
								addLabel(xleft,ytop-0.30,bquote(italic(.(linguaFranca("removed",l)))==.(removed)),adj=0,cex=0.8)
							box(bty="l")
						} ## end iii (known sex) if
					} ## end if plotit
				} ## end i (sex) loop
				if (plotit && ptype!="win") dev.off()
.flush.cat(outnam, xout[,1,1,1], "\n")
				ttget(LW); LW[["fit"]][[outnam]] <- out; ttput(LW)
			} ## end t1 (ttype) loop
		} ## a1 (areas) loop
	}; eop()
	attr(out,"xout") = xout
	if (plotit) attr(out,"plotNames") = plotNames
	#omess = c( 
	#	paste("assign(\"lenwt",strSpp,"\",out); ",sep=""),
	#	paste("save(\"lenwt",strSpp,"\",file=\"lenwt",strSpp,".rda\")",sep="") )
	#eval(parse(text=paste(omess,collapse="")))
	ttget(LW); LW$out=out; LW$xout=xout; LW$dat=dat; ttput(LW)

	# Output table for Pre-COSEWIC
	if (tabs) {
		ttget(LW)
		pout = out
		zero=is.na(pout[,"n",,]); pout[,"n",,][zero]=0 ## replace NA with 0 if no tows occurred
		if (append)
			fnam <- paste("calcLW(",strSpp,").csv",sep="")
		else if (missing(outnam))
			fnam <- paste("calcLW(",strSpp,")-data(",inObj,").csv",sep="")
		else
			fnam = paste0(outnam,".csv")
		attr(out,"tableName") = fnam
#browser();return()
		if(tabs && !append) clearFiles(fnam)
	
		jkey = paste(names(ttype),sapply(ttype,function(x){paste(x,collapse="+")}),sep=": ")
		dimnames(pout)$ttype = jkey
		kkey = paste(names(areas),sapply(areas,function(x){paste(x,collapse="+")}),sep=": ")
		dimnames(pout)$area = kkey
		header <- dimnames(pout)$par;  header=gsub("a$","log(a)",dimnames(pout)$par)
	
		for (k in dimnames(pout)[[4]]) {
			
			for (j in dimnames(pout)[[3]]) {
				cat(paste("Area (",k,")   Trip type (",j,")",sep=""),"\n", file=fnam, append=TRUE)
				cat(paste(c("",header),collapse=","),"\n", file=fnam, append=TRUE)
	#browser();return()
				for (i in dimnames(pout)[[1]]) {
					iout <- t(pout[i,,j,k])
					cat(paste(c(i,iout),collapse=",",sep=""),"\n",file=fnam,append=TRUE)
				}
				cat("\n",file=fnam, append=TRUE)
			}
		}
		ttget(LW); LW$pout=pout; ttput(LW)
	}
	invisible(out)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~calcLW


## calcSG-------------------------------2024-11-19
## Calculate growth curve using Schnute growth model.
## Note: ameth=3 otoliths broken & burnt
## ---------------------------------------------RH
calcSG <- function(dat=pop.age, strSpp="", yfld="len", tau=c(5,80), fixt0=FALSE,
   areas=list(major=NULL, minor=NULL, locality=NULL, srfa=NULL,srfs=NULL, popa=NULL),
   ttype=list(commercial=c(1,4,5),research=c(2,3)), stype=c(1,2,6,7), scat=NULL,
   sex=list(Females=2,Males=1), rm.studs=NULL,
   year=NULL, xlim=NULL, ylim=NULL, ameth=c(3,17), jit=c(0,0), 
   eps=FALSE, jpg=FALSE, pdf=FALSE, png=FALSE, tif=FALSE, wmf=FALSE,
   outnam, singles=FALSE, pages=FALSE, tables=TRUE, figures=TRUE,
   ioenv=.GlobalEnv, lang=c("e","f"))
{
	assign("SG",list(module="M02_Biology",call=match.call(),args=args(calcSG),ioenv=ioenv),envir=.PBStoolEnv)
	#Subfunctions----------------------------------
	SGfun <- function(P) {
		aModel  <- function(P,xobs,tau=NULL) { #,is.pred=FALSE) {
			# Growth models - Schnute 1981
			a<-P[1]; b<-P[2]; y1<-P[3]; y2<-P[4];
			aa <- round(a,10); bb <- round(b,10); # For testing zero-values
			t1 <- tau[1]; t2 <- tau[2];
			#---Case 1---
			if (aa!=0 & bb!=0) {
				frac <- (1 - exp(-a*(xobs-t1))) / (1 - exp(-a*(t2-t1)));
				y0 <- y1^b + (y2^b - y1^b) * frac; y0 <- GT0(y0,eps=1e-8);
				y <- y0^(1/b); return(y);  }
			#---Case 2---
			if (aa!=0 & bb==0) {
				frac <- (1 - exp(-a*(xobs-t1))) / (1 - exp(-a*(t2-t1)));
				y <- y1 * exp(log(y2/y1) * frac); return(y);  }
			#---Case 3---
			if (aa==0 & bb!=0) {
				frac <- (xobs-t1) / (t2-t1);  y0 <- y1^b + (y2^b - y1^b) * frac;
				y0 <- GT0(y0,eps=1e-8); y <- y0^(1/b); return(y);  }
			#---Case 4---
			if (aa==0 & bb==0) {
				frac <- (xobs-t1) / (t2-t1);
				y <- y1 * exp(log(y2/y1) * frac); return(y);  }
		}
		ttput(aModel)
		unpackList(ttcall(SG),scope="L") # contains tau & is.pred
		ttget(SGdat)
		if (is.pred) xobs=xpred else xobs=SGdat$age
		ypred = aModel(P=P,xobs=xobs,tau=tau) #,is.pred=is.pred)
		if (is.pred)  return(ypred) #{ SG$yobs <<- ypred; return(ypred); }
		yobs  = SGdat$yval
		n <- length(yobs);  ssq <- sum( (yobs-ypred)^2 )
		return(n*log(ssq)) }

	calcP <- function(P,Pfix) { # t0, yinf, tstar, ystar, zstar (Eqns 24-28, Schnute 1981)
		a<-P[1]; b<-P[2]; y1<-P[3]; y2<-P[4]; t1 <- Pfix[1]; t2 <- Pfix[2];
		aa <- round(a,10); bb <- round(b,10);
		t0 <- NA; yinf <- NA; tstar <- NA; ystar <- NA;
		if (aa!=0 & bb!=0) {
			t0 <- t1 + t2 - (1/a)*log((exp(a*t2)*y2^b - exp(a*t1)*y1^b)/(y2^b - y1^b));
			yinf <- ((exp(a*t2)*y2^b - exp(a*t1)*y1^b)/(exp(a*t2)-exp(a*t1)))^(1/b);
			tstar <- t1 + t2 - (1/a)*log(b*(exp(a*t2)*y2^b - exp(a*t1)*y1^b)/(y2^b - y1^b));
			ystar <- ((1-b)*(exp(a*t2)*y2^b - exp(a*t1)*y1^b)/(exp(a*t2)-exp(a*t1)))^(1/b); };
		if (aa==0 & bb!=0) {
			t0 <- t1 + t2 - (t2*y2^b - t1*y1^b)/(y2^b - y1^b); };
		if (aa!=0 & bb==0) {
			yinf <- exp((exp(a*t2)*log(y2) - exp(a*t1)*log(y1))/(exp(a*t2)-exp(a*t1)));
			tstar <- t1 + t2 - (1/a)*log((exp(a*t2) - exp(a*t1))/log(y2/y1));
			ystar <- exp((exp(a*t2)*log(y2) - exp(a*t1)*log(y1))/(exp(a*t2)-exp(a*t1)) - 1); }
		zstar <- a / (1-b);
		return(list(t0=as.vector(t0),yinf=as.vector(yinf),tstar=as.vector(tstar),
			ystar=as.vector(ystar),zstar=as.vector(zstar))) }

	## Device set-ups should match those in `calcLW'
	## ---------------------------------------------
	## Setup the EPS device for import to word, half page with 2 plots side by side
	createEPS <- function(plotName,rc=c(2,2), width=3.5, height=3) { ## width & height for each panel
		plotName <- paste(plotName,"eps",sep=".")
		clearFiles(plotName)
		postscript(plotName, width=width*rc[2], height=height*rc[1]+0.75, paper="special", horizontal=FALSE, family="NimbusSan")
		par(mfrow=rc, mar=c(2.6,2.5,1,0.5), oma=c(0,0,0,0), mgp=c(1.5,0.5,0), cex=1.2) }

	## Setup the JPG device for import to word, half page with 2 plots side by side
	createJPG <- function(plotName,rc=c(2,2), width=3.5, height=3) { ## width & height for each panel
		plotName <- paste(plotName,"jpg",sep=".")
		clearFiles(plotName)
		jpeg(plotName, quality=100, res=400, width=width*rc[2]*400, height=(height*rc[1]+0.75)*400, pointsize=12)
		par(mfrow=rc, mar=c(2.6,2.5,1,0.5), oma=c(0,0,0,0), mgp=c(1.5,0.5,0), cex=1.2) }

	## Setup the PDF device for import to word, half page with 2 plots side by side
	createPDF <- function(plotName,rc=c(2,2), width=3.5, height=3) { ## width & height for each panel
		plotName <- paste(plotName,"pdf",sep=".")
		clearFiles(plotName)
		pdf(plotName, width=width*rc[2], height=height*rc[1]+0.75, paper="special")
		par(mfrow=rc, mar=c(2.6,2.5,1,0.5), oma=c(0,0,0,0), mgp=c(1.5,0.5,0), cex=1.2) }

	## Setup the PNG device for import to word, half page with 2 plots side by side
	createPNG <- function(plotName,rc=c(2,2), width=3.5, height=3) { ## width & height for each panel
		plotName <- paste(plotName,"png",sep=".")
		clearFiles(plotName)
		png(plotName, units="in", res=400, width=width*rc[2], height=(height*rc[1]+0.75), pointsize=12)
		par(mfrow=rc, mar=c(2.6,2.5,1,0.5), oma=c(0,0,0,0), mgp=c(1.5,0.5,0), cex=1.2) }

	## Setup the TIFF device for import to word, half page with 2 plots side by side
	createTIF <- function(plotName,rc=c(2,2), width=3.5, height=3) { ## width & height for each panel
		plotName <- paste(plotName,"tif",sep=".")
		clearFiles(plotName)
		tiff(plotName, units="in", res=400, width=width*rc[2], height=(height*rc[1]+0.75), pointsize=12)
		par(mfrow=rc, mar=c(2.6,2.5,1,0.5), oma=c(0,0,0,0), mgp=c(1.5,0.5,0), cex=1.2) }

	## Setup the WMF device for import to word, half page with 2 plots side by side
	createWMF <- function(plotName,rc=c(1,2), width=3.5, height=3) { ## width & height for each panel
		plotName <- paste(plotName,"wmf",sep=".")
		clearFiles(plotName)
		if(.Platform$OS.type=="windows")
			do.call("win.metafile",list(filename=plotName, width=width*rc[2], height=height*rc[1]+0.75))
		par(mfrow=rc, mar=c(2.6,2.5,1,0.5), oma=c(0,0,0,0), mgp=c(1.5,0.5,0), cex=1.2) }
	#----------------------------------Subfunctions

	unpackList(areas,scope="L")
	fnam=as.character(substitute(dat))
	expr=paste("getFile(",fnam,",senv=ioenv,use.pkg=TRUE,try.all.frames=TRUE,tenv=penv()); dat=",fnam,sep="")
	eval(parse(text=expr))

	figaro = c(eps,jpg,pdf,png,tif,wmf)
	names(figaro) = c("eps","jpg","pdf","png","tif","wmf")
	figgy = any(figaro)
	if (sum(figaro)>1) .flush.cat(paste0("WARNING: More than 1 graphics device chosen -- only '",names(figaro[figaro][1]),"' will be used\n"))
	if (!figures && !figgy) {
		lang = "e"
	} else {
		## Create a subdirectory called `french' for French-language figures
		createFdir(lang)
	}
	if (tables) {
		if (!dir.exists("./tables/"))
			dir.create("./tables/")
	}

	FLDS=names(dat)
	if (!is.element(yfld,FLDS)) showError(yfld,"nofields")
	dat$yval = dat[,yfld] # can be 'len' or 'wt'
	isLen=isWt=FALSE
	if (is.element(yfld,c("len","length"))) {names(yfld)="length"; isLen=TRUE}
	if (is.element(yfld,c("wt","weight")))  {names(yfld)="weight"; isWt =TRUE}

	attSpp=attributes(dat)$spp
	if (is.null(strSpp) || strSpp=="") {
		if (is.null(attributes(dat)$spp) || attributes(dat)$spp=="") strSpp="999"
		else strSpp=attSpp }
	else if (!is.null(attSpp) && attSpp!=strSpp)
		showError(paste("Specified strSpp '",strSpp,"' differs from species attribute '",attSpp,"' of data file",sep=""))
	if (any(is.element(c("spp","species","sppcode"),FLDS))) {
		fldSpp=intersect(c("spp","species","sppcode"),FLDS)[1]
		dat=dat[is.element(dat[,fldSpp],strSpp),] }

	dat <- dat[!is.na(dat$age) & !is.na(dat$yval) & !is.na(dat$sex),]
	## Qualify data
	z=rep(TRUE,nrow(dat))
	if (isLen) {
		#if (strSpp=="394" || strSpp=="RER") z=dat$yval>100
		#if (strSpp=="396" || strSpp=="POP") z=dat$yval>75 & dat$yval<600
		if (strSpp=="440" || strSpp=="YMR") {
			z1=dat$yval>=210 & dat$yval<=600; z2=dat$age<=90; z=z1&z2
		}
		#dat$yval <- dat$yval/10.  # change from mm to cm
	}
	if (isWt) {
		if (strSpp=="396" || strSpp=="POP") z=dat$yval<2000
		#dat$yval = dat$yval/1000. # change from g to kg
	} 
	if (!all(z==TRUE)) dat=dat[z,] # keep good, remove outliers

	aflds=c("major","minor","locality","srfa","srfs","popa","stock")
	yarea=character(0)
	if (is.null(ttype)) {
		for (a in 1:length(areas)) {
			aa = names(areas[a])
			aaa= areas[[a]]
			ttype = c(ttype, .su(dat$ttype[is.element(dat[,aa],aaa)]))
		}
		ttype = list('ttype'=.su(ttype))
	}
#browser();return()
	flds=c("ttype","stype","year") #,names(areas))
	for (i in flds) {
		expr=paste("dat=biteData(dat,",i,")",sep="")
		eval(parse(text=expr))
		if (any(i==aflds)) eval(parse(text=paste("yarea=union(yarea,",i,")"))) 
	}
	if (!is.null(ameth)) {
		dat = extractAges(dat, ameth)
	}

	if (nrow(dat)==0) {
		if (figgy) return()
		else showError("No records selected for specified qualification") }
	if (!is.null(scat)) {
		z1 = !is.element(dat$ttype,c(1,4))
		z2 = is.element(dat$ttype,c(1,4)) & is.element(dat$scat,scat)
		dat = dat[z1 | z2,]
	}

	anams = sapply(areas,function(x){paste(paste(x,collapse="|",sep=""),sep="")})
	anams = paste(names(anams),anams,sep="_")
	tnams = sapply(ttype,function(x){paste(paste(x,collapse="|",sep=""),sep="")})
	tnams = paste(names(tnams),tnams,sep="_")
	nar   = length(anams)
	ntt   = length(tnams)

	if (is.null(xlim))
		xlim <- c(0,max(dat$age,na.rm=TRUE))
	if (is.null(ylim))
		ylim <- c(0,max(dat$yval,na.rm=TRUE))

	nsex = length(sex) # lower case sex chosen by usewr for display
	SEX <- list(Females=2,Males=1,Both=1:2,Unknown=c(0,3),All=0:3)
	out <- array(NA,dim=c(length(SEX),13,ntt,nar),
		dimnames=list(names(SEX),c("n","a","b","y1","y2","t0","yinf","tstar","ystar","zstar","Ymod","Ymin","Ymax"),tnams,anams))
	names(dimnames(out)) <- c("sex","par","ttype","area")
	xout = array(NA,dim=c(2,ntt,nar,2), dimnames=list(range=c("min","max"), ttype=tnams, area=anams, year=c("year","date" )))
	plotNames = as.character()

	getFile("parVec",senv=ioenv,use.pkg=TRUE,tenv=penv())
	pVec <- parVec[["SGM"]][[names(yfld)]][[strSpp]]
	if (is.null(pVec)) pVec=parVec[["SGM"]][[names(yfld)]][["POP"]] # default
	#if (strSpp%in%c("394","425") && yfld=="len") {pVec["y1","max"] = 30; pVec["y2","min"] = 31; pVec["y2","max"] = 70}
	if (yfld=="len") {pVec["y1","max"] = 30; pVec["y2","min"] = 31; pVec["y2","max"] = 70}
	## Cannot really fix t0 as it's not a parameter
#browser();return()

	## Labels & names --------------------
	aName=paste("-areas(",paste(anams,collapse="~"),")",sep="")      # area label
	tName=paste("-tt(",paste(tnams,collapse="~"),")",sep="")         # trip type label
	pName=paste("Age",ifelse(isLen,"Len","Wt"),sep="")             # property label
	yName=ifelse(is.null(year),"",paste("-(",paste(unique(range(year)),collapse="-"),")",sep=""))  # year label
	fName=paste("fits",gsub("[()]","",gsub("-","_",yName)),sep="")  # fits label
	if (missing(outnam)){
		plotName = paste0(pName,strSpp,aName,tName,yName,"-SG-data(",fnam,")")
	}
	else plotName = outnam
	plotName = tabName = gsub("\\|","",plotName)
	csv = paste0("./tables/",tabName,"-fits",".csv")
	adm = paste0("./tables/",tabName,"-fits",".dat")
	if (tables)  clearFiles(c(csv,adm))
	#------------------------------------
	DATA = list()

	pfout.e = plotName
	for (l in lang) {
		#if (l=="f") pfout = paste0("./french/",pfout.e)  ## could repeat for other languages
		pfout = switch(l, 'e' = paste0("./english/",pfout.e), 'f' = paste0("./french/",pfout.e) )
		if (!figgy && figures) { resetGraph()
			expandGraph(mfrow=c(length(ttype),nsex),mar=c(3,3,2,.1),oma=c(0,0,0,0),mgp=c(1.5,.5,0),cex=1.0) }
		else if ((!singles&!pages) && figgy && figures) {
			plotNames = c(plotNames,plotName)
			if (eps)      createEPS(pfout,rc=c(length(anams)*length(tnams),nsex))
			else if (jpg) createJPG(pfout,rc=c(length(anams)*length(tnams),nsex))
			else if (pdf) createPDF(pfout,rc=c(length(anams)*length(tnams),nsex))
			else if (png) createPNG(pfout,rc=c(length(anams)*length(tnams),nsex))
			else if (tif) createTIF(pfout,rc=c(length(anams)*length(tnams),nsex))
			else if (wmf) createWMF(pfout,rc=c(length(anams)*length(tnams),nsex))
		}

		fits=list()
		SGFITS = ttcall(SGFITS)
		if (is.null(SGFITS)) SGFITS = list()
		if (!is.null(year)) yrlim = range(year)
		else if (is.element("year",names(dat))) yrlim = range(dat$year,na.rm=TRUE)
		else if (is.element("date",names(dat))) yrlim = range(is.numeric(substring(dat$date,1,4)),na.rm=TRUE)
		else yrlim = rep(as.numeric(substring(Sys.time(),1,4)),2)
		if (tables && l=="e") {
			cat(paste("Fits for",plotName),"\n",file=csv)
			mess=c("# Parameters for ",plotName,"\n\n# number of years\n",diff(yrlim)+1,
				"\n# first year of period when SG parameters are valid\n",yrlim[1],
				"\n# last year for parameters\n",yrlim[2],"\n\n")
			cat(paste(mess,collapse=""),file=adm)
		}

		for (a1 in 1:length(areas)) {
			an = names(areas)[a1]; ar = areas[[a1]]
			adat <- dat[is.element(dat[,an],ar),]
			if (nrow(adat)==0) next
			amat = paste(an,paste0(ar,collapse="|"),sep="_")

			if (missing(outnam)) {
				aaName <- paste0(pName,strSpp,"-area(",amat,")",tName,yName,"-SG-data(",fnam,")")
				aaName = gsub("\\|","",aaName)
			} else
				aaName = plotName
			afout.e = aaName
			#if (l=="f") afout = paste0("./french/",afout.e)  ## could repeat for other languages
			afout = switch(l, 'e' = paste0("./english/",afout.e), 'f' = paste0("./french/",afout.e) )

			if (pages && figgy && figures){
				plotNames = c(plotNames,aaName)
				if (eps)      createEPS(afout,rc=c(length(tnams),nsex))
				else if (jpg) createJPG(afout,rc=c(length(tnams),nsex))
				else if (pdf) createPDF(afout,rc=c(length(tnams),nsex))
				else if (png) createPNG(afout,rc=c(length(tnams),nsex))
				else if (tif) createTIF(afout,rc=c(length(tnams),nsex))
				else if (wmf) createWMF(afout,rc=c(length(tnams),nsex))
			}
			for (t1 in 1:length(ttype)) {
				tt   = ttype[[t1]]; ttt = names(ttype)[t1]
				tdat <- adat[is.element(adat$ttype,tt),]
				if (nrow(tdat)==0) next
				tmat = paste(ttt,paste0(tt,collapse="|"),sep="_")
				amet = paste0(ameth,collapse="")
				if (missing(outnam)) {
					ttName <- paste0(pName,strSpp,"-area(",amat,")-tt(",tmat,")-am(",amet,")",yName,"-SG-data(",fnam,")")
					ttName = gsub("\\|","",ttName)
				} else
					ttName = plotName
				tfout.e = ttName
				#if (l=="f") tfout = paste0("./french/",tfout.e)  ## could repeat for other languages
				tfout = switch(l, 'e' = paste0("./english/",tfout.e), 'f' = paste0("./french/",tfout.e) )
#browser();return()

				xout[,tmat,amat,"year"] = range(tdat$year,na.rm=TRUE)
				if ("date" %in% names(tdat))
					xout[,tmat,amat,"date"] = range(substring(tdat$date,1,10),na.rm=TRUE)

				if (singles && figgy && figures) {
					plotNames = c(plotNames,ttName)
					if (eps)      createEPS(tfout,rc=c(1,nsex))
					else if (jpg) createJPG(tfout,rc=c(1,nsex))
					else if (pdf) createPDF(tfout,rc=c(1,nsex))
					else if (png) createPNG(tfout,rc=c(1,nsex))
					else if (tif) createTIF(tfout,rc=c(1,nsex))
					else if (wmf) createWMF(tfout,rc=c(1,nsex))
				}
				for (i in 1:length(SEX)) {
					ii   <- SEX[[i]]; iii <- names(SEX)[i]
					idat = SGdat =  tdat[is.element(tdat$sex,ii),]
					if (nrow(idat)==0) next
					DATA[[ttt]][[iii]] = idat  ## presumably overwrites if more than one language loop
					n1 = n0 = nrow(idat); ## keep track of initial number (n0) in case residuals are removed
					if (n1==0) {
						out[iii,"n",tmat,amat] = 0
						if (any(iii==c("All","Unknown"))) next 
						else if (figures){
							plot(0, 0, type="n", axes=FALSE, xlab="", ylab="")
							addLabel(0.5, 0.5, linguaFranca("NO DATA",l)); next
						}
					}
					if (n1==1) {
						out[iii,c("n","Ymod","Ymin","Ymax"),tmat,amat] = c(1,rep(idat$yval,3))
						if (any(iii==c("All","Unknown"))) next 
						else if (figures){
							points(idat$age,idat$yval,pch=20,bg=.colBlind["orange"],cex=0.8); next
						}
					}
					if (n1>1) {
						yval <- idat$yval; age <- idat$age
						ttput(SGdat)  #assign("SGdat",idat,envir=.PBStoolEnv)
						here = lenv()
						## make.pVec only rlevant to vonB
						make.parVec =function(xdat,xVec) { ## only affects row 1 of parVec, sometimes row 3
							xy <- hist(xdat$yval, nclass=20, plot=FALSE);
							Ymod <- xy$breaks[2:length(xy$breaks)][is.element(xy$density,max(xy$density))]
							Ymod <- mean(Ymod)
							Ymin <- min(xdat$yval,na.rm=TRUE)
							Ymax <- max(xdat$yval,na.rm=TRUE)
							xVec[ifelse(isLen,"Linf","Winf"),1:3] <- c(Ymod,Ymin,1.2*Ymax)
							if (fixt0) xVec[3,4]=FALSE
							tput(Ymod,tenv=here); tput(Ymin,tenv=here); tput(Ymax,tenv=here); 
							return(xVec)
						}
						junk  = make.parVec(idat,pVec)  ## just to get Ymod etc.
						ipVec = pVec 
						assign("SG",list(tau=tau,is.pred=FALSE,xpred=NULL),envir=.PBStoolEnv)
						calcMin(pvec=ipVec,func=SGfun)
						if (!is.null(rm.studs) && is.numeric(rm.studs)) {
							if (length(rm.studs)==1) rm.studs = rep(rm.studs,2) * c(-1,1)
							rm.studs = sort(rm.studs)
							P <- tcall(PBSmin)$end
							a = P[1];  b = P[2];  y1 = P[3];  y2 = P[4]
							ttget(SGdat)
							TL =  SGdat$yval
							Age = SGdat$age
							pars = list(a=a, b=b, y1=y1, y2=y2)                      ## Vector of initial values (sans sig)
							TLpred = ttcall(aModel)(P=c(a,b,y1,y2),xobs=Age,tau=tau)
							enough.points   = nrow(SGdat) >= 10
							enough.contrast = diff(range(TLpred)) > 2
#if (i==2) {browser();return()}
							if (enough.points && enough.contrast) {  ## nls cannot fit when calculation has no contrast
								#bModel = function(x, pars, tau=tau) {
								#	#P=coef[1:4]; tau=coef[5:6] #; is.pred=as.logical(coef[7])
								#	a = pars[[1]]; b=pars[[2]]; y1=pars[[3]]; y2=pars[[4]]
								#	junk=ttcall(aModel)(P=c(a,b,y1,y2), x, tau); browser();return()}
								#TLeps <- TL + rnorm(length(TL), sd = 1) 
								#nls(TL ~ bModel(Age,pars,tau=tau), start = pars) #list(coef = c(a,b,y1,y2,t1=tau[1],t2=tau[2])))
								#fit1 <- try(nls(TL~ttcall(aModel)(P=c(a,b,y1,y2),xobs=Age,tau=tau),start=pars),silent=TRUE)  ## Use nls for residual checking
								#if (class(fit1)[1] != "try-error") {
								#	res.norm = residuals(fit1)
									## http://www.mathworks.com/matlabcentral/newsreader/view_thread/330668
								res.norm = TL - TLpred
									r = matrix(res.norm,ncol=1)
									h = matrix(hat(Age),ncol=1) # equiv: (X-mean(X))^2/sum((X-mean(X))^2) + 1/length(X)
									MSE = as.vector((t(r)%*%r)/(length(Age)-length(pars))) # 3 parameters estimated
									res.stud = r/(sqrt(MSE*(1-h)))
									#resetGraph(); plot(res.stud)
									keep = res.stud >= rm.studs[1] & res.stud <= rm.studs[2]
									SGdat0 = SGdat; n0=n1; ag0=age; yval0=yval #; fit0=fit1
									idat = SGdat = idat[keep,]
									yval = idat$yval; age = idat$age
									#xlim <- c(0,max(idat$age,na.rm=TRUE))
									#ylim <- c(0,max(idat$yval,na.rm=TRUE))
									ttput(SGdat)
									DATA[[ttt]][[iii]] = idat
									n1   = nrow(idat)
									junk  = make.parVec(idat,pVec)  ## just to get Ymod etc.
									ipVec$val=c(a,b,y1,y2)
									calcMin(pvec=ipVec,func=SGfun)
								#} ## end if fit1 bad
							} ## end if enough points
						} ## end if rm.studs
						tget(PBSmin)  # located in .PBSmodEnv
						fmin <- PBSmin$fmin; np <- sum(pVec[,4]); ng <- nrow(idat);
						AICc <- 2*fmin + 2*np * (ng/(ng-np-1)); packList("AICc","PBSmin",tenv=.PBSmodEnv);  #print(PBSmin)
						P <- PBSmin$end

						Pcalc <- calcP(P,tau); # t0, yinf, tstar, ystar, zstar (Eqns 24-28, Schnute 1981)
						unpackList(Pcalc)
						a  = P[1]; par1 = format(signif(a,4),big.mark=options()$big.mark,scientific=FALSE)
						b  = P[2]; par2 = format(signif(b,4),big.mark=options()$big.mark,scientific=FALSE)
						y1 = P[3]; par3 = format(signif(y1,3),big.mark=options()$big.mark,scientific=FALSE)
						y2 = P[4]; par4 = format(signif(y2,3),big.mark=options()$big.mark,scientific=FALSE)
						par5 = format(signif(t0,3),big.mark=options()$big.mark,scientific=FALSE)
						par6 = format(signif(yinf,3),big.mark=options()$big.mark,scientific=FALSE)
						ovec <- c(n=n1,a,b,y1,y2,t0=t0,yinf=yinf,tstar=tstar,ystar=ystar,zstar=zstar,Ymod=Ymod,Ymin=Ymin,Ymax=Ymax)
						out[iii,,tmat,amat] <- ovec

						# Plot jittered data and add SGfun fit line
						if (any(iii==c("Females","Males","Both"))) {
							subtit = paste(amat,", ",tmat,sub("-"," ",yName),sep="")
							subtit = gsub("\\|","+",gsub("_",":",subtit))
							subtit = gsub("3\\+4\\+5\\+6\\+7\\+8\\+9",texThatVec(ar),subtit)
							subtit = gsub("1\\+2\\+3\\+4\\+5",paste0("tt_",texThatVec(tt)),subtit)

							xpred = seq(max(t0,0),xlim[2],length.out=100)
							ypred = ttcall(aModel)(P=c(a,b,y1,y2),xobs=xpred,tau=tau)
							#print(amat);print(tmat);print(iii)
							fits[[amat]][[tmat]][[iii]] = list(xpred=xpred,ypred=ypred)

							if (any(iii==names(sex)) && figures) {
								plot(0,0, type="n", xlab=linguaFranca("Age",l), ylab=ifelse(isLen, linguaFranca("Length (cm)",l), linguaFranca("Weight (kg)",l)), main=linguaFranca(iii,l), xlim=xlim, ylim=ylim, bty="l",cex.main=ifelse(figgy,1,1.5),cex.lab=ifelse(figgy,1,1.5))
								#abline(h=seq(0,ylim[2],ifelse(ylim[2]<=10,1,5)),v=seq(0,xlim[2],ifelse(xlim[2]<=15,1,ifelse(xlim[2]<=50,5,10))),col="grey90",lwd=0.5)
								abline(h=pretty(ylim,n=10), v=pretty(xlim,n=10), col="grey90", lwd=0.5)
								points(jitter(age,jit[1]),jitter(yval,jit[2]),pch=convUTF("\\u{25AA}"),col=.colBlind["orange"],cex=ifelse(figgy,0.7,1.2))
								lines(xpred,ypred,col=.colBlind["blue"], lwd=ifelse(figgy,2,3))
								mw <- format(round(mean(idat$wt,na.rm=TRUE),1),nsmall=1)
								posX=ifelse(figgy,0.6,0.5); posY=ifelse(figgy,0.40,0.30); difY=ifelse(figgy,0.05,0.025); 
								cexlab=ifelse(figgy,0.7,1.2)
							addLabel(posX,posY-difY*0,bquote(bolditalic(n)==.(format(n1,big.mark=options()$big.mark))),adj=0,cex=cexlab)
							addLabel(posX,posY-difY*1,bquote(bolditalic(a) == .(par1)),adj=0,cex=cexlab)
							addLabel(posX,posY-difY*2,bquote(bolditalic(b)==.(par2)),adj=0,cex=cexlab)
							addLabel(posX,posY-difY*3,bquote(bolditalic(y)[1]==.(par3)),adj=0,cex=cexlab)
							addLabel(posX,posY-difY*4,bquote(bolditalic(y)[2]==.(par4)),adj=0,cex=cexlab)
							addLabel(posX,posY-difY*5,bquote(bolditalic(t)[0]==.(par5)),adj=0,cex=cexlab)
							addLabel(posX,posY-difY*6,bquote(bolditalic(y)[infinity]==.(par6)),adj=0,cex=cexlab)
								if (!is.null(rm.studs) && is.numeric(rm.studs))
									addLabel(posX,posY-difY*7,bquote(italic(.(linguaFranca("removed",l)))==.(format(n0-n1,big.mark=options()$big.mark))),adj=0,cex=cexlab)
								#if (iii=="Males") {
								if (i==floor(median(1:nsex))) {
									subtit2 = gsub("ttype14","commercial",gsub("ttype23","research/survey",subtit))
									subtit2 = gsub("major3456789","coastwide",gsub("major89","5DE",subtit2))
									subtit2 = gsub("major567","5ABC",gsub("major34","3CD",subtit2))
									addLabel(.5,.96,subtit2,adj=c(.5,0),cex=cexlab+0.1)
								}
								box(bty="l")
							}
						} ## end if any femals, males, both
					} ## end if n1>1
				} ## end i (nsex) loop
				if (singles && figgy && figures) dev.off()
				if (tables && l=="e") {
					cat(paste("\nArea: ",amat,"   Trip type: ",tmat,sep=""),"\n",file=csv,append=TRUE)
					cat("age,females,males,both","\n",file=csv,append=TRUE)
					if (is.null(fits[[amat]][[tmat]])) next
					outsex = names(fits[[amat]][[tmat]])
					for (i in outsex) {
						if (i==outsex[1])
							mf = sapply(fits[[amat]][[tmat]][[i]],function(x){x},simplify=TRUE)
						else
							mf = cbind(mf, sapply(fits[[amat]][[tmat]][[i]],function(x){x},simplify=TRUE)[,2])
					}
					write.table(mf,file=csv,append=TRUE,sep=",",row.names=FALSE,col.names=FALSE)
					cat(paste("# Area: ",amat,"   Trip type: ",tmat,sep=""),"\n",file=adm,append=TRUE)
					mess=c("# Schnute growth parameters (for F  M  F+M)\n")
					cat(paste(mess,collapse=""),file=adm,append=TRUE)
					mess=c("sex,n,a,b,y1,y2,t0,Linf,t*,y*,z*\n")
					cat(paste(mess,collapse=""),file=adm,append=TRUE)
					mess = paste0(paste(outsex,apply(out[outsex,1:10,tmat,amat],1,paste0,collapse=","),sep=","),collapse="\n")
					cat(mess,file=adm,append=TRUE)
					cat("\n\n",file=adm,append=TRUE) 
				}
			}
			if (pages && figgy && figures) dev.off()
		}
		if ((!singles&!pages) && figgy && figures) dev.off()
	} ## end l (lang) loop
	attr(out,"xout") = xout
	if (figgy) attr(out,"plotNames") = plotNames
	assign(fName,fits,envir=.PBStoolEnv)
	fTag = sapply(strsplit(tabName,split="-"),function(x){paste0(x[-1],collapse="-")})
#browser();return()
	ttget(SGFITS); SGFITS[[fTag]] = out[names(sex),,tmat,amat]; ttput(SGFITS)
	ttput(DATA);
	stuff=c("out","pVec","xlim","ylim","anams","tnams","aName","tName","aaName","ttName","strSpp","fits","DATA")
	packList(stuff,"SG",tenv=.PBStoolEnv)
	omess = c( 
		paste("assign(\"SG",strSpp,"\",out); ",sep=""),
		paste("save(\"SG",strSpp,"\",file=\"SG",strSpp,"-(", fTag, ").rda\")",sep="")
	)
	eval(parse(text=paste(omess,collapse="")))
	invisible(out)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~calcSG


## calcVB-------------------------------2024-11-19
## Calculate von Bertalanffy growth curve.
## Note: ameth=3 otoliths broken & burnt; ameth=1 surface read only
##   areas: list of lists
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Steve Wischniowski: "there can be surface ages in combination with the typical 
## Break and Burn procedure. The SCL will surface age any rock/flat fish that is 
## 3 years and under. This is not new and has been an accepted procedure for longer
## than I've been here. For fish under 3 the break and burn technique can over estimate age, 
## so we find that a surface ageing 3 and under has better accuracy and precision rates.
## ---------------------------------------------RH
calcVB <- function(dat=pop.age, strSpp="", yfld="len", fixt0=FALSE, 
   areas=list(major=NULL, minor=NULL, locality=NULL, srfa=NULL,srfs=NULL, popa=NULL),
   ttype=list(commercial=c(1,4,5),research=c(2,3)), stype=c(1,2,6,7,17), scat=NULL,
   sex=list(Females=2,Males=1), rm.studs=NULL, 
   year=NULL, xlim=NULL, ylim=NULL, ameth=c(2,3,17), jit=c(0,0), 
   eps=FALSE, jpg=FALSE, pdf=FALSE, png=FALSE, tif=FALSE, wmf=FALSE,
   outnam,  subtitle, singles=FALSE, pages=FALSE, tables=TRUE, figures=TRUE,
   ioenv=.GlobalEnv, lang=c("e","f"))
{
	assign("VB",list(module="M02_Biology",call=match.call(),args=args(calcVB),ioenv=ioenv),envir=.PBStoolEnv)
	scipen = options()$scipen
	options(scipen=10)
	on.exit(options(scipen=scipen))

	## Start Subfunctions----------------------------------
	VBfun <- function(P) {
		Yinf <- P[1]; K <- P[2]; t0 <- P[3]; sig=P[4]
		ttget(VBdat)
		obs  <- VBdat$yval;
		pred <- Yinf * (1 - exp(-K*(VBdat$age-t0)) );
		n1   <- length(obs);
		ssq  <- sum( (obs-pred)^2 );
		fval <- n1*log(sig) + ssq/(2.0*sig*sig);  ## negative log likelihood
		#return(n1*log(ssq)); };
		return(fval);
	}
	msg = function(x,flag="data"){
		nx = nrow(x)
		.flush.cat("\t",paste0(flag,": ",nx," records"),"\n")
	}

	## Device set-ups should match those in `calcLW'
	## ---------------------------------------------
	## Setup the EPS device for import to word, half page with 2 plots side by side
	createEPS <- function(plotName,rc=c(2,2), width=3.5, height=3) { ## width & height for each panel
		plotName <- paste(plotName,"eps",sep=".")
		clearFiles(plotName)
		postscript(plotName, width=width*rc[2], height=height*rc[1]+0.75, paper="special", horizontal=FALSE, family="NimbusSan")
		par(mfrow=rc, mar=c(2.6,2.5,1,0.5), oma=c(0,0,0,0), mgp=c(1.5,0.5,0), cex=1.2) }

	## Setup the JPG device for import to word, half page with 2 plots side by side
	createJPG <- function(plotName,rc=c(2,2), width=3.5, height=3) { ## width & height for each panel
		plotName <- paste(plotName,"jpg",sep=".")
		clearFiles(plotName)
		jpeg(plotName, quality=100, res=400, width=width*rc[2]*400, height=(height*rc[1]+0.75)*400, pointsize=12)
		par(mfrow=rc, mar=c(2.6,2.5,1,0.5), oma=c(0,0,0,0), mgp=c(1.5,0.5,0), cex=1.2) }

	## Setup the PDF device for import to word, half page with 2 plots side by side
	createPDF <- function(plotName,rc=c(2,2), width=3.5, height=3) { ## width & height for each panel
		plotName <- paste(plotName,"pdf",sep=".")
		clearFiles(plotName)
		pdf(plotName, width=width*rc[2], height=height*rc[1]+0.75, paper="special")
		par(mfrow=rc, mar=c(2.6,2.5,1,0.5), oma=c(0,0,0,0), mgp=c(1.5,0.5,0), cex=1.2) }

	## Setup the PNG device for import to word, half page with 2 plots side by side
	createPNG <- function(plotName,rc=c(2,2), width=3.5, height=3) { ## width & height for each panel
		plotName <- paste(plotName,"png",sep=".")
		clearFiles(plotName)
		png(plotName, units="in", res=400, width=width*rc[2], height=(height*rc[1]+0.75), pointsize=12)
		par(mfrow=rc, mar=c(2.6,2.5,1,0.5), oma=c(0,0,0,0), mgp=c(1.5,0.5,0), cex=1.2) }

	## Setup the TIFF device for import to word, half page with 2 plots side by side
	createTIF <- function(plotName,rc=c(2,2), width=3.5, height=3) { ## width & height for each panel
		plotName <- paste(plotName,"tif",sep=".")
		clearFiles(plotName)
		tiff(plotName, units="in", res=400, width=width*rc[2], height=(height*rc[1]+0.75), pointsize=12)
		par(mfrow=rc, mar=c(2.6,2.5,1,0.5), oma=c(0,0,0,0), mgp=c(1.5,0.5,0), cex=1.2) }

	## Setup the WMF device for import to word, half page with 2 plots side by side
	createWMF <- function(plotName,rc=c(1,2), width=3.5, height=3) { ## width & height for each panel
		plotName <- paste(plotName,"wmf",sep=".")
		clearFiles(plotName)
		if(.Platform$OS.type=="windows")
			do.call("win.metafile",list(filename=plotName, width=width*rc[2], height=height*rc[1]+0.75))
		par(mfrow=rc, mar=c(2.6,2.5,1,0.5), oma=c(0,0,0,0), mgp=c(1.5,0.5,0), cex=1.2) }
	##----------------------------------End Subfunctions

	unpackList(areas,scope="L")
	fnam=as.character(substitute(dat))
	expr=paste("getFile(",fnam,",senv=ioenv,use.pkg=TRUE,try.all.frames=TRUE,tenv=penv()); dat=",fnam,sep="")
	eval(parse(text=expr))

	figaro = c(eps,jpg,pdf,png,tif,wmf)
	names(figaro) = c("eps","jpg","pdf","png","tif","wmf")
	figgy = any(figaro)
	if (sum(figaro)>1) .flush.cat(paste0("WARNING: More than 1 graphics device chosen -- only '",names(figaro[figaro][1]),"' will be used\n"))
	if (!figures && !figgy) {
		lang = "e"
	} else {
		## Create a subdirectory called `french' for French-language figures
		createFdir(lang)
	}
	if (tables) {
		if (!dir.exists("./tables/"))
			dir.create("./tables/")
	}

	FLDS=names(dat)
	if (!is.element(yfld,FLDS)) showError(yfld,"nofields")
	dat$yval = dat[,yfld] # can be 'len' or 'wt'
	isLen = isWt = FALSE
	if (is.element(yfld,c("len","length"))) {names(yfld)="length"; isLen=TRUE}
	if (is.element(yfld,c("wt","weight")))  {names(yfld)="weight"; isWt =TRUE}

	.flush.cat(paste0("Qualifying ",fnam,":"),"\n")

	attSpp=attributes(dat)$spp
	if (is.null(strSpp) || strSpp=="") {
		if (is.null(attributes(dat)$spp) || attributes(dat)$spp=="") strSpp="999"
		else strSpp=attSpp
	} else if (!is.null(attSpp) && attSpp!=strSpp)
		showError(paste("Specified strSpp '",strSpp,"' differs from species attribute '",attSpp,"' of data file",sep=""))
	if (any(is.element(c("spp","species","sppcode"),FLDS)))
	{
		fldSpp=intersect(c("spp","species","sppcode"),FLDS)[1]
		dat=dat[is.element(dat[,fldSpp],strSpp),]
		msg(dat,"strSpp")
	}

	dat <- dat[!is.na(dat$age) & !is.na(dat$yval) & !is.na(dat$sex),]
	msg(dat,"remove NAs")

	## Qualify data
	z=rep(TRUE,nrow(dat))
	if (isLen) {
		#if (strSpp=="394" || strSpp=="RER") z=dat$yval>100
		#if (strSpp=="396" || strSpp=="POP") z=dat$yval>75 & dat$yval<600
		if (strSpp=="440" || strSpp=="YMR") {
				z1=dat$yval>=210 & dat$yval<=600; z2=dat$age<=90; z=!(z1&z2)
		}
		#dat$yval <- dat$yval/10.  # change from mm to cm
	}
	if (isWt) {
		if (strSpp=="396" || strSpp=="POP") z=dat$yval<2000
		#dat$yval = dat$yval/1000. # change from g to kg
	} 
	if (!all(z==TRUE)){
		dat=dat[z,] # keep good, remove outliers
		msg(dat,"remove bad data")
	}

	aflds=c("major","minor","locality","srfa","srfs","popa","stock")
	yarea=character(0)
	if (is.null(ttype)) {
		for (a in 1:length(areas)) {
			aa = names(areas[a])
			aaa= areas[[a]]
			ttype = c(ttype, .su(dat$ttype[is.element(dat[,aa],aaa)]))
		}
		ttype = list('ttype'=.su(ttype))
	}
	flds=c("ttype","stype","year") #,names(areas))
	for (i in flds) {
		expr=paste("dat=biteData(dat,",i,")",sep="")
		eval(parse(text=expr))
		msg(dat,i)
		if (any(i==aflds)) eval(parse(text=paste("yarea=union(yarea,",i,")"))) 
	}
	if (!is.null(ameth)) {
		dat = extractAges(dat, ameth)
		msg(dat,"ameth")
	}
	if (nrow(dat)==0) {
		if (figgy) return()
		else showError("No records selected for specified qualification") }
	if (!is.null(scat)) {
		z1 = !is.element(dat$ttype,c(1,4:10))
		z2 = is.element(dat$ttype,c(1,4:10)) & is.element(dat$scat,scat)
		dat = dat[z1 | z2,]
		msg(dat,"scat")
	}
#browser();return()
	anams = sapply(areas,function(x){paste(paste(x,collapse="|",sep=""),sep="")})
	anams = paste(names(anams),anams,sep="_")
	tnams = sapply(ttype,function(x){paste(paste(x,collapse="|",sep=""),sep="")})
	tnams = paste(names(tnams),tnams,sep="_")
	nar   = length(anams)
	ntt   = length(tnams)

	if (is.null(xlim))
		xlim <- c(0,max(dat$age,na.rm=TRUE))
	if (is.null(ylim))
		ylim <- c(0,max(dat$yval,na.rm=TRUE))

	nsex = length(sex) # lower case sex chosen by usewr for display
	SEX <- list(Females=2,Males=1,Both=1:2,Unknown=c(0,3),All=0:3)
	out <- array(NA,dim=c(length(SEX),8,ntt,nar),
		dimnames=list(names(SEX),c("n","Yinf","K","t0","sig","Ymod","Ymin","Ymax"),tnams,anams))
	names(dimnames(out)) <- c("sex","par","ttype","area")
	xout = array(NA,dim=c(2,ntt,nar,2), dimnames=list(range=c("min","max"), ttype=tnams, area=anams, year=c("year","date" )))
	plotNames = as.character()

	getFile("parVec",senv=ioenv,use.pkg=TRUE,tenv=penv())
	pVec <- parVec[["vonB"]][[names(yfld)]][[strSpp]]
	if (is.null(pVec)) pVec=parVec[["vonB"]][[names(yfld)]][["POP"]] # default
	pVec = rbind(pVec,sig=data.frame(val=1,min=0,max=5,active=TRUE)) ## adding error component
	if (strSpp=="228" && yfld=="len") {pVec[1,"max"] = 80; pVec["sig","max"] = 10}
	if (strSpp=="439" && yfld=="len") {pVec[1,"max"] = 100; pVec["sig","max"] = 10; pVec["t0","min"] = -5}
	if (strSpp=="417" && yfld=="len") {pVec[1,"max"] = 100; pVec["sig","max"] = 10; pVec["t0","min"] = -5}
	if (strSpp%in%c("394","425") && yfld=="len") {pVec[1,"max"] = 80; pVec["sig","max"] = 10; pVec["t0","min"] = -5}
	if (strSpp%in%c("435") && yfld=="len") {pVec[1,"max"] = 100; pVec["sig","max"] = 10; pVec["t0","min"] = -5}
	if (strSpp%in%c("396","418","405") && yfld=="len") { pVec["t0","min"] = -3; pVec["t0","max"] = 3 }
	if (strSpp%in%c("405") && yfld=="len") { pVec["Linf","val"] = 45; pVec["t0","min"] = -0.5 }
	if (fixt0)  { pVec["t0","active"] = FALSE }
#browser();return()

	## Labels & names --------------------
	aName=paste("-areas(",paste(anams,collapse="~"),")",sep="")      # area label
	tName=paste("-tt(",paste(tnams,collapse="~"),")",sep="")         # trip type label
	pName=paste("Age",ifelse(isLen,"Len","Wt"),sep="")             # property label
	yName=ifelse(is.null(year),"",paste("-(",paste(unique(range(year)),collapse="-"),")",sep=""))  # year label
	fName=paste("fits",gsub("[()]","",gsub("-","_",yName)),sep="")  # fits label
	if (missing(outnam)){
		plotName = paste0(pName,strSpp,aName,tName,yName,"-VB-data(",fnam,")")
	}
	else plotName = outnam
	plotName = tabName = gsub("\\|","",plotName)
	if (fixt0)
		plotName = tabName = paste0(plotName,"-fixt0")
	csv = paste0("./tables/",tabName,"-fits",".csv")
	adm = paste0("./tables/",tabName,"-fits",".dat")
	if (tables)  clearFiles(c(csv,adm))
	#------------------------------------

	DATA = list()
	pfout.e = plotName
#browser();return()
	for (l in lang) {  ## could switch to other languages if available in 'linguaFranca'.
		changeLangOpts(L=l)
		#pfout = switch(l, 'e' = pfout.e, 'f' = paste0("./french/",pfout.e) )
		pfout = switch(l, 'e' = paste0("./english/",pfout.e), 'f' = paste0("./french/",pfout.e) )
		if (!figgy && figures) { resetGraph()
			expandGraph(mfrow=c(length(ttype),nsex),mar=c(3,3,2,.1),oma=c(0,0,0,0),mgp=c(1.5,.5,0),cex=1.0) }
		else if ((!singles&!pages) && figgy && figures) {
			plotNames = c(plotNames,plotName)
			if (eps)      createEPS(pfout,rc=c(length(anams)*length(tnams),nsex))
			else if (jpg) createJPG(pfout,rc=c(length(anams)*length(tnams),nsex))
			else if (pdf) createPDF(pfout,rc=c(length(anams)*length(tnams),nsex))
			else if (png) createPNG(pfout,rc=c(length(anams)*length(tnams),nsex))
			else if (tif) createTIF(pfout,rc=c(length(anams)*length(tnams),nsex))
			else if (wmf) createWMF(pfout,rc=c(length(anams)*length(tnams),nsex))
		}

		fits = list()
		VBFITS = ttcall(VBFITS)
		if (is.null(VBFITS)) VBFITS = list()
		if (!is.null(year)) yrlim = range(year)
		else if (is.element("year",names(dat))) yrlim = range(dat$year,na.rm=TRUE)
		else if (is.element("date",names(dat))) yrlim = range(is.numeric(substring(dat$date,1,4)),na.rm=TRUE)
		else yrlim = rep(as.numeric(substring(Sys.time(),1,4)),2)
		if (tables && l=="e") {
			cat(paste("Fits for",plotName),"\n",file=csv)
			mess=c("# Parameters for ",plotName,"\n\n# number of years\n",diff(yrlim)+1,
				"\n# first year of period when vonB parameters are valid\n",yrlim[1],
				"\n# last year for parameters\n",yrlim[2],"\n\n")
			cat(paste(mess,collapse=""),file=adm)
		}
		

		for (a1 in 1:length(areas)) {
			an = names(areas)[a1]; ar = areas[[a1]]
			adat <- dat[is.element(dat[,an],ar),]
			if (nrow(adat)==0) next
			amat = paste(an,paste0(ar,collapse="|"),sep="_")
			if (missing(outnam)) {
				aaName <- paste0(pName,strSpp,"-area(",amat,")",tName,yName,"-VB-data(",fnam,")")
				aaName = gsub("\\|","",aaName)
			} else
				aaName = plotName
			afout.e = aaName
			#if (l=="f") afout = paste0("./french/",afout.e)  ## could repeat for other languages
			afout = switch(l, 'e' = paste0("./english/",afout.e), 'f' = paste0("./french/",afout.e) )

			if (pages && figgy && figures){
				plotNames = c(plotNames,aaName)
				if (eps)      createEPS(afout,rc=c(length(tnams),nsex))
				else if (jpg) createJPG(afout,rc=c(length(tnams),nsex))
				else if (pdf) createPDF(afout,rc=c(length(tnams),nsex))
				else if (png) createPNG(afout,rc=c(length(tnams),nsex))
				else if (tif) createTIF(afout,rc=c(length(tnams),nsex))
				else if (wmf) createWMF(afout,rc=c(length(tnams),nsex))
			}
			for (t1 in 1:length(ttype)) {
				tt   = ttype[[t1]]; ttt = names(ttype)[t1]
				tdat <- adat[is.element(adat$ttype,tt),]
				if (nrow(tdat)==0) next
				tmat = paste(ttt,paste0(tt,collapse="|"),sep="_")
				amet = paste0(ameth,collapse="")
				if (missing(outnam)) {
					ttName <- paste0(pName,strSpp,"-area(",amat,")-tt(",tmat,")-am(",amet,")",yName,"-VB-data(",fnam,")")
					ttName = gsub("\\|","",ttName)
				} else
					ttName = plotName
				tfout.e = ttName
				#if (l=="f") tfout = paste0("./french/",tfout.e)  ## could repeat for other languages
				tfout = switch(l, 'e' = paste0("./english/",tfout.e), 'f' = paste0("./french/",tfout.e) )
#browser();return()

				xout[,tmat,amat,"year"] = range(tdat$year,na.rm=TRUE)
				if ("date" %in% names(tdat))
					xout[,tmat,amat,"date"] = range(substring(tdat$date,1,10),na.rm=TRUE)
				if (singles && figgy && figures) {
					plotNames = c(plotNames,ttName)
#browser();return()
					if (eps)      createEPS(tfout,rc=c(1,nsex))
					else if (jpg) createJPG(tfout,rc=c(1,nsex))
					else if (pdf) createPDF(tfout,rc=c(1,nsex))
					else if (png) createPNG(tfout,rc=c(1,nsex))
					else if (tif) createTIF(tfout,rc=c(1,nsex))
					else if (wmf) createWMF(tfout,rc=c(1,nsex))
				}
				for (i in 1:length(SEX)) {
					ii   <- SEX[[i]]; iii <- names(SEX)[i]
					idat = VBdat =  tdat[is.element(tdat$sex,ii),]
					if (nrow(idat)==0) next
					DATA[[ttt]][[iii]] = idat  ## presumably overwrites if more than one language loop
					n1 = n0 = nrow(idat); ## keep track of initial number (n0) in case residuals are removed
					if (n1==0) {
						out[iii,"n",tmat,amat] = 0
						if (any(iii==c("All","Unknown"))) next 
						else if (figures){
							plot(0, 0, type="n", axes=FALSE, xlab="", ylab="")
							addLabel(0.5, 0.5, linguaFranca("NO DATA",l)); next
						}
					}
					if (n1==1) {
						out[iii,c("n","Ymod","Ymin","Ymax"),tmat,amat] = c(1,rep(idat$yval,3))
						if (any(iii==c("All","Unknown"))) next 
						else if (figures){
							points(idat$age,idat$yval,pch=20,bg=.colBlind["orange"],cex=0.8); next
						}
					}
					if (n1>1) {
						yval <- idat$yval; age <- idat$age
						ttput(VBdat)  #assign("VBdat",idat,envir=.PBStoolEnv)
						here = lenv()
						make.parVec =function(xdat,xVec) { ## only affects row 1 of parVec, sometimes row 3
							xy <- hist(xdat$yval, nclass=20, plot=FALSE);
							Ymod <- xy$breaks[2:length(xy$breaks)][is.element(xy$density,max(xy$density))]
							Ymod <- mean(Ymod)
							Ymin <- min(xdat$yval,na.rm=TRUE)
							Ymax <- max(xdat$yval,na.rm=TRUE)
							xVec[ifelse(isLen,"Linf","Winf"),1:3] <- c(Ymod,Ymin,1.2*Ymax)
							if (fixt0) xVec[3,4]=FALSE
							tput(Ymod,tenv=here); tput(Ymin,tenv=here); tput(Ymax,tenv=here); 
							return(xVec)
						}
						ipVec = make.parVec(idat,pVec)
#browser();return()
						calcMin(pvec=ipVec,func=VBfun)
						if (!is.null(rm.studs) && is.numeric(rm.studs)) {
							if (length(rm.studs)==1) rm.studs = rep(rm.studs,2) * c(-1,1)
							rm.studs = sort(rm.studs)
							P <- tcall(PBSmin)$end
							Yinf = P[1];  K = P[2];  t0 = P[3];  sig = P[4]
							## http://cnrfiles.uwsp.edu/turyk/Database/Development/MJB/Chunk1/Classes/WATR784/Lab%20Exercises/VonB.pdf
							ttget(VBdat)
							TL =  VBdat$yval
							Age = VBdat$age
							pars = list(Yinf=Yinf,K=K,t0=t0)                      ## Vector of initial values (sans sig)
							enough.points   = nrow(VBdat) >= 10
							enough.contrast = diff(range(Yinf*(1-exp(-K*(Age-t0))))) > 2
							if (enough.points && enough.contrast) {  ## nls cannot fit when calculation has no contrast
								fit1 <- try(nls(TL~Yinf*(1-exp(-K*(Age-t0))),start=pars),silent=TRUE)  ## Use nls for residual checking
#browser();return()
								if (!inherits(fit1,"try-error")){
								#if (class(fit1)[1] != "try-error") {
									res.norm = residuals(fit1)
									## http://www.mathworks.com/matlabcentral/newsreader/view_thread/330668
									r = matrix(res.norm,ncol=1)
									h = matrix(hat(Age),ncol=1) # equiv: (X-mean(X))^2/sum((X-mean(X))^2) + 1/length(X)
									MSE = as.vector((t(r)%*%r)/(length(Age)-length(pars))) # 3 parameters estimated
									res.stud = r/(sqrt(MSE*(1-h)))
									#resetGraph(); plot(res.stud)
									keep = res.stud >= rm.studs[1] & res.stud <= rm.studs[2]
									VBdat0 = VBdat; fit0=fit1; n0=n1; ag0=age; yval0=yval
									idat = VBdat = idat[keep,]
									yval = idat$yval; age = idat$age
									#xlim <- c(0,max(idat$age,na.rm=TRUE))
									#ylim <- c(0,max(idat$yval,na.rm=TRUE))
									ttput(VBdat)
									DATA[[ttt]][[iii]] = idat
									n1   = nrow(idat)
									ipVec = make.parVec(idat,pVec)
									## Temporary calcs to get length sdevs by age (RH 210304)
									if (tables && l=="e") {
										tempdat=idat
										tempdat$age[tempdat$age>60]=60
										len.age = split(tempdat$len,tempdat$age)
										lcv = t(sapply(len.age,function(x){mn=mean(x,na.rm=TRUE); sd=sd(x,na.rm=TRUE); cv=sd/mn; return(c(mn,sd,cv))}))
										colnames(lcv) = c("mn","sd","cv")
										lcv.prefix = paste0("lcv.",substring(tolower(iii),1,1))
										lcv.suffix = sub("calcVB", "", tabName)
										lcv.out    = paste(lcv.prefix,lcv.suffix,sep=ifelse(grepl("^\\(|^\\.", lcv.suffix), "","."))
#browser();return()
										write.csv(lcv,file=paste0("./tables/",lcv.out,".csv"))
									}
									calcMin(pvec=ipVec,func=VBfun)
								} ## end if fit1 bad
							} ## end if enough points
						} ## end if rm.studs
						tget(PBSmin)  # located in .PBSmodEnv
						fmin <- PBSmin$fmin; np <- sum(pVec[,4]); ng <- nrow(idat);
						AICc <- 2*fmin + 2*np * (ng/(ng-np-1)); packList("AICc","PBSmin",tenv=.PBSmodEnv);  #print(PBSmin)
						P <- PBSmin$end
						Yinf <- P[1]; par1 <- format(round(Yinf,3),big.mark=options()$big.mark,scientific=FALSE)
						K    <- P[2]; par2 <- format(signif(K,4),big.mark=options()$big.mark,scientific=FALSE)
						t0   <- P[3]; par3 <- format(signif(round(t0,5),4),big.mark=options()$big.mark,scientific=FALSE)
						sig  <- P[4]; par4 <- format(signif(sig,4),big.mark=options()$big.mark,scientific=FALSE)
						ovec <- c(n=n1,Yinf,K,t0,sig,Ymod=Ymod,Ymin=Ymin,Ymax=Ymax)
						out[iii,,tmat,amat] <- ovec

						# Plot jittered data and add SGfun fit line
						if (any(iii==c("Females","Males","Both"))) {
							subtit = paste(amat,", ",tmat,sub("-"," ",yName),sep="")
							subtit = gsub("\\|","+",gsub("_",":",subtit))
							subtit = gsub("3\\+4\\+5\\+6\\+7\\+8\\+9",texThatVec(ar),subtit)
							subtit = gsub("1\\+2\\+3\\+4\\+5",paste0("tt_",texThatVec(tt)),subtit)
#browser();return()
							#xpred <- 0:xlim[2]; 
							xpred = seq(0,xlim[2],length.out=100)
							ypred <- Yinf * (1-exp(-K*(xpred-t0)))
							#print(amat);print(tmat);print(iii)
							fits[[amat]][[tmat]][[iii]] = list(xpred=xpred,ypred=ypred)

							if (any(iii==names(sex)) && figures) {
								plot(0,0, type="n", xlab=linguaFranca("Age",l), ylab=ifelse(isLen, linguaFranca("Length (cm)",l), linguaFranca("Weight (kg)",l)), main=linguaFranca(iii,l), xlim=xlim, ylim=ylim, bty="l", cex.main=ifelse(figgy,1,1.5), cex.lab=ifelse(figgy,1,1.5), xaxs="r", yaxs="r")
								#abline(h=seq(0,ylim[2],ifelse(ylim[2]<=10,1,5)),v=seq(0,xlim[2],ifelse(xlim[2]<=15,1,ifelse(xlim[2]<=50,5,10))),col="grey90",lwd=0.5)
								abline(h=pretty(ylim,n=10), v=pretty(xlim,n=10), col="grey90", lwd=0.5)
								points(jitter(age,jit[1]),jitter(yval,jit[2]),pch=convUTF("\\u{25AA}"),col=.colBlind["orange"],cex=ifelse(figgy,0.6,0.9))
								lines(xpred,ypred,col=.colBlind["blue"], lwd=ifelse(figgy,2,3))
								mw <- format(round(mean(idat$wt,na.rm=TRUE),1),nsmall=1)
								posX=ifelse(figgy,0.6,0.4); posY=.25; difY=.05; 
								cexlab=ifelse(figgy,0.7,1.2)
								addLabel(posX,posY-difY*0,bquote(bolditalic(Y)[infinity] == .(par1)),adj=0,cex=cexlab)
								addLabel(posX,posY-difY*1,bquote(bolditalic(K)==.(par2)),adj=0,cex=cexlab)
								addLabel(posX,posY-difY*2,bquote(bolditalic(t)[0]==.(par3)),adj=0,cex=cexlab)
								addLabel(posX,posY-difY*3,bquote(bolditalic(n)==.(format(n1,big.mark=options()$big.mark))),adj=0,cex=cexlab)
								if (!is.null(rm.studs) && is.numeric(rm.studs))
									addLabel(posX,posY-difY*4,bquote(italic(.(linguaFranca("removed",l)))==.(format(n0-n1,big.mark=options()$big.mark))),adj=0,cex=cexlab)
								#if (iii=="Males") {
								if (i==floor(median(1:nsex))) {
									subtit2 = gsub("ttype14","commercial",gsub("ttype23","research/survey",subtit))
									subtit2 = gsub("major3456789","coastwide",gsub("major89","5DE",subtit2))
									subtit2 = gsub("major567","5ABC",gsub("major34","3CD",subtit2))
									addLabel(0.5, 0.96, linguaFranca(ifelse(missing(subtitle), subtit2, subtitle),l), adj=c(0.5, 0), cex=cexlab)#+0.1) ## (RH: 200402)
								}
								box(bty="l")
							}
						} ## end if any femals, males, both
					} ## end if n1>1
				} ## end i (nsex) loop
				if (singles && figgy && figures) dev.off()
				if (tables && l=="e") {
					cat(paste("\nArea: ",amat,"   Trip type: ",tmat,sep=""),"\n",file=csv,append=TRUE)
					cat("age,females,males,both","\n",file=csv,append=TRUE)
#if (amat=="stock_5AB") {browser();return()}
					if (is.null(fits[[amat]][[tmat]])) next
					outsex = names(fits[[amat]][[tmat]])
					for (i in outsex) {
						if (i==outsex[1])
							mf = sapply(fits[[amat]][[tmat]][[i]],function(x){x},simplify=TRUE)
						else
							mf = cbind(mf, sapply(fits[[amat]][[tmat]][[i]],function(x){x},simplify=TRUE)[,2])
					}
#			mf=cbind(sapply(fits[[amat]][[tmat]][["Females"]],function(x){x},simplify=TRUE),
#				sapply(fits[[amat]][[tmat]][["Males"]],function(x){x},simplify=TRUE)[,2],
#				sapply(fits[[amat]][[tmat]][["Both"]],function(x){x},simplify=TRUE)[,2])
#browser();return()

					write.table(mf,file=csv,append=TRUE,sep=",",row.names=FALSE,col.names=FALSE)
					cat(paste("# Area: ",amat,"   Trip type: ",tmat,sep=""),"\n",file=adm, append=TRUE)
					#mess=c("# pars: n  Yinf  K  t0  sig  (for F, M, F+M)\n")
					mess = c("Sex,n,Yinf,K,t0,sig\n")
					mess = sub("Yinf",paste0(toupper(substring(yfld,1,1)),"inf"),mess)
					cat(paste(mess,collapse=""),file=adm,append=TRUE)
					mess = cbind(Sex=outsex, n=out[outsex,"n",tmat,amat],
						apply(out[outsex,2:5,tmat,amat],1,function(x){
						#paste(show0(format(round(x,6),scientific=FALSE,width=12,justify="right"),6,add2int=TRUE),collapse="")
						paste0(x,collapse=",")
					} ) )
					cat(paste(apply(mess,1,paste,collapse=","),collapse="\n"),sep="",file=adm,append=TRUE)
					cat("\n\n",file=adm,append=TRUE) 
				}
			}
			if (pages && figgy && figures) dev.off()
		}
		if ((!singles&!pages) && figgy && figures) dev.off()
	}; eop()
	attr(out,"xout") = xout
	if (figgy) attr(out,"plotNames") = plotNames
	assign(fName,fits,envir=.PBStoolEnv)
	fTag = sapply(strsplit(tabName,split="-"),function(x){paste0(x[-1],collapse="-")})
	ttget(VBFITS); VBFITS[[fTag]] = out[names(sex),,tmat,amat]; ttput(VBFITS)
#browser();return()
	ttput(DATA);
	stuff=c("out","pVec","xlim","ylim","anams","tnams","aName","tName","aaName","ttName","strSpp","fits","DATA")
	packList(stuff,"VB",tenv=.PBStoolEnv)
	omess = c( 
		paste("assign(\"VB",strSpp,"\",out); ",sep=""),
		paste("save(\"VB",strSpp,"\",file=\"VB",strSpp,"-(", fTag, ").rda\")",sep="")
	)
	eval(parse(text=paste(omess,collapse="")))
	invisible(out)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~calcVB


#compCsum-------------------------------2013-01-28
# Compare cumulative sum curves
# Note: ameth=3 otoliths broken & burnt
#-----------------------------------------------RH
compCsum <- function(dat=pop.age, pro=TRUE, strSpp="", xfld="age", plus=60,
	yfac="year",areas=list(major=NULL, minor=NULL, locality=NULL, 
	srfa=NULL, srfs=NULL, popa=NULL), ttype=list(c(1,4),c(2,3)), 
	stype=c(1,2,5:8), ameth=3, allTT=TRUE, years=1998:2006,
	pix=FALSE, wmf=FALSE, singles=FALSE, pages=FALSE, ioenv=.GlobalEnv) 
{
	assign("PBStool",list(module="M02_Biology",call=match.call(),args=args(compCsum),ioenv=ioenv),envir=.PBStoolEnv)
	#Subfunctions----------------------------------
	createPNG <- function(plotname,rc=c(1,3),rheight=4.5,mar=c(2,2,.5,.5),oma=c(2,1.5,0,0)) {
		plotname <- paste(plotname,"png",sep=".")
		png(plotname, units="in", res=300, width=6.5, height=rheight*rc[1])
		expandGraph(mfrow=rc,mar=mar,oma=oma,mgp=c(1.5,.5,0),cex=0.8) }
	createWMF <- function(plotname,rc=c(1,3),rheight=4.5,mar=c(2,2,.5,.5),oma=c(2,1.5,0,0)) {
		plotname <- paste(plotname,"wmf",sep=".")
		if (.Platform$OS.type=="windows")
			do.call("win.metafile",list(filename=plotname, width=6.5, height=rheight*rc[1]))
		expandGraph(mfrow=rc,mar=mar,oma=oma,mgp=c(1.5,.5,0),cex=0.8) }
	csum <- function(x,N,yspc=1){ # transform x-data to relative cumulative sums
		pos=attributes(x)$pos; lab=attributes(x)$lab
		if (pro) {
			xy=hist(x,breaks=seq(min(x)-.5,max(x)+.5,1),plot=FALSE)
			x=xy$mids; y=cumsum(xy$density) }
		else {
			x=sort(x); n=length(x); y=(1:n)/n }
		clr=clrs[lab]
		lines(x,y,col=clr)
		x50=median(x); N50=median(N)
		if (x50<N50) {y0=.8; sn=-1; Nvec=N[N<N50]} else {y0=.2; sn=1; Nvec=rev(N[N>=N50])}
		yqnt=y0+sn*(match(lab,names(Nvec))-1)*.04*yspc
		yqnt=max(0,yqnt); yqnt=min(1,yqnt); xqnt=approx(y,x,yqnt,rule=2,ties="ordered")$y
		out=c(xqnt,yqnt); attr(out,"clr")=clr; attr(out,"lab")=lab
		invisible(out) }
	clab <- function(coord,delim=""){ # label the curves
		x=coord[1]; y=coord[2]
		clr=attributes(coord)$clr; lab=attributes(coord)$lab
		if (delim!="") {dpos=regexpr(delim,lab); lab=substring(lab,dpos+1)}
		xdif=diff(par()$usr[1:2]); ydif=diff(par()$usr[3:4])
		xoff=0.5*((nchar(lab))^.7*0.1)*(xdif/par()$pin[1])
		yoff=0.5*((1)^.7*0.1)*(ydif/par()$pin[2])
		xbox=x+c(-1,-1,1,1)*xoff; ybox=y+c(-1,1,1,-1)*yoff
		polygon(xbox,ybox,col="ghostwhite",border=clr)
		text(x,y,lab,col=clr,adj=c(ifelse(wmf,.5,.45),ifelse(wmf,.3,.4)),cex=.7) 
		invisible() }
	plotCurve <- function(pdat,xfld,yfac,yspc=1) { # plot the relative cumulative curves
		paul=any(yfac==c("area","trip")); gap=!paul
		plot(0,0,type="n",xlab="",ylab="",xlim=xlim,ylim=ylim,axes=FALSE)
		mfg=par()$mfg; nr=mfg[3]
		row1=mfg[1]==mfg[3]; col1=mfg[2]==1
		tck=0.01*sqrt(prod(par()$mfg[3:4]))
		axis(1,labels=gap||row1,tck=-tck)
		axis(2,labels=gap||col1,tck=tck)
		if ((gap&col1) || (!gap&(col1&row1)))
			 mtext("Cumulative Frequency",outer=!gap,side=2,line=2,cex=nr^(-.01))
		if (row1) 
			mtext(paste(toupper(substring(xfld,1,1)),substring(xfld,2),sep=""),outer=!gap,side=1,line=2,cex=1.2)
		if (paul) addLabel(.9,.1,attributes(pdat)$yr,adj=1,cex=ifelse(pix|wmf,.9,1))
		else      addLabel(.9,.1,paste(amat,": trip type (",tmat,")",sep=""),adj=1,cex=ifelse(pix|wmf,.9,1))
		if (nrow(pdat)!=0) {
			ATlist=split(pdat[,xfld],pdat[,yfac]); packList("ATlist","PBStool",tenv=.PBStoolEnv)
			z=sapply(ATlist,function(x){length(x[!is.na(x)])}); ATlist=ATlist[z>7]
			if (length(ATlist)!=0) {
				abline(h=.5,col="gainsboro")
				if (pro) N=sort(sapply(ATlist,function(x){median(hist(x,breaks=seq(min(x)-.5,max(x)+.5,1),plot=FALSE)$mids)}))
				else     N=sort(sapply(ATlist,median))
				for (j in 1:length(ATlist)) {
					attr(ATlist[[j]],"pos")=j; attr(ATlist[[j]],"lab")=names(ATlist)[j] }
				lpos=sapply(ATlist,csum,N=N,yspc=yspc,simplify=FALSE)
				sapply(lpos,clab,delim=ifelse(yfac=="area","-",""))
			} else addLabel(.5,.5,cex=1.5,"Scarce Data",col="gainsboro")
		}  else addLabel(.5,.5,cex=1.5,"No Data",col="gainsboro")
		box(); invisible() }
	#----------------------------------Subfunctions

	CLRS=c("red","goldenrod","seagreen","blue","midnightblue") # Master colours
	unpackList(areas,scope="L") # make local all areas specified by user
	fnam=as.character(substitute(dat))
	expr=paste("getFile(",fnam,",senv=ioenv,try.all.frames=TRUE,tenv=penv()); dat=",fnam,sep="")
	eval(parse(text=expr))

	flds=names(dat)
	attSpp=attributes(dat)$spp
	if (is.null(strSpp) || strSpp=="") {
		if (is.null(attributes(dat)$spp) || attributes(dat)$spp=="") strSpp="999"
		else strSpp=attSpp }
	else if (!is.null(attSpp) && attSpp!=strSpp)
		showError(paste("Specified strSpp '",strSpp,"' differs from species attribute '",attSpp,"' of data file",sep=""))
	if (any(is.element(c("spp","species","sppcode"),flds))) {
		fldSpp=intersect(c("spp","species","sppcode"),flds)[1]
		dat=dat[is.element(dat[,fldSpp],strSpp),] }

	for (i in setdiff(c(xfld,yfac),c("area","trip")))  dat=dat[!is.na(dat[,i]),]
	if  (xfld=="len") dat[,xfld]=dat[,xfld]/10 # change from mm to cm
	nfac=length(yfac)

	aflds=c("major","minor","locality","srfa","srfs","popa")
	yarea=character(0)
	yrfld=intersect(c("year","yr","fyear","fyr"),names(dat))[1]
	if (!is.na(yrfld))
		eval(parse(text=paste(yrfld,"=",deparse(years))))
	
	flds=c("ttype","stype","ameth",yrfld)#,names(areas))
	for (i in flds) {
		expr=paste("dat=biteData(dat,",i,")",sep=""); eval(parse(text=expr))
		if (any(i==aflds)) eval(parse(text=paste("yarea=union(yarea,",i,")"))) }
	if (nrow(dat)==0) {
		if (pix|wmf) return()
		else showError("No records selected for specified qualification") }
	if (!is.null(plus) && plus>0) dat[,xfld][dat[xfld]>=plus]=plus

	if (!allTT) ttype=sort(unique(dat$ttype))
	tlab=sapply(ttype,function(x){paste(paste(x,collapse="+",sep=""),sep="")},simplify=TRUE); ntt=length(tlab)
	alist=sapply(areas,function(x){sapply(x,function(x){paste(x,collapse="+")},simplify=FALSE)},simplify=FALSE)
	alen=sapply(alist,length)
	alab=paste(rep(names(alist),alen),unlist(alist),sep="-"); nar=length(alab)
	aName=paste("-areas(",paste(alab,collapse=","),")",sep="")
	tName=paste("-tt(",paste(tlab,collapse=","),")",sep="")

	xlim=c(0,max(dat[,xfld],na.rm=TRUE)); ylim=c(0,1)
	sex <- list(1,2,1:2,c(0,3),0:3); names(sex) <- c("Males","Females","M+F","Unknown","All")

	# Special Case 1: Compare areas for specified years (thanks Paul)
	if (all(yfac=="area")) { 
		spooler(areas,"area",dat) # creates a column called 'area' and populates based on argument 'areas'.
		dat=dat[!is.na(dat$area),]
		ufac=sort(unique(dat$area))
		clrs=rep(CLRS,length(ufac))[1:length(ufac)]; names(clrs)=ufac
		if (nrow(dat)==0) showError("area","nodata")
		nyr=length(years); sqn=sqrt(nyr); m=ceiling(sqn); n=ceiling(nyr/m) 
		aName=paste("-area(",paste(alab,collapse=","),")",sep="")
		yName=paste("-year(",years[1],"-",years[length(years)],")",sep="")
		plotname <- paste("Csum-",strSpp,aName,tName,yName,sep="")
		mar=c(0,0,0,0); oma=c(4,3.5,.5,.5)
		if (!pix&!wmf) { resetGraph()
			expandGraph(mfrow=c(m,n),mar=mar,oma=oma,mgp=c(1.5,.5,0),cex=0.8) }
		else {
			if (pix)      createPNG(plotname,rc=c(m,n),rheight=3,mar=mar,oma=oma)
			else if (wmf) createWMF(plotname,rc=c(m,n),rheight=3,mar=mar,oma=oma) }
		for (i in years) {
			idat=dat[is.element(dat$year,i),]; attr(idat,"yr")=i
			plotCurve(idat,xfld,"area",yspc=2) }
		if (pix|wmf) dev.off()
		stuff=c("dat","xlim","ylim","ufac","aName","tName","yName","clrs","plotname"); packList(stuff,"PBStool",tenv=.PBStoolEnv)
		return(invisible())
	}
	if (all(yfac=="trip")) { # compare trip types for specified years (not implemented)
		spooler(ttype,"trip",dat)
		packList("dat","PBStool",tenv=.PBStoolEnv)
		showError("This option not yet implemented")
	}
	aName=paste("-areas(",paste(alab,collapse=","),")",sep="")
	tName=paste("-tt(",paste(tlab,collapse=","),")",sep="")
	fName=paste("-fac(",paste(yfac,collapse=","),")",sep="")
	plotname <- paste("Csum-",strSpp,aName,tName,fName,sep="")

	rc=c(length(alab)*length(tlab),nfac)
	if (!pix&!wmf) { resetGraph()
		expandGraph(mfrow=rc,mar=c(2,2,.5,.5),oma=c(2,1.5,0,0),mgp=c(1.5,.5,0),cex=0.8) }
	else if ((!singles&!pages) && (pix|wmf)) {
		if (pix)      createPNG(plotname,rc=rc)
		else if (wmf) createWMF(plotname,rc=rc) }

	for (a in names(areas)) {
		for (aa in 1:alen[a]) {
			aaa=areas[[a]][[aa]]; assign(a,aaa)
			expr=paste("adat=biteData(dat,",a,")",sep=""); eval(parse(text=expr))
			amat=paste(a,"-",paste(aaa,collapse="+"),sep="")
			aaName <- paste("Csum-",strSpp,"-area(",amat,")",tName,fName,sep="")
			if (pages && pix)      createPNG(aaName,rc=c(length(tlab),nfac))
			else if (pages && wmf) createWMF(aaName,rc=c(length(tlab),nfac))
			for (tt in ttype) {
				tmat=paste(tt,collapse="+")
				tdat <- adat[is.element(adat$ttype,tt),]
				if (nrow(tdat)==0) next
				ttName <- paste("Csum-",strSpp,aName,paste("-tt(",tt,")",sep=""),fName,sep="")
				if (singles && pix) createPNG(ttName,rc=c(1,nfac))
				else if (singles && wmf) createWMF(ttName,rc=c(1,nfac))
				for (i in yfac) {
					ufac=sort(unique(dat[,i]))
					clrs=rep(CLRS,length(ufac))[1:length(ufac)]; names(clrs)=ufac
					plotCurve(tdat,xfld,i,yspc=1) 
				}
				if (singles && (pix|wmf)) dev.off()
			}
			if (pages && (pix|wmf)) dev.off()
	}	}
	if ((!singles&!pages) && (pix|wmf)) dev.off()
	stuff=c("xlim","ylim","alab","tlab","aName","tName","aaName","ttName","clrs","plotname","strSpp")
	packList(stuff,"PBStool",tenv=.PBStoolEnv)
	invisible() 
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~compCsum


## estOgive-----------------------------2024-11-19
##  Creates ogives of some metric (e.g., % maturity at age).
##  Octals not supported in R.4.4.0+ (RH 230726)
##  Changed variance parameters to log of variance (like Awatea and SS3)
## Arguments:
##   dat     - specimen morphometrics data from GFBio
##   strSpp  - string code for species
##   method  - "EMP", "BL", "DN", "VH"
##   sex     - list of sex codes
##   mos     - list of months when maturity specifications apply: list(Male, Female, All)
##   mat     - codes that define maturity (e.g., 3+)
##   ameth   - ageing method
##   amod    - first age when model pmat is used in assessment (use raw pmat prior to amod)
##   azero   - ages for assessment where pmat=0 (force anomalous raw values to zero)
##   ttype   - list of trip types: 1=non-obs. commercial, 2=research, 3=survey, 4=obs. domestic, 5=JV hake
##   stype   - sample type
##   scat    - species category code (1 = unsorted samples)
##   SSID    - Survey Series ID
##   surveys - text to add to legend if multiple surveys combined
##   ofld    - ogive field (usually age)
##   obin    - bin size (y) to group ages
##   xlim    - range of morphometric (x-axis)
##   plines  - logical: add the predicted curve?
##   mpoints - logical: add model points along curve at integer ages
##   ppoints - logical: add predicted points along curve at integer ages
##   rpoints - logical: add observed points?
##   rtext   - logical: label obs. points w/ number of observations
##   fg      - foreground colours for ogives
##   Arcs    - degrees (0-360) away from age at 50% maturity to place label flag
##   radius  - length of the label flag pole
##   parList - initial values for parVec (val,min,max,active)
##   tabs    - logical: if TRUE, create table of output
##   outnam  - explicit ouput name to override an internally generated one
##   eps|png|wmf - logicals: if TRUE, send figure to the first TRUE device in this order
##   ioenv   - inout/output environment (primarily to comply with CRAN rules)
##   dots    - parameters to pass to subfunction `doLab'
## ---------------------------------------------RH
estOgive <- function(dat=pop.age, strSpp="", method=c("DN"),
   sex=list(Females=2,Males=1), years, mos=list(1:12,1:12), 
   mat=3:7, ameth=c(3,17), amod=NULL, azero=NULL,
   ttype=list(Commercial=c(1,4,5),Research=c(2:3)), 
   stype=c(1,2,6,7), scat = list(Unsorted=1), 
   SSID=NULL, surveys=NULL, subset="SSID",
   ofld="age", obin=1, xlim=c(0,45), PIN=c(8,5),
   mpoints=TRUE, ppoints=TRUE, plines=TRUE, rpoints=FALSE, rlines=FALSE, rtext=FALSE,
   fg=c("red","orange2","blue","green4"), Arcs=NULL, radius=0.2,
   parList=list(DN=list(val=c(15,2.5,100), min=c(5,0.01,10),
   max=c(60,10,150)), active=c(TRUE,TRUE,FALSE)), tabs=FALSE,
   outnam, eps=FALSE, png=FALSE, wmf=FALSE, ioenv=.GlobalEnv, lang=c("e","f"), ...)
{
##--Subfunctions------------------------
	## Proportions mature
	pmat <- function(x,mat) { # calculate proportions mature
		n=length(x); xm=x[is.element(x,mat)]; nm=length(xm)
		return(nm/n)
	}
	## Proportions-at-age (nu is now in log space)
	page <- function(p=.5,mu,nu) {
		mu - sqrt(-exp(nu)*log(p))
	}

	## Fit maturity using double normal
	fitDN <- function(P) {
		mu=P[1]; logvL=P[2]; logvR=P[3]
		vL = exp(logvL); vR= exp(logvR)
		a = obs$mn; pobs=obs$pemp; zL = a<=mu; zR = a>mu
		pred <- c( exp(-(a[zL]-mu)^2 / vL) , exp(-(a[zR]-mu)^2 / vR) )  ## see (D.3) in stock assessments
		n    <- length(pobs)
		ssq  <- sum( (pobs-pred)^2 )
#lines(obs$mn,pred)
#browser();return()
		return(n*log(ssq))
	}
	calcDN <- function(P, a=obs$mn) {
		mu=P[1]; logvL=P[2]; logvR=P[3]
		vL = exp(logvL); vR= exp(logvR)
		#a = seq(1,60,len=1000) #a = obs$mn; pobs=obs$pemp; 
		zL = a<=mu; zR = a>mu
		pred = c( exp(-(a[zL]-mu)^2 / vL) , exp(-(a[zR]-mu)^2 / vR) )
		return(pred)
	}

	## Logistic code extracted from Vivian Haist MSLM size-based NZ rock lobster model
	fitVH <- function(P) {
		mat50 = P[1]; mat95add = P[2]
		## mat50 = size/age at 50% maturity
		## mat95add = increment to get to 95% maturity
		#a = idat$obin; pobs=idat$pmat
		a = obs$mn; pobs=obs$pemp
		pred = 1. / (1. + exp((-log(19.) / mat95add) * (a - mat50)))  ## predicted maturity
		n <- length(pobs); ssq <- sum((pobs-pred)^2 )
		return(n*log(ssq))
	}
	calcVH <- function(P,a=obs$mn) {
		mat50=P[1]; mat95add=P[2]
		pred = 1. / (1. + exp((-log(19.) / mat95add) * (a - mat50)))  ## predicted maturity
		return(pred)
	}

	## Add label with pointer to median value
	doLab =function(x, y, x50, n=1, ...) {
		if (is.null(Arcs)) Arcs=c(150,165,315,330,135,120,345,360)
		if (x50>xlim[1] & x50<xlim[2]) {
			flagIt(a=x50, b=0.5, r=radius, A=Arcs[sin], col=fg[sin], n=n, ...)
			points(x50, 0.5, pch=bigPch[sin], bg=bg[sin], cex=1.2)
		} else {
			ldL = unlist(approx(x,y,xlim[1],rule=2,ties="ordered"))
			text(ldL[1], ldL[2]+.02*dy, show0(round(mdBL,1),1,add2int=TRUE), cex=0.8, col=fg[sin])
		}
	}
	lighten <- function(clrs,N=5,M=4){
		liteclrs = sapply(clrs, function(x){
			cFun = colorRampPalette(c(x,"white"))
			cFun(N)[M]
		})
		return(liteclrs)
	}
	amendLeg <- function(legtxt, muval, mulab, vLval=NULL){
		revleg  = rev(legtxt)
		lastleg = revleg[1]
#if (mulab=="DN") {browser();return()}
		#lastleg = gsub(")$",paste0(", \\\265.",mulab,"=",round(muval,1),")"),lastleg)  ## octals not supported in R.4.4.0+
		lastleg = gsub(")$", paste0(", ", convUTF("\\u{03BC}"), ".", mulab, "=", round(muval,1), ")"), lastleg)
		if (!is.null(vLval))
			lastleg = gsub(")$", paste0(" : logvL=",round(vLval,1),")"), lastleg)
		revleg[1] = lastleg
		legtxt  = rev(revleg)
		return(legtxt)
	}
##--End Subfunctions--------------------

	## Create a subdirectory called `french' for French-language figures
	createFdir(lang)

	sexcode=c("Unknown","Male","Female","Indeterminate"); names(sexcode)=0:3
	if (!is.null(SSID))
		sexmos = c(names(sex), paste0("SSID(",sapply(SSID,paste0,collapse="+"),")"))
	else 
		sexmos = paste0(names(sex),"-mo(",sapply(mos,paste,collapse="+"),")")
	if (missing(outnam))
		onam=paste0(strSpp,"-Ogive(",ofld,")-",paste0(sexmos,collapse="-"),"-Mat(",mat[1],"+)")
	else onam = outnam

	assign("PBStool",list(module="M02_Biology",call=match.call(),args=args(estOgive),ioenv=ioenv),envir=.PBStoolEnv)
	fnam = as.character(substitute(dat))
	flog = paste0(onam,".log")
	expr = paste("getFile(",fnam,",senv=ioenv,use.pkg=TRUE,try.all.frames=TRUE,tenv=penv()); dat=",fnam,sep="")
	eval(parse(text=expr))
	.flush.cat("before biting      : ",nrow(dat)," records\n", sep="", file=flog)

	flds = names(dat)
	need = c(ofld,"date","sex","mat")
	if (ofld=="age")
		need = c(need,"ameth")
	if(!all(is.element(need,flds)))
		showError(setdiff(need,flds),"nofields")
	if(!is.element("year",flds))
		dat$year = as.numeric(substring(dat$date,1,4))

	attSpp=attributes(dat)$spp
	if (is.null(strSpp) || strSpp=="") {
		if (is.null(attributes(dat)$spp) || attributes(dat)$spp=="") strSpp="999"
		else strSpp = attSpp
	} else if (!is.null(attSpp) && attSpp!=strSpp) {
		showError(paste("Specified strSpp '",strSpp,"' differs from species attribute '",attSpp,"' of data file",sep=""))
	}
	if (any(is.element(c("spp","species","sppcode"),flds))) {
		fldSpp=intersect(c("spp","species","sppcode"),flds)[1]
		dat=dat[is.element(dat[,fldSpp],strSpp),]
		.flush.cat("after bite strSpp  : ", nrow(dat), " records left\n", sep="", file=flog, append=TRUE)
	}

	#if (ofld=="len") dat$len=dat$len
	if (ofld=="age") {
		dat = extractAges(dat, ameth)
		.flush.cat("after bite ameth   : ",nrow(dat)," records left\n",sep="", file=flog, append=TRUE)
	}
	if (!missing(years) && !is.null(years)){
		dat = dat[is.element(dat$year, years),]
		.flush.cat("after bite years   : ",nrow(dat)," records left\n",sep="", file=flog, append=TRUE)
	}
	dat$month <- as.numeric(substring(dat$date,6,7))
	if (is.null(scat) || is.na(scat))
		scat = list('allscat'=.su(dat$scat))

	for (i in c("stype","scat")) {
		mess = paste0("dat = biteData(dat,",i,")")
		eval(parse(text=mess))
		.flush.cat("after bite ", i, rep(" ", 8 - nchar(i)),": ",nrow(dat)," records left\n",sep="", file=flog, append=TRUE)
	}
	dat$ogive <- dat[,ofld]
	dat <- dat[is.element(dat$sex,sort(unique(unlist(sex)))),]
	.flush.cat("after bite sex     : ",nrow(dat)," records left\n",sep="", file=flog, append=TRUE )
	dat <- dat[is.element(dat$mat,1:rev(mat)[1]),]  ## Maturity Codes depend on Maturity convention (e.g., 25 for PAH/WAP)
	.flush.cat("after bite mat     : ",nrow(dat)," records left\n",sep="", file=flog, append=TRUE)
	dat <- dat[dat$ogive>=xlim[1] & dat$ogive<=xlim[2] & !is.na(dat$ogive),]
	.flush.cat("after bite ogive   : ",nrow(dat)," records left\n",sep="", file=flog, append=TRUE)
	dat$obin <- ceiling(dat$ogive/obin)*obin # - (obin/2) # Mean age of each bin

	if (is.null(names(sex)))
		names(sex)=sapply(sex,function(x){paste("sex_",paste(x,collapse="+"),sep="")})
		#paste("sex",sapply(strsplit(names(sex),""),paste,collapse="+")
	if (any(duplicated(names(sex)))) showError("Choose unique combinations of sex")
	nsex = length(sex)

	asub = get(subset) ## this seems like it needs work
	nsub = length(ttype)+length(asub) ## subsets for each sex
	if (nsub==0) {
		ttype = list('avail.ttype'=.su(dat$ttype))
		nsub  = 1
	}
	subtype = c(rep("ttype",length(ttype)),rep(subset,length(asub)))
	subsets = c(ttype,asub)

	if (is.null(mos)) mos = list(.su(dat$month))
	mos=rep(mos,nsex)[1:nsex]
	nsexsub = nsex*nsub
	if (length(fg) < nsexsub) {
		fg = rep(fg,nsex)[1:nsex]
		fg = rep(fg,each=nsub)
	}
	#fg=rep(fg,nsexsub)[1:nsexsub]; #bg=rep(bg,nsexsub)[1:nsexsub]
	bg = lighten(fg,7,4)
	smClrs = lighten(fg,7,ifelse(ppoints,5,6))
	bigPch = rep(21:25,nsexsub)[1:nsexsub]

	abin <- split(dat$mat,dat$obin)
	x = as.numeric(names(abin))
	xpos = x - (obin-1)*.5   # mid-point of binned ogive
	if (is.null(xlim)) xlim = range(xpos); 
	dx <- diff(xlim)
	xtab = seq(obin,max(x),obin)
	#out = array(NA,dim=c(length(names(abin)),6,nsex,nsub),
	#	dimnames=list(bin=names(abin),val=c("pemp","n","mn","sd","pbin","pdbl"),sex=names(sex),sub=names(subsets)))
	out = array(NA,dim=c(length(xtab),8,nsex,nsub),
		dimnames=list(bin=xtab,val=c("n","mn","sd","pEMP","pBL","pVH","pDN","pMOD"),sex=names(sex),sub=names(subsets)))
	xout = array(NA,dim=c(2,nsub,nsex,2),dimnames=list(range=c("min","max"),sub=names(subsets),sex=names(sex),year=c("year","date" )))

	ylim <- c(0,1); dy <- diff(ylim)
	pend = CALCS = DATA = list() # create empty lists
	#clearFiles(paste0(outnam,".",c("csv","par","png","rda")))
#browser();return()

	fout.e = onam
	for (l in lang) {  ## could switch to other languages if available in 'linguaFranca'.
		changeLangOpts(L=l)
		#fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		fout = switch(l, 'e' = paste0("./english/",fout.e), 'f' = paste0("./french/",fout.e) )
		if (eps) postscript(paste0(fout,".eps"), width=PIN[1], height=PIN[2], paper="special")
		else if (wmf && .Platform$OS.type=="windows")
			do.call("win.metafile", list(filename=paste0(fout,".wmf"), width=PIN[1], height=PIN[2]))
		else if (png) {
			clearFiles(paste0(fout,".png"))
			png(filename=paste0(fout,".png"), width=PIN[1], height=PIN[2], units="in", res=400)
		}
		#else resetGraph()
		expandGraph(mfrow=c(1,1),mai=c(.6,.7,0.05,0.05),omi=c(0,0,0,0),las=1,lwd=1)
		MDX = xlim[2]/3
	
		plot(0,0, type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=xlim, ylim=c(0,1))
		abline(h=.5,col="gainsboro",lty=1,lwd=2)
		abline(h=seq(0,1,0.1), v=seq(0,xlim[2],ifelse(xlim[2]<30,1,ifelse(xlim[2]<60,2,5))), col="gainsboro", lty=3, lwd=1)
		if(ofld=="age") axis(1,at=seq(floor(xlim[1]),ceiling(xlim[2]),1),tcl=-.2,labels=FALSE)
		axis(1,at=seq(floor(xlim[1]),ceiling(xlim[2]),ifelse(xlim[2]<=10,1,5)),tcl=-.4,labels=FALSE)
		axis(1,at=seq(floor(xlim[1]),ceiling(xlim[2]),ifelse(xlim[2]<=10,2,10)),tcl=-.5,mgp=c(0,.5,0),cex=.9)
		axis(2,at=seq(0,1,.05),tcl=-.25,labels=FALSE)
		axis(2,at=seq(0,1,.1),tcl=-.5,mgp=c(0,.7,0),cex=.9,adj=1)
		mtext(linguaFranca(ifelse(ofld=="age","Age","Length"),l), side=1, cex=1.2, line=1.75)
		mtext(ifelse(l=="f","proportion mature","Proportion Mature"), side=2, cex=1.2, line=2.5, las=0)

		legtxt = character(); nleg = 0
		for (s in 1:nsex) {
			ss=names(sex)[s]
			#above=as.logical(s%%2); #print(c(s,above))
			sexlab=paste(substring(sexcode[as.character(sex[[s]])],1,1),collapse="+")
			sdat <- dat[is.element(dat$sex,sex[[s]]),]
			.flush.cat("after bite ", ss, rep(" ", 8-nchar(ss)), ": ", nrow(sdat)," records left\n",sep="", file=flog, append=TRUE )
#browser();return()

			for (i in 1:nsub) {
				ii  = subtype[i]
				iii = subsets[[i]]
				sss = names(subsets)[i]
				sin = (s-1)*nsub + i
				idat = sdat[is.element(sdat[,ii],iii),]
				if (nrow(idat)==0 || !any(idat$mat %in% mat) ) next
				.flush.cat("after bite ", ii, "   : ", nrow(idat)," records left\n", sep="", file=flog, append=TRUE)
				#if (ii=="ttype")
				idat <- idat[is.element(idat$month,mos[[s]]),]
				if (nrow(idat)==0) next
				.flush.cat("after bite month   : ", nrow(idat)," records left\n", sep="", file=flog, append=TRUE)
#browser();return()
				if (strSpp=="405" && ss=="Males")
					idat = idat[idat$age!=3 & !is.na(idat$age),] # special condition (may not always be necessary)
				mbin <- split(idat$mat,idat$obin)
				nbin <- sapply(mbin,length) # number of maturity codes in each age bin
				CALCS[[ss]][[sss]] = list()
				#CALCS[[ss]][[sss]][["mbin"]] = mbin
				CALCS[[ss]][[sss]][["nfish"]] = sum(nbin)
				nleg = nleg +1
				legtxt[nleg] = paste0(ss,": ",sss," (n=",sum(nbin),")")
				x = as.numeric(names(mbin))
				xpos = x - (obin-1)*.5   # mid-point of binned ogive
	
				## Empirical fit
				pemp  = sapply(mbin,pmat,mat=mat)   # empircal proportions mature
				enam  = names(pemp)
				nemp  = sapply(mbin, length)
				## Some weird shit that makes little sense:
				zbad1 = pemp>=0.99 & as.numeric(names(pemp)) <= parList$val[1]
				if (any(zbad1)) {.flush.cat("zbad1:\n"); print(zbad1)}
				zbad2 = pemp>=0.50 & as.numeric(names(pemp)) <= parList$min[1]
				if (any(zbad2)) {.flush.cat("zbad2:\n"); print(zbad2)}
				#pemp[zbad1|zbad2] = 0 ## why adjust to zero? Maybe for zbad2...
				ogive = split(idat$ogive,idat$obin) # age/lengths in the bin
				n     = sapply(ogive,length)        # number of ages/lengths in bin
				mn    = sapply(ogive,mean)          # mean age/length by bin
				sd    = sapply(ogive,sd)            # std dev of mean age/length by bin
				out[names(pemp),"pEMP",ss,sss] = pemp
				out[names(n),"n",ss,sss]=n; out[names(mn),"mn",ss,sss]=mn; out[names(sd),"sd",ss,sss]=sd
				obs  <- list(mn=mn, pemp=pemp); ttput(obs)
				#mdx  <- approx(pemp,mn,xout=.5,rule=2,ties="ordered")$y
				#mdxy <- approx(pemp,mn,xout=.5,rule=2,ties="ordered")
				mdEMP = mn[findPV(0.5, pemp)]
				muEMP = mn[findPV(1.0, pemp)]
				CALCS[[ss]][[sss]][["obs"]] = cbind(age=as.numeric(names(ogive)),n,mn,sd,pEMP=pemp)
				CALCS[[ss]][[sss]][["p50"]][["EMP"]] = mdEMP
				CALCS[[ss]][[sss]][["mu"]][["EMP"]] = muEMP

				## Binomial Logit (BL) fit
				idat$bmat = as.numeric(is.element(idat$mat,mat)) # TRUE/FALSE for mature
				idat$pmat = idat$mat / max(mat)
				DATA[[ss]][[sss]] = idat
				pdat = idat[,c("obin","bmat")]
				pout = fitLogit(pdat,"bmat","obin")
				coeffs = pout$coefficients
				a=coeffs[1]; b=coeffs[2]
				#pbin = exp(a+b*xpos)/(1+exp(a+b*xpos)); names(pbin)=x
				pbin = exp(a+b*xtab)/(1+exp(a+b*xtab)); names(pbin)=xtab
				#nmu  = which(abs(pbin-1)==min(abs(pbin-1))) ## https://stat.ethz.ch/pipermail/r-help/2008-July/167216.html
				nmu  = findPV(0.99,pbin) ## new function (181109)
				muBL = xtab[nmu]
				out[names(pbin),"pBL",ss,sss]=pbin
				ybin = seq(.0001,.9999,.0001)
				xbin = (log(ybin/(1-ybin))-a)/b
				zbin = xbin<=xlim[2]
				xbin = xbin[zbin]; ybin = ybin[zbin]
				mdBL = -a/b
				CALCS[[ss]][[sss]][["p50"]][["BL"]] = mdBL
				CALCS[[ss]][[sss]][["mu"]][["BL"]]  = muBL
				CALCS[[ss]][[sss]][["par"]][["BL"]] = c(a=as.vector(a),b=as.vector(b))

				## Vivian Haist (VH) logistic fit
				parVH = parList[["VH"]][c("val","min","max","active")] # must have these components
				if (!(length(parVH)==4 && all(sapply(parVH,length)==2)))
					showError("Vivian Haist `parList' must have 4 vectors\n\nnamed `val', `min', `max' and `active',\n\neach with 2 elements for `mat50' and `mat95add'",as.is=TRUE)
				parVec = data.frame(parVH,row.names=c("mat50","mat95add"), stringsAsFactors=FALSE)
				dlist  = calcMin(pvec=parVec, func=fitVH, method="nlm", repN=10, steptol=1e-10, reltol=1e-16)
				Pend   = dlist$Pend
				mdVH  = Pend[[1]]; p95vh = sum(Pend)
				xVH    = xtab; names(xVH)=xtab
				yVH    = calcVH(Pend, a=xVH)
				muVH   = xtab[findPV(0.99,yVH)]
				xVH    = as.numeric(names(yVH))
				out[names(yVH),"pVH",ss,sss] = yVH
				CALCS[[ss]][[sss]][["p50"]][["VH"]] = mdVH
				CALCS[[ss]][[sss]][["mu"]][["VH"]]  = muVH
				CALCS[[ss]][[sss]][["par"]][["VH"]] = Pend
				#pend[[ss]][[sss]] = c(Pend, p50=mdVH, p95=p95vh)
#browser();return()

				## Double Normal fit
#plot(obs$mn,obs$pemp,type="n")
				parDN = parList[["DN"]][c("val","min","max","active")] # must have these components
				if (!(length(parDN)==4 && all(sapply(parDN,length)==3)))
					showError("Double Normal `parList' must have 4 vectors\n\nnamed `val', `min', `max' and `active',\n\neach with 3 elements for `mu', `logvL', and `logvR'",as.is=TRUE)
				parVec = data.frame(parDN,row.names=c("mu","logvL","logvR"), stringsAsFactors=FALSE)
				dlist  = calcMin(pvec=parVec, func=fitDN, method="nlm", repN=10, steptol=1e-10, reltol=1e-20)#-16)
				Pend   = dlist$Pend
				muDN   = round(Pend[1],1)
				vLDN   = round(Pend[2],1)
				mdDN   = page(0.5,Pend[[1]],Pend[[2]])
				CALCS[[ss]][[sss]][["p50"]][["DN"]] = mdDN
				CALCS[[ss]][[sss]][["mu"]][["DN"]]  = muDN
				CALCS[[ss]][[sss]][["par"]][["DN"]] = Pend
				#pend[[ss]][[sss]] = c(Pend,p50=mdDN)
				xdbl = xtab; names(xdbl)=xtab
				ydbl = calcDN(Pend,xdbl)
				xdbl = as.numeric(names(ydbl))
				out[names(ydbl),"pDN",ss,sss] = ydbl
				xmod = xVH; ymod = yVH  ## use Vivian Haist logistic for model inputs (used to be double normal) (RH 241119)
				if (!is.null(amod) && amod > obin) {
					zmod = xmod >= amod
					araw = amod-1; xraw=seq(obin,araw,obin); yraw=rep(NA,length(xraw)); names(yraw)=xraw
					praw = pemp[intersect(names(pemp),names(yraw))]; yraw[names(praw)]=praw
					if (!is.null(azero)) {
						azero = intersect(xraw,azero) # just to be sure that user doesn't specify azero>araw
						yraw[as.character(azero)] = 0
					}
					out[names(ymod),"pMOD",ss,sss] = c(yraw,ymod[zmod])
#browser();return()
				} else {
					out[names(ymod),"pMOD",ss,sss] = ymod
				}
				xout[,sss,ss,"year"] = range(idat$year,na.rm=TRUE)
				xout[,sss,ss,"date"] = range(substring(idat$date,1,10),na.rm=TRUE)
	
				if (rpoints) {# raw (observed) proportion mature at age
					#points(xpos,pemp,pch=bigPch[sin],col=ifelse(ppoints,smClrs[sin],fg[sin]),bg=smClrs[sin],cex=ifelse(sum(c(rpoints,rtext))==1,1.2,1.0))
					## for debugging:
					#text(xpos,pemp,nemp,cex=0.7,col=c("black","blue"))
					points(xpos,pemp, pch=21, cex=2, col="gainsboro", bg=lucent(fg[sin],0.05))
					text(xpos,pemp,nemp,cex=0.7,col=fg[sin])
#if (s==2){browser();return()}
				}
				nmeth = 0
				if (any(method=="EMP")) {
					nmeth  = nmeth + 1
					legtxt = amendLeg(legtxt, muval=muEMP, mulab="EMP")
					if (!rpoints)  ## don't overplot raw points from a few lines previous
						points(mn,pemp,pch=bigPch[sin],col=fg[sin],bg=bg[sin],cex=1.2)
					if (rlines) {
						lines(mn,pemp,col=fg[sin])
						doLab(mn, pemp, mdEMP, n=nmeth, lab="EMP", ...)
					}
#browser();return()
				}
				if (any(method=="BL")) {
					nmeth   = nmeth + 1
					legtxt = amendLeg(legtxt, muval=muBL, mulab="BL")
					if (plines){
						lines(xbin,ybin,col=fg[sin],lwd=ifelse(all(method=="BL"),2,1),lty=ifelse(all(method=="BL"),1,3))
						doLab(xbin, ybin, mdBL, n=nmeth, lab="BL", ...) 
					}
				}
				if (any(method=="VH")) {
					nmeth   = nmeth + 1
					legtxt = amendLeg(legtxt, muval=muVH, mulab="VH")
					if (plines){
						lines(xVH,yVH,col=fg[sin],lwd=ifelse(all(method=="VH"),2,1),lty=ifelse(all(method=="VH"),1,5))
						doLab(xVH, yVH, mdVH, n=nmeth, lab="VH", ...) 
					}
				}
				if (any(method=="DN")) {
					nmeth  = nmeth + 1
					legtxt = amendLeg(legtxt, muval=muDN, mulab="DN",vLval=vLDN)
					Xdbl   = seq(xlim[1],xlim[2],len=1000)
					Ydbl   = calcDN(Pend,a=Xdbl)

					if (plines) {
						#lines(Xdbl,Ydbl,col=fg[sin],lwd=ifelse(all(method=="DN"),2,2),lty=sin) #ifelse(all(method=="DN"),1,1))
						lines(Xdbl,Ydbl,col=fg[sin],lwd=ifelse(all(method=="DN"),2,2),lty=1)
					} else {
						CALCS[[ss]][[sss]][["nrec"]] = nrow(idat)
						CALCS[[ss]][[sss]][["pend"]] = pend
						CALCS[[ss]][[sss]][["XYlineDN"]] = cbind(Xdbl,Ydbl)
						CALCS[[ss]][[sss]][["XYrptsDN"]] = cbind(xpos,pemp)
					}
					Xpts = seq(1,xlim[2],1)
					Ypts = calcDN(Pend,a=Xpts)
					if (ppoints) {
						points(Xpts,Ypts,pch=bigPch[sin],col=fg[sin],bg=bg[sin],cex=1.1)
					} else {
						CALCS[[ss]][[sss]][["XYpptsDN"]] = cbind(Xpts,Ypts)
					}
					if (mpoints) {
						if (!is.null(amod) && amod > obin) { # redundant (in case we want finer (X,Y) points for curve than integer ages in results array `out')
							zmod = Xpts>=amod
							points(Xpts[zmod], Ypts[zmod], pch=4, lwd=ifelse(png,1,2), col="black", cex=ifelse(png,1,1.2))
							araw = amod-1; xraw=seq(obin,araw,obin); yraw=rep(NA,length(xraw)); names(yraw)=xraw
							praw = pemp[intersect(names(pemp),names(yraw))]; yraw[names(praw)]=praw
							if (!is.null(azero)) {
								azero = intersect(xraw,azero) # just to be sure that user doesn't specify azero>araw
								yraw[as.character(azero)] = 0
								xraw = xraw - (obin-1)*.5   # mid-point of binned ogive
							}
							points(xraw, yraw, pch=4, lwd=ifelse(png,1,2), col="black", cex=ifelse(png,1,1.2))
						}
					}
					#else pmodel = Ypts
					if (rtext)
						text(xpos,pemp,nbin,col=fg[sin],font=ifelse(eps,1,2),cex=1,adj=if (rpoints) c(s%%2,1) else c(0.5,0.5))
	#if (i==5) {browser();return()}
					if (plines) {
#browser();return()
						doLab(Xdbl, Ydbl, mdDN, n=nmeth, lab="DN", ...)
					}
					MDX = max(MDX, approx(Ydbl,Xdbl,xout=.4,rule=2,ties="ordered")$y)  ## used to place the legend
				}
			}
		}
		#if (nleg==0) CALCS = NULL
		#addLegend(.7,.3,legend=paste("sex",sapply(strsplit(names(sex),""),paste,collapse="+")),lwd=ifelse(any(method==c("BL","DN")),2,1),
		if (length(legtxt)==0) {
			if (nsex>1) legtxt = paste0(rep(names(sex),each=nsub),": ")
			else legtxt = ""
			legtxt = paste0(legtxt,rep(names(subsets),nsex))
		}
#browser();return()
		#if (length(SSID)==1)
		#	legtxt=sub(names(SSID),paste0(names(SSID),"\n     ",paste(surveys,collapse="\n     "),"\n"),legtxt)
		if (plines)
			addLegend(ifelse(png,0,0.05)+MDX/xlim[2], ifelse(is.null(surveys),0.30,0.45), legend=linguaFranca(legtxt,l), lty=1, lwd=ifelse(is.element("DN",method)||nmeth==1,2,1), adj=c(0,ifelse(is.null(surveys),0.5,0.95)), pch=ifelse(is.element("EMP",method)&&!rpoints&&rlines,19,NA), col=fg[1:nsexsub], cex=ifelse(eps|png,0.8,1), bty="n", seg.len=2.5)
		box()
		if(eps|png|wmf) dev.off()
	}; eop()

	attr(out,"xout") = xout
	attr(out,"Nrecs") = nrow(dat)
	stuff=c("pmat","DATA","CALCS","out","strSpp")
	packList(stuff,"PBStool",tenv=.PBStoolEnv)

	#tabs = list(...)$tabs
	if (is.null(tabs) || !is.logical(tabs))
		tabs = FALSE
	if (tabs) {
		if (!dir.exists("./tables/"))
			dir.create("./tables/")
	}

	fcsv = paste0("./tables/", onam,".csv")
	fpar = paste0("./tables/", onam,".par")
	data("species", package="PBSdata", envir=penv())
	if (tabs) {
		cat(paste0("Maturity ogives: ",species[strSpp,"name"],"\n"),file=fcsv)
		cat(paste0("Model parameters: ",species[strSpp,"name"],"\n"),file=fpar)
	}
	for (s in 1:nsex) {
		ss = names(sex)[s]
		for (i in 1:nsub) {
			sss  = names(subsets)[i]
			sout = out[,,ss,sss]
			#if (any(method=="DN")) 
			#	sout = cbind(sout,pmod=pmodel)
			svec = apply(sout,1,paste,collapse=",")
			if (tabs) {
				cat(paste0("sex: ",ss,"  sub: ",sss,"\n"),file=fcsv,append=TRUE)
				cat("age,",paste0(colnames(sout),collapse=","),"\n",file=fcsv,append=TRUE)
				cat(paste0(paste(names(svec),svec,sep=","),collapse="\n"),"\n",file=fcsv,append=TRUE)
			}

			calcs = CALCS[[ss]][[sss]]
			if (tabs) {
				cat(paste0("\nsex: ",ss,"  sub: ",sss,"\n"),file=fpar,append=TRUE)
				for (m in c("EMP","BL","VH","DN")) {
					mm = switch(m, 'EMP'="empirical", 'BL'="binomial logit", 'VH'="Vivian Haist logistic", 'DN'="double normal")
					cat(paste0("\nmodel: ",m," -- ",mm,"\n"),file=fpar,append=TRUE)
					if (m!="EMP")
						cat(paste(paste(names(calcs[["par"]][[m]]),calcs[["par"]][[m]],sep=","),sep="", collapse="\n"),"\n", sep="",file=fpar, append=TRUE)
					cat(paste(c("p50","mu"),c(calcs[["p50"]][[m]],calcs[["mu"]][[m]]), sep=",",collapse="\n"), "\n", sep="",file=fpar, append=TRUE)
				}
			}
#browser();return()
		}
	}
	omess = c( 
		paste0("assign(\"ogive",strSpp,"\",out); "),
		paste0("save(\"ogive",strSpp,"\",file=\"",onam,".rda\"); ")#,
		#paste0("write.csv(t(as.data.frame(ttcall(PBStool)$pend$Females)),file=\"",onam,".par\")")
	)
	eval(parse(text=paste(omess,collapse="")))
	invisible(out)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~estOgive


## extractAges -------------------------2023-02-09
## Extract records with positive age and qualify
##  by the selected ageing method.
## If show.age=TRUE, a GUI reports # ages by
##  specified field pair for each sex.
## ---------------------------------------------RH
extractAges <- function(dat, ameth=c(3,17), use.sfc.age=1:3, sex=c(2,1),
   rmSM=TRUE, only.markSM=FALSE, rmRC=9,
   show.age=FALSE, show.flds=c("ameth","ttype"), show.only=FALSE)
{
	## Get rid of records with no ages
	dat = dat[dat$age>0 & !is.na(dat$age),]
	if (rmSM)
		dat = zapSeamounts(dat, only.mark=only.markSM)
	#if ("RC" %in% colnames(dat) && !is.null(rmRC) && class(rmRC)=="numeric")
	if ("RC" %in% colnames(dat) && !is.null(rmRC) && inherits(rmRC,"numeric"))
		dat = dat[!is.element(dat$RC, rmRC),]

	if (show.age) {
		show.flds = setdiff(show.flds, "sex")
		if (length(show.flds)!=2 || any(!is.element(show.flds, colnames(dat))))
			stop (paste0("Choose another pair of field names from:\n", paste0(deparse(colnames(dat)),collapse="\n") ) )
		ats = crossTab(dat, c(show.flds, "sex"), "age", length)
		winStr = c(
			"window name=\"sAges\" title=\"Ages Available\" bg=\"gainsboro\"",
			"grid 1 2 byrow=TRUE sticky=W relief=flat",
			paste0("label text=\"Number of ages by:\\n", show.flds[1], " (row)  \\&  ", show.flds[2], " (col)\" font=12"),
			"button text=\"Codes\" bg=moccasin font=10 sticky=E padx=\"15 0\" function=doAction action=\"openFile(paste0(system.file(package=`PBStools`),`/win/GFBtcodes.txt`))\"",
			paste0("grid ", dim(ats)[3], " 2 byrow=TRUE sticky=W")
		)
		sexlab = c("Not sexed", "Male", "Female", "Uncertain")
		sexcol = c("red", "green4", "dodgerblue", "purple")
		names(sexlab) = names(sexcol) = 0:3
		for (i in 1:dim(ats)[3]) {
			ii   = dimnames(ats)[[3]][i]
			assign (paste0("obj",i), ats[,,i])
			winStr = c( winStr,
			paste0("label text=\"",sexlab[ii],"\" font=bold fg=", sexcol[ii]),
			paste0("object name=obj", i, " edit=FALSE noeditbg=aliceblue sticky=W")
			)
		}
		createWin(winStr, astext=TRUE)
	}
	if (show.only && show.age)
		invisible(return(ats)) ## crosstab results

	## If user specifies all ageing methods that occur in the data object, skip the next three logical tests (RH 201021)
	if (!all(dat$ameth %in% ameth)) {
		## Logical vector for desired surface ages (usually very young fish)
		if ( any(ameth%in%c(3,17)) && !is.null(use.sfc.age) && !all(is.na(use.sfc.age)) )
			z.sf = is.element(dat$ameth, c(1,16)) & is.element(dat$age, use.sfc.age)
		else z.sf = rep(FALSE, nrow(dat))

		## Logical vector for unknown ageing method
		if ( any(ameth%in%c(3,17)) && !is.null(ameth) && !all(is.na(ameth)) && "year"%in%colnames(dat) && "oto"%in%colnames(dat) )
			z.ua = is.element(dat$ameth,0) & is.element(dat$oto,1) & dat$year>=1980
		else z.ua = rep(FALSE, nrow(dat))

		## Logical vector for ageing method
		if ( !is.null(ameth) && !all(is.na(ameth)) )
			z.am = is.element(dat$ameth, ameth)
		else z.am = rep(TRUE, nrow(dat))
#browser();return()

		## Select ages based on logical vectors for ameth
		dat = dat[(z.sf|z.ua|z.am),]  ## RH 201021
	}
	## Check for years that only have ages 1-3 surface-read otoliths ## RH 230201
	ages.year = split(dat$age,dat$year)
	z.yr      = sapply(ages.year,function(x){!all(x<=3)})
	ok.yr     = names(z.yr)[z.yr]
	dat = dat[is.element(dat$year,ok.yr),]

	## Logical vector for sex
	if ( !is.null(sex) && !all(is.na(sex)) )
		z.sx = is.element(dat$sex, sex)
	else z.sx = rep(TRUE, nrow(dat))

	## Select ages based on logical vector for sex
	#dat = dat[(z.sf|z.ua|z.am) & z.sx,]
	dat = dat[z.sx,]  ## RH 201021
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~extractAges


#genPa----------------------------------2013-01-18
# Generate proportions-at-age using the catch curve 
# composition of Schnute and Haigh (2007, Table 2).
#   np    : number of age classes
#   theta : {Z, mu, sigma, bh, rho, tau}
#   sim   : logical, if TRUE return components of the simulated proportions-at-age.
#-------------------------------------------JTS/RH
genPa <- function(np=40, 
   theta=list(Z=0.1,mu=15,sigma=5,bh=c(10,20),rho=c(3,2),tau=1.5), sim=TRUE) 
{
	unpackList(theta)
	if (length(bh)!=length(rho))
		showError("Lengths of 'bh' and 'rho'\n(ages at and magnitudes of recruitment anomalies, respectively)\nmust be the same",as.is=TRUE)
	if (sim)
		assign("PBStool",list(module="M02_Biology",call=match.call(),args=args(genPa),np=np,theta=theta),envir=.PBStoolEnv)
	m  = length(bh)                                # number of recruitment anomalies
	B  = np; a  = 1:B                              # maximum age used in model
	Sa = GT0(exp(-Z*a))                            # survival (T2.1)
	Ba = rep(1.0,B)                                # initialise the selectivity vector
	Ba[a<mu] = exp(-(a[a<mu]-mu)^2 / sigma^2)      # selectivity (F.7, POP 2010, left side of double normal)
	Ba = GT0(Ba)                                   # make sure selectivity at age is always greater than zero
	Rm = vector("numeric",B)                       # initialise the recruitment vector
	for (i in 1:m)
		Rm = Rm+rho[i]*exp(-0.5*((a-bh[i])/tau)^2)  # combined effect of recruitment anomalies (T2.3)
	Ra = 1 + Rm                                    # recruitment effect (T2.3)
	pa = Sa*Ba*Ra; pa = pa/sum(pa)                 # aggrgegation of survival, selectivity, and recruitment (T2.4)
	if (sim) {
		om =c("Sa","Ba","Ra","pa")
		#for (i in om) eval(parse(text=paste("PBStool$",i,"<<-",i,sep=""))) }
		ttget(PBStool)
		for (i in om) PBStool[[i]] <- get(i)
		ttput(PBStool)
	}
	return(pa) 
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~genPa


## getPrey------------------------------2019-12-12
##  Get stomach contents (if any) that match the
##  SPECIMEN_ID in a bioDat object ('gfb_bio.sql')
## ---------------------------------------------RH
getPrey <- function(bioDat)
{
	getData("B05e_Specimen_Stomach","GFBioSQL")
	stomachs = PBSdat
	z = is.element(stomachs$SPECIMEN_ID,bioDat$SPID)
	spp.stomachs = stomachs[z,]
	prey = table(spp.stomachs$SPECIES_CODE)
	getData("C_Species","GFBioSQL")
	spp  = PBSdat$SPECIES_DESC; names(spp) = PBSdat$SPECIES_CODE
	spp.prey = rev(sort(prey))
	names(spp.prey) = spp[names(spp.prey)]
	spp.out = t(t(spp.prey))
	res.getPrey = list(stomachs=stomachs, prey=prey, spp=spp, spp.prey)
	ttput(res.getPrey)
	print(spp.out)
	return(spp.out)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~getPrey


#histMetric-----------------------------2013-01-28
# Create a matrix of histograms for a specified metric
#-----------------------------------------------RH
histMetric <- function(dat=pop.age, xfld="age", xint=1, minN=50,
	ttype=1:4, year=NULL, plus=NULL, ptype="bars", allYR=FALSE, allTT=FALSE,
	major=NULL, minor=NULL, locality=NULL, srfa=NULL, srfs=NULL,
	xlim=NULL, ylim=NULL, pxlab=c(.075,.85),axes=TRUE,
	fill=FALSE, bg="grey90", fg="black", 
	hrow=0.75, hpage=8, wmf=FALSE, pix=FALSE, ioenv=.GlobalEnv)
{
	xlab=ifelse(xfld=="len","Length (cm)",ifelse(xfld=="age","Age (y)","Metric"))
	assign("PBStool",list(module="M02_Biology",call=match.call(),args=args(histMetric),ioenv=ioenv,xlab=xlab),envir=.PBStoolEnv)
	dat=as.character(substitute(dat)); spp=substring(dat,1,3)
	expr=paste("getFile(",dat,",senv=ioenv,use.pkg=TRUE,try.all.frames=TRUE,tenv=penv()); dat=",dat,sep="")
	eval(parse(text=expr))
	dat$x=dat[,xfld]; dat=dat[!is.na(dat$x),]
	if(any(ttype==14)) dat$ttype[is.element(dat$ttype,c(1,4))]=14
	if(any(ttype==23)) dat$ttype[is.element(dat$ttype,c(2,3))]=23

	areas=c("major","minor","locality","srfa","srfs"); yarea=character(0)
	flds=c("year","ttype",areas)
	for (i in flds) {
		expr=paste("dat=biteData(dat,",i,")",sep=""); eval(parse(text=expr))
		if (any(i==areas)) eval(parse(text=paste("yarea=union(yarea,",i,")"))) }
	if (nrow(dat)==0) {
		if (wmf) return()
		else showError("No records selected for specified qualification") }
	ylab=paste(ifelse(ptype=="csum","Cumulative","Relative"),"Frequency",
		ifelse(length(yarea)==0,"",paste("(areas: ",paste(yarea,collapse=","),")",sep="")))
	if (!allTT) ttype=sort(unique(dat$ttype))
	ntt=length(ttype)

	if(xfld=="len") { dat$x=dat$x/10 } #; dat=dat[dat$x<=70,] }
	if (!is.null(plus) && plus>0) dat$x[dat$x>=plus]=plus
	dat$xbin = ceiling(dat$x/xint) * xint
	if (is.null(xlim)) xlim=c(0,max(dat$xbin))
	xpos=pretty(xlim,n=10); xpos=xpos[xpos>=0]

	xy=as.list(ttype); names(xy)=ttype
	if (ptype=="csum") ylim=c(0,1)
	if (is.null(ylim)) {ylim=c(0,-Inf); calcylim=TRUE} else calcylim=FALSE
	YRS=NULL
	for (i in ttype) {
		ii=as.character(i)
		idat=dat[is.element(dat$ttype,i),]
		bdat=split(idat$xbin,idat$year) # binned data
		xdat=split(idat$x,idat$year) # original data
		N=sapply(bdat,length)
		z=N>=minN; bdat=bdat[z]; 
		yrs=as.numeric(names(bdat)); nyr=length(yrs)
		YRS=union(YRS,yrs); NYR=length(YRS)
		xy[[ii]]=as.list(yrs); names(xy[[ii]])=yrs
		for (j in yrs) {
			jj=as.character(j)
			jdat=bdat[[jj]]
			bins=split(jdat,jdat)
			y=sapply(bins,length); Y=sum(y); y=y/Y; 
			x=as.numeric(names(y))
			if (calcylim) {ylim[1]=min(y,ylim[1]); ylim[2]=max(y,ylim[2])}
			xy[[ii]][[jj]] = list(x=x,y=y,n=Y,mn=mean(xdat[[jj]]))
		}
	}
	ypos=pretty(ylim,n=5); #ypos=ypos[-1]
	YRS=sort(YRS)
	if (allYR) {
		if (!is.null(year)) YRS=min(year):max(year)
		else YRS=YRS[1]:YRS[length(YRS)] }
	NYR=length(YRS)

	if (ntt==1) {
		rc=.findSquare(NYR) 
		m=rc[1]; n=rc[2] }
	else {
		m=NYR; n=ntt }
	fnam=paste(spp,"-",xfld,"-",ptype,"-tt",paste(ttype,collapse=""),
		"-bin",xint,"-year(",YRS[1],"-",YRS[NYR],")",ifelse(length(yarea)==0,"",paste("-area(",
		paste(yarea,collapse=","),")",sep="")),sep="")
	if (wmf && .Platform$OS.type=="windows")
		do.call("win.metafile",list(filename=paste(fnam,".wmf",sep=""),width=6.5,height=min(hrow*m,hpage)))
	else if (pix) png(paste(fnam,".png",sep=""),units="in",res=300,width=6.5,height=min(hrow*m,hpage))
	else resetGraph()

	if (ntt==1)
		expandGraph(mfrow=c(ifelse(wmf,n,m),ifelse(wmf,m,n)),mar=c(0,0,0,0),oma=c(4,5,0.5,0.5))
	else
		expandGraph(mfcol=c(m,n),mar=c(0,0,0,0),oma=c(4,ifelse(axes,5,2),2,0.5))
	for (i in ttype) {
		ii=as.character(i)
		tlab=c("Non-Obs Comm","Research","Charter","Obs Comm","Commercial","Research")
		names(tlab)=c(1:4,14,23)
		for (j in YRS) {
			jj=as.character(j)
			xyij=xy[[ii]][[jj]]
			if (is.null(xyij)) 
				plot(0,0,type="n",xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="",axes=axes)
			else {
				x=xyij$x; y=xyij$y; n=xyij$n; mn=xyij$mn
				if (ptype=="csum") y=cumsum(y)
				plot(x,y,type="n",xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="",axes=axes)
				abline(h=ypos,v=xpos,col="grey95")
				if (ptype=="bars") {
					if (fill) {
						xpol=as.vector(sapply(x,function(x,xoff){c(x+c(-xoff,xoff,xoff,-xoff),NA)},xoff=xint/2))
						ypol=as.vector(sapply(y,function(y,base){c(base,base,y,y,NA)},base=0))
						polygon(xpol,ypol,col=bg,border=fg) }
					else
						drawBars(x,y,width=xint) }
				else if (ptype=="csum") {
					md=approx(y,x,.5,rule=2,ties="ordered")$y
					lines(x,y,col=fg) 
					points(md,.5,pch=21,bg=bg,cex=1.2)
					text(md+2,.5,round(md,1),adj=0,cex=1.2/sqrt(m^0.1)) }
				else if (ptype=="vlin") lines(x,y,type="h",col=fg)
			}
			if (axes | par()$mfg[1]==par()$mfg[3])
				axis(1,at=xpos,cex.axis=1.2,tck=.01,mgp=c(0,.5,0),labels=ifelse(par()$mfg[1]==par()$mfg[3],TRUE,FALSE))
			if (axes)
			axis(2,at=ypos,las=1,cex.axis=1+1/m,tck=.02,adj=1,mgp=c(0,.5,0),
				labels=ifelse(par()$mfg[2]==1 && par()$mfg[1]%%2==1,TRUE,FALSE))
			if (ntt==1 || par()$mfg[2]==1)
				addLabel(.06,.95,jj,adj=c(0,1),col="blue",cex=1.5/(m^0.1))
			if (ntt>1 && par()$mfg[1]==1)
				mtext(tlab[ii],side=3,line=0.2,col="blue",cex=.8)
			if (axes && !is.null(xyij)) 
				addLabel(pxlab[1],pxlab[2],paste("n",n,ifelse(ptype=="csum","",
					paste(",",toupper(substring(xfld,1,1)),round(mn)))),adj=c(0,1),col="green4",cex=1.2/(m^0.1)) 
			if (axes) box()
		} # end j loop
	} # end i loop
	mtext(xlab,outer=TRUE,side=1,las=1,line=2.5,cex=1.25)
	mtext(ylab,outer=TRUE,side=2,las=0,line=ifelse(axes,3.25,.25),cex=1.25)
	if (wmf|pix) dev.off()

	stuff=c("dat","xy","YRS","ttype","xlim","ylim")
	packList(stuff,"PBStool",tenv=.PBStoolEnv)
	invisible() 
}
#---------------------------------------histMetric


## histTail-----------------------------2024-10-24
## Create a histogram showing tail details
## ---------------------------------------------RH
histTail <- function(dat=pop.age, xfld="age", tailmin=NULL, 
   xlim=c(0,100), bcol="gold", tcol="moccasin",
   wmf=FALSE, png=FALSE, pngres=400, PIN=c(6.5,3.5), 
   ioenv=.GlobalEnv, outnam, lang=c("e","f"), ...)
{
	## Create a subdirectory called `french' for French-language figures
	createFdir(lang)

	assign("PBStool",list(module="M02_Biology",call=match.call(),args=args(histTail),ioenv=ioenv),envir=.PBStoolEnv)
	dat=as.character(substitute(dat))
	expr=paste("getFile(",dat,",senv=ioenv,use.pkg=TRUE,try.all.frames=TRUE,tenv=penv()); dat=",dat,sep="")
	eval(parse(text=expr))
	if (!requireNamespace("MASS", quietly=TRUE)) showError("`MASS` package is required for `truehist`")

	prob = zoomprob=list(...)[["prob"]]
	if(is.null(prob)) { prob=FALSE; zoomprob=FALSE }
	x    = dat[,xfld]
	x    = x[!is.na(x)]
	nx   = length(x)
	nyr  = table(dat$year)
	xlab = paste(toupper(substring(xfld,1,1)),substring(xfld,2),sep="",collapse=" ")
	spp  = attributes(dat)$spp
	if (missing(outnam)) {
		#outnam = paste0(spp,"-Hist-",xlab)
		#outnam = paste0("histTail-",strSpp,"-",gsub("\\s+","_",stock),"-",toUpper(xfld))
		outnam = paste0("histTail(",strSpp,")-",gsub("\\s+","",stock),"-",toUpper(xfld))
	}
	fout.e = outnam
	for (l in lang) {
		changeLangOpts(L=l)
		#fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		fout = switch(l, 'e' = paste0("./english/",fout.e), 'f' = paste0("./french/",fout.e) )
		if (wmf && .Platform$OS.type=="windows")
			do.call("win.metafile",list(filename=paste0(fout,".wmf"), width=PIN[1], height=PIN[2]))
		else if (png){
			clearFiles(paste0(fout,".png"))
			png(paste0(fout,".png"), units="in", res=pngres, width=PIN[1], height=PIN[2])
		}
		else resetGraph(reset.mf=FALSE)
		expandGraph(mfrow=c(1,1), mar=c(2.75,4,1,1), oma=c(0,0,0,0), las=1, xaxs="i", yaxs="i", cex.axis=0.9, cex.lab=1.1)
		
		truehist = MASS::truehist
		#evalCall(truehist,argu=list(data=x,col=bcol,cex.lab=1.2,xlab=xlab),...,checkpar=TRUE)
		#do.call(truehist, args=list(data=x, prob=prob, nbins=50, col=bcol, cex.lab=1.2, xlab=linguaFranca(xlab,l)))
		xoff = -0.5
		xmax = ceiling(max(max(x),xlim[2]))
		do.call(truehist, args=list(data=x, prob=prob, breaks=seq(1,xmax,1), xlim=xlim, xaxt="n", col=bcol, xlab=linguaFranca(xlab,l)))
		axis(1, at=seq(5+xoff,xmax+xoff,5), labels=FALSE, tcl=-0.25)
		axis(1, at=seq(10+xoff,xmax+xoff,10), labels=seq(10,xmax,10), tcl=-0.5)
		ylab = paste(ifelse(prob,ifelse((wmf|png)&&PIN[2]<3.5,"Rel. Freq.","Relative Frequency"),"Frequency")," ( N = ",format(nx,scientific=FALSE,big.mark=options()$big.mark)," )",sep="")
		mtext(linguaFranca(ylab,l), side=2, line=2.75, cex=par()$cex.lab, las=0)
		data("species", package="PBSdata", envir=penv())
		if (xfld=="len") { xpos=0.05; ypos=0.95; adj=0}
		else             { xpos=0.95; ypos=0.25; adj=1}
		if (strSpp=="REBS") {
			addLabel(xpos,ypos, linguaFranca(paste0(strSpp," ",switch(stock, 'BSR'="North", 'RER'="South", "Coast")),l), col="black", cex=1.2, adj=adj)
		} else {
			gobs = strsplit(outnam,split="\\(|\\)|-")[[1]]
			gobs = grep("^$|histTail",gobs,invert=TRUE,value=TRUE)
			glab = paste0(toUpper(species[gobs[1],"name"])," - ", paste0(gobs[2:3], collapse=" ("), ")")
			addLabel(xpos,ypos, linguaFranca(glab,l), col="black", cex=1.2, adj=adj)
		}
		if (!is.null(tailmin)){
			if (tailmin<=0) {
				qstart  = 0.975
				tailmin = floor(quantile(x, qstart))
			} else {
				qstart = signif(ecdf(x)(tailmin),3)
			}
			z = x>=tailmin & !is.na(x); nz=sum(z)
			brks = ((tailmin-1):ceiling(max(x[z])))+0.5
			par(new=TRUE, plt=c(0.7,0.975,0.6,0.9))
			evalCall(hist, argu=list(x=x[z], breaks=brks, probability=zoomprob, col=tcol,
				main=paste0(linguaFranca("Tail details",l)," (q=",qstart,")"), col.main="grey70", mgp=c(1.75,0.5,0),
				xlab=linguaFranca(xlab,l), ylab=linguaFranca(ifelse(prob,"Relative Frequency","Frequency"),l),
				las=ifelse(zoomprob,0,1), cex.main=0.9), ..., checkdef=TRUE, checkpar=TRUE)
			addLabel(0.95,0.7, linguaFranca(paste0("Min ", xfld, " = ", min(x[z]), "\nMax ",xfld, " = ", max(x[z]), "\nn = ",nz),l), col="grey60", cex=.8, adj=1) ## (RH 240305)
#browser();return()
		} ## end if tailmin
		if (wmf|png) dev.off()
	}; eop()
	stuff=c("x","nx","nyr","nz","brks","xlab","ylab")
	packList(stuff,"obj.histTail",tenv=.PBStoolEnv)
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~histTail


## mapMaturity--------------------------2024-10-24
## Plot maturity chart to see relative occurrence
## of maturity stages by month.
## Notes:
##  areas : code does not loop through separate areas but amalgamates them
##  type = "map" (tiles), "bubb" (bubbles)
##  If is.null(mats) then plot proportions by sex in areas (not maturities in one area)
##  Added ability to restrict survey data to synoptic surveys only (RH: 240312)
##  Added ability to display summaries by year instead of by month (RH: 240312)
## -----------------------------------------RH/PJS
mapMaturity <- function (dat=pop.age, strSpp="", type="map", mats=1:7,
   sex=list(Females=2), ttype=c(1:10,12:14), stype=c(1,2,6,7),
   areas=list(major=3:9), anams, synoptic.only=FALSE, period="month",
   stock, catch, brks=c(0,0.05,0.1,0.25,0.5,1), byrow=FALSE,
   clrs=list(colorRampPalette(c("honeydew","lightgreen","black"))(5),
   colorRampPalette(c("aliceblue","skyblue2","black"))(5)),
   outnam, eps=FALSE, png=FALSE, wmf=FALSE, pngres=400, PIN=c(8.5,6),
   ioenv=.GlobalEnv, lang=c("e","f"))
{
	if (is.null(mats) && type!="bubb")
		stop ("Set 'type=\"bubb\"' when 'mats=NULL' (i.e., plotting proportions by sex in areas)")
	## Check if user wants proportions by sex (NOT maturity map)
	is.psex = is.null(mats)# && is.list(areas)

	## Create a subdirectory called `french' for French-language figures
	createFdir(lang)

	assign("PBStool",list(module="M02_Biology", call=match.call(), args=args(mapMaturity), ioenv=ioenv), envir=.PBStoolEnv)
	dnam = as.character(substitute(dat))
	if (missing(outnam)) fnam = dnam
	expr=paste("getFile(",dnam,",senv=ioenv,use.pkg=TRUE,try.all.frames=TRUE,tenv=penv()); dat=",dnam,sep="")
	eval(parse(text=expr))

	flds=names(dat)
	attSpp=attributes(dat)$spp
	if (is.null(strSpp) || strSpp=="") {
		if (is.null(attributes(dat)$spp) || attributes(dat)$spp=="") strSpp="999"
		else strSpp=attSpp }
	else if (!is.null(attSpp) && attSpp!=strSpp)
		showError(paste("Specified strSpp '",strSpp,"' differs from species attribute '",attSpp,"' of data file",sep=""))
	if (any(is.element(c("spp","species","sppcode"),flds))) {
		fldSpp=intersect(c("spp","species","sppcode"),flds)[1]
		dat=dat[is.element(dat[,fldSpp],strSpp),] }

	mnam = mats
	if (!is.null(mats) && is.element(strSpp,as.character(394:453))) {
		mat1 <- c("Immature","Maturing","Developing","Developed","Running","Spent","Resting") # males
		mat2 <- c("Immature","Maturing","Mature","Fertilized","Embryos","Spent","Resting") # females
	} else if (!is.null(mats) && is.element(strSpp, as.character(c(602)))){
		mat1 <- c("Immature","Maturing","Developing","Ripe","Spawning","Spent","Resting") # males
		mat2 <- c("Immature","Maturing","Developing","Gravid","Ripe","Spent","Resting") # females
	} else if (!is.null(mats) && is.element(strSpp, as.character(c(228)))){
		## Walleye Pollock uses maturity convention 25 
		mat1 <- c("Immature","Maturing-small","Maturing-large","Mature","Ripe","Spent","Resting") # males
#browser();return()
		mat2 <- c("Immature","Maturing-small","Maturing-large","Mature","Ripe","Spent","Resting") # females
	} else if (is.psex) {
		## Proportions female or male by area
		mnam = sapply(areas,paste0,collapse="",simplify=FALSE)
		mat1 = paste0(sapply(1:length(areas),function(x){paste0(names(areas)[x],":\n", paste0(areas[[x]],collapse="+"))}) )
		mat2 = paste0(sapply(1:length(areas),function(x){paste0(names(areas)[x],":\n", paste0(areas[[x]],collapse="+"))}) )
	} else {
		mat1 = mat2 = c("Immature","Immature","Developing","Ripe","Spawning","Spent","Resting") # males & females
	}
	names(mat1) = names(mat2) = if (is.psex) as.character(mnam) else as.character(mats)
#browser();return()

	nsex <- length(sex)
	# Attention: colour handling still potentially a mess but OK for now.
	if (!is.list(clrs)) {
		if (length(clrs)!=(length(brks)-1)) stop ("Number of colours not right for `brks'")
		CLRS = sapply(1:nsex,function(x){return(clrs)},simplify=FALSE)
	} else {
		if (length(clrs)!=nsex)
			CLRS = rep(clrs[1],nsex)
		else {
			CLRS=list()
			for (i in 1:length(clrs))
				CLRS[[i]] = clrs[[i]]
		}
	}
	mos  <- 1:12
	mday <- c(31,28,31,30,31,30,31,31,30,31,30,31)
	mcut <- c(0,cumsum(mday))

	ncut <- length(brks)
	lout <- paste(show0(brks[1:(ncut-1)],2),show0(brks[2:ncut],2),sep="-")

	# Qualify the data as Paul Starr might
	z0 = if(!is.psex) is.element(dat$sex,.su(unlist(sex))) else is.element(dat$sex,1:2)
	z1 = if(!is.psex) is.element(dat$mat,mats) else rep(TRUE,nrow(dat))
	z2 = is.element(dat$ttype,ttype)
	z3 = is.element(dat$stype,stype)
	if (missing(stock) || is.null(stock)) {
		mess =  sapply(1:length(areas),function(x) {
			nx   = names(areas)[x]
			xx   = areas[[x]]
			is.c = is.character(xx)
			out  = paste0("is.element(dat$", nx, ifelse(is.c, ",\"", ","), areas[x], ifelse(is.c, "\")", ")"))
		})
		eval(parse(text=paste0("z4 = ",paste0(mess,collapse=" | "))))
#browser();return()
	} else {
		z4 = is.element(dat$stock, stock)
	}
#browser();return()
	dat = dat[z0&z1&z2&z3&z4,]
	dat$month <- as.numeric(substring(dat$date,6,7))
	dat$day   <- as.numeric(substring(dat$date,9,10))
	## Remove non-synoptic surveys
	if (synoptic.only)
		dat = dat[is.element(dat$SSID, c(1,3,4,16)),]
#browser();return()
	if (is.psex && !any(names(areas)=="region")) {
		dat$region = rep(NA,nrow(dat))
		for (m in areas) {
			zm = is.element(dat$major,m)
			dat$region[zm] = paste0(m, collapse="")
		}
	}

	## Limits for tiles
	xlim <- c(1,360)
	ylim <- if(!is.null(mats)) -rev(range(mats))  else c(1, length(mat2))
	ylim = ylim +  c(-1,1)
	xpos <- (mcut[1:12]+mcut[2:13])/2
	yspc <- 0.4

	CALCS = list() # to collect calculations (matrices primarily)

	if (missing(outnam)){
		fnam = paste0(ifelse(is.psex,"mapPsex-","mapMaturity-"), strSpp)
		if (!all((3:9) %in% .su(dat$major))) fnam = paste0(fnam, "-major(", paste0(.su(dat$major),collapse=""),")")
		fnam = paste0(fnam, "-ttype(", paste0(.su(dat$ttype),collapse=""),")")
		fnam = paste0(fnam,"-(by_",ifelse(byrow,"maturity)","month)"),"-(",dnam,")")
	}
	else fnam = outnam # user-specified output name
	#if (!missing(catch)) fnam = paste0(fnam,"-w(catch)")
	if (!missing(catch)) fnam = sub("maturity|month", "catch", fnam)
#browser();return()

	fout.e = fnam
	for (l in lang) {  ## could switch to other languages if available in 'linguaFranca'.
		changeLangOpts(L=l)
		#fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		fout = switch(l, 'e' = paste0("./english/",fout.e), 'f' = paste0("./french/",fout.e) )
		#devs=c(win=ifelse(missing(outnam) || sum(eps,png,wmf)==0,TRUE,FALSE),eps=eps,png=png,wmf=wmf); unpackList(devs)
		devs=c(win=ifelse(sum(eps,png,wmf)==0,TRUE,FALSE), eps=eps, png=png, wmf=wmf); unpackList(devs)
		for (d in 1:length(devs)) {
			dev = devs[d]; devnam=names(dev)
			if (!dev) next
			if (devnam=="eps"){
				clearFiles(paste0(fout,".eps"))
				postscript(paste0(fout,".eps"), width=PIN[1], height=PIN[2], horizontal=FALSE, paper="special")
			} else if (devnam=="png"){
				clearFiles(paste0(fout,".png"))
				png(paste0(fout,".png"), units="in", res=pngres, width=PIN[1], height=PIN[2])
			} else if (devnam=="wmf"){
				clearFiles(paste0(fout,".wmf"))
				do.call("win.metafile", list(filename=paste0(fout,".wmf"), width=PIN[1], height=PIN[2]))
			}
			else resetGraph()
			#left.margin = ifelse(is.psex,8,6) * ifelse(l=="f" && any(sapply(sex,function(x){any(1%in%x)})), 1.75, 1)  ## male labels are long
			#par(mfrow=c(nsex,1), mar = if(type=="map") c(1,4,0,0) else c(4,left.margin,0,0), oma=c(0,0,ifelse(is.psex,4,2),0))
			lm.default  = ifelse(strSpp %in% c("228"), 8, 6)
			left.margin = ifelse(is.psex,5,lm.default) * ifelse(l=="f" && any(sapply(sex,function(x){any(1%in%x)})), 1.75, 1)  ## male labels are long
			
			par(mfrow=c(nsex,1), mar = if(type=="map") c(1,4,0,0) else c(4,left.margin,0,0), oma=c(0,0,ifelse(is.psex,4,2),0))

			for (s0 in 1:nsex) {
				ss = sex[[s0]]
				sexlab = names(sex)[s0]
				sexcol = CLRS[[s0]][ncut-2]
				sdat <- dat[is.element(dat$sex,ss),]
				if (type=="map") {
					plot(0,0, type="n", xlim=xlim, ylim=ylim, xlab="", ylab="", axes=FALSE)
					axis(1, at=xpos, labels=linguaFranca(month.abb,l), tick=FALSE, pos=ifelse(devnam=="win",-7.5,-7.0), cex.axis=ifelse(nsex==1,1.2,1.2))
					mcode <- get(paste("mat",ss,sep=""))
					axis(2, at=-mats, labels=mcode[mats], adj=1, cex=1.2, tick=FALSE, pos=xlim[1], las=1)
					if (byrow) {
						ival=mats; ifld="mat"; ffld=period
					} else {
						ival=mos; ifld=period; ffld="mat"
					}
					for (i in ival) {
						idat <- sdat[is.element(sdat[,ifld],i),]
						if (nrow(idat)==0) next
						ibin <- split(idat$day,idat[,ffld])
						icnt <- sapply(ibin,length)
						icnt <- icnt/sum(icnt)
						iclr <- as.numeric(cut(icnt,breaks=brks)); names(iclr)=names(icnt)
						for (j in 1:length(iclr)) {
							jj  <- iclr[j]
							jjj <- as.numeric(names(jj))
							if (byrow) {
								x = c(mcut[jjj],mcut[jjj+1],mcut[jjj+1],mcut[jjj])
								y = rep(i,4) + c(-yspc,-yspc,yspc,yspc); y <- -y
							} else {
								x = c(mcut[i],mcut[i+1],mcut[i+1],mcut[i])
								y = rep(jjj,4) + c(-yspc,-yspc,yspc,yspc); y <- -y
							}
							polygon(x,y,col=CLRS[[s0]][jj],border="grey50")
						}
					}
					addLegend(0.2,1, legend=linguaFranca(lout,l), fill=CLRS[[s0]], cex=0.9, horiz=TRUE, bty="n")
				} ## end map type
				if (type=="bubb") {
					mcode = get(paste("mat",ss,sep=""))
					## Note that xfac and yfac are reversed for some odd reason
					yfac  = if (period=="year")  min(dat$year):max(dat$year) else 1:12 ## default to month (RH 240312)
#browser();return()
					bubbmat   = array(0,dim=c(length(mnam),length(yfac)),dimnames=list(mnam,yfac)) ## make 0 matrix
					if (is.psex) {
						xnam  = areas
						names(xnam) = xfac = unlist(mnam)
						crossbubb   = as.list(rep(NA,length(xfac))); names(crossbubb) = xfac
						for (i in 1:length(xfac)) {
							f   = xfac[i]
							ff  = xnam[[i]]
							fff = names(xfac)[i]
							fdat = dat[is.element(dat[,fff],ff),]
							fdat$xfac = f
							crossbubb[[f]] = crossTab(fdat, c("xfac",period),"sex",function(x){sum(is.element(x,ss))/length(x)})
						}
						#crossbubb = crossTab(dat,c("region",period),"sex",function(x){sum(is.element(x,ss))/length(x)})
					} else {
						crossbubb = list(mats=crossTab(sdat,c("mat",period),"mat",length))  ## artificially make into a list
					}
					## next line only needed if hadley=T in crossTab
					#bubbdat   = data.frame(crossbubb[,-1],row.names=crossbubb[,1],check.names=FALSE,stringsAsFactors=FALSE)
					for (i in 1:length(crossbubb)) {
						ibubb = crossbubb[[i]]
						rows  = intersect(rownames(bubbmat),rownames(ibubb))
						cols = intersect(colnames(bubbmat),colnames(ibubb))
						## need to populate like with like, i.e., matrices
						bubbmat[rows,cols]=as.matrix(ibubb[rows,cols])
					}
					if (is.psex) {
						fishnum = table(dat[,period])
						fishsum = rep(0,length(yfac)); names(fishsum) = yfac
						fishsum[names(fishnum)] = fishnum
						sampson = crossTab(dat,period,"SID",function(x){length(unique(x))})
					} else {
						fishsum = apply(bubbmat,2,sum) # number of specimens by month
						sampson = crossTab(sdat,period,"SID",function(x){length(unique(x))})
					}
					sampsum = rep(0,length(yfac)); names(sampsum) = yfac
					sampsum[names(sampson)] = sampson
					if (!is.psex && !missing(catch)) {
						crosscat = crossTab(catch, period, "catKg")
						monthcat  = rep(0,length(yfac)); names(monthcat) = yfac
						monthcat[names(crosscat)] = crosscat
						catprop = monthcat/sum(monthcat)
						bubbmat.orig = bubbmat
						CALCS[[sexlab]][["monthcat"]] = monthcat
						CALCS[[sexlab]][["catprop"]] = catprop
						CALCS[[sexlab]][["bubbmat.orig"]] = bubbmat.orig
						bubbmat = sweep(bubbmat,2,catprop,"*")
						zbig = bubbmat==max(bubbmat)
						nbig = bubbmat.orig[zbig]
						cbig = monthcat[apply(zbig,2,any)]
						mbig = linguaFranca(month.abb[as.numeric(names(cbig))],l)
						lout = paste0("Bubbles: largest = ", nbig, " specimens weighted by catch in ", mbig, " = ", round(cbig), " t")
					} else {
						if (is.psex)
							freqmat = bubbmat
						else
							freqmat = apply(bubbmat,ifelse(byrow,1,2),function(x){if (all(x==0)) x else x/sum(x)})  # proportions by column
						lout = paste0("Bubbles: largest = ",round(max(freqmat),3),", smallest = ",round(min(freqmat[freqmat>0]),3))
					}
					CALCS[[sexlab]][["bubbmat"]] = bubbmat

					matdat = crossTab(sdat,c(period,"mat","ttype"),"mat",length)
					CALCS[[sexlab]][[paste0("sex",ss,"matdat")]] = matdat
					save("matdat", file=paste0("sex",ss,"matdat.rda"))
#browser();return()

					## Limits for bubbles
					xlim = range(yfac) + c(-0.25,0.25)
					if (is.psex) {
						yrng=c(length(mnam),1)
						ylim = yrng + c(0.25,-0.25)
					} else {
						yrng=c(rev(mats)[1],mats[1])
						ylim = yrng - min(mats) + 1 + c(0.25,-0.75)
					}
					if (!missing(catch)) {
						plotBubbles(bubbmat, xlim=xlim, ylim=ylim, xaxt="n", yaxt="n", cpro=FALSE, rpro=FALSE, hide0=TRUE, size=0.3, lwd=2, clrs=rev(CLRS[[s0]])[2], frange=0.02)
						addLegend(0.425, 1, legend=linguaFranca(lout,l), cex=0.9, horiz=TRUE, bty="n", xjust=0.5, yjust=0.75)
					} else {
						if (is.psex) {
							plotBubbles(bubbmat, xlim=xlim, ylim=ylim, xaxt="n", yaxt="n", cpro=FALSE, rpro=FALSE, hide0=TRUE, size=0.3, lwd=2, clrs=rev(CLRS[[s0]])[2], frange=0.02, dnam=ifelse(period=="year",TRUE,FALSE))
#browser();return()
						} else {
							plotBubbles(bubbmat, xlim=xlim, ylim=ylim, xaxt="n", yaxt="n", cpro=ifelse(byrow,FALSE,TRUE), rpro=ifelse(byrow,TRUE,FALSE), hide0=TRUE, size=0.3, lwd=2, clrs=rev(CLRS[[s0]])[2], frange=0.02)
							addLegend(0.2, 1, legend=linguaFranca(lout,l), cex=0.9, horiz=TRUE, bty="n", yjust=0.75)
						}
					}

					## Add proportion labels to each bubble
					if (is.psex) {
						sapply(1:length(mnam), function(x,pmat){
							p = pmat[x,]; z=p>0
							text((yfac)[z], x, round(p[z],2), cex=0.8, col="grey20")
						}, pmat=bubbmat)
					}
					box(col="white",lwd=2) ## mask box outline
#browser();return()
					axis(1, at=yfac, labels=paste0(linguaFranca(if(period=="year") yfac else month.abb,l),"\n{",sampsum,"}\n(",fishsum,")"), padj=0.5, cex.axis=ifelse(devnam=="win",0.9,0.8))
					if (!missing(anams)){
						anams = rep(anams,length(mcode))[1:length(mcode)]
						mcode = anams
						#mcode = paste0(anams,gsub("\\+","",sub("^.+:\\n","\nm:",mcode)))
					}
					axis(2, at=1:length(mnam), labels=linguaFranca(mcode,l), las=1, cex.axis=1.2)
				} ## end bubb type
				mtext(linguaFranca(sexlab,l), side=3, line=ifelse(is.psex,0,-1.25), col=sexcol, cex=1.5, adj=ifelse(devnam=="win",-0.06,-0.10), font=2)
				#box() ## to help debug margins
			}
			if (is.psex)
				mtext(linguaFranca("Proportions by Sex",l), side=3, line=2, col=1, cex=1.5, adj=0.5, font=2, outer=TRUE)
			else
				mtext(linguaFranca(paste0("Relative Frequency ",ifelse(!missing(catch),"Weighted by Catch",ifelse(byrow,"by Maturity","by Month"))),l), side=3, line=0.5, col=1, cex=1.5, adj=0.5, font=2, outer=TRUE)
			if (devnam!="win") dev.off()
#browser();return()
		} ## end d (devs) loop
	}; eop()
	stuff=c("xlim","ylim","x","y","sdat","mday","mcut","idat","ibin","icnt","iclr","strSpp")
	packList(stuff,"PBStool",tenv=.PBStoolEnv)
	invisible(CALCS) 
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~mapMaturity


## plotAdens----------------------------2024-01-03
##  Plot density of field (length) by age as waveforms.
## ---------------------------------------------RH
plotAdens <- function(dat, xfld="len", yfld="age", type="dens", sd=3,
	strSpp="417", bysex=TRUE, stype=c(1,2,6:8),
	xlim=NULL, ylim=NULL, yspan=2, yrel=TRUE, nmin=1,
	dcol, png=FALSE, pngres=400, PIN=c(8,8), ...)
{
	dots   = list(...)
	pdots  = dots[intersect(names(dots),names(par()))]
	if (yfld %in% c("depth","fdep") && "fdep" %in% colnames(dat)) {
		dbin = 10
		dat$depth = ceiling(dat$fdep/dbin) * dbin
	} 
	dat = dat[dat[,xfld]>0 & !is.na(dat[,xfld]) & dat[,yfld]>0 & !is.na(dat[,yfld]),]
	dat = dat[is.element(dat$stype,stype),]
	if (bysex && "sex" %in% colnames(dat)) {
		sex = c(2,1)
		dat = dat[is.element(dat$sex,sex),]
	}
	if (yfld=="age" && "ameth" %in% colnames(dat)) {
		zam = is.element(dat$ameth,3)
		zam = zam | (is.element(dat$ameth,0) & dat$year>=1980)
		zam = zam | (is.element(dat$ameth,1) & dat$age<=3)  ## SCL uses surface ageing for very young fish
		dat = dat[zam,]
	}
	if ("SSID" %in% names(dots) && "SSID" %in% colnames(dat))
		dat = dat[is.element(dat$SSID, dots$SSID),]
	ttput(dat)
#browser();return()

	if (bysex) {
		sdata = split(dat, dat$sex)
	} else {
		sdata = list()
		sdata[["0123"]] = dat
	}
	ydata = lapply(sdata,function(x){split(x[,xfld], x[,yfld])})
	ndata = lapply(ydata,function(x){sapply(x,countVec)})
	zuse  = lapply(ndata,function(x){x >= nmin})
	for (i in 1:length(ydata)){
		ii = names(ydata)[i]
		ydata[[ii]] = ydata[[ii]][zuse[[ii]]]
		ndata[[ii]] = ndata[[ii]][zuse[[ii]]]
	}
	ydens = lapply(ydata, function(y){
		lapply(y, function(x){
		if (length(x)==1) {
			return(list(x=x,y=1,n=1,counts=1))
		} else {
			if (type=="dens") {
				if (!is.null(sd)) {
					sdr = range(rnorm(1000, mean=mean(x), sd=sd))
					z   = x >= sdr[1] & x <= sdr[2]
					x   = x[z]
				}
				out = density(x,bw="nrd0",kernel="gaussian")
			} else if (type=="bars") {
				xrng = range(x,na.rm=TRUE)
				out = hist(x, breaks=seq(floor(xrng[1]),ceiling(xrng[2]),1), plot=FALSE)
				out$x = out$mids
				out$y = out$dens
				out$n = length(out$mids)
#if (length(x)>20) {browser();return()}
			}
			return(out)
		}
		})
	})
#browser();return()
	ymax  = lapply(ydens, function(y){ sapply(y, function(x){max(x$y)}) })
	Ymax  = max( sapply(ymax, function(y){ max(y[y<1]) }) )
	nmax  = lapply(ydens, function(y){ sapply(y, function(x){return(max(x$y/sum(x$y)*x$n))}) })  ## wtf?
	Nmax  = max( sapply(nmax, function(n) { max(n) }) )
	fmax  = lapply(ydens, function(y){ sapply(y, function(x){ xmax=max(x$counts,na.rm=TRUE); return(xmax)}) })
#browser();return()
	Fmax  = max( sapply(fmax, function(n) { max(n) }) )
	
	yvals = lapply(ydens, function(y){ as.numeric(names(y)) })
	ndens = lapply(ydens, function(y){ length(y) })
	if (is.null(xlim)){
		xlim  = range(dat[,xfld]); xlim[1]=0
	}
	if (is.null(ylim)){
		ylim  = rev(range(yvals)) + c(0, -yspan)
	}
	if (missing(dcol)) {
		ramp = list()
		if (length(ydata)==1){
			ramp[["0123"]] = colorRampPalette(c("cyan","blue","navy"))
		} else {
			#if ("0"%in%names(ydata)) ramp[["0"]] = colorRampPalette(c("cyan","blue","navy"))
			#if ("1"%in%names(ydata)) ramp[["1"]] = colorRampPalette(c("green","green3","darkgreen"))
			#if ("2"%in%names(ydata)) ramp[["2"]] = colorRampPalette(c("orchid1","orchid3","darkorchid4"))
			if ("0"%in%names(ydata)) ramp[["0"]] = colorRampPalette(c("green","green3","darkgreen"))
			if ("1"%in%names(ydata)) ramp[["1"]] = colorRampPalette(c("cyan","blue","navy"))
			if ("2"%in%names(ydata)) ramp[["2"]] = colorRampPalette(c("pink","red","darkred"))
		}
		dcol = lapply(ramp, function(f){ f(length(.su(dat[,yfld]))) })
	} else if (!is.list(dcol)) {
		dcol.usr = dcol
		dcol = list()
		dcol[["0123"]] = rep(dcol.usr,max(dat[,yfld]))
	} else {
		dcol = lapply(dcol, function(x){ rep(x,length(.su(dat[,yfld]))) })
	}

	fout = paste0("Adens",strSpp)
	if (png) png(filename=paste0(fout,".png"), units="in", res=pngres, width=PIN[1], height=PIN[2])
	par(mfrow=c(1,1), mar=c(3.5,4,1,1), oma=c(0,0,0,0), mgp=c(2,0.5,0))
	plot(NA, xlim=xlim, ylim=ylim, type="n", xlab="", ylab="", yaxt="n")
	abline(h=.su(unlist(yvals)), col="gainsboro", lty=3)
	for (s in 1:length(ydata)) {
		ss = names(ydata)[s]
		for (i in 1:ndens[[ss]]) {
			ii  = yvals[[s]][i]
			iii = as.character(ii)
			idens = ydens[[ss]][[i]]
			i.x = idens$x
#browser();return()
			if (length(i.x)==1){ # && type=="dens") {
				i.y = ii
				points(i.x, ii - i.y + ii, pch=3, col=switch(ss, '0'="green3", '1'="blue",'2'="red", '3'="purple"))
			} else {
				if (yrel)
					yoff = yspan * ymax[[s]][iii] / Ymax
				else
					yoff = yspan * nmax[[s]][iii] / Nmax
				if (type=="dens") {
					i.y = scaleVec(idens$y, ii, ii+yoff) #+ yoff)#  * (ymax[iii] / Ymax)
					lines(i.x, ii - i.y + ii, col=dcol[[s]][i], pdots)
				}
				if (type=="bars"){
					if (ii>ylim[1] || ii<ylim[2]) next  ## ylim is reversed
					i.y = scaleVec(c(0,idens$counts), ii, ii+yoff)[-1] #+ yoff)#  * (ymax[iii] / Ymax)
					#drawBars(i.x + switch(ss,'0'=0,'1'=-0.25,'2'=0.25), -(i.y-ii)+ii, col=dcol[[s]][i], width=1, base=ii, ...)
#browser();return()
					#drawBars(i.x + switch(ss,'0'=0,'1'=-0.25,'2'=0.25), -(i.y-ii)+ii, col="transparent", fill=dcol[[s]][i], width=0.5, base=ii, unlist(pdots))
					do.call(drawBars, args=c(list(x=i.x + switch(ss,'0'=0,'1'=-0.25,'2'=0.25), y=-(i.y-ii)+ii, col="transparent", fill=dcol[[s]][i], width=0.5, base=ii), pdots))
					#.flush.cat(dcol[[s]][i],"\n")
				}
#print(i)
#if (ii=="150"){browser();return()}
			}
			i.n = sum(idens$counts); idens$n
			#text (xlim[switch(ss,'0'=1,'1'=1,'2'=2)], ii, i.n, col=switch(ss,'0'="green4",'1'="blue",'2'="red"), adj=c(1,0), cex=0.6)
			text (xlim[switch(ss,'0'=1,'1'=1,'2'=2)], ii, i.n, col=rev(dcol[[ss]])[1], adj=c(1,0), cex=0.8)
#browser();return()
#if (i==2) {browser();return()}
		} ## end i loop (ndens)
	} ## end s loop (sex)
	yint = ifelse(length(.su(dat[,yfld][dat[,yfld]<=ylim[1] & dat[,yfld]>=ylim[2]])) <=10, 1, 5)
	axis(2, at=seq(yint,max(dat[,yfld]),yint), mgp=c(2,0.5,0), las=1)
	xlab = switch(xfld, 'len'="Length (cm)", 'Age'="Age (yr)")
	ylab = switch(yfld, 'Age'="Age (yr)", 'year'="Year", 'depth'="Depth (m)")
	mtext(xlab, side=1, line=2, cex=1.5)
	mtext(ylab, side=2, line=2.5, cex=1.5, las=0)
	data (species, package="PBSdata", envir=penv())
	addLabel(0.95,0.95,toUpper(species[strSpp,"name"]), adj=c(1,0), cex=1.5)
	box()
	if (png) dev.off()
#browser();return()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotAdens


## plotProp-----------------------------2022-03-02
## Plot proportion-at-age (or length) from GFBio specimen data.
## ---------------------------------------------RH
plotProp <- function(fnam, hnam=NULL, ioenv=.GlobalEnv, ...)
{
	if(!"globo"%in%tlisp() || !"plotProp"%in%ttcall(globo)$func)
		assign("globo",list(func="plotProp", module="M02_Biology", call=match.call(), args=args(plotProp), ioenv=ioenv, plotname="Rplot"), envir=.PBStoolEnv)
	fnam = as.character(substitute(fnam))
	if (!is.character(fnam)) stop("Argument 'fnam' must be a string name of an R object, usually from a call to 'gfb_bio.sql'")
	getFile(fnam, tenv=ioenv)
	eval(parse(text=paste0("dat = ", fnam)))
	options(warn=-1)
	data("spn", package="PBSdata", envir=.PBStoolEnv)

	path <- paste(system.file(package="PBStools"),"/win",sep=""); 
	rtmp <- tempdir(); rtmp <- gsub("\\\\","/",rtmp)
	wnam <- paste(path,"plotPropWin.txt",sep="/")
	wtmp <- paste(rtmp,"plotPropWin.txt",sep="/")
	tfile <- readLines(wnam)
	tfile <- gsub("@wdf",wtmp,tfile)
	tfile <- gsub("@fnam",fnam,tfile)
	#tfile <- gsub("@tmpdir",rtmp,tfile)

	## Collect info on areas available
	zareas = is.element(colnames(dat),c("major","PMFC","GMA","srfa","srfs","popa","stock"))
	mareas = lapply(dat[,zareas], function(x){xx=.su(x); xx[is.na(xx) | xx==""]="UNK"; xx})
	names(mareas) = paste0("M",names(mareas))
#browser();return()
	setPBSoptions("Mareas", mareas)

	uareas = sapply(mareas,function(x){rep(FALSE,length(x))},simplify=FALSE)
	names(uareas) = sub("^M","U",names(mareas))
	setPBSoptions("Uareas",uareas)

	## Collect info on data types available
	ztypes = is.element(colnames(dat),c("ttype","stype","gear","SSID","ameth","scat"))
	mtypes = sapply(dat[,ztypes], .su, simplify=FALSE)
	names(mtypes) = paste0("M",names(mtypes))
	setPBSoptions("Mtypes", mtypes)

	utypes = sapply(mtypes,function(x){rep(FALSE,length(x))},simplify=FALSE)
	names(utypes) = sub("^M","U",names(mtypes))
	setPBSoptions("Utypes",utypes)

#browser();return()
	ttget(globo); globo$DATA=dat; ttput(globo)  ## save the original data file
	.plotProp.chooseAreas(gui=TRUE, tab=TRUE)
	tfile = c(tfile,"\t## Tab 2 -- choose areas", readLines(paste0(rtmp,"/choose.areas.txt")))
	.plotProp.chooseTypes(gui=TRUE, tab=TRUE)
	tfile = c(tfile,"\t## Tab 3 -- choose types", readLines(paste0(rtmp,"/choose.types.txt")))
	tfile[grep("OK",tfile)] = "null"  ## get rid of the OK button used in pop-up GUIs


	if (!is.null(hnam) && is.character(hnam))
		tfile <- gsub("#import=",paste("import=\"",hnam,"\"",sep=""),tfile)
	tfile = tfile[grep("^#",tfile,invert=TRUE)] ##Get rid of potentailly confusing deprecated lines
	writeLines(tfile,con=wtmp)
#browser();return()
	createWin(wtmp)
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotProp

## .plotProp.calcP----------------------2024-11-28
## Perform calculations to get proportions.
## ---------------------------------------------RH
.plotProp.calcP <- function(reload=FALSE)
{
	getWinVal(winName="window",scope="L")
	ioenv = ttcall(globo)$ioenv
	expr  = paste("getFile(",fnam,",senv=ioenv,use.pkg=TRUE,try.all.frames=TRUE,reload=reload,tenv=penv()); dat=",fnam,sep="")
	eval(parse(text=expr))

	## Refresh the logical area list in case history is being used (RH 241128)
	uareas = getWinVal(winName="window")[c("Umajor","UPMFC","UGMA","Usrfa","Usrfs","Upopa","Ustock")]
	setPBSoptions("Uareas",uareas)

	#ttget(globo); globo$DATA = dat; ttput(globo) # save the original

	fspp  = attributes(dat)$spp
	if (!is.null(fspp) && fspp!=spp) { spp=fspp; setWinVal(list(spp=fspp)) }
	xy   <- as.character(XYopt[,"fld"])
	XLIM <- unlist(XYopt[1,c("lim1","lim2")])
	YLIM <- unlist(XYopt[2,c("lim1","lim2")]);
	xint <- unlist(XYopt[1,"int"]); yint <- unlist(XYopt[2,"int"])

	## Collect info on areas available
	#zarea = is.element(colnames(dat),c("major","minor","locality","PMFC","PFMA","PFMS","GMA","srfa","srfs","popa","stock"))
	#zarea = is.element(colnames(dat),c("major","PMFC","GMA","srfa","srfs","popa","stock"))
	#marea = sapply(dat[,zarea], .su, simplify=FALSE)
	#names(marea) = paste0("M",names(marea))
	#setPBSoptions("Mareas", marea)

	## Collect info on data types available
	#ztype = is.element(colnames(dat),c("ttype","stype","gear","SSID","SVID","ameth","scat"))
	#ztype = is.element(colnames(dat),c("ttype","stype","gear","SSID","ameth","scat"))
	#mtype = sapply(dat[,ztype], .su, simplify=FALSE)
	#names(mtype) = paste0("M",names(mtype))
	#setPBSoptions("Mtypes", mtype)

	## Check available fields
	flds <- names(dat); fldstr <- paste("(",paste(flds,collapse=","),")",collapse="")
	if (!all(is.element(xy,flds)==TRUE)) {
		if (xy[1]=="year" && any(is.element(flds,"date")==TRUE)) {
			dat$year <- as.numeric(as.character(substring(dat$date,1,4)))
			flds <- c(flds,"year") }
		if (!all(is.element(xy,flds)==TRUE)) 
			showError(str=paste(paste(xy[!is.element(xy,flds)],collapse=","),"in",fldstr,sep="\n"),type="nofields")
	}
	if (!is.null(strat) && strat!="" && !is.element(strat,flds))
		showError(paste(strat,"in",fldstr,sep="\n"),"nofields")
	if (bbonly && !any(flds=="ameth"))
		showError(paste("ameth","in",fldstr,sep="\n"),"nofields")

	## Get the qualifiers
	lister <- function(vec) {
		out <- list(vec);
		for (i in 1:length(vec)) out[i+1] <- vec[i]
		return(out) }
	Msex <- c(1,2,3,0)
	if (all(Usex==FALSE)) sex <- NULL 
	else sex <- sort(unique(unlist(lister(Msex)[Usex])))

	## Process the areas
	if (is.null(getPBSoptions("Uareas")))
		.plotProp.chooseAreas(gui=FALSE)
	Mareas = getPBSoptions("Mareas"); unpackList(Mareas)
	#Uareas = getPBSoptions("Uareas"); unpackList(Uareas)  ## logicals corresponding to Mareas
	Uareas = getWinVal()[names(getPBSoptions("Uareas"))]; unpackList(Uareas)  ## logicals from GUI corresponding to Mareas

	for (a in substring(names(Mareas),2)) {
		aM = paste0("M",a)
		aU = paste0("U",a)
		mess = paste0("if (all(", aU, "==FALSE)) ", a, "=NULL else ", a, "=", aM, "[", aU, "]")
		eval(parse(text=mess))
	}

	## Process the data types
	if (is.null(getPBSoptions("Utypes")))
		.plotProp.chooseTypes(gui=FALSE)
	Mtypes = getPBSoptions("Mtypes"); unpackList(Mtypes)
	#Utypes = getPBSoptions("Utypes"); unpackList(Utypes)  ## logicals corresponding to Mtypes
	Utypes = getWinVal()[names(getPBSoptions("Utypes"))]; unpackList(Utypes)  ## logicals from GUI corresponding to Mtypes

	for (a in substring(names(Mtypes),2)) {
		aM = paste0("M",a)
		aU = paste0("U",a)
		mess = paste0("if (all(", aU, "==FALSE)) ", a, "=NULL else ", a, "=", aM, "[", aU, "]")
		eval(parse(text=mess))
	}

	## Qualify the data
	#qflds=c("spp","sex","ttype","stype","gear","major","srfa","srfs")
	qflds=c("spp","sex",substring(names(Mtypes),2),substring(names(Mareas),2))
	for (i in qflds) {
		expr=paste("dat=biteData(dat,",i,")",sep=""); eval(parse(text=expr)) 
		#.flush.cat("after bite ", i, " = ", nrow(dat), "\n") ## activate for debugging
		if (nrow(dat)==0) showError(paste("No data for '",i,"' chosen",sep=""))
	}
	if (bbonly) {
		#dat <- dat[is.element(dat$ameth,3),]
		dat <- extractAges(dat, sex=sex) ## new function (2018-10-12)
		if(nrow(dat)==0) showError("spp:ttype:stype:gear:area:ameth","nodata")
	}

	## Reconfigure the X,Y data
	dat$X <- dat[,xy[1]]; dat$Y <- dat[,xy[2]]
	dat   <- dat[!is.na(dat$X) & !is.na(dat$Y),]
	if(nrow(dat)==0) showError(paste(xy,collapse=","),"nodata")
	if (is.na(YLIM[2])) YLIM[2] <- max(dat$Y)
	ylo <- dat$Y < YLIM[1]; yhi <- dat$Y > YLIM[2]
	if (agg) {
		dat$Y[ylo] <- rep(YLIM[1],length(ylo[ylo==TRUE]))
		dat$Y[yhi] <- rep(YLIM[2],length(yhi[yhi==TRUE])) }
	else  dat <- dat[!ylo & !yhi,]

	dat$x <- ceiling(dat$X/xint) * xint
	dat$y <- ceiling(dat$Y/yint) * yint
	xlim <- range(dat$x,na.rm=TRUE); ylim <- range(dat$y,na.rm=TRUE) # actual data ranges
	
	if (is.na(XLIM[1]) | is.na(XYopt[1,2])){ 
		XLIM[1] <- xlim[1]; setWinVal(list("XYopt[1,2]d"=xlim[1]),winName="window") }
	if (is.na(XLIM[2]) | is.na(XYopt[1,3])){ 
		XLIM[2] <- xlim[2]; setWinVal(list("XYopt[1,3]d"=xlim[2]),winName="window") }
	xval <- seq(xlim[1],xlim[2],xint)
	dat <- dat[is.element(dat$x,xval),]
	if (nrow(dat)==0) showError(paste(xlim[1],"to",xlim[2]),"nodata")

	if (is.na(YLIM[1]) | is.na(XYopt[2,2])){ 
		YLIM[1] <- ylim[1]; setWinVal(list("XYopt[2,2]d"=ylim[1]),winName="window") }
	if (is.na(YLIM[2]) | is.na(XYopt[2,3])){ 
		YLIM[2] <- ylim[2]; setWinVal(list("XYopt[2,3]d"=ylim[2]),winName="window") }
	yval <- seq(ylim[1],ylim[2],yint)
	dat <- dat[is.element(dat$y,yval),]
	if (nrow(dat)==0) showError(paste(ylim[1],"to",ylim[2]),"nodata")
	yy <- dat$y # cause R now strips names from data.frame columns
	if (!is.null(strat) && strat!="") names(yy) <- dat[,strat]
	else {names(yy) <- dat$x; setWinVal(list(strat=as.character(XYopt[1,1])),winName="window") }

	## Construct the z-value matrix
	zmat <- array(0,dim=c(length(xval),length(yval),2),dimnames=list(xval,yval,c("count","prop")))
	names(dimnames(zmat)) <- c(xy,"prop")
	tcount=list(); tprop=list()
	lenv=sys.frame(sys.nframe())

	## Stratified weighted frequency (3/7/08)
	freak <- function(x,w=FALSE) {
		ages  <- sort(unique(x))
		strat <- sort(unique(names(x))); snum <- as.numeric(strat); n=length(strat)
		stab  <- array(0,dim=c(length(ages),length(strat)),dimnames=list(ages,strat))
		flist <- split(x, x)
		for (i in names(flist)) {
			nst <- sapply(split(flist[[i]],names(flist[[i]])),length,simplify=TRUE)
			stab[i,names(nst)] <- nst }
		prop <- sweep(stab,2,apply(stab,2,sum),"/") # convert numbers to proportions
		if (w) p=snum/sum(snum)
		else p=rep(1/n,n) # determine proportional weighting
		wprop <- apply(prop,1,function(x,w){sum(x*w)},w=p)
		fcount <- apply(stab,1,sum)
		assign("tcount",c(tcount,list(fcount)),envir=lenv)
		assign("tprop",c(tprop,list(wprop)),envir=lenv)
		return(wprop) }

	nSID   <- sapply(sapply(split(dat$SID,dat$x),unique,simplify=FALSE),length);
	ylist  <- split(yy,dat$x)
	ycount <- sapply(ylist,freak,w=wted,simplify=FALSE)
	yrStr=names(tcount)=names(tprop)=names(ylist)

	for (i in yrStr) {
		zmat[i,names(tcount[[i]]),"count"] <- tcount[[i]]
		zmat[i,names(tprop[[i]]),"prop"]   <- tprop[[i]]  }
	pa <- array(t(zmat[,,"prop"]),dim=c(length(yval),length(xval)),dimnames=list(yval,xval)) 
	Na <- array(t(zmat[,,"count"]),dim=c(length(yval),length(xval)),dimnames=list(yval,xval))
	for (i in c("sex","ttype","gear","stype","SSID")) { # actually left in data file
		ui=sort(unique(dat[,i])); ui=setdiff(ui,c("",NA))
		if (length(ui)==0) ui=""
		else eval(parse(text=paste(i,"=c(",paste(ui,collapse=","),")"))) }
	areas=c(major,PMFC,srfa,srfs); if (is.null(areas)) areas="all"

	stuff=c("dat","flds","XLIM","xlim","YLIM","ylim","xval","yval","zmat",
			"freak","nSID","ylist","ycount","pa","Na","xy","yy","sex","ttype","gear","stype","areas","SSID")
	ttget(globo)
	for (i in stuff)
		eval(parse(text=paste("globo$",i,"=",i,sep="")))
	ttput(globo)
	.plotProp.plotP()
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.plotProp.calcP

## .plotProp.plotP----------------------2024-11-28
## Guts of the plotting routine.
## ---------------------------------------------RH
.plotProp.plotP <- function(wmf=FALSE, png=FALSE)
{
	## Start blowing bubbles
	sordid <- function(x, lab="wot", count=TRUE) {  ## count and classify specimens
		z <- is.element(x,c("",NA,NaN))
		x <- x[!z]; if(length(x)==0) return("");
		xcnt <- sapply(split(x,x),length)
		if (count)  xstr <- paste(paste(names(xcnt)," (",xcnt,")",sep=""),collapse=", ")
		else        xstr <- paste(names(xcnt),collapse=",")
		xstr <- paste(lab,": ",xstr,sep="");
		return(xstr) }
	darken <- function(col) { ## choose the predominant RGB colour and darken it
		RGB = col2rgb(col)
		x = as.vector(RGB); names(x) = row.names(RGB)
		dom = names(rev(sort(x)))[1]
		paste("dark",dom,sep="") }
	plaster  <- function(x,sep="",enc=c("(",")")) { ## paste together unique values
		if (is.null(x) || all(x=="")) return(NULL)
		lab=as.character(substitute(x))
		ux=sort(unique(x)); ux=setdiff(ux,c("",NA))
		paste(lab,enc[1],paste(ux,collapse=sep),enc[2],sep="") }

	if (is.null(ttcall(globo)$xy)) .plotProp.calcP()
	act <- getWinAct()[1]; 
	if (!is.null(act) && act=="wmf") wmf <- TRUE else wmf <- FALSE
	if (!is.null(act) && act=="png") png <- TRUE else png <- FALSE
	getWinVal(winName="window",scope="L")
	unpackList(ttcall(globo),scope="L")
	#print(plaster(strat)); print(plaster(sex)); print(plaster(ttype)); print(plaster(stype))
	## New code -------- (RH 241128)
	if (!exists("SSID",inherits=FALSE))  SSID = "NOS"  ## Not Otherwise Specified
	Mareas = getPBSoptions("Mareas")
	Uareas = getPBSoptions("Uareas")
	areas = unlist(sapply(1:length(Mareas), function(i){Mareas[[i]][Uareas[[i]]]}))
	if (length(areas)==0) areas="NOS"
	## -----------------------------
	plotname = paste0(c(paste0("age",spp), xy[2], plaster(areas,sep="+"), plaster(strat), plaster(sex), plaster(ttype), plaster(gear), plaster(stype), plaster(SSID)), collapse="-")
#browser();return()
	if (wmf && .Platform$OS.type=="windows")
		do.call("win.metafile",list(filename=paste(plotname,".wmf",sep=""),width=8,height=8))
	else if (png)
		png(paste0(plotname,".png"), units="in", res=600, width=8, height=8)
	else resetGraph()
	expandGraph(mfrow=c(1,1),mai=c(.6,.7,0.1,0.1),las=1)

	xlim <- unlist(XYopt[1,c("lim1","lim2")])
	ylim <- unlist(XYopt[2,c("lim1","lim2")])
	ypretty = pretty(ylim)
	px   <- is.element(xval,xlim[1]:xlim[2])
	py   <- is.element(yval,ylim[1]:ylim[2])
	pa   <- subset(pa,select=px); Na <- subset(Na,select=px)
	xval <- xval[px]; yval <- yval[py]; 
	nSID <- nSID[is.element(names(nSID),xlim[1]:xlim[2])]

	pin=par()$pin
	ad <- c(.02,0.01,ifelse(ltype==1,0.9/pin[2],.25/pin[2]),ifelse(ltype==1,.35/pin[2],0)) # extend range (x1,x2,y1,y2)
	dx <- max(1,diff(xlim)); xl <- xlim + c(-ad[1]*dx,ad[2]*dx)
	dy <- max(1,diff(ylim)); yl <- ylim + c(-ad[3]*dy,ad[4]*dy)
	zxt <- is.element(xval,seq(1950,2050,ifelse(length(xval)>20,5,ifelse(length(xval)>6,2,1))))
	zyt <- is.element(yval,seq(0,5000,ifelse(any(xy[2]==c("wt")),100, ifelse(any(xy[2]==c("len")),50, ifelse(any(xy[2]==c("age")),5,5)))))
	xlab <- paste(LETTERS[is.element(letters,substring(xy[1],1,1))],substring(xy[1],2),sep="")
	ylab <- paste(LETTERS[is.element(letters,substring(xy[2],1,1))],substring(xy[2],2),sep="")

	plotBubbles(pa, dnam=TRUE, smo=1/100, size=psize*pin[1], lwd=lwd, powr=powr, clrs=bcol, hide0=hide0, xlim=xl, ylim=yl, xlab="", ylab="", xaxt="n", yaxt="n", frange=0.01)
	usr=par()$usr; dyu=diff(usr[3:4])
	if (showH) abline(h=ypretty[ypretty>0],lty=3,col="grey30")
	if (showM) {
		ymean = apply(pa,2,function(x,y){sum(x*y)},y=yval)     ## calculate mean len|age
		zpos  = ymean>0 & !is.na(ymean) & apply(Na,2,sum)>10   ## remove zero-value means and also small nos. of specimens
		lines(xval[zpos],ymean[zpos],col="red",lwd=3)
		points(xval[zpos],ymean[zpos],pch=22,col="darkred",bg="chartreuse",cex=1.5)
	}
	tcol <- darken(bcol[1]); ## text colour as darker RGB based on positive bubbles

   mtext(xlab,side=1,line=1.75,cex=1.5)
   mtext(ylab,side=2,line=2.25,cex=1.5,las=0)

   axis(1,at=xval,labels=FALSE,tck=-.005)
   axis(1,at=xval[zxt],mgp=c(0,.5,0),tck=-.02,adj=.5,cex=1)
   axis(2,at=seq(ylim[1],ylim[2],diff(ypretty)[1]/5),tck=0.005,labels=FALSE)
   axis(2,at=ypretty,mgp=c(0,0.5,0),tck=0.02,adj=1,cex=1)

	mainlab <- paste(ttcall(spn)[spp],xy[2],ifelse(!is.null(strat),paste("\n...stratified ",
		ifelse(wted,"& weighted ",""),"by ",strat,sep=""),"not stratified"))
	i1=sordid(dat$sex,"sex");      i2=sordid(dat$gear,"gear")
	i3=sordid(dat$ttype,"ttype");  i4=sordid(dat$stype,"stype")
	i5=sordid(dat$major,"major");  i6=sordid(dat$srfa,"srfa")
	i7=sordid(dat$srfs,"srfs")
	infolab=""
	for (i in 1:7) {
		j=get(paste("i",i,sep=""))
		if (j=="") next
		else infolab=paste(infolab,j,ifelse(i==1,";   ","\n"),sep="") }
	infolab=substring(infolab,1,nchar(infolab)-1)
	Nlab <- apply(Na,2,sum); Nlab <- Nlab[Nlab>0 & !is.na(Nlab)];
	Npos <- as.numeric(names(Nlab)); Nfac=max(nchar(Nlab))
	Slab <- nSID; Slab <- Slab[Slab>0 & !is.na(Slab)];
	Spos <- as.numeric(names(Slab)); Sfac=max(nchar(Slab))
	switch (ltype,
		{addLabel(.03,.01,mainlab,cex=.8,adj=c(0,0),col=tcol);
			addLabel(.97,.01,infolab,cex=.7,adj=c(1,0),col="grey30");
			addLabel(.02,.99,"N",col=tcol,cex=1,adj=c(0,1));
			text(Npos,usr[4]-0.01*dy,Nlab,col=tcol,srt=90,adj=c(1,.5),cex=0.8)},
		{addLabel(.02,.02,"N",col="blue",cex=1,adj=c(0,0));
			text(Npos,usr[3]+0.01*dyu,Nlab,col=tcol,srt=270,adj=c(1,.5),cex=0.8)},
		{addLabel(.02,.02,"S:N",srt=270,col=tcol,cex=1,adj=c(1,0.5));
			text(Spos,usr[3]+0.02*dyu,paste(Slab,Nlab,sep=":"),col=tcol,srt=270,adj=c(1,0.5),cex=0.8)}
	)
	if (wmf|png) dev.off()
	stuff=c("infolab","Nlab","Slab","Npos","Spos","plotname")
	ttget(globo)
	for (i in stuff)
		eval(parse(text=paste("globo$",i,"=",i,sep="")))
	ttput(globo)
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.plotProp.plotP

## .plotProp.resetP---------------------2017-07-19
.plotProp.resetP <- function() 
{
	resList <-
	structure(list(psize = 0.03, powr = 0.5, lwd = 2, bcol = c("blue", 
"grey", "coral"), showH = TRUE, hide0 = TRUE, showM = FALSE, 
    ltype = 1, spp = "POP", Usex = c(TRUE, FALSE, FALSE, FALSE, 
    FALSE), agg = FALSE, bbonly = FALSE, strat = "year", wted = FALSE, 
    fnam = "pop.age", XYopt = structure(list(fld = c("year", "age"
    ), lim1 = c(NA, 0), lim2 = c(NA_real_, NA_real_), int = c(1, 1)), .Names = c("fld", 
    "lim1", "lim2", "int"), row.names = c("X", "Y"), class = "data.frame")), .Names = c("psize", 
"powr", "lwd", "bcol", "showH", "hide0", "showM", "ltype", "spp", 
"Usex", "agg", "bbonly", "strat", "wted", "fnam", "XYopt"))
	setWinVal(resList,winName="window")
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.plotProp.resetP

## .plotProp.resetT---------------------2010-10-20
.plotProp.resetT <- function()
{
	resList <- list(agg = FALSE, XYopt = structure(list(
        fld = structure(c(2L, 1L), .Label = c("age", "year"), class = "factor"), 
        lim1 = c(NA, 0), lim2 = c(NA_real_, NA_real_), int = c(1, 
        1)), .Names = c("fld", "lim1", "lim2", "int"), row.names = c("X", 
    "Y"), class = "data.frame"))
	setWinVal(resList,winName="window")
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.plotProp.resetT

## .plotProp.chooseAreas----------------2020-09-30
.plotProp.chooseAreas <- function(gui=TRUE, tab=FALSE)
{
	ttget(globo)
	dat = globo$DATA
	mareas = getPBSoptions("Mareas")
	if (!gui) {
		uareas = sapply(mareas,function(x){rep(FALSE,length(x))},simplify=FALSE)
		names(uareas) = sub("^M","U",names(mareas))
		setPBSoptions("Uareas",uareas)
	} else {
		winStr = c(
			"window name=\"pPareas\" title=\"Area Codes\"",
			"label text=\"Choose areas from list below\" font=bold"
		)
		for (a in names(mareas)) {
			aa = mareas[[a]]
			a0 = substring(a,2)
			na = length(aa)
			winStr = c( winStr,
			"grid 1 2 sticky=W",
			paste0("label text=\"",a0,"\" font=bold"),
			paste0("vector mode=logical length=", na, " names=",sub("^M","U",a)," sticky=NW values=\"", paste0(rep(FALSE,na),collapse=" "), "\" labels=\"", paste0(aa,collapse=" "), "\" vertical=FALSE stick=W")
			)
		}
		winStr = c( winStr,
			"grid 1 2 sticky=E",
			"button text=\"Codes\" bg=moccasin font=bold function=doAction sticky=SE action=\"openFile(paste0(system.file(package=`PBStools`),`/win/GFBacodes.txt`))\"",
			"button text=\" OK \" bg=aquamarine font=bold function=doAction sticky=SE action=\"setPBSoptions(`Uareas`,getWinVal(winName=`pPareas`)); setWinVal(getPBSoptions(`Uareas`),winName=`window`); closeWin(`pPareas`)\""
		)
		if (tab) {
			winStr = winStr[-1]
			#Oagrid = globo$Nagrid ## last number of area grids
			Nagrid = length(grep("grid",winStr))
			globo$Nagrid = Nagrid
			nrows  = ceiling(Nagrid/2); 
			winStr[1] = paste0("grid ", nrows, " 2 toptitle=\"Choose Areas:\" topfont=\"10 bold\"")
			if (Nagrid%%2==1) winStr=c(winStr,"null")
			vLins = grep("vector",winStr)
			vVals = sapply(strsplit(winStr[vLins],"[[:space:]]"),function(x){sub("names=","",grep("names",x,value=TRUE))})
			#vFuns = paste0(" function=doAction action=\"setWinVal(vars=getWinVal(win=`window`)[`",vVals,"`],win=`pPareas`)\"") 
#browser();return()
			vFuns = paste0(" function=doAction action=\"setPBSoptions(option=`Uareas`,value=getWinVal(win=`window`)[`",vVals,"`],sublist=TRUE)\"") 
			winStr[vLins] = paste0(winStr[vLins],vFuns)
			writeLines(winStr, con=paste0(tempdir(),"/choose.areas.txt"))
			ttput(globo)
		} else {
			createWin(winStr,astext=TRUE)
			setWinVal(getPBSoptions("Uareas"),winName="pPareas")
		}
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~.plotProp.chooseAreas

## .plotProp.chooseTypes----------------2020-09-30
.plotProp.chooseTypes <- function(gui=TRUE, tab=FALSE)
{
	ttget(globo)
	dat = globo$DATA
	mtypes = getPBSoptions("Mtypes")
	if (!gui) {
		utypes = sapply(mtypes,function(x){rep(FALSE,length(x))},simplify=FALSE)
		names(utypes) = sub("^M","U",names(mtypes))
		setPBSoptions("Utypes",utypes)
	} else {
		winStr = c(
			"window name=\"pPtypes\" title=\"Type Codes\"",
			"label text=\"Choose types from list below\" font=bold"
		)
		for (a in names(mtypes)) {
			aa = mtypes[[a]]
			a0 = substring(a,2)
			na = length(aa)
			winStr = c( winStr,
			"grid 1 2 sticky=W",
			paste0("label text=\"",a0,"\" font=bold"),
			paste0("vector mode=logical length=", na, " names=",sub("^M","U",a)," sticky=NW values=\"", paste0(rep(FALSE,na),collapse=" "), "\" labels=\"", paste0(aa,collapse=" "), "\" vertical=FALSE stick=W")
			)
		}
		winStr = c( winStr,
			"grid 1 2 sticky=E",
			"button text=\"Codes\" bg=moccasin font=bold function=doAction sticky=SE action=\"openFile(paste0(system.file(package=`PBStools`),`/win/GFBtcodes.txt`))\"",
			"button text=\" OK \" bg=lawngreen font=bold function=doAction sticky=SE action=\"setPBSoptions(`Utypes`,getWinVal(winName=`pPtypes`)); setWinVal(getPBSoptions(`Utypes`),winName=`window`); closeWin(`pPtypes`)\""
		)
		if (tab) {
			winStr = winStr[-1]
			Ntgrid = length(grep("grid",winStr))
			globo$Ntgrid = Ntgrid
			nrows  = ceiling(Ntgrid/2); 
			winStr[1] = paste0("grid ", nrows, " 2 toptitle=\"Choose Types:\" topfont=\"10 bold\"")
			if (Ntgrid%%2==1) winStr=c(winStr,"null")
			vLins = grep("vector",winStr)
			vVals = sapply(strsplit(winStr[vLins],"[[:space:]]"),function(x){sub("names=","",grep("names",x,value=TRUE))})
			vFuns = paste0(" function=doAction action=\"setPBSoptions(option=`Utypes`,value=getWinVal(win=`window`)[`",vVals,"`],sublist=TRUE)\"") 
			winStr[vLins] = paste0(winStr[vLins],vFuns)
			writeLines(winStr, con=paste0(tempdir(),"/choose.types.txt"))
			ttput(globo)
		} else {
#browser();return()
			createWin(winStr,astext=TRUE)
			setWinVal(getPBSoptions("Utypes"),winName="pPtypes")
		}
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~.plotProp.chooseTypes

##==================================plotProp Suite


#predictRER-----------------------------2011-06-14
# Discriminant function for the accurate classification
# of Rougheye Rockfish (RER) and Blackspotted Rockfish (BSR).
#
# Exploring 35 morphometric and 9 meristic characters,
# Orr and Hawkins (2008) provide a discriminant function 
# (using only 6 morphometrics L and 2 meristics N ) 
# that claims to correctly classify the two species 97.8%
# of the time. The discriminant score D predictions:
#
# When D < 0 then predicts Sebastes aleutianus (RER)
# When D > 0 then predicts Sebastes melanostictus (BSR)
# where
#  S    = standard fish length measured from tip of snout,
#  L[1] = length of dorsal-fin spine 1,
#  L[2] = snout length,
#  N[1] = number of gill rakers,
#  L[3] = length of gill rakers,
#  N[2] = number of dorsal-fin rays,
#  L[4] = length of pelvic-fin rays,
#  L[5] = length of soft-dorsal-fin base,
#  L[6] = preanal length.
#-----------------------------------------------RH
predictRER <- function(S,L,N)
{
	if (length(L)!=6) {
		mess = "Need 6 length measurements:\n\n\t1) dorsal fin spine 1\n\t2) snout\n\t3) gill rakers\n\t4) pelvic fin rays\n\t5) soft-dorsal-fin base\n\t6) preanal"
		showAlert(mess); stop(mess) }
	if (length(N)!=2) {
		mess = "Need 2 numbers:\n\n\t1) # gill rakers\n\t2) # dorsal-fin rays"
		showAlert(mess); stop(mess) }

	sL = L/S
	D  = 101.557*sL[1] + 52.453*sL[2] +  0.294* N[1] +
		   51.920*sL[3] +  0.564* N[2] - 38.604*sL[4] -
		   22.601*sL[5] - 10.203*sL[6] - 10.445
	return(D)
}


#reportCatchAge-------------------------2010-10-20
# Analyses and plots from catch-at-age report.
# Originals by Jon Schnute for Pacific ocean perch (pop).
#-----------------------------------------------RH
reportCatchAge <- function(prefix="pop", path=getwd(), hnam=NULL, ...)
{
	prefix=as.character(substitute(prefix))
	assign("PBStool",list(module="M02_Biology",call=match.call(),args=args(reportCatchAge),plotname="Rplot",prefix=prefix),envir=.PBStoolEnv)
	options(warn=-1)

	wpath <- .getWpath()
	if (is.null(path)) path <- .getApath()
	rtmp <- tempdir(); rtmp <- gsub("\\\\","/",rtmp)
	wnam <- paste(wpath,"reportCatchAgeWin.txt",sep="/")
	wtmp <- paste(rtmp,"reportCatchAgeWin.txt",sep="/")
	
	temp <- readLines(wnam)
	temp <- gsub("@wdf",wtmp,temp)
	temp <- gsub("@prefix",prefix,temp)
	temp <- gsub("@path",path,temp)
	if (!is.null(hnam) && is.character(hnam))
		temp <- gsub("#import=",paste("import=\"",hnam,"\"",sep=""),temp)
	writeLines(temp,con=wtmp)
	createWin(wtmp) 
	invisible()
}

#.reportCatchAge.getrep-----------------2010-10-20
# Get the report file created by ADMB as directed by a TPL file
#-----------------------------------------------RH
.reportCatchAge.getrep <- function()
{
	getWinVal(scope="L")
	report=paste(prefix,".rep",sep=""); repfile=paste(path,"/",report,sep="")
	if (!file.exists(convSlashes(repfile))) showError(repfile,"nofile")
	admRep <- readList(repfile)
	unpackList(admRep,scope="L")
	yr <- cdata$yr; age <- k:(k+nage-1); 
	Btot <- wgta %*% Nat; RkB <- wgta[1] * Nat[1,]
	catch <- cdata$catch; Ft <- -log(1 - catch/Btot);
	srng <- 1:(nyr-k);
	rbio <- wgta[1] * Nat[1,]; Rkprod <- rbio[k+srng]/SSB[srng];
	#nsurv=ncol(cdata)-3; sname=names(cdata)[4:ncol(cdata)]
	sname=setdiff(names(cdata),c("yr","catch","cf")); nsurv=length(sname)
	if (exists("q2",envir=sys.frame(sys.nframe()))) qq=c(qq,q2) # For backwards compatibility with 2-q model
	surveys=list()
	for (s in sname) {
		x=yr; y=cdata[,s]; z=y>0 & !is.na(y)
		if (substring(prefix,1,3)=="pop" && s=="B1") z = z & yr!=1995  # take out extra charter index from GBReed colum

		x=x[z]; y=y[z]
		if (substring(prefix,1,3)=="pop" && s=="B2") {
			x=c(x,1995); y=c(y,cdata[is.element(cdata$yr,1995),"B1"]) }
		surveys[[s]]=list(x=x,y=y) }
	poi <- function(x,y,...) {
		points(x,y,pch=15,col="darkolivegreen1",cex=1); points(x,y,pch=0,cex=1); };
	lin <- function(x,y,...) {
		lines(x,y,col="cornflowerblue",lwd=2); };
	ryrs <- c(1975.5,1988.5,1997.5) # regime shift years

	stuff=c("prefix","report",names(admRep),"yr","age","Btot","RkB","catch","Ft","Rkprod",
			"surveys","poi","lin","ryrs","qq")
	packList(stuff,"PBStool",tenv=.PBStoolEnv) 
	invisible()
}

#.reportCatchAge.checkssb---------------2010-10-20
# Check ADMB SSB calculation
#----------------------------------------------JTS
.reportCatchAge.checkssb <- function()
{
	unpackList(ttcall(PBStool),scope="L");
	(mata %*% (wgtat*Nat)) -SSB
}

#.reportCatchAge.plotrep----------------2010-10-20
# Plot various components of the report file.
#-------------------------------------------JTS/RH
.reportCatchAge.plotrep <- function(plotcodes, unique.only=TRUE)
{
	choices=c("AA","AP","BE","CP","HS","PP","RN","RP","SB","SG","SM","WA")
	if (missing(plotcodes) || is.null(plotcodes) || length(plotcodes)==0 || all(plotcodes=="")) {
		showMessage(paste(c("The function '.reportCatchAge.plotrep' needs a vector of codes.\n",
		"Choose report items from:\n", "AA = Ages actual", "AP = Ages predicted", 
		"BE = Biomass estimates", "CP = Catch (sustainable) vs. productivity", 
		"HS = Harvest strategy", "PP = Probability of achieving productivity", 
		"RN = Recruitment numbers", "RP = Recruitment productivity", 
		"SB = Simulation: biomass at fixed catches", "SG = Simulation: growth rate vs. catch", 
		"SM = Selectivity and maturity vs. age", "WA = Weight vs. age"), 
		collapse="\n"), as.is=TRUE,col="dodgerblue",x=.05,adj=0); return() }
	if (is.null(ttcall(PBStool)$report)) .reportCatchAge.getrep()

	# Plot bubbles for an age distribution
	AAfun = APfun <- function(z,pow=0.5,siz=0.10){
		unpackList(ttcall(PBStool),scope="L")
		xlim <- range(yr);  xdiff <- diff(xlim); xlim <- xlim+0.02*c(-1,1)*xdiff;
		ylim <- range(age); ydiff <- diff(ylim); ylim <- ylim+0.05*c(-1,1)*ydiff;
		z[z==0 | is.na(z)] <- -.001;
		plotBubbles(z,powr=pow,size=siz,xval=yr,yval=age,xlab="Year",ylab="Age",
			xlim=xlim,ylim=ylim,clrs=c("grey30","white"),xaxt="n",yaxt="n")
		if (diff(xlim)<25) axis(1,at=seq(min(yr),max(yr),1),labels=TRUE)
		else axis(1,at=pretty(yr,n=10),labels=TRUE)
		ages=sort(unique(c(min(age),pretty(age,n=5),max(age))))
		axis(2,at=age[age%in%ages],labels=TRUE)
		amean <- age %*% z; 
		apos <- amean > 0; # exclude missing years
		packList("amean","PBStool",tenv=.PBStoolEnv)
		lines(yr[apos],amean[apos],col="red",lwd=4)
		invisible() }

	# Plot biomass (total, mature, selected)
	BEfun <- function() { 
		unpackList(ttcall(PBStool),scope="L");
		mb <- max(Btot);
		plot(yr,Bt,type="l",ylim=c(0,mb),xlab="Year",ylab="Relative Biomass (t)",lwd=2); 
		lines(yr,SSB,lty=5,col="red",lwd=2); lines(yr,Btot,lty=3,col="blue",lwd=2); 
		for (i in 1:length(surveys)){
			unpackList(surveys[[i]])
			points(x,y/qq[i],pch=i,cex=1.25)}
		drawBars(yr,catch,width=1,col="cornflowerblue",lwd=2)
		invisible() }

	# Risk analysis: Ceq vs. RP
	# RPmin,RPmax=range of R; nRP=number of steps; C1,C2,nC,nT=choices for the simulation
	CPfun <- function() {
		unpackList(ttcall(PBStool),scope="L"); getWinVal(scope="L")
		rp <- seq(RPmin,RPmax,length=nRP); cp <- rep(0,nRP);
		for (i in 1:nRP) {
			simPop(RPsim=rp[i]);
			cp[i] <- ttcall(PBStool)$Ceq; };
		packList(c("rp","cp"),"PBStool",tenv=.PBStoolEnv)
		plot(rp,cp,xlab="Productivity (R/SSB)",ylab="Sustainable Catch (t)",xlim=c(0,RPmax),type="n");
		lin(rp,cp);  poi(rp,cp)
		invisible() }

	# Harvest strategy plot: F vs. B
	HSfun <- function() {
		unpackList(ttcall(PBStool),scope="L"); getWinVal(scope="L")
		nyr=length(yr); z=c(yr[1],ryrs,yr[nyr]); nreg=length(z)-1
		regimes=as.numeric(cut(yr,z,include.lowest=TRUE,labels=1:nreg))
		regs=sort(unique(regimes))
		clrs=c("red","green4","orange","blue","tan","purple")[regs]

		plot(Bt,Ft,xlim=c(0,max(Bt)),ylim=c(0,max(Ft)),
			xlab="Relative Biomass (t)",ylab="Fishing Mortality",type="n");
		lines(par()$usr[1:2],c(mmor,mmor),lty=1,col="grey",lwd=2); 
		if (HStrc)
			arrows(Bt[1:(nyr-1)],Ft[1:(nyr-1)],Bt[2:nyr],Ft[2:nyr],length=.075,col="darkgrey")
		for (i in regs) {
			zi=is.element(regimes,i)
			points(Bt[zi],Ft[zi],pch=i,col=clrs[i],cex=1.5) }
		points(Bt[nyr],Ft[nyr],pch=22,bg="whitesmoke",cex=1.5) # Final year
		Bmn=sapply(split(Bt,regimes),mean,na.rm=TRUE)
		Fmn=sapply(split(Ft,regimes),mean,na.rm=TRUE)
		if (HSreg) {
			lines(Bmn,Fmn,col="black",lwd=2);
			for (i in regs)
				points(Bmn[i],Fmn[i],pch=i,cex=switch(i,4,3,2.5,3),col=clrs[i],lwd=4) }
		stuff=c("regimes","Bmn","Fmn")
		packList(stuff,"PBStool",tenv=.PBStoolEnv)
		box(); invisible() }

	# Productivity probability
	PPfun <- function() {
		unpackList(ttcall(PBStool),scope="L"); getWinVal(scope="L")
		srng <- 1:(nyr-k);
		rbio <- wgta[1] * Nat[1,]; rprod <- rbio[k+srng]/SSB[srng];
		rok <- rprod[rprod<=RPmax]; rn <- length(rok);
		pp <- 1 - (0:(rn-1))/max(srng); rps <- sort(rok);
		packList(c("rbio","rprod","rps","pp"),"PBStool",tenv=.PBStoolEnv)
		plot(rps,pp,xlim=c(0,RPmax),ylim=c(0,1),xlab="Productivity (R/SSB)",ylab="Probability",typ="n");
		lin(rps,pp);  poi(rps,pp)
		invisible() }

	# Recruitment biomass at ages k and kplus
	RNfun <- function() { 
		unpackList(ttcall(PBStool),scope="L"); getWinVal(scope="L")
		Rk2B <- wgta[1+Roff]*Nat[1+Roff,]
		rmax <- max(RkB,Rk2B);
		plot(yr,RkB,type="n",xlim=range(yr),ylim=c(0,rmax),xlab="Year",ylab="Recruitment (t)")
		for (i in 1:length(ryrs)) {
			ii=if(i%%2) 1 else 2
			lines(c(ryrs[i]+k,ryrs[i]+k),par()$usr[3:4],lty=3,lwd=2,
			col=switch(ii,"green4","red")) }
		lines(yr,Rk2B,col="cornflowerblue",lty=5,lwd=2)
		lines(yr,RkB,col="grey40",lty=1,lwd=2)
		packList("Rk2B","PBStool",tenv=.PBStoolEnv)
		invisible() }

	# Recruitment productivity
	RPfun <- function() { 
		unpackList(ttcall(PBStool),scope="L");
		srng <- 1:(nyr-k);
		#rbio <- wgta[k-6] * Nat[k-6,]; rprod <- rbio[k+srng]/SSB[srng];
		rbio <- wgta[1] * Nat[1,]; rprod <- rbio[k+srng]/SSB[srng];
		rmx <- max(rprod);
		plot(yr[srng],rprod,xlim=range(yr),xlab="Year",ylab="Productivity (R/SSB)",type="n"); 
		for (i in 1:length(ryrs)) {
			ii=if(i%%2) 1 else 2
			lines(c(ryrs[i],ryrs[i]),par()$usr[3:4],lty=3,lwd=2,
			col=switch(ii,"green4","red")) }
		lines(yr[srng],rprod,lwd=2);  poi(yr[srng],rprod)
		invisible() }

	# Simulate biomass at fixed catch
	SBfun <- function() {
		simPop()
		unpackList(ttcall(PBStool),scope="L"); getWinVal(scope="L")
		xlim <- range(pyr);  xdiff <- diff(xlim); xlim <- xlim+0.02*c(-1,1)*xdiff;
		ylim <- range(Cval); ydiff <- diff(ylim); ylim <- ylim+0.05*c(-1,1)*ydiff;
		plotBubbles(SB,xval=pyr,yval=Cval,xlim=xlim,ylim=ylim,
			xlab="Year",ylab="Catch (t)",clrs=c("grey30","white"))
		if (Ceq>0) abline(h=Ceq,col="cornflowerblue",lwd=2)
		invisible() }
	
	# Simulate biomass at fixed catch
	SGfun <- function() {
		simPop()
		unpackList(ttcall(PBStool),scope="L"); getWinVal(scope="L")
		plot(Cval,(SB[,nT]/SB[,1])^(1/nT),type="n",xlab="Catch (t)",ylab="Growth Rate");
		lin(Cval,(SB[,nT]/SB[,1])^(1/nT));
		if (Ceq>0) { lines(c(par()$usr[1],Ceq,Ceq),c(1,1,par()$usr[3]),lty=3,col="blue"); };
		invisible() }

	# Plot selectivity beta[a] and maturity[a]
	SMfun <- function() {
		unpackList(ttcall(PBStool),scope="L");
		plot(age,bta,ylim=c(0,1),xlab="Age",ylab="Selectivity (pts), Maturity (line)",type="n");
		lines(age,mata,col="red",lwd=2); 
		for (i in 1:4) lines(c(5*(1+i),5*(1+i)),c(0,bta[5*i-1]),lty=3); 
		poi(age,bta)
		invisible() }

	# Plot weight[a]
	WAfun <- function() { 
		unpackList(ttcall(PBStool),scope="L");
		y <- wgtat[,nyr]; ym <- max(y);
		plot( age, y, xlab="Age", ylab="Weight (kg)", ylim=c(0,ym) );
		lin(age,y); poi(age,y)
		invisible() }

	# Simulate populations across a range of catches
	# C1,C2=range of catches   nC=number of catch values
	# nT=number of time steps  RP=constant value of RecProd
	# Output: SB=matrix[nC,nT] of stock biomasses
	simPop <- function(RPsim=NULL) {
		unpackList(ttcall(PBStool),scope="L"); getWinVal(scope="L")
		if (!is.null(RPsim)) RP=RPsim
		Cval <- seq(C1,C2,length=nC); pyr <- yr[length(yr)] + (1:nT);
		SB <- matrix(0,nrow=nC,ncol=nT);
		Nvec0 <- Nat[,nyr];
		for (i in 1:nC) {
			Nvec <- Nvec0;
			Cbio <- Cval[i];                          # catch biomass
			stk <- (mata * wgta) %*% Nvec;            # stock biomass
			SB[i,1] <- stk;
			for (tt in 2:nT) {
				u <- bta*Nvec/sum(bta*Nvec);           # proportions in catch
				w <- u %*% wgta;                       # mean weight in catch
				Cnum <- Cbio/w;                        # catch numbers
				Nvec1 <- pmax( exp(-mmor)*(Nvec-u*Cnum), 0 );
				Nvec1[nage] <- Nvec1[nage] + Nvec1[nage-1];
				Nvec[2:(nage-1)] <- Nvec1[1:(nage-2)];
				Nvec[1] <- RP*stk; 
				stk <- (mata * wgta) %*% Nvec;         # spawning stock biomass
				SB[i,tt] <- stk;
			}; };
		grate <- (SB[,nT]/SB[,1])^(1/nT);            # growth rate
		gi0 <- (grate < 1); gi1 <- (grate > 1);
		if (any(gi0==TRUE)) i0 <- min((1:nC)[gi0]) else i0 <- nC; 
		if (any(gi1==TRUE)) i1 <- max((1:nC)[gi1]) else i1 <- 1;
		Ceq <- 0;
		if ( (i0>1) & (i1<=nC) ) 
			Ceq <- approx(grate,Cval,xout=1,rule=2,ties="ordered")$y 
		packList(c("pyr","Ceq","Cval","SB","grate","nT"),"PBStool",tenv=.PBStoolEnv)
		invisible() }

	# Main body of .reportCatchAge.plotrep()
	plotcodes=plotcodes[is.element(plotcodes,choices)]
	if (unique.only) plotcodes=unique(plotcodes) # order is retained
	N=length(plotcodes); rc=.findSquare(N); pnum=0; cexN=3/sqrt(N)
	figpars=list(mfrow=rc,mar=c(4.5,4,0.5,1),oma=c(0,0,0,0),mgp=c(2.5,.5,0),las=1,tcl=-.3)
	unpackList(ttcall(PBStool),scope="L")
	unpackList(figpars); resetGraph()
	expandGraph(mfrow=mfrow, mar=mar,oma=oma,mgp=mgp,las=las,tcl=tcl)
	for (i in plotcodes) {
		pnum = pnum + 1
		if (i=="AA") arg="pat" else if (i=="AP") arg="uat" else arg=""
		ifun=paste(i,"fun",sep="")
		if (exists(ifun,envir=sys.frame(sys.nframe())))
			eval(parse(text=paste(ifun,"(",arg,")",sep="")))
		else { 
			plot(0,0,type="n",xaxt="n",yaxt="n",xlab="",ylab="")
			addLabel(.5,.5,paste("'",i,"' not available",sep=""),cex=cexN,col="red") }
		if (N>1) addLabel(.95,.95,LETTERS[pnum],cex=cexN,adj=1)
	}
	invisible()
}
#-----------------------------------reportCatchAge


## requestAges--------------------------2022-10-24
##  Determine which otoliths to sample for ageing requests.
##  Note: only have to use run.sql=TRUE once for each species 
##  using any year before querying a group of years.
##  Note: ageing methdology is not a sensible selection 
##  criterion because the fish selected have not been aged.
## ---------------------------------------------RH
requestAges <- function(strSpp, nage=500, year=2016, 
   areas=list(major=3:9, minor=NULL), ttype=c(1,4), gear=1,
   sex=1:2, nfld = "nallo", run.sql=TRUE, only.sql=FALSE, bySID=FALSE,
   spath=.getSpath(), uid=Sys.info()["user"], pwd=uid, ...)
{
	on.exit(gc())
	if (!only.sql) {
	assign("PBStool",list(module="M02_Biology",call=match.call(),args=args(requestAges)),envir=.PBStoolEnv)

##Subfunctions --------------------------
	adjustN <- function(a,b,nmin=10){           ## a=available , b=desired, nmin=minimum acceptable
#print(a); print(b)
		if (round(sum(b),5) > round(sum(a),5)) {
			showMessage(paste("There are only",sum(round(a,0)),"otoliths available.\n",
				"All were selected."),as.is=TRUE)
			return(a) }
		ttget(iters); iters=iters+1; ttput(iters)
		target = sum(b)                ## Total number desired
		za.use = a >= nmin             ## Only use avail. samples with at least the acceptable min # ages
		a[!za.use] = 0                 ## Automatically set low availablity to 0
		zb.gta = b > a                 ## Determine which desired #ages exceed the available
		zover  = za.use & zb.gta       ## Index the over-desired
		if (any(zover) && iters < 1000) { 
			aN = rep(0,length(a))       ## adjusted N
			aN[zover] = a[zover]
			bnew = b[!zover]
			bnew = pmin(bnew,a[!zover]) ## make sure bnew doesn't exceed avaialble
			pb = bnew/sum(bnew)         ## proportion of non-restricted desired
			sN = target - sum(a[zover]) ## surplus desired
			bnew = pb*(sN)              ## rescale by surplus desired
			bnew[bnew<nmin] = 0         ## get rid of desired with less than minimum
			bnew = sN*bnew/sum(bnew)    ## rescale the desired again to achieve the surplus desired
			aN[!zover] = bnew           ## add the rescaled surplus to the desired vector
#browser()
			adjustN(a,aN)               ## re-iterate
		}
		else return(b) }

	moveCat <- function(C,S){
		if (all(S)) return(C) ## all periods sampled, no need to move catch
		n=length(C); x=1:n; y=rep(0,n)
		y[!S]=.05
		for (i in x) {
			if (S[i]) next
			z   = (x-x[i])^2 + (y-y[i])^2
			zz  = z==sort(z[S])[1]
			zzz = zz/sum(zz)
			C   = C + C[i]*zzz
			C[i]= 0 }
		return(C) }

	catnip <- function(...) {cat(... ,sep="")}
##---------------------------subfunctions
	}

	if (run.sql || only.sql) {
#if(FALSE){
		expr=paste(c("getData(\"gfb_age_request.sql\"",
			",dbName=\"GFBioSQL\",strSpp=\"",strSpp,"\",path=\"",spath,"\",tenv=penv(),gear=",deparse(gear),")"),collapse="")
#browser();return()
		expr=paste(c(expr,"; Sdat=PBSdat"),collapse="")
		expr=paste(c(expr,"; save(\"Sdat\",file=\"Sdat",strSpp,".rda\")"),collapse="")      # Sample data (binary)
		expr=paste(c(expr,"; write.csv(Sdat,file=\"Sdat",strSpp,".csv\")"),collapse="")     # Sample data (ascii)

#expr="a=1"
		expr=paste(c(expr,"; getData(\"gfb_pht_catch.sql\",\"",
			"GFBioSQL\",strSpp=\"",strSpp,"\",path=\"",spath,"\",tenv=penv())"),collapse="")
		expr=paste(c(expr,"; phtcat=PBSdat"),collapse="")

		expr=paste(c(expr,"; getData(\"gfb_fos_catch.sql\",\"",
			"GFBioSQL\",strSpp=\"",strSpp,"\",path=\"",spath,"\",tenv=penv())"),collapse="")
		expr=paste(c(expr,"; foscat=PBSdat"),collapse="")
		expr=paste(c(expr,"; Ccat=rbind(phtcat,foscat[,names(phtcat)])"),collapse="")
		expr=paste(c(expr,"; save(\"Ccat\",file=\"Ccat",strSpp,".rda\")"),collapse="")      # Commercial catch (binary)
		expr=paste(c(expr,"; write.csv(Ccat,file=\"Ccat",strSpp,".csv\")"),collapse="")     # Commercial catch (ascii)
#}
#expr=as.character()
		#expr=paste(c(expr,"; getData(\"gfb_gfb_catch.sql\",\"",
		expr=paste(c(expr,"; getData(\"gfb_catch_records.sql\",\"",                         ## switch to using only one SQL rather maintaining two
			"GFBioSQL\",strSpp=\"",strSpp,"\",path=\"",spath,"\",tenv=penv())"),collapse="")
		expr=paste(c(expr,"; Scat=PBSdat"),collapse="")
		expr=paste(c(expr,"; save(\"Scat\",file=\"Scat",strSpp,".rda\")"),collapse="")      # Survey catch (binary)
		expr=paste(c(expr,"; write.csv(Scat,file=\"Scat",strSpp,".csv\")"),collapse="")     # Survey catch (ascii)
#browser();return()
		clearFiles(paste0(rep(c("Sdat","Scat","Ccat"),each=2), ".", rep(c("csv","rda"),3)))
		eval(parse(text=expr))
	}
	if (!only.sql && !run.sql) { 
		expr=paste(c("load(\"Sdat",strSpp,".rda\"); load(\"Ccat",strSpp,".rda\"); ",
			"load(\"Scat",strSpp,".rda\")"),collapse="")
		eval(parse(text=expr)) 
	}
	else
		return(invisible(list(expr=expr,Sdat=Sdat,Ccat=Ccat,Scat=Scat)))

	if (!is.null(gear))
		Sdat = Sdat[is.element(Sdat$gear,gear),]
	if (nrow(Sdat)==0 || nrow(Ccat)==0) showError("Not enough data")

	samp = Sdat
	if (strSpp %in% c("396","440")) {
		samp  = calcStockArea(strSpp,samp)
		samp$major_orig = samp$major
		samp$major = samp$major_adj
	}
	## Check for duplicated sample IDs and reduce the catch accordingly
	## NOTE: No need as proportions are adjusted for duplicated samples later in code (see pcat.sid ~L.270)
	redSIDcat = list(...)$redSIDcat ## reduce duplicated SID catch
	if (is.null(redSIDcat)) redSIDcat=FALSE
	redSIDrec = list(...)$redSIDrec ## reduce duplicated SIDs (collapse records)
	if (is.null(redSIDrec)) redSIDrec=FALSE
	if (redSIDcat || redSIDrec) {
		samp  = samp[order(samp$SID, samp$firstSerial),]
		sdupe = duplicated(samp$SID)
		if (any(sdupe)) {
			ndupe = (1:length(sdupe))[sdupe]
			#if (all(diff(ndupe)>1)) { ## i.e. no triplicated (or greater) SIDs
				idupe = sort(c(ndupe-1,ndupe))
				cdupe = samp$catchKg[ndupe]; names(cdupe) = samp$SID[ndupe]
				if (redSIDcat) {
					samp$catchKg[idupe] = samp$catchKg[idupe]/2
					attr(samp,"catchKg.dupe.original") = cdupe
				}
				if (redSIDrec) {
					nsamp   = junk=sapply(split(samp$SID,samp$SID),length)
					oldsamp = samp
					newsamp = samp[!sdupe,]  ## includes all SIDs just once
					onename = names(nsamp)[is.element(nsamp,1)]
					onesamp = newsamp[is.element(newsamp$SID, onename),]
					mooname = names(nsamp)[!is.element(nsamp,1)]  ## more than one record per SID
					moosamp = newsamp[is.element(newsamp$SID, mooname),]

					addCols = c("Noto","Foto","Moto","Nbba","Fbba","Mbba","Nage","Fage","Mage")
					mrgCols = c("firstSerial","lastSerial","storageID")
					for (i in mooname) {
						ii  = is.element(moosamp$SID,i)
						iii = is.element(oldsamp$SID,i)
						moosamp[ii,addCols] = apply(oldsamp[iii, addCols], 2, sum, na.rm=TRUE)
						moosamp[ii,mrgCols] = apply(oldsamp[iii, mrgCols], 2, function(x){paste0(gsub("[[:space:]]", "",x), collapse="|")})
					}
					samp = rbind(onesamp,moosamp)
					samp = samp[order(samp$SID),]
				}
			#} else {
			#	mess= "Some SIDs are more than just duplicated.\nCreate code to deal with it (line 141)."
			#	showError(mess, as.is=TRUE)
			#}
		}
	}

	if (all(sex==1)){
		samp$NOTO=samp$Moto; samp$NBBA=samp$Mbba; samp$NAGE=samp$Mage}  # males
	else if (all(sex==2)){
		samp$NOTO=samp$Foto; samp$NBBA=samp$Fbba; samp$NAGE=samp$Fage}  # females
	else if (all(sex==c(1,2))) {  ## males and females only
		samp$NOTO = ifelse((samp$Moto+samp$Foto)==0,samp$Noto,samp$Moto+samp$Foto)
		samp$NBBA = ifelse((samp$Mbba+samp$Fbba)==0,samp$Nbba,samp$Mbba+samp$Fbba)
		samp$NAGE = ifelse((samp$Mage+samp$Fage)==0,samp$Nage,samp$Mage+samp$Fage)
	}
	else {
		samp$NOTO=samp$Noto; samp$NBBA=samp$Nbba; samp$NAGE=samp$Nage; sex=0:3}  # all available
	samp$year=convFY(samp$tdate,1)
	nfree = round(samp$NOTO-samp$NAGE)

	if (!is.null(list(...)$nfree)) samp <- samp[nfree>=list(...)$nfree & !is.na(nfree),]
	if (is.null(ttype)) showError("Choose a trip type")
	else if (sum(is.element(ttype,2:3))==0 & sum(is.element(ttype,c(1,4:14)))>0) type="C" ## commercial
	else if (sum(is.element(ttype,2:3))>0 & sum(is.element(ttype,c(1,4:14)))==0) type="S" ## research/survey
	else showError("Choose ttypes out of:\n   {2,3} (research/survey) OR\n   {1,4:14} (commercial)")

	if (type=="C") catch = Ccat
	else if (type=="S") catch = Scat
	else showError ("No catch data")
#browser();return()

	z = catch$date < as.POSIXct("1996-01-01") | catch$date > Sys.time()
	catch = catch[!z,]
	catch$year=convFY(catch$date,1)
	if (type=="C") {
		samp$tid = rep(0,nrow(samp)); catch$tid = rep(0,nrow(catch))
		zfos = samp$tdate > as.POSIXct("2007-03-31") #& samp$TID_fos > 0
		samp$tid[zfos]   = samp$TID_fos[zfos]
		samp$tid[!zfos]  = samp$TID_gfb[!zfos]
		zfos = catch$date > as.POSIXct("2007-03-31")
		catch$tid[zfos]   = catch$TID_fos[zfos]
		catch$tid[!zfos]  = catch$TID_gfb[!zfos]
	}
	else {
		samp$tid  = paste(samp$TID_gfb,samp$FEID,sep=".")
		catch$tid = paste(catch$TID,catch$FEID,sep=".")
	}
	unpackList(list(...))
	for (i in intersect(c("TID_gfb","TID_fos"),ls())) {
		eval(parse(text=paste("samp=biteData(samp,",i,")",sep="")))
		if (nrow(samp)==0) showError(paste0("No sample data for ", i, " = ",get(i)))
		eval(parse(text=paste("catch=biteData(catch,",i,")",sep="")))
		areas = list(major=.su(samp$major))    ## if TID is specified, areas become defined in terms of PMFCs
		year  = .su(samp$year)
	}
	if (sum(samp$NOTO-samp$NAGE)==0)
		showError ("No otoliths free in sample data")
#browser();return()
	spooler(areas,"area",samp)                ## creates a column called 'area' and populates based on argument 'areas'.
	spooler(areas,"area",catch)
	area=sort(unique(samp$area))

	for (i in intersect(c("year","area","ttype"),ls())) {
		eval(parse(text=paste("samp=biteData(samp,",i,")",sep="")))
		if (i!="ttype") eval(parse(text=paste("catch=biteData(catch,",i,")",sep="")))
	}
	if (nrow(samp)==0)  showError("No records left in 'samp'. Ease your restrictions")
	if (nrow(catch)==0) {
		showMessage("No records left in 'catch'. Ease your restrictions")
		return(samp) }

	samp=samp[order(samp[[ifelse(type=="C","tdate","FEID")]]),]
	z0=is.element(samp$tid,0)
	if (any(z0)) samp$tid[z0] = 1:sum(z0)
#browser();return()

	catch=catch[order(catch$date),]
	z0=is.element(catch$tid,0)
	if (any(z0)) catch$tid[z0] = catch$hail[z0]

	if (type=="C") {
		Clev = convYP(catch$date)    ## all periods in commercial catch
		clev = names(Clev)
		Slev = convYP(samp$tdate)    ## periods in sample data
		slev= names(Slev)
	}
	if (type=="S") {
		cid=catch$tid
		sid=samp$tid
#browser();return()
		# 'gfb_age_request.sql" now gathers Grouping Code (GC)
		#group=catch$GC; names(group)=cid
		#samp$GC=group[sid]; samp=samp[!is.na(samp$GC),] ## get rid of unidentified groups (strata)
		Clev = paste(catch$year,pad0(catch$GC,3),sep="-")## all groups in survey catch
		Slev = paste(samp$year,pad0(samp$GC,3),sep="-")  ## groups in sample data
		clev = names(Clev)=Clev                          ## levels (quarters/strata) in catch file
		slev = names(Slev)=Slev                          ## levels (quarters/strata) in samples file
	}
	samp$lev = slev
	ulev = sort(unique(samp$lev))                       ## unique periods in sample data
	catfac = ifelse(type=="C",1000.,1.)                 ## factor to convert catch (C=tonnes, S=kg)
	C    = sapply(split(catch$catKg,clev),sum)/catfac   ## total catch (Tcat) at each level (quarter/stratum)
	S    = is.element(names(C),ulev)
#browser();return()

	### Quarterly catch (t)
	CC   = moveCat(C,S)
	if (round(sum(C),5)!=round(sum(CC),5)) showError("Catch shuffling amongst periods not successful")
	ccat = catch$catKg; names(ccat) = catch$tid         ## trip catch of target species
	zper = is.element(clev,ulev)                        ## index of unique periods
	cper = Clev[zper]                                   ## comm.catch periods that appear in samples
	ccat = ccat[zper]                                   ## comm.catch relevant to sample periods
	qcat = split(ccat,names(cper))
	#qC   = CC[ulev]
	qC   = CC[intersect(names(CC),ulev)]
	pC   = qC / sum(qC)                                 ## proportion of total catch in each period
	nC   = pC * nage                                    ## number of otoliths to age per period, given a fixed budget
	samp[["Tcat"]] = qC[samp$lev]/catfac                ## populate the df with period/strata catches
	samp[["Pcat"]] = pC[samp$lev]                       ## populate the df with period/strata catches
	samp[["Ncat"]] = nC[samp$lev]                       ## populate the df with period/strata catches
#browser();return()

	### ---Trip catch (t)---
	samp$ncat=samp$pcat=samp$pcat.sid=samp$pcat.tid=samp$tcat=rep(0,nrow(samp)) ## prepare blank columns in data frame (df)
	for (i in names(qC)) {                              ## loop through periods (quarters)
		iqcat = qcat[[i]]
		zc = is.element(names(iqcat),samp$tid)           ## index the trips from the comm.catch in each period
		#if (!any(zc)) next
		zl   = is.element(samp$lev,i)
		tid.sid  = split(samp$SID[zl],samp$tid[zl])
		scat.sid = samp$catchKg[zl]; names(scat.sid) = samp$SID[zl]
		scat.sid = split(scat.sid,samp$tid[zl])          ## sample catch by trip
		pcat.sid = lapply(scat.sid,function(x){x/sum(x)})## prop. sample catch by trip
		if (!any(zc)) {                                  ## If commercial catch is not matched, assume that trip
			tcat.tid = sapply(scat.sid,sum)               ## at least caught the sampled catch.
		} else {
			tcat.tid = iqcat[zc]                          ## can contain multiple catches per trip due to areas or multiple samples
			zt = samp$tid[zl] %in% names(tcat.tid)
			if (!all(zt)) { #???
				xcat = samp$catchKg[zl][!zt]; names(xcat) = samp$tid[zl][!zt]
				tcat.tid = c(tcat.tid,xcat)
			}
			#tcat = sapply(split(tcat,names(tcat)),sum)/catfac  ## rollup area catches
			tcat.tid = split(tcat.tid,names(tcat.tid))
			tcat.tid = sapply(tcat.tid,function(x){as.vector(x[1])})/catfac ## rollup area catches with same TID
		}
		pcat.tid = tcat.tid/sum(tcat.tid)                ## proportion of period catch taken by each trip
		#ncat = pcat*nC[i]                               ## allocate fish to age based on proportion of trip catch in period
		
		zs   = is.element(samp$tid,names(tcat.tid))      ## index the trips from the sample data in each period
		zss  = as.character(.su(samp$tid[zs]))           ## tid can have more than one sample
		for (j in zss) {
			zj = is.element(samp$tid,j)                   ## indexes SID including duplicated SIDs
			for (k in tid.sid[[j]]){
				kk = as.character(k)                       ## sample ID
#if (k==487362) {browser();return()}
				zjk = zj & is.element(samp$SID,k)
				nsamp = length(pcat.sid)
				samp[["tcat"]][zjk] = tcat.tid[j]                         ## populate the df with trip catches (incl.dupes)
				samp[["pcat.tid"]][zjk] =                                 ## prop. trip catches within a stratum
				samp[["pcat.sid"]][zjk] = pcat.sid[[j]][kk]               ## prop. duplicated samples in each trip
				samp[["pcat"]][zjk] = pcat.tid[j]*pcat.sid[[j]][kk]       ## adjusted prop. trip catches in strata
				samp[["ncat"]][zjk] = pcat.tid[j]*pcat.sid[[j]][kk]*nC[i] ## no. specimens to age
			}
		}
#browser();return()
#if (i=="2012-02") {browser();return()}
	}
	packList(c("Sdat","catch","C"),"PBStool",tenv=.PBStoolEnv)
	samp$ncat  = nage*samp$ncat/sum(samp$ncat)          ## Force the calculated otoliths back to user's desired number (nage)
	samp$nwant = round(pmax(1,samp$ncat))               ## No. otoliths wanted by the selection algorithm (inflated when many values are <1)
	samp$ndone = samp$NBBA                              ## No. otoliths broken & burnt
	samp$nfree = round(samp$NOTO-samp$NAGE)             ## No. of free/available otoliths not yet processed
	samp$ncalc = pmin(pmax(0,samp$nwant-samp$ndone),samp$nfree) ## No. of otoliths calculated to satisfy Nwant given constraint of Nfree
	nardwuar = samp$ncalc > 0 & !is.na(samp$ncalc)
	samp$nallo = rep(0,nrow(samp))
	
	if (sum(samp$nfree)==0)
		showError ("No otoliths free in sample data")
	iters=0; ttput(iters)
	samp$nallo = adjustN(a=samp$nfree,b=samp$ncat)      ## No. of otoliths allocated to satisfy user's initial request, given constraint of Nfree
#browser();return()
	#samp$nallo[nardwuar] = adjustN(a=samp$nfree[nardwuar],b=samp$ncat[nardwuar]) ## No. of otoliths allocated to satisfy user's initial request, given constraint of Nfree

	## Adjust for many small n-values (<1) using median rather than 0.5 as the determinant of 0 vs.1
	zsmall = samp$nallo[nardwuar] < 1. & round(samp$nallo[nardwuar],5) > 0
	if (any(zsmall)) {
		msmall = median(samp$nallo[nardwuar][zsmall])
		zzero  = samp$nallo[nardwuar][zsmall] < msmall
		samp$nallo[nardwuar][zsmall][zzero] = 0
		samp$nallo[nardwuar][zsmall][!zzero] = 1
	}
	samp$nallo[nardwuar] = round(samp$nallo[nardwuar])
	narduse = samp[,nfld] > 0 & !is.na(samp[,nfld])
	sampuse = samp[narduse,]

	clearFiles("nalloSID.csv")
	write.csv(t(t(rev(sort(sapply(split(samp$nallo,samp$SID),sum))))),file="nalloSID.csv")

	### ---End sample calculations--- ###
#browser();return()

	yearmess = if (length(year)>3) paste(min(year),"-",max(year),sep="") else paste(year,collapse="+")
	if (is.null(list(...)$describe))
		describe=paste("-",type,"(",yearmess,")-area(",area,")-sex(",paste(sex,collapse="+"),")-N",round(sum(samp[,nfld])),sep="")
	attr(samp,"Q") = cbind(qC,pC,nC)
	attr(samp,"call") = deparse(match.call())
		packList(c("samp","describe"),"PBStool",tenv=.PBStoolEnv)

	clearFiles(paste0("Sdat",strSpp,describe,c(".csv",".rda")))
	save("samp",file=paste("Sdat",strSpp,describe,".rda",sep=""))
	write.csv(samp,  paste("Sdat",strSpp,describe,".csv",sep=""))

	tid = sampuse[["tid"]]
	if (bySID) {
		tid = sapply(split(sampuse[["SID"]],sampuse[["tid"]]),unique,simplify=FALSE)
	}
	else {
		tid = sapply(split(sampuse[["storageID"]],sampuse[["tid"]]),unique,simplify=FALSE)
	}
	usid     =.su(sampuse[["SID"]])
	tripsids = sapply(split(sampuse[["SID"]],sampuse[["tid"]]),unique,simplify=FALSE)
	#utray    = .su(sampuse[["storageID"]])
	traytids = sapply(split(sampuse[["tid"]],sampuse[["storageID"]]),unique,simplify=FALSE)
	#traysids = sapply(split(sampuse[["SID"]],sampuse[["storageID"]]),unique,simplify=FALSE)
#PBSdat=PBSdatT2
	## Get list of available otoliths. 
	#expr=paste("SELECT B5.SAMPLE_ID AS SID, B5.SPECIMEN_SERIAL_NUMBER AS SN FROM B05_SPECIMEN B5 WHERE B5.SAMPLE_ID IN (", paste(usid,collapse=","),") AND B5.AGEING_METHOD_CODE IS NULL AND B5.SPECIMEN_SEX_CODE IN (", paste(sex,collapse=","),")",sep="")
	
	## CONTAINER_ID in SAMPLE_COLLECTED = Bin, CONTAINER_ID in SPECIMEN_COLLECTED = Tray
	## Note: SAMPLE_COLLECTED sometimes misses samples
	expr = c("SET NOCOUNT ON",
	"SELECT DISTINCT",
		"SAMPLE_ID,",
		"STUFF((",
			"SELECT '+' + CAST([STORAGE_CONTAINER_ID] AS VARCHAR(20))",
			"FROM SAMPLE_COLLECTED f2",
			"WHERE f1.SAMPLE_ID = f2.SAMPLE_ID",
			"FOR XML PATH ('')), 1, 1, '') AS BinID",
		"INTO #Unique_Bin",
		"FROM SAMPLE_COLLECTED f1",
	"WHERE f1.SAMPLE_ID IN (",
	paste(usid,collapse=","),")",
	"SELECT",
		"COALESCE(UB.SAMPLE_ID,SPC.SAMPLE_ID) AS SID,",
		"ISNULL(UB.BinID,'UB'+CAST(SPC.SAMPLE_ID as varchar(6)))+':'+ISNULL(SPC.STORAGE_CONTAINER_ID,'UT'+CAST(SPC.SAMPLE_ID as varchar(6))) AS storageID,",
		"ISNULL(SP.SPECIMEN_SERIAL_NUMBER,0) AS SN",
	"FROM",
		"#Unique_Bin UB RIGHT OUTER JOIN",
		"SPECIMEN_COLLECTED SPC INNER JOIN",
		"SPECIMEN SP ON",
		"SP.SAMPLE_ID = SPC.SAMPLE_ID AND",
		"SP.SPECIMEN_ID = SPC.SPECIMEN_ID ON",
		"SPC.SAMPLE_ID = UB.SAMPLE_ID",
	"WHERE SPC.COLLECTED_ATTRIBUTE_CODE IN (20)",
		"AND SPC.SAMPLE_ID IN (",
		paste(usid,collapse=","),")"
	)
#browser();return()

	getData(paste(expr,collapse=" "),"GFBioSQL",strSpp=strSpp,type="SQLX",tenv=penv())
	SNdat = PBSdat
	Opool=split(paste(PBSdat$storageID,PBSdat$SN,sep="."),PBSdat$SID)
#browser();return()
	Npool = Opool[.su(as.character(sampuse$SID))]         ## Pool relevant to the n field
	Nsamp = sapply(split(sampuse[,nfld],sampuse$SID),sum) ## Number of otoliths to sample from the pool (sapply-split: because samples might be split across trays)
	Nsamp = Nsamp[order(names(Nsamp))]
	#names(sid)=tid
	Osamp = sapply(usid,function(x,O,N){                  ## Otoliths sampled randomly from pool
		xx=as.character(x); oo=O[[xx]]; nn=N[xx]; olen=length(oo)
		if (nn==0) return(NA)
		else if (olen==1) return(oo)
		else return(sample(x=oo,size=min(nn,olen),replace=FALSE)) }, 
		O=Npool, N=Nsamp, simplify=FALSE)
	names(Osamp)=usid
#browser();return()
	packList(c("expr","SNdat","Opool","Nsamp","Osamp"),"PBStool",tenv=.PBStoolEnv)

	if (redSIDrec) invisible(return(samp))
	
	fnam = paste("oto",strSpp,describe,".csv",sep="")
	clearFiles(fnam)
	#fnam = "test.csv"
	data("species", "pmfc", package="PBSdata", envir=penv())
	catnip(paste("Otolith Samples for", species[strSpp,]["name"],"(", species[strSpp,]["latin"],")"),"\n\n",file=fnam)
	if (bySID) {
	for (i in tid) {
		ii=as.character(i)
		otos=Osamp[[ii]]
		if ( all(is.na(otos))) next
		x=sampuse[is.element(sampuse$tid,i),]
		unpackList(x); ntray=0; ser1=NULL
#browser();return()
		for (j in 1:length(firstSerial)) {
			sers = firstSerial[j]:lastSerial[j]
			ser1 = c(ser1, sers[seq(1,length(sers),100)]) } ## first serials including n samples > 100 (tray)
		ntray=length(ser1)
		tray=array("",dim=c(5,20,ntray),dimnames=list(LETTERS[1:5],1:20,ser1))
		for (j in ser1) {
			serT = matrix(seq(j,j+99,1),nrow=5,ncol=20,byrow=TRUE)
			zoto = is.element(serT,otos)
			if (any(zoto))  tray[,,as.character(j)][zoto]=serT[zoto] }
		TRAY = apply(tray,1:2,function(x){paste(x[x!=""],collapse=" & ")})
		catnip("Vessel,",vessel[1],",\n",file=fnam,append=TRUE)
		catnip("Trip date,",format(tdate[1],format="%d-%b-%Y"),",",file=fnam,append=TRUE)
		catnip("Otoliths","\n",file=fnam,append=TRUE)
		catnip("Sample date,",format(sdate[1],"%d-%b-%Y"),",",file=fnam,append=TRUE)
		catnip("<",length(otos),">","\n",file=fnam,append=TRUE)
		#if (type=="C") catnip("Hail,",hail[1],",",file=fnam,append=TRUE)
		#else           catnip("FEID,",FEID[1],",",file=fnam,append=TRUE)
		catnip("Sample ID,",SID[1],",",file=fnam,append=TRUE)
		catnip("Tray,",paste(1:20,collapse=","),"\n",file=fnam,append=TRUE)
		catnip("Set,",paste(unique(set),collapse="+"),",",file=fnam,append=TRUE)
		catnip("A,",paste(TRAY[1,],collapse=","),"\n",file=fnam,append=TRUE)
		#catnip("PMFC major,",paste(unique(major),collapse="+"),",",file=fnam,append=TRUE)
		#catnip("PMFC areas,",paste(paste(unique(major),collapse="+"),paste(unique(minor),collapse="+"),sep=" - "),",",file=fnam,append=TRUE)
		catnip("PMFC,",pmfc[as.character(major[1]),"name"],",",file=fnam,append=TRUE)
		catnip("B,",paste(TRAY[2,],collapse=","),"\n",file=fnam,append=TRUE)
		#catnip("PMFC minor,",paste(unique(minor),collapse="+"),",",file=fnam,append=TRUE)
		catnip("Storage box,",storageID[1],",",file=fnam,append=TRUE)
		catnip("C,",paste(TRAY[3,],collapse=","),"\n",file=fnam,append=TRUE)
		catnip("Prefix,",prefix[1],",",file=fnam,append=TRUE)
		catnip("D,",paste(TRAY[4,],collapse=","),"\n",file=fnam,append=TRUE)
		catnip("First serial,",paste(firstSerial,collapse=" | "),",",file=fnam,append=TRUE)
		catnip("E,",paste(TRAY[5,],collapse=","),"\n",file=fnam,append=TRUE)
		catnip("Last serial,",paste(lastSerial,collapse=" | "),"\n\n",file=fnam,append=TRUE)
	} }
	else {
		isUNK = is.element(sampuse$storageID,"UNK")
		#if (any(isUNK)) sampuse$storageID[isUNK] = paste("UNK",sampuse$TID_gfb,sep="")
		if (any(isUNK)) stop("Unknown storageID needs debugging")
			#sampuse$storageID[isUNK] = paste("UNK_",sampuse$SID[isUNK],sep="")
		#utray = .su(sampuse[["storageID"]])
		tdate.storage=sapply(split(sampuse$tdate,sampuse$storageID),function(x){ xmin=min(x);substring(xmin,1,10)})
		trid = names(tdate.storage[order(tdate.storage)])
#browser();return()
		for (i in trid) {
			Otos = NULL
			itid = as.character(traytids[[i]])
			for (j in itid) {
				jsid = as.character(tripsids[[j]])
				for (k in jsid) {
					ksamp = Osamp[[k]]
					ktemp = ksamp[grep(i,ksamp)]
					if (length(ktemp)==0) next
					Otos = c(Otos,gsub(paste(i,".",sep=""),"",ktemp))
				}
			}
			if ( is.null(Otos) || all(is.na(Otos))) next
			x = sampuse[is.element(sampuse$storageID,i),] ## use because some samples span trays
			unpackList(x)
#browser();return()
			ocells = min(firstSerial):max(lastSerial)     ## available otoliths
			pcells = match(Otos,ocells)                   ## cell positions to take samples
			pcells = pcells[pcells>0 & !is.na(pcells)]    ## remove NAs caused by a samples spanning trays
			otos   = unique(ocells[pcells])               ## otoliths specific to this tray (forceably remove duplicates even though they should not be here)
			TRAY   = array("",dim=c(5,20),dimnames=list(LETTERS[1:5],1:20))
			serT   = matrix(seq(ocells[1],ocells[1]+99,1),nrow=5,ncol=20,byrow=TRUE)
			#if (i=="16X:1") {browser();return()}         ## there are apparently 103 Shortraker otoliths in this trip (16X:1)
			otos   = otos[otos%in%serT]                   ## tray only has room for 100; be sure extras are excluded
#browser();return()
			if (any(is.na(pmatch(otos,serT)))) {print("NAs generated, probably duplicated oto numbers");browser();return()}
			TRAY[pmatch(otos,serT)] = otos
#if (i=="9H:11100") {browser();return()}
			catnip("Vessel,",vessel[1],",\n",file=fnam,append=TRUE)
			catnip("Trip date,",format(tdate[1],format="%d-%b-%Y"),",",file=fnam,append=TRUE)
			catnip("Otoliths","\n",file=fnam,append=TRUE)
			catnip("Sample date,",format(sdate[1],"%d-%b-%Y"),",",file=fnam,append=TRUE)
			catnip("<",length(otos),">","\n",file=fnam,append=TRUE)
			catnip("Sample ID,",ifelse(min(SID)==max(SID),SID,paste(min(SID),max(SID),sep=" to ")),",",file=fnam,append=TRUE)
			catnip("Tray,",paste(1:20,collapse=","),"\n",file=fnam,append=TRUE)
			catnip("Set,",ifelse(min(set)==max(set),set,paste(min(set),max(set),sep=" to ")),",",file=fnam,append=TRUE)
			catnip("A,",paste(TRAY[1,],collapse=","),"\n",file=fnam,append=TRUE)
			catnip("PMFC,",paste(pmfc[as.character(.su(major)),"gmu"],collapse="+"),",",file=fnam,append=TRUE)
			catnip("B,",paste(TRAY[2,],collapse=","),"\n",file=fnam,append=TRUE)
			catnip("Bin:Tray,",storageID[1],",",file=fnam,append=TRUE)
			catnip("C,",paste(TRAY[3,],collapse=","),"\n",file=fnam,append=TRUE)
			catnip("Prefix,",prefix[1],",",file=fnam,append=TRUE)
			catnip("D,",paste(TRAY[4,],collapse=","),"\n",file=fnam,append=TRUE)
			catnip("First serial,",min(firstSerial),",",file=fnam,append=TRUE)
			catnip("E,",paste(TRAY[5,],collapse=","),"\n",file=fnam,append=TRUE)
			catnip("Last serial,",max(lastSerial),"\n\n",file=fnam,append=TRUE)
	}	}
	invisible(samp)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~requestAges


## residVB------------------------------2024-10-24
## Calculate and plot Studentised residuals as 
## quantile plots to show how well each age is fit.
## Need to run 'calcVB' first to get DATA
## ---------------------------------------------RH
residVB <- function(P, L1="surv", L2="Females", yval="len", 
   area="major_3|4", ttype="surv_2|3", Amax, ylim,
   png=FALSE, pngres=400, PIN=c(8,8), lang=c("e","f"))
{
	ttget(DATA)
	if (is.null(DATA)) stop("Run function 'calcVB' first.")
	if (missing(Amax))
		Amax =  max(sapply(L2, function(x,dat){range(dat[[x]]$age)},dat=DATA[[L1]]))
	## Create a subdirectory called `french' for French-language figures
	createFdir(lang)

	do.call("assign", args=list(x="boxpars", value=list(
		boxwex=0.8, boxfill="gainsboro", staplewex=0.5, 
		medlwd=2, medcol=lucent("slategrey",0.75), 
		outwex=0.5, whisklty=1, outpch=3, outcex=0.5, outcol=lucent("grey20",0.5)
		))) #, envir=.PBStoolEnv))

	fout.e = paste0("VBres-",gsub("\\|","+",gsub("_","(",area)),")-",gsub("\\|","+",gsub("_","(",ttype)),")")
	for (l in lang) {  ## could switch to other languages if available in 'linguaFranca'.
		changeLangOpts(L=l)
		#fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		fout = switch(l, 'e' = paste0("./english/",fout.e), 'f' = paste0("./french/",fout.e) )
		if (png) {
			clearFiles(paste(fout,".png"))
			png(filename=paste(fout,".png"), unit="in", res=pngres, width=PIN[1], height=PIN[2])
		}
		if (length(L2)>1)
			expandGraph(mfrow=c(length(L2),1), mar = c(1,1,0.5,0.5), oma=c(2,2,0,0))
		else
			expandGraph(mfrow=c(length(L2),1), mar = c(3,3,0.5,0.5), oma=c(0,0,0,0))
	
		for (i in L2) {
			VBdat = DATA[[L1]][[i]][,c(yval,"age")]
			iP    = P[i, c("Yinf","K","t0"), ttype, area]
			unpackList(iP)
			TL    = VBdat[,yval]
			Age   = VBdat$age
			pars  = list(Yinf=Yinf, K=K, t0=t0) ## Vector of initial values (sans sig)
		
			enough.points   = nrow(VBdat) >= 10
			enough.contrast = diff(range(Yinf*(1-exp(-K*(Age-t0))))) > 2
			if (enough.points && enough.contrast) {  ## nls cannot fit when calculation has no contrast
				fit1 <- try(nls(TL~Yinf*(1-exp(-K*(Age-t0))),start=pars),silent=TRUE)  ## Use nls for residual checking
				if (class(fit1)[1] != "try-error") {
					res.norm = residuals(fit1)
					## http://www.mathworks.com/matlabcentral/newsreader/view_thread/330668 [defunct]
					## https://www.mathworks.com/help/stats/residuals.html
					r   = matrix(res.norm,ncol=1)
					h   = matrix(hat(Age),ncol=1)                          ## equiv: (X-mean(X))^2/sum((X-mean(X))^2) + 1/length(X)
					MSE = as.vector((t(r)%*%r)/(length(Age)-length(pars))) ## sum of residuals squared / (number of observations - number parameters estimated)
					res.stud = r/(sqrt(MSE*(1-h)))                         ## same as: res.norm/sd(res.norm)
#browser();return()
					#Amax = max(Age)
					Abox = as.list(rep(NA,Amax)); names(Abox)=1:Amax
					abox = split(res.stud,Age)
					Abox[names(abox)] = abox
	
					if (missing(ylim)) srlim = range(res.stud, na.rm=TRUE)
					else               srlim = ylim
	
					quantBox(Abox, type="n", xaxt="n", las=1, outline=T, pars=boxpars, ylim=srlim)
					abline(h=0, col=lucent("blue",0.5), lty=5)
					axis(1, at=seq(0,Amax,5))
					if (length(L2)>1) {
						if (i==L2[1]) {
							mtext(linguaFranca("Predicted Age (y)",l), outer=TRUE, side=1, line=0.75, cex=1.5)
							mtext(linguaFranca("Standardised Residuals",l), outer=TRUE, side=2, line=0.5, cex=1.5)
						}
					} else {
						mtext(linguaFranca("Predicted Age (y)",l), side=1, line=1.75, cex=1.5)
						mtext(linguaFranca("Standardised Residuals",l), side=2, line=1.5, cex=1.5)
					}
#browser();return()
					addLabel(0.98,0.95, paste(linguaFranca(i,l), linguaFranca(area,l), linguaFranca(ttype,l),sep="\n"), adj=c(1,1), cex=1.0)
	
					#z   = order(Age)
					#plot(Age,res.stud, type="n", xlab="", ylab="")
					#text(Age[z], res.stud[z], labels=Age[z], cex=0.8)
				}
			}
		}
		if (png) dev.off()
	}; eop()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~residVB


#simBSR---------------------------------2011-06-14
# Simulate Blackspotted Rockfish biological data.
#-----------------------------------------------RH
simBSR <- function(Nfish)
{
	SL  = runif(Nfish,95.5,539)   # Standard length (mm)
	L1 = rnorm(Nfish,7.8,0.7)     # Length of dorsal-fin spine 1 (%SL)
	L2 = rnorm(Nfish,8.0,0.6)     # Snout length (%SL)
	L3 = rnorm(Nfish,5.6,0.6)     # Length of gill rakers (%SL)
	L4 = rnorm(Nfish,21.4,1.2)    # Length of pelvic-fin rays (%SL)
	L5 = rnorm(Nfish,21.4,1.5)    # Length of soft-dorsal-fin base (%SL)
	L6 = rnorm(Nfish,70.2,2.6)    # Preanal length (%SL)
	N1 = rnorm(Nfish,33.0,1.2)    # Number of gill rakers
	N2 = rnorm(Nfish,13.7,0.5)    # Number of dorsal-fin rays

	S = cbind(SL)
	L = cbind(L1,L2,L3,L4,L5,L6); L = sweep(L,1,SL,"*")/100
	N = cbind(N1,N2)
	SLN = cbind(S,L,N); dimnames(SLN)=list(1:Nfish,c("S","L1","L2","L3","L4","L5","L6","N1","N2"))
	attr(SLN,"spp") = "BSR"
	return(SLN)
}

#simRER---------------------------------2011-06-14
# Simulate Rougheye Rockfish biological data.
#-----------------------------------------------RH
simRER <- function(Nfish)
{
	SL  = runif(Nfish,63.4,555.2) # Standard length (mm)
	L1 = rnorm(Nfish,5.8,0.6)     # Length of dorsal-fin spine 1 (%SL)
	L2 = rnorm(Nfish,7.5,0.7)     # Snout length (%SL)
	L3 = rnorm(Nfish,4.9,0.6)     # Length of gill rakers (%SL)
	L4 = rnorm(Nfish,22.1,1.1)    # Length of pelvic-fin rays (%SL)
	L5 = rnorm(Nfish,22.8,1.2)    # Length of soft-dorsal-fin base (%SL)
	L6 = rnorm(Nfish,71.8,2.3)    # Preanal length (%SL)
	N1 = rnorm(Nfish,31.2,1.0)    # Number of gill rakers
	N2 = rnorm(Nfish,13.5,0.5)    # Number of dorsal-fin rays

	S = cbind(SL)
	L = cbind(L1,L2,L3,L4,L5,L6); L = sweep(L,1,SL,"*")/100
	N = cbind(N1,N2)
	SLN = cbind(S,L,N); dimnames(SLN)=list(1:Nfish,c("S","L1","L2","L3","L4","L5","L6","N1","N2"))
	attr(SLN,"spp") = "RER"
	return(SLN)
}


## sumBioTabs---------------------------2022-03-15
## Summarize frequency occurrence of biological samples
## and specimens and send output to a data table.
## ---------------------------------------------RH
sumBioTabs <- function(dat, fnam="sumBioTab.csv", samps=TRUE, specs=TRUE,
   facs=list(c("year","major"), c("year","ttype"), c("year","stype"),
   c("year","ameth"), c("year","gear") ) )
{
	if(samps) {
		if (!requireNamespace("reshape", quietly=TRUE)) stop("`reshape` package is required")
	}
	cat("Summary Tables\n\n",file=fnam)
	for (f in facs) {
		cat(paste(f,collapse=" vs "),": ",file=fnam,append=TRUE)
		if (samps) {
			molten = reshape::melt.data.frame(dat,f,"SID")
			sa = reshape::cast(molten,paste(f,collapse="~"),function(x){length(unique(x))}) 
			cat(paste("Samples",ifelse(specs," & ","\n"),sep=""),file=fnam,append=TRUE) }
		if (specs) {
			sp=table(dat[[f[1]]],dat[[f[2]]])
			sp=matrix(sp,nrow=nrow(sp),dimnames=dimnames(sp))
			cat("Specimens\n",file=fnam,append=TRUE) }
		if (samps) out=data.frame(A=sa)
		if (samps & specs) out=data.frame(out,C.=rep("",nrow(sa)))
		if (specs & !samps) { 
			out=rownames(sp)
			eval(parse(text=paste("out=as.",class(dat[[f[1]]]),"(out); out=data.frame(",f[1],"=out)",sep=""))) }
		if (specs) out=data.frame(out,B=sp)
		header=gsub("[ABC]\\.","",names(out))
		cat(paste(header,collapse=","),"\n",file=fnam,append=TRUE)
		write.table(out,fnam,sep=",",row.names=FALSE,col.names=FALSE,append=TRUE)
		cat("\n",file=fnam,append=TRUE)
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~sumBioTabs


## -------------------------------------2024-11-08
## Tabulate mean/median weight (or some other measure like len)
## (not in package yet)
## ---------------------------------------------RH
tabMW <- function(dat, flds=c("year","SSID"), zfld="wt", ttype, 
  q=c(0.05,0.25,0.50,0.75,0.95), spp.name,
  debug=TRUE, tabs=TRUE, plot=TRUE, outnam="Species-Weights", 
  png=FALSE, pngres=400, PIN=c(10,10), lang=c("f","e"))
{
	dat   = dat[dat[,zfld]>0 & !is.na(dat[,zfld]),]
	if (!missing(ttype))
		dat = dat[is.element(dat$ttype, ttype),]
	if (missing(spp.name)) {
		if ("spp" %in% colnames(dat)) {
			data ("species", package="PBSdata", envir=penv())
			spp.name = toUpper(tolower(species[as.character(.su(dat$spp)),"name"]))
		} else {
			spp.name = "Unknown species"
		}
	}
	if (!debug && !is.null(tcall(wlist))) {
		tget(wlist)
	} else {
		## Colllect results
		wlist = list()
		for (i in 1:length(flds)){
			ii    = flds[i]
			imean = crossTab(dat, ii, zfld, mean)
			nmean = crossTab(dat, ii, zfld, countVec)
			iii   = as.numeric(names(imean))
			if (ii %in% c("year"))
				iii = iii[1]:rev(iii)[1]
			idat  = split(dat[,zfld], coalesce(dat[,ii],rep(999,nrow(dat))))
			ilist = as.list(rep(NA,length(iii))); names(ilist)=iii
			ilist[names(idat)] = idat
			qdat  = sapply(idat, function(x) { quantile(x, q, an.rm=TRUE) })
			wlist[[ii]][["nobs"]] = nmean
			wlist[[ii]][["mean"]] = imean
			wlist[[ii]][["qnts"]] = qdat
			wlist[[ii]][["vals"]] = ilist
		}
		wlist[["cross"]][["nobs"]] = crossTab(dat, flds, zfld, countVec)
		wlist[["cross"]][["mean"]] = crossTab(dat, flds, zfld, mean)
		wlist[["cross"]][["qnts"]] = crossTab(dat, flds, zfld, function(x){quantile(x,q)})
		tput(wlist) ## save to shorten debugging
	}
	ssid = c('1'="QCS synoptic", '3'="HS synoptic", '4'="WCVI synoptic", '8'="1997 WCHG rockfish", '14'="IPHC Longline", 
		'16'="WCHG synoptic", '21'="GIG historical", '22'="HBLL N outside", '34'="SoG acoustic", '36'="HBLL S outside",
		'39'="HBLL N inside", '40'="HBLL S inside", '45'="SoG synoptic", '48'="Dogfish surveys", 
		'51'="Submersible+LL", '68'="Hake acoustic", '76'="SoG Dogfish LL", '79'="NMFS triennial",
		'350'="Sablefish trap", '670'="Shrimp trawl", '820'="Jig surveys", '999'="Commercial fishery")
	ttype = c('1'="Non-obs domestic", '2'="Research", '3'="Charter", '4'="Obs domestic", '5'="Observed JV", 
		'6'="Non-obs JV", '7'="Polish comm natl", '8'="Russian comm natl", '9'="Russian comm supp", '10'="Polish comm supp",
		'11'="Recreational", '12'="Japanese obs natl", '13'="Japanese comm", '14'="Unknown foreign")
	gear = c('0'="Unknown", '1'="Bottom trawl", '2'="Trap", '3'="Gillnet", '4'="Handline", '5'="Longline", '6'="Midwater trawl",
		'7'="Troll", '8'="Unknown trawl", '9'="Seine", '10'="Jig", '11'="Shrimp trawl", '12'="Recreational", '13'="Prawn trap", 
		'14'="Tucker trawl", '15'="Plankton sampler", '16'="Twin beam trawl", '17'="Hand seine", '18'="Purse seine", 
		'19'="Bongo net", '20'="Profiling CTD", '21'="Camera", '22'="DAISY")
	if (tabs) {
		iflds = c(flds, "cross")
		jflds = c("nobs","mean","qnts")
		fname = paste0(outnam,".csv")
		cat(paste0("Results for '", zfld, "' by '", paste0(flds, collapse="' and '"), "'\n"), file=fname)
		for (i in 1:length(iflds)) {
			ii = iflds[i]
			if (ii %in% flds) {
				cat("\n", zfld, " by ", ii, ":\n", sep="", file=fname, append=TRUE)
				ftab = data.frame(nobs=wlist[[ii]][["nobs"]], mean=wlist[[ii]][["mean"]], t(wlist[[ii]][["qnts"]]))
				colnames(ftab)[3:7] = c("q05","q25","median","q75","q95")
				if (ii=="SSID")
					rownames(ftab) = paste0(rownames(ftab), ": ", ssid[rownames(ftab)])
				if (ii=="ttype")
					rownames(ftab) = paste0(rownames(ftab), ": ", ttype[rownames(ftab)])
				if (ii=="gear")
					rownames(ftab) = paste0(rownames(ftab), ": ", gear[rownames(ftab)])
				cat(paste0(c(ii,colnames(ftab)), collapse=","), "\n", sep="", file=fname, append=TRUE)
#if(i==2) {browser();return()}
				for (j in 1:nrow(ftab))
					cat(paste0(c(rownames(ftab)[j], ftab[j,]), collapse=","), "\n", sep="", file=fname, append=TRUE)
			} else {
				cat("\n", zfld, " by ", paste0(flds, collapse=" and "), ":\n", sep="", file=fname, append=TRUE)
				for (j in 1:length(jflds)) {
					jj = jflds[j]
					ftab = wlist[[ii]][[jj]]
#if (jj=="qnts") {browser();return()}
					if (jj=="qnts"){
						ftab.tmp = ftab[,,"50%"]  ## extract medians
						if (is.null(dim(ftab.tmp))) {
							ftab = matrix(ftab.tmp,nrow=nrow(ftab),ncol=ncol(ftab), dimnames=dimnames(ftab)[1:2])
						} else {
							ftab = ftab.tmp
						}
					}
					cat(ifelse(jj=="qnts","median",jj), ":\n", sep="", file=fname, append=TRUE)
					cat(paste0(c(paste0(flds,collapse="|"), colnames(ftab)), collapse=","), "\n", sep="", file=fname, append=TRUE)
					for (k in 1:nrow(ftab))
						cat(paste0(c(rownames(ftab)[k], ftab[k,]), collapse=","), "\n", sep="", file=fname, append=TRUE)
				}
			}
		}
	}
	if (plot) {
		createFdir(lang)
		fout.e = outnam
		for (l in lang) {
			changeLangOpts(L=l)
			fout = switch(l, 'e' = paste0("./english/",fout.e), 'f' = paste0("./french/",fout.e) )
			if (png) {
				clearFiles(paste0(fout,".png"))
				png(filename=paste0(fout,".png"), width=PIN[1], height=PIN[2], units="in", res=pngres)
			}
#browser();return()
			expandGraph(mfrow=c(length(flds),1), mar=c(3,3,0,0), oma=c(ifelse(length(wlist[[rev(flds)[1]]][["vals"]])>10,ifelse(lang=="l",6,5),0),0,1.5,0.5), mgp=c(1.6,0.5,0), las=1)
			for (i in 1:length(flds)) {
				ii = flds[i]
				vdat = wlist[[ii]][["vals"]]
				mdat = wlist[[ii]][["mean"]]
				xpos = match(names(mdat),names(vdat))
				nobs = wlist[[ii]][["nobs"]]
				if (ii=="SSID")
					names(vdat) = ssid[names(vdat)]
				if (ii=="ttype")
					names(vdat) = ttype[names(vdat)]
				if (ii=="gear")
					names(vdat) = gear[names(vdat)]
				## Display results
				boxpars = list(boxwex=ifelse(ii%in%c("SSID","ttype","gear"),0.5,0.8), boxcol="slategray", boxfill="gainsboro", medcol="navyblue", medlwd=2, whisklty=1, whiskcol="slategray", staplecol="slategray", outpch=95, outcex=0.5, outcol="slategray")
				quantBox(vdat, pars=boxpars, names=rep("",length(vdat)), xaxt="n", ylab=linguaFranca("Fish weight (kg)",l), cex.lab=1.5)
				axis(1, at=xpos, linguaFranca(names(vdat)[xpos],l), las=ifelse(length(xpos)>10,2,1), tcl=-0.25)
				mtext(linguaFranca(paste0(spp.name, " weights by ", flds[1], " and ", flds[2]),l), side=3, outer=T, line=0.25, cex=1.25)
				#points(xpos, mdat, pch=21, col="green3", bg=lucent("green3",0.5), cex=1)
				points(xpos, mdat, pch=21, col="blue", bg=lucent("cyan",0.5), cex=0.8)
				addLegend(0.05,0.9, legend=linguaFranca(c("median","mean"),l), lty=c(1,NA), pch=c(NA,21), col=c("navyblue","blue"), pt.bg=c(NA,"cyan"), bty="n", xjust=0, lwd=c(2,1))
				text(xpos, par()$usr[4]-0.01*diff(par()$usr[3:4]), nobs, cex=0.7, adj=c(1,0.5), srt=90)
				addLabel(0.0075,0.98, paste0("N ", convUTF("\\u{2192}")), cex=0.8, adj=c(0,1))
			}
			if (png) dev.off()
		}; eop()
#browser();return()
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tabMW


## weightBio----------------------------2025-06-05
## Weight age|length frequencies|proportions by catch|density.
##   adat = age123 from query 'gfb_bio.sql'    -- e.g., getData("gfb_bio.sql","GFBioSQL",strSpp="607",path=.getSpath()); bio607=processBio()
##   cdat = cat123gfm -- call 'fos_mcatSPP.sql' to get commercial catches from GFFOS' GF_MERGED_CATCH table. (RH 190807)
##        = cat123gfb -- call 'gfb_catch_records.sql' to get survey catches from GFBioSQL. (RH 191016)
##   [DEPRECATED] cat123.wB from function 'getCatch' -- e.g., cat607 = getCatch("607",sql=TRUE)
## Note: Any modifications to 'gfb_bio.sql' may require similar changes in 'gfb_catch_records.sql'
## NOAA PDO data: https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat
## ---------------------------------------------RH
weightBio <- function(adat, cdat, sunit="TID", sweight="catch", 
   ttype=NULL, stype=c(0,1,2,5:8,17), scat=NULL, ameth=c(2:4,17), sex=2:1, major=NULL,
   wSP=c(TRUE,TRUE), wN=TRUE, plus=60, accum=TRUE, Nmin=0, Amin=NULL, 
   ctype="C", per=90, SSID=NULL, tabs=TRUE, gear=NULL,
   plot=TRUE, ptype="bubb", size=0.05, powr=0.5, zfld="wp", 
   clrs=list( c(.colBlind["blue"],"cyan"), c(.colBlind["bluegreen"],"chartreuse")),
   cohorts=NULL,
   regimes=list(1900:1908, 1912:1915, 1923:1929, 1934:1943,1957:1960, 1976:1988, 1992:1998, 2002:2006, 2014:2017), ## +'ve annual PDO (as of 211130)
   layout="portrait", win=TRUE, eps=FALSE, pdf=FALSE, png=FALSE, wmf=FALSE,
   longside=10, outnam, outres=400, ioenv=.GlobalEnv, lang=c("e","f"), ...)
{
	## Determine function for level-1 weighting
	if (sweight=="density") func=mean else func=sum  ## perhaps try the geometric mean 'calcGM' in future
	#if (sweight=="density") func=sum else func=sum  ## this seems suspect but leave for now (does not affect WWR because survey strata only have 1 sample each)

	if (inherits(adat,"character") || is.null(adat)) {  ## (RH 240117)
		expr = paste("getFile(",substitute(adat),",",substitute(cdat),",ioenv=ioenv,try.all.frames=TRUE,tenv=penv())",sep="")
		eval(parse(text=expr))
	}
	strSpp=attributes(adat)$spp; if(is.null(strSpp)) strSpp="000"
	assign("PBStool",list(module="M02_Biology",call=match.call(),args=args(weightBio),ioenv=ioenv,spp=strSpp),envir=.PBStoolEnv)
	sysyr=as.numeric(substring(Sys.time(),1,4))    ## maximum possible year
	if (is.null(major)) {
		adat$major[is.null(adat$major)] = 0
		cdat$major[is.null(cdat$major)] = 0
		major = sort(unique(adat$major)) }
	if (is.null(ttype)) {
		if (ctype=="C")      ttype = c(1,4,5) ## commercial
		else if (ctype=="S") ttype = c(2,3)   ## research/survey
		else                 ttype = 1:14 }

	## Age data
	adat$SVID[is.na(adat$SVID)]=0; adat$GC[is.na(adat$GC)]=0; adat$area[is.na(adat$area)]=1 ## to avoid grouping errors later on
	ages = adat
	if (any(sex==12)) { # thanks brian
			age12 = ages[is.element(ages$sex,1:2),]
		age12$sex = 12
		ages= rbind(ages,age12)
	}
	flds=c("ttype","gear","stype","sex","major","scat")
	if (ctype=="S" && !is.null(SSID)) flds=c(flds,"SSID")
	ii = character()
	for (i in flds) {
		ii = paste0(ii,ifelse(i==flds[1],"",":"),i)
#if (i=="SSID") {browser();return()}
		expr = paste("ages=biteData(ages,",i,")",sep=""); eval(parse(text=expr))
		.flush.cat(paste0(i,": ",nrow(ages)), "\n")
		if (nrow(ages)==0) showError(paste0("No data for field selection ",ii))
	}
#browser();return()
	#if (nrow(ages)==0) showError(paste("No data for '",paste(flds,collapse=', '),"' chosen",sep=""))
	nsex   = length(.su(ages$sex))
	names(ages)[grep("scat",names(ages))] = "spcat" #rename `scat' field to `spcat' to avoid conflict with object`scat' later in code.

	## Specific fiddle to get only 1994 age data for SSID 21 (GIG Historic)
	#if (!is.null(SSID) && SSID==21 && !is.element(strSpp,c("417","440")))
	#	ages = ages[is.element(ages$year, 1994),]  ## this does not work for WWR

	## There is no known (recorded) ameth for NMFS Triennial survey
#browser();return()
	if (!is.null(SSID) && all(SSID==79))  ## (RH 241120)
		ameth = .su(c(0,ameth))

	#ages = extractAges(ages, ameth, rmSM=FALSE) ## use seamounts for REBS french fig equivalent
	ages = extractAges(ages, ameth, rmSM=TRUE)
#browser();return()
	if (accum)
		ages$age[ages$age>=plus & !is.na(ages$age)] = plus
	else
		ages = ages[ages$age<=plus & !is.na(ages$age),]
#browser();return()

	if (strSpp=="621" && ctype=="C") ages = ages[ages$age>=4 & !is.na(ages$age),]
	## If weighting by [sweight], you need a positive [sweight] :
	#if (wSP[1]) ages = ages[ages[,sweight]>0 & !is.na(ages[,sweight]),]
	## Sometimes catch weight is not reported but the age sample is still OK.
	ages[,sweight][ages[,sweight]==0 | is.na(ages[,sweight])] = 1. ## set low smaple catch

	## Restratify the survey age data (because there are numerous restratification schemes not reflected in B21)
	if (ctype=="S"){
		cdat   = cdat[is.element(cdat$SSID,SSID),]
		cdat$SVID[is.na(cdat$SVID)]=0; cdat$GC[is.na(cdat$GC)]=0 ## to avoid grouping errors later on
		feid   = cdat$FEID
		strata = cdat$GC; names(strata)  = feid     ## take stratification scheme from survey catch data
		SVID   = cdat$SVID;  names(SVID) = feid
#browser();return()
		ages   = ages[is.element(ages$FEID,feid),]  ## get ages associated with the survey series
		if (nrow(ages)==0) showError(paste("No age data left after matching FEID of survey series ",
			SSID,"\n(catch linked to age via 'FEID')",sep=""),as.is=TRUE)
		ages$SVID  = SVID[as.character(ages$FEID)]  ## restratified survey IDs
		ages$GC = strata[as.character(ages$FEID)]   ## restratified survey IDs
		## In ages for specified GC, some records are missing 'SSID' and 'area' (stratum area)
		zSSID = !is.element(ages$SSID,SSID)
		if (any(zSSID))
			ages$SSID[zSSID] = SSID[1]
		zarea = ages$area<=0 | is.na(ages$area)
		if (any(zarea)) {
			areas = ages$area; names(areas) = ages$EID
			garea = split(areas,ages$GC) # group areas
			farea = sapply(garea,function(x){ # fixed areas
				z = x<=0 | is.na(x)
				if (!any(z)) return(x)
				else if (all(z)) return(rep(0,length(x)))
				else mval = mean(unique(x[!z]),na.rm=TRUE)
				x[z] = mval; return(x)
			})
			for (a in farea)
				ages$area[is.element(ages$EID,names(a))] = a
		}
	}

	## Derive sample unit (e.g., 'TID' or 'SID') data
	sfeid = sapply(split(ages[,"FEID"], ages[,sunit]), function(x){as.character(sort(unique(x)))}, simplify=FALSE)
	fcat  = sapply(split(ages[,sweight],ages$FEID),
		function(x){if(all(is.na(x))) 0 else mean(x,na.rm=TRUE)})   ## [sweight] by each fishing event (tow)

	## [sweight] by sample unit (i.e., catch or density)
	## Just in case a sample covers more than one tow (fishing event)
	## Not likely to happen in surveys and so using sum or mean will have no effect *****
	scat  = sapply(sfeid,function(x,cvec){func(cvec[x],na.rm=TRUE)},cvec=fcat)
	sdate = sapply(split(ages[,"date"],ages[,sunit]),function(x){as.character(rev(sort(unique(x)))[1])})
	ages$scat  = scat[as.character(ages[,sunit])]                  ## populate age data with sample unit [sweight]
	ages$sdate = sdate[as.character(ages[,sunit])]                 ## populate age data with sample unit dates

	if (ctype=="S"){
		slev = paste(ages$year,pad0(ages$SVID,3),pad0(ages$GC,3),sep=".")  ## sample survey year.survey.group
		names(slev) = paste(ages$year,pad0(ages$SVID,3),pad0(ages$GC,3),sep="-")
		attr(slev,"Ylim") = range(ages$year,na.rm=TRUE)
		#attr(slev,"years") = .su(ages$year)
		SLEV = paste(adat$year,pad0(adat$SVID,3),pad0(adat$GC,3),sep=".")  ## larger dataset
	} else {
		slev = convYP(ages$sdate,per)     ## sample commercial year.period (e.g. quarter)
		SLEV = convYP(ages$date,per)
	}
	attr(slev,"years") = .su(ages$year)  ## need for sumtab for surveys and commercial
	agelev=sort(unique(slev))            ## unique sample unit periods
	ages$slev = slev
	if (ctype=="S") {
		ageLEV=sort(unique(SLEV))
		adat$SLEV = SLEV                  ## only used for surveys to get all potential stratum areas later
	}
	packList("ages","PBStool",tenv=.PBStoolEnv)
	major = .su(ages$major)

	## Collect age frequency by sample unit and calculate proportions
	sagetab=array(0,dim=c(plus,length(scat),length(sex),2),
		dimnames=list(age=1:plus,sunit=names(scat),sex=sex,np=c("n","p")))   ## age nos. & props. by year-level
	ii=as.character(1:plus)
	for (k in sex) {
		kk=as.character(k)
		kdat=ages[is.element(ages$sex,k),]
		kage=split(kdat$age,kdat[,sunit])
		khis=sapply(kage,function(x){xy=hist(x,plot=FALSE,breaks=seq(0.5, plus + 0.5, 1)); return(list(xy))})
		knat=sapply(khis,function(x){x$counts}); rownames(knat)=ii
		jj=colnames(knat)
		sagetab[ii,jj,kk,"n"] = knat[ii,jj]
	}
	sN = apply(sagetab[,,,"n",drop=FALSE],2,sum)   ## total aged fish by sample (thanks Andy)
	for (k in sex) {
		kk=as.character(k)
		sagetab[ii,names(sN),kk,"p"] = t(apply(sagetab[ii,names(sN),kk,"n",drop=FALSE],1,function(x){x/sN}))
	}
	## Sample unit [sweight] (from 'scat' above)
	punits=split(ages[,sunit],ages$slev)
	punit=sapply(punits,function(x){as.character(sort(unique(x)))},simplify=FALSE)

	## Proportion sample unit [sweight] by level -- treat catch and density the same in standardisation
	## Note that `scat' is likely the same whether it is a sum or a mean because a sample usually only covers one fishing event.
	pprop=sapply(punit,simplify=FALSE,function(x,cvec){
		if (sum(cvec[x])==0) cvec[x] else cvec[x]/sum(cvec[x])},cvec=scat)

	## Sample unit [sweight] by level -- total catch (t) or mean density (t/km2)
	pcat = atmp = sapply(punit,function(x,cvec){
		xval = cvec[x]/ifelse(sweight=="catch",1000.,1000.)  ## convert catch and density to tonnes
		return(func(xval,na.rm=TRUE)) },  cvec=scat)         ## func now defined at beginning of weightBio
	vcat = sapply(punit,function(x,cvec){                   ## just for debugging (not used any where else)
		xval = cvec[x]/ifelse(sweight=="catch",1000.,1000.)
		return(xval) },  cvec=scat)
	#if (round(sum(pcat),5)!=round(sum(scat)/ifelse(sweight=="density",1.,1000.),5))
		#showError("Sample unit catch was not allocated correctly")

	pagetab=array(0,dim=c(plus,length(pprop),length(sex),3),
		dimnames=list(age=1:plus,lev=names(pprop),sex=sex,np=c("n","p","w"))) ## nos., props, weighted n or p by year-level
	for (j in names(pprop)){
		jvec=pprop[[j]];  jj=names(jvec)
		jone=rep(1,length(jvec)); names(jone)=jj; jpro=jone/length(jone)
		if (!wSP[1]) jvec=jpro   ## if no weighting use equal proportions
		if (all(jvec==0)) next   ## happens when survey recorded no weight of strSpp
		for (k in sex) {
			kk=as.character(k)
			ntab=sagetab[,,kk,"n"]; ptab=sagetab[,,kk,"p"]; wtab=sagetab[,,kk,ifelse(wN,"n","p")]
			if(dim(sagetab)[2]==1) {
				ntab=array(ntab,dim=dim(sagetab)[1:2],dimnames=dimnames(sagetab)[1:2])
				ptab=array(ptab,dim=dim(sagetab)[1:2],dimnames=dimnames(sagetab)[1:2])
				wtab=array(wtab,dim=dim(sagetab)[1:2],dimnames=dimnames(sagetab)[1:2])
			}
			kpro=wtab[,jj,drop=FALSE]%*%jvec  ## for each sex, weight each level n/p vector and sum
			pagetab[1:plus,j,kk,"n"] = ntab[1:plus,jj,drop=FALSE] %*% jone ## row sum
			pagetab[1:plus,j,kk,"p"] = ptab[1:plus,jj,drop=FALSE] %*% jpro ## row mean
			pagetab[1:plus,j,kk,"w"] = kpro[1:plus,1]
		}
	}
	psums=apply(pagetab,c(2,4),sum)  ## calculate sum by 'lev' and 'np'
	if (wN) {
		inflate=psums[,"n"]/psums[,"w"]
		inflate[is.na(inflate)]=1
		for (k in sex) {
			kk=as.character(k)
			if (length(pprop)==1)
				pagetab[ii,j,kk,"w"] = pagetab[ii,j,kk,"w"] * inflate
			else
				pagetab[ii,names(inflate),kk,"w"] = t(apply(pagetab[ii,names(inflate),kk,"w"],1,function(x){x*inflate}))
		}
	}

	yrs  = floor(as.numeric(substring(names(pcat),1,8))-0.001)
#browser();return()

	uyrs = .su(yrs)
	names(atmp) = yrs
	acat = sapply(split(pcat,yrs),func,na.rm=TRUE) ## total sample [sweight] per year
	pvec = pcat/acat[names(atmp)]                  ## proportion of annual sample tow [sweight] in level (quarter/stratum)

	## Fishery catch data
	## ==================
	catdat = cdat[!is.na(cdat$date),]
	catdat = catdat[is.element(catdat$year,uyrs) & is.element(catdat$major,major),]
	if (nrow(catdat)>0) {
		if (ctype=="S"){
			clev = paste(catdat$year,pad0(catdat$SVID,3),pad0(catdat$GC,3),sep=".") ## catch level year.survey.group
			names(clev) = paste(catdat$year,pad0(catdat$SVID,3),pad0(catdat$GC,3),sep="-")
			attr(clev,"Ylim") = range(catdat$year,na.rm=TRUE)
			attr(clev,"years") = .su(catdat$year)
		} else
			clev   = convYP(catdat$date,per)  ## catch level year.period
		catdat$lev = clev
		#catdat=catdat[is.element(catdat$per,agelev),]  ## only look at fishery catch that matches age periods
		packList("catdat","PBStool",tenv=.PBStoolEnv)

		## Fishery catch proportions
		if (is.element("catKg",names(catdat)))      Ccat = split(catdat$catKg,catdat$lev)
		else if (is.element("catch",names(catdat))) Ccat = split(catdat$catch,catdat$lev)
		else showError("Fishery catch field 'catKg' or 'catch' is not available")
		PCAT = sapply(Ccat,function(x){sum(x,na.rm=TRUE)/1000.})  ## fishery catch (t) for all levels
	} else {
		PCAT = rep(1,length(agelev))           ## sometimes ages exist w/out catch wt so set to a small value for now
		names(PCAT) = agelev
	}

	## Second reweighting by Quarter if commercial or by Stratum Area if survey
	#if (wSP[2]) {  ## Why has this been disabled?
		wtdMA = function(A){
			zA = A>0; zA1 = A==1; zA2 = A>1
			if (any(zA1) && !all(zA1)) {
				pAList = sapply(split(A[zA2],A[zA2]),function(x,y){x/y},y=sum(A[zA2]),simplify=FALSE)
				pArea  = sapply(pAList,sum)
				mA = sum(pArea * as.numeric(names(pArea)))
				A[zA1] = mA
			}
			return(A)
		}
		if (sweight=="catch") {
			Pcat = rep(1,length(agelev))           ## sometimes ages exist w/out catch wt so set to a small value for now
			names(Pcat) = agelev
			common = intersect(agelev,names(PCAT))
			Pcat[common] = PCAT[common]            ## add catch from common sampled levels
			ttput(Pcat)
#.flush.cat(.flush.cat(paste(strwrap(paste0(Pcat,collapse=" "),width=100),collapse="\n")), "\n")
		} else if  (sweight=="density") {
			alev = paste(ages$year,pad0(ages$SVID,3),pad0(ages$GC,3),sep=".")
			Alst = split(ages$area,alev)           ## use area (km^2) from age object
			areaRaw = sapply(Alst[agelev],unique)  ## get unique stratum area values
			if (is.list(areaRaw))
				areaRaw = sapply(areaRaw,function(x){.su(wtdMA(x))})
			area = wtdMA(areaRaw)
			PcatRaw = sapply(area,mean,na.rm=TRUE) ## fornow just use function terminology 'Pcat'
			Pcat = wtdMA(PcatRaw)
			AREAraw = sapply(split(adat$area,adat$SLEV),function(x){if(all(is.na(x))) 1 else mean(x,na.rm=TRUE)}) ## all potential stratum A from original age data file (still not complete)
			AREA = wtdMA(AREAraw)
		}
		Atmp = Pcat
		Fyrs = floor(as.numeric(substring(names(Pcat),1,8))-.001)
		names(Atmp)=Fyrs
		Acat = sapply(split(Pcat,Fyrs),sum,na.rm=TRUE)
		Pvec = Pcat/Acat[names(Atmp)]          ## proportion of annual fishery tow catch|area in level (quarter|stratum)
	#}

	## Annual proportions weighted by commercial catch|area in levels (quarter|stratum)
	names(yrs)=agelev; YRS=min(yrs):max(yrs)
	agetab = array(0, dim=c(plus,length(YRS),length(sex),4),
		dimnames=list(age=1:plus,year=YRS,sex=sex,np=c("n","p","w","wp")))  ## nos., props., weighted n or p by year
	for (j in YRS){
		jj = as.character(j)
		jyr=yrs[is.element(yrs,j)]
		if (length(jyr)==0) next
		jjj = names(jyr)
		jone=rep(1,length(jjj)); names(jone)=jjj; jpro=jone/length(jone)
		if (wSP[2]) jvec=Pvec[jjj]
		else        jvec=jpro   ## if no weighting use equal proportions
		for (k in sex) {
			kk=as.character(k)
			if (dim(pagetab)[2]==1) {
				agetab[1:plus,jj,kk,"n"] = pagetab[,,kk,"n"] # transfet n
				agetab[1:plus,jj,kk,"p"] = pagetab[,,kk,"p"] # transfer p
				agetab[1:plus,jj,kk,"w"] = pagetab[,,kk,"w"] # transfer w
			}
			else {
				ntab=pagetab[,,kk,"n"]; ptab=pagetab[,,kk,"p"]; wtab=pagetab[,,kk,"w"]
				kpro=wtab[,jjj,drop=FALSE]%*%jvec  ## for each sex, weight each level n/p vector and sum
				agetab[1:plus,jj,kk,"n"] = ntab[1:plus,jjj,drop=FALSE] %*% jone ## row sum
				agetab[1:plus,jj,kk,"p"] = ptab[1:plus,jjj,drop=FALSE] %*% jpro ## row mean
				agetab[1:plus,jj,kk,"w"] = kpro[1:plus,1]
			}
		}
	}
	asums=apply(agetab,c(2,4),sum)  ## calculate sum by 'year' and 'np'
	if (wN) {
		inflate=asums[,"n"]/asums[,"w"]
		inflate[is.na(inflate)]=1
		for (k in sex) {
			kk=as.character(k)
			if (length(YRS)==1)
				agetab[ii,jj,kk,"w"] = agetab[ii,jj,kk,"w"] * inflate
			else
				agetab[ii,names(inflate),kk,"w"] = t(apply(agetab[ii,names(inflate),kk,"w"],1,function(x){x*inflate}))
		}
	}
	aN=apply(agetab,c(2,4),sum)[,"w"]  ## calculate sum by 'year' and 'np', then grab 'w'
	aN[is.element(aN,0)]=1
	for (k in sex) {
		kk=as.character(k)
		if (length(YRS)==1)
			agetab[ii,jj,kk,"wp"] = agetab[ii,jj,kk,"w"] / aN
		else
			agetab[ii,names(aN),kk,"wp"] = t(apply(agetab[ii,names(aN),kk,"w"],1,function(x){x/aN}))
	}

	## Summary stats per year
	atid = sapply(split(ages$TID,ages$year),
		function(x){as.character(sort(unique(x)))},simplify=FALSE)      ## trips in each year
	Atid = sapply(atid,length)                                         ## no. trips in each year
	asid = sapply(split(ages$SID,ages$year),
		function(x){as.character(sort(unique(x)))},simplify=FALSE)      ## samples in each year
	Asid = sapply(asid,length)                                         ## no. samples in each year
	aeid = sapply(split(ages$EID,ages$year),
		function(x){as.character(sort(unique(x)))},simplify=FALSE)      ## specimens in each year
	Aeid = sapply(aeid,length)                                         ## no. specimens each year

	## Summary stats per level (e.g., quarter/stratum)
	ntid = sapply(split(ages$TID,ages$slev),
		function(x){as.character(sort(unique(x)))},simplify=FALSE)      ## trips in each level
	Ntid = sapply(ntid,length)                                         ## no. trips in each year
	nsid = sapply(split(ages$SID,ages$slev),
		function(x){as.character(sort(unique(x)))},simplify=FALSE)      ## samples in each level
	Nsid = sapply(nsid,length)                                         ## no. samples in each year
	neid = sapply(split(ages$EID,ages$slev),
		function(x){as.character(sort(unique(x)))},simplify=FALSE)      ## specimens in each level
	Neid = sapply(neid,length)                                         ## no. specimens in each year
	Scat = pcat                                                        ## sample unit (trip|sample) catch|density
	Fcat = if (sweight=="density") AREA else PCAT                      ## fishery commercial catch (complete set) or stratum area
#browser();return()

	stats= c("Ntid","Nsid","Neid","Scat","Fcat","Psamp")               ## summary table stats

	acyr = range(c(attributes(slev)$Ylim)) #,attributes(clev)$Ylim))   ## range of years in age data & catch data
	acy  = attributes(slev)$years                                      ## year vector for summary tab
	if (ctype=="S") {
		levs = pad0(sort(unique(ages$GC)),3)
		lpy  = length(levs) }
	else {
		lpy  = attributes(slev)$Nper                                    ## no. levels per year
		levs = 1:lpy }
	sumtab=array(0,dim=c(length(acy),lpy,length(stats)),
		dimnames=list(year=acy, lev=levs, stat=stats))                  ## summary table

	for (k in setdiff(stats,"Psamp")) {
		kdat = get(k)
		nxy = names(kdat); dot=regexpr("\\.",nxy)
		if (ctype=="S") {
			#x = substring(nxy,1,dot-1); y = substring(nxy,dot+1) }
			x = substring(nxy,1,4); y = revStr(substring(revStr(nxy),1,3)) }
		else {
			x = as.character(floor(as.numeric(nxy)-.001))
			y = as.character((as.numeric(nxy)-as.numeric(x))*lpy) }
		for (j in levs) {
			z=is.element(y,j)
			ii=x[z]
			idat=kdat[z]; names(idat)=ii
			for (i in unique(ii)) {
				if (!i%in%acy) next
				sumtab[i,j,k] = func(idat[is.element(names(idat),i)]) ## necessary when more than one survey per year (func defined at beginning of weightBio)
			}
		}
	}
	## Populate missing AREA info for density 
	if (sweight=="density" && dim(sumtab)[1]>1){
		Fcat.tmp = sumtab[,,"Fcat"]
		if (is.null(dim(Fcat.tmp)))
			Fcat.tmp = matrix(Fcat.tmp,ncol=1,dimnames=dimnames(sumtab)[1:2])
		Fcat.use = apply(Fcat.tmp,2,function(x){mean(x[x>0])})
		for (i in 1:dim(sumtab)[1])
			Fcat.tmp[i,] = Fcat.use
		sumtab[,,"Fcat"] = Fcat.tmp
	}
	## Proportion of trip:fishery catch (commercial) or expanded catch (sample density times stratum area) ***** review
	Psamp = if (sweight=="density") sumtab[,,"Scat"]*sumtab[,,"Fcat"] else sumtab[,,"Scat"]/sumtab[,,"Fcat"]
	Psamp[is.nan(Psamp)]=0
	sumtab[,,"Psamp"]=Psamp

	##---output name---
	if (missing(outnam)) {
		wpanam = paste("output-wpa",strSpp,"(",substring(sweight,1,3),")",sep="")
		if (ctype=="S")
			wpanam = paste(c(wpanam,"-ssid(",paste0(SSID,collapse="+"),")"),collapse="")
		else
			wpanam = paste(c(wpanam,"-tt(",ttype,")"),collapse="")
		if (!is.null(gear)) wpanam=paste(c(wpanam,"-gear(",paste(gear,collapse=""),")"),collapse="")
		if (!is.null(major)) wpanam=paste(c(wpanam,"-major(",paste(major,collapse=""),")"),collapse="")
	} else {
		wpanam = paste0("output-",outnam)
		#save("sumtab",file=paste0(outnam,".rda"))
	}
	##-----------------

	##----------Start Tables----------
	if (tabs){
		createTdir(".")
		sumsum = sub("output","sumtab",wpanam)
		sumcsv = paste("./tables/", sumsum,".csv",sep="")
		sumrda = paste("./tables/", sumsum,".rda",sep="")
		clearFiles(c(sumcsv,sumrda))
		save("sumtab",file=sumrda)
		cat("Supplementary Table for Weighted Proportions-at-Age","\n\n",file=sumcsv)
		for (k in stats) {
			cat(k,"\n",file=sumcsv,append=TRUE)
			cat("year,",paste(colnames(sumtab),collapse=","),"\n",sep="",file=sumcsv,append=TRUE)
			## very obscure but use for now:
			## (https://stackoverflow.com/questions/33206002/subset-an-r-array-that-may-have-a-dimension-of-length-1?noredirect=1&lq=1)
			ktab = `dim<-`(sumtab[,,k], dim(sumtab)[1:2])
			dimnames(ktab) = dimnames(sumtab)[1:2]
			apply(cbind(year=rownames(sumtab),ktab),1,function(x){
			#apply(cbind(year=rownames(sumtab),sumtab[,,k]),1,function(x){  ## dimension collapses only one year or stratum
				cat(paste(x,collapse=","),"\n",sep="",file=sumcsv,append=TRUE)})
			cat("\n",file=sumcsv,append=TRUE)
		}
		## Coleraine data file: weighted proportions-at-age (wpa) to CSV
		yy=dimnames(agetab)[[2]]; y=as.numeric(yy)
		sexy =  sapply(sex,function(s){switch(as.character(s),'0'="u",'1'="m",'2'="f",'3'="nd",'12'="mf")})
		wpatab=array(0,dim=c(length(y),6+nsex*plus), dimnames=list(year=yy,cols=c("year","ntid","nsid","neid","age1","ageN",
			paste0(rep(sexy,each=plus),rep(pad0(1:plus,2),nsex)) )))
		wpatab[yy,"year"]=y
		wpatab[names(Atid),"ntid"]=Atid
		wpatab[names(Asid),"nsid"]=Asid
		wpatab[names(Aeid),"neid"]=Aeid

		wpatab[yy,"age1"]=rep(1,length(y))
		wpatab[yy,"ageN"]=rep(plus,length(y))

		for (k in 1:nsex) {
			kk  = sex[k]
			kkk = as.character(kk)
			jj=paste( switch(kkk,'0'="u",'1'="m",'2'="f",'3'="nd",'12'="mf"), pad0(1:plus,2),sep="")
			wpatab[yy,jj] = t(agetab[1:plus,yy,kkk,zfld])
		}
		wsunit = paste0("n",tolower(sunit))        ## choose the right sample unit
		bsunit = setdiff(c("ntid","nsid"),wsunit)  ## identify the bad (unwanted) sample unit.
		wpatxt = wpatab[wpatab[,wsunit]>0 & !is.na(wpatab[,wsunit]),grep(bsunit,dimnames(wpatab)[[2]],invert=TRUE),drop=FALSE]
		if (nrow(wpatxt)==0) showError("No records with # SIDs > 0")
		sernam = if (ctype=="S") paste0(SSID,collapse=".") else if (!is.null(FID)) paste0(FID,collapse="") else 1
		wpatxt = cbind(series=rep(sernam,nrow(wpatxt)),wpatxt)
		wpacsv = sub("output","awatea",wpanam)
		wpacsv = paste0("./tables/",wpacsv,".csv")
		agerda = sub("output","agetab",wpanam)
		agerda = paste0("./tables/",agerda,".rda")
#browser();return()
		clearFiles(c(wpacsv,agerda))

		write.table(wpatxt, file=wpacsv, sep=",", na="", row.names=FALSE, col.names=TRUE)
	
		attr(agetab,"wpatab") = wpatab
		attr(agetab,"wpatxt") = wpatxt
		attr(agetab,"sumtab") = sumtab
		save("agetab",file=agerda)
		.flush.cat(rep("=",nchar(agerda)+27),"\n",agerda, " contains all prop-age data\n",rep("=",nchar(agerda)+27),"\n",sep="")
		packList(c("acat","pvec","Acat","Pvec","pagetab","sagetab","agetab","sumtab","wpatab","wpatxt"),"PBStool",tenv=.PBStoolEnv)

		## ADMB data file: weighted proportions-at-age to DAT
		sidyrs = wpatab[,"year"]
		sidtab = agetab[,,,zfld,drop=FALSE]
		sidtab=array(sidtab,dim=dim(sidtab)[1:3],dimnames=dimnames(sidtab)[1:3]) ## needed when only 1 year is available
		sidtab=sidtab[,as.character(sidyrs),,drop=FALSE]
		admdat = sub("output","admb",wpanam)
		admdat = paste0("./tables/", admdat, ".dat")
		mess=c("# Proportions-at-age data\n\n",
			"# number of years with ageing data 'NYearAge'\n",length(sidyrs),"\n",
			"# start year\n",min(sidyrs),"\n# end year\n",max(sidyrs),"\n",
			"# number of age classes 'NAge'\n", plus-1+1,"\n",
			"# first age\n",1,"\n# final age\n",plus,"\n\n")
		cat(paste(mess,collapse=""),file=admdat)
		mess=c("# age proportions of males as a matrix data_age_males.\n",
			"#  First row lists the years, then the next 60 rows give the proportions.\n",
			"#  Element [a][t] gives the proportion that are age a-1 in year start_year+t-1.\n",
			"#  Therefore, the matrix has dimensions (final age - first age + 2) * (end year - start year + 1).\n")
		cat(paste(mess,collapse=""),file=admdat,append=TRUE)
		cat( paste(paste(sidyrs,"      ",sep=""),collapse=""),"\n",file=admdat,append=TRUE)
		if (length(sidyrs)==1)
			mess=paste(show0(format(round(sidtab[,1,"1"],6),scientific=FALSE),6,add2int=TRUE),collapse="  ")
		else
			mess=apply(sidtab[,,"1"],1,function(x){paste(show0(format(round(x,6),scientific=FALSE),6,add2int=TRUE),collapse="  ")})
		cat(paste(mess,collapse="\n"),"\n\n",file=admdat,append=TRUE)
		mess=c("# age proportions of females as a matrix data_age_females.\n",
			"#  Same format as for males.\n")
		cat(paste(mess,collapse=""),file=admdat,append=TRUE)
		cat( paste(paste(sidyrs,"      ",sep=""),collapse=""),"\n",file=admdat,append=TRUE)
		if (length(sidyrs)==1)
			mess=paste(show0(format(round(sidtab[,1,"2"],6),scientific=FALSE),6,add2int=TRUE),collapse="  ")
		else
			mess=apply(sidtab[,,"2"],1,function(x){paste(show0(format(round(x,6),scientific=FALSE),6,add2int=TRUE),collapse="  ")})
		cat(paste(mess,collapse="\n"),"\n\n",file=admdat,append=TRUE)
		mess=c("# number of trips (ntid) and number of samples (nsid) for each year\n",
			"#  (year, ntid, nsid)\n")
		cat(paste(mess,collapse=""),file=admdat,append=TRUE)
		mess=apply(format(wpatab[,c("year","ntid","nsid"),drop=FALSE]),1,paste,collapse="")
		cat(paste(mess,collapse="\n"),"\n\n",file=admdat,append=TRUE)
	}
	##----------End Tables----------

	## Test for bubbles of same size
	#z=agetab[,,"1","wp"]==agetab[,,"2","wp"]
	#agetab[,,"1","wp"][z] = -agetab[,,"1","wp"][z]
	#agetab[,,"2","wp"][z] = -agetab[,,"2","wp"][z]

	makeCmat =function(x,colname="Y") {
		matrix(x,ncol=1,dimnames=list(names(x),colname)) }

	##----------Start Plot----------
	if (plot) { ## eventually make an independent function `plot<Something>` and call here
	## Plot weighted proportions-at-age
		if (win|eps|pdf|png|wmf) {
			## Create a subdirectories `english' and `french' for figures
			createFdir(lang)
		}
		if (missing(outnam)) {
			plotname = sub("output",paste("age",ptype,sep=""),wpanam)
			plotname = paste(c(plotname,"-sex(",sex,")"),collapse="")
		} else plotname=outnam

		plt.noto = crossTab(ages,c("year","sex"),"SPID",function(x){length(.su(x))})
		plt.nsam = crossTab(ages,c("year","sex"),sunit,function(x){length(.su(x))})

		display = agetab
		reject  = apply(display[,,,"n",drop=FALSE],2:3,sum) < Nmin ## do not display these years

		for (k in dimnames(display)$sex)
			display[,reject[,k],k,]=0
		xuse = names(clipVector(apply(reject,1,all),TRUE))
		display = display[,xuse,,,drop=FALSE]

		ffx = list(...)$fx
		ffx = if(is.null(ffx))  ffx = c(0.01,0.01)
			else                 ffx = rep(ffx,length(ffx))[1:length(ffx)]
		ffy = list(...)$fy
		ffy = if(is.null(ffy))  ffy = c(0.075,0.01)
			else                 ffy = rep(ffy,length(ffy))[1:length(ffy)]

		xlim = list(...)$xlim
		if (is.null(xlim)) 
			xlim = range(as.numeric(xuse))
		xsho = xlim[1]:rev(xlim)[1]
		xlimx = extendrange(xlim, f=ffx)
		if (length(xuse)==1) xlim = xlim + c(-.5,.5)
		ylimx = extendrange(c(0,plus), f=ffy)
#print(c(xlim,xlimx));cat("\n")
#browser(); return()

		if (is.null(Amin)) 
			Amin = as.numeric(names(clipVector(apply(display[,,,"n"],1,sum),0,1))[1])
		bigBub = max(apply(display[!is.element(rownames(display),plus),,,zfld,drop=FALSE],3,max)) ## smallest maximum bubble across sexes (???)
		#bigBub = max(apply(display[,,,zfld,drop=FALSE],3,max))   # largest maximum bubble across sexes (including plus class)

		LAYOUT = 1:3; names(LAYOUT)=c("portrait","landscape","juxstapose")
		nlay   = LAYOUT[layout]
		rows=switch(nlay,nsex,1,1)
		cols=switch(nlay,1,nsex,1)
		shortside=switch(nlay,min(1+1*length(xsho)*0.5,0.80*longside), 0.80*longside, 0.80*longside)
		longside=switch(nlay, longside, min(2+2*length(xsho)*0.5,longside), longside)

		devs=c(win=win,eps=eps,pdf=pdf,png=png,wmf=wmf); unpackList(devs)
		fout.e = plotname
		for (l in lang) {  ## could switch to other languages if available in 'linguaFranca'.
			changeLangOpts(L=l)
			#fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
			fout = switch(l, 'e' = paste0("./english/",fout.e), 'f' = paste0("./french/",fout.e) )
			for (d in 1:length(devs)) {
				dev = devs[d]; devnam=names(dev)
				if (!dev) next
				if (devnam=="eps")  ## ps2pdf orientation is largely controlled by Ghostscript (see GS_OPTIONS env var)
					postscript(paste(fout,".eps",sep=""), width=switch(nlay,shortside,longside,longside), height=switch(nlay,longside,shortside,longside), horizontal=FALSE, paper="special")
				else if (devnam=="pdf") ## PDF file for convenience
					pdf(paste(fout,".pdf",sep=""), width=switch(nlay,shortside,longside,longside), height=switch(nlay,longside,shortside,longside), paper="special")
				else if (devnam=="png") {
					clearFiles(paste0(fout,".png"))
					ppi=100; pnt=ppi*10/72; zoom=ppi/72
					width  = ifelse(layout=="landscape",longside,shortside)
					height = ifelse(layout=="landscape",shortside,longside)
					png(filename=paste0(fout,".png"), width=width, height=height, units="in", res=outres) #, pointsize=ifelse(outres>100,14,12))
				} else if (devnam=="wmf")
					do.call("win.metafile",list(filename=paste0(fout,".wmf"), width=switch(nlay,shortside,longside,longside), height=switch(nlay,longside,shortside,longside)))
				else resetGraph()
				expandGraph(mfrow=c(rows,cols), mar=c(2.5,2.75,0.25,0.5), mgp=c(1.5,0.25,0), oma=c(0,0,0,0))
				##clrs=rep(clrs,length(sex))[1:length(sex)]

				for (k in 1:nsex) {
					kk  = sex[k]
					kkk = as.character(kk)
					sexBub = max(display[,,kkk,zfld,drop=FALSE])     ## largest bubble for sex k
					zval = display[,,kkk,zfld]
					if (length(xuse)==1)
						zval=makeCmat(zval,xuse)
					zval = as.data.frame(zval)
					plot(0,0, type="n", axes=FALSE, xlim=xlimx, ylim=ylimx, xlab="", ylab="")
					#plot(0,0, type="n", axes=FALSE, xaxs="i", yaxs="i", xlim=xlimx, ylim=ylimx, xlab="", ylab="") ## (RH 230201)
#abline(h=ylimx, v=xlimx,lwd=2)
					if (!is.null(regimes)) {
						x1 = par()$usr[1]; x2 = par()$usr[2]
						if (length(xuse)>1) {
							for (i in 1:length(regimes)) {
								x = regimes[[i]]; a1=-min(x); a2=-max(x)
								y1lo=a1+x1; y1hi=a2+x1; y2lo=a1+x2; y2hi=a2+x2
								xreg=c(x1,x1,x2,x2); yreg=c(y1lo,y1hi,y2hi,y2lo)
#print(xreg)  ## ***** xregs change by sex! need to fix
								## polygon(xreg,yreg,border=FALSE, col=lucent("orange",0.2)) #"orange" #"floralwhite") #col="grey92")
								#if (is.null(clrs$r)) clrs[["r"]] = lucent("orange",0.2) 
								## peach=#FFE5B4, saffron=#F4C430, melon=#FDBCB4, apricot=#FBCEB1, banana=#FCF4A3
								#polygon(xreg, yreg, border=FALSE, col="#FCF4A3") #lucent("blue",0.5)) #"moccasin") #clrs[["r"]])  ##lucent doesn't work in polygons originating on x-axis !?!
								polygon(xreg, yreg, border=FALSE, col=darkenRGB("#C6D1D4",-0.25) ) ## new colour for SGR 2025 -- light silver grey
							}
						}
					}
					polygon(x=par()$usr[c(1,1,2,2)], y=c(par()$usr[3],0,0,par()$usr[3]), col="ivory", border="ivory")
					abline(h=0, lty=2, col="grey")
#abline(v=c(2006,xlim))
#abline(h=0,v=2014.542); abline(h=0,v=2017.792)
#if(k==2) {browser();return()}
					par(new=TRUE)  ## this allows plotBubbles to add to null plot

					if (ptype=="bubb") {
						inch = size * (sexBub/bigBub)^powr     # standardise across sexes
						bcol = ifelse(kk==0, 1, clrs[[k]][1])
						dots = list(...)
						mots = dots[setdiff(names(dots),c("f"))]  ## (RH 211201)
						dots =  mots[!sapply(mots,function(x){any(is.null(x))})]  ## get rid of NULL elements
						if (!is.null(dots$fill) && dots$fill)
							dots$bg = lucent(bcol,0.3) #0.2)  ## (RH 250604) try darker fill to improve the look
						#dots.formal = dots[intersect(names(dots), names(formals(fun=plot.default)))]
						dots.formal = dots[setdiff(names(dots), c("cex.noto","fx","fy","xlim","ylim"))]
						unpackList(dots.formal)
						dotty =  paste0(paste0(names(dots.formal),"=",names(dots.formal)),collapse=", ")
						mess = paste0("plotBubbles(z=zval, dnam=TRUE, hide0=TRUE, size=inch, xlim=xlimx, ylim=ylimx, frange=c(0.0,0), clrs=bcol, las=3, ylab=\"\", tcl=.25, ", dotty, ")")  ## frange needs to be c(0,0) to align with previous plot command (RH 250414)
						eval(parse(text=mess))
#browser();return()
#abline(h=0,col=lucent("red",0.75))
#browser();return()
#if(k==2) {browser();return()}
					}
					if (ptype=="bars") {
						zmax = zval[-plus,] ## exclude the plus class
						if (length(xuse)==1) zmax=makeCmat(zmax,xuse)
						imax = apply(zmax,2,function(x){x >= quantile(x,0.95) })
						zmax[!imax] = 0
						makePoly = function(amat,rel=FALSE) {
							amat[is.element(amat,0)] = NA
							xval = as.numeric(dimnames(amat)[[2]])
							xdif = ifelse(length(xval)==1,1,diff(xval)[1])
							yval = as.numeric(dimnames(amat)[[1]])
							ydif = ifelse(length(yval)==1,1,diff(yval)[1])
							if (rel) xoff = amat = (amat/bigBub) * (size*par()$fin[1]/2)
							else     xoff = (amat/bigBub) * (size/2)
							xL   = t(apply(xoff,1,function(x){xval-x})); xLvec = as.vector(xL)
							xR   = t(apply(xoff,1,function(x){xval+x})); xRvec = as.vector(xR)
							xpol = as.vector(rbind(xLvec,xLvec,xRvec,xRvec,rep(NA,length(xLvec))))
							ybox = as.vector(sapply(yval,function(y,yoff){c(y-yoff,y+yoff,y+yoff,y-yoff,NA)},yoff=ydif/2))
							ypol = rep(ybox,length(xval))
							return(list(xpol=xpol,ypol=ypol)) }
						poly1 = makePoly(zval,rel=TRUE)
						poly2 = makePoly(zmax,rel=TRUE)
						## TODO: bar width should be nyr * .5/par()$fin[1] for consistency
						sclr = ifelse(kk==0,"black",clrs[[k]][1])
						aclr = ifelse(kk==0,"grey",clrs[[k]][2])
						#aclr = c(as.vector(col2rgb(sclr)/255),0.5) # transparent alpha colour
						#aclr = rgb(aclr[1],aclr[2],aclr[3],alpha=aclr[4])
						plot(0,0, type="n", axes=FALSE, xlim=xlimx, ylim=ylimx, xlab="", ylab="")
						xpos=intersect(min(xuse):max(xuse),round(pretty(xlim,n=10),5))
						axis(1, at=xpos, las=3, cex.axis=ifelse(is.null(list(...)$cex.axis),0.8,list(...)$cex.axis), tcl=0.25)
						ypos=intersect(0:plus,round(pretty(ylim,n=10),5))
						axis(2, at=ypos, las=1, cex.axis=ifelse(is.null(list(...)$cex.axis),0.8,list(...)$cex.axis), tcl=0.25)
						polygon(poly1$xpol,poly1$ypol,border="gainsboro",col=aclr)
						polygon(poly2$xpol,poly2$ypol,border="gainsboro",col=sclr)
					}
					if (!is.null(cohorts)) {
						for (i in 1:length(cohorts$x)) {
							a = cohorts$y[i]-cohorts$x[i]
							abline(a=a, b=1, col=.colBlind["orange"]) 
							xcoho = plus-1-a; ycoho = plus-1
							if (xcoho>xlim[1] & xcoho<xlim[2] & ycoho>ylim[1] & ycoho<ylim[2])
								text(plus+1-a,plus+1,-a,cex=ifelse(png,0.7,0.8),col=.colBlind["orange"],font=2) #,adj=c(0.6,1.5))
							else
								text(YRS[length(YRS)]+0.5,a+YRS[length(YRS)]+0.5,-a,cex=ifelse(png,0.7,0.8),col=.colBlind["orange"],font=2) #,adj=c(0.5,1.4)) 
						}
					}
					noto = plt.noto[,kkk]; names(noto) = dimnames(plt.noto)$year #plt.noto[,"year"]  ## because of old crappy hadley crosstab
					noto = noto[noto>0 & !is.na(noto)]
					nsam = plt.nsam[,kkk]; names(nsam) = dimnames(plt.nsam)$year #plt.nsam[,"year"]  ## because of old crappy hadley crosstab
					nsam = nsam[nsam>0 & !is.na(nsam)]

					nsep = test=eval(parse(text=deparse("\u{00BB}")))  ## compressed right arrow
					nsep = test=eval(parse(text=deparse("\u{2022}")))  ## bullet
					#labs = paste0(nsam,"\225",noto)
					#labs = as.expression( sapply(1:length(nsam), function(i) { bquote(textstyle(.(nsam[i])%->%.(noto[i]))) }) )
					#labs = as.expression( sapply(1:length(nsam), function(i) { bquote(textstyle(.(nsam[i])%~%.(noto[i]))) }) )
					labs = as.expression( sapply(1:length(nsam), function(i) { bquote(.(nsam[i])~.(nsep)~.(noto[i])) }) )
					text(as.numeric(names(noto)), par()$usr[3]+0.015*abs(diff(par()$usr[3:4])), labels=labs, font=1, adj=0, srt=90,
					#text(as.numeric(names(noto)), par()$usr[3]+0.02*abs(diff(par()$usr[3:4])), labels=bquote(.(nsam)%->%.(noto)), font=1, adj=0, srt=90,
						cex=ifelse(is.null(list(...)$cex.noto),0.9,list(...)$cex.noto), col="grey15") #clrs[[k]][1])
#browser();return()
					box(lwd=ifelse(devnam=="png",max(1,floor(zoom/2)),1))
					mtext(linguaFranca(paste("Age (",ifelse(kk==1,"Males",ifelse(kk==2,"Females",ifelse(kk==12,"Males + Females","Unknown"))),")"),l), side=2, line=1.5, las=3, cex=ifelse(is.null(list(...)$cex.lab), 1, list(...)$cex.lab))
					#addLabel(0.05,0.05,txt=switch(k,"M","F","U"),cex=1.2,col=clrs[k],font=2)
					par(new=FALSE)
				}
				if (devnam!="win") dev.off()
			} ## end d (devs) loop
		}; eop()
		packList(c("Amin","bigBub","reject", "display","plotname"), "PBStool",tenv=.PBStoolEnv)
	}
	##----------End Plot----------

	invisible(agetab)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~weightBio


