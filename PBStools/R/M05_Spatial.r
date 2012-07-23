#===============================================================================
# Module 5: Spatial
# -----------------
#  calcHabitat.....Calculate potential habitat using bathymetry.
#  calcOccur.......Calculate percent occurrence of events in PolySet.
#  calcSRFA........Determine SRF areas using major, minor, and locality areas.
#  calcSurficial...Calculate intersection of surficial geology and bathymetry interval.
#  clarify.........Analyse catch proportions in blocks, then cluster into fisheries groups.
#  findHoles.......Find holes and place them under correct parents.
#  plotGMA.........Plot the Groundfish Management Areas.
#  preferDepth.....Histogram showing depth-of-capture.
#  zapHoles........Attempts to remove holes overwritten by solids.
#===============================================================================

#calcHabitat----------------------------2011-04-21
# Calculate potential habitat using bathymetry.
#-----------------------------------------------RH
calcHabitat <- function(topofile="bctopo", isob=c(150,435),
     digits=1, minVerts=10, col.hab="greenyellow", col.land="moccasin",
     plot=TRUE, pin=c(7,8), eps=FALSE, pix=FALSE, wmf=FALSE) {

	assign("PBSfish",list(module="M05_Spatial",func="calcHabitat"),envir=.GlobalEnv)
	icol = rgb(t(round(col2rgb(col.hab)*.65)),maxColorValue=255) # darker than col.hab
	expr=paste("getFile(",topofile,",use.pkg=TRUE,try.all.frames=TRUE,scope=\"G\"); ",
		"bathy=makeTopography(",topofile,",digits=",digits,")",sep="")
	eval(parse(text=expr))
	bCL <- contourLines(bathy,levels=isob)
	bCP <- convCP(bCL,projection="LL",zone=9)
	bPoly <- bCP$PolySet
	rng = apply(bPoly,2,range); xlim=rng[,"X"]; ylim=rng[,"Y"]

	box=as.PolySet(data.frame(PID=rep(3,4),POS=1:4,X=xlim[c(1:2,2:1)],Y=ylim[c(1,1,2,2)]),projection="LL",zone=9)
	polyOut = closePolys(fixBound(bPoly,.00001))
	polyOut = joinPolys(polyOut,operation="UNION")
	polyOut = findHoles(polyOut,minVerts=minVerts)
	habitat = joinPolys(box,polyOut,operation="DIFF")

#	polyA=bPoly[is.element(bPoly$PID,1),]; polyA=closePolys(fixBound(polyA,.00001))
#	polyB=bPoly[is.element(bPoly$PID,2),]; polyB=closePolys(fixBound(polyB,.00001))
#	maskB=joinPolys(box,polyB,"DIFF") # deeper poly first
#	habitat=joinPolys(maskB,polyA,"DIFF") # subtract the shallower poly

	warn <- options()$warn; options(warn = -1)
	area=calcArea(habitat); area=sum(area$area,na.rm=TRUE)
	attr(habitat,"area")=area
	options(warn = warn)

	stuff=c("bathy","bCL","bCP","bPoly","polyOut","habitat"); packList(stuff,"PBSfish")

	if (plot) {
		fout=paste("Habitat-",isob[1],"m-",isob[2],"m",sep="")
		devs = c(eps=eps,pix=pix,wmf=wmf,win=TRUE)
		for (i in names(devs)[devs]) {
			if (i=="eps")      postscript(file=paste(fout,".eps",sep=""),width=pin[1],height=pin[2],fonts="mono") 
			else if (i=="pix") png(paste(fout,".png",sep =""), width=round(100*pin[1]), height=round(100*pin[2])) 
			else if (i=="wmf") win.metafile(paste(fout,".wmf",sep=""),width=pin[1],height=pin[2])
			else          resetGraph()
			expandGraph(mar=c(3,3,.5,.5),mgp=c(2.5,.5,0),las=1)
			plotMap(box,type="n",plt=NULL,cex.axis=1.2,cex.lab=1.5)
			addPolys(habitat,col=col.hab)
			#if (eps|pix|wmf) addPolys(habitat,col=col.hab,colHoles="white")
			#else addPolys(habitat,col=col.hab)
			addLines(habitat,col=icol)
			data(nepacLL); addPolys(nepacLL,col=col.land)
			.addAxis(xlim=xlim,ylim=ylim,tckLab=FALSE,tck=0.014,tckMinor=.007)
			box()
			if (i %in% c("eps","pix","wmf")) dev.off()
		}
	}
	invisible(habitat) }
#--------------------------------------calcHabitat

#calcOccur------------------------------2009-07-30
# Calculate percent occurrence of events in PolySet
#-----------------------------------------------RH
calcOccur = function(polyset="qcb", events, wt=1, mess=FALSE) {
	assign("PBSfish",list(module="M05_Spatial",func="calcOccur"),envir=.GlobalEnv)
	polyset=as.character(substitute(polyset)) #string name of PolySet object
	events=as.character(substitute(events))   #string name of events bject
	if (polyset=="polyset" || events=="events")
		showError("Do not name objects polyset='polyset' or events='events'")
	expr=paste("getFile(",polyset,",use.pkg=TRUE,try.all.frames=TRUE); polyset=",polyset,sep="")
	eval(parse(text=expr))
	expr=paste("getFile(",events,",use.pkg=TRUE,try.all.frames=TRUE); events=",events,sep="")
	eval(parse(text=expr))
	if (is.character(wt) && !is.element(wt,names(events)))
		showError("Specified 'wt' is not a field in 'events'")
	pd=attributes(polyset)$PolyData
	pd$spp=rep(0,nrow(pd))

	loc=findPolys(events,polyset)
	if (wt==1) loc$wt=rep(1,nrow(loc))
	else {
		wtval=events[,wt]; names(wtval)=events$EID
		loc$wt = wtval[as.character(loc$EID)] }
	eidpid=sapply(split(loc$wt,loc$PID),sum,na.rm=TRUE)
	pd$spp=eidpid[as.character(pd$PID)]
	occur=sapply(split(pd$spp,pd$label),sum,na.rm=TRUE)
	occur=rev(sort(occur))
	poccur=occur[!is.element(names(occur),c("Unknown","Land"))]
	poccur=100*poccur/sum(poccur)
	if (mess) {
		mess=paste(names(poccur),"  =  ",show0(round(poccur,1),1,add2int=TRUE),"%",sep="")
		mess=paste(mess,collapse="\n")
		mess=paste("Percent Occurrence:\n\n",mess,collapse="")
		showMessage(mess,as.is=TRUE,adj=1,x=.9,col="dodgerblue") }
	stuff=c("pd","loc","eidpid","occur","poccur"); packList(stuff,"PBSfish")
	return(poccur) }
#----------------------------------------calcOccur

#calcSRFA-------------------------------2008-12-01
# Determine SRF assessment areas or subareas (gullies) based on 
# combinations of major and minor areas and localities.
#-----------------------------------------------RH
calcSRFA <- function(major, minor=NULL, loc=NULL, subarea=FALSE) { 
	getsrfa <- function(x) {
		i <- ifelse(is.na(x[1]),"",x[1])
		j <- ifelse(is.na(x[2]),"",x[2])
		k <- ifelse(is.na(x[3]),"",x[3])
		if (i==3 || j==25)         area <- "3C"
		else if (any(j==c(26,27))) area <- "3D"
		else if (i==5 || (j==8 && (is.na(k) || any(k==c(0:5,7:10,13)))))
                                 area <- "5AB"
		else if (any(i==c(7,8)) || (j==8 && any(k==c(6,11,12,14,15))))
                                 area <- "5CD"
		else if (any(j==c(31,34))) area <- "5ES"
		else if (j==35)            area <- "5EN"
		else if (i==1)             area <- "SG"
		else                       area <- ""
		return(area) }

	getsrfs <- function(x) {
		i <- ifelse(is.na(x[1]),"",x[1])
		j <- ifelse(is.na(x[2]),"",x[2])
		k <- ifelse(is.na(x[3]),"",x[3])
		if (i==5 || (j==8 && (is.na(k) || any(k==c(0:2,7:10,13)))))    area <- "GS"
		else if (j==8 && any(k==c(3:5)))                               area <- "MI"
		else if (any(i==c(7,8)) || (j==8 && any(k==c(6,11,12,14,15)))) area <- "MR"
		else                                                           area <- ""
		return(area) }

	if (!is.vector(major)) {
		if (ncol(major)==3) xin <- major
		else showError("Supply c(major,minor,locality) as\n3 vectors or 1 matrix with 3 columns") }
	else {
		if (!all(diff(c(length(major),length(minor),length(loc)))==0))
			showError("Need equal lengths for\nmajor, minor, loc")
		xin <- cbind(major,minor,loc) }
	if (subarea) fn <- getsrfs else fn <- getsrfa
	srfa <- apply(xin,1,fn)
	return(srfa) }
#-----------------------------------------calcSRFA

#calcSurficial--------------------------2009-07-31
# Calculate the intersection of surficial geology
# and bathymetry interval object from calcHabitat()
#-----------------------------------------------RH
calcSurficial <- function(surf="qcb", hab,
		xlim=c(-133.4,-127.8), ylim=c(50.5,54.8),
		col.hab=c("aliceblue","grey"), col.cst="grey85", 
		pix=FALSE, wmf=FALSE) {

	assign("PBSfish",list(module="M05_Spatial",func="calcSurficial"),envir=.GlobalEnv)
	surf=as.character(substitute(surf)) #string name of surficial object
	if (missing(hab))
		showError("Supply a habitat PolySet")
	else 
		hab=as.character(substitute(hab))   #string name of bathymetry object
	fout=paste(surf,hab,sep=".")
	expr=paste("getFile(",surf,",use.pkg=TRUE,try.all.frames=TRUE); surf=",surf,sep="")
	eval(parse(text=expr))
	expr=paste("getFile(",hab,",use.pkg=TRUE,try.all.frames=TRUE); hab=",hab,sep="")
	eval(parse(text=expr))
	pd=attributes(surf)$PolyData
	hab=clipPolys(hab,xlim=xlim,ylim=ylim)
	getFile(nepacLL,use.pkg=TRUE)
	coast=clipPolys(nepacLL,xlim=xlim,ylim=ylim)

	surfhab=joinPolys(surf,hab) # intersection of surficial and bathymetry
	warn <- options()$warn; options(warn = -1)
	areasum=calcArea(surfhab,rollup=1)
	options(warn = warn)
	areasum$label=rep(0,nrow(areasum))
	lab=pd$label; names(lab)=pd$PID
	areasum$label=lab[as.character(areasum$PID)]
	area=sapply(split(areasum$area,areasum$label),sum)
	area=rev(sort(area))
	write.csv(area,paste(fout,".csv",sep=""))

	z=is.element(pd$col,"white")
	leg=sapply(split(pd$label[!z],pd$col[!z]),unique)
	leg=leg[order(leg)]
	legarea=paste(leg," (",format(round(area[leg]),big.mark=",",trim=TRUE)," km\262)",sep="")

	stuff=c("surfhab","areasum","area","leg","legarea"); packList(stuff,"PBSfish")

	if(wmf) win.metafile(paste(fout,".wmf",sep=""),width=6.75,height=8.5)
	else if (pix) {
		pin <- par()$pin; PIN <- 7.5 * pin/max(pin)
		png(paste(fout,".png",sep =""), width=round(100*PIN[1]), height=round(100*PIN[2])) }
	else resetGraph()
	expandGraph(mar=c(3,ifelse(pix|wmf,3,4),.5,.5),mgp=c(2.5,.5,0),las=1)
	plotMap(coast,xlim=xlim,ylim=ylim,plt=NULL,cex.axis=1.2,cex.lab=1.5)
	if (pix|wmf) addPolys(hab,col=col.hab[1],border=FALSE,colHoles="white")
	else addPolys(hab,col=col.hab[1],border=FALSE)
	addPolys(surf,polyProps=pd,border=FALSE)
	addLines(hab,col=col.hab[2])
	addPolys(coast,col=col.cst)
	addLegend(.025,.025,fill=names(leg),legend=legarea,bty="n",yjust=0,cex=.9)
	.addAxis(xlim=xlim,ylim=ylim,tckLab=FALSE,tck=0.014,tckMinor=.007)
	box()
	if (pix|wmf) dev.off()
	invisible() }
#------------------------------------calcSurficial

#clarify--------------------------------2009-07-31
#  Analyse catch proportions in blocks, then cluster into fisheries groups.
#  Initially, done to address the CASSIS proposal and its impact (John Pringle).
#  CASSIS = CAScadia SeISmic experiment
#-----------------------------------------------RH
clarify <- function(dat, wmf=FALSE, cell=c(0.1,0.075), nG=8,
   xlim=c(-134.2,-127), ylim=c(49.6,54.8), zlim=NULL, targ="YMR", 
   clrs=c("red","orange","yellow","green","forestgreen","deepskyblue",
   "blue","midnightblue"), hpage=10) {

	tcomp <- function(x,i) {
		iU <- sort(unique(i))
		pvec <- rep(0,length(iU)); names(pvec) <- iU
		temp <- sapply(split(x,i),sum,na.rm=TRUE)
		pvec[names(temp)] <- temp
		return(pvec)
	}
	pcomp <- function(x) {
		return(x/sum(x,na.rm=TRUE)) }

	if (length(clrs)<nG) showError("Supply more colours 'clrs' or reduce number of groups 'nG'")
	parlist=list(mfrow=c(1,1),mar=c(2.5,2.5,.5,.5),oma=c(0,0,0,0),las=0,cex=1.4,mgp=c(1,.2,0))

	require(cluster)
	assign("PBSfish",list(module="M05_Spatial",func="clarify",plotname="Rplot"),envir=.GlobalEnv)
	fnam=as.character(substitute(dat))
	expr=paste("getFile(",fnam,",use.pkg=TRUE,try.all.frames=TRUE,scope=\"G\"); dat=",fnam,sep="") # input file made global cause it's big
	eval(parse(text=expr))
	#clrs <- c("blue","magenta","orange","green","cyan","forestgreen","yellow","black","purple4","red") #PNCIMA
	#clrs <- c("red","orange","yellow","green","forestgreen","deepskyblue","blue","midnightblue")
	flds=names(dat); spp=setdiff(flds,c("EID","X","Y","Z","tcat"))

	nr <- nrow(dat); nc <- ncol(dat[,spp])
	dat$tcat <- apply(dat[,spp],1,sum,na.rm=TRUE)
	dat <- dat[dat$tcat>0 & !is.na(dat$tcat),]
	dat=dat[dat$X>=xlim[1] & dat$X<=xlim[2] & !is.na(dat$X),]
	dat=dat[dat$Y>=ylim[1] & dat$Y<=ylim[2] & !is.na(dat$Y),]
	if (!is.null(zlim)) dat=dat[dat$Z>=zlim[1] & dat$Z<=zlim[2] & !is.na(dat$Z),]

	gx <- seq(xlim[1],xlim[2],cell[1]); gy <- seq(ylim[1],ylim[2],cell[length(cell)])
	agrid <- makeGrid(x=gx,y=gy,projection="LL",zone=9,byrow=TRUE,addSID=FALSE)

	events <- data.frame(EID=dat$EID,X=dat$X,Y=dat$Y,Z=dat$tcat)
	events <- na.omit(events)
	attr(events,"class") <- c("EventData","data.frame")
	attr(events,"projection") <- "LL"
	locData <- findCells(events,agrid)
	z <- !duplicated(locData$EID)
	locData <- locData[z,]
	dat <- dat[is.element(dat$EID,locData$EID),]
	dat$PID <- rep(NA,nrow(dat))
	dat$PID[match(dat$EID,locData$EID)] <- locData$PID

	#PID <- sort(unique(events$PID)); nPID <- length(PID)
	nspp <- length(spp)

	tcat <- apply(dat[,spp],2,tcomp,i=dat$PID)
	pcat <- t(apply(tcat,1,pcomp))

	ppos <- apply(pcat,2,function(x){length(x[x>0])/length(x)})
	z    <- ppos >= .10
	spp  <- names(ppos[z])
	psub <- pcat[,spp]

	ptree  <- clara(psub, k=nG)
	clus   <- ptree$clustering
	groups <- split(as.numeric(names(clus)),clus)

	pout = as.data.frame(psub); k = dimnames(pout)[[1]]
	pout$group = clus[k]
	grps = sort(unique(pout$group))
	pdata = array(NA,dim=c(length(k),5),
		dimnames=list(k,c("PID","col","label","group","target")))
	pdata = as.data.frame(pdata)
	smat <- NULL; skey <- NULL

	for (i in grps) {
		gout <- pout[is.element(pout$group,i),]
		sord <- rev(sort(apply(gout[,spp],2,mean,na.rm=TRUE)))
		tpos <- match(targ,names(sord),nomatch=99)+i/100; #print(tpos)
		smat <- c(smat,list(sord))
		j    <- dimnames(gout)[[1]]
		col  <- clrs[gout$group]
		lab  <- paste(names(sord[1:3]),collapse="-")
		skey <- c(skey,lab)
		pdata[j,"PID"]   <- as.numeric(j)
		pdata[j,"col"]   <- col
		pdata[j,"label"] <- rep(lab,length(j))
		pdata[j,"group"] <- rep(i,length(j))
		if (is.null(targ)) pdata[j,"target"] = pdata[j,"group"]
		else               pdata[j,"target"]  = rep(tpos,length(j))
	}
	names(smat) <- grps
	gnum = sapply(split(pdata$PID,pdata$group),length)
	gtar = sapply(split(pdata$PID,pdata$target),length)
	if (is.null(targ)) gord = rev(order(gnum))
	else               gord = order(as.numeric(names(gtar))) # order by placement of target in clara group
	names(clrs)[1:length(gtar)]=names(gtar[gord])            # enforce colour order
	pdata$col = clrs[as.character(pdata$target)]             # repopulate pdata's col field

	kcol = clrs[1:nG]
	glab = sapply(split(pdata$label,pdata$target),unique)
	kgrp = glab[gord]

	getFile(isobath,use.pkg=TRUE)
	isob <- isobath[is.element(isobath$PID,c(200,1000,1800)),] # & is.element(isobath$SID,1),]

	stuff=c("tcat","pcat","ptree","pout","pdata","clrs","glab","kgrp"); packList(stuff,"PBSfish")

	#--Plot-results---------------------------------------------
	# Try to automate based on par()$pid - plot must be ready on-screen first
	PIN = par()$pin/max(par()$pin)*hpage
	fn = paste(ifelse(is.null(targ),"ALL",targ),"-Clara-nG",nG,".wmf",sep="")
	if (wmf) win.metafile(fn,width=PIN[1],height=PIN[2]) # OTHER
	else resetGraph()
	getFile(nepacLL,use.pkg=TRUE)

	par(parlist)
	plotMap(nepacLL,xlim=xlim,ylim=ylim,plt=NULL,col="white")
	addPolys(agrid,polyProps=pdata,border=FALSE)
	addLines(isob,col=c("steelblue4","navy","black"))
	addPolys(nepacLL,col="lightgrey")
	xoff=ifelse(wmf,-.05,0); yoff=ifelse(wmf,-.01,0)
	addLegend(.01,.01,fill=kcol,legend=kgrp,bty="n",cex=ifelse(wmf,0.8,0.7),yjust=0)
	if (!is.null(targ)) 
		addLegend(.35,.01,legend=floor(as.numeric(names(kgrp))),bty="n",
			cex=ifelse(wmf,0.8,0.7),adj=1,xjust=1,yjust=0,text.col="blue",title=targ)
	.addAxis(xlim=xlim,ylim=ylim,tckLab=FALSE,tck=0.014,tckMinor=.007)
	box()
	if (wmf) dev.off()
	invisible() }
#------------------------------------------clarify

#findHoles------------------------------2011-04-21
# Find holes and place them under correct parents.
#-----------------------------------------------RH
findHoles = function(polyset, minVerts=10) {
	newpoly = list()
	pid = sort(unique(polyset$PID))
	for (i in pid) {
		ipoly = polyset[is.element(polyset$PID,i),]
		ipoly$PID = ipoly$SID
		ipoly$SID = rep(1, nrow(ipoly))
		ilst = split(1:nrow(ipoly),ipoly$PID)
		ilen = rev(sort(sapply(ilst,length)))
		ilen = ilen[ilen>=minVerts]
		olst = ilst[names(ilen)]
		ivec = unlist(olst,use.names=FALSE)
		opoly = ipoly[ivec,]
		ihole = rep(0,length(ilen)); names(ihole)=names(ilen)
		parents = NULL               # keep track of identified parents
		for (a in names(ihole)) {    # potential hole
#cat("\n\n",a,"\n")
			zsmall=is.element(opoly$PID,a)
			for (b in names(ihole)) { # potential parent
				if  (b==a || ihole[b]>0) next
#print(b)
				zbig=is.element(opoly$PID,b)
				is.in = sp::point.in.polygon(opoly$X[zsmall],opoly$Y[zsmall],opoly$X[zbig],opoly$Y[zbig]) # is a in b ?
				if (all(is.in>0)) {
					ihole[a] = as.numeric(b)          # reverse the role of a & b so that every potential hole a shows its parent b
					next }
			}
			if (ihole[a]==0) parents = c(parents,a) # potential hole is in fact a parent
#print (parents)
		}
		bigs = ihole[ihole==0]
		inew = list()
		for (j in names(bigs)) {
			jj = as.numeric(j)
			inew = rbind(inew,opoly[is.element(opoly$PID,jj),])
			holes = ihole[ihole==jj]
			if (length(holes)==0) next
			for (k in names(holes)) {
				kk = as.numeric(k)
				hole = opoly[is.element(opoly$PID,kk),]
				if (hole$POS[1]==1)
					hole$POS = rev(hole$POS)
				inew = rbind(inew,hole)
			}
		}
		inew$SID = inew$PID
		inew$PID = rep(i,nrow(inew))
		newpoly = rbind(newpoly,inew)
		attr(newpoly,paste("holes_in_",i,sep="")) = ihole
	}
	return(newpoly) }
#----------------------------------------findHoles

#plotGMA--------------------------------2011-05-24
# Plot the Groundfish Management Areas
#-----------------------------------------------RH
plotGMA = function(gma=gma.popymr, xlim=c(-134,-123), ylim=c(48.05,54.95), 
     dimfig=c(9,9), pix=FALSE, extra=NULL) {
	
	if (is.null(extra)) {
		extra = as.EventData(data.frame(
			EID = 1:4,
			X   = c(-132.3628,-130.0393,-129.1727,-126.3594),
			Y   = c(53.74724,51.48773,51.05771,50.15860),
			label = c("Haida\nGwaii","QCS","GIG","Vancouver\nIsland") ),
			projection="LL" )
	}
	fnam = as.character(substitute(gma))
	data(nepacLLhigh,major)
	edata = pdata = attributes(gma.popymr)$PolyData
	names(edata)[1]="EID"
	edata=as.EventData(edata,projection="LL")

	if (pix) png(filename=paste(fnam,"amend","png",sep="."),width=dimfig[1],height=dimfig[2],units="in",res=200)
	else resetGraph()
	plotMap(gma, xlim=xlim, ylim=ylim, polyProps=pdata,border="grey",
		plt=c(0.07,0.99,0.07,0.99), mgp=c(3,.5,0), las=1, cex.axis=1.2, cex.lab=1.5)
	addPolys(major,col="transparent",border="navyblue",lwd=2)
	addPolys(nepacLLhigh,col="white",border="grey")
	.addAxis(par()$usr[1:2],ylim=par()$usr[3:4],tckLab=FALSE,tck=.015,tckMinor=.015/2)
	addLabels(edata,cex=2.5,font=2)
	addLabels(extra,cex=1.8,col="blue")
	text(-125.2416,52.70739,"BC",cex=5,col="grey",font=2)
	box(lwd=2)
	if (pix) dev.off()
	invisible(list(pdata=pdata,extra=extra))
}
#------------------------------------------plotGMA

#preferDepth----------------------------2010-10-19
# Histogram showing depth-of-capture
#-----------------------------------------------RH
preferDepth = function(strSpp="410", fqtName="pht_fdep.sql", dbName="PacHarvest",
     spath=NULL, type="SQL", hnam=NULL, get.effort=TRUE) {

	if (!require(PBSmodelling, quietly=TRUE)) stop("PBSmodelling package is required")
	if (!require(PBStools, quietly=TRUE)) stop("PBStools package is required")
	warn <- options()$warn; options(warn=-1)
	assign("PBSfish",list(module="M05_Spatial",func="preferDepth",plotname="Rplot"),envir=.GlobalEnv)

	wpath <- .setWpath(win=FALSE)
	if (is.null(spath) || spath=="") spath <- .getSpath()
	rtmp <- tempdir(); rtmp <- gsub("\\\\","/",rtmp)
	wnam <- paste(wpath,"preferDepthWin.txt",sep="/")
	wtmp <- paste(rtmp,"preferDepthWin.txt",sep="/")
	snam <- paste(spath,fqtName,sep="/")
	temp <- readLines(wnam)
	temp <- gsub("@wdf",wtmp,temp)
	temp <- gsub("@sppcode",strSpp,temp)
	temp <- gsub("@fqtName",fqtName,temp)
	temp <- gsub("@dbName",dbName,temp)
	temp <- gsub("@path",spath,temp)
	temp <- gsub("@type",type,temp)
	if (!is.null(hnam) && is.character(hnam))
		temp <- gsub("#import=",paste("import=\"",hnam,"\"",sep=""),temp)
	writeLines(temp,con=wtmp)
	createWin(wtmp); options(warn=warn)
	if (get.effort) .preferDepth.getEffort()
	else {
		effort=NULL; setWinVal(list(showE=FALSE),winName="window") 
		packList("effort","PBSfish") }
	invisible() }

#.preferDepth.getEffort-----------------2010-10-19
.preferDepth.getEffort=function(strSpp="ALL") {
		resetGraph(); par(mar=c(0,0,0,0),oma=c(0,0,0,0))
		showMessage("Please wait while effort data is retrieved for this GUI session",col="blue",cex=1.2)
		getData("pht_effort.sql",strSpp=strSpp,path=.getSpath())
		effort=PBSdat; effort$effort=effort$effort/60 ### convert to hours
		eval(parse(text="PBSfish$effort <<- effort"))
		#packList("effort","PBSfish") ### too slow
		frame(); addLabel(.5,.5,"Effort loaded",col="darkgreen",cex=1.2) }

#.preferDepth.getDepth------------------2011-04-01
.preferDepth.getDepth <- function() { 
	getWinVal(scope="L",winName="window"); act <- getWinAct()[1]
	if (!is.null(act) && act=="getdata") getdata <- TRUE else getdata <- FALSE
	if (path==""){ path <- getwd(); setWinVal(list(path=path),winName="window") }
	spp  <- eval(parse(text=paste("c(\"",gsub(",","\",\"",strSpp),"\")",sep="")));
	year <- eval(parse(text=paste("c(",strYear,")",sep="")))
	gear <- eval(parse(text=paste("c(",strGear,")",sep="")))
	nAc  <- length(findPat(c(LETTERS,letters),strArea))

	if (strArea!="") {
		if (any(disA=="srfa") && nAc>0)
			area <- eval(parse(text=paste("c(\"",gsub(",","\",\"",strArea),"\")",sep="")))
		else if(!any(disA=="srfa") && nAc==0)
			area <- eval(parse(text=paste("c(",strArea,")",sep="")))
		else showError(paste("\"",strArea,"\" does not match \"",disA,"\"",sep="")) 
	} else area <- NULL
	if (any(spp=="") || length(spp)>1) showError("Choose 1 species")

	if (!exists("PBSdat",where=1) || is.null(attributes(PBSdat)$spp) || 
		spp!=attributes(PBSdat)$spp || fqtName!=attributes(PBSdat)$fqt || getdata) {
		expr=paste("getData(fqtName=\"",fqtName,"\",dbName=\"",dbName,"\",strSpp=\"",
			spp,"\",type=\"",type,"\",path=\"",path,"\",trusted=",trusted,
			",uid=\"",uid,"\",pwd=\"",pwd,"\")",sep="")
		eval(parse(text=expr))
		if (!is.null(attributes(PBSdat)$spp) && attributes(PBSdat)$spp!=spp) {
			spp <- attributes(PBSdat)$spp
			setWinVal(list(strSpp=spp),winName="window") } }
	assign("dat",PBSdat)
	if (nrow(dat)==0) showError(paste("Species =",spp),type="nodata")
	eff = PBSfish$effort
	isE = ifelse(is.null(eff),FALSE,TRUE)
	if (!isE) setWinVal(list(showE=FALSE),winName="window")

	if (!is.null(gear)) {
		if (isE) eff = eff[is.element(eff$gear,gear),]
		dat = dat[is.element(dat$gear,gear),]
		if (nrow(dat)==0) showError(paste("gear =",paste(gear,collapse=", ")),type="nodata") }
	if (!is.null(year)) {
		if (isE) eff = eff[is.element(eff$year,year),]
		dat <- dat[is.element(dat$year,year),]
		if (nrow(dat)==0) showError(paste("year =",paste(year,collapse=", ")),type="nodata") }
	if (disA=="all") {
		if (isE) eff$area = rep("CST",nrow(eff))
		dat$area <- rep("CST",nrow(dat)) }
	else {
		if (isE) eff$area <- eff[,disA]
		dat$area <- dat[,disA]
		if (!is.null(area)) {
			if (isE) eff <- eff[is.element(eff$area,area),]
			dat <- dat[is.element(dat$area,area),] } 
		else {
			if (isE) eff <- eff[!is.na(eff$area),]
			dat <- dat[!is.na(dat$area),] }
		if (nrow(dat)==0) showError(paste(disA,"=",paste(area,collapse=", ")),type="nodata") }
	if (is.null(year)) year <- 0 else year <- sort(unique(dat$year)); nyr <- length(year);
	if (is.null(area)) area <- 0 else area <- sort(unique(dat$area)); nar <- length(area);

	plotname = paste("dep",spp,ifelse(year==0,"",paste("-(",paste(year,collapse="."),")",sep="")),
		ifelse(area==0,"",paste("-(",paste(area,collapse=""),")",sep="")),
		ifelse(is.null(gear),"",paste("-gear",paste(gear,collapse=""),sep="")),sep="")
	packList("plotname","PBSfish")
	nrow <- max(nyr,nar); ncol <- min(nyr,nar);
	if (nyr>=nar) { ifac <- year; jfac <- area; yba <- TRUE}
	else { ifac <- area; jfac <- year; yba <- FALSE}
	if (pix) png(width=6.5,height=5*nrow^0.25,units="in",res=100,filename=paste(plotname,"png",sep="."))
	else if (wmf) win.metafile(width=6.5,height=5*nrow^0.25,filename=paste(plotname,"wmf",sep="."))
	else resetGraph()
	if (nrow*ncol==1)
		expandGraph(mfrow=c(nrow,ncol),mar=c(0,0,0,0),oma=c(4,5,1,1),cex=0.9,yaxs="i",mgp=c(2,.5,0),las=1)
	else
		expandGraph(mfrow=c(nrow,ncol),mar=c(0,0,0,0),oma=c(5,5,1,1),cex=0.7,yaxs="i",mgp=c(2,.5,0),las=1);
	xlim <- c(0,round(max(c(dat$depth,eff$depth),na.rm=TRUE),-1)) + c(-xwid,xwid); xdiff <- diff(xlim);
	ylim <- c(0,.19)
	brks=seq(xlim[1],xlim[2],xwid)
	stuff=c("xlim","ylim","brks","area"); packList(stuff,"PBSfish")
	
	for (i in ifac) {
		if (all(ifac!=0)) {
			if (isE) ieff = eff[is.element(eff[,ifelse(yba,"year","area")],i),]
			idat <- dat[is.element(dat[,ifelse(yba,"year","area")],i),] }
		else {
			idat <- dat; if (isE) ieff <- eff }
		for (j in jfac) {
			if (all(jfac!=0)) {
				jdat <- idat[is.element(idat[,ifelse(yba,"area","year")],j),]
				if (isE) jeff <- ieff[is.element(ieff[,ifelse(yba,"area","year")],j),] }
			else {
				jdat <- idat; if (isE) jeff <- ieff }
			ntows <- nrow(jdat)
			stuff=c("ifac","jfac","ntows") # (,"jdat","jeff") ### too big slow things down
			packList(stuff,"PBSfish")

			xy <- hist(jdat$depth, breaks=brks, plot=FALSE);
			xy$density <- xy$counts/sum(xy$counts)
			xyout <- paste("xy",spp,i,j,sep=".")
			eval(parse(text="PBSfish[[xyout]] <<- xy"))

			if (isE) {
				jeff$dbin=cut(jeff$depth,brks)
				effTot=sapply(split(jeff$effort,jeff$dbin),sum)
				effDen=effTot/sum(effTot)
				xye=list(breaks=brks,counts=effTot,density=effDen)
				attr(xye,"class")="histogram"
				packList("xye","PBSfish") }

			plot(xy,freq=FALSE, xlab="", ylab="", main="",cex.lab=1.2,
				col="white", xlim=XLIM, ylim=YLIM, axes=FALSE);
			if (par()$mfg[1]==par()$mfg[3]) axis(1,cex.axis=1.2)
			if (par()$mfg[2]==1) axis(2,cex.axis=1.2,tcl=.5) else axis(2,labels=FALSE,tcl=.5)

			if (isE && showE) { #---Total fishing effort density
				plot(xye,freq=FALSE, xlab="", ylab="", main="",cex.lab=1.2,
					col=barcol[2], border=barcol[2],xlim=XLIM, ylim=YLIM, axes=FALSE,add=TRUE) }
			z  = order(jdat$depth)
			xc = jdat$depth[z]
			yz = ((1:length(xc))/length(xc))
			cc = cumsum(jdat$catch[z])/1000
			if (showD) { #---Cumulative depth
				mz = approx(yz,xc,.50,rule=2,ties="ordered"); yz=yz*YLIM[2]
				points(mz$y,par()$usr[4],pch=25,col=1,bg="gainsboro",cex=1.5)
				text(mz$y,par()$usr[4],round(mz$y),col="grey20",adj=c(-0.5,0.5),cex=0.9,srt=270)
				}
			if (showC) { #---Cumulative catch
				yc = (cc/max(cc,na.rm=TRUE)) #*YLIM[2]
				mc = approx(yc,xc,.50,rule=2,ties="ordered"); yc=yc*YLIM[2]
				#abline(h=mc$x*YLIM[2],v=mc$y,lty=3,col=ccol)
				points(mc$y,par()$usr[4],pch=25,col=1,bg=ccol,cex=1.5)
				text(mc$y,par()$usr[4],round(mc$y),col=ccol,adj=c(-0.5,0),cex=0.9,srt=270)
				}
			if (showQ) { #---Depth quantiles
				qnt <- round(quantile(jdat$depth,quants,na.rm=TRUE),0)
				abline(v=qnt, lwd=2, col="cornflowerblue")
				text(qnt[1]-xwid,par()$usr[4],qnt[1],col="blue",adj=c(1,2),cex=1.2)
				text(qnt[2]+xwid,par()$usr[4],qnt[2],col="blue",adj=c(0,3),cex=1.2) }
			if (showL) { #---Legend information
				if (nrow*ncol>1)
					addLabel(.98,.90,paste(ifelse(i==0,"",i),ifelse(j==0,"",j),
						sep=ifelse(nrow==1 | ncol==1,""," \225 ")),col="grey30",adj=c(1,1))
				else {
					ylabpos = 0.88
					data(spn); addLabel(.98,ylabpos,spn[spp],adj=c(1,1),col="black",cex=0.9)
					addLabel(.98,ylabpos-0.06,paste("N =",format(ntows,big.mark=","),"tows"),cex=0.8,col="grey30",adj=c(1,1)) 
					addLabel(.98,ylabpos-0.10,paste("C =",format(round(max(cc,na.rm=TRUE)),
						big.mark=","),"t"),cex=0.8,col="grey30",adj=c(1,1)) }
			}
			plot(xy,freq=FALSE, xlab="", ylab="", main="",cex.lab=1.2,
				col=barcol[1], xlim=XLIM, ylim=YLIM, axes=FALSE,add=TRUE)
			if (showD) lines(xc,yz,col="grey20",lwd=clwd)
			if (showC) lines(xc,yc,col=ccol,lwd=clwd)
			box();
		}
	}
	mtext(paste("Depth (m)",ifelse(ncol==1,"",paste(" [",ifelse(yba,"area","year"),
		"by column ]"))),outer=TRUE,side=1,line=2*nrow^0.25,cex=1.2)
	mtext(paste("Proportions",ifelse(nrow==1,"",paste(" [",ifelse(yba,"year","area"),
		"by row ]"))),outer=TRUE,side=2,line=3.25,cex=1.25,las=0)
	if (pix|wmf) dev.off()
	invisible() }
#--------------------------------------preferDepth

#zapHoles-------------------------------2009-08-06
# Attempts to remove holes overwritten by solids.
#-----------------------------------------------RH
zapHoles <- function(pset){
	if (!is.PolySet(pset)) showError("'zapHoles()' requires a valid PolySet")
	flds=names(pset); key=c("PID","SID")
	if (any(match(key,flds,nomatch=0)==0)) return(pset) # no holes to zap (need PID and SID)
	atts=attributes(pset); atts=atts[!is.element(names(atts),c("names","row.names"))]
	pset$index=.createIDs(pset,key,fastIDdig=3)
	cent=calcCentroid(pset,rollup=3)
	cent$index=.createIDs(cent,key,fastIDdig=3)
	cent$area=cent$hole=rep(0,nrow(cent)); cent$hole=as.logical(cent$hole)
	areas=calcArea(pset,rollup=3)
	areas$index=.createIDs(areas,key,fastIDdig=3)
	z=match(cent$index,areas$index)
	cent$area[z]=areas$area[z]
	cent$hole[z]=(areas$area<0)[z]
	ocent=cent[!cent$hole,]    #outer polygons (solids)
	icent=cent[cent$hole,]     #inner polygons (holes)
	xio=match(icent$X,ocent$X,nomatch=0)
	yio=match(icent$Y,ocent$Y,nomatch=-1)
	zio=xio==yio
	zap=icent[zio,]; zap$index=.createIDs(zap,key,fastIDdig=3)
	pset=pset[!is.element(pset$index,zap$index),flds]
	atts$keep=cent[!is.element(cent$index,zap$index),]
	atts$zap=zap
	attributes(pset)=c(attributes(pset),atts)
	return(pset) }
#-----------------------------------------zapHoles

