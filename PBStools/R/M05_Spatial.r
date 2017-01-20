#===============================================================================
# Module 5: Spatial
# -----------------
#  calcHabitat.....Calculate potential habitat using bathymetry.
#  calcOccur.......Calculate percent occurrence of events in PolySet.
#  calcSRFA........Determine SRF areas using major, minor, and locality areas.
#  calcStockArea   Assign a stock area designation based on species HART code and PMFC major and/or minor areas.
#  calcWAParea     Assign a stock area designation for Walleye Pollock using PMFC major and minor areas, a stratifying vector, and a weighting vector.
#  calcSurficial...Calculate intersection of surficial geology and bathymetry interval.
#  clarify.........Analyse catch proportions in blocks, then cluster into fisheries groups.
#  findHoles.......Find holes and place them under correct parents.
#  plotGMA.........Plot the Groundfish Management Areas.
#  plotTernary.....Plot a ternary diagram for data amalgamated into 3 groups.
#  plotTertiary....Composition plots within a polygonal space.
#  preferDepth.....Histogram showing depth-of-capture.
#  prepClara.......Prepare a data object for use by `clarify`.
#  zapHoles........Attempts to remove holes overwritten by solids.
#===============================================================================


#calcHabitat----------------------------2017-01-16
# Calculate potential habitat using bathymetry.
#-----------------------------------------------RH
calcHabitat <- function(topofile="bctopo", isob=c(150,435),
     digits=1, minVerts=25, col.hab="greenyellow", col.land="moccasin",
     xlim=NULL, ylim=NULL,areas=list(), col.areas="red", isolab=TRUE, labtit="",
     plot=TRUE, pin=c(7,8), eps=FALSE, png=FALSE, wmf=FALSE, ioenv=.GlobalEnv)
{
	on.exit(gc(verbose=FALSE))
	assign("PBStool",list(module="M05_Spatial",call=match.call(),args=args(calcHabitat),ioenv=ioenv),envir=.PBStoolEnv)
	icol = rgb(t(round(col2rgb(col.hab)*.65)),maxColorValue=255) # darker than col.hab
	expr=paste("getFile(",topofile,",senv=ioenv,use.pkg=TRUE,try.all.frames=TRUE,tenv=penv()); ",
		"bathy=makeTopography(",topofile,",digits=",digits,")",sep="")
	eval(parse(text=expr))
	bCL <- contourLines(bathy,levels=isob)
	bCP <- convCP(bCL,projection="LL",zone=9)
	bPoly <- bCP$PolySet
	rng = apply(bPoly,2,range)
	if (is.null(xlim)) xlim=rng[,"X"]
	if (is.null(ylim)) ylim=rng[,"Y"]

	box=as.PolySet(data.frame(PID=rep(1,4),POS=1:4,X=xlim[c(1:2,2:1)],Y=ylim[c(1,1,2,2)]),projection="LL",zone=9)
	poly0 = closePolys(fixBound(bPoly,.00001))   ## PolySet of outer-depth polygons
#browser();return()
	polyA = clipPolys(poly0,xlim=xlim,ylim=ylim)
	polyB = findHoles(polyA,minVerts=minVerts)
	#polyC = joinPolys(polyB,operation="UNION")
	habitat = joinPolys(box,polyB,operation="DIFF") ### There is still a bug in joinPolys!!!
	habitat = findHoles(habitat)

	warn <- options()$warn; options(warn = -1)
	area=calcArea(habitat); area=sum(area$area,na.rm=TRUE)
	attr(habitat,"area")=area
	options(warn = warn)

	stuff=c("bathy","bCL","bCP","bPoly","box","polyA","polyB","polyC","habitat","xlim","ylim")
	packList(stuff,"PBStool",tenv=.PBStoolEnv)

	if (plot) {
		fout=paste("Habitat-",ifelse(labtit=="","",paste(gsub(" ","-",labtit),"-",sep="")),isob[1],"m-",isob[2],"m",sep="")
		devs = c(eps=eps,png=png,wmf=wmf,win=TRUE)
		for (i in names(devs)[devs]) {
			if (i=="eps")      postscript(file=paste(fout,".eps",sep=""),width=pin[1],height=pin[2],fonts="mono",paper="special") 
			else if (i=="png") png(paste(fout,".png",sep =""), width=round(pin[1]), height=round(pin[2]), units="in", res=300) 
			else if (i=="wmf" && .Platform$OS.type=="windows")
				do.call("win.metafile",list(filename=paste(fout,".wmf",sep=""),width=pin[1],height=pin[2]))
			else          resetGraph()
			expandGraph(mar=c(3,3.5,0.5,0.5),mgp=c(3,.5,0),las=1)
			plotMap(box,type="n",plt=NULL,cex.axis=1.2,cex.lab=1.5)
			addPolys(habitat,col=col.hab)
			if (!is.null(areas)&&length(areas)>0){
				nareas = length(areas)
				clrs   = rep(col.areas,nareas)[1:nareas]
				sapply(1:nareas,function(x){addPolys(areas[[x]],border=clrs[[x]],lwd=2)})
			}
			#if (eps|png|wmf) addPolys(habitat,col=col.hab,colHoles="white")
			#else addPolys(habitat,col=col.hab)
			addLines(habitat,col=icol)
			data(nepacLL,envir=penv()); addPolys(nepacLL,col=col.land)
			.addAxis(xlim=xlim,ylim=ylim,tckLab=FALSE,tck=0.014,tckMinor=.007)
			if (isolab) legend("bottomleft",inset=0.06,fill=col.hab,title=labtit,title.adj=0.5,
				legend=paste(isob[1],"\226",isob[2],"m  (",format(round(area),big.mark=","),"km\262)",sep=""),bty="n",cex=1.5)
			box(lwd=2)
			if (i %in% c("eps","png","wmf")) dev.off()
		}
	}
	invisible(habitat) }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~calcHabitat


#calcOccur------------------------------2013-01-28
# Calculate percent occurrence of events in PolySet
#-----------------------------------------------RH
calcOccur = function(polyset="qcb", events, wt=1, mess=FALSE, ioenv=.GlobalEnv) {
	assign("PBStool",list(module="M05_Spatial",call=match.call(),args=args(calcOccur),ioenv=ioenv),envir=.PBStoolEnv)
	polyset=as.character(substitute(polyset)) #string name of PolySet object
	events=as.character(substitute(events))   #string name of events bject
	if (polyset=="polyset" || events=="events")
		showError("Do not name objects polyset='polyset' or events='events'")
	expr=paste("getFile(",polyset,",senv=ioenv,use.pkg=TRUE,try.all.frames=TRUE,tenv=penv()); polyset=",polyset,sep="")
	eval(parse(text=expr))
	expr=paste("getFile(",events,",senv=ioenv,use.pkg=TRUE,try.all.frames=TRUE,tenv=penv()); events=",events,sep="")
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
	stuff=c("pd","loc","eidpid","occur","poccur")
	packList(stuff,"PBStool",tenv=.PBStoolEnv)
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


#calcStockArea--------------------------2016-10-17
# Assign a stock area designation based on species
# HART code and PMFC major and/or minor areas.
#-----------------------------------------------RH
#calcStockArea = function (strSpp, major, minor, ...)
calcStockArea = function (strSpp, dat, stockFld="stock")
{
	if (missing(strSpp))
		stop("Must specify a species to determine stock allocation")
	if (missing(dat))
		stop("Must specify a data file with location fields to determine stock allocation")
	flds = names(dat)
	#if (missing(major) && missing(minor))
		#stop("Must supply at least one vector of 'major' or 'minor' areas")
	if (!any(is.element(c("major","minor"),flds)))
		stop("Data file must supply at least one field of 'major' or 'minor' areas")
	if (all(is.element(c("major","minor"),flds))) {
		## Allocations based on combinations of major and minor
		#if (length(major)!=length(minor))
		#	stop("Vectors of 'major' and 'minor' must be equal length.")
		major = dat$major; minor = dat$minor
		newA = rep("UNK",length(major))
		if (is.element(strSpp, c("228"))) {
			newA[is.element(major,7:9)] = "5CDE"
			newA[is.element(major,5:6) | is.element(minor,12)] = "5AB"
			newA[is.element(major,3:4) | is.element(minor,20)] = "3CD"
			newA[is.element(major,1) & !is.element(minor,c(12,20))] = "4B"
		}
		else minor=NULL
	}
	if (!missing(major) && (missing(minor)||is.null(minor))) {
		## Allocations based on major only (emulates the IFMP TAC areas)
		newA = rep("UNK",length(major))
		if (is.element(strSpp,c("059","056","222","228","626"))){
			newA[is.element(major,7:9)] = "5CDE"
			newA[is.element(major,5:6)] = "5AB"
			newA[is.element(major,3:4)] = "3CD"
			newA[is.element(major,1)]   = "4B"
		}
		else if (is.element(strSpp,c("396"))){
			if (is.element("Y",flds)) {
				Y = dat$Y
				newA[is.element(major,9) & Y>52.33333 & !is.na(Y)] = "5DE"
				newA[is.element(major,9) & Y<=52.33333 & !is.na(Y)] = "5ABC"
				newA[is.element(major,9) & is.na(Y)] = "5DE"
			} else
				newA[is.element(major,9)] = "5DE"
			newA[is.element(major,8)]   = "5DE"
			#newA[is.element(major,7)]  = "5C"  ## recent change to IFMP
			newA[is.element(major,5:7)] = "5ABC"
			newA[is.element(major,3:4)] = "3CD"
			newA[is.element(major,1)]   = "4B"
		}
		else if (is.element(strSpp,c("405","437","621"))){
			newA[is.element(major,9)]   = "5E"
			newA[is.element(major,7:8)] = "5CD"
			newA[is.element(major,5:6)] = "5AB"
			newA[is.element(major,3:4)] = "3CD"
			newA[is.element(major,1)]   = "4B"
		}
		else if (is.element(strSpp,c("439","440"))){
			newA[is.element(major,9)]   = "5E"
			newA[is.element(major,7:8)] = "5CD"
			newA[is.element(major,4:6)] = "3D5AB"
			newA[is.element(major,3)]   = "3C"
			newA[is.element(major,1)]   = "4B"
		}
		else if (is.element(strSpp,c("407","424","431","433","442"))){
			newA[is.element(major,9)]   = "5E"
			newA[is.element(major,7:8)] = "5CD"
			newA[is.element(major,6)]   = "5B"
			newA[is.element(major,3:5)] = "3CD5A"
			newA[is.element(major,1)]   = "4B"
		}
		else if (is.element(strSpp,c("465","467"))){
			newA[is.element(major,7:9)] = "5CDE"
			newA[is.element(major,5:6)] = "5AB"
			newA[is.element(major,4)]   = "3D"
			newA[is.element(major,3)]   = "3C"
			newA[is.element(major,1)]   = "4B"
		}
		else if (is.element(strSpp,c("418"))){
			newA[is.element(major,4:9)] = "3D5"
			newA[is.element(major,3)]   = "3C"
			newA[is.element(major,1)]   = "4B"
		}
		else if (is.element(strSpp,c("042","044","224","225"))){
			newA[is.element(major,3:9)] = "OFFS"
			newA[is.element(major,1)]   = "GULF"
		}
		else if (is.element(strSpp,c("394","401","403","435","451","453","454","455","602","607","614"))){
			newA[is.element(major,c(1,3:9))] = "CST"
		}
	}
	if (missing(major) && !missing(minor)) {
		## Allocations based on minor only
		newA = rep("UNK",length(minor))
	}
	dat[,stockFld] = newA
	return(dat)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~calcStockArea


#calcWAParea----------------------------2016-07-28
# Assign a stock area designation for Walleye Pollock
# using PMFC major and minor areas, a stratifying vector,
# and a weighting vector.
#-----------------------------------------------RH
calcWAParea = function (major, minor, strat, wts)
{
	stratify = function (major,minor,strat,wts,targ.major,targ.minor) {
		area.good = is.element(major,targ.major) & !is.element(minor,c(0,99))
		area.kno  = area.good & !is.element(minor,targ.minor)
		area.bad  = is.element(major,targ.major) &  is.element(minor,c(0,99))
		true.bad  = grep(TRUE,area.bad)
		TRUE.bad  = rep(FALSE, length(true.bad))
		names(TRUE.bad) = true.bad
		TRUE.good = TRUE.bad
		ttput(TRUE.good)
		TRUE.targ = list()

		for (gg in names(targ.minor)){
			g = targ.minor[[gg]]
			area.targ = is.element(major,targ.major) & is.element(minor,g)
			if (!missing(strat) && !is.null(strat)){
				strat.bad  = strat[area.bad]
				strat.targ = strat[area.targ]
				strat.good = strat[area.good]
			} else {
				strat.bad  = major[area.bad]
				strat.targ = major[area.bad]
				strat.good = major[area.bad]
			}
			if (!missing(wts) && !is.null(wts)){
				wts.bad  = wts[area.bad]
				wts.targ = wts[area.targ]
				wts.good = wts[area.good]
			} else {
				wts.bad  = rep(1,sum(area.bad))
				wts.targ = rep(1,sum(area.targ))
				wts.good = rep(1,sum(area.good))
			}
			sum.bad  = sapply(split(wts.bad,strat.bad),sum)
			sum.targ = sapply(split(wts.targ,strat.targ),sum)
			sum.good = sapply(split(wts.good,strat.good),sum)

			rec.bad   = sapply(split(rep(1,sum(area.bad)),strat.bad),sum)
			pro.targ = sum.targ/GT0(sum.good[names(sum.targ)])
			rec.targ = round(pro.targ[names(rec.bad)] * rec.bad)
	
			true.strat = split(true.bad,strat.bad)
			true.targ  = sapply(names(true.strat),function(i){
				if (rec.targ[i]>0) {
					ttget(TRUE.good)
					pos1 = match(F,TRUE.good[match(true.strat[[i]],names(TRUE.good))])
					out  = true.strat[[i]][pos1:(rec.targ[i]+pos1-1)]
					TRUE.good[as.character(out)] = TRUE
					ttput(TRUE.good)
					#if (i=="2010") browser()
					return(out)
				}
			})
			TRUE.targ[[gg]] = true.targ
			ttget(TRUE.good)
			area.true = area.bad
			true.good = TRUE.bad    ## use the original untouched bad vector
			true.good[as.character(as.vector(unlist(true.targ)))] = TRUE
			area.true[as.numeric(names(true.good))] = true.good
			ttget(newA)
			newA[area.true] = gg
			ttput(newA)
#if (g==20) {browser();return()}
		}
		TRUE.unk = TRUE.good==TRUE.bad
		area.unk = area.bad
		true.unk = TRUE.bad    ## use the original untouched bad vector
		true.unk[names(TRUE.unk[TRUE.unk])] = TRUE
		area.unk[as.numeric(names(true.unk))] = true.unk
		ttget(newA)
		newA[area.unk] = names(targ.major)
		newA[area.kno] = names(targ.major)
		ttput(newA)
#browser();return()
	}
	##-----------------------MAIN----------------------------
	if (length(major)!=length(minor))
		stop("Vectors of 'major' and 'minor' must be equal length.")
	newA = rep("UNK",length(major))
	newA[is.element(major,7:9)] = "5CDE"
	newA[is.element(major,5:6) | is.element(minor,12)] = "5AB"
	newA[is.element(major,3:4) | is.element(minor,20)] = "3CD"

	if (!missing(strat) && !is.null(strat)) {
		ttput(newA)
		stratify(major=major, minor=minor, strat=strat, wts=wts, targ.major=c(`4B`=1), targ.minor=list(`5AB`=12,`3CD`=20))
		ttget(newA)
	} else 
		newA[is.element(major,1) & !is.element(minor,c(0,12,20,99))] = "4B"
	return(newA)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~calcWAParea


#calcSurficial--------------------------2013-01-28
# Calculate the intersection of surficial geology
# and bathymetry interval object from calcHabitat()
#-----------------------------------------------RH
calcSurficial <- function(surf="qcb", hab,
		xlim=c(-133.4,-127.8), ylim=c(50.5,54.8),
		col.hab=c("aliceblue","grey"), col.cst="grey85", 
		pix=FALSE, wmf=FALSE, ioenv=.GlobalEnv) {

	assign("PBStool",list(module="M05_Spatial",call=match.call(),args=args(calcSurficial),ioenv=ioenv),envir=.PBStoolEnv)
	surf=as.character(substitute(surf)) #string name of surficial object
	if (missing(hab))
		showError("Supply a habitat PolySet")
	else 
		hab=as.character(substitute(hab))   #string name of bathymetry object
	fout=paste(surf,hab,sep=".")
	expr=paste("getFile(",surf,",senv=ioenv,use.pkg=TRUE,try.all.frames=TRUE,tenv=penv()); surf=",surf,sep="")
	eval(parse(text=expr))
	expr=paste("getFile(",hab,",senv=ioenv,use.pkg=TRUE,try.all.frames=TRUE,tenv=penv()); hab=",hab,sep="")
	eval(parse(text=expr))
	pd=attributes(surf)$PolyData
	hab=clipPolys(hab,xlim=xlim,ylim=ylim)
	getFile(nepacLL,use.pkg=TRUE,tenv=penv())
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

	stuff=c("surfhab","areasum","area","leg","legarea")
	packList(stuff,"PBStool",tenv=.PBStoolEnv)

	if (wmf && .Platform$OS.type=="windows")
		do.call("win.metafile",list(filename=paste(fout,".wmf",sep=""),width=6.75,height=8.5))
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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~calcSurficial

#clarify--------------------------------2015-03-06
#  Analyse catch proportions in blocks, then cluster into fisheries groups.
#  Initially, done to address the CASSIS proposal and its impact (John Pringle).
#  CASSIS = CAScadia SeISmic experiment
#-----------------------------------------------RH
clarify <- function(dat, cell=c(0.1,0.075), nG=8,
   xlim=c(-134.2,-127), ylim=c(49.6,54.8), zlim=NULL, targ="YMR", 
   clrs=c("red","orange","yellow","green","forestgreen","deepskyblue",
   "blue","midnightblue"), land="ghostwhite", spp.names=FALSE, 
   wmf=FALSE, eps=FALSE, hpage=10, ioenv=.GlobalEnv) {

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

	if (!requireNamespace("cluster", quietly=TRUE)) stop("`cluster` package is required")
	assign("PBStool",list(module="M05_Spatial",call=match.call(),args=args(clarify),ioenv=ioenv,plotname="Rplot"),envir=.PBStoolEnv)
	fnam=as.character(substitute(dat))
	expr=paste("getFile(",fnam,",senv=ioenv,use.pkg=TRUE,try.all.frames=TRUE,tenv=penv()); dat=",fnam,sep="") # input file made global cause it's big
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

#browser();return()
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

	ptree  <- cluster::clara(psub, k=nG, pamLike=TRUE, metric="euclidean")
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
	if (spp.names) {
		data(species,envir=penv()) # local
		kcode=strsplit(kgrp,split="-")
		kgrp=sapply(kcode,function(x){paste(gsub(" ",".",species[x,"name"]),collapse="+")})
	}
#browser()

	getFile(isobath,use.pkg=TRUE,tenv=penv())
	isob <- isobath[is.element(isobath$PID,c(200,1000,1800)),] # & is.element(isobath$SID,1),]

	stuff=c("tcat","pcat","ptree","pout","pdata","clrs","glab","kgrp")
	packList(stuff,"PBStool",tenv=.PBStoolEnv)

	#--Plot-results---------------------------------------------
	# Try to automate based on par()$pid - plot must be ready on-screen first
	dev=TRUE; plop = list(dev=dev,eps=eps,wmf=wmf)
	for (i in c("dev","eps","wmf")) {
		ii = plop[[i]]
		if (ii) {
	PIN = par()$pin/max(par()$pin)*hpage
	fn = paste(fnam,ifelse(is.null(targ),"",targ),"-Clara-nG",nG,"-(",paste(round(PIN,1),collapse="x"),")",sep="")
	if (i=="eps" && ii) postscript(paste(fn,".eps",sep=""),width=PIN[1],height=PIN[2],paper="special") 
	else if (i=="wmf" && ii && .Platform$OS.type=="windows")
		do.call("win.metafile",list(filename=paste(fn,".wmf",sep=""),width=PIN[1],height=PIN[2]))
	else resetGraph()
	getFile(nepacLL,use.pkg=TRUE,tenv=penv())

	par(parlist)
	plotMap(nepacLL,xlim=xlim,ylim=ylim,plt=NULL,col="white")
	addPolys(agrid,polyProps=pdata,border=FALSE)
	addLines(isob,col=c("steelblue4","navy","black"))
	addPolys(nepacLL,col=land)
	xoff=ifelse(wmf,-.05,0); yoff=ifelse(wmf,-.01,0)
	addLegend(.02,.02,fill=kcol,legend=kgrp,bty="n",cex=ifelse(wmf,0.8,0.7),yjust=0)
	if (!is.null(targ)) 
		addLegend(.35,.01,legend=floor(as.numeric(names(kgrp))),bty="n",
			cex=ifelse(wmf,0.8,0.7),adj=1,xjust=1,yjust=0,text.col="blue",title=targ)
	.addAxis(xlim=xlim,ylim=ylim,tckLab=FALSE,tck=0.014,tckMinor=.007)
	box()
	if (i!="dev" && ii) dev.off()
	}	}
	gc(verbose=FALSE)
	invisible() }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~clarify

#findHoles------------------------------2015-07-17
# Find holes and place them under correct parents.
#-----------------------------------------------RH
findHoles = function(polyset, minVerts=25) 
{
	on.exit(gc(verbose=FALSE))
	findHope = function(X,Y,Z){
		znames = names(Z)
		slop = array(FALSE,dim=c(length(X),length(Y)),dimnames=list(hole=znames,solid=znames))
		for (i in X){
			for(j in Y) {
				slop[i,j] = all(sp::point.in.polygon(
					point.x=Z[[i]]["X"][,1],
					point.y=Z[[i]]["Y"][,1],
					pol.x=Z[[j]]["X"][,1],
					pol.y=Z[[j]]["Y"][,1]))
			}
		}
		return(slop)
	}
	polyset$ID = .createIDs(polyset,c("PID","SID"))
	poly0 = split( polyset , f = polyset$ID )
	bad   = sapply(poly0,nrow) < minVerts
	polyG = poly0[!bad]
	npoly = length(polyG)

	#cents = sapply(polyG,function(x){calcCentroid(x,rollup=1)[c("X","Y")]},simplify=F)
	HinP = findHope(X=1:npoly, Y=1:npoly, Z=polyG)

	## Cols = solids, Rows = holes
	sumC  = apply(HinP,2,sum)
	sumR  = apply(HinP,1,sum)
	allGs = names(polyG)

	## Sum(Cols)==1 and Sum(Rows)==1 are essentially solid land masses (no holes)
	solidGs = allGs[sumC==1 & sumR==1]

	## Sum(Cols)>1 are polygons with holes in them -- definite solids with holes
	swissGs = allGs[sumC>1]
	if (length(swissGs) > 0) {
		warnout = options()$warn; options(warn=-1)  ## temporarily disable warnings from calcArea
		#swissAs = rev(sort(sapply(polyG[swissGs],function(x){calcArea(x,rollup=2)$area})))
		swissAs = rev(sort(sapply(polyG[swissGs],function(x){abs(sum(calcArea(x)$area))})))
		options(warn=warnout)
		swissGs = names(swissAs)
		holes   = sapply(swissGs,function(x){setdiff(allGs[HinP[,x]],x)},simplify=FALSE)
		aholes  = character() ## keep track of holes assigned (sometimes swissGs can be holes in larger swissGs)
	}

	## Start building the final polygon
	rnames = names(polyset)
	if (length(solidGs) > 0) {
		## https://stat.ethz.ch/pipermail/r-help/2008-March/156337.html
		lis   = lapply(polyG[solidGs], "names<-", value = rnames)
#browser();return()
		slis  = lapply(1:length(lis),function(x){xlis=lis[[x]]; xlis$PID=x; xlis$SID=1; xlis})
		pid   = length(slis)
		polyF = do.call("rbind", lapply(slis, data.frame, stringsAsFactors = FALSE))
	} else {
		pid = 0
		polyF = list()
	}
	
	if (length(swissGs) > 0) {
		## Go through swiss cheese polygons and put the holes in the right place
		for (i in names(holes)) {
			if (any(i %in% aholes)) next  # skip smaller swissGs within larger swissGs
			pid = pid + 1; sid = 1
			iswiss = polyG[[i]]
			iswiss$PID = pid
			iswiss$SID = sid
			aholes = c(aholes,holes[[i]]) # keep track of holes assigned to a swissG
			for (j in holes[[i]]) {
				sid = sid + 1
				jhole = polyG[[j]]
				jhole$PID = pid
				jhole$SID = sid
				if (all(diff(jhole$POS)>0))
					jhole$POS = rev(jhole$POS) ## reverse POS means hole in PBSmapping
				iswiss = rbind(iswiss,jhole)
			}
			polyF = rbind(polyF,iswiss)
		}
	}
	polyF = as.PolySet(polyF,projection="LL")
#browser();return
	return(polyF) 
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~findHoles

#plotGMA--------------------------------2017-01-20
# Plot the Groundfish Management Areas
#-----------------------------------------------RH
plotGMA = function(gma=gma.popymr, xlim=c(-134,-123), ylim=c(48.05,54.95), 
   dimfig=c(9,9), eps=FALSE, png=FALSE, extra.labels=NULL, strSpp) {

	oldpar=par(no.readonly=-TRUE); on.exit(par(oldpar))
	if (!is.null(extra.labels) && mode(extra.labels)=="character" && extra.labels[1]=="default") {
		extra.labels = as.EventData(data.frame(
			EID = 1:4,
			X   = c(-132.3628,-130.0393,-129.1727,-126.3594),
			Y   = c(53.74724,51.48773,51.05771,50.15860),
			label = c("Haida\nGwaii","QCS","GIG","Vancouver\nIsland") ),
			projection="LL" )
		if (!missing(strSpp) && strSpp%in%c("228"))
			extra.labels = extra.labels[!is.element(extra.labels$label,c("QCS","GIG")),]
	}
	fnam = as.character(substitute(gma))
	data(nepacLLhigh,major,minor,envir=penv())
	edata = pdata = attributes(gma)$PolyData
	names(edata)[1]="EID"
#browser();return()
	edata=as.EventData(edata,projection="LL")

	if (png) png(filename=paste(fnam,"png",sep="."),width=dimfig[1],height=dimfig[2],units="in",res=300)
	else if (eps) postscript(file=paste0(fnam,".eps"),width=dimfig[1],height=dimfig[2],paper="special",horizontal=FALSE)
	else resetGraph()

	plotMap(gma, type="n", xlim=xlim, ylim=ylim, polyProps=pdata, border="grey",
		plt=c(0.08,0.99,0.07,0.99), mgp=c(3.0,.5,0), las=1, cex.axis=1.25, cex.lab=1.75)

	if (!missing(strSpp) && strSpp%in%c("228")) {
		earlgrey = "grey30"
		maj228 = list()
		maj228[["m5CDE"]] = joinPolys(major[is.element(major$PID,7:9),],operation="UNION")
		maj228[["m5AB"]]  = joinPolys(rbind(major[is.element(major$PID,5:6),], minor[is.element(minor$PID,11:12),]), operation="UNION")
		maj228[["m3CD"]]  = joinPolys(rbind(major[is.element(major$PID,3:4),], minor[is.element(minor$PID,20:21),]), operation="UNION")
		maj228[["m4B"]]   = joinPolys(minor[is.element(minor$PID,c(13:19,28,29)),], operation="UNION")
		majnew = data.frame()
		for (i in 1:length(maj228)) {
			ii = names(maj228)[i]
			idat = maj228[[i]]
			idat$PID = i
			if (!any(grepl("SID",names(idat))))
				idat = data.frame(PID=i,SID=1,idat[,-1])
			majnew = rbind(majnew,idat)
		}
		major = as.PolySet(majnew, projection="LL")
		addPolys(major,col=lucent(c("green","orange","blue","red"),0.55),border=earlgrey)
		addPolys(gma, ,col="transparent", border=earlgrey, lwd=2)
#browser();return()
	} else {
		earlgrey = "grey"
		addPolys(gma, polyProps=pdata, border=earlgrey)
		addPolys(major,col="transparent",border="navyblue",lwd=2)
	}
	addPolys(nepacLLhigh,col="white",border=earlgrey)
	.addAxis(par()$usr[1:2],ylim=par()$usr[3:4],tckLab=FALSE,tck=.015,tckMinor=.015/2)
	addLabels(edata,cex=2.5,font=2)
	if (!is.null(extra.labels))
		addLabels(extra.labels,cex=1.5,col="blue")
	text(-125.2416,52.70739,"BC",cex=5,col="grey",font=2)
	box(lwd=2)
	if (eps|png) dev.off()
	gc(verbose=FALSE)
	invisible(list(pdata=pdata,extra.labels=extra.labels))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotGMA


#plotTernary----------------------------2013-02-26
# Ternary plots - compositions in triangular space
# Equations in Schnute and Haigh (2007)
#-----------------------------------------------RH
plotTernary <- function(x=c(3,2,1), connect=FALSE, show.geometry=TRUE,
   bw=FALSE, eps=FALSE, wmf=FALSE)
{
	ciao = function(opar){ gc(verbose=FALSE); par(opar) }
	oldpar=par(no.readonly=TRUE); on.exit(ciao(oldpar))
	fill = ifelse(bw,"light gray","yellow")
	xlim <- c(0,1); ylim <- c(0,sqrt(3)/2)
	lwd = 2; tadj = 0.02; xyrng = c(-tadj, 1 + tadj) # text placement adjustments

	sq3 = sqrt(3)
	triangle = as.PolySet(data.frame(PID=rep(1,3),POS=1:3,X=c(1,0,.5),Y=c(0,0,sq3/2)),projection=1)

	if (is.vector(x)) x = matrix(x,nrow=1)
	if (!is.matrix(x) && !is.data.frame(x)) stop ("Input x a matrix or data.frame")
	if (ncol(x)!=3) stop("Data for ternary diagrams nust have three observations (columns) per event")
	if (is.null(rownames(x))) xnames = paste("x",1:nrow(x),sep="") else xnames=rownames(x)
	if (is.null(colnames(x))) ynames = paste("p",1:3,sep="") else ynames=colnames(x)
	
	p = t(apply(x,1,function(x){x/sum(x)}))
	N = dim(p)[1]
	p1=p[,1]; p2=p[,2]; p3=p[,3] #fornow
	#psum <- p1 + p2 + p3; 
	#p1 <- p1/psum; p2 <- p2/psum; p3 <- p3/psum

	# Appendix A. Ternary diagram equations (p.231)
	x   = (2*p2 + p3)/2       ; y  = sq3*p3 / 2              #(A.1)
	x1  = (3 + x - sq3*y) / 4 ; y1 = (sq3*(1-x) + 3*y) / 4   #(A.2)
	x2  = (x + sq3*y) / 4     ; y2 = (sq3*x + 3*y) / 4       #(A.3)
	x3  = x                   ; y3 = rep(0,N)                #(A.4)
	#----------------------------------------------

	if (eps) postscript(file="ternary.eps", width=8,height=7,paper="special")
	else if (wmf && .Platform$OS.type=="windows")
		do.call("win.metafile",list(filename="ternary.wmf",width=10,height=8.7,pointsize=12))
	par( mfrow=c(1,1), mai=c(.2,.2,.2,.2), omi=c(0,0,0,0))
	plotMap(triangle,axes=FALSE,xlab="",ylab="",xlim=extendrange(xlim),ylim=extendrange(ylim),plt=NULL,lwd=lwd*2)
	#xpts <- c(x1,x2,x3); ypts <- c(y1,y2,y3)
	#points(xpts,ypts,col="blue")
	XY = cbind(x,y,x1,y1,x2,y2,x3,y3)
	if (connect) {
		#colPal = colorRampPalette(c("grey95", "grey30"))
		colPal = colorRampPalette(c("navy","green4","yellow","red"))
		lines(x,y,lwd=lwd,col="gainsboro")
		points(x,y,pch=21,bg=colPal(N),cex=1.5)
		text(x,y-0.02,xnames)
	}
	if (show.geometry) {
		apply(XY,1,function(xy){
			lines(c(xy[3],xy[1]),c(xy[4],xy[2]),lwd=lwd)
			lines(c(xy[5],xy[1]),c(xy[6],xy[2]),lwd=lwd)
			lines(c(xy[7],xy[1]),c(xy[8],xy[2]),lwd=lwd)
			lines(c(0,xy[1]),c(0,xy[2]),lty=2)
			lines(c(1,xy[1]),c(0,xy[2]),lty=2)
			lines(c(0.5,xy[1]),c(sq3/2,xy[2]),lty=2)
			points(xy[1],xy[2],pch=21,bg=fill,cex=1.5)
		})
		text((x+x1)/2,(y+y1)/2+tadj,expression(italic(p)[1]))
		text((x+x2)/2,(y+y2)/2+tadj,expression(italic(p)[2]))
		text((x+x3)/2+tadj,(y+y3)/2,expression(italic(p)[3]))
	}
	text(-tadj,0,"1",font=2)
	text(1+tadj,0,"2",font=2)
	text(0.5,sq3/2+tadj,"3",font=2)
	if(eps|wmf) dev.off()
#browser();return()
	invisible() }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotTernary


#plotTertiary---------------------------2013-03-13
# Composition plots within a polygonal space.
#-----------------------------------------------RH
plotTertiary = function(x=c(100,5,25,10,50), pC=c(0.5,0.5), r=0.5,
   diag=FALSE, eps=FALSE, wmf=FALSE)
{
	ciao = function(opar){ gc(verbose=FALSE); par(opar) }
	oldpar=par(no.readonly=TRUE); on.exit(ciao(oldpar))
	assign("PBStool",list(module="M05_Spatial",call=match.call(),args=args(plotTertiary)),envir=.PBStoolEnv)

	if (is.vector(x)) x = matrix(x,nrow=1)
	if (!is.matrix(x) && !is.data.frame(x)) stop ("Input x a matrix or data.frame")
	if (is.null(rownames(x))) xnames = paste("M",1:nrow(x),sep="") else xnames=rownames(x)
	if (is.null(colnames(x))) ynames = paste("p",1:ncol(x),sep="") else ynames=colnames(x)

	M = dim(x)[1]; N = dim(x)[2]; MNnam = list(xnames,ynames)
	pA = 360/N 
	A0 = pA/2; R0 = A0 * pi/180
	A1 = 270-A0; A = c(A1, A1+(1:(N-1))*pA); 
	A[A>360] = A[A>360] - 360                 # angles from centroid to vertex
	R  = A * pi/180                           # centroid-vertex angles in radians
	Aside = A+A0
	Aside[Aside>360] = Aside[Aside>360] - 360     # angles from centroid to polygon sides
	Rside  = Aside * pi/180                   # centroid-side angles in radians
	Avert = A - 180
	Avert[Avert<0] = Avert[Avert<0] + 360     # angles from vertex to centroid
	Rvert  = Avert * pi/180                   # vertex-centroid angles in radians

	stuff = c("MNnam","pA","A0","A1","A","Aside","Avert","R0","R","Rside","Rvert")
	packList(stuff,target="PBStool",tenv=.PBStoolEnv)

	b  = round(tan(R),6)                      # slopes relative to shape centroid
	a  = round(pC[2] - b * pC[1], 6)          # line intercepts
	sx = as.numeric(A<90|A>280); sx[sx<1]=-1; sx[A%in%seq(90,360,180)]=0  # shape x-vertices >= centroid x
	sy = as.numeric(A>0&A<180);  sy[sy<1]=-1; sy[A%in%seq(0,360,180)]=0   # shape y-vertices >= centroid y
	X  = pC[1]+r*cos(R)                       # polygon vertex X-coordinates
	Y  = pC[2]+r*sin(R)                       # polygon vertex Y-coordinates

	xlim = range(X); ylim = range(Y)
	lwd = 2
	tadj = 0.02; xyrng = c(-tadj, 1 + tadj)   # text placement adjustments

	shape = as.PolySet(data.frame(PID=rep(1,N),POS=1:N,X=X,Y=Y),projection=1)

	Nside = ceiling(N/2)                           # Number of opposite polygon side
	zside = c(Nside:N,1:(Nside-1))                 # Index of opposite side for each vertex
	s = rbind(shape,shape[1,])
	xside = shape$X+diff(s$X)/2
	yside = shape$Y+diff(s$Y)/2
	XYside = data.frame(x=xside[zside], y=yside[zside])
	#bside = round(diff(s$Y),6)/round(diff(s$X),6)  # slopes of polygon sides
	#aside = round(shape$Y-bside*shape$X,6)         # y-intercept of polygon sides
	#Nside = ceiling(N/2)                           # Number of opposite polygon side
	#zside = c(Nside:N,1:(Nside-1))                 # Index of opposite side for each vertex
	#xside = (aside[zside]-a)/(b-bside[zside])      # x-coordinate at mid-point of each opposite side
	#yside = aside[zside] + bside[zside]*xside      # y-coordinate at mid-point of each opposite side
	#XYside = data.frame(x=xside,y=yside)

	if (as.logical(N%%2))
	# Opposite point of vertex in polygon with odd number of sides is mid-point of an opposite side (e.g., triangle)
		Dtab = apply(shape,1,function(S1,S2){
			apply(S2,1,function(x,y){sqrt((x[1]-y[3])^2 + (x[2]-y[4])^2)},y=S1)
		},S2=XYside)
	else
	# Opposite point of vertex in polygon with even number of sides is another vertex (e.g., square)
		Dtab = apply(shape,1,function(S1,S2){
			apply(S2,1,function(x,y){sqrt((x[3]-y[3])^2 + (x[4]-y[4])^2)},y=S1)
		},S2=shape)
	#D = max(Dtab)  # maximum distance between vertex and opposite side (even-sided polygon) or vertex (odd-sided polygons)

	Dmin = sum(apply(XYside,1,function(x,m){sqrt((x[1]-m[1])^2+(x[2]-m[2])^2)},m=pC))            # maximum sum of p if mode occurs at the centre
	if (as.logical(N%%2))
		h = Dmin/N + r  # height of the polygon
	else
		h = 2 * Dmin/N
	Dmax = sum(apply(XYside,1,function(x,m){sqrt((x[1]-m[1])^2+(x[2]-m[2])^2)},m=shape[1,3:4]))  # maximum sum of p if mode occurs at a vertex

	#D = sqrt((pC[1]-pC[1])^2+(pC[2]-shape$Y[1])^2)*N # N times distance from centroid to the base
	#D = sqrt((pC[1]-pC[1])^2+(pC[2]-shape$Y[1])^2)+r # N times distance from centroid to the base
	#D = sqrt((pC[1]-pC[1])^2+(pC[2]-shape$Y[1])^2)*N^(1/(N-2)) # N times distance from centroid to the base

	p  = t(apply(x,1,function(x){x/sum(x)})); dimnames(p)=MNnam
	if (N==3) pD = t(apply(p,1,function(x){x*sqrt(3)/2}))           # p-vector lengths proportional to p (ternary)
	else      pD = t(apply(p,1,function(x,h) { h*x/max(x) },h=h^N)) # p-vector lengths proportional to p
	dimnames(pD)=MNnam
	D  = apply(pD,1,sum)
	#pD = t(apply(p,1,function(x) { D*x }));   dimnames(pD)=MNnam    # p-vector lengths proportional to p
#browser()
	q  = t(sapply(xnames,function(x){D[x]-p[x,]*D[x]}));   dimnames(q)=MNnam
	#q = t(apply(p,1,function(x) { D-scaleVec(x,min(x),D) }));   dimnames(q)=MNnam

	# XYmean deprecated (mean of points does not work)
	XYmean = apply(q,1,function(qvec,shp,rad){  # p-vertices at end of vector with magnitude q and direction Rvert
		x = shp$X + qvec*cos(rad)
		y = shp$Y + qvec*sin(rad)
		return(data.frame(x=x,y=y))
	}, shp=shape, rad=Rvert)
	pM = sapply(XYmean,function(px){apply(px,2,mean)},simplify=FALSE) # Midpoint resultant for each M set
	pMtab = as.data.frame(t(sapply(pM,function(x){c(x)})))            # Midpoints in tabular format

	# Takes the mean of coordinate intersections between two pvecs perpendicular to each side of each vertex
	XYmode = sapply(xnames,function(xnam,pvec,dvec,side,R,A){  # p-vertices at end of vector with magnitude q and direction Rvert
		xP = side$x + dvec[xnam,]*cos(R)
		yP = side$y + dvec[xnam,]*sin(R)
		Aperp = A - 90; Aperp[Aperp<0] = Aperp[Aperp<0]+360
		Rperp = Aperp*pi/180
		# from sides opposite vertices 1,2,3,...
		bperp = round(tan(Rperp),6)
		aperp = yP - bperp*xP
		if (N==3) {
			x0 = (2*pvec[xnam,2]+pvec[xnam,3])/2
			y0 = (sqrt(3)*pvec[xnam,3])/2
			xmode = scaleVec(c(0,x0,1),shape$X[1],shape$X[2])[2]
			ymode = scaleVec(c(0,y0,sqrt(3)/2),shape$Y[2],shape$Y[3])[2]
			#xmode = (aperp[2]-aperp[1])/(bperp[1]-bperp[2]) # Only need to solve (x,y) for the first two lines (only true for triangles);
			#ymode = aperp[1] + bperp[1]*xmode               # all others are assumed to have the same intersection.
			#xmode = xmode + xoff; ymode = ymode + yoff
			dside = sqrt((shape$X[1]-shape$X[2])^2 + (shape$Y[1]-shape$Y[2])^2)
#browser()
		}
		else {
			xmode=ymode=dside=numeric(N)
			for (i in 1:N) {
				j = ifelse(i==N,1,i+1)
				xmode[i] = (aperp[j]-aperp[i])/(bperp[i]-bperp[j])  # Only need to solve (x,y) for the first two lines (only true for triangles);
				ymode[i] = aperp[i] + bperp[i]*xmode[i]             # all others are assumed to have the same intersection.
				dside[i] = sqrt((shape$X[i]-shape$X[j])^2 + (shape$Y[i]-shape$Y[j])^2)
			}
		}
#browser()
		return(data.frame(x=xmode,y=ymode,d=dside))
	}, pvec=p, dvec=pD, side=XYside, R=R, A=A,simplify=FALSE)
	#XYmode = t(XYmode)
#browser();return()
	pM = sapply(XYmode,function(px){apply(px,2,mean)},simplify=FALSE) # Midpoint resultant for each M set
	pMtab = as.data.frame(t(sapply(pM,function(x){c(x)})))            # Midpoints in tabular format

	stuff = c("shape","XYside","Dtab","Dmin","Dmax","p","pD","D","q","XYmean","XYmode","pM","pMtab")
	packList(stuff,target="PBStool",tenv=.PBStoolEnv)

	if (eps) postscript(file="tertiary.eps", width=8,height=8,paper="special")
	else if (wmf && .Platform$OS.type=="windows")
		do.call("win.metafile",list(filename="tertiary.wmf",width=10,height=10,pointsize=12))
	par( mfrow=c(1,1), mai=c(.2,.2,.2,.2), omi=c(0,0,0,0))
	plotMap(shape,axes=FALSE,xlab="",ylab="",xlim=extendrange(xlim),ylim=extendrange(ylim),plt=NULL,lwd=lwd*2)
	if (diag) {
		# Raw vecors of pD (proportional to p) coming from the opposite side
		xraw = XYside$x + pD*cos(Rside-ifelse(N%%2,R0,R0*2))
		yraw = XYside$y + pD*sin(Rside-ifelse(N%%2,R0,R0*2))
		xvec = as.vector(t(cbind(XYside$x,t(xraw),rep(NA,N))))
		yvec = as.vector(t(cbind(XYside$y,t(yraw),rep(NA,N))))
		lines(xvec,yvec,col="blue")
		points(xvec,yvec,pch=21,bg="pink")
	}
	if (M==1) {
		sapply(pM,function(x){
			xM = x[[1]]; yM = x[[2]]; clen=x[[3]]
			xvec = as.vector(t(cbind(shape$X,rep(xM,N),rep(NA,N))))
			yvec = as.vector(t(cbind(shape$Y,rep(yM,N),rep(NA,N))))
			lines(xvec,yvec,lty=2,lwd=1)
			# pvecs: see Law of Cosines - http://en.wikipedia.org/wiki/Law_of_cosines
			blen = sqrt((xM-shape$X)^2+(yM-shape$Y)^2)
			alen = blen[c(2:N,1)]
			beta = acos((alen^2 + clen^2 - blen^2)/(2*alen*clen))
			alph = acos((blen^2 + clen^2 - alen^2)/(2*blen*clen))
			zOK = alph*180/pi<=90 & beta*180/pi <= 90 # angle >90 will cause pvec to lie outside the polygon
			Nshow = sum(zOK)
			dlen = alen*sin(beta)
			pvx = xM + dlen * cos(Rside)
			pvy = yM + dlen * sin(Rside)
			xpvec = as.vector(rbind(pvx[zOK],rep(xM,Nshow),rep(NA,Nshow)))
			ypvec = as.vector(rbind(pvy[zOK],rep(yM,Nshow),rep(NA,Nshow)))
			lines(xpvec,ypvec,lwd=lwd)
		})
		points(pM[[1]][1],pM[[1]][2],pch=21,cex=1.5,bg="yellow",lwd=1)
	} else {
		colPal = colorRampPalette(c("navy","green4","yellow","red"))
		lines(pMtab$x,pMtab$y,lwd=lwd,col="gainsboro")
		points(pMtab$x,pMtab$y,pch=21,bg=colPal(M),cex=1.5,lwd=1)
		text(pMtab$x,pMtab$y-0.02,xnames)
#browser()
	}
	
	text(tadj*sx+X,tadj*sy+Y,1:N,cex=1.2,font=2)
	if(eps|wmf) dev.off()
#browser();return()
	invisible(pMtab) }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotTertiary


#preferDepth----------------------------2010-10-19
# Histogram showing depth-of-capture
#-----------------------------------------------RH
preferDepth = function(strSpp="410", fqtName="pht_fdep.sql", dbName="PacHarvest",
   spath=NULL, type="SQL", hnam=NULL, get.effort=TRUE)
{
	warn <- options()$warn; options(warn=-1)
	assign("PBStool",list(module="M05_Spatial",call=match.call(),args=args(preferDepth),plotname="Rplot"),envir=.PBStoolEnv)

	wpath <- .getWpath()
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
		packList("effort","PBStool",tenv=.PBStoolEnv) }
	invisible() }

#.preferDepth.getEffort-----------------2010-10-19
.preferDepth.getEffort=function(strSpp="ALL") {
		resetGraph(); par(mar=c(0,0,0,0),oma=c(0,0,0,0))
		showMessage("Please wait while effort data is retrieved for this GUI session",col="blue",cex=1.2)
		getData("pht_effort.sql",strSpp=strSpp,path=.getSpath(),tenv=penv())
		effort=PBSdat; effort$effort=effort$effort/60 ### convert to hours
		#packList("effort","PBStool",tenv=.PBStoolEnv) ### too slow
		#eval(parse(text="PBStool$effort <<- effort"))
		ttget(PBStool); PBStool$effort <- effort; ttput(PBStool)
		frame(); addLabel(.5,.5,"Effort loaded",col="darkgreen",cex=1.2) }

#.preferDepth.getDepth------------------2017-01-16
.preferDepth.getDepth <- function()
{
	opar <- par(lwd=2); on.exit(par(opar))
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
			",uid=\"",uid,"\",pwd=\"",pwd,"\",tenv=penv())",sep="")
		eval(parse(text=expr))
		if (!is.null(attributes(PBSdat)$spp) && attributes(PBSdat)$spp!=spp) {
			spp <- attributes(PBSdat)$spp
			setWinVal(list(strSpp=spp),winName="window") } }
	assign("dat",PBSdat)
	if (nrow(dat)==0) showError(paste("Species =",spp),type="nodata")
	packList("dat","PBStool",tenv=.PBStoolEnv) ## RH: save the raw data for manual sunsetting
	eff = ttcall(PBStool)$effort
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
		## RH: At least make sure that the majors match
		majors = .su(dat$major)
		if (isE){
			eff = eff[is.element(eff$major,majors),]
			eff$area = rep("CST",nrow(eff))
		}
		dat$area <- rep("CST",nrow(dat))
	} else {
		if (isE) eff$area <- eff[,disA]
		dat$area <- dat[,disA]
		if (!is.null(area)) {
			if (isE) eff <- eff[is.element(eff$area,area),]
			dat <- dat[is.element(dat$area,area),] } 
		else {
			if (isE) eff <- eff[!is.na(eff$area),]
			dat <- dat[!is.na(dat$area),] }
		if (nrow(dat)==0) showError(paste(disA,"=",paste(area,collapse=", ")),type="nodata")
	}
	if (is.null(year)) year <- 0 else year <- sort(unique(dat$year)); nyr <- length(year);
	if (is.null(area)) area <- 0 else area <- sort(unique(dat$area)); nar <- length(area);

	plotname = paste("dep",spp,ifelse(year==0,"",paste("-(",paste(year,collapse="."),")",sep="")),
		ifelse(area==0,"",paste("-(",paste(area,collapse=""),")",sep="")),
		ifelse(is.null(gear),"",paste("-gear",paste(gear,collapse=""),sep="")),sep="")
	if (type=="FILE") plotname = sub(paste("dep",spp,sep=""),fqtName,plotname)
	packList("plotname","PBStool",tenv=.PBStoolEnv)
	nrow <- max(nyr,nar); ncol <- min(nyr,nar);
	if (nyr>=nar) { ifac <- year; jfac <- area; yba <- TRUE}
	else { ifac <- area; jfac <- year; yba <- FALSE}

	eps=png=wmf=FALSE
	if (!is.null(act)){
		if (act=="eps") eps=TRUE
		else if (act=="png") png=TRUE
		else if (act=="wmf") wmf=TRUE
	}
	if (eps)      postscript(file=paste(plotname,"eps",sep="."),width=10,height=6*nrow^0.25,horizontal=FALSE,paper="special")
	else if (png) png(filename=paste(plotname,"png",sep="."),width=10,height=6*nrow^0.25,units="in",res=300)
	else if (wmf && .Platform$OS.type=="windows")
		do.call("win.metafile",list(filename=paste(plotname,"wmf",sep="."),width=10,height=6*nrow^0.25))
	else resetGraph()

	if (nrow*ncol==1)
		expandGraph(mfrow=c(nrow,ncol),mar=c(0,0,0,0),oma=c(4,5,1,1),cex=0.9,yaxs="i",mgp=c(2,.5,0),las=1)
	else
		expandGraph(mfrow=c(nrow,ncol),mar=c(0,0,0,0),oma=c(5,5,1,1),cex=0.7,yaxs="i",mgp=c(2,.5,0),las=1);
	xlim <- c(0,round(max(c(dat$depth,eff$depth),na.rm=TRUE),-1)) + c(-xwid,xwid); xdiff <- diff(xlim);
	ylim <- c(0,.19)
	brks = seq(xlim[1],xlim[2],xwid)
	BRKS = seq(XLIM[1],XLIM[2],xwid)
	stuff=c("xlim","ylim","brks","BRKS","area")
	packList(stuff,"PBStool",tenv=.PBStoolEnv)

	brks = BRKS
	dat  = dat[dat$depth>=brks[1] & dat$depth<=rev(brks)[1] & !is.na(dat$depth),]
	if (isE)
		eff  = eff[eff$depth>=brks[1] & eff$depth<=rev(brks)[1] & !is.na(eff$depth),]

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
			packList(stuff,"PBStool",tenv=.PBStoolEnv)
#browser();return()
			#xy <- hist(jdat$depth, breaks=brks, plot=FALSE);
			xy <- hist(jdat$depth, breaks=brks, plot=FALSE);
			xy$density <- xy$counts/sum(xy$counts)
			xyout <- paste("xy",spp,i,j,sep=".")
			#eval(parse(text="PBStool[[xyout]] <<- xy"))
			ttget(PBStool); PBStool[[xyout]] <- xy; ttput(PBStool)

			if (isE) {
				jeff$dbin=cut(jeff$depth,brks)
				effTot=sapply(split(jeff$effort,jeff$dbin),sum)
				effDen=effTot/sum(effTot)
				xye=list(breaks=brks,counts=effTot,density=effDen)
				attr(xye,"class")="histogram"
				packList("xye","PBStool",tenv=.PBStoolEnv) }

			par(lwd=2) ## doesn't seem to take when specified in first line of function
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
					ylabpos = 0.88; labcex=1
					data(spn,envir=penv())
					addLabel(.98,ylabpos,spn[spp],adj=c(1,1),col="black",cex=labcex)
					addLabel(.98,ylabpos-0.06,paste("N =",format(ntows,big.mark=","),"tows"),cex=labcex-0.1,col="grey30",adj=c(1,1)) 
					addLabel(.98,ylabpos-0.10,paste("C =",format(round(max(cc,na.rm=TRUE)),
						big.mark=","),"t"),cex=labcex-0.1,col="grey30",adj=c(1,1)) }
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
	if (eps|png|wmf) dev.off()
	invisible() }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~preferDepth


#prepClara----------------------------2013-01-28
# Prepare a data object for use by `clarify`,
#  which uses the cluster function `clara`.
# Arguments:
#  ssid = survey series ID
#  gfld = grouping field name
#  sfld = species filed name
#  mfld = metric field name
#  fnam = assume input data object (not file) is always called `dat` and
#         output object (not file) is always called `claradat`.
#-----------------------------------------------RH
prepClara =function(ssid=7, gfld=c("X","Y"), sfld="spp", mfld="catKg", fnam=NULL, ioenv=.GlobalEnv)
{
	SSID = paste(ssid,collapse=".")
	if (is.null(fnam)){ 
		getData("gfb_clara_survey.sql","GFBioSQL",strSpp="000",path=.getSpath(),survserid=ssid)
		dat = PBSdat
		save("dat",file=paste("ssid.",SSID,".dat.rda",sep=""))
		fout = paste("clara.ssid.",SSID,".rda",sep="")
	}
	else {
		mess=paste("getFile(\"",fnam,"\",senv=ioenv,reload=TRUE,try.all.frames=TRUE,tenv=penv())",sep="")
		eval(parse(text=mess))
		fout = paste("clara.",fnam,".rda",sep="")
	}
	dat  = dat[dat$X<0 & !is.na(dat$X) & dat$Y>0 & !is.na(dat$Y),]
#browser();return()
	if (all(gfld==c("X","Y"))){
		index = -round(dat$X,5)*1e5 + round(dat$Y,5)/100
		uID = sort(unique(index))
		dat$index = match(index,uID)
		gfld = "index"
	}
	rows = sort(unique(dat[,gfld]))
	cdat = array(NA,dim=c(length(rows),3),dimnames=list(rows,c("X","Y","Z")))
	for (i in c("X","Y","Z")) {
		mess = paste("g",i," = sapply(split(dat[,\"",i,"\"],dat[,\"",gfld,"\"]),function(x){",
			"z = round(x,5)==0; ",
			"if (all(z)) 0 else mean(x[!z]) } ); ",
			"cdat[names(g",i,"),\"",i,"\"]=g",i, sep="")
		eval(parse(text=mess))
	}
	spptab = crossTab(dat,c(gfld,sfld),mfld,sum)
	gtab   = spptab[,-1]; attr(gtab,"class")="data.frame"; row.names(gtab)=spptab[,gfld]
	zrows  = sort(unique(intersect(row.names(cdat),row.names(gtab))))
	claradat = data.frame(EID=1:length(zrows),cdat[zrows,],gtab[zrows,],check.names=FALSE)
	save("claradat",file=fout)
	gc(verbose=FALSE)
	return(claradat)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~prepClara


#zapHoles-------------------------------2009-08-06
# Attempts to remove holes overwritten by solids.
#-----------------------------------------------RH
zapHoles <- function(pset){
	if (!is.PolySet(pset)) showError("`zapHoles()` requires a valid PolySet")
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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~zapHoles

