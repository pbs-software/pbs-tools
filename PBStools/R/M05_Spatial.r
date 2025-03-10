## =============================================================================
## Module 5: Spatial
## -----------------
##  calcHabitat.....Calculate potential habitat using bathymetry.
##  calcOccur.......Calculate percent occurrence of events in PolySet.
##  calcSRFA........Determine SRF areas using major, minor, and locality areas.
##  calcStockArea...Assign a stock area designation based on species HART code and PMFC major and/or minor areas.
##  calcWAParea.....Assign a stock area designation for Walleye Pollock using PMFC major and minor areas, a stratifying vector, and a weighting vector.
##  calcSurficial...Calculate intersection of surficial geology and bathymetry interval.
##  clarify.........Analyse catch proportions in blocks, then cluster into fisheries groups.
##  findEP..........Find events in polys and and add poly info to events.
##  findHoles.......Find holes and place them under correct parents.
##  plotConcur......Horizontal barplot of concurrent species in tows.
##  plotEO..........Plot Extent of Occurrence for a species using a convex hull.
##  plotGMA.........Plot the Groundfish Management Areas.
##  plotHabitat.....Plot potential habitat using bathymetry output from 'calcHabitat'.
##  plotLocal.......Plot DFO fishing localities with the highest catch.
##  plotTernary.....Plot a ternary diagram for data amalgamated into 3 groups.
##  plotTertiary....Composition plots within a polygonal space.
##  preferDepth.....Histogram showing depth-of-capture.
##  prepClara.......Prepare a data object for use by `clarify`.
##  zapHoles........Attempts to remove holes overwritten by solids.
## ==============================================================================


## calcHabitat--------------------------2024-02-29
## Calculate potential habitat using bathymetry.
## Can be a slow process -- set 'use.sp.pkg=TRUE' for faster execution
##   Passing 'use.sp.pkg' to 'findHoles':
##   TRUE --  uses sp::point.in.polygon ~ 35s
##   FALSE -- uses PBSmapping::.is.in   ~ 95s
## ---------------------------------------------RH
calcHabitat <- function(topofile="bctopo", isob=c(150,435),
   digits=1, minVerts=25, use.sp.pkg=TRUE, areas=list(),
   col.hab="greenyellow", col.land="moccasin", col.areas="red",
   xlim=NULL, ylim=NULL, isolab=TRUE, labtit="", plot=TRUE,
   PIN=c(7,8), eps=FALSE, png=FALSE, wmf=FALSE, ioenv=.GlobalEnv)
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

	## box should not be bigger than the extent of polyB, otherwise 
	## area of extent will be inflated by land not covered by polyB
	if (is.null(xlim)) xlim = rng[,"X"]
	else               xlim = c(max(xlim[1],rng[1,"X"]),min(xlim[2],rng[2,"X"]))
	if (is.null(ylim)) ylim=rng[,"Y"]
	else               ylim = c(max(ylim[1],rng[1,"Y"]),min(ylim[2],rng[2,"Y"]))

	box=as.PolySet(data.frame(PID=rep(1,4),POS=1:4,X=xlim[c(1:2,2:1)],Y=ylim[c(1,1,2,2)]),projection="LL",zone=9)
	poly0 = closePolys(fixBound(bPoly,.00001))   ## PolySet of outer-depth polygons
	polyA = clipPolys(poly0, xlim=xlim, ylim=ylim)
	polyB = findHoles(polyA, minVerts=minVerts, use.sp.pkg=use.sp.pkg)
	#polyC = joinPolys(polyB,operation="UNION")

	habitat = joinPolys(box,polyB,operation="DIFF") ### There is still a bug in joinPolys!!!
	habitat = findHoles(habitat, minVerts=minVerts, use.sp.pkg=use.sp.pkg)
#browser();return()

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
			if (i=="eps")      postscript(file=paste(fout,".eps",sep=""),width=PIN[1],height=PIN[2],fonts="mono",paper="special") 
			else if (i=="png") png(paste(fout,".png",sep =""), width=round(PIN[1]), height=round(PIN[2]), units="in", res=300) 
			else if (i=="wmf" && .Platform$OS.type=="windows")
				do.call("win.metafile",list(filename=paste(fout,".wmf",sep=""),width=PIN[1],height=PIN[2]))
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
			data("nepacLL", package="PBSmapping", envir=penv())
			addPolys(nepacLL,col=col.land)
			.addAxis(xlim=xlim,ylim=ylim,tckLab=FALSE,tck=0.014,tckMinor=.007)  ## .addAxis currently not exported from PBSmapping namespace
			if (isolab) legend("bottomleft", inset=0.06, fill=col.hab, title=labtit, title.adj=0.5, bty="n", cex=1.5,
				#legend=paste(isob[1],"\226",isob[2],"m  (",format(round(area),big.mark=","),"km\262)",sep=""),bty="n",cex=1.5)
				legend=paste0(isob[1], convUTF("\\u{2013}"), isob[2], "m  (", format(round(area), big.mark=","), "km", convUTF("\\u{00B2}"), ")") )
			box(lwd=2)
			if (i %in% c("eps","png","wmf")) dev.off()
		}
	}
	invisible(habitat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~calcHabitat


#calcOccur------------------------------2013-01-28
# Calculate percent occurrence of events in PolySet
#-----------------------------------------------RH
calcOccur <- function(polyset="qcb", events, wt=1, mess=FALSE, ioenv=.GlobalEnv)
{
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
	return(poccur)
}
#----------------------------------------calcOccur


#calcSRFA-------------------------------2008-12-01
# Determine SRF assessment areas or subareas (gullies) based on 
# combinations of major and minor areas and localities.
#-----------------------------------------------RH
calcSRFA <- function(major, minor=NULL, loc=NULL, subarea=FALSE)
{ 
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
	return(srfa)
}
#-----------------------------------------calcSRFA


## calcStockArea------------------------2023-05-09
## Assign a stock area designation based on species
## HART code and PMFC major and/or minor areas.
## ---------------------------------------------RH
calcStockArea <- function (strSpp, dat, stockFld="stock", gmu=TRUE)
{
	if (missing(strSpp))
		stop("Must specify a species to determine stock allocation")
	if (missing(dat))
		stop("Must specify a data file with location fields to determine stock allocation")
	flds = names(dat)
	if (!any(is.element(c("major","minor"),flds)))
		stop("Data file must supply at least one field of 'major' or 'minor' areas")

	if (all(is.element(c("major","minor"),flds)) && strSpp %in% c("228")) {
		## Allocations based on combinations of major and minor
		major = dat$major; minor = dat$minor
		newA = rep("UNK",length(major))
		if (is.element(strSpp, c("228"))) {
			newA[is.element(major,7:9)] = "5CDE"
			newA[is.element(major,5:6) | is.element(minor,12)] = "5AB"
			newA[is.element(major,3:4) | is.element(minor,20)] = "3CD"
			newA[is.element(major,1) & !is.element(minor,c(12,20))] = "4B"
		}
		else minor=NULL
	} else if ("major" %in% flds) {
		## Allocations based on major only (emulates the IFMP TAC areas)
		major = dat$major
		newA  = rep("UNK",length(major))
		if (is.element(strSpp,c("059","056","222","228","626"))){
			newA[is.element(major,7:9)] = "5CDE"
			newA[is.element(major,5:6)] = "5AB"
			newA[is.element(major,3:4)] = "3CD"
			newA[is.element(major,1)]   = "4B"
		}
		else if (is.element(strSpp,c("396","440"))){
			if (!all(c("major_adj","major_old") %in% colnames(dat))) ## check to see if the majors have already been adjusted
				dat = expand5C(dat)  ## Special area adjustment for POP and YMR
			if (strSpp %in% "396"){
				newA[is.element(major,8:9)] = "5DE"
				newA[is.element(major,7)]   = "5C"
				newA[is.element(major,5:6)] = "5AB"
				newA[is.element(major,3:4)] = "3CD"
				newA[is.element(major,1)]   = "4B"
			}
			if (strSpp %in% "440"){
				newA[is.element(major,9)]   = "5E"
				newA[is.element(major,7:8)] = "5CD"
				newA[is.element(major,4:6)] = "3D5AB"
				newA[is.element(major,3)]   = "3C"
				newA[is.element(major,1)]   = "4B"
			}
		}
		else if (is.element(strSpp,c("405","437","621"))){
			newA[is.element(major,9)]   = "5E"
			newA[is.element(major,7:8)] = "5CD"
			newA[is.element(major,5:6)] = "5AB"
			newA[is.element(major,3:4)] = "3CD"
			newA[is.element(major,1)]   = "4B"
		}
		else if (is.element(strSpp,c("439"))){
			if (strSpp=="439" && !gmu){
				newA[is.element(major,8:9)] = "North"
				newA[is.element(major,3:7)] = "South"
			} else {
				newA[is.element(major,9)]   = "5E"
				newA[is.element(major,7:8)] = "5CD"
				newA[is.element(major,4:6)] = "3D5AB"
				newA[is.element(major,3)]   = "3C"
				newA[is.element(major,1)]   = "4B"
			}
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
			newA[is.element(major,3)]   = "3C"
			newA[is.element(major,4:9)] = "3D5"  ## all of area 5
			newA[is.element(major,1)]   = "4B"
		}
		else if (is.element(strSpp,c("042","044","224","225"))){
			newA[is.element(major,3:9)] = "OFFS"
			newA[is.element(major,1)]   = "GULF"
		}
		else if (is.element(strSpp,c("394","401","403","435","451","453","454","455","602","607","614"))){
			newA[is.element(major,c(1,3:9))] = "CST"
		}
		newA[is.element(major,10)]   = "AK"
		newA[is.element(major,11)]   = "BC"
		newA[is.element(major,22)]   = "OR"
		newA[is.element(major,23)]   = "CA"
		newA[is.element(major,24)]   = "MX"
		newA[is.element(major,40)]   = "RU"
		newA[is.element(major,45)]   = "PO" ## guess
		newA[is.element(major,50)]   = "JP"
		newA[is.element(major,61)]   = "1A"
		newA[is.element(major,62)]   = "1B"
		newA[is.element(major,63)]   = "1C"
		newA[is.element(major,64)]   = "2A"
		newA[is.element(major,65)]   = "2B"
		newA[is.element(major,66)]   = "2C"
		newA[is.element(major,67)]   = "3A"
		newA[is.element(major,68)]   = "4A"
	} else {
		## Allocations scheme not identified currently
		newA = rep("UNK", nrow(dat))
	}
	dat[,stockFld] = newA
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~calcStockArea


#calcWAParea----------------------------2016-07-28
# Assign a stock area designation for Walleye Pollock
# using PMFC major and minor areas, a stratifying vector,
# and a weighting vector.
#-----------------------------------------------RH
calcWAParea <- function (major, minor, strat, wts)
{
	stratify <- function (major,minor,strat,wts,targ.major,targ.minor) {
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
					pos1 = match(FALSE,TRUE.good[match(true.strat[[i]],names(TRUE.good))])
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


## calcSurficial------------------------2024-02-29
##  Calculate the intersection of surficial geology
##  and bathymetry interval object from calcHabitat()
## ---------------------------------------------RH
calcSurficial <- function(surf="qcb", hab,
		xlim=c(-133.4,-127.8), ylim=c(50.5,54.8),
		col.hab=c("aliceblue","grey"), col.cst="grey85", 
		pix=FALSE, wmf=FALSE, ioenv=.GlobalEnv)
{
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

	z   = is.element(pd$col,"white")
	leg = sapply(split(pd$label[!z],pd$col[!z]),unique)
	leg = leg[order(leg)]
	#legarea=paste(leg," (",format(round(area[leg]),big.mark=",",trim=TRUE)," km\262)",sep="")
	legarea = paste0(leg, " (", format(round(area[leg]), big.mark=",", trim=TRUE), " km", convUTF("\\u{\\u00B2"), ")")

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
	.addAxis(xlim=xlim,ylim=ylim,tckLab=FALSE,tck=0.014,tckMinor=.007)  ## .addAxis currently not exported from PBSmapping namespace
	box()
	if (pix|wmf) dev.off()
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~calcSurficial


## clarify------------------------------2024-10-24
##  Analyse catch proportions in blocks, then cluster into fisheries groups.
##  Initially, done to address the CASSIS proposal and its impact (John Pringle).
##  CASSIS = CAScadia SeISmic experiment
##  Note: log10 transform seems useful, but change catches from tonnes to kgs first (done internally)
## ---------------------------------------------RH
clarify <- function(dat, cell=c(0.1,0.075), nG=8, dSpp=3, Vmin=3, trfun, 
   xlim=c(-134.2,-123.2), ylim=c(47.8,54.8), zlim=NULL, targ="YMR", 
   clrs=c("red","orange","yellow","green","forestgreen","deepskyblue",
   "blue","midnightblue"), land="ghostwhite", spp.names=FALSE, outnam,
   png=FALSE, pngres=400, PIN=NULL, isobs=c(200,1000,1800), 
   wmf=FALSE, eps=FALSE, hpage=10, ioenv=.GlobalEnv, lang=c("e","f"))
{
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
	if (missing(outnam))
		fnam = as.character(substitute(dat))
	else
		fnam = outnam
	#expr=paste("getFile(",fnam,",senv=ioenv,use.pkg=TRUE,try.all.frames=TRUE,tenv=penv()); dat=",fnam,sep="") # input file made global cause it's big
	#eval(parse(text=expr))
	#clrs <- c("blue","magenta","orange","green","cyan","forestgreen","yellow","black","purple4","red") #PNCIMA
	#clrs <- c("red","orange","yellow","green","forestgreen","deepskyblue","blue","midnightblue")

	flds = colnames(dat)
	if ("EID" %in% flds) {
		is.map = TRUE
		spp = setdiff(flds,c("EID","X","Y","Z","tcat","date","major","minor","locality","vessel"))
	} else {
		is.map = FALSE
		spp = flds[-c(1:5)]
	}
	nr <- nrow(dat); nc <- ncol(dat[,spp])

	## Remove records (rows) with no catch
	dat$tcat <- apply(dat[,spp],1,sum,na.rm=TRUE)
	dat <- dat[dat$tcat>0 & !is.na(dat$tcat),]
	## Remove species (cols) with no catch (RH 240124)
	sppcat <- apply(dat[,spp],2,sum,na.rm=TRUE)
	subspp <- spp[sppcat>0]
	dat = dat[,c(setdiff(colnames(dat),c(spp,"tcat")), subspp, "tcat")]
#browser();return()
	## Try a transformation (RH 240124)
	if (!missing(trfun) && is.function(trfun)) {
		zerocat  = dat[,subspp]<=0 & !is.na(dat[,subspp])
		dat[,subspp] = dat[,subspp] * 1000. ## convert kgs to grams
		dat[,subspp][zerocat] = 1  ## add a gram (doesn't work when adding to kgs)
		dat[,subspp] = trfun(dat[,subspp])
	}

	if (is.map) {
		dat=dat[dat$X>=xlim[1] & dat$X<=xlim[2] & !is.na(dat$X),]
		dat=dat[dat$Y>=ylim[1] & dat$Y<=ylim[2] & !is.na(dat$Y),]
		if (!is.null(zlim))
			dat = dat[dat$Z>=zlim[1] & dat$Z<=zlim[2] & !is.na(dat$Z),]
		if (!is.element("EID", colnames(dat)))
			dat$EID = 1:nrow(dat)
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
		## Remove cells with less than Vmin vessels (RH 240124)
		Vcount = sapply(split(dat$vessel,dat$PID), function(x){length(unique(x))})  ## vessel count
		Vvalid = Vcount[Vcount >= Vmin]
#browser();return()
		dat    = dat[is.element(dat$PID, names(Vvalid)),]


		#PID <- sort(unique(events$PID)); nPID <- length(PID)
		nspp <- length(spp)
		tcat <- apply(dat[,subspp], 2, tcomp, i=dat$PID) # offload_port does not work here
		pcat <- t(apply(tcat,1,pcomp))
		ppos <- apply(pcat,2,function(x){length(x[x>0])/length(x)})
		z    <- ppos >= 0.05
		spp  <- names(ppos[z])
		psub <- pcat[,subspp]
#browser();return()

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
#browser();return()

		for (i in grps) {
			gout <- pout[is.element(pout$group,i),]
			sord <- rev(sort(apply(gout[,subspp],2,mean,na.rm=TRUE)))
			tpos <- match(targ,names(sord),nomatch=99)+i/100; #print(tpos)
			smat <- c(smat,list(sord))
			j    <- dimnames(gout)[[1]]
			col  <- clrs[gout$group]
			lab  <- paste(names(sord[1:dSpp]),collapse="-")
			skey <- c(skey,lab)
			pdata[j,"PID"]   <- as.numeric(j)
			pdata[j,"col"]   <- col
			pdata[j,"label"] <- rep(lab,length(j))
			pdata[j,"group"] <- rep(i,length(j))
			if (is.null(targ)) pdata[j,"target"] = pdata[j,"group"]
			else               pdata[j,"target"] = rep(tpos,length(j))
		}
		names(smat) <- grps
		gnum = sapply(split(pdata$PID,pdata$group),length)
		gtar = sapply(split(pdata$PID,pdata$target),length)
		if (is.null(targ)) gord = rev(order(gnum))
		else               gord = order(as.numeric(names(gtar))) # order by placement of target in clara group
		names(clrs)[1:length(gtar)]=names(gtar[gord])            # enforce colour order
		pdata$col = clrs[as.character(pdata$target)]             # repopulate pdata's col field
#browser();return()

		kcol = clrs[1:nG]
		glab = sapply(split(pdata$label,pdata$target),unique)
		kgrp = glab[gord]
		if (spp.names) {
			data("species", package="PBSdata", envir=penv()) # local
			kcode=strsplit(kgrp,split="-")
			kgrp=sapply(kcode,function(x){paste(gsub(" ",".",species[x,"name"]),collapse="+")})
		}

		if (is.map && !is.null(isobs)) {
			getFile(isobath, use.pkg=TRUE, tenv=penv())
			isob <- isobath[is.element(isobath$PID,isobs),]
		}
		stuff=c("tcat","pcat","ptree","pout","pdata","clrs","glab","kgrp")
		packList(stuff,"PBStool",tenv=.PBStoolEnv)
#browser();return()

		##--Plot-results---------------------------------------------
		## Try to automate based on par()$pid - plot must be ready on-screen first
		win = TRUE; plop = list(win=win, png=png, eps=eps, wmf=wmf)
		for (i in c("win","png","eps","wmf")) {
			ii = plop[[i]]
			if (ii) {
				if (is.null(PIN))
					PIN = par()$pin/max(par()$pin)*hpage

				createFdir(lang)
				fn = paste(fnam,ifelse(is.null(targ),"",paste0("-",targ)),"-nG",nG,"-(",paste(round(PIN,1),collapse="x"),")",sep="")
				fout.e = fn
				for (l in lang) {
					changeLangOpts(L=l)
					#fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
					fout = switch(l, 'e' = paste0("./english/",fout.e), 'f' = paste0("./french/",fout.e) )
					if (i=="png" && ii) {
						clearFiles(paste0(fout,".png"))
						png(filename=paste0(fout,".png"), units="in", res=pngres, width=PIN[1], height=PIN[2])
					} else if (i=="eps" && ii) {
						clearFiles(paste0(fout,".eps"))
						postscript(paste(fout,".eps",sep=""),width=PIN[1],height=PIN[2],paper="special")
					} else if (i=="wmf" && ii) {
						clearFiles(paste0(fout,".wmf"))
						do.call("win.metafile",list(filename=paste(fout,".wmf",sep=""),width=PIN[1],height=PIN[2]))
					}
					else resetGraph()
					getFile(nepacLL,use.pkg=TRUE,tenv=penv())
#browser();return()
					par(parlist)
					plotMap(nepacLL, xlim=xlim, ylim=ylim, plt=NULL, col="white", cex.axis=1, cex.lab=1.2, mgp=c(1.5,0.2,0))
					addPolys(agrid, polyProps=pdata, border=FALSE)
					if (!is.null(isobs)) {
						icols = colorRampPalette(c("steelblue4","navy"))(length(isobs))
						addLines(isob, col=icols)
					}
					addPolys(nepacLL, col=land)
					xoff=ifelse(wmf,-0.05,0); yoff=ifelse(wmf,-0.01,0)
					addLegend(0.02, 0.02, fill=kcol, legend=linguaFranca(kgrp,l), cex=ifelse(wmf,0.8,0.7), yjust=0, bty="n")
#browser();return()
					if (!is.null(targ)) 
						addLegend(0.35, 0.02, legend=floor(as.numeric(names(kgrp))), bty="n", cex=ifelse(wmf,0.8,0.7), adj=1, xjust=1, yjust=0, text.col="blue", title=linguaFranca(targ,l))
					.addAxis(xlim=xlim, ylim=ylim, tckLab=FALSE, tck=0.014, tckMinor=0.007)  ## .addAxis currently not exported from PBSmapping namespace
					box()
					if (i!="win" && ii) dev.off()
				}; eop()
			}
		}
	} else {
		## Hierarchical Clustering
		dmeth = "canberra"
		createFdir(lang)
		fn = paste0("offload-cluster-distance(", dmeth, ")")
		fout.e = fn
		for (l in lang) {
			changeLangOpts(L=l)
			#fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
			fout = switch(l, 'e' = paste0("./english/",fout.e), 'f' = paste0("./french/",fout.e) )
			if (png) {
				clearFiles(paste0(fout,".png"))
				png(filename=paste0(fout,".png"), units="in", res=pngres, width=12, height=4)
			}
			expandGraph(mfrow=c(1,4), mar=c(1,3.5,2,1), mgp=c(2,0.5,0))
			for (i in c("5E","5CD","5AB","3CD")) {
				idat <- dat[dat$pmfc_area %in% i,]
				nspp <- length(spp)
				tcat <- apply(idat[,subspp], 2, tcomp, i=idat$OID) # offload_port does not work here
				pcat <- t(apply(tcat,1,pcomp))
				ppos <- apply(pcat,2,function(x){length(x[x>0])/length(x)})
				z    <- ppos >= 0.05
				spp  <- names(ppos[z])
				psub <- pcat[,subspp]

				## options:"euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"
				d  = dist(t(psub), method=dmeth)
				hd = hclust(d)
				plot(hd, main=paste0(i, " -- distance method=", dmeth), cex.axis=1.25)
			}
			if (png) dev.off()
		}; eop()
	}
#browser();return()
	gc(verbose=FALSE)
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~clarify


## findEP-------------------------------2024-02-29
## Find events in polys and and add poly info to events
## ---------------------------------------------RH
findEP <- function(events, polys, maxRows=1e+08)
{
	if (!is.EventData(events))
		stop("Object 'events' must be an EvenData set (see PBSmapping help)")
	if (!is.PolySet(polys))
		stop("Object 'polys' must be a PolySet (see PBSmapping help)")

	pdata = attributes(polys)$PolyData
	if (is.null(pdata))
		stop("Object 'polys' must have a 'PolyData' attribute containg fields:\n\t'PID' and 'SID' along with some label identifier.")

	locflds   = intersect(c("major","minor","locality"), colnames(pdata))
	locflds   = intersect(locflds, colnames(events))
	events$ID = .createIDs(events, locflds)  ## .createIDs currently not exported from PBSmapping namespace
	pdata$ID  = .createIDs(pdata,  locflds)

	ldata = findPolys(events, polys, maxRows=maxRows, includeBdry=1) ## use only the first (lowest PID/SID) polygon boundary

	#zinsi = ldata$Bdry==0  ## event located inside a polygon
	#zbord = ldata$Bdry==1  ## event located on a polygon boundary (often between 2 polygons)
	#linsi = ldata[zinsi,]

	ldata$ID  = .createIDs(ldata,c("PID","SID"))
	eid = ldata$ID; names(eid) = ldata$EID
	pid = pdata$ID; names(pid) = .createIDs(pdata,c("PID","SID"))
	events$ID2 = pid[as.character(eid[as.character(events$EID)])]
	naID = is.na(events$ID2)
	events$ID2[naID] = events$ID[naID]  ## fill in the missing gaps
	return(events)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~findEP


## findHoles----------------------------2024-02-29
## Find holes and place them under correct parents.
## Can be a slow process -- set 'use.sp.pkg=TRUE' for faster execution
## ---------------------------------------------RH
findHoles <- function(polyset, minVerts=25, nlevs=1, use.sp.pkg=TRUE) 
{
	on.exit(gc(verbose=FALSE))
	## Subfunctions----------------------
	findHope <- function(X,Y,Z,use.sp=TRUE){
		znames = names(Z)
		slop = array(FALSE,dim=c(length(X),length(Y)),dimnames=list(hole=znames,solid=znames))
		for (i in X){
			for(j in Y) {
				if (i==j) next
				if (use.sp) {
					## Use Spatial Data (sp) function 'point.in.polygon'
					slop[i,j] = all(sp::point.in.polygon(
						point.x=Z[[i]]["X"][,1],
						point.y=Z[[i]]["Y"][,1],
						pol.x=Z[[j]]["X"][,1],
						pol.y=Z[[j]]["Y"][,1]))
				} else {
					## use PBSmapping function '.is.in' (but cannot use holes as standalone PolySets)
					iZ = Z[[i]]; jZ = Z[[j]]
					if (all(diff(iZ$POS)<0)) iZ$POS = rev(iZ$POS)
					if (all(diff(jZ$POS)<0)) jZ$POS = rev(jZ$POS)
					slop[i,j] = .is.in(iZ, jZ)$all.in
				}
			}
		}
		return(slop)
	}
	ammendPoly <- function(fswiss, pid, rawP, targetP) {
		## Go through swiss cheese polygons and put the holes in the right place
		for (j in 1:ncol(fswiss)) {
			jj  = colnames(fswiss)[j]
			ii  = names(fswiss[,jj][fswiss[,jj]])
			pid = pid + 1; sid = 1
			jswiss = rawP[[jj]]
			jswiss$PID = pid
			jswiss$SID = sid
			for (i in ii) {
				sid = sid + 1
				ihole = rawP[[i]]
				ihole$PID = pid
				ihole$SID = sid
				ihole$POS = nrow(ihole):1 ## reverse POS means hole in PBSmapping
				jswiss    = rbind(jswiss,ihole)
			}
			targetP = rbind(targetP,jswiss)
		}
		return(targetP)
	}
	##-------------------end Subfunctions

	if (nlevs>2) stop("Set 'nlevs' to 1 or 2")

	polyset$ID = .createIDs(polyset,c("PID","SID"))
	poly0 = split( polyset , f = polyset$ID )
	bad   = sapply(poly0,nrow) < minVerts
	polyG = poly0[!bad]
	npoly = length(polyG)

	#cents = sapply(polyG,function(x){calcCentroid(x,rollup=1)[c("X","Y")]},simplify=FALSE)
	HinP = findHope(X=1:npoly, Y=1:npoly, Z=polyG, use.sp=use.sp.pkg)

	## Cols = solids, Rows = holes
	sumC  = apply(HinP,2,sum)
	sumR  = apply(HinP,1,sum)
	allGs = names(polyG)

	## Sum(Cols)==0 and Sum(Rows)==0 are solids without holes
	solidGs = allGs[sumC==0 & sumR==0]

	## Determine polygons with holes in them
	swissG = sumC[sumC>0]
	swissH = sumR[sumR>0]
	swissP = HinP[names(swissH),names(swissG)]
	if (is.vector(swissP)) { ## test whether swissP should be a 1-row matrix (RH 230727)
		swissP = array(swissP, dim=c(length(swissG), length(swissH)), dimnames=list(hole=names(swissG), solid=names(swissH)))
	}
	holes  = try(apply(swissP,1,sum), silent=TRUE)
	if (inherits(holes,"try-error")) {
		browser();return()
	}

	## Shapes likey to be holes in solids (e.g. lakes on land or islands in the ocean)
	aholes = names(holes)[holes==1]
	if (length(aholes) > 0) {
		swissA  = swissP[aholes,,drop=FALSE]
		solids  = apply(swissA,2,sum)
		asolids = names(solids)[solids>0]
		swissA  = swissA[,asolids,drop=FALSE]
	} else {
		swissA = NULL
	}
	## Shapes likely to also be solids in holes (e.g. islands in lakes on land or lakes on islands in the ocean)
	## Preliminary tests suggest that using 'bholes' (nlevs=2) is redundant (already detected at nlevs=1)
	bholes = names(holes)[holes==2]
	if (length(bholes) > 0) {
		swissB  = swissP[bholes,,drop=FALSE]
		solids  = apply(swissB,2,sum)
		bsolids = names(solids)[solids>0]
		swissB  = swissB[,bsolids,drop=FALSE]
		## If swissB solids occur in swissA, transfer them to swissA
		if (!is.null(swissA)) {
			inAB = intersect(asolids,bsolids)
			if (length(inAB)>0) {
				swissA = swissP[.su(c(aholes,bholes)),asolids,drop=FALSE]
				swissB = swissB[,setdiff(bsolids,asolids),drop=FALSE]
			}
		}
	} else {
		swissB = NULL
	}
	## Start building the final polygon
	rnames = names(polyset)
	if (length(solidGs) > 0) {
		## https://stat.ethz.ch/pipermail/r-help/2008-March/156337.html
		lis   = lapply(polyG[solidGs], "names<-", value = rnames)
		slis  = lapply(1:length(lis),function(x){xlis=lis[[x]]; xlis$PID=x; xlis$SID=1; xlis})
		pid   = length(slis)
		polyF = do.call("rbind", lapply(slis, data.frame, stringsAsFactors = FALSE))
	} else {
		pid = 0
		polyF = list()
	}
	if (!is.null(swissA)) {
		pid.last = max(polyF$PID)
		polyF = ammendPoly(swissA, pid=pid.last, rawP=polyG, targetP=polyF)
	}
	## Polygons in swissB have basically been picked up in swissA and here appear as reverse holes
	if (nlevs==2 && !is.null(swissB)) {
		pid.last = max(polyF$PID)
		## Transpose 'swissB' because at nlevs=2, solids are holes and holes are solids.
		polyFa = ammendPoly(t(swissB), pid=pid.last, rawP=polyG, targetP=polyF)
	}
	polyF = as.PolySet(polyF,projection="LL")
	packList(c("HinP","solidGs","aholes","bholes","swissA","swissB"), target="obj.findHoles", tenv=.PBStoolEnv)
	return(polyF) 
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~findHoles


## plotConcur---------------------------2024-10-24
## Horizontal barplot of concurrent species in tows.
## ---------------------------------------------RH
plotConcur <- function(strSpp="410", dbName="GFFOS", spath=.getSpath(),
   mindep=150, maxdep=435, major=NULL, minor=NULL, top=NULL, gear=1,
   saraSpp=c("027","034","394","410","424","435","437","440","442","453"),
   reset.mf=TRUE, eps=FALSE, png=FALSE, pngres=300, colour="topo", 
   print.tab=TRUE, run.sql=TRUE, outnam, lang=c("e","f"))
{
	assign("PBStool",list(module="M03_Fishery",call=match.call(),args=args(plotConcur),outnam="Concur"),envir=.PBStoolEnv)
	data("species", package="PBSdata", envir=penv())
	zspp=species$name!=species$latin
	species$name[zspp] = toUpper(species$name[zspp])

	## If dummy is not specified it defaults to '' which is tranlsated as minor=0 and unwanted records are collected
	if (is.null(minor)) minor = 999 ## doesn't exist -- used to fool SQL

	if (missing(outnam)) {
		outnam <- paste("Concur",strSpp,dbName,paste0(c("Btrawl","Mtrawl","HookLine","Trap")[gear],collapse="+"),sep="-")
		outnam <- paste0(outnam, "-d(", mindep, "-", maxdep, ")")
		if (!is.null(minor) && !all(minor==999)) outnam = sub(strSpp,paste(strSpp,"-minor(",paste(minor,collapse=""),")",sep=""),outnam)
		if (!is.null(major)) outnam = sub(strSpp,paste(strSpp,"-major(",paste(major,collapse=""),")",sep=""),outnam)
	}
#browser();return()

	if (run.sql && dbName=="GFFOS")
		getData("fos_concurrent.sql", "GFFOS", strSpp, path=spath,
			mindep=mindep, maxdep=maxdep, major=major, dummy=minor, top=top, gear=gear, tenv=penv())
	else if (run.sql && dbName=="PacHarvest")
		getData("pht_concurrent.sql","PacHarvest", strSpp, path=spath,
			mindep=mindep,maxdep=maxdep,major=major,dummy=minor,top=top,gear=gear,tenv=penv())
	else if (run.sql && dbName=="PacHarvHL")
		getData("phhl_concurrent.sql","PacHarvHL",strSpp,path=spath,
			mindep=mindep,maxdep=maxdep,major=major,dummy=minor,top=top,tenv=penv())
	else {
		## Assume CSV file exists from previous query
		if (!run.sql) dbName =outnam
		if (file.exists(paste0(dbName,".csv"))) {
			PBSdat = read.csv(paste0(dbName,".csv"), check.names=FALSE)
			print.tab = FALSE
		} else
			showError("SQL choices for 'dbName' are 'GFFOS', 'PacHarvest' or 'PacHarvHL'\n\tor supply name of a local CSV file")
	}
	dat = PBSdat
	sql = attributes(PBSdat)$sql

	dat$spp   = as.character(dat$spp)
	dat$code  = pad0(dat$code,3)
	dat$latin = species[dat$code,"latin"]
	dat  = dat[,c("code","spp","latin","catKt","pct")]
	dat  = dat[rev(order(dat$pct)),]
	dat$spp = toUpper(dat$spp)

	## Re-format the same table so that it's latex-ready
	if (print.tab) {
		write.csv(dat,paste(outnam,".csv",sep=""),row.names=FALSE)
		textab = dat
		textab$code  = pad0(textab$code,3)
		#textab$spp   = toUpper(textab$spp)
		textab$latin = paste("\\emph{",textab$latin,"}",sep="")
		textab$catKt = format(round(textab$catKt * 1000.),big.mark=",",trim=TRUE)
		textab$pct   = show0(round(textab$pct,3),3)
		names(textab)=c("Code","Species","Latin name","Catch (t)","Catch (\\%)")
		write.csv(textab,paste("tex-",outnam,".csv",sep=""),row.names=FALSE)
		## Note: to read the table back into R use 'read.csv("xyz.csv",check.names=FALSE)'
	}

	##----- Plotting -----
	dat  = dat[order(dat$pct),] # for plotting as horizontal bars w/ largest at top
	fout.e = outnam
	for (l in lang) {
		changeLangOpts(L=l)
		#fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		fout = switch(l, 'e' = paste0("./english/",fout.e), 'f' = paste0("./french/",fout.e) )
		if (eps) {
			clearFiles(paste0(fout,".eps"))
			postscript(file=paste0(fout,".eps"),width=10,height=6,horizontal=FALSE,paper="special")
			par(mfrow=c(1,1),cex=0.8,mar=c(3,12,0.5,1),oma=c(0,0,0,0),mgp=c(1.5,.5,0))
		} else if (png) {
			clearFiles(paste0(fout,".png"))
			png(filename=paste0(fout,".png"), units="in", res=pngres, width=6.5, height=3.5)
			par(mfrow=c(1,1), cex=0.7, mar=c(3,switch(l,'e'=10,'f'=12),0.5,1), oma=c(0,0,0,0), mgp=c(1.5,0.5,0))
		} else if (reset.mf)
			expandGraph(mfrow=c(1,1), cex=1.0, mar=c(4,switch(l,'e'=10,'f'=12),1,1), oma=c(0,0,0,0), mgp=c(2,.5,0))
		else
			expandGraph(cex=1.0,mar=c(4,switch(l,'e'=10,'f'=12),1,1),mgp=c(2,.5,0))
		if (colour=="topo")
			barcol = topo.colors(nrow(dat))
		if (colour=="sombre")
			barcol = rep("gainsboro",nrow(dat))
		xy <- barplot(dat$pct, col=barcol, names.arg=linguaFranca(dat$spp,l), horiz=TRUE, las=1, col.axis="grey30", xlab=switch(l,'e'="Percent",'f'="Pourcentage"), cex.axis=0.8)
		z <- xy[match(species[saraSpp,"name"],dat$spp)]
		zbar = is.element(dat$spp,species[saraSpp,"name"])
		if (any(!is.na(z))) {
			axis(side=2, at=z, las=1, tick=FALSE, labels=linguaFranca(species[saraSpp,"name"],l), col.axis="red")
			if (!is.element(colour,c("topo")) && !is.null(colour)) {
				barcol[zbar] = "pink"
				barplot(dat$pct, col=barcol, names.arg=NULL, horiz=TRUE, las=1,col.axis="grey20", xaxt="n", yaxt="n", add=TRUE)
			}
		}
		z <- xy[match(species[strSpp,"name"],dat$spp)]
		zbar = is.element(dat$spp,species[strSpp,"name"])
		if (!is.na(z)) {
			axis(side=2, at=z, las=1, tick=FALSE, labels=linguaFranca(species[strSpp,"name"],l), col.axis="blue")
			if (!is.element(colour,c("topo")) && !is.null(colour)) {
				barcol[zbar] = "lightblue1"
				barplot(dat$pct, col=barcol, names.arg=NULL, horiz=TRUE, las=1,col.axis="grey20", xaxt="n", yaxt="n", add=TRUE)
			}
		}
		if (l=="f") {
			addLabel(0.90,0.20,bquote(Sigma~Prise~(kt) == .(round(sum(dat$catKt)))), cex=1.2, adj=1)
			addLabel(0.90,0.15,bquote(Sigma~Prise~('%') == .(round(sum(dat$pct),1))), cex=1.2, adj=1)
		} else {
			addLabel(0.90,0.20,bquote(Sigma~Catch(kt) == .(round(sum(dat$catKt)))), cex=1.2, adj=1)
			addLabel(0.90,0.15,bquote(Sigma~Catch('%') == .(round(sum(dat$pct),1))), cex=1.2, adj=1)
		}
		if (eps|png) dev.off()
	}; eop()
	packList(c("dat","xy","z","sql"),"obj.preferDepth",tenv=.PBStoolEnv)
	invisible(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotConcur


## plotEO-------------------------------2024-10-24
## Plot the Extent of Occurrence for a species 
## using a convex hull to surround events.
## ---------------------------------------------RH
plotEO <- function (id="lst", strSpp="453", col="red", 
   xlim=c(-136,-122.5), ylim=c(48,54.8), prefix="map", 
   inarea=NULL, exarea=NULL, areafld="PMFC",
   exland=FALSE, rmXY=FALSE, rmSM=TRUE,
   png=FALSE, pngres=400, PIN =c(9,7.3), lang=c("e","f"))
{
	old.warn=options()$warn
	on.exit(options(warn=old.warn))

	fenv = lenv()  ## function environment
	if (!exists(paste0(prefix,strSpp), envir=.PBStoolEnv)) {
		if (!file.exists(paste0(prefix,strSpp,".rda"))){
			getData("fos_map_density.sql", dbName="GFFOS", strSpp=strSpp, path=.getSpath(), tenv=fenv)
			mapDat = as.EventData(data.frame(EID=1:nrow(PBSdat), PBSdat, check.names=FALSE), projection="LL")
			assign(paste0(prefix,strSpp), mapDat, envir=.PBStoolEnv)
			save(list=paste0(prefix,strSpp), file = paste0(prefix,strSpp,".rda"), envir=.PBStoolEnv)
		} else {
			load(paste0(prefix,strSpp,".rda"))
			assign("mapDat", get(paste0(prefix,strSpp)))
		}
	} else {
		assign("mapDat", get(paste0(prefix,strSpp), envir=.PBStoolEnv))
	}
	if (is.null(ttcall("hulk")))
		hulk = list()
	else ttget(hulk)
	data("nepacLL", package="PBSmapping", envir=fenv)
	data("species", package="PBSdata", envir=fenv)

	mapdat = mapDat
	mapdat = mapdat[mapdat[,strSpp]>0 & !is.na(mapdat[,strSpp]),]
	mapdat  = mapdat[mapdat$X >= xlim[1] & mapdat$X <= xlim[2] & !is.na(mapdat$X) &
	                 mapdat$Y >= ylim[1] & mapdat$Y <= ylim[2] & !is.na(mapdat$Y),]

	## Remove seamounts
	if (rmSM)
		mapdat = zapSeamounts(mapdat)
	## Remove coordinates that are located in localities that don't match the reported localities
	## Need to fiddle for POP|YMR which have had majors from 9 and 6 reassigned to 7 by function 'expand5C'
	if (rmXY) { # && !is.element(strSpp,c("396","440")) ) {
		isPOP = is.element(strSpp,c("396","440"))
		data(locality.plus, package="PBSdata", envir=fenv)
		loca = locality.plus
		mapdat$ID = .createIDs(mapdat, c(ifelse(isPOP,"major_old","major"), "minor", "locality"))
		inlocs = findPolys(mapdat, loca, maxRows=1e7)
		pdata  = attributes(loca)$PolyData
		inlocs$PS = .createIDs(inlocs, c("PID","SID"))
		pdata$PS  = .createIDs(pdata,  c("PID","SID"))
		pkey = pdata$ID; names(pkey) = pdata$PS
		inlocs$ID = pkey[as.character(inlocs$PS)]
		ikey = inlocs$ID; names(ikey) = inlocs$EID
		mapdat$IDloc = rep("",nrow(mapdat))
		mapdat$IDloc = ikey[as.character(mapdat$EID)]
		zloc = (mapdat$ID==mapdat$IDloc) & !is.na(mapdat$IDloc)
		mapdat = mapdat[zloc,]
#browser();return()
	}
	if (is.element(strSpp,"228")) {
		sub12 = is.element(mapdat$minor, 12)
		sub20 = is.element(mapdat$minor, 20)
		mapdat[,areafld][sub12] = "5A"
		mapdat[,areafld][sub20] = "3C"
	}
	
	## New code for multiple stocks (RH 230726)
	#if (!is.null(inarea))
	#mapdat = mapdat[is.element(mapdat[,areafld],inarea),]

	stocks = list()
	if (is.null(inarea)) {
		stocks[["BC"]] = mapdat
	} else {
		for (i in 1:length(inarea)){
			ii = names(inarea)[i]
			stocks[[ii]] = mapdat[is.element(mapdat[,areafld],inarea[[ii]]),]
		}
	}
	#if (!is.null(exarea))
	#	mapdat = mapdat[!is.element(mapdat[,areafld],exarea),]
	if (!is.null(exarea)) {
		for (i in 1:length(exarea)){
			ii = names(exarea)[i]
			tmpdat = stocks[[ii]]
			stocks[[ii]] = tmpdat[!is.element(tmpdat[,areafld],exarea[[ii]]),]
		}
	}

	land   = clipPolys(nepacLL,xlim=xlim, ylim=ylim)
	if (exland) { ## exclude points on land (takes a long time)
		inland = findPolys(mapdat, land)
		mapdat = mapdat[!is.element(mapdat$EID,.su(inland$EID)),]
	}
	attr(mapdat,"projection") = attributes(mapDat)$projection

	options(warn=-1)  ## Turn warnings off for area calc functions
	for (i in 1:length(stocks)) {
		ii = names(stocks)[i]
		.flush.cat("Calculating hull for stock", ii, "\n")
		iii = id[i]
		tmpdat = stocks[[ii]]
		hull = calcConvexHull(tmpdat)
		# table(mapdat$year)  ## determine which years contribute to the hull
		hulk[[iii]][["hull"]]            = hull
		hulk[[iii]][["area.hull"]]       = calcArea(hull)
		hull.less.land                   = joinPolys(hull, land, operation="DIFF")
		hulk[[iii]][["area.hull.water"]] = sum(calcArea(hull.less.land, rollup=2)$area)
		hulk[[iii]][["date.range"]]      = range(tmpdat$date, na.rm=TRUE)
	}
	options(warn=old.warn)
	ttput(hulk)

	big.mark = ifelse(is.null(options()$big.mark), ",", options()$big.mark)
	arealab  = paste0(format(round(sapply(hulk[id], function(x){ x[["area.hull"]][["area"]] }),0), big.mark=big.mark), collapse=", ")
	arealab  = paste0("Convex hull area = ", arealab, " km", convUTF("\\u{00B2}") )
	waterlab = paste0(format(round(sapply(hulk[id], function(x){ x[["area.hull.water"]] }),0), big.mark=big.mark), collapse=", ")
	waterlab = paste0("    (hull on water = ", waterlab, " km", convUTF("\\u{00B2}"), ")" )
	stocklab = ifelse(is.null(inarea),"BC",paste0(names(inarea),collapse=", "))
	datelab  = paste0(paste0(names(stocks), ": ", sapply(hulk[id], function(x){paste0(x[["date.range"]],collapse=" to ")})), collapse="\n")
	spplab   = paste0(c(
		paste0(toUpper(species[strSpp,"name"]), " -- ", stocklab),
		paste0("    (", species[strSpp,"latin"], ")"),
		arealab,
		waterlab
		), collapse="\n")
#browser();return()
	spplab.f  = paste0(c(
		linguaFranca(paste0(toUpper(species[strSpp,"name"]), " -- ", stocklab),"f"),
		paste0("    (", species[strSpp,"latin"], ")"),
		gsub(",([0-9])"," \\1",sub("Convex hull area","zone de coque convexe",arealab)),
		gsub(",([0-9])"," \\1",sub("hull on water","coque sur l'eau",waterlab))
		), collapse="\n")

	fout.e = paste0("plotEO-",strSpp,"-", gsub("\\s+","",stocklab))
#browser();return()
	for (l in lang) {
		changeLangOpts(L=l)
		#fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		fout = switch(l, 'e' = paste0("./english/",fout.e), 'f' = paste0("./french/",fout.e) )
		if (png) {
			clearFiles(paste0(fout,".png"))
			png(paste0(fout,".png"), units="in", res=pngres, width=PIN[1], height=PIN[2])
		}
		plotMap(nepacLL, xlim=xlim, ylim=ylim, plt=c(0.07,0.99,0.08,0.99), cex.axis=1.5, cex.lab=1.75, las=1)
		for (i in 1:length(stocks)) {
			ii = names(stocks)[i]
			iii = id[i]
			tmpdat = stocks[[ii]]
			addPoints(tmpdat, pch=20, col=col[i], cex=1)
			addPolys(nepacLL, col="grey95", border="slategray")
			addPolys(hulk[[iii]][["hull"]], col=lucent(col[i],0.1))
		}

		addLabel(0.8,0.8, linguaFranca("BC",l), cex=5, col="grey80")
		addLabel(0.05, 0.15, switch(l, 'e'=spplab, 'f'=spplab.f), cex=1.25, col="black", adj=0)
		addLabel(0.975, 0.975, linguaFranca(datelab,l), cex=0.9, col="black", adj=c(1,1))
		if (png) dev.off()
	}; eop()
#browser();return()

	fidnam = c("trawl","halibut","sable","dogling","hlrock"); names(fidnam) = 1:5
	#events = rep(0,5); names(events) = fidnam
	#edata  = table(mapdat$fid); names(edata) = fidnam[names(edata)]
	#events[names(edata)] = edata
	events = array(0, dim=c(length(stocks),length(fidnam)), dimnames=list(stock=names(stocks),fid=fidnam) )
	edata  = t(sapply(names(stocks), function(i) { table(stocks[[i]]$fid) }))
	colnames(edata) = fidnam[colnames(edata)]
	events[rownames(edata),colnames(edata)] = edata
	colnames(events) = paste0("events_",colnames(events))

	#out = data.frame(area.hull, water=round(area.hull.water), t(events), strSpp=strSpp, stock=stocklab)
	out = data.frame(
		stock      = names(stocks),
		area_hull  = round(sapply(hulk[id],function(x){x[["area.hull"]][["area"]]}),0),
		area_water = round(sapply(hulk[id],function(x){x[["area.hull.water"]]}),0),
		events
	)
	#hulk[[id]][["out"]] = out
	#ttput(hulk)
	#for (i in 1:length(hulk)) {
	#	if (i==1) hulltab = hulk[[i]][["out"]]
	#	else hulltab = rbind(hulltab,hulk[[i]][["out"]])
	#}
	#write.csv(hulltab,"hulltab.csv", row.names=FALSE)
	write.csv(out,"hulltab.csv", row.names=TRUE)
	return(out)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotEO


## plotGMA------------------------------2024-10-24
##  Plot the Groundfish Management Areas
## ---------------------------------------------RH
plotGMA <- function(gma=gma, major=major, xlim=c(-134,-123), ylim=c(48.05,54.95), 
   eps=FALSE, png=FALSE, extra.labels=NULL, isobath, strSpp,
   PIN=c(9,9), pngres=400, lang=c("e","f"))
{
	oldpar=par(no.readonly=-TRUE); on.exit(par(oldpar))

	## Create a subdirectory called `french' for French-language figures
	createFdir(lang)

	if (!is.null(extra.labels) && mode(extra.labels)=="character" && extra.labels[1]=="default") {
		extra.labels = as.EventData(data.frame(
			EID = 1:6,
			#X   = c(-132.3628,-130.0393,-129.1727,-126.3594),
			#Y   = c(53.74724,51.48773,51.05771,50.15860),
			#label = c("Haida\nGwaii","QCS","GIG","Vancouver\nIsland") ),
			X   = c(-132.3628,-126.3594,-129.6393,-129.4904,-129.9202,-130.4673),
			Y   = c(53.74724,50.15860,51.4,51.14122,51.70878,52.00878),
			label = c("Haida\nGwaii","Vancouver\nIsland","Queen Charlotte Sound","GIG","MIG","MRG") ),
			projection="LL" )
		if (!missing(strSpp) && strSpp%in%c("228"))
			extra.labels = extra.labels[!is.element(extra.labels$label,c("Queen Charlotte Sound","GIG","MIG","MRG")),]
	}
	fnam = as.character(substitute(gma))
	if (!missing(strSpp))
		fnam = paste0(fnam,strSpp)
	data("nepacLLhigh", package="PBSmapping", envir=penv())
	#data("major", "minor", package="PBSdata", envir=penv())
	data("minor", package="PBSdata", envir=penv())
	edata = pdata = attributes(gma)$PolyData
	names(edata)[1]="EID"
	edata=as.EventData(edata,projection="LL")

	fout.e = fnam
	for (l in lang) {  ## could switch to other languages if available in 'linguaFranca'.
		changeLangOpts(L=l)
		#fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		fout = switch(l, 'e' = paste0("./english/",fout.e), 'f' = paste0("./french/",fout.e) )

		if (png) {
			clearFiles(paste(fout,"png",sep="."))
			png(filename=paste(fout,"png",sep="."), width=PIN[1], height=PIN[2], units="in", res=pngres)
		}
		else if (eps) postscript(file=paste0(fout,".eps"), width=PIN[1], height=PIN[2], paper="special", horizontal=FALSE)
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
		} else {
			earlgrey = "grey"
			addPolys(gma, polyProps=pdata, border=earlgrey)
			addPolys(major,col="transparent",border="navyblue",lwd=2)
		}
		if (!missing(isobath) && is.PolySet(isobath))
			addLines(isobath)
		addPolys(nepacLLhigh,col="white",border=earlgrey)
		.addAxis(par()$usr[1:2],ylim=par()$usr[3:4],tckLab=FALSE,tck=.015,tckMinor=.015/2)  ## .addAxis currently not exported from PBSmapping namespace
		addLabels(edata,cex=2.5,font=2)
		if (!is.null(extra.labels)) {
			elabs = extra.labels
			elab = linguaFranca(gsub("\\\n"," ",elabs$label),l)
			touche = rep(TRUE,length(elab))
			if (!strSpp%in%c("sumting"))
				touche = !is.element(elab, "Queen Charlotte Sound")
			elab[touche] = gsub("le\\\nde","le de",gsub("\\\nde\\\nla"," de la",gsub(" ","\n",elab[touche])))
			elabs$label = elab
			addLabels(elabs,cex=1.5,col="blue")
#browser();return()
		}
		text(-125.2416,52.70739,linguaFranca("BC",l),cex=5,col="grey",font=2)
		box(lwd=2)
	#browser();return()
		if (eps|png) dev.off()
	}; eop()
	gc(verbose=FALSE)
	invisible(list(pdata=pdata,extra.labels=extra.labels))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotGMA


## plotHabitat--------------------------2024-10-24
## WAP (bctopo only reaches -124.5, bctopo2 goes to -122.5)
## hab228=calcHabitat(topofile="bctopo2",isob=c(62,447),digits=2,xlim=c(-134.5,-122.5))
## save("hab228",file="hab228.rda")
## ---------------------------------------------RH
plotHabitat <- function(fnam="hab228", isob=c(62,448), col.hab="greenyellow",
   major.code, col.maj="orange", lab.maj="0AAA",
   png=FALSE, pngres=400, PIN = c(9,8.7),
   fout="daHood", lang=c("e","f"))
{
	##PIN = c(9,8.7) ## for bctopo2; PIN = c(7.8,8.8) ## for bctopo
	data("eez.bc", "major","major.pop", package="PBSdata", envir=penv())
	data("nepacLL", package="PBSmapping", envir=penv())
	eval(parse(text=paste0("load(\"",fnam,".rda\"); habSpp = ",fnam)))
	Harea = sum(calcArea(habSpp)$area)
	habSpp.eez = joinPolys(eez.bc,habSpp,operation="INT")
	harea = sum(calcArea(habSpp.eez)$area)

	## Attempt extract species code from fnam
	strSpp = substring(gsub("\\D","",fnam),1,3)
	major  = if(strSpp %in% c("396","440")) major.pop else major
	if (!missing(major.code)) {
		major.sub = major[is.element(major$PID, major.code),]
		habSpp.major = joinPolys(major.sub, habSpp.eez, operation="INT")
		harea = sum(calcArea(habSpp.major)$area)
	}
#browser();return()

	xlim = range(habSpp$X); ylim = range(habSpp$Y)

	fout.e = fout
	for (l in lang) {
		changeLangOpts(L=l)
		#fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		fout = switch(l, 'e' = paste0("./english/",fout.e), 'f' = paste0("./french/",fout.e) )
		if (png) {
			clearFiles(paste0(fout,".png"))
			png(paste0(fout,".png"), width=PIN[1], height=PIN[2], units="in", res=pngres) 
		}
		expandGraph(mar=c(3,3.2,0.5,0.5), mgp=c(3.0,0.5,0))
		plotMap(eez.bc, xlim=xlim, ylim=ylim, plt=NULL, lwd=2, border=FALSE, col="aliceblue", cex.axis=1.5, cex.lab=2, las=1)
		addPolys(habSpp,col=col.hab,border="black")
		if (!missing(major.code))
			addPolys(habSpp.major, col=col.maj, border="black")
#browser();return()
		addPolys(major, col="transparent",border="red")
		addPolys(eez.bc, col="transparent", border="blue",lwd=2)
		addPolys(nepacLL,col="moccasin",border="grey40")
		tit.txt = switch(l, 'e'="Bathymetry habitat in BC EEZ:", 'f'=eval(parse(text=deparse("habitat de bathym\u{00E9}trie dans la ZEE de la C-B"))))
		if (!missing(major.code))
			tit.txt = sub(":", paste0(" (", lab.maj, "):"), tit.txt)
		leg.txt = paste0(isob[1], convUTF("\\u{2013}"), isob[2], " m  (", format(round(harea),big.mark=options()$big.mark), "km", convUTF("\\u{00B2}"), ")")
		addLegend(switch(l, 'e'=0.02, 'f'=0.02), 0.25, fill=ifelse(missing(major.code),col.hab,col.maj), title.adj=0, title=tit.txt, legend=leg.txt, bty="n", cex=1.25)
		box(lwd=2)
		if (png) dev.off()
	}; eop()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotHabitat


## plotLocal----------------------------2024-10-24
## Plot DFO fishing localities with the highest catch.
## ---------------------------------------------RH
plotLocal <- function(dat, area, aflds=NULL, pcat=0.95, cpue=FALSE, powr=1,
   useLL=FALSE, showAll=FALSE, xlim=c(-136,-122.5), ylim=c(48,54.8),
   fid=NULL, fidtype="PBStools", strSpp, Ntop=5, years, short=TRUE,
   plot=TRUE, png=FALSE, pngres=400, PIN=c(9,7.3),
   csv=FALSE, outnam="refA439", lang=c("e","f"))
{
	## Create a subdirectory called `french' for French-language figures
	createFdir(lang)

	fenv = lenv()  ## function environment
	datnam = as.character(substitute(dat))

	if (!missing(years))
		dat = dat[is.element(as.numeric(substring(dat$date,1,4)),years),]
	xbnd = xlim+diff(xlim)*0.02*c(1,-1) ## outer X boundaries for polygon labels
	ybnd = ylim+diff(ylim)*0.02*c(1,-1) ## outer Y boundaries for polygon labels


	pdata = attributes(area)$PolyData
	if (is.null(pdata))
		stop("Object 'polys' must have a 'PolyData' attribute containg fields:\n\t'PID' and 'SID' along with some label identifier.")
	#if (!is.element("ID",colnames(pdata)))
	#	pdata$ID = .createIDs(pdata,aflds)

	## User might prefer to find events in are polygons rather than
	## rely on combinations of major-minor-locality
	if (useLL) {
		dat.orig = dat
		dat = findEP(dat, area)  ## new function (RH 190121)
		dat$ID = dat$ID2
	} else {
		## mirror routine in 'findEP'
		locflds  = intersect(c("major","minor","locality"), colnames(pdata))
		locflds  = intersect(locflds, colnames(dat))
		dat$ID   = .createIDs(dat, locflds)
		pdata$ID = .createIDs(pdata,  locflds)
	}

	if (grepl("ph[[:alnum:]]|gfm", substring(datnam,1,3)))
		dat$catKG = dat$landed + dat$discard
	if (substring(datnam,1,3) %in% c("map","hab","eve")) {
		if (missing(strSpp))
			showError(paste0("Specify catch field in '",datnam,"'"))
		if (!is.element(strSpp,colnames(dat)))
			showError(paste0("User-specified catch field '", strSpp,"' does not occur in '",datnam,"'"))
		dat$catKG = dat[,strSpp]
	}
	if (!is.element("catKG",colnames(dat)))
		showError("Field 'catKG' not assigned; check 'datnam' in code")

	if (cpue){
		if(any(is.element(c("cpue","eff"),colnames(dat)))) {
			if (is.element("cpue",colnames(dat))) {
				dat = dat[dat$cpue>=0 & !is.na(dat$cpue),]
				dat$catKG = cat$cpue
			} else {
				dat = dat[dat$eff>0 & !is.na(dat$eff),]
				dat$catKG = dat$catKG/(dat$eff/60)
			}
		} else
			showError("No fields to use for CPUE")
	} else
		dat = dat[dat$catKG > 0 & !is.na(dat$catKG),]  ## catch
	dat$catKG = dat$catKG

	if (!missing(strSpp))
		data("species", package="PBSdata", envir=fenv)
	if (strSpp=="394")
		sppnam = "Rougheye/Blackspotted Rockfish"
	else
		sppnam = toUpper(species[strSpp,"name"])
	if (fidtype=="PBStools") {
		FID = 1:6
		if (short)
			names(FID) = c("T","H","S","DL","HL","C") ## short-name defaults
		else
			names(FID) = c("Trawl","Halibut","Sablefish","Dogfish.Lingcod","HL.Rockfish","Combined") ## long-name defaults
		dat$fid[is.element(dat$fid,9)] = 1       ## put FOREIGN sector catch into trawl
	} else if (fidtype=="GFFOS") {
		getData("FISHERY",dbName="GFFOS")
		FID = PBSdat$FISHERY_CODE
		names(FID) = PBSdat$FISHERY_NAME
	} else {
		stop ("fidtype specified is not recognised")
	}
	if (is.null(fid)) {
		dat.fid.old = dat$fid
		dat$fid = rep(6,nrow(dat))
		#fid = .su(dat$fid)
	} else {
		dat = dat[is.element(dat$fid,fid),]
		if (nrow(dat)==0) stop ("No records with specified fid")
	}
	if (is.null(fid))
		fid = 6
	fid =  FID[match(fid,FID,nomatch=0)]

	data("nepacLL", package="PBSmapping", envir=fenv)
#browser();return()

	#paint = colorRampPalette(c("lightblue1","green","yellow","red"))(500)
	paint = colorRampPalette(c("aliceblue","lightblue1","green",rep("yellow",2),"orange","red"))(500)
	FDATA = YRCAT = list()

	for (f in fid) {
		ff   = names(fid[match(f,fid)])
		fdat = dat[is.element(dat$fid,f),]
		if (nrow(fdat)==0) {
			showMessage(paste0("No positive catch data for fid=",f))
			next
		}
		if (cpue) {
			loccat = rev(sort(sapply(split(fdat$catKG,fdat$ID),function(x){mean(x)})))
			yrcat  = sapply(split(fdat$catKG,as.numeric(substring(fdat$date,1,4))),function(x){mean(x)})
		} else {
			loccat = rev(sort(sapply(split(fdat$catKG,fdat$ID),function(x){sum(x)/1000.})))
			yrcat  = sapply(split(fdat$catKG,as.numeric(substring(fdat$date,1,4))),function(x){sum(x)/1000.})
		}
		YRCAT[[as.character(f)]] = yrcat
		procat = loccat/sum(loccat)
		cumcat = cumsum(procat)
		bigcat = loccat[cumcat <= pcat]
		bigloc = names(bigcat)
		yesloc = is.element(bigloc,pdata$ID)
		fdata  = pdata[match(bigloc[yesloc],pdata$ID),]
#browser();return()
		fdata$catT  = bigcat[yesloc]
		fdata$pcat  = procat[cumcat <= pcat][yesloc]
		fdata$catP  = fdata$catT^powr
		procat.powr = (loccat^powr)/sum(loccat^powr)
		cumcat.powr = cumsum(procat.powr)
		fdata$ppcat = procat.powr[cumcat.powr <= pcat][yesloc]
#browser();return()
		## Include 0 catch as a common base for all scaling, but remove it from vector
		sVec = rev(rev(round(scaleVec(c(fdata$ppcat,0),1,500)))[-1])
		fdata$col = paint[sVec]  ## include 0 catch as a common base for all scaling
		topN   = fdata[1:(min(nrow(fdata),Ntop)),]

		#topcat = unlist(formatCatch(topN$catT,3))
		#legtxt = paste0(topcat," - ",1:nrow(topN),". ",topN$name)
		if (plot) {
			fout.e = paste0(outnam,".",ff)
			for (l in lang) {  ## could switch to other languages if available in 'linguaFranca'.
				changeLangOpts(L=l)
				#fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
				fout = switch(l, 'e' = paste0("./english/",fout.e), 'f' = paste0("./french/",fout.e) )
				topcat = unlist(formatC(topN$catT,3,format="fg",big.mark=options()$big.mark))
				legtxt = paste0(topcat, " - ", 1:nrow(topN), switch(l,'e'=". ",'f'=", "), linguaFranca(topN$name,l,localnames=TRUE))
##legtest<<-topN$name
				if (png) {
					clearFiles(paste0(fout,".png"))
					png(filename=paste0(fout,".png"), width=PIN[1], height=PIN[2], units="in", res=pngres)
				}
				plotMap(area, type="n", plt=c(0.06,0.99,0.06,0.99), 
					xlim=xlim, ylim=ylim, mgp=c(2.2,0.5,0), cex.axis=1.2, cex.lab=1.5)
				if (showAll)
					addPolys(area, border="grey", col="transparent")
				addPolys(area, polyProps=fdata[fdata$catT>0,]) ## only add polys with cpue|catch > 0
				addPolys(nepacLL, col="lightyellow1", border="grey20", lwd=0.5)
				#text(fdata$X[1:Ntop],fdata$Y[1:Ntop],1:Ntop,cex=0.8)
				#text(pmin(pmax(fdata$X[1:Ntop],xbnd[1]),xbnd[2]), pmin(pmax(fdata$Y[1:Ntop],ybnd[1]),ybnd[2]), 1:Ntop, cex=0.8)
				inbox = (1:nrow(fdata))<=Ntop #& fdata$X>xbnd[1] & fdata$X<xbnd[2] & fdata$Y>ybnd[1] & fdata$Y<ybnd[2]
				text(pmax(par()$usr[1]+abs(diff(par()$usr[1:2]))*0.025,fdata$X[inbox]), fdata$Y[inbox], (1:nrow(fdata))[inbox], cex=0.8)
				addLegend(0.99, 0.95, fill=topN$col, legend=legtxt, bty="n", title=linguaFranca(paste0("Fishery: ",ff, " - ", ifelse(cpue, "CPUE (kg/h)", "catch (t)")),l), xjust=1, title.adj=0, cex=0.9)
#browser();return()
				if (!missing(strSpp)) {
					derange = paste0(gsub("-",".",range(fdat$date,na.rm=TRUE)),collapse=" to ")
#browser();return()
					addLabel(0.3755, 0.96, linguaFranca(sppnam,l), cex=1.2, adj=c(0,0))
					addLabel(0.975, 0.96, linguaFranca(paste0("(", derange, ")"),l), cex=0.8, adj=c(1,0))
				}
				box()
				if (png) dev.off()
			} ; eop()
		}    ## end if plot
		if (csv) {
			#write.csv(fdata, paste0(outnam,".",fidnam[f],".csv"), row.names=FALSE)
			write.csv(fdata, paste0(outnam,".",ff,".csv"), row.names=FALSE)
		}
		FDATA[[as.character(f)]] = fdata
	}
	ttput(YRCAT)
	attr(FDATA, "fishery") = names(fid)
	return(FDATA)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotLocal


#plotTernary----------------------------2013-02-26
# Ternary plots - compositions in triangular space
# Equations in Schnute and Haigh (2007)
#-----------------------------------------------RH
plotTernary <- function(x=c(3,2,1), connect=FALSE, show.geometry=TRUE,
   bw=FALSE, eps=FALSE, wmf=FALSE)
{
	ciao <- function(opar){ gc(verbose=FALSE); par(opar) }
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
	invisible()
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotTernary


#plotTertiary---------------------------2018-01-02
# Composition plots within a polygonal space.
#-----------------------------------------------RH
plotTertiary <- function(x=c(100,5,25,10,50), pC=c(0.5,0.5), r=0.5,
   diag=FALSE, eps=FALSE, wmf=FALSE)
{
	ciao <- function(opar){ gc(verbose=FALSE); par(opar) }
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
#browser();return()
	Dmax = sum(apply(XYside,1,function(x,m){sqrt((x[1]-m[1])^2+(x[2]-m[2])^2)},m=unlist(shape[1,3:4])))  # maximum sum of p if mode occurs at a vertex

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
	else if (.Platform$OS.type=="windows" && wmf)
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
	invisible(pMtab)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotTertiary


## preferDepth--------------------------2024-10-24
##  Plot histogram showing depth-of-capture
##  Modified code to make use of the map object (RH 230727)
## ---------------------------------------------RH
preferDepth <- function(strSpp="410", fqtName="pht_fdep.sql", dbName="PacHarvest",
   spath=NULL, type="SQL", hnam=NULL, get.effort=FALSE, lang=c("f","e"))
{
	warn   = options()$warn; options(warn=-1)
	fenv   = lenv()  ## function's environment
	effort = ttcall(PBStool)$effort  ## recover effort, if it exists, to save time
	if (is.null(effort) && exists("effort",envir=.GlobalEnv))  ## (RH 210408)
		assign("effort", get("effort",envir=.GlobalEnv), envir=fenv)
	assign("PBStool", list(module="M05_Spatial", call=match.call(), args=args(preferDepth), plotname="Rplot", effort=effort), envir=.PBStoolEnv)

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
	if (get.effort && is.null(effort)) {
		.preferDepth.getEffort()
	} else {
		#effort=NULL; 
		setWinVal(list(showE=FALSE),winName="window") 
		packList("effort","PBStool",tenv=.PBStoolEnv)
	}
	ttput(lang)
	invisible()
}
##.preferDepth.getEffort----------------2021-04-08
.preferDepth.getEffort <- function(strSpp="ALL")
{
	resetGraph()
	par(mar=c(0,0,0,0),oma=c(0,0,0,0))
	showMessage("Please wait while effort data is retrieved for this GUI session",col="blue",cex=1.2)
	getData("pht_effort.sql",strSpp=strSpp,path=.getSpath(),tenv=penv())
	effort = PBSdat
	effort$effort = effort$effort/60 ### convert to hours
	ttget(PBStool)
	PBStool$effort <- effort
	ttput(PBStool)
	save("effort",file=paste0("effort.",substring(gsub("-","",Sys.Date()),3),".rda")) ## (RH 210408)
	frame(); addLabel(.5,.5,"Effort loaded",col="darkgreen",cex=1.2)
}
##.preferDepth.getDepth-----------------2024-02-29
.preferDepth.getDepth <- function()
{
	opar <- par(lwd=2); on.exit(par(opar))
	env.getDepth = lenv()
	data("species",package="PBSdata",envir=env.getDepth)
	getWinVal(scope="L",winName="window")
	act <- getWinAct()[1]
	if (!is.null(act) && act=="getdata")
		getdata <- TRUE 
	else
		getdata <- FALSE
	if (path==""){
		path <- getwd()
		setWinVal(list(path=path),winName="window")
	}
	spp  <- eval(parse(text=paste("c(\"",gsub(",","\",\"",strSpp),"\")",sep="")));
	year <- eval(parse(text=paste("c(",strYear,")",sep="")))
	YEAR <- year
	gear <- eval(parse(text=paste("c(",strGear,")",sep="")))
	nAc  <- length(findPat(c(LETTERS,letters),strArea))

	if (strArea!="") {
		if (any(disA=="srfa") && nAc>0)
			area <- eval(parse(text=paste("c(\"",gsub(",","\",\"",strArea),"\")",sep="")))
		else if(!any(disA=="srfa") && nAc==0)
			area <- eval(parse(text=paste("c(",strArea,")",sep="")))
		else showError(paste("\"",strArea,"\" does not match \"",disA,"\"",sep="")) 
	} else
		area <- NULL
	AREA = area
	if (any(spp=="") || length(spp)>1)
		showError("Choose 1 species")

	ttget(PBSdat)
	if (!exists("PBSdat",where=.PBStoolEnv) || is.null(attributes(PBSdat)$spp) || 
		spp!=attributes(PBSdat)$spp || fqtName!=attributes(PBSdat)$fqt || getdata) {
		expr=paste("getData(fqtName=\"",fqtName,"\",dbName=\"",dbName,"\",strSpp=\"",
			spp,"\",type=\"",type,"\",path=\"",path,"\",trusted=",trusted,
			",uid=\"",uid,"\",pwd=\"",pwd,"\",tenv=penv())",sep="")
		eval(parse(text=expr))
		if (!is.null(attributes(PBSdat)$spp) && attributes(PBSdat)$spp!=spp) {
			spp <- attributes(PBSdat)$spp
			setWinVal(list(strSpp=spp),winName="window")
		}
		ttput(PBSdat)
		depth=PBSdat
		if (type=="SQL") ## (RH 210408)
			save("depth",file=paste0("depth",spp,".",substring(gsub("-","",Sys.Date()),3),".rda"))
	}
	assign("dat",PBSdat)
	if (nrow(dat)==0)
		showError(paste("Species =",spp),type="nodata")
	packList("dat","PBStool",tenv=.PBStoolEnv) ## RH: save the raw data for manual subsetting

	## Scroll through map object and make it compatible
	if (type=="FILE" && fqtName==paste0("map",spp)) {
		needflds = c("depth","catch","effort")
		fixflds <- function(dat, nflds=needflds) {
			for (nn in nflds) {
				if (is.element(nn, colnames(dat))) {
					return(dat)
				} else {
					altflds = switch(nn, 'depth'=c("fdep","dep"), 'catch'=c(strSpp,"catKg"), 'effort'=c("eff","hours"))
					if (any(altflds %in% colnames(dat))) {
						dat[,nn] = dat[,match(altflds,colnames(dat))[altflds %in% colnames(dat)][1]]
					} else {
						stop(paste0("No candidate ", nn, " fields identified in 'mapdat' object"))
					}
				}
			}
			return(dat)
		}
		dat = fixflds(dat)
		effort = dat[,c("year","date","major","minor","locality","depth","effort","gear")]
		dat = dat[dat$catch>0 & !is.na(dat$catch),]
		ttget(PBStool)
		PBStool$dat = dat
		PBStool$effort = effort
		ttput(PBStool)
	}
#browser();return()

	eff = ttcall(PBStool)$effort
	isE = ifelse(is.null(eff),FALSE,TRUE)
	if (!isE) setWinVal(list(showE=FALSE),winName="window")

	if (!is.null(gear)) {
		if (isE) eff = eff[is.element(eff$gear,gear),]
		dat = dat[is.element(dat$gear,gear),]
		if (nrow(dat)==0) showError(paste("gear =",paste(gear,collapse=", ")),type="nodata")
	}
	if (!is.null(year)) {
		if (isE) eff = eff[is.element(eff$year,year),]
		dat <- dat[is.element(dat$year,year),]
		if (nrow(dat)==0) showError(paste("year =",paste(year,collapse=", ")),type="nodata")
	}
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
	if (is.null(year) || group) year <- 0 else year <- sort(unique(dat$year)); nyr <- length(year);
	if (is.null(area) || group) area <- 0 else area <- sort(unique(dat$area)); nar <- length(area);

	plotname = paste0("dep", spp, paste0("-d(",paste0(XLIM,collapse="-"),")"),
		#ifelse(year==0 && is.null(YEAR),"",paste0("-y(",paste0(range(YEAR),collapse="-"),")")),
		#ifelse(area==0 && is.null(AREA),"",paste0("-a(",paste0(AREA,collapse="."),")")),
		#ifelse(is.null(gear),"",paste0("-g(",paste0(gear,collapse="."),")")) )
		ifelse(year==0 && is.null(YEAR),"",paste0("-y(",texThatVec(YEAR),")")),
		ifelse(area==0 && is.null(AREA),"",paste0("-a(",texThatVec(AREA),")") ),
		ifelse(is.null(gear),"",paste0("-g(",texThatVec(gear),")") )
	)
	plotname = gsub(" and ", ",", plotname)
	#if (type=="FILE") plotname = sub(paste0("dep",spp),fqtName,plotname)
	if (type=="FILE") plotname = paste0(plotname, "-(", fqtName, ")")
#browser();return()
	packList("plotname","PBStool",tenv=.PBStoolEnv)
	nrow <- max(nyr,nar); ncol <- min(nyr,nar);
	if (nyr>=nar) { ifac <- year; jfac <- area; yba <- TRUE }
	else { ifac <- area; jfac <- year; yba <- FALSE }

	eps=png=wmf=FALSE
	if (!is.null(act)){
		if (act=="eps")      eps=TRUE
		else if (act=="png") png=TRUE
		else if (act=="wmf") wmf=TRUE
	}
	fout.e = plotname
	for (l in ttcall(lang)) {
		changeLangOpts(L=l)
		#fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		fout = switch(l, 'e' = paste0("./english/",fout.e), 'f' = paste0("./french/",fout.e) )
		if (eps)
			postscript(file=paste(fout,"eps",sep="."),width=10,height=6*nrow^0.25,horizontal=FALSE,paper="special")
		else if (png) {
			clearFiles(paste0(fout,".png"))
			png(filename=paste(fout,"png",sep="."), width=10, height=6*nrow^0.25, units="in", res=400)
		}
		else if (wmf && .Platform$OS.type=="windows")
			do.call("win.metafile",list(filename=paste(fout,"wmf",sep="."),width=10,height=6*nrow^0.25))
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
			eff = eff[eff$depth>=brks[1] & eff$depth<=rev(brks)[1] & !is.na(eff$depth),]
	
		for (i in ifac) {
			if (all(ifac!=0)) {
				if (isE) ieff = eff[is.element(eff[,ifelse(yba,"year","area")],i),]
				idat <- dat[is.element(dat[,ifelse(yba,"year","area")],i),]
			} else {
				idat <- dat
				if (isE) ieff <- eff
			}
			for (j in jfac) {
				if (all(jfac!=0)) {
					jdat <- idat[is.element(idat[,ifelse(yba,"area","year")],j),]
					if (isE) jeff <- ieff[is.element(ieff[,ifelse(yba,"area","year")],j),]
				} else {
					jdat <- idat
					if (isE) jeff <- ieff
				}
#browser();return()
				ntows <- nrow(jdat)
				stuff = c("ifac","jfac","ntows")
				packList(stuff,"PBStool",tenv=.PBStoolEnv)
				xy <- hist(jdat$depth, breaks=brks, plot=FALSE);
				xy$density <- xy$counts/sum(xy$counts)
				xyout <- paste("xy",spp,i,j,sep=".")
				ttget(PBStool); PBStool[[xyout]] <- xy; ttput(PBStool)
	
				if (isE) {
					jeff$dbin = cut(jeff$depth,brks)
					effTot = sapply(split(jeff$effort,jeff$dbin),sum)
					effDen = effTot/sum(effTot)
					##ee     = sum(jeff$effort,na.rm=TRUE)     ## cumulative effort (effort already in in hours)
					ee     = length(jeff$effort[jeff$effort>0 & !is.na(jeff$effort)])  ## count number of tows with effort
					xye    = list(breaks=brks,counts=effTot,density=effDen)
					attr(xye,"class")="histogram"
					packList("xye","PBStool",tenv=.PBStoolEnv) }
	
				par(lwd=1.6) ## doesn't seem to take when specified in first line of function
				plot(xy,freq=FALSE, xlab="", ylab="", main="",cex.lab=1.2, col="white", border="white", xlim=XLIM, ylim=YLIM, axes=FALSE)
				if (par()$mfg[1]==par()$mfg[3])
					axis(1,cex.axis=1.2)
				if (par()$mfg[2]==1)
					axis(2,cex.axis=1.2,tcl=.5) else axis(2,labels=FALSE,tcl=.5)
	
				if (isE && showE) { #---Total fishing effort density
					plot(xye,freq=FALSE, xlab="", ylab="", main="",cex.lab=1.2, col=barcol[2], border=barcol[2],xlim=XLIM, ylim=YLIM, axes=FALSE,add=TRUE)
				}
				z  = order(jdat$depth)
				xc = jdat$depth[z]
				yz = ((1:length(xc))/length(xc))
				cc = cumsum(jdat$catch[z])/1000
				yy = paste0("(",paste0(range(jdat$year),collapse="-"),")")
#browser();return()
				if (showD) { ##---Cumulative depth
					mz = approx(yz,xc,.50,rule=2,ties="ordered"); yz=yz*YLIM[2]
					points(mz$y,par()$usr[4],pch=25,col=1,bg="gainsboro",cex=1.5)
					text(mz$y,par()$usr[4],round(mz$y),col="grey20",adj=c(-0.5,0.25),cex=0.9,srt=270)  ## adj (vert, horiz)
				}
				if (showC) { ##---Cumulative catch
					yc = (cc/max(cc,na.rm=TRUE)) #*YLIM[2]
					mc = approx(yc,xc,.50,rule=2,ties="ordered"); yc=yc*YLIM[2]
					#abline(h=mc$x*YLIM[2],v=mc$y,lty=3,col=ccol)
					points(mc$y,par()$usr[4],pch=25,col=1,bg=ccol,cex=1.5)
					text(mc$y,par()$usr[4],round(mc$y),col=ccol,adj=c(-0.5,ifelse(!isThere("mz"), 0.25, ifelse(mc$y<mz$y,0.75,0.25))),cex=0.9,srt=270)  ## adj (vert, horiz)
	#browser();return()
				}
				if (showQ) { ##---Depth quantiles
					qnt <- round(quantile(jdat$depth,quants,na.rm=TRUE),0)
					abline(v=qnt, lwd=2, col="cornflowerblue")
					text(qnt[1]-xwid,par()$usr[4],qnt[1],col="blue",adj=c(1,2),cex=1.2)
					text(qnt[2]+xwid,par()$usr[4],qnt[2],col="blue",adj=c(0,3),cex=1.2) 
				}
				if (showL) { ##---Legend information
					if (nrow*ncol>1) {
						#addLabel(.98,.90,paste(ifelse(i==0,"",i),ifelse(j==0,"",j), sep=ifelse(nrow==1 | ncol==1,""," \225 ")),col="grey30",adj=c(1,1))
						addLabel(0.98, 0.90, paste(ifelse(i==0,"",i), ifelse(j==0,"",j), sep=ifelse(nrow==1 | ncol==1,"",convUTF(" \\u{2022} "))), col="grey30", adj=c(1,1) )
					} else {
						ylabpos = 0.88; labcex = 1; labinc = 0.05 #diff(par()$usr[3:4])*0.05
						this.species = switch(l, 'e'=species[strSpp,"code3"], 'f'=eval(parse(text=deparse("cette esp\u{00E8}ce"))) )
						data(species,package="PBSdata",envir=penv())
						addLabel(0.98, ylabpos, linguaFranca(paste0( species[strSpp,"name"],"\n",yy),l), adj=c(1,0.5), col="black", cex=labcex)
						addLabel(0.98, ylabpos-2*labinc, paste0(this.species," = ", format(ntows,big.mark=options()$big.mark), linguaFranca(" events",l)), cex=labcex-0.1, col="grey30", adj=c(1,1)) 
						addLabel(0.98, ylabpos-3*labinc, paste0(this.species, " = ",format(round(max(cc,na.rm=TRUE)), big.mark=options()$big.mark)," t"), cex=labcex-0.1, col="grey30", adj=c(1,1))
						if (isE && showE) {##---Total effort (h) in depth range specified
							addLabel(0.98, ylabpos-4*labinc, paste0(switch(l,'e'="ALL = ",'f'=eval(parse(text=deparse("toutes les esp\u{00E8}ces = "))) ), format(round(ee), big.mark=options()$big.mark), linguaFranca(" events",l)), cex=labcex-0.1, col="grey30", adj=c(1,1))
						}
					}
				}
#browser();return()
				old.lwd = par()$lwd; par(lwd=0.5)
				plot(xy, freq=FALSE, xlab="", ylab="", main="",cex.lab=1.2, col=lucent(barcol[1],0.2), border="slategrey", xlim=XLIM, ylim=YLIM, axes=FALSE, add=TRUE)
				par(lwd=old.lwd)
				if (showD) lines(xc,yz,col="grey20",lwd=clwd)
				if (showC) lines(xc,yc,col=ccol,lwd=clwd)
				box();
			}
		}
		mtext(linguaFranca(paste("Depth (m)",ifelse(ncol==1,"",paste(" [",ifelse(yba,"area","year"),
			"by column ]"))),l),outer=TRUE,side=1,line=2*nrow^0.25,cex=1.2)
		mtext(linguaFranca(paste("Proportions",ifelse(nrow==1,"",paste(" [",ifelse(yba,"year","area"),
			"by row ]"))),l),outer=TRUE,side=2,line=3.25,cex=1.25,las=0)
		if (eps|png|wmf) dev.off()
	}; eop()
	invisible() 
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~preferDepth


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
prepClara  <- function(ssid=7, gfld=c("X","Y"), sfld="spp", mfld="catKg", fnam=NULL, ioenv=.GlobalEnv)
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


## zapHoles-----------------------------2024-02-29
## Attempts to remove holes overwritten by solids.
## ---------------------------------------------RH
zapHoles <- function(pset)
{
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
	return(pset)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~zapHoles

