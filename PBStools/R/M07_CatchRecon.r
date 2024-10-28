##==============================================================================
## Module 7: Catch Reconstruction
## ------------------------------
##  buildCatch......Build a catch history of BC rockfish 1918--present.
##  plotDiag........Plot diagnostic data for catch reconstructions.
##  plotGREFS.......Plot gamma for reference years by fishery.
##  plotRecon.......Plot reconstructed catch using barplots stacked by PMFC area.
##  quickCat........Subsets a large dataset derived from query 'fos_mcatORF.sql'.
##  surveyCatch.....Query the survey catch data and make summary tables.
##  zapSeamounts....Remove seamount records using combinations of major, minor, and locality codes.
##==============================================================================


## buildCatch---------------------------2024-03-14
## Catch reconstruction algorithm for BC rockfish.
## Use ratios of RRF (reconstructed rockfish) to ORF 
## (rockfish other than POP) landings for multiple fisheries.
## Matrix indices: i=year, j=major, k=fid, l='spp'
## Arguments definitions appear below:
## ---------------------------------------------RH
buildCatch=function(
   dbdat,                ## List object of landing records from eight DFO databases
   strSpp="396",         ## Hart species code for the rockfish to be reconstructed (RRF)
   orfSpp="TRF",         ## Field name of the denominator in the ratio of RRF to other rockfish (usually ORF but can be TRF or POP if these are more appropriate)
   major=c(1,3:9),       ## Major PMFC area codes to see in plots; catch is always reconstructed for majors c(1,3:9)
   fidout=c(1:5,10),     ## Fishery IDs for which an annual series barplot stacked by PMFC area is produced
   useYR1=c(1996,2000,2007,2007,1986), ## First year to start using reported landings (i.e. not estimated from gamma), one for each fishery:
   useGFM=TRUE,          ## Use the latest official GF_MERGED_CATCH table (compiled by Norm and Kate)
   useCA=TRUE,           ## Use RRF|ORF catch from the CA fleet
   useUS=TRUE,           ## Use RRF|ORF catch from the US fleet
   useFF=TRUE,           ## Use RRF|ORF catch from the foreign (UR, JP, PO, etc) fleet
   useSM=FALSE,          ## Use catch data from seamounts
   useLS=TRUE,           ## Use ORF catch from Langara Spit in gamma calculation
   useAI=FALSE,          ## Use Anthony Island catch as 5C catch (chiefly for POP and maybe YMR)
   useGM=FALSE,          ## Use geometric mean to average annual ratios for gamma and delta (not used if strat.gamma, strat.delta, or useBG)
   useBG=FALSE,          ## Sample from the binomial-gamma to estimate ratios RRF/ORF or RRF/TRF.
   #refyrs=1997:2005,    ## Reference years to use for calculating gamma (e.g., RRF/ORF).
   refyrs = c(list(1996:2023), rep(list(2007:2011),4)),  ## Reference years to use for calculating gamma (e.g., RRF/ORF). Now a list (RH 190917)
   refarea=NULL,         ## Name of file containing reference areas to use when calculating gamma
   refgear=NULL,         ## Reference gear types years to use for calculating gamma (1=bottom trawl, 2=trap, 3=midwater trawl, 4=h&l, 5=longline, 8=h&l/longline/trap)
   strat.gamma=FALSE,    ## Stratify the RRF numerator and ORF denominator by depth zone and weight by frequency of RRF landed > 0
   strat.delta=FALSE,    ## Stratify the discard numerator and denominator by depth zone and weight by frequency of RRF discards > 0
   depbin=100,           ## Depth (m) bin to use for strat.gamma and strat.delta
   defyrs = c(list(1997:2006), rep(list(2000:2004),4)), ## Reference years to use for calculating delta (discard rate)
   disyrs = list(1954:1995, 1986:2005, 1986:2005, 1986:2005, 1986:2005), ## Discard years (on set for each fishery (trawl, halibut, sable, dogling, hlrock)
   sensitivity=NULL,     ## Sensitivity name for tweaking decisions
   reconstruct=TRUE,     ## Complete the reconstruction to its end, otherwise terminate the code once the modern catch array has been compiled and saved
   run.name=NULL,        ## Run name to keep track of various run configurations with gamma, delta, refarea, and refgear
   ascii.tables=TRUE,    ## Create ASCII ouput tables and dump them into the subdirectory called `tables'
   diagnostics=FALSE,    ## Create automatically-numbered diagnostic images files to a subdirectory called `diags'
   saveinfo=TRUE,        ## Save various data and function objects to a list object called `CR' in the temporary environment `.PBStoolEnv'
   sql=FALSE,            ## Query the databases, otherwise load catch records from binary files saved from a previous query run (to save time)
   sql.only=FALSE,       ## Only execute the queries to download catch data from remote databases
   sql.force=FALSE,      ## Force the SQL query even if the data file (.rda) exists
   spath=.getSpath(),    ## Path to SQL code files -- defaults to the `sql' directory under `system.file(package="PBStools")'
   dpath=getwd(),        ## Database path for times when user wants to build alternative catch histories (in another directory) using data already queried
   eps=FALSE, png=FALSE, wmf=FALSE, # Send the figures to `.eps' , `.png', and `.wmf' files, respectively
   uid=Sys.info()["user"], pwd=uid, # User ID and password for Oracle DB account authentication (only used for PacHarv3 currently)
   ioenv=.GlobalEnv,     ## Input/output environment for function input data and output results
   hadley=FALSE,         ## Use Hadley Wickham's bloated R packages
   debug=FALSE,          ## set to TRUE to activate some lines that spew nonsense
   ...)                  ## Additional ad hoc arguments to deal with PJS issues
{
	## Reset to current working directory in case of code malfunction
	cwd= getwd()
	on.exit(setwd(cwd), add=TRUE)
	catDir = dpath ## (RH: 240216) assume this was the intent of 'dpath' (may need to go through code)
	## R's default of treating strings as factors screws up so many things.
	sAF  = options()$stringsAsFactors
	scip = options()$scipen
	options(stringsAsFactors=FALSE, scipen=10)
	on.exit(options(stringsAsFactors=sAF, scipen=scip), add=TRUE)

	data("pmfc", "species", package="PBSdata", envir=penv())
	bigdate = Sys.Date()
	numdate = format(Sys.time(),"%y%m%d(%H%M)")

	.flush.cat(paste0("-----Start reconstructing catch for ",species[strSpp,"latin"],"-----\n\n"))
	fidnam  = c("trawl","halibut","sablefish","sched2","zn","sabzn","sabhal","dogfish","lingcod","combined")
	fshnam  = c("trawl","h&l","trap",rep("h&l",6),"combined")  ## general category vector
	fidfive = c("Trawl","Halibut","Sablefsh","DogLing","HLRock")
	REFYRS  = .su(unlist(refyrs))

	run.details = paste0( orfSpp, 
		"_rA(",paste0(major,collapse=""),")",
		"_rY(",paste0(substring(range(REFYRS),3,4),collapse=""),")"
	)
	if (!is.null(useYR1)) ## Use reported catch starting in these years (by fishery)
		run.details = paste0(run.details,"_y1(", paste0(substring(useYR1,3,4),collapse="'"),")")
	if (!is.null(refarea)) ## reference fishery-specific localities
		run.details = paste0(run.details,"_rF(",paste0(names(refarea),collapse=""),")")
	if (!is.null(refgear)) ## reference fishery-specific gear types
		run.details = paste0(run.details,"_rG(",paste0(names(refgear),collapse=""),")")
	if (any(c(strat.gamma,strat.delta))) ## depth-stratified gamma and/or delta
		run.details = paste0(run.details,"_sGD(", substring(strat.gamma,1,1), substring(strat.delta,1,1), ")")

	if (!sql.only && is.null(run.name)) run.name = run.details

	## -----------------------------------------(RH 1901924)
	## Report inputs to a text file called 'run.details.txt'
	## -----------------------------------------------------
	run.details = c(run.details, 
		paste0("call: ", gsub(" ","",deparse(match.call(),width.cutoff=500))),
		"Formal arguments:",
		paste0("dgbdat = ", deparse(as.character(substitute(dbdat)))) )
	fargs = setdiff(formalArgs(buildCatch),"...")
	for (f in 2:length(fargs)) {
		ff = fargs[f]
		mess = paste0(ff, " = ", ifelse(is.environment(get(ff)),
			deparse(environmentName(ioenv)), deparse(get(fargs[f]))))
		run.details = c(run.details, mess)
	}
	if (length(list(...)) > 0) {
		run.details = c(run.details, "User-specified (dot) arguments:")
		for (ff in names(list(...))){
			mess = paste0(ff, " = ", deparse(list(...)[[ff]]))
			run.details = c(run.details, mess)
		}
	}
	if (!sql.only && !file.exists(run.name))
		dir.create(run.name)
	writeLines(run.details, con=paste0(ifelse(sql.only,catDir,run.name),"/run.details.txt"))

	datDir = paste0(ifelse(is.null(run.name),catDir,run.name),"/data")
	if (!sql.only && !file.exists(datDir)){
		dir.create(datDir)
		datPrev = list(...)$datPrev  ## if transferring data from a previous reconstruction
		if (!is.null(datPrev)) {
			datPrev = paste0(catDir,"/",datPrev)
			if (dir.exists(datPrev)) {
				rda.files = list.files(datPrev, pattern="\\.rda$", full.names=TRUE)
				file.copy(from=rda.files, to=paste0(catDir,"/",datDir), copy.date=TRUE)
			} else {
				mess = paste0("!!! Previous data directory:\n",datPrev,"\ndoes not exist.")
				message(mess); return(mess)
			}
		}
	}
	figDir = paste0(ifelse(is.null(run.name),catDir,run.name),"/figs")
	if (!sql.only && !file.exists(figDir))
		dir.create(figDir)
	tabDir = paste0(ifelse(is.null(run.name),catDir,run.name),"/tables")
	if (ascii.tables){
		if (!sql.only && !file.exists(tabDir))
			dir.create(tabDir)
	}
	diaDir = paste0(ifelse(is.null(run.name),catDir,run.name),"/diags")
	if (diagnostics){
		if (!sql.only && !file.exists(diaDir))
			dir.create(diaDir)
	}

	if (!sql.only) {
		## -------------------------------------------------------------
		## Global list object 'CR' stores results from the analysis
		## -------------------------------------------------------------
		assign("CR",list(module="M07_BuildCatch",call=match.call(),args=args(buildCatch),
			spp=strSpp,pD=1,eps=eps,wmf=wmf,fidnam=fidnam,fshnam=fshnam),envir=.PBStoolEnv)
		## ------------------------------------------
		## pD = Counter for plotDiag diagnostics
		## Function to convert numbers to proportions
		## ------------------------------------------
		pcalc=function(x){if (all(x==0)) rep(0,length(x)) else x/sum(x)}
		sysyr=as.numeric(substring(Sys.time(),1,4)) ## maximum possible year

		if (diagnostics){
			diag.files = list.files(diaDir)
			if (length(diag.files)>0) 
				junk = file.remove(paste0(diaDir,"/",diag.files,sep=""))
		}
		clrs.fishery=c("red","blue","orange","green","yellow"); names(clrs.fishery)=1:5

		fold01 = TRUE ## ----- only used for code folding -----
		if (fold01) { ## 1. Compile historical catch
		## ====================================================
		## 1. Compile historical catches for RRF, POP, ORF, TRF
		## ====================================================
		.flush.cat(paste0("Compiling historical catches for ",strSpp,", POP, ORF, TRF\n"))

		for (h in c("rrf","orf")) {
			## ------------------------------------------------------------------------------------------------------------
			## Use following line for times when objects are not registered in PBSdata (i.e. package has not beeen rebuilt)
			#  expr = paste0("getFile(",h,"history,use.pkg=TRUE,tenv=penv()); dat=",h,"history")
			## ------------------------------------------------------------------------------------------------------------
			expr = paste0("getFile(",h,"history,path=paste0(system.file(package=\"PBSdata\"),\"/data\"),tenv=penv(),reload=TRUE); hdat=",h,"history")
			eval(parse(text=expr))

			if (useAI && any(strSpp==c("396","440"))){
				.flush.cat(h, ": expanding 5C to include Flamingo/Anthony in 5E and Moresby Gully in 5B...\n")
				hdat = expand5C(hdat)
			}
#browser();return()
#if(h=="rrf") hdat$spp[is.element(hdat$spp,"418")] = "396" ## for testing only

			#getFile(orfhistory,use.pkg=TRUE,tenv=penv())
			#dat    = rrfhistory ## catch in kg
			if (h=="orf") {
				if (!useFF) hdat = hdat[!is.element(hdat$source,c("Ketchen80","Ketchen80b","Leaman80")),] # don't use foreign ORF in reconstruction
				#if (!useLS) hdat = hdat[!(is.element(hdat$major,9) & is.element(hdat$minor,35)),] # don't use Langara Spit catch BUT no minor area info in early series
			}
			colnames(hdat) = tolower(colnames(hdat))
			hisyrs = sort(unique(hdat$year))
			HISYRS = hisyrs[1]:hisyrs[length(hisyrs)]  # ends up being all years from orfhis
			if (h=="orf")
				useYR1def = rev(HISYRS)[1] + 1
			majhis = sort(unique(hdat$major))
			sou    = sort(unique(hdat$source))
			nat    = sort(unique(hdat$nation))
			natUse = c(c("CA","US")[c(useCA,useUS)], if (useFF) setdiff(nat,c("CA","US")) else NULL)
			act    = rev(sort(unique(hdat$action))) # force `max' to come before `add'
			fsh    = sort(unique(hdat$fishery))
			spp    = sort(unique(hdat$spp))
			if (h=="rrf" && any(is.element(c("455","467","614"),spp))){ 
				## ---------------------------------------------------------------------
				## collect IPHC halibut data (and others) for discard calculations later
				## ---------------------------------------------------------------------
				jj = as.character(c(1,3:9)) ## BC reconstruction PMFCs
				if (is.element("614",spp)){ 
					iphcdat = hdat[is.element(hdat$spp,"614") & is.element(hdat$fishery,"halibut"),]
					cat614  = sapply(split(iphcdat$catch,iphcdat$year),sum,na.rm=TRUE)/1000.
				}
				if (is.element("455",spp)){ 
					## Ketchen (1976) FID 3:1 SBF caught by Sablefish fishery 10.235 times higher than that caught by trawl fishery (ratio geomean from 1996-2011)
					sbfdat = hdat[is.element(hdat$spp,"455") & is.element(hdat$fishery,"trawl"),]
					cat455 = crossTab(sbfdat, c("year","major"), "catch")
					cat455 = 10.235 * cat455[,intersect(jj,colnames(cat455))]
#browser();return()
					#cat455 = 10.235 * sapply(split(sbfdat$catch,sbfdat$year), sum, na.rm=TRUE)/1000.  ## rendering as vector destroys area info
				}
				if (is.element("467",spp)){ 
					## Ketchen (1976) FID 4:1 LIN caught by DOG/LIn (Sched II) fishery 0.351 of that caught by trawl fishery (ratio geomean from 1996-2011)
					lindat = hdat[is.element(hdat$spp,"467") & is.element(hdat$fishery,"trawl"),]
					cat467 = crossTab(lindat, c("year","major"), "catch")
					cat467 = 0.351 * cat467[,intersect(jj,colnames(cat467))]
					#cat467 = 0.351 * sapply(split(lindat$catch,lindat$year), sum, na.rm=TRUE)/1000.  ## rendering as vector destroys area info
				}
			}
			## Basically, h loop stops here for "rrf" if there are no supplemental data for strSpp in rrfhistory
			if (h=="rrf" && !is.element(strSpp,spp)) {
				rrfbase = NULL
				next
			}
			if (h=="rrf"){
				htab=array(0,dim=c(length(HISYRS),length(majhis),length(sou),length(nat),length(act),length(fsh),length(spp)),
					dimnames=list(HISYRS,majhis,sou,nat,act,fsh,spp))
				names(dimnames(htab))=c("year","major","source","nation","action","fishery","RRF")
			} else {
				htab=array(0,dim=c(length(HISYRS),length(majhis),length(sou),length(nat),length(act),length(fsh),3),
					dimnames=list(HISYRS,majhis,sou,nat,act,fsh,c("POP","ORF","TRF")))
				names(dimnames(htab))=c("year","major","source","nation","action","fishery","catch")
			}
			for (a in act) {
				adat = hdat[is.element(hdat$action,a),]
				if(nrow(adat)==0) next
				for (b in fsh) {
					bdat = adat[is.element(adat$fishery,b),]
					if(nrow(bdat)==0) next
					for (i in sou) {
						ii   = as.character(i)
						idat = bdat[is.element(bdat$source,i),]
						if(nrow(idat)==0) next
						for (nn in nat) { ## keep track of nationality (RH 161109)
							ndat = idat[is.element(idat$nation,nn),]
							if(nrow(ndat)==0) next
							for (j in majhis) {
								jj   = as.character(j)
								jdat = ndat[is.element(ndat$major,j),]
								if(nrow(jdat)==0) next
								if (h=="rrf"){
									for (k in spp) {
										kk   = as.character(k)
										kdat = jdat[is.element(jdat$spp,k),]
#if(b=="trawl" && i=="Ketchen80b" && nn=="PO" && j==4 && kk=="418") {browser();return()} ## check out YTR catch by Poland
										if(nrow(kdat)==0) next
										## -------------------------------------------------------------------
										## Note sum over nation (CA+US should be treated as "max" in database)
										## -------------------------------------------------------------------
										RRF = sapply(split(kdat$catch,kdat$year),sum,na.rm=TRUE)/1000
										htab[names(RRF),jj,ii,nn,a,b,kk] = RRF
									}
								} else {
									z=is.element(jdat$spp,"396")
									if (any(z)) {
										POP= sapply(split(jdat$catch[z],jdat$year[z]),sum,na.rm=TRUE)/1000
										htab[names(POP),jj,ii,nn,a,b,"POP"]=POP
									}
									ORF= sapply(split(jdat$catch[!z],jdat$year[!z]),sum,na.rm=TRUE)/1000
									TRF= sapply(split(jdat$catch,jdat$year),sum,na.rm=TRUE)/1000
									htab[names(ORF),jj,ii,nn,a,b,"ORF"]=ORF
									htab[names(TRF),jj,ii,nn,a,b,"TRF"]=TRF
								}
							} ## close j  loop
						}    ## end nat loop
					}       ## end sou loop
				}          ## end fsh loop
			}             ## end act loop
#if(h=="orf") {browser();return()}
			if (h=="rrf") {
				## ------------------------------------------------------
				## RRF historical catch not always recorded in modern DBs
				## ------------------------------------------------------
				rrfbase = htab[,,,,,,strSpp,drop=FALSE]
				rrfmax  = rrfadd = array(0,dim= dim(rrfbase)[c(1,2,6,4)],dimnames=dimnames(rrfbase)[c("year","major","fishery","nation")]) ## initialise
				for (nn in natUse) {
					for (b in fsh){
						for (i in sou){
							if (is.element("add",dimnames(rrfbase)$action))
								rrfadd[,,b,nn] = rrfmax[,,b,nn] + rrfbase[,,i,nn,"add",b,strSpp]
							else rrfadd = NULL
							if (is.element("max",dimnames(rrfbase)$action)) {
								zbase  = (rrfbase[,,i,nn,"max",b,strSpp] > rrfmax[,,b,nn]) > 0
								rrfmax[,,b,nn][zbase] = rrfbase[,,i,nn,"max",b,strSpp][zbase]
							} else rrfmax = NULL
						} ## end i loop
					} ## end b loop
				} ## end nn loop
## ***** need to revise where these appear later in the code
				htab.rrf = htab ## (RH 240312)
				if (saveinfo)
					packList(c("htab.rrf","rrfbase","rrfadd","rrfmax"),"CR",tenv=.PBStoolEnv)
			} else
				htab.orf = htab ## (RH 240312)
		} ## end h loop
		## From this point forward, 'htab' is 'htab.orf'

		## -------------------------------------------------------------------------------------------------
		## For the years 1918 to 1949, no records exist to delineate POP, ORF, and TRF.
		## In essence TRF = ORF for Dominion Bureau Stats and Stewart's US catch of BC rockfish
		## Therefore, use empirical data from 1951 to 1995 to estimate ORF from TRF (the difference is POP).
		## -------------------------------------------------------------------------------------------------
		sARF = apply(htab,c("year","catch","nation"),sum)   ## sum of all rockfish
		for (nn in nat) {
			nsARF = sARF[,,nn]
			#zUNK  = nsARF[,"POP"]==0 & nsARF[,"TRF"]>0
			zUNK  = nsARF[,"POP"]==0 & nsARF[,"ORF"]!=nsARF[,"TRF"] & nsARF[,"TRF"]>0
#if (nn=="PO") {browser();return()}
			if (!any(zUNK)) next
			zOBS  = nsARF[,"TRF"]>0 & nsARF[,"ORF"]>0 & nsARF[,"POP"]>0
			oTRF  = nsARF[zOBS,"TRF"]
			oORF  = nsARF[zOBS,"ORF"]
#if (nn=="PO") {browser();return()}
			lmo   = lm(log2(oORF)~log2(oTRF))
			alp   = lmo$coeff[1]; bet = lmo$coeff[2]
			htab[zUNK,,,nn,,,"ORF"] = pmin(htab[zUNK,,,nn,,,"TRF"],2^(alp+bet*log2(htab[zUNK,,,nn,,,"TRF"]))) ## ORF cannot be bigger than TRF
			htab[zUNK,,,nn,,,"POP"] = htab[zUNK,,,nn,,,"TRF"] - htab[zUNK,,,nn,,,"ORF"]
		}
#browser();return()
#		zUNK = is.element(rownames(sARF),as.character(1918:1952))               ## unknown ORF and POP
#		zOBS = is.element(rownames(sARF),as.character(c(1953:1965,1967:1995)))  ## exclude anomalous 1966 observation
#		oTRF = sARF[zOBS,"TRF"]
#		oORF = sARF[zOBS,"ORF"]
#		lmo  = lm(log2(oORF)~log2(oTRF))
#		alp  = lmo$coeff[1]; bet = lmo$coeff[2]
#		htab[zUNK,,,,,"ORF"] = pmin(htab[zUNK,,,,,"TRF"],2^(alp+bet*log2(htab[zUNK,,,,,"TRF"]))) ## ORF cannot be bigger than TRF
#		htab[zUNK,,,,,"POP"] = htab[zUNK,,,,,"TRF"] - htab[zUNK,,,,,"ORF"]

		if (diagnostics){
			mm = as.character(major)
			for (spp in dimnames(htab)$catch) {
				for (fish in dimnames(htab)$fishery) {
					plotDiag(apply(htab[,mm,,,,fish,spp],c(1,3),sum), paste0("htab ",spp," in ",fish)) }
				plotDiag(apply(htab[,mm,,,,,spp],c(1,3),sum), paste0("htab ",spp," in all fisheries"))
			}
		}

		## -----------------------------------------------------------------------
		## Sabotage htab to remove PacHarv3 for trap and trawl between 1954 & 1995
		## (GFCatch provides best estimates, see Rutherford 1999)
		## -----------------------------------------------------------------------
		htab[as.character(1954:1995),,"PacHarv3","CA","max",c("trap","trawl"),] = 0
		if (diagnostics)
			plotDiag(apply(htab[,mm,,,,"trawl","POP"],c(1,3),sum), paste0("htab sabotaged POP in trawl"))

		## -------------------------------------------------------------------------------
		## historical maximum catch (POP,ORF,TRF) by year, major, gear (i.e. comparative):
		## -------------------------------------------------------------------------------
		htabmax = htab[,,,,"max",,]
		orfmax = apply(htabmax,c(1:2,4:6),max,na.rm=TRUE) ## collapse 'source' (dim 3); ## now contains nation dimension

		## maybe update ORF and TRF with addtional RRF history
		#htabmax.rrf = htab.rrf[,,,,"max",,]
		#rrfmax = apply(htabmax.rrf,c(1:2,4:6),max,na.rm=TRUE)
		## ***** still needs work

		## ---------------------------------------------------------------------------
		## historical unique catch (POP,ORF,TRF) by year, major, gear (i.e. additive):
		## ---------------------------------------------------------------------------
		htabadd = htab[,,,,"add",,]
		orfadd = apply(htabadd,c(1:2,4:6),sum,na.rm=TRUE) ## collapse 'source' (dim 3); ## now contains nation dimension
#browser();return()

		MAJ = dimnames(orfmax)$major
		clrs.major=rep("gainsboro",length(MAJ)); names(clrs.major)=MAJ
		#clrs.major[as.character(c(1,3:9))]=c("moccasin","blue","lightblue","yellow","orange","red","seagreen","lightgreen")  ## original colour scheme
		#clrs.major[as.character(c(1,3:9))]=c("moccasin","seagreen","lightgreen","yellow","orange","red","dodgerblue","lightblue1")  ## RH 230202 -- switch colours for 3CD and 5DE (North=blues, Central=ambers, South=greens)
		#clrs.major[as.character(c(1,3:9))]=c("moccasin","seagreen","lightgreen","yellow","orange","red","lightblue1","dodgerblue")  ## RH 230202 -- switch colours for 3CD and 5DE (North=blues, Central=ambers, South=greens)
		clrs.major[as.character(c(1,3:9))]=c("moccasin","forestgreen","lightgreen","yellow","orange","red","lightblue1","dodgerblue")  ## RH 240222 -- change colour for 3C from 'seagreen' to 'forestgreen' (softer green)

		if (saveinfo)
			packList(c("htab","htabmax","htabadd","orfmax","orfadd"),"CR",tenv=.PBStoolEnv)
		## Historical used again on line 335
		.flush.cat("-----Finished collecting historical landings from `orfhistory'-----\n\n")

		} ## end fold01
		fold02 = TRUE ## ----- only used for code folding -----
		if (fold02) { ## 2. Gather modern RRF catch
		## =====================================================
		## 2. Gather the modern RRF catches (landed & discarded)
		## =====================================================
		.flush.cat("Start collecting modern landings:\n")

		if (missing(dbdat) && sql==FALSE) {
			mess=paste0("Either provide a list object 'dbdat' or set 'sql=TRUE'.\n\n",
				"Ideally, 'dbdat' should contain data frames:\n", 
				"'ph3cat' = PacHarv3 database (all fisheries)\n")
			if (useGFM)
				mess = paste0(mess,"'gfmdat' = GFFOS merged catch table GF_MERGED_CATCH\n")
			else {
				mess = paste0(mess,
				"'gfcdat' = GFCatch database (trawl, trap, h&l)\n",
				"'phtdat' = PacHarvest database (groundfish trawl)\n",
				"'phhdat' = PacHarvHL database (halibut bycatch from DMP validated landings)\n",
				"'phsdat' = PacHarvSable database (sablefish trap)\n",
				"'phvdat' = PacHarvHL database (validated landings Sched II & ZN)\n",
				"'phfdat' = PacHarvHL database (fisherlog records Sched II & ZN)\n",
				"'fosdat' = GFFOS database on the GFSH server (all fisheries)\n",
				"'jvhdat' = GFBioSQL database (joint-venture hake)\n")
			}
			mess = paste0(mess,"with fields:\nc( 'fid', 'date', 'major', 'landed', 'discard', 'POP', 'ORF', 'IRF' )")
			showError(mess,as.is=TRUE,x=0.05,adj=0,cex=1.2)
		}

		lenv=sys.frame(sys.nframe()) ## local environment
		cflds = c("landed","discard","POP","ORF","IRF")
		keep  = c("fid","date","major","minor",cflds)
		ufos  = c("POP","PAH","SBF","DOG","IRF","IRF","PAH","DOG","LIN")
		} ## end fold02
	} ## end if !sql.only
	## end skip if sql.only ------------------------------------------

	if (sql) { ## Start querying DFO databases
		.flush.cat("Start querying the DFO databases:\n")
		blob = list()
		if (isThere("PBSdat")) rm(PBSdat) ## remove from current environment
		uid=rep(uid,2)[1:2]; pwd=rep(pwd,2)[1:2]
		## -----------------------------------------------------------------------------
		## Start querying the databases
		## PacHarv3 catch summary for fids 1:5 and 0 (unknown)
		## Note: only used for h&l fisheries (2,4,5) as GFCATCH contains trawl and trap.
		## May need to refresh 'tnsnames.ora' in C:\Oracle\12.2.0\cli\network\admin
		## Switched to fixed SQL table PH3_CATCH_SUMMARY rather than use Oracle (RH 221222)
		## -----------------------------------------------------------------------------

		if (file.exists(paste0(catDir,"/ph3dat.rda")) ) { ##&& !sql.force)  ## Once should be enough unless sql query is altered
			.flush.cat("   loading 'ph3dat' from binary\n")
			load(paste0(catDir,"/ph3dat.rda"))
		} else {
			.flush.cat("   SQL Server -- GFFOS (table PH3_CATCH_SUMMARY);\n")  ## RH 221222
			getData("ph3_fcatORF.sql", dbName="GFFOS", strSpp=strSpp, path=spath, tenv=penv())
			#.flush.cat("   Oracle -- PacHarv3 (table CATCH_SUMMARY);\n")
			#getData("ph3_fcatORF.sql",dbName="HARVEST_V2_0",strSpp=strSpp,path=spath,
			#	server="FOS_V1_1.WORLD",type="ORA",trusted=FALSE,uid=uid[1],pwd=pwd[1],tenv=penv())
			#	server="ORAPROD.WORLD",type="ORA",trusted=FALSE,uid=uid[1],pwd=pwd[1],tenv=penv())
			assign("ph3dat",PBSdat);  rm(PBSdat) ## just to be safe
			dimnames(ph3dat)[[1]]=1:nrow(ph3dat)
			save("ph3dat",file="ph3dat.rda")
		}
		blob[["ph3dat"]] = ph3dat

		if (useGFM) {
			## ------------------------------------------------------------------------------------------------------
			## GFFOS merged catch from all fisheries (fid=1:5)
			## GF_MERGED_CATCH table contains records from GFBio, GFCatch, GFFOS, PacHarvest, PacHarvHL, PacHarvSable
			## ------------------------------------------------------------------------------------------------------
			if (file.exists(paste0(catDir,"/gfmdat.rda")) && !sql.force) {
				.flush.cat("   loading 'gfmdat' from binary\n")
				load(paste0(catDir,"/gfmdat.rda"))
			} else {
				.flush.cat("   SQL Server -- GFFOS (table GF_MERGED_CATCH) [takes ~ 5 minutes];\n")
				clearFiles("gfmdat.rda") ## backup in case sql.force=TRUE
				getData("fos_mcatORF.sql","GFFOS",strSpp=strSpp,path=spath,tenv=penv())
				PBSdat$Y[round(PBSdat$Y,5)==0] = NA
				PBSdat$X[round(PBSdat$X,5)==0] = NA
				PBSdat = calcStockArea(strSpp,PBSdat)
				assign("gfmdat",PBSdat); rm(PBSdat) ## just to be safe
				save("gfmdat",file="gfmdat.rda")
			}
			blob[["gfmdat"]] = gfmdat
		}
		else {
			## --------------------------------
			## GFCatch records for fids (1,3,5)
			## --------------------------------
			if (file.exists(paste0(catDir,"/gfcdat.rda")) && !sql.force)
				load(paste0(catDir,"/gfcdat.rda"))
			else {
				.flush.cat("   SQL Server -- GFCatch;\n")
				getData("gfc_fcatORF.sql","GFCatch",strSpp=strSpp,path=spath,tenv=penv())
				assign("gfcdat",PBSdat); rm(PBSdat) ## just to be safe
				#gfcdat$year=as.numeric(substring(gfcdat$date,1,4))  ## doesn't appear to be used
				dimnames(gfcdat)[[1]]=1:nrow(gfcdat)
				save("gfcdat",file="gfcdat.rda")
			}
			blob[["gfcdat"]] = gfcdat

			## -------------------------------------------------------------------------------------------------------------------------------
			## GFBioSQL records for fids (1) and TRIP_SUB_TYPE_CODE (5,7,8,9,10,12) -- 5=Canada, 7=Polish, 8-9=Russian, 10=Polish, 12=Japanese
			## -------------------------------------------------------------------------------------------------------------------------------
			.flush.cat("   SQL Server -- GFBioSQL (joint venture hake);\n")
			getData("gfb_jcatORF.sql","GFBioSQL",strSpp=strSpp,path=spath,tenv=penv())
				assign("jvhdat",PBSdat); rm(PBSdat) ## just to be safe
				save("jvhdat",file="jvhdat.rda")
				blob[["jvhdat"]] = jvhdat

			## -------------------------------
			## PacHarvest records for fids (1)
			## -------------------------------
			if (file.exists(paste0(catDir,"/phtdat.rda")) && !sql.force)
				load(paste0(catDir,"/phtdat.rda"))
			else {
				.flush.cat("   SQL Server -- PacHarvest (trawl);\n")
				getData("pht_tcatORF.sql","PacHarvest",strSpp=strSpp,path=spath,tenv=penv())
				assign("phtdat",PBSdat); rm(PBSdat) ## just to be safe
				save("phtdat",file="phtdat.rda")
			}
			blob[["phtdat"]] = phtdat

			## ---------------------------------------------------
			## PacHarvHL halibut validation records for fids (2,7)
			## ---------------------------------------------------
			if (file.exists(paste0(catDir,"/phhdat.rda")) && !sql.force)
				load(paste0(catDir,"/phhdat.rda"))
			else {
				.flush.cat("   SQL Server -- PacHarvHL (halibut DMP);\n")
				getData("phhl_hcatORF.sql","PacHarvHL",strSpp=strSpp,path=spath,tenv=penv())   ## validated (DMP) catch
				assign("phhdat",PBSdat); rm(PBSdat) ## just to be safe
				save("phhdat",file="phhdat.rda")
			}
			blob[["phhdat"]] = phhdat

			## ------------------------------------
			## PacHarvSable fisherlogs for fids (3)
			## ------------------------------------
			if (file.exists(paste0(catDir,"/phsdat.rda")) && !sql.force)
				load(paste0(catDir,"/phsdat.rda"))
			else {
				.flush.cat("   SQL Server -- PacHarvSable (fisherlogs);\n")
				getData("phs_scatORF.sql","PacHarvSable",strSpp=strSpp,path=spath,fisheryid=3,logtype="FISHERLOG",tenv=penv())
				assign("phsdat",PBSdat); rm(PBSdat) ## just to be safe
				save("phsdat",file="phsdat.rda")
			}
			blob[["phsdat"]] = phsdat

			## -------------------------------------------
			## PacHarvHL validation records for fids (4,5)
			## -------------------------------------------
			if (file.exists(paste0(catDir,"/phvdat.rda")) && !sql.force)
				load(paste0(catDir,"/phvdat.rda"))
			else {
				.flush.cat("   SQL Server -- PacHarvHL (rockfish DMP);\n")
				getData("phhl_vcatORF.sql","PacHarvHL",strSpp=strSpp,path=spath,tenv=penv())   ## validated (DMP) catch
				assign("phvdat",PBSdat); rm(PBSdat) ## just to be safe
				save("phvdat",file="phvdat.rda")
			}
			blob[["phvdat"]] = phvdat

			## ------------------------------------------
			## PacHarvHL fisherlog records for fids (4,5)
			## ------------------------------------------
			if (file.exists(paste0(catDir,"/phfdat.rda")) && !sql.force)
				load(paste0(catDir,"/phfdat.rda"))
			else {
				.flush.cat("   SQL Server -- PacHarvHL (rockfish fisherlogs);\n")
				getData("phhl_fcatORF.sql","PacHarvHL",strSpp=strSpp,path=spath,logtype="FISHERLOG",tenv=penv()) ## fisherlog catch
				assign("phfdat",PBSdat); rm(PBSdat) ## just to be safe
				save("phfdat",file="phfdat.rda")
			}
			blob[["phfdat"]] = phfdat

			## ---------------------------------------------------------------------------------
			## FOS catch from all fisheries (fid=1:5) -- now use GFFOS on SQL Server, not Oracle
			## ---------------------------------------------------------------------------------
			.flush.cat("   SQL Server -- GFFOS (table GF_D_OFFICIAL_FE_CATCH);\n")
			getData("fos_vcatORF.sql", dbName="GFFOS", strSpp=strSpp, path=spath, tenv=penv())
				#server="GFSH",type="ORA",trusted=FALSE,uid=uid[2],pwd=pwd[2])
				assign("fosdat",PBSdat); rm(PBSdat) ## just to be safe
				dimnames(fosdat)[[1]]=1:nrow(fosdat)
				save("fosdat",file="fosdat.rda")
				blob[["fosdat"]] = fosdat
		}
		## -----------------------------------------------------------------------------
		## Get the survey catches which will be added to the combined catch near the end
		## -----------------------------------------------------------------------------
		if (file.exists(paste0(catDir,"/gfbdat.rda")) && !sql.force) {
			.flush.cat("   loading 'gfbdat' from binary\n")
			load(paste0(catDir,"/gfbdat.rda"))
		} else {
			.flush.cat("   SQL Server -- GFBioSQL (surveys).\n")
#browser();return()
			surveyCatch(strSpp=strSpp, gfbdat=NULL, hadley=hadley, catDir=catDir, tabDir=tabDir, datDir=datDir, sql=TRUE)
			.flush.cat("   loading 'gfbdat' from binary\n")
			load(paste0(catDir,"/gfbdat.rda"))
		}
		blob[["gfbdat"]] = gfbdat

		## ------------------------------------------------------------
		## Wrap up the fisheries and survey landings into a list object
		## ------------------------------------------------------------
		eval(parse(text=paste0("dbdat=\"cat",strSpp,"dat\"")))
		expr=paste0(dbdat,"=blob; save(\"",dbdat,"\",file=\"",dbdat,".rda\")")
		eval(parse(text=expr))
		##-----Stop querying the databases-----
	} ## end sql
	else { ## Load binaries from previous queries
		dbdat=as.character(substitute(dbdat)) ## database list object name
		expr=paste0("getFile(",dbdat,",senv=ioenv,try.all.frames=TRUE,tenv=penv(),path=catDir,reload=TRUE); fnam=names(",dbdat,"); unpackList(",dbdat,")")
		eval(parse(text=expr)) 
	}
	if (sql.only) return()

	## -------------------------------
	## Remove seamounts if useSM=FALSE
	## -------------------------------
	if (!useSM){
		.flush.cat("Removing seamount records ...\n")
		nSMrec=as.list(rep(0,length(fnam))); names(nSMrec)=fnam; tSMcat=nSMrec
		for (i in fnam){
			eval(parse(text=paste0("idat = zapSeamounts(",i,")")))
			SMrem = attributes(idat)$SMrem
#if (i==fnam[2]) {browser();return()}
			if (!is.null(SMrem)) {  ## gymnastics to avoid dimensions with one element (e.g., year=1981)
				nSMrec[[i]] = array(SMrem[,,"nrec"], dim=dim(SMrem)[1:2], dimnames=dimnames(SMrem)[1:2])
				tSMcat[[i]] = array(SMrem[,,"tcat"], dim=dim(SMrem)[1:2], dimnames=dimnames(SMrem)[1:2])
			}
			assign(i,idat)
		}
		packList(c("nSMrec","tSMcat"),"CR",tenv=.PBStoolEnv)
	}

	## -----------------------------------------------------------------------------------
	## For expediency add ORF and POP together to get TRF; also get year if it's not there
	## -----------------------------------------------------------------------------------
	for (i in setdiff(fnam,"gfbdat")){
		eval(parse(text=paste0("idat = ",i)))
		idat$TRF = idat$ORF + idat$POP
		if (!is.element("year",names(idat)) & is.element("date",names(idat)))
			idat$year = as.numeric(substring(idat$date,1,4))
		assign(i,idat)
	}
	## -------------------------------------------------------------------------
	## Remove Langara Spit trawl catch of orfSpp (ORF,POP,or TRF) if useLS=FALSE
	## Note: not many records coded minor=35
	## -------------------------------------------------------------------------
	if (!useLS){
		.flush.cat(paste0("Removing Lanagara Spit trawl catch of ",orfSpp," ...\n"))
		for (i in setdiff(fnam,"gfbdat")){
			eval(parse(text=paste0("idat = ",i)))
			isTY = is.element(idat$fid,1) & is.element(idat$year,1983:1993) ## Langara Spit experiment years (from Leaman and IFMPs)
			isLS = (is.element(idat$major,8) & is.element(idat$minor,3) & is.element(idat$locality,3)) |
			       (is.element(idat$major,9) & is.element(idat$minor,35) & is.element(idat$locality,c(1,2,4:7)))
			if (any(isTY & isLS)) {
				LSdat = idat[isTY & isLS,]
				LScat = sapply(split(LSdat[,orfSpp],LSdat$year),sum,na.rm=TRUE)/1000.
				idat[isTY & isLS, orfSpp] = 0.
			} else LScat="none"
			attr(idat,paste0("LScat.rm",orfSpp)) = LScat
#if (i=="gfmdat") {browser();return()}
			assign(i,idat)
		}
		packList(c("LScat"),"CR",tenv=.PBStoolEnv)
	}
	## ---------------------------------------------
	## Transfer Anthony Island catch to PMFC 5C catch
	## ---------------------------------------------
	if (useAI && any(strSpp==c("396","440"))){
		#if ("gfmdat" %in% fnam) {
		for (i in fnam) {
			.flush.cat(i, ": expanding 5C to include Flamingo/Anthony in 5E and Moresby Gully in 5B...\n")
			eval(parse(text=paste0("idat = ",i)))
			idat = expand5C(idat)
			assign(i,idat)
#if(i=="gfmdat"){browser();return()}
		}
	}
#	if (!sql | !useSM){ ## wtf?
	if (file.exists(paste0(datDir,"/gfbcat.rda"))) {
		load(paste0(datDir,"/gfbcat.rda"))
	} else {
		gfbcat = surveyCatch(strSpp=strSpp, gfbdat=gfbdat, hadley=hadley, catDir=catDir, tabDir=tabDir, datDir=datDir, sql=FALSE)
	}

	.flush.cat("Start consolidating landings records:\n")

	## -----------------------------------------
	## Consolidate PacHarv3 records (fid=c(1:5))
	## Updated by RH 191007 to put:
	##   POP+ORF into TAR for FID 1
	##   DOG+LIN into TAR for FID 4
	## -----------------------------------------
	.flush.cat("   PacHarv3 records ...\n")
	ph3cat = as.data.frame(t(apply(ph3dat,1,function(x){
		ufos = c("POP","ORF","PAH","SBF","DOG","LIN","IRF")
		ufid = c(1,1,2,3,4,4,5); names(ufid) = ufos
		f = x["fid"]
		if (f==0) { 
			z = x[ufos]==max(x[ufos],na.rm=TRUE)
			utar = ufos[z][1]
			fid = ufid[utar]
			ff  = names(ufid)[grep(fid,ufid)]
			ucat = sum(x[ff],na.rm=TRUE)
		}
		else {
			fid = f
			ff  = names(ufid)[grep(fid,ufid)]
			ucat = sum(x[ff],na.rm=TRUE)
		}
		out = c(x["year"],fid,date=as.Date(paste0(x["year"],"-07-01")),
			x[c("major","minor","landed","discard","POP","ORF")],ucat)
		return(out)
	} )))
	names(ph3cat) = c("year",keep)
	ph3cat$date = as.Date(paste0(ph3cat$year,"-07-01"))
	## -----------------------------------------------------------------------------------
	## Sabotage ph3cat object to remove all trawl records & non-trawl records from 2006 on
	## -----------------------------------------------------------------------------------
	ph3cat = ph3cat[is.element(ph3cat$fid, c(2:5)),]
	ph3cat = ph3cat[ph3cat$year<2006 & !is.na(ph3cat$year),]
	ph3cat = ph3cat[,-1] ## get rid of 'year'
	save("ph3cat",file=paste0(datDir,"/ph3cat.rda"))

	if (useGFM){ ## use GF_MERGED_CATCH in GFFOS
		.flush.cat("   GFFOS merged catch ...\n")
		dbs = c("ph3cat","gfmcat")
		## ------------------------------------------------------------------------
		## Databases to compare and merge (using maximum)
		## drop PacHarv3 for Trawl and Trap (see Rutherford 1999)
		## Note: PH3 for modern catch has been sabotaged (intentionally):
		## PH3 records for FID 1 are removed,
		## PH3 records for FID 5 appear to be redundant to those in GFM (RH 190926)
		## ------------------------------------------------------------------------
		dbmerge = list(
			trawl     = c("gfmcat"),
			halibut   = NULL,
			sablefish = NULL,
			dogling   = NULL,
			hlrocks   = c("ph3cat","gfmcat"))
		## ------------------------------------------------------------------------------
		## Databases that are additive (e.g. J-V Hake, BUT already in merged catch table)
		## Note: PH3 for modern catch has been sabotaged (intentionally):
		## PH3 records for FIDs 2:4 appear to be additive to those in GFM (RH 190926)
		## ------------------------------------------------------------------------------
		dbadd = list (
			trawl     = NULL,
			halibut   = c("ph3cat","gfmcat"),
			sablefish = c("ph3cat","gfmcat"),
			dogling   = c("ph3cat","gfmcat"),
			hlrocks   = NULL)

		## ----------------------------------------------------------------------------------------
		## GF_MERGED_CATCH  contains foreign catch, represented here as fid=9.
		## For some species like WWR, this can be substantial; assign to fids 1-5 based on gear.
		## 1=bottom trawl, 2=trap, 3=midwater trawl, 4=h&l, 5=longline, 8=h&l/longline/trap
		## ----------------------------------------------------------------------------------------
		ff9 = is.element(gfmdat$fid,9)
		if (any(ff9)) {
			if (useFF) {
				gear9 = gfmdat$gear[ff9]
				fid9  = rep(0,length(gear9))
				fid9[is.element(gear9,c(1,3))] = 1  ## put bottom and midwater trawl into trawl
				fid9[is.element(gear9,c(2,8))] = 3  ## put trap and line-trap mix into sablefish
				fid9[is.element(gear9,c(4))]   = 5  ## put h&l into h&l rockfish
				fid9[is.element(gear9,c(5))]   = 2  ## put longline into halibut
				gfmdat$fid[ff9] = fid9
			} else {
				gfmdat = gfmdat[!ff9,]  ## remove foreign catch reported by GFBio
			}
		}
		gfmcat = gfmdat[,keep]
		trash  = apply(gfmcat[,cflds],1,function(x){all(x==0)})
		gfmcat = gfmcat[!trash,]; dimnames(gfmcat)[[1]]=1:nrow(gfmcat)
		save("gfmcat",file=paste0(datDir,"/gfmcat.rda"))
#browser();return()
	} ## end GFM
	else { ## query multiple database tables [DEPRECATED]
		dbs = c("ph3cat","gfccat","phtcat","phhcat","phscat","phvcat","phfcat","foscat","jvhdat")
		## ------------------------------------------------------
		## Databases to compare and merge (using maximum)
		## drop PacHarv3 for Trawl and Trap (see Rutherford 1999)
		## ------------------------------------------------------
		dbmerge = list(
			trawl     = c("gfccat","phtcat","foscat"),
			halibut   = c("ph3cat","phhcat","phvcat","foscat"),
			sablefish = c("gfccat","phscat","foscat"),
			dogling   = c("ph3cat","phvcat","phfcat","foscat"),
			hlrocks   = c("ph3cat","gfccat","phvcat","phfcat","foscat"))
		## -----------------------------------------------------
		## Databases that are strictly additive (e.g., J-V Hake)
		## -----------------------------------------------------
		dbadd = list (trawl="jvhdat", halibut=NULL, sablefish=NULL, dogling=NULL, hlrocks=NULL)

		## ------------------------------------------
		## Consolidate GFCatch records (fid=c(1,3,5))
		## ------------------------------------------
		.flush.cat("   GFCatch catch ...\n")
		gfccat = gfcdat
		gfccat$TAR = rep(0,nrow(gfccat))
		for (i in 1:5) {
			ii = is.element(gfccat$fid,i)
			if (any(ii)) gfccat$TAR[ii] = gfccat[,ufos[i]][ii]
		}
		gfccat = gfccat[,keep]
		trash  = apply(gfccat[,cflds],1,function(x){all(x==0)})
		gfccat = gfccat[!trash,] ; dimnames(gfccat)[[1]] = 1:nrow(gfccat)
		save("gfccat",file=paste0(datDir,"/gfccat.rda"))

		## ---------------------------------------
		## Consolidate PacHarvest landings (fid=1)
		## ---------------------------------------
		.flush.cat("   PacHarvest catch ...\n")
		phtcat = phtdat[,keep]
		trash  = apply(phtcat[,cflds],1,function(x){all(x==0)})
		phtcat = phtcat[!trash,] ; dimnames(phtcat)[[1]] = 1:nrow(phtcat)
		save("phtcat",file=paste0(datDir,"/phtcat.rda"))

		## --------------------------------------------
		## Consolidate GFBioSQL JV Hake bycatch (fid=1)
		## --------------------------------------------
		.flush.cat("   JV Hake bycatch ...\n")
		jvhcat = jvhdat[,keep]
		trash  = apply(jvhcat[,cflds],1,function(x){all(x==0)})
		jvhcat = jvhcat[!trash,] ; dimnames(jvhcat)[[1]] = 1:nrow(jvhcat)
		save("jvhcat",file=paste0(datDir,"/jvhcat.rda"))

		## ---------------------------------------------
		## Consolidate PacHarvHL halibut bycatch (fid=2)
		## ---------------------------------------------
		.flush.cat("   PacHarvHL halibut bycatch ...\n")
		phhcat = phhdat
		phhcat$TAR = phhcat$PAH
		phhcat = phhcat[,keep]
		phhcat$fid = rep(2,nrow(phhcat))  ## because there are a few fid=7 (halibut + sablefish)
		trash  = apply(phhcat[,cflds],1,function(x){all(x==0)})
		phhcat = phhcat[!trash,] ; dimnames(phhcat)[[1]] = 1:nrow(phhcat)
		save("phhcat",file=paste0(datDir,"/phhcat.rda"))

		## -----------------------------------------
		## Consolidate PacHarvSable landings (fid=3)
		## -----------------------------------------
		.flush.cat("   PacHarvSable catch ...\n")
		phscat = phsdat[,keep]
		trash  = apply(phscat[,cflds],1,function(x){all(x==0)})
		phscat = phscat[!trash,] ; dimnames(phscat)[[1]] = 1:nrow(phscat)
		save("phscat",file=paste0(datDir,"/phscat.rda"))

		## --------------------------------------------------------
		## Consolidate PacHarvHL validation landings (fid=c(2,4,5))
		## --------------------------------------------------------
		.flush.cat("   PacHarvHL validation landings ...\n")
		phvcat = phvdat
		phvcat$TAR = rep(0,nrow(phvcat))
		for (i in 1:9) { ## 2=Halibut, 3=Sablefish, 4=Schdeule II, 5=ZN, 6=Sablefish+ZN, 7=Sablefish+Halibut, 8=Dogfish, 9=Lingcod
			ii = is.element(phvcat$fid,i)
			if (any(ii)) {
				phvcat$TAR[ii] = phvcat[,ufos[i]][ii]
				if (i==4) phvcat$TAR[ii] = phvcat$TAR[ii] + phvcat$LIN[ii] ## add lingcod to dogfish if Sched II
				if (i==6) phvcat$fid[ii] = 5                               ## Sablefish/ZN
				if (i==7) phvcat$fid[ii] = 2                               ## Sablefish/Halibut
				if (any(i==c(8,9))) phvcat$fid[ii] = 4                     ## Dogfish or lingcod
			}
		}
		phvcat = phvcat[,keep]
		trash  = apply(phvcat[,cflds],1,function(x){all(x==0)})
		phvcat = phvcat[!trash,] ; dimnames(phvcat)[[1]] = 1:nrow(phvcat)
		save("phvcat",file=paste0(datDir,"/phvcat.rda"))

		## ----------------------------------------------------
		## Consolidate PacHarvHL fisherlog records (fid=c(4,5))
		## ----------------------------------------------------
		.flush.cat("   PacHarvHL fisherlog catch ...\n")
		phfcat = phfdat[,keep]
		trash=apply(phfcat[,cflds],1,function(x){all(x==0)})
		phfcat=phfcat[!trash,]; dimnames(phfcat)[[1]]=1:nrow(phfcat)
		save("phfcat",file=paste0(datDir,"/phfcat.rda"))

		## -----------------------------------
		## Consolidate GFFOS records (fid=1:5)
		## -----------------------------------
		.flush.cat("   GFFOS official catch ...\n")
		z = fosdat$date >= as.POSIXct("2000-01-01") & fosdat$date <= Sys.time()  ## up to the current date
		#z = fosdat$date >= as.POSIXct("2000-01-01") & fosdat$date <= as.POSIXct("2010-06-30") ## for POP assessment
		foscat = fosdat[z,keep]
		trash=apply(foscat[,cflds],1,function(x){all(x==0)})
		foscat=foscat[!trash,]; dimnames(foscat)[[1]]=1:nrow(foscat)
		save("foscat",file=paste0(datDir,"/foscat.rda"))
	}  ## end multi-DB

	modyrs = majmod = fid = NULL
	for (i in dbs) { 
		if(!isThere(i,envir=lenv)) next
		icat = get(i)
		if ("date" %in% colnames(icat))
			modyrs = c(modyrs,.su(as.numeric(substring(icat$date,1,4))))
		else if ("year" %in% colnames(icat))
			modyrs = c(modyrs,.su(as.numeric(icat$year)))
		else
			modyrs = c(modyrs, 1951:sysyr)
		majmod = c(majmod,.su(icat$major))
		fid=c(fid,unique(icat$fid))
	}
	modyrs = .su(modyrs); majmod=.su(majmod); fid=.su(fid)
	modyrs = modyrs[is.element(modyrs,1945:sysyr)]

	if (isThere("refyrs") && !is.null(refyrs) && any(sapply(lapply(refyrs,intersect,modyrs),length)==0)) ## refyrs now a list (RH 190917)
		showError("refyrs","nodata") 
	MODYRS = modyrs[1]:modyrs[length(modyrs)]  ## Basically years for the catmod (GFM+PH3) DB catch

	## -------------------------------------------------------------
	## Need to reconcile majors (remove concept of reference majors)
	## -------------------------------------------------------------
	majmax=intersect(majhis,majmod)  ## maximum available overlap in majors from data
	if (is.null(major)) {
		MM.avail = majmax
		major = c(1,3:9)
	}
	else 
		MM.avail = intersect(major, majmax)
	MM = c(1,3:9) # always use coastwide set
	mm = as.character(MM)
	Cflds=c("landed","discard","POP","ORF","TRF","TAR")

	## -----------------------------------------------------------------------------------
	## Collect repcat landings (t), including those in unknown areas from database queries
	## -----------------------------------------------------------------------------------
	.flush.cat("Collecting repcat landings, including those in unknown areas (catmod0) ...\n")
	catmod0 = array(0, dim=c(length(MODYRS), length(majmod), length(fid), length(Cflds), length(dbs)), dimnames=list(year=MODYRS, major=majmod, fid=fid, catch=Cflds, dbs=dbs))
	for (a in dbs) {
		if(!isThere(a,envir=lenv)) next
		acat = get(a)  ## ph3cat & gfmcat
		acat$year = as.numeric(substring(acat$date,1,4))
		acat = acat[is.element(acat$year,MODYRS),] # to remove anomalous years like 1900
		acat$TRF  = acat[["POP"]] + acat[["ORF"]]  ## total rockfish
		if (is.null(acat$discard)) acat$discard = rep(0,nrow(acat))
		for (k in fid) {
			kk=as.character(k)
			kdat=acat[is.element(acat$fid,k),]
#if (kk=="5" && a=="gfmcat") {browser();return()}
			if(nrow(kdat)==0) next
			for (j in majmod) {
				jj=as.character(j)
				jdat=kdat[is.element(kdat$major,j),]
				if(nrow(jdat)==0) next
				landed=sapply(split(jdat$landed,jdat$year),sum,na.rm=TRUE)/1000.
				POP=  sapply(split(jdat$POP,jdat$year),sum,na.rm=TRUE)/1000.
				ORF=  sapply(split(jdat$ORF,jdat$year),sum,na.rm=TRUE)/1000.
				TRF=  sapply(split(jdat$TRF,jdat$year),sum,na.rm=TRUE)/1000.
				## Tentatively set TAR to the 5 Inshore Rockfish species but 
				## see delta ratios for which TAR is actually used (search for [mirror1])
				TAR=  sapply(split(jdat$IRF,jdat$year),sum,na.rm=TRUE)/1000.
#print(c(names(landed),jj,kk,a))
#if (jj=="1" && kk=="4" && a=="gfmcat") {browser();return()}

				catmod0[names(landed),jj,kk,"landed",a] = landed
				catmod0[names(POP),jj,kk,"POP",a]       = POP
				catmod0[names(ORF),jj,kk,"ORF",a]       = ORF
				catmod0[names(TRF),jj,kk,"TRF",a]       = TRF
				catmod0[names(TAR),jj,kk,"TAR",a]       = TAR
				if (!is.null(jdat$discard)){
					discard=sapply(split(jdat$discard,jdat$year),sum,na.rm=TRUE)/1000.
					catmod0[names(discard),jj,kk,"discard",a] = discard }
			} ## j loop (majors)
		} ## end k loop (fids)
	} ## end a loop (dbs)

	if (diagnostics){
		for (spp in dimnames(catmod0)$catch) {
			for (db in dimnames(catmod0)$dbs) {
				plotDiag(apply(catmod0[,mm,,spp,db],c(1,3),sum), paste0("catmod0 ",spp," in ",db), col=clrs.fishery)
			}
			plotDiag(apply(catmod0[,mm,,spp,],c(1,3),sum), paste0("catmod0 ",spp," in all databases"), col=clrs.fishery)
		}
	}

	## ----------------------------------------------------------------------------------
	## Allocate DB landings (t) from unknown major (code=0) to user-specified majors (mm)
	## ----------------------------------------------------------------------------------
	.flush.cat("Allocating landings from unknown major (code=0) to user-specified majors (catmod1) ...\n")
	catmod1 = catmod0[,is.element(dimnames(catmod0)[[2]],mm),,,,drop=FALSE]
	if (is.element("0",dimnames(catmod0)$major)) {
		for (aa in dimnames(catmod1)$dbs) {          ## databases
			for (kk in dimnames(catmod1)$fid) {       ## fishery IDs
				for (ll in dimnames(catmod1)$catch) {  ## catch categories
					## -------------------------------------------------------------------------------------
					## Allocate based on mean proportions in each major observed over a number of years (>2)
					## -------------------------------------------------------------------------------------
					cattab = catmod1[,mm,kk,ll,aa]
					catobs = apply(cattab,1,function(x){length(x[x>0])})
					catyrs = names(catobs)[catobs>2]
					if (length(catyrs)==0) next
					pmaj   = apply(apply(cattab[catyrs,,drop=FALSE],1,pcalc),1,mean)  ## do not use geometric mean here (proportion allocation) !!!
					allo   = t(t(catmod0[,"0",kk,ll,aa]))%*%pmaj; dimnames(allo)[[2]] = mm
#if(aa=="gfmcat" && kk=="1" && ll=="landed") {browser();return()}
					catmod1[,mm,kk,ll,aa] = catmod1[,mm,kk,ll,aa] + allo 
				} ## end ll loop (catch)
			} ## kk loop (fid)
		} ## end aa loop(dbs)
	} ## end if
#browser(); return()

	if (diagnostics){
		for (spp in dimnames(catmod1)$catch) {
			for (db in dimnames(catmod1)$dbs) {
				plotDiag(apply(catmod1[,mm,,spp,db],c(1,3),sum), paste0("catmod1 ",spp," in ",db), col=clrs.fishery)
			}
			plotDiag(apply(catmod1[,mm,,spp,],c(1,3),sum), paste0("catmod1 ",spp," in all databases"), col=clrs.fishery)
		}
	}

	## -------------------------------------------------------------------
	## Allocate DB landings (t) from unknown fid (code=0) to standard fids (1:5)
	## -------------------------------------------------------------------
	.flush.cat("Allocating landings from unknown fid (code=0) to standard fids (catmod2) ...\n")
	catmod2 = catmod1[,,is.element(dimnames(catmod1)[[3]],1:5),,,drop=FALSE]
	if (is.element("0",dimnames(catmod1)$fid)) {
		for (aa in dimnames(catmod2)$dbs) {          ## databases
			for (jj in dimnames(catmod2)$major) {     ## major IDs
				kk = as.character(1:5)                 ## fishery IDS
				for (ll in dimnames(catmod2)$catch) {  ## catch categories
					## -------------------------------------------------------------------------------------
					## Allocate based on mean proportions in each major observed over a number of years (>2)
					## -------------------------------------------------------------------------------------
					cattab = catmod2[,jj,kk,ll,aa]
					catobs = apply(cattab,1,function(x){length(x[x>0])})
					catyrs = names(catobs)[catobs>2]
#if(aa=="gfmcat" && jj=="3" && ll=="POP") {browser();return()}
					if (length(catyrs)==0) next
					pfid = apply(apply(cattab[catyrs,,drop=FALSE],1,pcalc),1,mean)   ## do not use geometric mean here (proportion allocation) !!!
					allo   = t(t(catmod1[,jj,"0",ll,aa]))%*%pfid; dimnames(allo)[[2]] = kk
					catmod2[,jj,kk,ll,aa] = catmod2[,jj,kk,ll,aa] + allo 
				} ## end ll loop (catch)
			} ## end jj loop (major)
		} ## end aa loop (dbs)
	} ## end if (fid)
	fid = as.numeric(dimnames(catmod2)$fid) ## otherwise fid=0 continues to screw up code later.

	if (diagnostics){
		for (spp in dimnames(catmod2)$catch) {
			for (db in dimnames(catmod2)$dbs) {
				plotDiag(apply(catmod2[,mm,,spp,db],c(1,3),sum), paste0("catmod2 ",spp," in ",db), col=clrs.fishery)
			}
			plotDiag(apply(catmod2[,mm,,spp,],c(1,3),sum), paste0("catmod2 ",spp," in all databases"), col=clrs.fishery)
		}
	}

	## -------------------------------------------------------------------------
	## Merge modern DB landings from various databases (ie, collapse DB sources)
	## -------------------------------------------------------------------------
	.flush.cat("Merging modern landings from various databases (catmod) ...\n")
	catmod = array(0, dim=rev(rev(dim(catmod2))[-1]), dimnames=rev(rev(dimnames(catmod2))[-1]))
	ii = dimnames(catmod)$year
	jj = dimnames(catmod)$major
	ll = dimnames(catmod)$catch
	for (kk in dimnames(catmod)$fid) {  ## fishery IDs
		k = as.numeric(kk)
#if (k==2) {browser();return()}
		fcat = 0
		if (!is.null(dbmerge[[k]])) {
			fcat = apply(catmod2[ii,jj,kk,ll,dbmerge[[k]],drop=FALSE],1:4,max)
		}
		if (!is.null(dbadd[[k]])) {
			xcat = apply(catmod2[ii,jj,kk,ll,dbadd[[k]],drop=FALSE],1:4,sum)
			fcat = fcat + xcat
		}
		catmod[ii,jj,kk,ll] = fcat
		if (!useGFM) {
			## ----------------------------------------------------------------------------
			## adjust for quirks in DB transitions (when using individual database sources)
			## ----------------------------------------------------------------------------
			if (any(k==c(1,3))) {
				if (k==1) { iii="2007"; aaa=c("phtcat","foscat") }
				if (k==3) { iii="2006"; aaa=c("phscat","foscat") }
				qcat=apply(catmod2[iii,jj,kk,ll,aaa,drop=FALSE],1:4,sum) }
			if (any(k==c(2,4,5))) {
				iii = "2006"
				if (k==2) { aaa=c("phhcat","phvcat","foscat") }
				if (any(k==c(4,5))) { aaa=c("phvcat","phfcat","foscat") }
				qcat=apply(catmod2[iii,jj,kk,ll,aaa,drop=FALSE],1:4,function(x){ max(x[1:2]) + x[3] }) }
			catmod[iii,jj,kk,ll] = qcat
		} ## end if (not GFM)
#if (k==4) {browser();return()}
	} ## end kk loop (fid)

	## ------------------------------
	## Make sure that TRF = ORF + POP
	## ------------------------------
	.flush.cat("Ensuring that TRF = ORF + POP ...\n")
	TOPmod = apply(catmod[,,,c("POP","ORF","TRF")],1:3,function(x){
		if(round(x[1]+x[2],5)==round(x[3],5)) return(x)
		else {
			x[3] = max(x[3],x[1]+x[2]) ## ensure TRF is the largest value
			x[1] = x[3] - x[2]         ## assume ORF is now correct, POP is residual
			return(x) } })
	kk = dimnames(catmod)$fid
	for (ll in c("POP","ORF","TRF"))
		catmod[ii,jj,kk,ll] = TOPmod[ll,ii,jj,kk]

	## ------------------------------------------------------------------------------------------
	## If suplementary RRF landings were supplied by `rrfhistory', incorporate them into `catmod'
	## ------------------------------------------------------------------------------------------
	if (!is.null(rrfbase)) {
		.flush.cat("Incorporating suplementary RRF catch `rrfhistory' into `catmod' ...\n")
		ii = intersect(dimnames(catmod)$year,dimnames(rrfbase)$year)
		jj = intersect(dimnames(catmod)$major,dimnames(rrfbase)$major)
		kval = intersect( fidnam[as.numeric(dimnames(catmod)$fid)], dimnames(rrfbase)$fishery)
		for (k in fid) { 
			if (!is.element(fidnam[k],kval)) next
			kk  = as.character(k)
			kkk = fidnam[k] ## used to index `rrfbase', `rrfadd', and `rrfmax'

			## --------------------------------------------------------------------------------------------
			## Choose the maximum of RRF landings reported by DBs (e.g., PH3, GFM) and 
			##   RRF landings from non-DB sources (e.g., Polish fleet in Ketchen 80) supplied in rrfhistory
			## --------------------------------------------------------------------------------------------
			if (!is.null(rrfmax)) {
				rrfcat = array(0, dim=dim(rrfmax[ii,jj,kkk,])[1:2], dimnames=dimnames(rrfmax[ii,jj,kkk,])[1:2])
				for (nn in dimnames(rrfmax)$nation)
					rrfcat = rrfcat + rrfmax[ii,jj,kkk,nn]
				zrrf  = (rrfcat[ii,jj] - catmod[ii,jj,kk,"landed"]) > 0
				catmod[ii,jj,kk,"landed"][zrrf] = rrfcat[ii,jj][zrrf]
			}
			if (!is.null(rrfadd)){
				rrfcat = array(0, dim=dim(rrfadd[ii,jj,kkk,])[1:2], dimnames=dimnames(rrfadd[ii,jj,kkk,])[1:2])
				for (nn in dimnames(rrfadd)$nation)
					rrfcat = rrfcat + rrfadd[ii,jj,kkk,nn]
				catmod[ii,jj,kk,"landed"] = catmod[ii,jj,kk,"landed"] + rrfcat[ii,jj]
			}
		} ## end k loop (fid)
	} ## end if (rrfbase)
	## Save DB (+RRF supplemental) landings to binary file 'cat123mod'
	expr = paste0("cat",strSpp,"mod=catmod; save(\"cat",strSpp,"mod\", file=\"",datDir,"/cat",strSpp,"mod.rda\")")
	eval(parse(text=expr))

	if (diagnostics){
		for (spp in dimnames(catmod)$catch) {
			plotDiag(apply(catmod[,mm,,spp],c(1,2),sum), paste0("catmod ",spp," by major"), col=clrs.fishery)
			plotDiag(apply(catmod[,mm,,spp],c(1,3),sum), paste0("catmod ",spp," by fishery"), col=clrs.fishery)
		}
	}

	## ----------------------------------------------------
	## Get three historical time lines : trawl, trap, & H&L
	## Consolidate previously computes catches
	##   (either summed or taking the maximum).
	## ----------------------------------------------------
	.flush.cat("Getting three historical time lines : trawl, trap, & H&L ...\n")
	tmp0 = orfmax[,mm,,,,drop=FALSE]
	## initialize the array for landings of  POP, ORF, and TRF
	allhis = array(0, dim=dim(tmp0), dimnames=dimnames(tmp0))
	for (l in dimnames(allhis)$catch) {
		for (k in fsh) {
			tmp1 = orfmax[,,,k,l]
			tmp1 = tmp1[,mm,,drop=FALSE]  ## use only specified majors
			tmp2 = orfadd[,,,k,l]
			tmp2 = tmp2[,mm,,drop=FALSE]  ## use only specified majors
			allhis[,,,k,l] = tmp1 + tmp2
		} ## end k loop (fsh)
	} ## end l loop (catch)

	if (diagnostics){
		for (spp in dimnames(allhis)$catch) {
			plotDiag(apply(allhis[,mm,,,spp],c(1,2),sum), paste0("allhis ",spp," by major"), col=clrs.major[mm])
			plotDiag(apply(allhis[,mm,,,spp],c(1,3),sum), paste0("allhis ",spp," by fishery"))
		}
	}

	## ------------------------------------------------------------
	## Isolate landings of the reference catch (usually ORF or TRF)
	## ------------------------------------------------------------
	orfhis = allhis[,,,,orfSpp]
	for (K in dimnames(orfhis)$fishery){
		orfcat = orfhis[,,,K] ## historical landed catch of the rockfish group (ORF of TRF) used to estimate RRF
		if (ascii.tables) {
			for (N in dimnames(orfhis)$nation)
				write.csv(orfcat[,,N],paste0(tabDir,"/",orfSpp,"-Landed-Historic-fishery-",N,"-",K,"-",numdate,".csv"))
		}
	} ## end K loop (fishery)
	if (saveinfo)
		packList(c("catmod","catmod0","catmod1","catmod2","MM.avail","MM","mm","allhis"),"CR",tenv=.PBStoolEnv)

	## ------------------------------------------------------
	## Terminate here if all you want are the modern landings
	## ------------------------------------------------------
	.flush.cat("-----Finished collecting the historical and modern landings-----\n\n")
	if (!reconstruct)
		return(list(catmod0=catmod0, catmod1=catmod1, catmod2=catmod2, catmod=catmod)) 

	fold03 = TRUE ## ----- only used for code folding -----
	if (fold03) { ## 3. Calculate ratios
	## ===================
	## 3. Calculate ratios
	## ===================

	## ----------------------------------------
	## Extract catch (ctab) for reference catch
	## ----------------------------------------
	.flush.cat("Start calculating ratios:\n")

	#if (isThere("refyrs") && !is.null(refyrs) ) 
	if (isThere("REFYRS") && !is.null(REFYRS) ) 
		ctab = catmod[as.character(REFYRS),,,,drop=FALSE] 
	else ctab = catmod

	## ---------------------------------------------------------------------------------
	## Revise reference catch based on user-suppplied areas (e.g., B.Mose via P.J.Starr)
	## ********* USE BOTTOM TRAWL ONLY -- not implemented yet  *************
	## ---------------------------------------------------------------------------------
	.flush.cat("Collecting reference data ...\n")
	RAWDAT = REFDAT = list()
	aflds = c("major","minor","locality")
	for (kk in dimnames(catmod)$fid) {
		k = as.numeric(kk)
		if (useGFM) rawdat = gfmdat[is.element(gfmdat$fid,k),]
		else {
			if (k==1) {
				tmpdat = gfcdat[is.element(gfcdat$fid,1),]
				tmpdat$TAR = tmpdat$IRF
				kfld = intersect(intersect(names(tmpdat),names(phtdat)),names(fosdat))
				rawdat = rbind(tmpdat[,kfld],phtdat[,kfld],fosdat[is.element(fosdat$fid,1),kfld])
			} else if (k==2) {
				kfld = intersect(names(phhdat),names(phvdat))
				tmpdat = rbind(phhdat[,kfld],phvdat[,kfld])
				tmpdat$TAR = tmpdat$PAH
				kfld = intersect(names(tmpdat),names(fosdat))
				rawdat = rbind(tmpdat[,kfld],fosdat[is.element(fosdat$fid,2),kfld])
			} else if (k==3) {
				tmpdat = gfcdat[is.element(gfcdat$fid,3),]
				tmpdat$TAR = tmpdat$SBF
				kfld = intersect(intersect(names(tmpdat),names(phsdat)),names(fosdat))
				rawdat = rbind(tmpdat[,kfld],phsdat[,kfld],fosdat[is.element(fosdat$fid,3),kfld])
			} else if (k==4) {
				tmpdat = phvdat[is.element(phvdat$fid,4),]
				tmpdat$TAR = tmpdat$DOG
				kfld = intersect(intersect(names(tmpdat),names(phfdat)),names(fosdat))
				rawdat = rbind(tmpdat[,kfld],phfdat[is.element(phfdat$fid,4),kfld],fosdat[is.element(fosdat$fid,4),kfld])
			} else if (k==5) {
				tmpda1 = gfcdat[is.element(gfcdat$fid,5),]
				tmpda1$TAR = tmpda1$IRF
				tmpda2 = phvdat[is.element(phvdat$fid,5),]
				tmpda2$TAR = tmpda2$IRF
				kfld = intersect(intersect(intersect(names(tmpda1),names(tmpda2)),names(phfdat)),names(fosdat))
				rawdat = rbind(tmpda1[,kfld],tmpda2[,kfld],phfdat[is.element(phfdat$fid,5),kfld],fosdat[is.element(fosdat$fid,5),kfld])
			}
		}
		rawdat$TRF    = rawdat$ORF + rawdat$POP
		## TAR now called 'IRF' in gfmdat; defined by species and fishery sector:
		##   TRF in GF|JV trawl; PAH in halibut|halibut+sable|K/L; SBF in Sablefish; DOG in Spiny Dogfish
		##   LIN in Lingcod; DOG+LIN in SchedII; QBR+CPR+CHR+TIR+YYR in Rockfish Inside|RF Outside|ZN|K/ZN
		rawdat$TAR    = rawdat$IRF
		rawdat$aindex = .createIDs(rawdat,aflds)  ## .createIDs currently not exported from PBSmapping namespace
		RAWDAT[[kk]]  = refdat = rawdat
		refdat$year   = as.numeric(substring(refdat$date,1,4))
		refdat        = refdat[is.element(refdat$year,REFYRS),]  ## use flattened refyrs (can be a mixture of years by FID)
		## Oberverlogs not really necessary for gamma calculation but may want to use for trawl.
		## Other fisheries don't always have observerlogs in the merged catch table (e.g., Halibut)
		## RH decided to leave as is (RH: 190910)
		#refdat        = refdat[is.element(refdat$log,1),]
		REFDAT[[kk]]  = refdat
	}

	## --------------------------------------------------
	## Get rid of reference data
	##   not in reference localities using reference gear
	## --------------------------------------------------
	if ( !is.null(refarea) || !is.null(refgear) ) {
		if ( !is.null(refarea) && !is.null(refgear) ) {
			## -----------------------------------------------------
			## Get rid of reference data
			##   not in reference localities that use reference gear
			## -----------------------------------------------------
			.flush.cat("Qualifying ref data by reference area & gear ...\n")
			#aflds = c("major","minor","locality")
			kkk = names(refarea)
			for (kk in kkk) {
				k = as.numeric(kk)
				if (!file.exists(refarea[[kk]])) next
				kareas = read.csv(refarea[[kk]])
				if (!all(aflds %in% names(kareas)))
				showError(paste0("User-specified file `",refarea[[kk]],"' does not have the required fields\n",deparse(aflds)))
				kareas$aindex = .createIDs(kareas,aflds)  ## .createIDs currently not exported from PBSmapping namespace
				refdat = REFDAT[[kk]]
				if (any(is.element(refdat$aindex,kareas$aindex))){  ## otherwise leave unchanged
					refdat = refdat[is.element(refdat$aindex,kareas$aindex),]
					REFDAT[[kk]] = refdat
				}
				if (!is.null(refgear[[kk]])) {
					refdat = refdat[is.element(refdat$gear,refgear[[kk]]),]
					if (nrow(refdat)>0) REFDAT[[kk]] = refdat
				}
			}
		} else if( !is.null(refarea) && is.null(refgear) ){  ## only refarea is supplied (named list, named by fid)
			## -----------------------------------------------
			## Get rid of reference data not in reference area
			## -----------------------------------------------
			.flush.cat("Qualifying ref data by reference area ...\n")
			#aflds = c("major","minor","locality")
			kkk = names(refarea)
			for (kk in kkk) {
				k = as.numeric(kk)
				if (!file.exists(refarea[[kk]])) next
				kareas = read.csv(refarea[[kk]])
				if (!all(aflds %in% names(kareas)))
				showError(paste0("User-specified file `",refarea[[kk]],"' does not have the required fields\n",deparse(aflds)))
				kareas$aindex = .createIDs(kareas,aflds)  ## .createIDs currently not exported from PBSmapping namespace
				refdat = REFDAT[[kk]]
				if (any(is.element(refdat$aindex,kareas$aindex))){  ## otherwise leave unchanged
					refdat = refdat[is.element(refdat$aindex,kareas$aindex),]
					REFDAT[[kk]]=refdat
				}
			}
		} else if( is.null(refarea) && !is.null(refgear) ){  ## only refgear is supplied (named list, named by fid)
			## -----------------------------------------------
			## Get rid of reference data not in reference area
			## -----------------------------------------------
			.flush.cat("Qualifying ref data by reference gear ...\n")
			kkk = names(refgear)
			for (kk in kkk) {
				k = as.numeric(kk)
				if ( !k%in%fid ) next
				refdat = REFDAT[[kk]]
				refdat = refdat[is.element(refdat$gear, refgear[[kk]]),]
				if (nrow(refdat)>0) REFDAT[[kk]] = refdat
			}
		}
	}
	packList(c("REFDAT"),"CR",tenv=.PBStoolEnv)

	## ---------------------------------------------------------------------------------
	## Replace elements of the reference catch array (ctab) with modified reference data
	## ---------------------------------------------------------------------------------
	if ( !is.null(refarea) || !is.null(refgear) ) {
		if (is.null(refgear)) kkk1 = "" else kkk1 = names(refgear)
		if (is.null(refarea)) kkk2 = "" else kkk2 = names(refarea)
		kkk = union(kkk1,kkk2)
		ctab.orig = ctab

		for (kk in dimnames(catmod)$fid) {
			#if ( !kk %in% kkk1 & !kk %in% kkk2 ) next
			if ( !kk %in% kkk ) next
			k = as.numeric(kk)
			ii =as.character(refyrs[[k]]) ## each FID now has its own set of reference years
			refdat = REFDAT[[kk]]
			if (is.null(dim(refdat)) && is.na(refdat)) {
				ctab[ii,,kk,"landed"] = 0 ## just set all landed to zero for now.
				next
			}
			for (ll in dimnames(catmod)$catch) {
				CTdat = crossTab(refdat, c("year","major"), ll, hadley=hadley)
				if (hadley) CTdat = convCT(CTdat)
				iii = intersect(ii,rownames(CTdat))
				ii0 = setdiff(ii,rownames(CTdat))
				LLdat = CTdat[iii,]
				jj = intersect(dimnames(ctab)$major, dimnames(LLdat)[[2]])
				if (length(jj)==0) next
				jj0 = setdiff(MM,jj)
				ctab[iii,jj,kk,ll] = LLdat[iii,jj,drop=FALSE]
				if (ll=="landed") {
					if(length(ii0)>0)
						ctab[ii0,jj,kk,ll] = NA  ## exclude annual reference catch not specified by refarea and refgear
					if(length(jj0)>0)
						ctab[ii,as.character(jj0),kk,ll] = 0  ## exclude area reference catch not specified by refarea and refgear
				}
			}
		}
	}

	## ------------------------------
	## Catch reference summary tables
	## ------------------------------
	.flush.cat("Calculating catch reference summary tables ...\n")
	cref  = ctab[,mm,,,drop=FALSE]               ## catch reference (start with ctab which is a subset of catmod using reference years)
	cyrs  = as.numeric(dimnames(catmod)$year)
	yy    = as.character(intersect(cyrs, min(1996,REFYRS):max(cyrs)))
	CREF  = catmod[yy,mm,,,drop=FALSE]  ## Used only to derive an expanded GREFS
	catMF = array(0,dim=dim(cref)[2:4],dimnames=dimnames(cref)[2:4])
	for (k in fid) {
		ii = as.character(refyrs[[k]])
		iref = cref[ii,,,,drop=FALSE]
		kref = apply(iref,2:4,sum,na.rm=TRUE)[,k,,drop=FALSE]  ## total catch by major and fid
		catMF[dimnames(kref)[[1]],dimnames(kref)[[2]],dimnames(kref)[[3]]] = kref
	}
	if (saveinfo)
		packList(c("cref","CREF","catMF"),"CR",tenv=.PBStoolEnv)

	## ----------------------------------------------------------
	## alpha - Proportion RRF caught in a major area for each fid
	## ----------------------------------------------------------
	.flush.cat("Calculating alpha (prop RRF caught in major area for each fid) ...\n")
	RATES = list()
	alpha = apply(catMF[,,"landed"],2,function(x){
		if (all(x==0)) rep(0,length(x)) else x/sum(x)}) ## columns (fids) sum to 1
	dimnames(alpha) = dimnames(catMF)[1:2]
	RATES[["alpha"]] = alpha
	if (ascii.tables)
		write.csv(alpha,file=paste0(tabDir,"/alpha-",strSpp,"_",orfSpp,"-",numdate,".csv"))
	if (diagnostics){
		plotDiag(alpha,"alpha (fishery in major)",col=clrs.fishery,type="bars")
		plotDiag(t(alpha),"alpha (major in fishery)",col=clrs.major[mm],type="bars")
	}

	## ------------------------------------------------------------
	## beta - Proportion RRF caught in H&L fisheries for each major
	## ------------------------------------------------------------
	.flush.cat("Calculating beta (prop RRF caught in H&L fisheries for each major) ...\n")
	dnam = intersect(c("2","4","5"),dimnames(alpha)[[2]]) ## dimnames for H&L
	beta = t(apply(catMF[,dnam,"landed",drop=FALSE],1,function(x){
		if (all(x==0)) rep(0,length(x)) else x/sum(x)})) ## columns (fids) sum to 1
	dimnames(beta)[[2]]=dnam; names(dimnames(beta)) = c("major","fid")
	RATES[["beta"]] = beta
	if (ascii.tables)
		write.csv(beta,file=paste0(tabDir,"/beta-",strSpp,"_",orfSpp,"-",numdate,".csv"))
	if (diagnostics){
		plotDiag(beta,"beta (fishery in major)",col=clrs.fishery,type="bars")
		plotDiag(t(beta),"beta (major in fishery)",col=clrs.major[mm],type="bars")
	}

	## -----------------------------------------------------------------
	## Ratio RRF catch to other catch (RATES)
	## Originally called 'rtar', changed to 'GRATES' and then to 'RATES'
	## -----------------------------------------------------------------
	.flush.cat("Calculating gamma (ratio of RRF to a larger group like ORF) ...\n")
	## collect components of stratified gamma (if strat.gamma=TRUE)
	gamma.strat=list()
	for (kk in as.character(fid)) { for (ll in c("POP","ORF","TRF","TAR")) { gamma.strat[[kk]][[ll]]=list() }}
	## -------------------------
	## Use cref instead of catMF
	## -------------------------
	for (ll in c("POP","ORF","TRF","TAR")) {  ## calculated for main base groups but gamma only uses `orfSpp'
		## --------------------------------------------(RH 141002)
		## If use binomial-gamma for estimating RRF/ORF or RRF/TRF
		## -------------------------------------------------------
		if (useBG && !strat.gamma) { ## use binomial-gamma
			if (ll=="POP") .flush.cat("   using binomial-gamma to estimate gamma.\n")
			getPMRrat <- function(x) {
				if (length(x)==0) pmr = 0 #c(n=0,p=NA,mu=NA,rho=NA)
				else              pmr = calcPMR(x)
				return(pmr)
			}
			getBGrat <- function(x) {
				rat = mean(sampBG(n=1e5,p=x[2],mu=x[3],rho=x[4]))
				return(rat)
			}
			gmat = array(0,dim=c(length(mm),length(fid)), dimnames=list(major=mm,fid=fid))
			for (k in fid) {
				kk = as.character(k)
				refdat = REFDAT[[kk]]
				refdat = refdat[is.element(refdat$year,refyrs[[k]]),]
				if (k==2) refdat$fid[is.element(refdat$fid,7)] = 2
				refdat = refdat[is.element(refdat$major,mm) & is.element(refdat$fid,k),]
				refdat = refdat[refdat[[ll]]>0 & !is.na(refdat[[ll]]),]
				if (nrow(refdat)==0) next
				refdat$ratio = refdat$landed / refdat[[ll]]
				refpmr = crossTab(refdat,c("major","fid"),"ratio",getPMRrat, hadley=hadley)  ## crossTab may be a bit dodgy here
				if (hadley) refpmr = convCT(refpmr)
				else        refpmr = matrix(refpmr[,1,],nrow=dim(refpmr)[1],ncol=dim(refpmr)[3],dimnames=dimnames(refpmr)[c(1,3)])
				refmat = t(t(apply(refpmr,1,getBGrat)))
				colnames(refmat) = kk
				gmat[dimnames(refmat)[[1]],dimnames(refmat)[[2]]] = refmat
			} ## end k loop
		}
		else if (strat.gamma && !useBG) {  ## use stratified gamma 
			gmat = array(0,dim=c(length(mm),length(fid)), dimnames=list(major=mm,fid=fid))
			for (k in fid) {
				kk = as.character(k)
				refdat = REFDAT[[kk]]
				refdat = refdat[is.element(refdat$year,refyrs[[k]]),]
				refdat = refdat[is.element(refdat$major,mm),]

				## Not sure why the following line was done because in the end the ratio is a sum over a sum; disbale for now (RH 190613)
				#refdat = refdat[refdat[[ll]]>0 & !is.na(refdat[[ll]]),]  ## maybe only do this for halibut:TAR
				if (nrow(refdat)==0) {agamma=NULL; next}

				refdat$dzone = ceiling(refdat$fdep/depbin)*depbin
				refdat$dzone[is.na(refdat$dzone)] = 0
				## ---------------------------------------------------------
				## need at least 10% of records to contain depth information
				## ---------------------------------------------------------
				if (!all(refdat$dzone==0) && length(refdat$dzone[refdat$dzone>0])/nrow(refdat) > 0.1) 
					refdat = refdat[refdat$dzone>0 & !is.na(refdat$dzone),]
				else refdat$dzone = rep(999,nrow(refdat))
				nrefc = apply(crossTab(refdat, c("year","major","dzone"), ll, function(x){length(x[x>0])}, hadley=hadley), 3, sum)
				if (debug) .flush.cat(c(k,ll,nrefc,"\n"))
				## -------------------------------------------------------------------
				## only use depth zones with at least 10 non-zero discard observations
				## -------------------------------------------------------------------
				urefc = nrefc[nrefc>=ifelse(k==3,3,ifelse(k==4,4,10))] 
				if (length(urefc)==0) {
					agamma=NULL; next
				}
				if (debug) .flush.cat(c(k,ll,urefc,"\n"))

				refdat = refdat[is.element(refdat$dzone,names(urefc)),]
				dnum  = crossTab(refdat,c("year","major","dzone"),"landed",function(x){if(all(is.na(x))) 0 else sum(x,na.rm=TRUE)/1000.}, hadley=hadley)
				dden  = crossTab(refdat,c("year","major","dzone"),ll,function(x){if(all(is.na(x))) 0 else sum(x,na.rm=TRUE)/1000.}, hadley=hadley)
				dnos  = crossTab(refdat,c("year","major","dzone"),ll,length, hadley=hadley)
				snos  = apply(dnos,2,sum)        ## stratify by year and depth
				pnos  = dnos
				for (p in 1:dim(dnos)[3]){
					pdnos = matrix(as.vector(dnos[,,p]),nrow=dim(dnos)[1],ncol=dim(dnos)[2],dimnames=dimnames(dnos)[1:2])
					ztemp = apply(pdnos,1,function(x){x/snos})
					ztemp[!is.finite(ztemp)] = 0
					pnos[,,p] = t(ztemp)
				}
				zgamma = dnum/dden
				zgamma[dnum==0 & dden==0] = 0 ## if both numerator and denominator are zero, set gamma to 0
				zgamma[dnum>0  & dden==0] = 1 ## if numerator is positive and denominator is zero, arbitrarily set gamma to 1 (100%)
				pgamma = zgamma * pnos           ## weight each gamma by the proportion of observations per area
				agamma = apply(pgamma,2,sum)     ## derive the gamma vector by area
				gmat[names(agamma),kk] = agamma

				## collect the stratified components of gamma
				gamma.strat[[kk]][[ll]][["pnos"]]   = pnos
				gamma.strat[[kk]][[ll]][["zgamma"]] = zgamma
				gamma.strat[[kk]][[ll]][["pgamma"]] = pgamma
				gamma.strat[[kk]][[ll]][["agamma"]] = agamma
			} ## end k loop
		} else {  ## use default ratios method
			## use original method of sums over sums
			if (ll=="POP") .flush.cat(paste0("   calculating mean of annual ratios (",strSpp,"/",orfSpp,")\n"))
			z0  = cref[,,,"landed"]==0 ; Z0  = CREF[,,,"landed"]==0
			z1  = cref[,,,ll]==0       ; Z1  = CREF[,,,ll]==0
			z2  = z0&z1                ; Z2  = Z0&Z1
			rland = cref[,,,"landed"]/cref[,,,ll]
			RLAND = CREF[,,,"landed"]/CREF[,,,ll] ## only for expanded GREFS
			## order is important here (process equalities, zero-denominator, then zero-numerator)
			rland[z2]=0; rland[!z2&z1]=1; rland[!z2&!z1&z0]=0 
			RLAND[Z2]=0; RLAND[!Z2&Z1]=1; RLAND[!Z2&!Z1&Z0]=0 
			## Calculate discard rates to compare with those from observerlogs later (calculating delta)
			z0  = cref[,,,"discard"]==0
			z1  = cref[,,,ll]==0
			z2  = z0&z1
			rdisc = cref[,,,"discard"]/cref[,,,ll]
			rdisc[z2]=0; rdisc[!z2&z1]=1; rdisc[!z2&!z1&z0]=0  ## order important

			## Check for conditions that should never be possible
			## --------------------------------------------------
			if ((strSpp=="396" && ll%in%c("POP","TRF")) || (strSpp!="396" && ll%in%c("ORF","TRF"))){
				z3 = rland > 1 & !is.na(rland)
				if (any(z3)) rland[z3] = 1
				Z3 = RLAND > 1 & !is.na(RLAND)
				if (any(Z3)) RLAND[Z3] = 1
			}
			## ----------------------------------------------------------------------------------
			## Apply arithmetic or geometric mean to annual ratios of gamma.
			## Originally assumed that years with gamma=0 were meaningful but data can be sparse.
			## Currently assume that years with gamma=0 are not representative.
			## ----------------------------------------------------------------------------------
			#if (useGM) {
			#	#rmat = apply(rtmp, 2:3, calcGM, exzero=FALSE, offset=1e-9) ## geometric mean rate by major (with small offset to use zero rates)
			#	rmat = apply(rtmp, 2:3, calcGM, exzero=TRUE)
			#} else {
			#	#rmat = apply(rtmp, 2:3, mean, na.rm=TRUE)
			#	rmat = apply(rtmp, 2:3, function(x){z=x>0 & !is.na(x); if(!any(z)) 0 else mean(x[z])})
			#}
			## initialize the array because now each fid has a different set of reference years

			gmat = array(0,dim=dim(rland)[2:3],dimnames=dimnames(rland)[2:3])
			dmat = array(0,dim=dim(rdisc)[2:3],dimnames=dimnames(rdisc)[2:3])
			for (k in fid) {
				ii = as.character(refyrs[[k]])
				kland = rland[ii,,,drop=FALSE]
				kdisc = rdisc[ii,,,drop=FALSE]
				if (useGM) {
					kgmat = apply(kland, 2:3, calcGM, exzero=TRUE)[,k,drop=FALSE]
					kdmat = apply(kdisc, 2:3, calcGM, exzero=TRUE)[,k,drop=FALSE]
				} else {
					kgmat = apply(rland, 2:3, function(x){z=x>0 & !is.na(x); if(!any(z)) 0 else mean(x[z])})[,k,drop=FALSE]
					kdmat = apply(rdisc, 2:3, function(x){z=x>0 & !is.na(x); if(!any(z)) 0 else mean(x[z])})[,k,drop=FALSE]
				}
				gmat[dimnames(kgmat)[[1]],dimnames(kgmat)[[2]]] = kgmat
				dmat[dimnames(kdmat)[[1]],dimnames(kdmat)[[2]]] = kdmat
			} ## end k loop
		}
		RATES[["gamma"]][[ll]]   = gmat
		if (!strat.gamma)
			RATES[["discard"]][[ll]] = dmat  ## not really delta
		if (ll==orfSpp && !strat.gamma){
			## save annual gamma rates for RRF/ORF or RRF/TRF
			GREFS = RLAND
			save("GREFS",file=paste0(datDir,"/GREFS",strSpp,".rda"))
			ttput(GREFS)
		}
	}
	save("RATES",file=paste0(datDir,"/RATES",strSpp,".rda")) ## save target-specific gamma rates
	ttput(RATES)
	if (strat.gamma) {
		save("gamma.strat",file=paste0(datDir,"/gamma.strat.rda")) ## save components of stratified gamma (if applicable)
		ttput(gamma.strat)
	}

	## -------------------------------------------------------------
	## gamma - Ratio of RRF to a larger group (e.g., other rockfish)
	## -------------------------------------------------------------
	rfac  = RATES[["gamma"]][[orfSpp]]
	gamma = rfac[mm,,drop=FALSE]  ## use only specified majors

	## ----------------------------------------------------------------------------------
	## Special trawl calculations by Paul Starr for YTR based on Brian Mose's suggestions
	## ----------------------------------------------------------------------------------
	if (strSpp=="418" && !is.null(list(...)$pjs) && list(...)$pjs) {
		.flush.cat("   adjusting gamma for Yellowtail based on PJS fixed ratios (majors 3:9).\n")
		if (orfSpp=="ORF") {
			if (!is.null(list(...)$outside) && list(...)$outside)
				gamma[,1]=matrix(c(0,0.3139525,0.253063,0.0467262,0.0034517,0,0.4451063,0.0049684),ncol=1)
			else
				gamma[,1]=matrix(c(0,0.3138979,0.253063,0.2196372,0.2346488,0.1861062,0.4605869,0.0049684),ncol=1)
		}
		if (orfSpp=="TRF") {
			if (!is.null(list(...)$outside) && list(...)$outside)
				gamma[,1]=matrix(c(0,0.2596861,0.2291434,0.0388679,0.0009657,0,0.1670699,0.0029361),ncol=1)
			else
				gamma[,1]=matrix(c(0,0.2596484,0.2291434,0.1595966,0.1024332,0.1238941,0.239913,0.0029361),ncol=1)
		}
	}
	## ----------------------------------------------------------------------------------
	## Fraidenberg ratios used by Stanley (2009)
	## ----------------------------------------------------------------------------------
	if (strSpp=="437" && !is.null(list(...)$rds) && list(...)$rds) {
		.flush.cat("   adjusting trawl gamma for Canary based on Fraidenberg ratios in Stanley (2009).\n")
		if (orfSpp=="ORF") {
			gamma[,1]=matrix(c(0,rep(0.46,2),rep(0.16,5)),ncol=1)
		}
	}
	## ---------------------------------------------------------------------------
	## Explicit gamma ratios for YYR/ORF (5ABCD) from the Halibut fishery provided
	## by Chris Sporer (PHMA, 2015-06-12) and deemed appropriate by their caucus.
	## ---------------------------------------------------------------------------
	if (strSpp=="442" && !is.null(list(...)$phma) && list(...)$phma) {
		if (orfSpp=="ORF") {
			gamma.phma = matrix(c(0.25,0.25,0.375,0.3),nrow=4,ncol=1,dimnames=list(major=5:8,fid=2))
			gamma[row.names(gamma.phma),colnames(gamma.phma)] = gamma.phma
		}
	}
	if (ascii.tables)
		write.csv(gamma,file=paste0(tabDir,"/gamma-",strSpp,"_",orfSpp,"-",numdate,".csv")) #;return(gamma)
	if (diagnostics){
		plotDiag(gamma,"gamma (fishery in major)",col=clrs.fishery,type="bars")
		plotDiag(t(gamma),"gamma (major in fishery)",col=clrs.major[mm],type="bars")
	}

	## -----------------------------------------------------------------
	## delta - Discard rate of landed species per ORF from observer logs
	## -----------------------------------------------------------------
	.flush.cat("Calculating delta (discard rate of RRF per target) ...\n")
	assign("CR",ttget(CR))  ## remember global collection object because 'calcRatio' overwrites it
	drSpp = list(c("discard","landed"),c("discard","TAR"),c("discard","ORF"))
	drate = sapply(drSpp,function(x){paste(x,collapse=":")})   ## discard ratio description
	drN   = length(drSpp)                                      ## number of discard rates
	dfac  = array(0,dim=c(length(mm),length(fid),drN+1),
		dimnames=list(major=mm,fid=fid,rate=c(drate,"dr")))
	ologs=as.list(rep(NA,length(fid))); names(ologs)=fid
	PNOS = list()

	## collect components of stratified delta (if strat.delta=TRUE)
	delta.strat=list()
	for (kk in as.character(fid)) { for (dd in c("POP","ORF","TRF","TAR")) { delta.strat[[kk]][[dd]]=list() }}

	for (k in fid) {
		if (k==0) next  ## just in case
		kk=as.character(k); jj=dimnames(dfac)[[1]]; j=as.numeric(jj)
		if (k==1) {
			dyrs=defyrs[[k]] #1997:2006
			dyrs = intersect(dyrs, 1996:2007) ## phfOlogs dates range from '1996-02-17' to '2007-03-31'
			if (file.exists(paste0(catDir,"/phtOlogs.rda"))) {
				.flush.cat("   loading trawl observer logs from `phtOlogs.rda';\n")
				load(paste0(catDir,"/phtOlogs.rda"))
			} else {
				## ------------------------------------------
				## Query observer logs only (can take 30 sec)
				## Should only need to do this once for any species.
				## ------------------------------------------
				.flush.cat("   querying trawl observer logs (~30 sec);\n")
				getData("pht_obsORF.sql", dbName="PacHarvest", strSpp=strSpp, path=spath, tenv=penv(), dummy=dyrs)
				zOpht = do.call('order', PBSdat)  ## Try ordering (RH: 240216): calls to SQL seem to return same records in different order.
				phtOlogs = PBSdat[zOpht,]
				save("phtOlogs", file=paste0(catDir,"/phtOlogs.rda"))
			}
			discat = phtOlogs
		}
		else if (any(k==c(2:5))) {
			dyrs=defyrs[[k]]  ##2000:2004 #2007:2013
			if (any(k==c(2,4,5))) {
				dyrs = intersect(dyrs, 1999:2005) ## phfOlogs dates range from '1999-05-22' to '2005-03-24'
				if (!file.exists(paste0(catDir,"/phfOlogs.rda"))) {
					## --------------------------------------------------
					## Query observer logs only (for halibut, sched2, ZN)
					## --------------------------------------------------
					.flush.cat("   querying h&l observer logs;\n")
					getData("phhl_fcatORF.sql","PacHarvHL",strSpp=strSpp,path=spath,fisheryid=c(2,4,5),logtype="OBSERVRLOG",tenv=penv())
					#discat=fosdat[is.element(fosdat$fid,k) & is.element(fosdat$log,105),] # fisherlogs (supposed to record all discards, electronic monitoring)
					#phfOlogs = PBSdat
					zOphf = do.call('order', PBSdat)  ## Try ordering (RH: 240216): calls to SQL seem to return same records in different order.
					phfOlogs = PBSdat[zOphf,]
					save("phfOlogs",file=paste0(catDir,"/phfOlogs.rda"))
				} else {
					if (k==2){
						.flush.cat("   loading h&l observer logs from `phfOlogs.rda';\n")
						load(paste0(catDir,"/phfOlogs.rda"))
					}
				}
				discat = phfOlogs[is.element(phfOlogs$fid,k),]
			}
			if (k==3) {
				dyrs = intersect(dyrs, 2000:2005) ## phfOlogs dates range from '2000-05-26' to '2005-06-13'
				if (file.exists(paste0(catDir,"/phsOlogs.rda")) && !isThere("phsOlogs")) {
					.flush.cat("   loading sablefish observer logs from `phsOlogs.rda';\n")
					load(paste0(catDir,"/phsOlogs.rda"))
				} else {
					## ---------------------------------------
					## Query observer logs only (for sabefish)
					## ---------------------------------------
					.flush.cat("   querying sablefish observer logs;\n")
					getData("phs_scatORF.sql","PacHarvSable",strSpp=strSpp,path=spath,fisheryid=k,logtype="OBSERVRLOG",tenv=penv())
					zOphs = do.call('order', PBSdat)  ## Try ordering (RH: 240216): calls to SQL seem to return same records in different order.
					phsOlogs = PBSdat[zOphs,]
					save("phsOlogs",file=paste0(catDir,"/phsOlogs.rda"))
				}
				discat = phsOlogs
			}
		}  ## end k 2:5
		if (nrow(discat)==0) next
		if (!useSM) {
			discat = zapSeamounts(discat)
			if (nrow(discat)==0) next
		}
		if (useAI && any(strSpp==c("396","440")))
			discat = expand5C(discat)
		discat$year = as.numeric(substring(discat$date,1,4))
		discat = discat[is.element(discat$year,dyrs) & !is.na(discat$year),]
		discat = discat[is.element(discat$major,MM) & !is.na(discat$major),]
		if (nrow(discat)==0) next
		ologs[[kk]] = discat
		DISCAT = discat
		for (d in 1:drN) { ## discard ratio combos 'drSpp'
			discat = DISCAT
			dd   = drate[d]
			fldN = drSpp[[d]][1]
			fldD = drSpp[[d]][2]
			if (strat.delta && is.element("fdep",names(discat)) ){ 
				# && length(discat$fdep[!is.na(discat$fdep)])>=10) {
				## Not sure why the following line was done because in the end the ratio is a sum over a sum; disbale for now (RH 190613)
				#discat = discat[discat[[fldD]]>0 & !is.na(discat[[fldD]]),]  ## maybe only do this for halibut:TAR
				if (nrow(discat)==0) {DRAT=NULL; next }

				discat$dzone = ceiling(discat$fdep/depbin)*depbin
				discat$dzone[is.na(discat$dzone)] = 0
				## ---------------------------------------------------------
				## need at least 10% of records to contain depth information
				## ---------------------------------------------------------
				if (!all(discat$dzone==0) && length(discat$dzone[discat$dzone>0])/nrow(discat) > 0.1) 
					discat = discat[discat$dzone>0 & !is.na(discat$dzone),]
				else discat$dzone = rep(999,nrow(discat))
				ndisc = apply(crossTab(discat,c("year","major","dzone"),fldD,function(x){length(x[x>0])},hadley=hadley),3,sum)
				if (debug) .flush.cat(c(k,dd,ndisc,"\n"))
				## -------------------------------------------------------------------
				## only use depth zones with at least 10 non-zero discard observations
				## -------------------------------------------------------------------
				udisc = ndisc[ndisc>=ifelse(k==3,3,ifelse(k==4,4,10))]
				if (length(udisc)==0) {DRAT=NULL; next }
				if (debug) .flush.cat(c(k,dd,udisc,"\n"))
				discat = discat[is.element(discat$dzone,names(udisc)),]
				dnum  = crossTab(discat,c("year","major","dzone"),fldN,function(x){if(all(is.na(x))) 0 else sum(x,na.rm=TRUE)/1.},hadley=hadley)
				dden  = crossTab(discat,c("year","major","dzone"),fldD,function(x){if(all(is.na(x))) 0 else sum(x,na.rm=TRUE)/1.},hadley=hadley)
				dnos  = crossTab(discat,c("year","major","dzone"),fldD,length,hadley=hadley)
				snos  = apply(dnos,2,sum)    ## stratify by year and depth
				pnos  = dnos
				for (p in 1:dim(dnos)[3]){
					pdnos = matrix(as.vector(dnos[,,p]),nrow=dim(dnos)[1],ncol=dim(dnos)[2],dimnames=dimnames(dnos)[1:2])
					ztemp = apply(pdnos,1,function(x){x/snos})
					ztemp[!is.finite(ztemp)] = 0
					pnos[,,p] = t(ztemp)
				}
				zdelta = dnum/dden
				zdelta[dnum==0 & dden==0] = 0 ## if both numerator and denominator are zero, set delta to 0
				zdelta[dnum>0  & dden==0] = 1 ## if numerator is positive and denominator is zero, arbitrarily set delta to 1 (100%)
				#zdelta[!is.finite(zdelta)]=0  ## temporay artificially set rate
				pdelta = zdelta * pnos        ## weight each discard rate by the proportion of observations per area
				adelta = apply(pdelta,2,sum)  ## area delta
				DRAT  =  adelta               ## derive the discard rate vector by area
				PNOS[[kk]][[dd]] = pnos       ## proportion weighting by year, major and depth zone applied to discard rates (weights sum to 1 for each area)

				## collect the stratified components of delta
				delta.strat[[kk]][[dd]][["pnos"]]   = pnos
				delta.strat[[kk]][[dd]][["zdelta"]] = zdelta
				delta.strat[[kk]][[dd]][["pdelta"]] = pdelta
				delta.strat[[kk]][[dd]][["adelta"]] = adelta

#if (k==2) {browser();return()}
			} else { ## original method
				dnum = crossTab(discat,c("year","major"),fldN,sum,hadley=hadley)
				dden = crossTab(discat,c("year","major"),fldD,sum,hadley=hadley)
				if (hadley) {
					dnum = convCT(dnum)
					dden = convCT(dden)
				}
				## Initialize discard matrix with zeroes
				## No need to check if numerator=0 and denominator>0 because ratio dnum/dden=0
				DRAT = array(0,dim=dim(dnum),dimnames=dimnames(dnum))
				## Non-zero values in numerator and denominator
				zyes = dnum>0 & dden>0
				if (any(zyes))
					DRAT[zyes] = dnum[zyes]/dden[zyes]
				## Non-zero values in numerator and zero values in denominator (discarded everything, landed nothing)
				zmay = dnum>0 & dden<=0
				if (any(zmay))
					DRAT[zmay] = 1 ## 100% discarded
				#DRAT = dnum/dden ## annual discard rates by major
			}
			RATES[["delta"]][[kk]][[dd]] = DRAT
			drat = rep(0,length(mm)); names(drat)=mm
			if (!is.null(DRAT)){
				if (is.vector(DRAT)) {
					drat0 = DRAT
				} else {
					## Originally assumed that years with delta=0 were meaningful but data can be sparse.
					## Currently assume that years with delta=0 are not representative.
					## ------------------------------------------------------------
					if (useGM) {
						drat0 = apply(DRAT, 2, calcGM, exzero=TRUE)
					} else {
						drat0 = apply(DRAT, 2, function(x){z=x>0 & !is.na(x); if(!any(z)) 0 else mean(x[z])})
					}
				}
				mmm = intersect(names(drat),names(drat0))
				drat[mmm] = drat0[mmm]
			}
			dfac[names(drat),kk,dd]=drat 
		}
		## --------------------------------------------------------------
		## Assign a default discard rate by fishery
		## Changes here must also be made below -- search for [mirror1]
		## --------------------------------------------------------------
		if (any(k==c(1,5)))
			dfac[jj,kk,"dr"] = dfac[jj,kk,"discard:landed"]
		if (any(k==c(2,3,4)))
			dfac[jj,kk,"dr"] = dfac[jj,kk,"discard:TAR"]
	} ## end k loop
	dfac[is.na(dfac) | !is.finite(dfac)] = 0
	delta = dfac
	RATES[["delta"]][["delta"]] = delta
	assign("CR",CR,envir=.PBStoolEnv); rm(CR)   ## restore to .PBStoolEnv and remove local copy
	save("ologs", file=paste0(datDir,"/ologs",strSpp,".rda"))  ## save observerlogs  with discard information
	save("PNOS",file=paste0(datDir,"/PNOS",strSpp,".rda"))     ## save proportion weightings applied to discard rates
	save("RATES",file=paste0(datDir,"/RATES",strSpp,".rda")) ## save annual discard rates
	ttput(RATES)
	if (strat.delta) {
		save("delta.strat",file=paste0(datDir,"/delta.strat.rda")) ## save components of stratified gamma (if applicable)
		ttput(delta.strat)
	}
	if (ascii.tables)
		write.csv(delta[,,"dr"],file=paste0(tabDir,"/delta-",strSpp,"_",orfSpp,"-",numdate,".csv")) ## save discard rate used in CR
	if (diagnostics){
		for (rate in dimnames(delta)$rate) {
			rr = sub(":","2",rate)
			plotDiag(delta[,,rate], paste0("delta ",rr," (fishery in major)"), col=clrs.fishery, type="bars")
			plotDiag(t(delta[,,rate]), paste0("delta ",rr," (major in fishery)"), col=clrs.major[mm], type="bars")
		}
	}
	if (saveinfo)
		packList(c("ctab","alpha","beta","RATES","GREFS","gamma","delta","dfac"),"CR",tenv=.PBStoolEnv)
	.flush.cat("-----Finished calculating reference ratios-----\n\n")
	} ## fold03
	fold04 = TRUE ## ----- only used for code folding -----
	if (fold04) { ## 4. Allocate ancient rockfish
	
	## =======================================================
	## 4. Allocate the ancient rockfish catch by unknown gear 
	##    type to RRF by fishery (decompose "combined" only). 
	## =======================================================

	## -----------------------------------------------------------------------------
	## Get sector allocation for very early series from sales slip data (Obradovich)
	## -----------------------------------------------------------------------------
	.flush.cat("Allocating ancient rockfish catch to RRF from unknown gear type...\n")
	ancyrs = 1918:1950; prewar = ancyrs[1]:1938; postwar = 1939:rev(ancyrs)[1]
	gear = c("h&l","trap","trawl")
	epoch = c("prewar","postwar")
	cobra = htab[as.character(1951:1952),mm,"Obradovich","CA","max",gear,"ORF"]
	major.gear = t(apply(apply(cobra,2:3,sum),1,function(x){if (all(x==0)) rep(0,length(x)) else x/sum(x)}))
	## RH (180621) -- Arbitrary decision to ensure that 10% activity is from trawl in major ares with 0% activity (mainly to fix 5E).
	major.gear = t(apply(major.gear,1,function(x){
		if (round(x[3],5)==0) {
			x[3]=0.1; x[1:2] = x[1:2]*0.9
		}
		return(x)
	}))
	if (!all(round(apply(major.gear,1,sum),5)==1))
		showError("Not all rows in 'major.gear' for postwar 'lambda' add to equal 1")

	## -------------------------------------------------------
	## lambda - Proportion of early catch by general gear type
	## -------------------------------------------------------
	.flush.cat("   calculating lambda (prop. early catch by general gear type).\n")
	lambda = array(0,dim=c(dim(major.gear),2),dimnames=list(major=mm,gear=gear,epoch=epoch))
	## Before WW II (1918-1938)
	lambda[,"h&l","prewar"]=0.9; lambda[,"trap","prewar"]=0; lambda[,"trawl","prewar"]=0.1
	lambda[rownames(major.gear),colnames(major.gear),"postwar"] = major.gear
	RATES[["lambda"]] = lambda
	if (ascii.tables)
		write.csv(lambda,file=paste0(tabDir,"/lambda-",strSpp,"_",orfSpp,"-",numdate,".csv")) ## save lambda used in CR
	if (diagnostics){
		for (epo in dimnames(lambda)$epoch) {
			plotDiag(lambda[,,epo], paste0("lambda ",epo," (gear in major)"), col=clrs.fishery, type="bars")
			plotDiag(t(lambda[,,epo]), paste0("lambda ",epo," (major in gear)"), col=clrs.major[mm], type="bars")
		}
	}

	ancientRRF = ancientORF = array(0,dim=c(length(ancyrs),length(mm),length(fid),length(nat)),
		dimnames=list(year=ancyrs,major=mm,fid=fid,nat=nat)) ## unknown gear type only

	## ---------------------------------------------
	## Allocate the catch from unkown combined gears
	## ---------------------------------------------
	oldies = sapply(epoch,function(x){get(x)},simplify=FALSE)
	for (i in names(oldies)) {
		oldyrs = oldies[[i]]
		ii = as.character(oldyrs); jj = mm
		L245 = lambda[,"h&l",i] * beta  ## expand h&l contribution to fid (2,4,5)
		LAMBDA = cbind(lambda[,"trawl",i],L245[,"2"],lambda[,"trap",i],L245[,c("4","5")]) ## prop. combined ORF by major and fid
		dimnames(LAMBDA)[[2]] = fid
		gamma.lambda = gamma * LAMBDA ## prop. of combined ORF comprising RRF by PMFC and FID
		RATES[["gamma.lambda"]][[i]] = gamma.lambda
		## -----------------------------------------------
		## Partition the 'combined' rockfish catch to fids
		## -----------------------------------------------
		for (k in fid) { 
			kk = as.character(k)
			deconRRF = t(apply(orfhis[ii,jj,"CA","combined"],1,function(x,gala){x*gala},gala=gamma.lambda[,kk]))
			ancientRRF[ii,jj,kk,"CA"] = ancientRRF[ii,jj,kk,"CA"] + deconRRF
			deconORF = t(apply(orfhis[ii,jj,"CA","combined"],1,function(x,gala){x*gala},gala=LAMBDA[,kk]))
			ancientORF[ii,jj,kk,"CA"] = ancientORF[ii,jj,kk,"CA"] + deconORF
		}
	}
	if (diagnostics){
		plotDiag(apply(ancientRRF,c(1,2),sum),"ancient in major",col=clrs.major[mm])
		plotDiag(apply(ancientRRF,c(1,3),sum),"ancient in fishery",col=clrs.fishery)
	}
	if (saveinfo)
		packList(c("cobra","lambda","gamma.lambda","ancientRRF"),"CR",tenv=.PBStoolEnv)
	.flush.cat("-----Finished allocating ancient rockfish catch by unknown gear-----\n\n")

	} ## end fold04
	fold05 = TRUE ## ----- only used for code folding -----
	if (fold05) { ## 5. Reconstruct RRF

	## ================================================
	## 5. Reconstruct RRF catch prior to reported catch (specified by useYR)
	## ================================================

	.flush.cat("Starting reconstruction of RRF ...\n")
	ALLYRS = sort(union(HISYRS,MODYRS)); nyrs=length(ALLYRS)
	MEDYRS = setdiff(HISYRS,ancyrs) ## medieval years not yet used from `orfhis'

	## ---------------------------------------------------------
	## Create the final collection arrays for the reconstruction
	## ---------------------------------------------------------
	sppnewRRF = sppnewORF = 
		array(0,dim=c(nyrs,length(mm),length(fid)+1), dimnames=list(year=ALLYRS,major=mm,fid=c(fid,10)))

	## -------------------------------------------------------
	## Add the ancient RRF and ORF caught by unknown gear type
	##   from Dominion Bureau dataset to final CR array
	## Only CA catch appears in this array even though dimensions
	##   allow for foreign nations
	## -------------------------------------------------------
	sppnewRRF[as.character(ancyrs),mm,as.character(fid)] = ancientRRF[as.character(ancyrs),mm,as.character(fid),"CA"]
	sppnewORF[as.character(ancyrs),mm,as.character(fid)] = ancientORF[as.character(ancyrs),mm,as.character(fid),"CA"]

	## -------------------------------------------------------
	## beta.gamma -- allocation matrix from fishery K to FID k
	## beta  -- splits 'h&l' fishery into FIDs 2,4,5
	## gamma -- applies RRF/ORF ratio to ORF
	## -------------------------------------------------------
	.flush.cat("   calculating beta.gamma to allocate early catch from fishery K to k.\n")
	BETA = cbind(rep(1,length(mm)),beta[,"2"],rep(1,length(mm)),beta[,c("4","5")])
	dimnames(BETA)[[2]] = fid
	beta.gamma = BETA * gamma  ## Expansion of RRF:ORF from K to k
	names(dimnames(beta.gamma)) = c("major","fid")
	RATES[["beta.gamma"]] = beta.gamma
	save("RATES",file=paste0(datDir,"/RATES",strSpp,".rda")) ## save updated RATES list
	ttput(RATES)
	if (ascii.tables)
		write.csv(beta.gamma,file=paste0(tabDir,"/beta.gamma-",strSpp,"_",orfSpp,"-",numdate,".csv")) ## save beta.gamma used in CR
	if (diagnostics){
		plotDiag(beta.gamma,"beta.gamma (fishery in major)",col=clrs.fishery,type="bars")
		plotDiag(t(beta.gamma),"beta.gamma (major in fishery)",col=clrs.major[mm],type="bars")
	}

	## ------------------------------------------------------
	## Record years that catch was reconstructed and reported
	## ------------------------------------------------------
	yrs.rec = catmats = list()
	for (k in fid) { ## big k (fid) loop; ends on L2450
		.flush.cat(paste0("Fishery ",k,":\n"))
		kk = as.character(k)
		jj = dimnames(catmod)$major
		#ii = as.character(MEDYRS)
		## -----------------------------------------------------
		## Reconstruct catch history from data with gear type K.
		## Years to estimate landings of 'strSpp' from ORF/TRF
		## -----------------------------------------------------
		repcatRRF = catmod[,jj,kk,"landed"]   ## all reported landed catch of RRF (catmod from GFM+PH3+RRFhis)
		repcatORF = catmod[,jj,kk,orfSpp]     ## reported landed catch of the rockfish group (ORF|TRF) used to estimate gamma
		repdisRRF = catmod[,jj,kk,"discard"]  ## all reported discarded catch of RRF
		if (ascii.tables) {
			write.csv(repcatRRF,paste0(tabDir,"/",fidfive[k],"-",strSpp,"-Landed-Dbase-",numdate,".csv"))
			write.csv(repcatORF,paste0(tabDir,"/",fidfive[k],"-",orfSpp,"-Landed-Dbase-",numdate,".csv"))
			write.csv(repdisRRF,paste0(tabDir,"/",fidfive[k],"-",strSpp,"-Discard-Dbase-",numdate,".csv"))
		}
		## ------------------------------------------------------------------
		## Need to compare reconstructed RRF using ORF from orfhis (non-DB)
		##  and using ORF from catmod (DB = GFM + PH3)
		## ------------------------------------------------------------------
		reccatRRFall = reccatORFall = array(0,dim=c(dim(allhis)[1:2],length(nat)), dimnames=c(dimnames(allhis)[1:2],list(nat=nat))) ## initialise non-DB
		attr(reccatRRFall,"fid") = k  ## just to keep track when debugging
		for (nn in nat) { ## nn loop ends L2046
			## ------------------------------------------------------
			## All reconstructed RRF landed catch from historical ORF
			## Exception [disabled]: POP which is reported as one of the reference
			## landings (POP, ORF, TRF), but only if user specifies an early 'useYR1'
			## ------------------------------------------------------
			reccatRRFall[,,nn] = t(apply(orfhis[,,nn,fshnam[k]],1,function(x,bega){x*bega},bega=beta.gamma[,kk])) ## where gamma is applied to all nonDB ORF (for all nations)
			reccatORFall[,,nn] = t(apply(orfhis[,,nn,fshnam[k]],1,function(x,beta){x*beta},beta=BETA[,kk]))       ## all historical ORF|TRF landed catch

			## Overwrite reconstructed landings if user has specified using reported landings (!!! this seems odd !!!):
			## Disabled this tweak for POP because it should not be necessary given numerous changes since 2011 (TechRep)
			## Also want option of reconstructing POP instead of using reported catches
			#if (!is.null(useYR1) && useYR1[k]>=1950 && useYR1[k]<=1995) {
			#	iifug = as.character(useYR1[k]:1995) ## no records later than 1995 in allhis|orfhis
			#	if (strSpp=="396")  ## because this species landings are available
			#		reccatRRFall[iifug,,nn] = t(apply(allhis[iifug,,nn,fshnam[k],"POP"],1,function(x,beta){x*beta},beta=BETA[,kk]))
			#}
		} ## end nn loop
		## --------------------------------------------------------
		## Reconstructed CA RRF landings from modern ORF (DB catch)
		## --------------------------------------------------------
		## Only place where gamma is applied alone (but is used in beta.gamma above).
		## Appears to be a subset of reccatRRFall, i.e. CAcatRRF.db ~= reccatRRFall[,,"CA"]
		##   but uses gamma on catmod vs. beta.gamma on orfhis
		CAcatRRF.db = t(apply(catmod[,mm,kk,orfSpp], 1, function(x,gamm){x*gamm}, gamm=gamma[,kk]))  ## RRF from ORF in GFM+PH3+RRFhis
		CAcatORF.db = catmod[,mm,kk,orfSpp]  ## ORF in GFM+PH3+RRFhis
		icom = intersect(dimnames(reccatRRFall)[[1]],dimnames(CAcatRRF.db)[[1]])  ## icom = in common
		if (length(icom)>0) {
			## Compares the two methods for estimating reconstructed RRF in CA and chooses the largest value
			CAcatRRF.db.com  = CAcatRRF.db[icom,,drop=FALSE]
			CAcatRRF.ndb     = reccatRRFall[,,"CA"]
			CAcatRRF.ndb.com = CAcatRRF.ndb[icom,,drop=FALSE]
			## ---------------------------------------------------
			## Is the modern recon (DB) > historical recon (nonDB)
			## ---------------------------------------------------
			useMod = CAcatRRF.db.com > CAcatRRF.ndb.com

			## From 1985-95 there was a lot of misreported catch so always use 
			## recon from gamma on database ORF not beta.gamma on historical ORF (RH 190923)
			useMod[intersect(rownames(useMod),as.character(1985:1995)),] = TRUE

			## Use adjusted modern RRF for Langara Spit -- not sure about the logic of this
			if (k==1 && !useLS)
				useMod[intersect(1983:1993,icom),"9"] = TRUE
			if (any(useMod))
				CAcatRRF.ndb.com[useMod] = CAcatRRF.db.com[useMod]
			reccatRRFall[icom,,"CA"] = CAcatRRF.ndb.com
		}
		## ------------------------------------------------------------------------------------------------
		## We are not trying to reconstruct ORF for user-specified years.
		## There is an overlap period between orfhis and reported ORF where landings should be similar.
		## Here we take the maximum during the overlap, though this does not affect the RRF reconstruction.
		## ------------------------------------------------------------------------------------------------
		icon = intersect(dimnames(reccatORFall)[[1]],dimnames(repcatORF)[[1]])
		if (length(icon)>0) {
			CAcatORF.ndb     = reccatORFall[,,"CA"]
			CAcatORF.ndb.com = CAcatORF.ndb[icon,,drop=FALSE]
			CAcatORF.db.com  = CAcatORF.db[icon,,drop=FALSE]
			## ---------------------------------------------------
			## Is the modern recon (DB) > historical recon (nonDB)
			## ---------------------------------------------------
			useMod = CAcatORF.db.com > CAcatORF.ndb.com

			## From 1985-95 there was a lot of misreported catch so always use 
			##   database ORF not historical ORF (RH 190923)
			##   probably redundant to checking reported ORF below.
			useMod[intersect(rownames(useMod),as.character(1985:1995)),] = TRUE

			if (any(useMod))
				CAcatORF.ndb.com[useMod] = CAcatORF.db.com[useMod]
			repcatORF.com = repcatORF[icon,,drop=FALSE]

			## -------------------------------------
			## Is the reported ORF > historical ORF?
			## -------------------------------------
			useNod = repcatORF.com > CAcatORF.ndb.com
			if (k==1 && !useLS) ## use adjusted modern ORF for Langara Spit
				useNod[intersect(1983:1993,icon),"9"] = TRUE
			if (any(useNod))
				CAcatORF.ndb.com[useNod]  = repcatORF.com[useNod]
			reccatORFall[icon,,"CA"] = CAcatORF.ndb.com
		}

		## -----------------------------------------------------------------------------------
		## Add any reconstructed ancient catch from (trawl,trap,h&l), essentially from 1945-1950
		##   (keep separate form MEDYRS)
		## This reconstructed catch does not use Dominion Bureau catch
		## -----------------------------------------------------------------------------------
		iiaa = as.character(ancyrs)
		## Update ancient catch for all nations
		for (nn in nat) {
			ancientRRF[iiaa,mm,kk,nn] = ancientRRF[iiaa,mm,kk,nn] + reccatRRFall[iiaa,mm,nn]
			ancientORF[iiaa,mm,kk,nn] = ancientORF[iiaa,mm,kk,nn] + reccatORFall[iiaa,mm,nn]
			## Only add user-specified nations to catch reconstruction array
			if (nn %in% natUse) {
				sppnewRRF[iiaa,mm,kk]  = sppnewRRF[iiaa,mm,kk]  + reccatRRFall[iiaa,mm,nn]
				sppnewORF[iiaa,mm,kk]  = sppnewORF[iiaa,mm,kk]  + reccatORFall[iiaa,mm,nn]
			}
		}
		## 'sppnewRRF' only contains fishery catch from 1918-1950 at this stage

		## ------------------------------------------------------------(RH 240314)
		## Need to determine reccatRRF ahead of the next section because
		## catmod (GFM+PH3+RRFhis) now contains catches from potentially foreign sources
		## -----------------------------------------------------------------------
		reccatRRF = apply(reccatRRFall,c("year","major"),function(x){0})  ## initialise array
		reccatORF = apply(reccatORFall,c("year","major"),function(x){0})
		for (nn in natUse){
			if (nn %in% "CA") {
				## If Canadian, extract Canadian reconstructed catch
				reccatRRF = reccatRRF + reccatRRFall[,,"CA"]
				reccatORF = reccatORF + reccatORFall[,,"CA"]
			} else {
				## Foreign fleets -- Canada's EEZ established in 1977 (allow for some catch in 1977, none afterwards)
				iimed = as.character(MEDYRS[1]:1977)  
				## ----------------------------------------------------------------------
				## Kate Rutherford (2016-11-14):
				## For many years after EEZ, Canadian trawlers landed their fish in Washington
				## state (Blaine, Bellingham). The fish were still caught in Canada.
				## ----------------------------------------------------------------------
				if (nn %in% "US")
					iimed = as.character(MEDYRS)  ## This was first active in ver.161115.
				reccatRRF[iimed,mm] = reccatRRF[iimed,mm] + reccatRRFall[iimed,mm,nn]
				reccatORF[iimed,mm] = reccatORF[iimed,mm] + reccatORFall[iimed,mm,nn]
			}
		} ## end nn loop (nat)

		## --------------------------------------------------------------------------------------------
		## Detect modern RRF landings and mesh with reconstructed RRF landings if no `useYR1' specified
		## --------------------------------------------------------------------------------------------
		if (is.null(useYR1)) {  ## may need work
			.flush.cat(paste0("   automatically detecting modern catches of ",strSpp,";\n"))
			irep   = dimnames(repcatRRF)[[1]]
			xrep = as.numeric(irep)
			## ------------------------------------------------------------------------------------
			## Start believing reported catch when it exceeds the 10th percentile of non-zero catch
			## ------------------------------------------------------------------------------------
			x0 = apply(repcatRRF,2,function(y,x){x[y>quantile(y[y>0],0.10)][1]}, x=xrep) ## vector of start years for each major
			x0 = pmax(x0,max(ancyrs)+1) ## cannot go back into the ancient years
			x0 = pmin(x0,1996)          ## have to believe all catches from 1996 on
			x0[is.na(x0)] = 1996        ## ditto 
			zrep = sapply(x0,function(x,xrep){xrep>=x},xrep=xrep)
			sppnewRRF[irep,jj,kk][zrep] = repcatRRF[zrep]   ## use automatically detected repcat catch (CA)
			sppnewORF[irep,jj,kk][zrep] = repcatORF[zrep]   ## use automatically detected repcat catch (CA)
			## ----------------------------------------------------------
			## select the reconstructed catch for reported catch not used
			## ----------------------------------------------------------
			irec = dimnames(reccatRRFall)[[1]]
			xrec = as.numeric(irec)
			## ------------------------------------------------------------------
			## identify reconstructed catch that had no believable reported catch
			## ------------------------------------------------------------------
			#zrec = sapply(x0,function(x,xrec){xrec<x & xrec>rev(ancyrs)[1]},xrec=xrec)  ## years after 1950 and before first reported catch
			zrec = sapply(x0,function(x,xrec){xrec<x & xrec>rev(ancyrs)[1]},xrec=xrec)  ## years after 1950 and before first reported catch
			zall = array(TRUE,dim=c(length(irec),length(jj)))
			for (nn in natUse){
				zuse = if (nn %in% "CA") zrec else zall
				sppnewRRF[irec,jj,kk][zuse] =  sppnewRRF[irec,jj,kk][zuse] + reccatRRFall[irec,jj,nn][zuse]
				sppnewORF[irec,jj,kk][zuse] =  sppnewORF[irec,jj,kk][zuse] + reccatORFall[irec,jj,nn][zuse]
			}
			yrs.rec[[kk]][["xrec"]] = sapply(as.data.frame(zrec),function(z){xrec[z]},simplify=FALSE)  ## record reconstructed years
			yrs.rec[[kk]][["xrep"]] = sapply(as.data.frame(zrep),function(z){xrep[z]},simplify=FALSE)  ## record reported years
		} ## end if useYR1
		else { ## useYR1 specified
			## -------------------------------------------------------------------------
			## Needed because estimation from orfSpp cannot occur before 1996 at present
			## useYRstart = min(ifelse(is.na(useYR1[k]), useYR1def, useYR1[k]), 1996)
			## Below, irec2 now allows the use of useYRstart later than 1996 (rev.180228)
			useYRstart = min(ifelse(is.na(useYR1[k]), useYR1def, useYR1[k]))
			## -------------------------------------------------------------------------

			## ------------------------------------------------------------------
			## [DEPRECATED] because may wish to override reported catch with gamma*ORF
			## POP trawl catch relatively well known back to 1956
			#if (strSpp=="396" && k==1) useYRstart = 1956
			## YYR H&L catch relatively well known back to 1982
			#if (any(strSpp==c("442","396")) && any(k==c(4,5))) useYRstart = 1982
			## ------------------------------------------------------------------

			useYRS = useYRstart:rev(MODYRS)[1]
			irep   = as.character(useYRS)
			yrs.rec[[kk]][["xrep"]] = irep  ## record reported years
			estYRend = useYRstart - 1

			## --------------------------------------------------
			## Only estimate RRF if useYRstart (believable catch)
			##   occurs later than 1950 and is earlier than 1996
			## --------------------------------------------------
			if (estYRend > MEDYRS[1]) { ## weave recostructed and reported catch
				.flush.cat(paste0("   estimating ",strSpp," from ",MEDYRS[1]," to ",estYRend," using gamma;\n"))
				estYRS = MEDYRS[1]:estYRend
				irec   = as.character(estYRS)
				## -----------------------------------------------
				## Combine estimated RRF landings (earlier period)
				##   with reported RRF landings (later period)
				## Note: repcatRRF now contains reported catch from GFM+PH3+RRFhis (not just CA)
				##   and reccatRRF has been summed across nations (above) for comparability
				## -----------------------------------------------
				yrs.rec[[kk]][["xrec"]] = irec    ## record reconstructed years
				#reccatRRF = reccatRRFall[,,"CA"]  ## RRF catch from ORF * beta.gamma
				#reccatORF = reccatORFall[,,"CA"]
				#reccatRRF = apply(reccatRRFall,c("year","major"),sum,na.rm=TRUE)
				#reccatORF = apply(reccatORFall,c("year","major"),sum,na.rm=TRUE)
				## Reconstructed catch for period specified by reported catch
				## If user specifies useYR1 later than 1995, then use RRF calculated from ORF * gamma
				irec2  = setdiff(irec,rownames(reccatRRF))  ## years later than 1995 (when orfhis stops)
				if (length(irec2)==0) {
					## row bind reconstructed (medieval) and reported catches (medieval + modern)
					comcatRRF = rbind(reccatRRF[irec,,drop=FALSE], repcatRRF[irep,,drop=FALSE])
					comcatORF = rbind(reccatORF[irec,,drop=FALSE], repcatORF[irep,,drop=FALSE])
				} else {
					## row bind reconstructed (medieval + modern) and reported catches (modern)
					comcatRRF = rbind(reccatRRF[setdiff(irec,irec2),,drop=FALSE], CAcatRRF.db[irec2,,drop=FALSE], repcatRRF[irep,,drop=FALSE])
					comcatORF = rbind(reccatORF[setdiff(irec,irec2),,drop=FALSE], CAcatORF.db[irec2,,drop=FALSE], repcatORF[irep,,drop=FALSE])
				}
			} else {  ## only use reported catch
				## No reconstructed years in the medieval period; just use reported years
				yrs.rec[[kk]][["xrec"]] = "none"  ## record reconstructed years
				comcatRRF = repcatRRF[irep,,drop=FALSE]
				comcatORF = repcatORF[irep,,drop=FALSE]
			}
			iicc = dimnames(comcatRRF)[[1]]
			jjcc = dimnames(comcatRRF)[[2]]
			sppnewRRF[iicc,jjcc,kk] = sppnewRRF[iicc,jjcc,kk] + comcatRRF[iicc,jjcc,drop=FALSE]
			sppnewORF[iicc,jjcc,kk] = sppnewORF[iicc,jjcc,kk] + comcatORF[iicc,jjcc,drop=FALSE]

#			## Scroll through nationalities and accumulate catch [DEPRECATED] (done above)
#			## sppnewRRF|sppnewORF only contains ancient catch at this point
#			for (nn in natUse){
#				if (nn %in% "CA") {
#					## If Canadian, add the comcat matrix generated above
#					sppnewRRF[iicc,jjcc,kk] = sppnewRRF[iicc,jjcc,kk] + comcatRRF[iicc,jjcc,drop=FALSE]
#					sppnewORF[iicc,jjcc,kk] = sppnewORF[iicc,jjcc,kk] + comcatORF[iicc,jjcc,drop=FALSE]
#				} else {
#					## Foreign fleets -- Canada's EEZ established in 1977 (allow for some catch in 1977, none afterwards)
#					iimed = as.character(MEDYRS[1]:1977)  
#					## ----------------------------------------------------------------------
#					## Kate Rutherford (2016-11-14):
#					## For many years after EEZ, Canadian trawlers landed their fish in Washington
#					## state (Blaine, Bellingham). The fish were still caught in Canada.
#					## ----------------------------------------------------------------------
#					if (nn %in% "US")
#						iimed = as.character(MEDYRS)  ## This was first active in ver.161115.
#					sppnewRRF[iimed,jjcc,kk] = sppnewRRF[iimed,jjcc,kk] + reccatRRFall[iimed,jjcc,nn]
#					sppnewORF[iimed,jjcc,kk] = sppnewORF[iimed,jjcc,kk] + reccatORFall[iimed,jjcc,nn]
#				}
#			} ## end nn loop (nat)
		} ## end else useYR1
		## `sppnewRRF' now contains additional fishery catch from 1951 to current year at this stage (previously only 1918-1950 catch)

		if (ascii.tables) {
			recLfileRRF = paste0(tabDir,"/",fidfive[k],"-",strSpp,"-Landed-Recon-",numdate,".csv")
			recLfileORF = paste0(tabDir,"/",fidfive[k],"-",orfSpp,"-Landed-Recon-",numdate,".csv")
			write.csv(sppnewRRF[,,kk],recLfileRRF)
			write.csv(sppnewORF[,,kk],recLfileORF)
			for (recLfile in c(recLfileRRF,recLfileORF)){
				cat("\nYears,start,end\n",file=recLfile,append=TRUE)
				cat(paste0("Ancient,",ancyrs[1],",",rev(ancyrs)[1],"\n"),file=recLfile,append=TRUE)
				cat(paste0("Medieval,",irec[1],",",rev(irec)[1],"\n"),file=recLfile,append=TRUE)
				cat(paste0("Modern,",irep[1],",",rev(irep)[1],"\n"),file=recLfile,append=TRUE)
			}
		}

		## ----------------------------------------------------------------------
		## Add in the RRF discards
		## discard rate - either RRF discard:RRF landed or RRF discard:TAR landed
		## ----------------------------------------------------------------------
		dr = delta[,kk,"dr"]
		## ------------------------------------------------------------
		## inone = years when RRF discards assumed reported in landings
		## icalc = years when RRF discards calculated by rates
		## idata = years when RRF discards reported as data
		## ------------------------------------------------------------
		## Discard list now determined by argument 'disyrs' (rev.180301)
		##  trawl and h&l rockfish use 'landed' for discard rate;
		##  other fisheries (2:4) use halibut, sablefish, dogfish+lingcod, respectively
		discard.regimes  = list(inone=NA, icalc=NA, idata=NA)
		if (all(is.na(disyrs[[k]]))) {
			disyr = switch(k,1996,2005,2005,2005,2005)
			discard.regimes[["inone"]] = ALLYRS[1]:(disyr-1)
			discard.regimes[["idata"]] = disyr:ALLYRS[nyrs]
		} else {
			if (ALLYRS[1] < disyrs[[k]][1])         discard.regimes[["inone"]] = ALLYRS[1]:(disyrs[[k]][1]-1)
			if (!all(is.na(disyrs[[k]])))           discard.regimes[["icalc"]] = disyrs[[k]]
			if (ALLYRS[nyrs] > rev(disyrs[[k]])[1]) discard.regimes[["idata"]] = (rev(disyrs[[k]])[1]+1):ALLYRS[nyrs]
		}

		## --------------------------------------------------------
		## Special conditions for YYR (feedback from 2015 workshop)
		## --------------------------------------------------------
		if (strSpp %in% c("424","442")){
			## ---------------------------------------------------------
			## Trawl -- assume no calculated discards (Brian Mose, 2015)
			## ---------------------------------------------------------
			if (k==1) discard.regimes = list(inone=ALLYRS[1]:1996, icalc=NA, idata=1997:ALLYRS[nyrs])
			## ------------------------------------------------------------------------
			## Sensitivity -- Halibut bycatch applied back to 1918 (Chris Sporer, 2015)
			## ------------------------------------------------------------------------
			if (k==2 && "A.2" %in% sensitivity) discard.regimes = list(inone=NA, icalc=1918:2005, idata=2006:ALLYRS[nyrs])
		}
		unpackList(sapply(discard.regimes,as.character))
		if (!all(is.na(icalc))) {
			.flush.cat(paste0("   discards only estimated from ",icalc[1]," to ",rev(icalc)[1],".\n"))

			## ---------------------------------------------------------------
			## Calculate/retrieve the RRF discards using default discard rates
			## Changes here must also be made above -- search for [mirror1]
			## ---------------------------------------------------------------
			if (any(k==c(1,5))) {
				icalc = intersect(dimnames(sppnewRRF)[[1]],icalc)
				kcalc = sppnewRRF[,,kk]
			}
			if (any(k==c(2,3,4))) {
				kcalc = catmod[,,kk,"TAR"]                     ## Get TAR, which will be specific to each fishery
				if ( (k==2 && isThere("cat614")) ||            ## use Lynne's IPHC halibut data from Kong (2015)
				     (k==3 && isThere("cat455")) ||            ## use Ketchen's (1976) trawl catch of sablefish
				     (k==4 && isThere("cat467"))               ## use Ketchen's (1976) trawl catch of lingcod
				) {
					##---- Mesh original k TAR catch with supplemental k TAR catch
					zpos = apply(kcalc,1,function(x){all(x>0)}) ## determine when all BC areas have halibut|sablefish|lingcod catch
					#zyrs = names(zpos)[zpos][1:5]              ## use the first 5 years
					#area.hcat  = apply(kcalc[zyrs,],2,sum)     ## calculate total catch by area
					#parea.hcat = area.hcat/sum(area.hcat)      ## calculate the proportion catch by area
					zyrs = names(zpos)[zpos]                    ## use all years to calculate geometric mean
					year.pcat = t(apply(kcalc[zyrs,],1,function(x){x/sum(x)}))
					area.pcat = apply(year.pcat,2,calcGM)       ## calculate the proportion catch by area
					kvec = apply(kcalc, 1, sum, na.rm=TRUE)
					kyrs = names(kvec)

					## Retain any of the original k TAR catch that is larger than the supplemental k TAR catch
					## Expand annual coastwide catch to catch by areas 
					## (this obliterated the originally observed proportions of TAR by area).
					#catsup[kyrs] = pmax(kvec[kyrs], catsup[kyrs], na.rm=TRUE)

					## Supplemental years of k TAR (RH 190925)
					catsup = switch(kk, '2'=cat614, '3'=cat455, '4'=cat467)
					if (is.vector(catsup)) {
						## redistribute annual catches to PMFC areas
						syrs  = names(catsup)
						scalc = t(sapply(catsup,function(x,p){x*p},p=area.pcat))
					} else {
						## annual catches already distributed to PMFC areas
						syrs  = rownames(catsup)
						scalc = catsup
					}
					#ksyrs = .su(c(kyrs,syrs))
					ksyrs = as.character(min(c(kyrs,syrs)):max(c(kyrs,syrs)))
					kscalc = array(0,dim=c(length(ksyrs),length(jj)), dimnames=list(ksyrs,jj))
					kscalc[rownames(kcalc),colnames(kcalc)] = kcalc          ## add original TAR catches
					syrs.use = setdiff(syrs,kyrs[apply(kcalc,1,countVec)>0]) ## use only supplemental TAR data if it's missing from original
					kscalc[syrs.use,colnames(scalc)] = scalc[syrs.use,]      ## add supplemental TAR catches
					kcalc  = kscalc  ## reset TAR object to merging of original and suppplemental
					icalc  = intersect(dimnames(kcalc)[[1]],icalc)
					if (saveinfo) packList(c("area.hcat","parea.hcat"),"CR",tenv=.PBStoolEnv)
				} else {
					icalc = intersect(dimnames(catmod)[[1]],icalc)
				}
			}
			if (ascii.tables)
				write.csv(kcalc, file=paste0(tabDir,"/",fidfive[k],"-TAR-Dbase-",numdate,".csv")) ## (RH 190925)
			kcalc = kcalc[icalc,jj,drop=FALSE] 
			disC  = t(apply(kcalc,1,function(x,dr){x*dr},dr=dr))
		} else 
			disC = NULL

		## ----------------------------------------------
		## Assume that there are always reported discards
		## ----------------------------------------------
		idata = intersect(dimnames(catmod)[[1]],idata)
		disD  = catmod[,,kk,"discard"]
		disD  = disD[idata,jj,drop=FALSE]

		## -------------------------------------------------
		## Add in the RRF discards (calculated and reported)
		## -------------------------------------------------
		if (!is.null(disC))
			sppnewRRF[icalc,jj,kk]  = sppnewRRF[icalc,jj,kk] + disC[icalc,jj] ## calculated discards
		sppnewRRF[idata,jj,kk]  = sppnewRRF[idata,jj,kk] + disD[idata,jj]    ## reported discards

		## -------------------------------------------------------------------------
		## PHMA feels that 1999 catch should be only 15-20% lower than that for 1998
		## Letter: Chris Sporer, PHMA, 2015-06-12
		## -------------------------------------------------------------------------
		if (strSpp=="442" && !is.null(list(...)$phma) && list(...)$phma && k==2) {
			phma.1999.orig = sppnewRRF["1999",,"2"]
			phma.1999.prop = phma.1999.orig/sum(phma.1999.orig)
			phma.1999.targ = (1-0.175) * sum(sppnewRRF["1998",,"2"])
			phma.1999.adj  = phma.1999.prop * phma.1999.targ
			sppnewRRF["1999",,"2"] = phma.1999.adj
			## check : (sum(sppnewRRF["1998",,"2"])-sum(phma.1999.adj))/sum(sppnewRRF["1998",,"2"]) should equal 0.175
#browser();return()
		}

		## ------------------------------
		## Discard (releases) output file
		## ------------------------------
		recDfile = paste0(tabDir,"/",fidfive[k],"-",strSpp,"-Discard-Recon-",numdate,".csv")
		disT = disD[idata,jj]
		if (!is.null(disC))
			disT = rbind(disC[icalc,jj],disT)
		if (!all(is.na(inone))) {
			disN=array(0,c(length(inone),length(jj)),dimnames=list(inone,jj)) # construct zero discard matrix in years before regulations
			disT = rbind(disN[inone,jj],disT) #[c(icalc,idata),jj])
		}
		if (ascii.tables) {
			write.csv(disT,file=recDfile)
			cat("\nYears,start,end\n",file=recDfile,append=TRUE)
			if (all(is.na(inone)))
				cat("Discards always assumed\n",file=recDfile,append=TRUE)
			else
				cat(paste0("No discards,",inone[1],",",as.numeric(rev(inone)[1])+1,"\n"),file=recDfile,append=TRUE)
			if (all(is.na(icalc)))
				cat("No calculated discards\n",file=recDfile,append=TRUE)
			else
				cat(paste0("Calculated,",icalc[1],",",rev(icalc)[1],"\n"),file=recDfile,append=TRUE)
			cat(paste0("Database,",idata[1],",",rev(idata)[1],"\n"),file=recDfile,append=TRUE)
		}

		## -----------------
		## Catch output file
		## -----------------
		recCfile = paste0(tabDir,"/",fidfive[k],"-",strSpp,"-Catch-Recon-",numdate,".csv")
#browser();return()
		if (ascii.tables) {
			write.csv(sppnewRRF[,,kk],file=recCfile)
			cat("\nLanded,start,end\n",file=recCfile,append=TRUE)
			cat(paste0("Calculated,",irec[1],",",rev(irec)[1],"\n"),file=recCfile,append=TRUE)
			cat(paste0("Database,",irep[1],",",rev(irep)[1],"\n"),file=recCfile,append=TRUE)
			cat("\nDiscard,start,end\n",file=recCfile,append=TRUE)
			if (all(is.na(inone)))
				cat("Discards always assumed\n",file=recCfile,append=TRUE)
			else
				cat(paste0("No discards,",inone[1],",",as.numeric(rev(inone)[1])+1,"\n"),file=recCfile,append=TRUE)
			if (all(is.na(icalc)))
				cat("No calculated discards\n",file=recCfile,append=TRUE)
			else
				cat(paste0("Calculated,",icalc[1],",",rev(icalc)[1],"\n"),file=recCfile,append=TRUE)
			cat(paste0("Database,",idata[1],",",rev(idata)[1],"\n"),file=recCfile,append=TRUE)
		}
		if (diagnostics){
			plotDiag(reccatRRF, paste0("reccat for ",fidnam[k]," by major"), col=clrs.major[mm])
			plotDiag(repcatRRF, paste0("repcat for ",fidnam[k]," by major"), col=clrs.major[mm])
			plotDiag(disC, paste0("disC for ",fidnam[k]," by major"), col=clrs.major[mm])
			plotDiag(disD, paste0("disD for ",fidnam[k]," by major"), col=clrs.major[mm])
			plotDiag(sppnewRRF[,,kk], paste0("sppnew for ",fidnam[k]," by major"), col=clrs.major[mm])
		}
		## Save catch matrices for later inspection or debugging (RH 240221)
		catmats[[fidnam[k]]] = list(reccatORF=reccatORF, reccatRRF=reccatRRF, repcatORF=repcatORF, repcatRRF=repcatRRF, reccatRRFall=reccatRRFall, reccatORFall=reccatORFall)
	} ## end k (fid) loop

	} ## end fold05

	## Save the various reccat and repcat arrays (RH 240221)
	save ("catmats", file=paste0(datDir,"/catmats.rda"))

	attr(sppnewRRF,"yrs.rec") = yrs.rec # years of reconstructed and reported catch
	sppnewRRF[,,"10"] = apply(sppnewRRF[,,as.character(fid)],1:2,sum)  ## sum across fisheries
	
	pmfc.major = pmfc$major; names(pmfc.major) = pmfc$gmu

	## -------------------------------------------------------------------
	## OFFICIAL CATCH tables already include catches from charter surveys,
	## so not necessary to add to combo. (2015-03-26)
	## -------------------------------------------------------------------
	addGFB = FALSE
	if (addGFB) {
		.flush.cat("   adding in survey catches.\n")
		gfbcat = as.matrix(gfbcat)
		names(dimnames(gfbcat))=c("year","major")
		dimnames(gfbcat)$major = pmfc.major[dimnames(gfbcat)$major]
		iii=intersect(dimnames(sppnewRRF)$year,dimnames(gfbcat)$year)
		jjj=intersect(dimnames(sppnewRRF)$major,dimnames(gfbcat)$major)
		sppnewRRF[iii,jjj,"10"] = sppnewRRF[iii,jjj,"10"] + gfbcat[iii,jjj]  # add survey catches to combined only
	}
#browser(); return()
	if (diagnostics){
		plotDiag(sppnewRRF[,,"10"], paste0("sppnew for ",fidnam[10]," by major"), col=clrs.major[mm])
		collectFigs(path=diaDir,width=5,capskip=-20,is.fnum=TRUE)
	}
	expr=paste0("cat",strSpp,"rec=sppnewRRF; save(\"cat",strSpp,"rec\",file=\"",datDir,"/cat",strSpp,"rec.rda\")")
	eval(parse(text=expr))

	if (saveinfo)
		packList(c("HISYRS","MODYRS","ALLYRS","inone","icalc","idata","disC","disD","sppnewRRF","beta.gamma"),"CR",tenv=.PBStoolEnv)

	## ----------------
	## Plot the results
	## ----------------
	.flush.cat("Plotting and reporting reconstruction results ...\n\n")
	for (k in fidout) {
		yrs = as.numeric(dimnames(sppnewRRF)$year)
		plotRecon(sppnewRRF, strSpp=strSpp, major=major, fidout=k, years=yrs, png=png, eps=eps, wmf=wmf, figDir=figDir, timestamp=numdate, shadier=TRUE) # use user-specified major
	}
	fidlab = c("Trawl","Halibut","Sablefish","Dogfish-Lingcod","H&L Rockfish","Sablefish + ZN",
		"Sablefish + Halibut","Dogfish","Lingcod",paste0("Combined Fisheries",ifelse(addGFB," + Surveys","")))
	
	## ---------------------------------------------
	## ADMB catch data file for the combined fishery
	## ---------------------------------------------
	admdat = paste0(tabDir,"/admb-cat",strSpp,"-",numdate,ifelse(useGFM,"","-multiDB"),".dat")
	cat(paste0("# Catch History - ",species[strSpp,"latin"]," (built ",bigdate,")",ifelse(useGFM,""," multiDB")),"\n\n",sep="",file=admdat)
	mess = c(
		"# number of years of catch data 'NYearCat'\n", ALLYRS[nyrs]-ALLYRS[1]+1, "\n",
		"# start year 'tilde{t}' in model\n", ALLYRS[1], "\n",
		"# end year\n", ALLYRS[nyrs], "\n",
		"# number of areas\n", length(mm), "\n",
		"# areas (column headings of matrix), areas 5, 6 and 7 give QCS.\n",
		paste(mm,collapse="   "), "\n\n",
		"# catch history in tonnes (all fisheries combined)\n",
		"#  first column lists the years, each row gives the\n",
		"#  catch in corresponding area that year. Size is therefore\n",
		"#  (end year - start year + 1) * (number of areas + 1)\n\n")
	cat(paste(mess,collapse=""),file=admdat,append=TRUE)
	for (i in 10){
		ii = as.character(i)
		cat("# ",fidlab[i],"\n",sep="",file=admdat,append=TRUE)
		mess = cbind(year=as.numeric(rownames(sppnewRRF)),
			apply(sppnewRRF[,,ii],1,function(x){
			paste(show0(format(round(x,3),scientific=FALSE,width=12,justify="right"),3,add2int=TRUE),collapse="") } ) )
		cat(paste(apply(mess,1,paste,collapse=""),collapse="\n"),sep="",file=admdat,append=TRUE)
		cat("\n\n",file=admdat,append=TRUE) }

	## ------------------------------------------
	## Output specified FID catch (fidout) as CSV
	## ------------------------------------------
	onam=paste0(tabDir,"/Catch-History-",strSpp,"-",numdate,ifelse(useGFM,"","-multiDB"),".csv")  ## output file name
	cat(paste0("Catch History - ",species[strSpp,"latin"]," (built ",bigdate,")",ifelse(useGFM,""," multiDB")),"\n",sep="",file=onam)
	xlab=dimnames(sppnewRRF)[[1]];  xpos=(1:length(xlab))-.5
	for (i in fidout){
		ii=as.character(i)
		cat(fidlab[i],"\n",sep="",file=onam,append=TRUE)
		cat("year,",paste(colnames(sppnewRRF),collapse=","),"\n",sep="",file=onam,append=TRUE)
		apply(cbind(year=rownames(sppnewRRF),sppnewRRF[,,ii]),1,function(x){cat(paste(x,collapse=","),"\n",sep="",file=onam,append=TRUE)})
		cat("\n",file=onam,append=TRUE)
	}
	if (saveinfo) {
		packList(c("plotname","clrs.major","clrs.fishery","fidlab"),"CR",tenv=.PBStoolEnv)
		ttget(CR)
		save("CR",file=paste0(datDir,"/CR",strSpp,".rda"))
	}
	if (eps)
		collectFigs(path=catDir, ext="eps", fout=paste0("Catch-Recon-Summary-",strSpp), width=6.75, pattern="Catch-History")
	.flush.cat(paste0("-----Finished reconstructing catch for ",species[strSpp,"latin"],"-----\n-----   [ see dir: ",run.name," ]\n\n"))
#browser();return()
	invisible(sppnewRRF)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~buildCatch


## plotDiag-----------------------------2017-10-12
## Plot diagnostic data for catch reconstructions.
## ---------------------------------------------RH
plotDiag =function(x, description="something",
	col=c("red","coral","gold","green2","skyblue","blue","blueviolet","purple4"),...)
{
	if (dev.cur()>1) { oldpar=par(no.readonly=TRUE); on.exit(par(oldpar)) }
	dots=list(...)
	if (!is.matrix(x) & !is.data.frame(x)) {
		stop(cat(paste(deparse(substitute(x)), "is not a matrix or a data frame.\n")))
	}
	xlab = names(dimnames(x))[1]; zlab=names(dimnames(x))[2]; ylab=paste("Value for each ",zlab,sep="")
	xnam = dimnames(x)[[1]]
	xchr = strsplit(xnam,split="")
	if (all(sapply(xchr,function(x){all(x%in%c(as.character(0:9),"."))})))
		xval=as.numeric(xnam)
	else xval = 1:length(xnam)
	#xval = as.numeric(dimnames(x)[[1]]); 
	xlim=range(xval); ylim = range(x)
	if (all(x==0)) return(invisible())
	xx = x; xx[xx==0] = NA
	cnam = colnames(x); nc = length(cnam) # column names
	col =rep(col,nc)[1:nc]; names(col) = cnam
	pD = ttcall(PBStool)$pD
	eps= ttcall(PBStool)$eps
	wmf= ttcall(PBStool)$wmf
	if (!eps & !wmf) eps=TRUE
	plotname = paste0(diaDir,"/pD",pad0(pD,3),"-",gsub(" ","-",description))
#browser();return()
	if (eps) 
		postscript(file=paste(plotname,".eps",sep = ""), width=6.5,height=5,paper="special")
	else if (wmf && .Platform$OS.type=="windows")
		do.call("win.metafile",list(filename=paste(plotname,".wmf",sep = ""), width=6.5, height=5))
	expandGraph()
	if (!is.null(dots$type) && dots$type=="bars") {
		xpos = barplot(t(xx),beside=TRUE,col=col,ylim=ylim,xlab=xlab,ylab=ylab,space=c(0,1.5))
		xsum = apply(t(x),2,max,na.rm=TRUE); xadj=0.5
		xval = apply(xpos,2,median)
	} else {
		plot(0,0,type="n",xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab)
		sapply(1:nc,function(cpos,xtab,xval,clrs){
			cval = cnam[cpos]
			lines(xval,xtab[,cval],lwd=2,col=clrs[cval])
			}, xtab=xx, xval=xval,clrs=col)
		xsum = apply(x,1,max,na.rm=TRUE); xadj=0
	}
	xmin = is.element(xsum,min(xsum))
	zuse = apply(x,2,sum)>0  # which columns have data
	#legend("topleft",col=col[zuse],lwd=2,legend=cnam[zuse],inset=0.025,bty="n",title=zlab)
	legend(xval[xmin][1],par()$usr[4],col=col[zuse],lwd=2,legend=cnam[zuse],bty="n",title=zlab,xjust=xadj)
	if (eps|wmf) dev.off()
	#eval(parse(text="PBStool$pD <<- pD +1"))
	ttget(PBStool); PBStool$pD <- pD + 1; ttput(PBStool)  # increment number of diagnostic plot
#browser();return()
	invisible() }
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotDiag


## plotGREFS----------------------------2024-10-24
## Plot gamma for reference years by fishery.
## ---------------------------------------------RH
plotGREFS = function(dat, years=1996:2019, majors=3:9, fid=1,
   strSpp="394", addRGM=FALSE, onefig=FALSE, vlines, rlines, legpos, 
   png=FALSE, pngres=400, PIN=c(12,9), lang="e")
{
	calcRGM = function(x) { ## running geometric mean
		mess = paste0("calcGM(x[1:",1:length(x),"])")
		RGM  = sapply(mess,function(y){eval(parse(text=y))})
		return(as.vector(RGM))
	}
	mcols = c("grey","mediumblue","dodgerblue","goldenrod1","sienna1","red","green2","green4")
	names(mcols) = c(1,3:9)
	fidnams = c("Trawl","Halibut","Sablefish","Dog/Ling","H&L Rock")
	cyears  = dimnames(dat)[[1]]
	years   = intersect(years,as.numeric(cyears))
	cyears  = as.character(years)
	onam    = paste0("grefs", strSpp, "(", paste0(.su(range(years)),collapse="-"), ")")
#browser(); return()
	if (addRGM)
		onam = paste0(onam,".rgm")
	if (onefig) {
		onam = paste0(onam,".fid",paste0(fid,collapse=""))
		fout.e = onam
		changeLangOpts(L=lang)
		#fout = switch(lang, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		fout = switch(l, 'e' = paste0("./english/",fout.e), 'f' = paste0("./french/",fout.e) )
		if (png){
			clearFiles(paste0(fout,".png"))
			png(paste0(fout,".png"), units="in", res=pngres, width=PIN[1], height=PIN[2])
		}
		par(mfrow=c(1,length(fid)), mar=c(3,0,1,0), oma=c(0,3,0,1), mgp=c(1.6,0.5,0))
	}
	for (k in fid) {
		if (!onefig) {
			fout.e = paste0(onam,".fid",k)
			changeLangOpts(L=lang)
			#fout = switch(lang, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
			fout = switch(l, 'e' = paste0("./english/",fout.e), 'f' = paste0("./french/",fout.e) )
			if (png){ 
				clearFiles(paste0(fout,".png"))
				png(paste0(fout,".png"), units="in", res=pngres, width=PIN[1], height=PIN[2])
			}
			par(mfrow=c(1,1), mar=c(3,3,1,1), oma=c(0,0,0,0), mgp=c(1.6,0.5,0))
		}
		kdat = dat[cyears,,k]
		yting = !onefig || k == fid[1]
		ylim = c(0,ifelse(onefig, max(dat[cyears,as.character(majors),as.character(fid)]), max(kdat[cyears,as.character(majors)])))

		test = apply(kdat,2,function(x){ ## just in case we want to remove values >2 or 3 SDs
			ss=2*sd(x); mean(x)>=(x-ss) & mean(x)<=(x+ss)})
		plot(0,0, xlim=c(years[1],rev(years)[1]), ylim=ylim, type="n", xaxt="n", xlab=linguaFranca("Year",lang), yaxt=ifelse(yting,"s","n"), ylab=ifelse(yting,"gamma",""), cex.axis=1.2, cex.lab=1.5)
		if (onefig) mtext("gamma",side=2, outer=T, line=2, cex=1.5)
		if (!missing(vlines))
			abline(v=vlines, col="dimgrey", lty=5)
		if (!missing(rlines)) {
			#abline(v=rlines, col="red", lty=1, lwd=2)
			polygon(x=rlines[c(1,1,2,2)], y=par()$usr[c(3,4,4,3)], col=lucent("yellow",0.25), border=F)
		}
		#lines(years, kdat[cyears,"3"], col=lucent(mcols["3"],0.5), lwd=2)
#browser();return()
		sapply(as.list(majors),function(j){
			jj=as.character(j)
			lines(years, kdat[cyears,jj], col=lucent(mcols[jj],0.5), lwd=3)
			if (addRGM) {
				lines(years, calcRGM(kdat[cyears,jj]), lty=1, col="gainsboro", lwd=2)
				lines(years, calcRGM(kdat[cyears,jj]), lty=3, col=lucent(mcols[jj],1), lwd=2)
			}
		})
		axis(1, at=years[1]:rev(years)[1], tck=-0.01, labels=F)
#browser();return()
		axis(1, at=intersect(seq(1950,2050,ifelse(onefig,5,2)),years[1]:years[length(years)]), tck=-0.02, labels=T)
		if (length(years)!=length(years[1]:years[length(years)]))
			points(years, rep(par()$usr[3],length(years)), pch=21, col="black", bg="ghostwhite", xpd=NA)
		if (yting)
			axis(2, at=seq(0,1,0.1), tck=-0.01, labels=F)
		if (!yting){
			axis(2, at=seq(0,1,0.1), tck=-0.005, labels=F)
			axis(2, at=seq(0,1,0.1), tck=0.005, labels=F)
		}
		if (missing(legpos)) {
			LL = !onefig && k%in%c(1,2,3,4,5)
			if (yting || length(fid)==1) {
				addLegend(ifelse(LL,0.025,0.975), ifelse(LL,0.95,0.92), lty=1, lwd=2, col=rev(lucent(mcols[as.character(majors)],0.5)), legend=rev(c("3C","3D",paste0("5",LETTERS[1:5]))), bty="n", seg.len=3, xjust=ifelse(LL,0,1), cex=ifelse(png,0.7,1), y.intersp=ifelse(png,0.8,1))
			}
		} else {
			addLegend(legpos[1], legpos[2], xjust=ifelse(is.na(legpos[3]),1,legpos[3]), lty=1, lwd=2, col=rev(lucent(mcols[as.character(majors)],0.5)), legend=rev(c("3C","3D",paste0("5",LETTERS[1:5]))), bty="n", seg.len=3, cex=ifelse(png,0.7,1), y.intersp=ifelse(png,0.8,1))
		}
		addLabel(ifelse(strSpp%in%c("396") && fid==1,0.225, 0.05), 0.95, linguaFranca(fidnams[k],lang), cex=1.2, adj=c(0,0))
		if (png && !onefig) dev.off()
	} ; eop()
	if (png && onefig) dev.off()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotGREFS


## plotRecon----------------------------2024-10-24
## Plot reconstructed catch using barplots
## stacked by PMFC area.
## ---------------------------------------------RH
plotRecon = function(dat=cat440rec, strSpp="440", major=c(1,3:9), fidout=10, 
   years=1918:2018, xlab=seq(1920,2050,5), yrs.rec=attributes(dat)$yrs.rec,
   ymax=NULL, shade=FALSE, shadier=FALSE, figDir=getwd(), timestamp="",
   png=FALSE, pngres=400, eps=FALSE, wmf=FALSE, PIN=c(10,5), lang=c("e","f"),
   outnam=NULL )
{
	if (dev.cur()>1) { oldpar=par(no.readonly=TRUE); on.exit(par(oldpar)) }
	## Create a subdirectory called `french' for French-language figures
	createFdir(lang,figDir)

	fshnam=c("trawl", "h&l", "trap", rep("h&l",6), "combined","other") ## general category vector
	fidnam=c("trawl", "halibut", "sablefish", "sched2", "zn", "sabzn", "sabhal", "dogfish", "lingcod", "combined","other")
	fidlab=c(paste0("Fishery -- ",c("Trawl", "Halibut", "Sablefish", "Dogfish / Lingcod", "H&L Rockfish", "Sablefish + ZN",
		"Sablefish + Halibut", "Dogfish", "Lingcod")), "All Groundfish Fisheries", "Other Fisheries") #, "All Commercial Groundfish Fisheries")
	yy  = as.character(years)
	if (length(dim(dat))==2) ydat = dat[yy,,drop=FALSE]
	else                     ydat = dat[yy,,,drop=FALSE]
	MAJ = as.character(1:10); mm=as.character(major)
	clrs = rep("gainsboro",length(MAJ)); names(clrs)=MAJ
	#clrs[as.character(c(1,3:9))]=c("moccasin","blue","lightblue","yellow","orange","red","seagreen","lightgreen")  ## original colour scheme
	#clrs[as.character(c(1,3:9))]=c("moccasin","seagreen","lightgreen","yellow","orange","red","dodgerblue","lightblue1")  ## RH 230202 -- switch colours for 3CD and 5DE (North=blues, Central=ambers, South=greens)
	#clrs[as.character(c(1,3:9))]=c("moccasin","seagreen","lightgreen","yellow","orange","red","lightblue1","dodgerblue")  ## RH 230202 -- switch colours for 3CD and 5DE (North=blues, Central=ambers, South=greens)
	clrs[as.character(c(1,3:9))]=c("moccasin","forestgreen","lightgreen","yellow","orange","red","lightblue1","dodgerblue")  ## RH 240222 -- change colour for 3C from 'seagreen' to 'forestgreen' (softer green)
	mclrs=clrs[mm]
	data("pmfc", "species", package="PBSdata", envir=penv())
	XLAB = dimnames(ydat)[[1]];  xpos=(1:length(XLAB))-.5; zlab=is.element(XLAB,xlab)

	for (i in fidout){
		ii=as.character(i)
		if (length(dim(ydat))==2) idat = t(ydat[,mm])
		else idat = t(ydat[,mm,ii])
		if (is.null(ymax))
			ylim = c(0,max(apply(idat,2,sum)))
		else
			ylim = c(0,ymax)
		if (is.null(outnam))
			plotname = paste0(strSpp, "-Catch-History", ifelse(i==10,0,i), "-", fidnam[i], "-years(", min(years), "-", max(years), ")-major(", paste(major, collapse=""), ")")
		else
			plotname = outnam ## (RH 240222) really only good for one plot and/or debugging
		if (!is.null(timestamp) && !is.na(timestamp) && timestamp!="")
			plotname = paste0(plotname,"-",timestamp)

		fout.e = plotname
		for (l in lang) {
			if (l=="f") fout = paste0(figDir,"/french/",fout.e)
			else        fout = paste0(figDir,"/english/",fout.e)
			if (png) {
				clearFiles(paste0(fout,".png"))
				png(filename=paste(fout,".png",sep=""), units="in", res=pngres, width=PIN[1], height=PIN[2])
			}
			else if (eps) postscript(file=paste(fout,".eps",sep=""), width=PIN[1], height=PIN[2], fonts="mono", paper="special")
			else if (wmf && .Platform$OS.type=="windows")
				do.call("win.metafile", list(filename=paste(fout,".wmf",sep=""), width=PIN[1], height=PIN[2]))
			else  resetGraph()
			expandGraph(mar=c(3,3.2,1,0.5),mgp=c(1.6,.5,0))
			barplot(idat,col=0,space=0,xaxt="n",yaxt="n",xaxs="i",ylim=ylim)

			## Apply shading to delimit ancient, reconstructed, and reported (database) periods
			if (shadier) shade=TRUE ## (RH 240222)
			if (shade && is.null(yrs.rec)) { ## (RH 240305)
				polygon(par()$usr[c(1,1,2,2)], par()$usr[c(3,4,4,3)], border=FALSE, col=lucent("ivory",ifelse(shadier,0.5,0.25)))
			}
			if (shade && !is.null(yrs.rec)) {
				## shade the ancient years (Dominion Bureau of Stats)
				xanc = as.character(1918:1950)
				panc = (1:length(yy))[is.element(yy,xanc)]
				nanc = length(panc)
				if (nanc>1)
					polygon(panc[c(1,1,nanc,nanc)]+c(-1,-1,0,0),c(0,rep(par()$usr[4],2),0),border=FALSE,col=lucent("blue",ifelse(shadier,0.1,0.05)))
					#polygon(panc[c(1,1,nanc,nanc)]+c(-1,-1,0,0),c(0,rep(par()$usr[4],2),0),border=FALSE,col=ifelse(shadier,"aliceblue","#FAFCFF"))

				getRange = function(xyrs) {
					if (is.list(xyrs))
						xrange = max(sapply(xyrs,function(x){min(x,na.rm=TRUE)})) : min(sapply(xyrs,function(x){max(x,na.rm=TRUE)}))
					else
						xrange = xyrs
					#return((xrec))
					return(as.numeric(xrange))
				}
				## shade the reconstructed years
				xrec = lrec = list()
				if (ii %in% c("10","11")) {  ## (RH 240305) include non-trawl 11
					for (j in switch(ii,'10'=1,'11'=2,1):5) {
						jj = as.character(j)
						xrec[[jj]] = getRange(yrs.rec[[jj]][["xrec"]])
						lrec[[jj]] = c(min(xrec[[jj]]), max(xrec[[jj]]))
					}
					#xrec = getRange(xrec)
				} else {
					xrec[[ii]] = getRange(yrs.rec[[ii]][["xrec"]])
					lrec[[ii]] = c(min(xrec[[ii]]), max(xrec[[ii]]))
				}
				lapply(xrec, function(x){
					prec = (1:length(yy))[is.element(yy,x)]
					nrec = length(prec)
					if (nrec>1)  ## more than one year (to make a polygon)
						polygon(prec[c(1,1,nrec,nrec)]+c(-1,-1,0,0),c(0,rep(par()$usr[4],2),0),border=FALSE,col=lucent("salmon",ifelse(shadier,0.2,0.1)/length(xrec)))
#browser();return()
				})
				## year pos on barplot axis occurs at end (right side of bar)
				yr0.rec = .su(sapply(lrec,function(x){x[1]}))
				yy0.rec = match(yr0.rec,yy)-1
				yr1.rec = .su(sapply(lrec,function(x){x[2]}))
				yy1.rec = match(yr1.rec,yy)
				abline(v=c(yy0.rec, yy1.rec), lty=2)

#				prec = (1:length(yy))[is.element(yy,xrec)]
#				nrec = length(prec)
#				if (nrec>1)
#					polygon(prec[c(1,1,nrec,nrec)]+c(-1,-1,0,0),c(0,rep(par()$usr[4],2),0),border=FALSE,col=ifelse(shadier,"lightyellow","ivory"))

				## shade the reported years
				xrep = lrep = list()
				if (ii %in% c("10","11")) {  ## include non-trawl 11 (RH 240305)
					for (j in switch(ii,'10'=1,'11'=2,1):5) {
						jj = as.character(j)
						xrep[[jj]] = getRange(yrs.rec[[jj]][["xrep"]])
						lrep[[jj]] = c(min(xrep[[jj]]), max(xrep[[jj]]) + 1)
					}
					#xrep = getRange(xrep)
				} else {
					xrep[[ii]] = getRange(yrs.rec[[ii]][["xrep"]])
					lrep[[ii]] = c(min(xrep[[ii]]), max(xrep[[ii]]))
				}
				lapply(xrep, function(x){
					prep = (1:length(yy))[is.element(yy,x)]
					nrep = length(prep)
					if (nrep>1)
						polygon(prep[c(1,1,nrep,nrep)]+c(-1,-1,0,0),c(0,rep(par()$usr[4],2),0),border=FALSE,col=lucent("green",ifelse(shadier,0.2,0.1)/length(xrep)))
				})
				## year pos on barplot axis occurs at end (right side of bar)
				yr0.rep = .su(sapply(lrep,function(x){x[1]}))
				yy0.rep = match(yr0.rep,yy)-1
				yr1.rep = setdiff(.su(sapply(lrep,function(x){x[2]})),rev(years)[1])  ## get rid of last year
				yy1.rep = match(yr1.rep,yy)
				abline(v=c(yy0.rep, yy1.rep), lty=2)
#				prep = (1:length(yy))[is.element(yy,xrep)]
#				nrep = length(prep)
#				if (nrep>1)
#					polygon(prep[c(1,1,nrep,nrep)]+c(-1,-1,0,0),c(0,rep(par()$usr[4],2),0),border=FALSE,col=ifelse(shadier,"honeydew","mintcream"))

#				## delimit with dashed vertial line
#				if (panc[nanc]==(prec[1]-1))
#					abline(v=panc[nanc],lty=2)
#				if (prec[nrec]==(prep[1]-1))
#					abline(v=prec[nrec],lty=2)
#				else {
#					polygon(c(prec[c(nrec,nrec)],prep[c(1,1)])+c(0,0,-1,-1),c(0,rep(par()$usr[4],2),0),border=FALSE,col=ifelse(shadier,"ivory","white"))
#					abline(v=prec[nrec],lty=2)
#					abline(v=prep[1]-1,lty=2)
#				}
			}  ## end shady loop
			yaxp = par()$yaxp
			yint = yaxp[2]/yaxp[3]
			hlin = seq(yint,yaxp[2],yint)
			segments(rep(0,length(hlin)), hlin, rep(par()$usr[2], length(hlin)), hlin,col="gainsboro")
			barplot(idat, col=mclrs, space=0, cex.names=0.8, mgp=c(1.5,0.5,0), xaxt="n", xaxs="i", border="grey30", add=TRUE, lwd=0.3)
#browser();return()
			axis(1,at=xpos[zlab],labels=XLAB[zlab],tick=FALSE,las=3,mgp=c(0,.2,0),cex.axis=.8,hadj=1)
			addLabel(0.05,0.975, species[strSpp,"latin"], font=3, cex=1, col="#400080", adj=0)
			addLabel(0.05,0.925, linguaFranca(fidlab[i],l), cex=1.2, col="#400080", adj=0)
			mtext(linguaFranca("Year",l), side=1, line=1.75, cex=1.2)
			mtext(linguaFranca("Catch (t)",l), side=2, line=2, cex=1.3)
			addStrip(.05, 0.85, col=mclrs, lab=pmfc[mm,"gmu"]) ## RH: New function addition (151201)
			if (eps|png|wmf) dev.off()
		} ## end l (lang) loop
	}  ## end i (fidout) loop
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotRecon


## quickCat-----------------------------2020-02-19
##  Subsets a large dataset derived from query 'fos_mcatORF.sql'
##  to remove non-zero catches of the RRF (landed+discard).
##  This facilitates crossTab queries for data summaries.
## ---------------------------------------------RH
quickCat = function(dat, strSpp="000", removeSM=TRUE)  ## incorporated revampCat
{
	flds = colnames(dat)
	if (removeSM) ## seamounts
		dat    = zapSeamounts(dat)
	else if (!is.element("seamount",flds))
		dat = zapSeamounts(dat, only.mark=TRUE)
	if (!is.element("year",flds))
		dat$year  = as.numeric(substring(dat$date,1,4))
	if (!is.element("catKg",flds))
		dat$catKg = dat$landed + dat$discard
	dat = dat[dat$catKg>0 & !is.na(dat$catKg),]
	dat = dat[is.element(dat$major,3:9),]
	if (nrow(dat)==0) stop("No recorded catch for this species")

	if (strSpp %in% c("394","425")) {
		dat$stock = rep("",nrow(dat))
		dat$stock[is.element(dat$major,8:9)] = "BSR"  ## Blackspotted Rockfish
		dat$stock[is.element(dat$major,3:6)] = "RER"  ## Rougheye Rockfish
		dat$stock[is.element(dat$major,7)  ] = "HYB"  ## Hybrids of BSR and RER

		dat$fishery = rep("",nrow(dat))
		dat$fishery[is.element(dat$major,8:9) & is.element(dat$fid, c(1,9))] = "BSR_trawl"  ## Blackspotted trawl
		dat$fishery[is.element(dat$major,8:9) & is.element(dat$fid, c(2:5))] = "BSR_other"  ## Blackspotted non-trawl
		dat$fishery[is.element(dat$major,3:6) & is.element(dat$fid, c(1,9))] = "RER_trawl"  ## Rougheye trawl
		dat$fishery[is.element(dat$major,3:6) & is.element(dat$fid, c(2:5))] = "RER_other"  ## Rougheye non-trawl
		dat$fishery[is.element(dat$major,7)   & is.element(dat$fid, c(1,9))] = "HYB_trawl"  ## Hybrids trawl
		dat$fishery[is.element(dat$major,7)   & is.element(dat$fid, c(2:5))] = "HYB_other"  ## Hybrids non-trawl
	}
	flds = colnames(dat)

	if (all(is.element(c("year","fid","catKg"), flds))){
		tab.fid = crossTab(dat, c("year","fid"), "catKg")
		fid.nam = c("Trawl","Halibut","Sablefish","Dogfish/Lingcod","H&L Rockfish","GF Longline","Foreign"); names(fid.nam) = as.character(c(1:5,8:9))
		colnames(tab.fid) = fid.nam[colnames(tab.fid)]
		write.csv(tab.fid, file=paste0("cat",strSpp,".fid.sum.csv"))
		ttput(tab.fid)
	}
	if (all(is.element(c("year","fishery","catKg"), flds))){
		tab.fishery = crossTab(dat, c("year","fishery"), "catKg")
		write.csv(tab.fishery, file=paste0("cat",strSpp,".fishery.sum.csv"))
		ttput(tab.fishery)
	}
	if (all(is.element(c("year","gear","catKg"), flds))){
		tab.gear = crossTab(dat, c("year","gear"), "catKg")
		gear.nam = c("Unknown","Bottom Trawl","Trap","Midwater Trawl","Hook and Line","Longline"); names(gear.nam) = as.character(0:5)
		colnames(tab.gear) = gear.nam[colnames(tab.gear)]
#browser();return()
		write.csv(tab.gear, file=paste0("cat",strSpp,".gear.sum.csv"))
		ttput(tab.gear)
	}
	if (all(is.element(c("year","db","catKg"), flds))){
		tab.db = crossTab(dat, c("year","db"), "catKg")
		write.csv(tab.db, file=paste0("cat",strSpp,".db.sum.csv"))
		ttput(tab.db)
	}
	if (all(is.element(c("year","sector","catKg"), flds))){
		tab.sector = crossTab(dat, c("year","sector"), "catKg")
		write.csv(tab.sector, file=paste0("cat",strSpp,".sector.sum.csv"))
		ttput(tab.sector)
	}

	mess = paste0("cat",strSpp,"=dat; save(\"cat", strSpp, "\", ", "file=\"cat", strSpp, ".rda\")")
	eval(parse(text=mess))
	invisible(return(dat))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~quickCat


## surveyCatch--------------------------2024-03-19
## Query GFBioSQL for survey catch and summarise
## catches by year and PMFC area.
## ---------------------------------------------RH
surveyCatch = function(strSpp="396", spath=.getSpath(), gfbdat=NULL, sql=FALSE,
   hadley=FALSE, catDir=getwd(), tabDir="./tables", datDir="./data")
{
	bigdate = Sys.Date(); numdate = substring(gsub("-","",bigdate),3)
	## Make sure paths are properly speciffied
	for (i in c("catDir","tabDir","datDir")) {
		ii = get(i)
		if (!grepl("^\\./|^[A-Z]:", ii)) {
			iii = paste0("./",ii)
			assign(i, iii)
		}
	}
	if (is.null(gfbdat)) {
		sql = TRUE
		if (isThere("PBSdat")) rm(PBSdat) ## remove from current environment
		## GFBioSQL catch summary for surveys
		getData("gfb_rcatORF.sql", dbName="GFBioSQL", strSpp=strSpp, path=spath, tenv=penv())
		#assign("gfbdat", PBSdat)
		zgfb = do.call('order', PBSdat)  ## Try ordering (RH: 240216): calls to SQL seem to return same records in different order.
		gfbdat = PBSdat[zgfb,]
		save("gfbdat", file=paste0(catDir,"/gfbdat.rda"))
		#if (file.exists(tabDir) && tabDir!=".")
		if (file.exists(tabDir) && tabDir!=".")
			write.csv(gfbdat, paste0(tabDir,"/Survey-Records-",strSpp,"-",numdate,".csv"),row.names=FALSE)
		rm(PBSdat) ## just to be safe
	} else if (file.exists(paste0(catDir,"/gfbdat.rda"))) {
		load(paste0(catDir,"/gfbdat.rda"))
	} else {
		message("Cannot find 'gfbdat'")
		browser(); return()
	}

	gfbtab = crossTab(gfbdat, c("year","major"), "catKg", hadley=hadley) # summarise catch (t)
	if (hadley){
		gfbcat = as.data.frame(gfbtab[,-1])
		row.names(gfbcat) = gfbtab[,1]
	} else {
		gfbcat = as.data.frame(gfbtab)
	}
	data(pmfc,package="PBSdata",envir=penv())
	names(gfbcat) = pmfc[names(gfbcat),"gmu"]
	if (file.exists(datDir))
		save("gfbcat", file=paste0(datDir,"/gfbcat.rda"))
	if (file.exists(tabDir) && tabDir!=".")
		write.csv(gfbcat, file=paste0(tabDir,"/Catch-Survey-",strSpp,"-(Year-PMFC)-",numdate,".csv"))
#browser();return()

	## The rest is just here for table output (not critical)
	if (sql && file.exists(tabDir) && tabDir!=".") { ## (RH 240319)
		gfbdat$SVID[is.na(gfbdat$SVID)] = 999
		gfbdat$SSID[is.na(gfbdat$SSID)] = 999
	
		spp.svid = crossTab(gfbdat, c("SVID","year"), "catKg", hadley=hadley)
		getData("SURVEY","GFBioSQL")
		survey = PBSdat
		svid = survey[[2]]; names(svid) = survey[[1]]
		svid = c(svid,`999`="No Survey Identified")
		if (hadley) {
			SVID = spp.svid$SVID
			svidcat = spp.svid[,-1]; attr(svidcat,"class") = "data.frame"
		} else {
			SVID =  rownames(spp.svid)
			svidcat = as.data.frame(spp.svid)
		}
		dimnames(svidcat)[[1]] = paste0("SVID ",SVID,": ",svid[as.character(SVID)])
		if (file.exists(tabDir) && tabDir!=".")
			write.csv(svidcat, file=paste0(tabDir,"/Catch-Survey-",strSpp,"-(SVID-Year)-",numdate,".csv"))
	
		spp.ssid = crossTab(gfbdat, c("SSID","year"), "catKg", hadley=hadley)
		getData("SURVEY_SERIES","GFBioSQL")
		series = PBSdat
		ssid = series[[2]]; names(ssid) = series[[1]]
		ssid = c(ssid,`999`="No Survey Series Identified")
		if (hadley) {
			SSID = spp.ssid$SSID
			ssidcat = spp.ssid[,-1]; attr(ssidcat,"class") = "data.frame"
		} else {
			SSID = rownames(spp.ssid)
			ssidcat = as.data.frame(spp.ssid)
		}
		dimnames(ssidcat)[[1]] = paste0("SSID ",SSID,": ",ssid[as.character(SSID)])
		if (file.exists(tabDir) && tabDir!=".")
			write.csv(ssidcat, file=paste0(tabDir,"/Catch-Survey-",strSpp,"-(SSID-Year)-",numdate,".csv"))
	} ## end if table output
	invisible(gfbcat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~surveyCatch


## zapSeamounts-------------------------2021-12-09
## Remove seamount records use combinations of 
## major, minor, and locality codes.
## ---------------------------------------------RH
zapSeamounts = function(dat, only.mark=FALSE) {
	seamounts = t(data.frame(
		heck=c(3,24,11), eickelberg=c(3,24,12), union=c(4,27,5), dellwood=c(5,11,8), bowie=c(9,31,10),
		pratt=c(10,33,6), surveyor=c(10,33,7), durgin=c(10,33,8), murray=c(10,40,4), cowie=c(10,40,5),
		pathfinder=c(11,42,1), morton=c(11,42,2), miller=c(11,42,3), cobb=c(67,67,1), brownbear=c(67,67,2), 
		row.names=c("major","minor","locality"), stringsAsFactors=FALSE))
	fnam = as.character(substitute(dat))
	ii = fnam; idat = dat
	if (!is.element("year",colnames(idat))){
		if (!is.element("date",colnames(idat)))
			idat$year = rep(as.numeric(substring(Sys.time(),1,4)), nrow(idat))
		else
			idat$year = as.numeric(substring(idat$date,1,4))
	}
	yrs = .su(idat$year); nyrs = length(yrs)
	SMrem = array(0, dim=c(nyrs,nrow(seamounts),2), dimnames=list(year=yrs, seamount=rownames(seamounts), value=c("nrec","tcat")))
	nSMrec = tSMcat = list()
	if (!all(c("major","minor","locality") %in% colnames(idat))) { 
		nSMrec[ii] = NA; tSMcat[ii] = NA
	} else {
		.flush.cat(as.character(substitute(dat)), ": ")
		if (only.mark)
			idat$seamount = rep("",nrow(idat))
		for (j in 1:nrow(seamounts)) {
			jj  = seamounts[j,]
			jjj = rownames(seamounts)[j]
			.flush.cat(jjj,ifelse(j<nrow(seamounts),",",""), sep="")
			seamo = is.element(idat$major,jj[1]) & is.element(idat$minor,jj[2]) & is.element(idat$locality,jj[3])
			if (sum(seamo)==0) next
			if (only.mark) {
				idat$seamount[seamo] = jjj
			} else {
				ij = paste0(jjj, "(", paste0(jj,collapse="."), ")")
				nrec = table(idat$year[seamo])
				SMrem[names(nrec),jjj,"nrec"] = nrec
				catflds = names(idat)[is.element(names(idat),c("landed","discard","catKg"))]
				if (length(catflds)>0){
					tcat  = sapply(split(apply(idat[seamo,catflds,drop=FALSE],1,sum),idat$year[seamo]),sum)/1000.
					SMrem[names(tcat),jjj,"tcat"] = tcat
				}
				idat = idat[!seamo,]
			}
		}
		.flush.cat("\n")
	}
	if (only.mark) {
		if(all(idat$seamount==""))
			idat = idat[,setdiff(colnames(idat),"seamount")]
		return(idat)
	}
	keep = apply(SMrem[,,"nrec",drop=FALSE],1,sum)>0 ## only keep the years when seamount catches occurred
	attr(idat,"seamounts") = seamounts
	attr(idat,"SMrem") = if(sum(keep)>0) SMrem[keep,,,drop=FALSE] else NULL
	return(idat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~zapSeamounts

