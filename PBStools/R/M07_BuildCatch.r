#===============================================================================
# Module 7: Catch Reconstruction
# ------------------------------
#  buildCatch...Build a catch history of BC rockfish 1918--present.
#  plotData.....Plot diagnostic data for catch reconstructions.
#  plotRecon....Plot reconstructed catch using barplots stacked by PMFC area.
#  surveyCatch..Query the survey catch data and make summary tables.
#  zapSeamounts.Remove seamount records using combinations of major, minor, and locality codes.
#===============================================================================

#buildCatch-----------------------------2017-02-20
# Catch reconstruction algorithm for BC rockfish.
# Use ratios of RRF (reconstructed rockfish) to ORF 
# (rockfish other than POP) landings for multiple fisheries.
# Matrix indices: i=year, j=major, k=fid, l='spp'
# Arguments definitions appear below:
#-----------------------------------------------RH
buildCatch=function(
   dbdat,                # List object of landing records from eight DFO databases
   strSpp="396",         # Hart species code for the rockfish to be reconstructed (RRF)
   orfSpp="TRF",         # Field name of the denominator in the ratio of RRF to other rockfish (usually ORF but can be TRF or POP if these are more appropriate)
   major=c(1,3:9),       # Major PMFC area codes to see in plots; catch is always reconstructed for majors c(1,3:9)
   fidout=c(1:5,10),     # Fishery IDs for which an annual series barplot stacked by PMFC area is produced
   refyrs=1997:2005,     # Reference years to use for calculating gamma (e.g., RRF/ORF).
   refarea=NULL,         # Name of file containing reference areas to use when calculating gamma
   refgear=NULL,         # Reference gear types years to use for calculating gamma
   useYR1=NULL,          # First year to start using reported landings (i.e. not estimated from gamma), one for each fishery
   useBG=FALSE,          # Sample from the binomial-gamma to estimate ratios RRF/ORF or RRF/TRF.
   useSM=FALSE,          # Use catch data from seamounts
   useGFM=TRUE,          # Use the latest official GF_MERGED_CATCH table (compiled by Norm and Kate)
   useLS=TRUE,           # Use ORF catch from Langara Spit in gamma calculation
   useAI=FALSE,          # Use Anthony Island catch as 5C catch (chiefly for POP and maybe YMR)
   useCA=TRUE,           # Use ORF catch from the CA fleet
   useUS=TRUE,           # Use ORF catch from the US fleet
   useFF=TRUE,           # Use ORF catch from the foreign (UR, JP, PO, etc) fleet
   strat.gamma=FALSE,    # Stratify the RRF numerator and ORF denominator by depth zone and weight by frequency of RRF landed > 0
   strat.delta=FALSE,    # Stratify the discard numerator and denominator by depth zone and weight by frequency of RRF discards > 0
   depbin=100,           # Depth (m) bin to use for strat.gamma and strat.delta
   sensitivity=NULL,     # Sensitivity name for tweaking decisions
   reconstruct=TRUE,     # Complete the reconstruction to its end, otherwise terminate the code once the modern catch array has been compiled and saved
   diagnostics=FALSE,    # Create automatically-numbered diagnostic images files to a subdirectory called `CRdiag'
   ascii.tables=TRUE,    # Create ASCII ouput tables and dump them into the subdirectory called `tables'
   saveinfo=TRUE,        # Save various data and function objects to a list object called `PBStool' in the temporary environment `.PBStoolEnv'
   sql=FALSE,            # Query the databases, otherwise load catch records from binary files saved from a previous query run (to save time)
   only.sql=FALSE,       # Only execute the queries to download catch data from remote databases
   sql.force=FALSE,      # Force the SQL query even if the data file (.rda) exists
   spath=.getSpath(),    # Path to SQL code files -- defaults to the `sql' directory under `system.file(package="PBStools")'
   dpath=getwd(),        # Database path for times when user wants to build alternative catch histories (in another directory) using data already queried
   eps=FALSE, png=FALSE, wmf=FALSE, # Send the figures to `.eps' , `.png', and `.wmf' files, respectively
   uid=Sys.info()["user"], pwd=uid, # User ID and password for Oracle DB account authentication (only used for PacHarv3 currently)
   ioenv=.GlobalEnv,     # Input/output environment for function input data and output results
   ...)                  # Additional ad hoc arguments to deal with PJS issues
{
	data(pmfc,species,envir=penv())
	bigdate = Sys.Date(); numdate = substring(gsub("-","",bigdate),3)
	.flush.cat(paste0("-----Start reconstructing catch for ",species[strSpp,"latin"],"-----\n\n"))
	fidnam  = c("trawl","halibut","sablefish","sched2","zn","sabzn","sabhal","dogfish","lingcod","combined")
	fshnam  = c("trawl","h&l","trap",rep("h&l",6),"combined")  ## general category vector
	fidfive = c("Trawl","Halibut","Sablefsh","DogLing","HLRock")

#browser();return()
	if (ascii.tables){
		if (!file.exists("./tables")) dir.create("tables")
	}
	if (!only.sql) {
		## -------------------------------------------------------------
		## Global list object 'PBStool' stores results from the analysis
		## -------------------------------------------------------------
		assign("PBStool",list(module="M07_BuildCatch",call=match.call(),args=args(buildCatch),
			spp=strSpp,pD=1,eps=eps,wmf=wmf,fidnam=fidnam,fshnam=fshnam),envir=.PBStoolEnv)
		## ------------------------------------------
		## pD = Counter for plotData diagnostics
		## Function to convert numbers to proportions
		## ------------------------------------------
		pcalc=function(x){if (all(x==0)) rep(0,length(x)) else x/sum(x)}
		sysyr=as.numeric(substring(Sys.time(),1,4)) ## maximum possible year

		if (diagnostics){
			if (!file.exists("./CRdiag")) dir.create("CRdiag")
			else {
				diag.files = list.files("./CRdiag")
				if (length(diag.files)>0) 
					junk = file.remove(paste("./CRdiag/",diag.files,sep=""))
			}
		}
		clrs.fishery=c("red","blue","orange","green","yellow"); names(clrs.fishery)=1:5

		## ====================================================
		## 1. Compile historical catches for RRF, POP, ORF, TRF
		## ====================================================
		.flush.cat(paste0("Compiling historical catches for ",strSpp,", POP, ORF, TRF\n"))

		for (h in c("rrf","orf")) {
			## ------------------------------------------------------------------------------------------------------------
			## Use following line for times when objects are not registered in PBSdata (i.e. package has not beeen rebuilt)
			#  expr = paste0("getFile(",h,"history,use.pkg=TRUE,tenv=penv()); dat=",h,"history")
			## ------------------------------------------------------------------------------------------------------------
			expr = paste0("getFile(",h,"history,path=paste0(system.file(package=\"PBSdata\"),\"/data\"),tenv=penv()); hdat=",h,"history")
			eval(parse(text=expr))

#if(h=="rrf") hdat$spp[is.element(hdat$spp,"418")] = "396" ## for testing only

			#getFile(orfhistory,use.pkg=TRUE,tenv=penv())
			#dat    = rrfhistory ## catch in kg
			if (h=="orf") {
				if (!useFF) hdat = hdat[!is.element(hdat$source,c("Ketchen80","Leaman80")),] # don't use foreign ORF in reconstruction
				#if (!useLS) hdat = hdat[!(is.element(hdat$major,9) & is.element(hdat$minor,35)),] # don't use Langara Spit catch BUT no minor area info in early series
			}
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
			if (h=="rrf" && is.element("614",spp)){ 
				## ------------------------------------------------------------
				## collect the IPHC halibut data for discard calculations later
				## ------------------------------------------------------------
				iphcdat = hdat[is.element(hdat$spp,"614") & is.element(hdat$fishery,"halibut"),]
				cat614  = sapply(split(iphcdat$catch,iphcdat$year),sum,na.rm=TRUE)/1000.
			}
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
#browser();return()
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
									htab[names(POP),jj,ii,nn,a,b,"POP"]=POP }
								ORF= sapply(split(jdat$catch[!z],jdat$year[!z]),sum,na.rm=TRUE)/1000
								TRF= sapply(split(jdat$catch,jdat$year),sum,na.rm=TRUE)/1000
								htab[names(ORF),jj,ii,nn,a,b,"ORF"]=ORF
								htab[names(TRF),jj,ii,nn,a,b,"TRF"]=TRF
							}
#if (h=="rrf") {browser();return()}
			} } } } } ## close loops nn,j,i,b,a

			if (h=="rrf") {
				## ------------------------------------------------------
				## RRF historical catch not always recorded in modern DBs
				## ------------------------------------------------------
#browser();return()
				rrfbase = htab[,,,,,,strSpp,drop=FALSE]
				rrfmax  = rrfadd = array(0,dim= dim(rrfbase)[c(1,2,6,4)],dimnames=dimnames(rrfbase)[c("year","major","fishery","nation")])
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
						}
					}
				}
## ***** need to revise where these appear later in the code
				if (saveinfo)
					packList(c("rrfbase","rrfadd","rrfmax"),"PBStool",tenv=.PBStoolEnv)
			}
		}

		## -------------------------------------------------------------------------------------------------
		## For the years 1918 to 1949, no records exist to delineate POP, ORF, and TRF.
		## In essence TRF = ORF for Dominion Bureau Stats and Stewart's US catch of BC rockfish
		## Therefore, use empirical data from 1951 to 1995 to estimate ORF from TRF (the difference is POP).
		## -------------------------------------------------------------------------------------------------
		sARF = apply(htab,c("year","catch","nation"),sum)                       ## sum of all rockfish
		for (nn in nat) {
			nsARF = sARF[,,nn]
			zUNK  = nsARF[,"POP"]==0 & nsARF[,"TRF"]>0
			if (!any(zUNK)) next
			zOBS  = nsARF[,"TRF"]>0 & nsARF[,"ORF"]>0 & nsARF[,"POP"]>0
			oTRF  = nsARF[zOBS,"TRF"]
			oORF  = nsARF[zOBS,"ORF"]
#if (nn=="US") {browser();return()}
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
					plotData(apply(htab[,mm,,,,fish,spp],c(1,3),sum),paste("htab ",spp," in ",fish,sep="")) }
				plotData(apply(htab[,mm,,,,,spp],c(1,3),sum),paste("htab ",spp," in all fisheries",sep=""))
			}
		}

		## -----------------------------------------------------------------------
		## Sabotage htab to remove PacHarv3 for trap and trawl between 1954 & 1995
		## (GFCatch provides best estimates, see Rutherford 1999)
		## -----------------------------------------------------------------------
		htab[as.character(1954:1995),,"PacHarv3","CA","max",c("trap","trawl"),] = 0
		if (diagnostics)
			plotData(apply(htab[,mm,,,,"trawl","POP"],c(1,3),sum),paste("htab sabotaged POP in trawl",sep=""))

		## -------------------------------------------------------------------------------
		## historical maximum catch (POP,ORF,TRF) by year, major, gear (i.e. comparative):
		## -------------------------------------------------------------------------------
		htabmax = htab[,,,,"max",,]
#browser();return()
		orfmax = apply(htabmax,c(1:2,4:6),max,na.rm=TRUE) ## collapse 'source' (dim 3); ## now contains nation dimension
		## ---------------------------------------------------------------------------
		## historical unique catch (POP,ORF,TRF) by year, major, gear (i.e. additive):
		## ---------------------------------------------------------------------------
		htabadd = htab[,,,,"add",,]
		orfadd = apply(htabadd,c(1:2,4:6),sum,na.rm=TRUE) ## collapse 'source' (dim 3); ## now contains nation dimension

		MAJ = dimnames(orfmax)$major
		clrs.major=rep("gainsboro",length(MAJ)); names(clrs.major)=MAJ
		clrs.major[as.character(c(1,3:9))]=c("moccasin","blue","lightblue","yellow","orange","red","seagreen","lightgreen")

		if (saveinfo)
			packList(c("htab","htabmax","htabadd","orfmax","orfadd"),"PBStool",tenv=.PBStoolEnv)
		## Historical used again on line 335
		.flush.cat("-----Finished collecting historical landings from `orfhistory'-----\n\n")

		## =====================================================
		## 2. Gather the modern RRF catches (landed & discarded)
		## =====================================================
#browser();return()
		.flush.cat("Start collecting modern landings:\n")

		if (missing(dbdat) && sql==FALSE) {
			mess=paste0("Either provide a list object 'dbdat' or set 'sql=TRUE'.\n\n",
				"Ideally, 'dbdat' should contain data frames:\n", 
				"'ph3cat' = PacHarv3 database (all fisheries)\n")
			if (useGFM)
				mess = paste0(mess,"'gfmdat' = GFFOS merged catch table GF_MERGED_CATCh\n")
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
			mess = paste0(mess,"with fields:\nc( 'fid', 'date', 'major', 'landed', 'discard', 'POP', 'ORF', 'TAR' )")
			showError(mess,as.is=TRUE,x=0.05,adj=0,cex=1.2)
		}

		lenv=sys.frame(sys.nframe()) ## local environment
		cflds = c("landed","discard","POP","ORF","TAR")
		keep  = c("fid","date","major","minor",cflds)
		ufos  = c("POP","PAH","SBF","DOG","RFA","RFA","PAH","DOG","LIN")
	} ## end skip if only.sql ------------------------------------------

#browser();return()


	if (sql) {
		.flush.cat("Start querying the DFO databases:\n")
		blob = list()
		if (isThere("PBSdat")) rm(PBSdat) ## remove from current environment
		uid=rep(uid,2)[1:2]; pwd=rep(pwd,2)[1:2]
		## -----------------------------------------------------------------------------
		## Start querying the databases
		## PacHarv3 catch summary for fids 1:5 and 0 (unknown)
		## Note: only used for h&l fisheries (2,4,5) as GFCATCH contains trawl and trap.
		## -----------------------------------------------------------------------------

		if (file.exists(paste0(dpath,"/ph3dat.rda")) && !sql.force)
			load(paste0(dpath,"/ph3dat.rda"))
		else {
			.flush.cat("   Oracle -- PacHarv3 (table CATCH_SUMMARY);\n")
			getData("ph3_fcatORF.sql",dbName="HARVEST_V2_0",strSpp=strSpp,path=spath,
				server="ORAPROD",type="ORA",trusted=FALSE,uid=uid[1],pwd=pwd[1],tenv=penv())
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
			if (file.exists(paste0(dpath,"/gfmdat.rda")) && !sql.force)
				load(paste0(dpath,"/gfmdat.rda"))
			else {
				.flush.cat("   SQL Server -- GFFOS (table GF_MERGED_CATCH) [takes a few minutes];\n")
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
			if (file.exists(paste0(dpath,"/gfcdat.rda")) && !sql.force)
				load(paste0(dpath,"/gfcdat.rda"))
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
			if (file.exists(paste0(dpath,"/phtdat.rda")) && !sql.force)
				load(paste0(dpath,"/phtdat.rda"))
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
			if (file.exists(paste0(dpath,"/phhdat.rda")) && !sql.force)
				load(paste0(dpath,"/phhdat.rda"))
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
			if (file.exists(paste0(dpath,"/phsdat.rda")) && !sql.force)
				load(paste0(dpath,"/phsdat.rda"))
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
			if (file.exists(paste0(dpath,"/phvdat.rda")) && !sql.force)
				load(paste0(dpath,"/phvdat.rda"))
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
			if (file.exists(paste0(dpath,"/phfdat.rda")) && !sql.force)
				load(paste0(dpath,"/phfdat.rda"))
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
		if (file.exists(paste0(dpath,"/gfbdat.rda")) && file.exists(paste0(dpath,"/gfbcat.rda")) && !sql.force) {
			load(paste0(dpath,"/gfbdat.rda"))
			load(paste0(dpath,"/gfbcat.rda"))
		} else {
			.flush.cat("   SQL Server -- GFBioSQL (surveys).\n")
			gfbcat = surveyCatch(strSpp=strSpp)
			getFile(gfbdat)
		}
		blob[["gfbdat"]] = gfbdat

		## ------------------------------------------------------------
		## Wrap up the fisheries and survey landings into a list object
		## ------------------------------------------------------------
		eval(parse(text=paste("dbdat=\"cat",strSpp,"dat\"",sep="")))
		expr=paste(dbdat,"=blob; save(\"",dbdat,"\",file=\"",dbdat,".rda\")",sep="")
		eval(parse(text=expr))
		##-----Stop querying the databases-----
	}
	else {
		dbdat=as.character(substitute(dbdat)) ## database list object name
		expr=paste("getFile(",dbdat,",senv=ioenv,try.all.frames=TRUE,tenv=penv(),path=dpath); fnam=names(",dbdat,"); unpackList(",dbdat,")",sep="")
		eval(parse(text=expr)) 
	}
	if (only.sql) return()
	
	## -------------------------------
	## Remove seamounts if useSM=FALSE
	## -------------------------------
	if (!useSM){
		.flush.cat("Removing seamount records ...\n")
		nSMrec=rep(0,length(fnam)); names(nSMrec)=fnam; tSMcat=nSMrec
		for (i in fnam){
			eval(parse(text=paste0("idat = zapSeamounts(",i,")")))
			nSMrec[i] = attributes(idat)$nSMrec
			tSMcat[i] = attributes(idat)$tSMcat
#browser();return()
			assign(i,idat)
		}
		packList(c("nSMrec","tSMcat"),"PBStool",tenv=.PBStoolEnv)
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
		packList(c("nSMrec","tSMcat"),"PBStool",tenv=.PBStoolEnv)
	}
	## ---------------------------------------------
	## Include Anthony Island catch in PMFC 5C catch
	## ---------------------------------------------
	if (useAI && any(strSpp==c("396","440"))){
		.flush.cat("Changing Anthony Is. catch (PMFC 5E south of 52.3333) to PMFC 5C catch...\n")
		if ("gfmdat"%in%fnam) {
			i = "gfmdat"
			eval(parse(text=paste0("idat = ",i)))
			ai1 = is.element(idat$major,9) & is.element(idat$stock,"5ABC")
			ai2 = is.element(idat$major,9) & is.element(idat$minor,34) & is.element(idat$locality,c(1,5))
#browser();return()
			idat$major[ai1 | ai2] = 7 ## 5C
			assign(i,idat)
		}
	}
	if (!sql | !useSM){
		if (file.exists("gfbcat.rda"))
			load("gfbcat.rda")
		else
			gfbcat = surveyCatch(strSpp=strSpp, gfbdat=gfbdat)
	}

	.flush.cat("Start consolidating landings records:\n")

	## -----------------------------------------
	## Consolidate PacHarv3 records (fid=c(1:5))
	## -----------------------------------------
	.flush.cat("   PacHarv3 records ...\n")
	ph3cat = as.data.frame(t(apply(ph3dat,1,function(x){
		ufos=c("POP","PAH","SBF","DOG","RFA"); ufid=1:5; names(ufid)=ufos
		f = x["fid"]
		if (f==0) { 
			z = x[ufos]==max(x[ufos],na.rm=TRUE)
			utar = ufos[z][1]
			fid = ufid[utar]
			ucat = x[utar]
		}
		else {
			fid=f
			ucat=x[ufos[f]]
		}
		out = c(x["year"],fid,date=as.Date(paste(x["year"],"-07-01",sep="")),
			x[c("major","minor","landed","discard","POP","ORF")],ucat)
		return(out) } )))
	names(ph3cat) = c("year",keep)
	ph3cat$date = as.Date(paste(ph3cat$year,"-07-01",sep=""))
	ph3cat = ph3cat[,-1] ## get rid of 'year'
	save("ph3cat",file="ph3cat.rda")

	if (useGFM){
		.flush.cat("   GFFOS merged catch ...\n")
		dbs = c("ph3cat","gfmcat")
		## ------------------------------------------------------
		## Databases to compare and merge (using maximum)
		## drop PacHarv3 for Trawl and Trap (see Rutherford 1999)
		## ------------------------------------------------------
		dbmerge = list(
			trawl     = c("gfmcat"),
			halibut   = c("ph3cat","gfmcat"),
			sablefish = c("gfmcat"),
			doglingi  = c("ph3cat","gfmcat"),
			hlrocks   = c("ph3cat","gfmcat"))
		## ----------------------------------------------------------------------------------------
		## Databases that are strictly additive (e.g., J-V Hake, but already in merged catch table)
		## ----------------------------------------------------------------------------------------
		dbadd = list (trawl=NULL, halibut=NULL, sablefish=NULL, dogling=NULL, hlrocks=NULL)

		gfmcat = gfmdat[,keep]
		trash  = apply(gfmcat[,cflds],1,function(x){all(x==0)})
		gfmcat = gfmcat[!trash,]; dimnames(gfmcat)[[1]]=1:nrow(gfmcat)
		save("gfmcat",file="gfmcat.rda")
#browser();return()
	}
	else {
		dbs = c("ph3cat","gfccat","phtcat","phhcat","phscat","phvcat","phfcat","foscat","jvhdat")
		## ------------------------------------------------------
		## Databases to compare and merge (using maximum)
		## drop PacHarv3 for Trawl and Trap (see Rutherford 1999)
		## ------------------------------------------------------
		dbmerge = list(
			trawl     = c("gfccat","phtcat","foscat"),
			halibut   = c("ph3cat","phhcat","phvcat","foscat"),
			sablefish = c("gfccat","phscat","foscat"),
			doglingi  = c("ph3cat","phvcat","phfcat","foscat"),
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
		save("gfccat",file="gfccat.rda")

		## ---------------------------------------
		## Consolidate PacHarvest landings (fid=1)
		## ---------------------------------------
		.flush.cat("   PacHarvest catch ...\n")
		phtcat = phtdat[,keep]
		trash  = apply(phtcat[,cflds],1,function(x){all(x==0)})
		phtcat = phtcat[!trash,] ; dimnames(phtcat)[[1]] = 1:nrow(phtcat)
		save("phtcat",file="phtcat.rda")

		## --------------------------------------------
		## Consolidate GFBioSQL JV Hake bycatch (fid=1)
		## --------------------------------------------
		.flush.cat("   JV Hake bycatch ...\n")
		jvhcat = jvhdat[,keep]
		trash  = apply(jvhcat[,cflds],1,function(x){all(x==0)})
		jvhcat = jvhcat[!trash,] ; dimnames(jvhcat)[[1]] = 1:nrow(jvhcat)
		save("jvhcat",file="jvhcat.rda")

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
		save("phhcat",file="phhcat.rda")

		## -----------------------------------------
		## Consolidate PacHarvSable landings (fid=3)
		## -----------------------------------------
		.flush.cat("   PacHarvSable catch ...\n")
		phscat = phsdat[,keep]
		trash  = apply(phscat[,cflds],1,function(x){all(x==0)})
		phscat = phscat[!trash,] ; dimnames(phscat)[[1]] = 1:nrow(phscat)
		save("phscat",file="phscat.rda")

		## --------------------------------------------------------
		## Consolidate PacHarvHL validation landings (fid=c(2,4,5))
		## --------------------------------------------------------
		.flush.cat("   PacHarvHL validation landings ...\n")
		phvcat = phvdat
		phvcat$TAR = rep(0,nrow(phvcat))
		for (i in 1:9) {
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
		save("phvcat",file="phvcat.rda")

		## ----------------------------------------------------
		## Consolidate PacHarvHL fisherlog records (fid=c(4,5))
		## ----------------------------------------------------
		.flush.cat("   PacHarvHL fisherlog catch ...\n")
		phfcat = phfdat[,keep]
		trash=apply(phfcat[,cflds],1,function(x){all(x==0)})
		phfcat=phfcat[!trash,]; dimnames(phfcat)[[1]]=1:nrow(phfcat)
		save("phfcat",file="phfcat.rda")

		## -----------------------------------
		## Consolidate GFFOS records (fid=1:5)
		## -----------------------------------
		.flush.cat("   GFFOS official catch ...\n")
		z = fosdat$date >= as.POSIXct("2000-01-01") & fosdat$date <= Sys.time()  ## up to the current date
		#z = fosdat$date >= as.POSIXct("2000-01-01") & fosdat$date <= as.POSIXct("2010-06-30") ## for POP assessment
		foscat = fosdat[z,keep]
		trash=apply(foscat[,cflds],1,function(x){all(x==0)})
		foscat=foscat[!trash,]; dimnames(foscat)[[1]]=1:nrow(foscat)
		save("foscat",file="foscat.rda")
	}

	modyrs = majmod = fid = NULL
	for (i in dbs) { 
		if(!isThere(i,envir=lenv)) next
		icat = get(i)
		modyrs = c(modyrs,unique(as.numeric(substring(icat$date,1,4))))
		majmod = c(majmod,unique(icat$major))
		fid=c(fid,unique(icat$fid)) }
	modyrs = sort(unique(modyrs)); majmod=sort(unique(majmod)); fid=sort(unique(fid))
	modyrs = modyrs[is.element(modyrs,1945:sysyr)]

	if (isThere("refyrs") && !is.null(refyrs) && length(intersect(refyrs,modyrs))==0)
		showError("refyrs","nodata") 
	MODYRS = modyrs[1]:modyrs[length(modyrs)]

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
#browser();return()

	## -------------------------------------------------------------
	## Collect repcat landings (t), including those in unknown areas
	## -------------------------------------------------------------
	.flush.cat("Collecting repcat landings, including those in unknown areas (catmod0) ...\n")
	catmod0=array(0,dim=c(length(MODYRS),length(majmod),length(fid),length(Cflds),length(dbs)),
		dimnames=list(year=MODYRS,major=majmod,fid=fid,catch=Cflds,dbs=dbs))
	for (a in dbs) {
		if(!isThere(a,envir=lenv)) next
		acat=get(a)
		acat$year = as.numeric(substring(acat$date,1,4))
		acat = acat[is.element(acat$year,MODYRS),] # to remove anomalous years like 1900
		acat$TRF  = acat[["POP"]] + acat[["ORF"]]  ## total rockfish
		if (is.null(acat$discard)) acat$discard = rep(0,nrow(acat))
		for (k in fid) {
			kk=as.character(k)
			kdat=acat[is.element(acat$fid,k),]
			if(nrow(kdat)==0) next
			for (j in majmod) {
				jj=as.character(j)
				jdat=kdat[is.element(kdat$major,j),]
				if(nrow(jdat)==0) next
				landed=sapply(split(jdat$landed,jdat$year),sum,na.rm=TRUE)/1000.
				POP=  sapply(split(jdat$POP,jdat$year),sum,na.rm=TRUE)/1000.
				ORF=  sapply(split(jdat$ORF,jdat$year),sum,na.rm=TRUE)/1000.
				TRF=  sapply(split(jdat$TRF,jdat$year),sum,na.rm=TRUE)/1000.
				TAR=  sapply(split(jdat$TAR,jdat$year),sum,na.rm=TRUE)/1000.
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
	} } } ## close loops j & k & i
#browser(); return()

	if (diagnostics){
		for (spp in dimnames(catmod0)$catch) {
			for (db in dimnames(catmod0)$dbs) {
				plotData(apply(catmod0[,mm,,spp,db],c(1,3),sum),paste("catmod0 ",spp," in ",db,sep=""),col=clrs.fishery) }
			plotData(apply(catmod0[,mm,,spp,],c(1,3),sum),paste("catmod0 ",spp," in all databases",sep=""),col=clrs.fishery)
	}	}

	## ----------------------------------------------------------------------------
	## Allocate catch (t) from unknown major (code=0) to user-specified majors (mm)
	## ----------------------------------------------------------------------------
	.flush.cat("Allocating catch from unknown major (code=0) to user-specified majors (catmod1) ...\n")
	catmod1=catmod0[,is.element(dimnames(catmod0)[[2]],mm),,,,drop=FALSE]
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
					pmaj   = apply(apply(cattab[catyrs,,drop=FALSE],1,pcalc),1,mean)
					allo   = t(t(catmod0[,"0",kk,ll,aa]))%*%pmaj; dimnames(allo)[[2]] = mm
					catmod1[,mm,kk,ll,aa] = catmod1[,mm,kk,ll,aa] + allo 
	} } } }

	if (diagnostics){
		for (spp in dimnames(catmod1)$catch) {
			for (db in dimnames(catmod1)$dbs) {
				plotData(apply(catmod1[,mm,,spp,db],c(1,3),sum),paste("catmod1 ",spp," in ",db,sep=""),col=clrs.fishery) }
			plotData(apply(catmod1[,mm,,spp,],c(1,3),sum),paste("catmod1 ",spp," in all databases",sep=""),col=clrs.fishery)
	}	}

	## -------------------------------------------------------------------
	## Allocate catch (t) from unknown fid (code=0) to standard fids (1:5)
	## -------------------------------------------------------------------
	.flush.cat("Allocating catch from unknown fid (code=0) to standard fids (catmod2) ...\n")
	catmod2=catmod1[,,is.element(dimnames(catmod1)[[3]],1:5),,,drop=FALSE]
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
					pfid   = apply(apply(cattab[catyrs,,drop=FALSE],1,pcalc),1,mean)
					allo   = t(t(catmod1[,jj,"0",ll,aa]))%*%pfid; dimnames(allo)[[2]] = kk
					catmod2[,jj,kk,ll,aa] = catmod2[,jj,kk,ll,aa] + allo 
	} } } }
	fid = as.numeric(dimnames(catmod2)$fid) ## otherwise fid=0 continues to screw up code later.

	if (diagnostics){
		for (spp in dimnames(catmod2)$catch) {
			for (db in dimnames(catmod2)$dbs) {
				plotData(apply(catmod2[,mm,,spp,db],c(1,3),sum),paste("catmod2 ",spp," in ",db,sep=""),col=clrs.fishery) }
			plotData(apply(catmod2[,mm,,spp,],c(1,3),sum),paste("catmod2 ",spp," in all databases",sep=""),col=clrs.fishery)
	}	}

	## ----------------------------------------------------------------------
	## Merge modern landings from various databases (ie, collapse DB sources)
	## ----------------------------------------------------------------------
	.flush.cat("Merging modern landings from various databases (catmod) ...\n")
	catmod = array(0, dim=rev(rev(dim(catmod2))[-1]), dimnames=rev(rev(dimnames(catmod2))[-1]))
	ii = dimnames(catmod)$year
	jj = dimnames(catmod)$major
	ll = dimnames(catmod)$catch
	for (kk in dimnames(catmod)$fid) {  ## fishery IDs
		k = as.numeric(kk)
		fcat = apply(catmod2[ii,jj,kk,ll,dbmerge[[k]],drop=FALSE],1:4,max)
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
		}
	}
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

	## ------------------------------------------------------------------------------------
	## If suplementary RRF catch was supplied by `rrfhistory', incorporate it into `catmod'
	## ------------------------------------------------------------------------------------
	if (!is.null(rrfbase)) {
		.flush.cat("Incorporating suplementary RRF catch `rrfhistory' into `catmod' ...\n")
		ii = intersect(dimnames(catmod)$year,dimnames(rrfbase)$year)
		jj = intersect(dimnames(catmod)$major,dimnames(rrfbase)$major)
		kval = intersect( fidnam[as.numeric(dimnames(catmod)$fid)], dimnames(rrfbase)$fishery)
		for (k in fid) { 
			if (!is.element(fidnam[k],kval)) next
			kk  = as.character(k)
			kkk = fidnam[k] ## used to index `rrfbase', `rrfadd', and `rrfmax'

			## ------------------------------------------------------------------------------------------------
			## Determine the maximum (Canadian database) catch before adding foreign or non-DB sources of catch
			## ------------------------------------------------------------------------------------------------
			if (!is.null(rrfmax)) {
				rrfcat = array(0, dim=dim(rrfmax[ii,jj,kkk,])[1:2], dimnames=dimnames(rrfmax[ii,jj,kkk,])[1:2])
				for (nn in dimnames(rrfmax)$nation)
					rrfcat = rrfcat + rrfmax[ii,jj,kkk,nn]
#browser();return()
				zrrf  = (rrfcat[ii,jj] - catmod[ii,jj,kk,"landed"]) > 0
				catmod[ii,jj,kk,"landed"][zrrf] = rrfcat[ii,jj][zrrf]
			}
			if (!is.null(rrfadd)){
				rrfcat = array(0, dim=dim(rrfadd[ii,jj,kkk,])[1:2], dimnames=dimnames(rrfadd[ii,jj,kkk,])[1:2])
				for (nn in dimnames(rrfadd)$nation)
					rrfcat = rrfcat + rrfadd[ii,jj,kkk,nn]
				catmod[ii,jj,kk,"landed"] = catmod[ii,jj,kk,"landed"] + rrfcat[ii,jj]
			}
		}
	}
	expr=paste("cat",strSpp,"mod=catmod; save(\"cat",strSpp,"mod\",file=\"cat",strSpp,"mod.rda\")",sep="")
	eval(parse(text=expr))

	if (diagnostics){
		for (spp in dimnames(catmod)$catch) {
			plotData(apply(catmod[,mm,,spp],c(1,2),sum),paste("catmod ",spp," by major",sep=""),col=clrs.fishery)
			plotData(apply(catmod[,mm,,spp],c(1,3),sum),paste("catmod ",spp," by fishery",sep=""),col=clrs.fishery)
	}	}
	

	## ----------------------------------------------------
	## Get three historical time lines : trawl, trap, & H&L
	## Consolidate previously computes catches (either summed or taking the maximum).
	## ----------------------------------------------------
	.flush.cat("Getting three historical time lines : trawl, trap, & H&L ...\n")
	tmp0 = orfmax[,mm,,,,drop=FALSE]
	allhis = array(0, dim=dim(tmp0), dimnames=dimnames(tmp0)) ## initialize the POP/ORF/TRF array
	for (l in dimnames(allhis)$catch) {
		for (k in fsh) {
			tmp1 = orfmax[,,,k,l]
			tmp1 = tmp1[,mm,,drop=FALSE]  ## use only specified majors
			tmp2 = orfadd[,,,k,l]
			tmp2 = tmp2[,mm,,drop=FALSE]  ## use only specified majors
			allhis[,,,k,l] = tmp1 + tmp2
		}
	}
#browser();return()
	if (diagnostics){
		for (spp in dimnames(allhis)$catch) {
			plotData(apply(allhis[,mm,,,spp],c(1,2),sum),paste("allhis ",spp," by major",sep=""),col=clrs.major[mm])
			plotData(apply(allhis[,mm,,,spp],c(1,3),sum),paste("allhis ",spp," by fishery",sep=""))
	}	}

	orfhis = allhis[,,,,orfSpp]
	for (K in dimnames(orfhis)$fishery){
		orfcat = orfhis[,,,K] ## historical landed catch of the rockfish group (ORF of TRF) used to estimate RRF
		if (ascii.tables) {
			for (N in dimnames(orfhis)$nation)
				write.csv(orfcat[,,N],paste0("./tables/",orfSpp,"-Landed-Historic-fishery-",N,"-",K,"-",numdate,".csv"))
		}
	}
	if (saveinfo)
		packList(c("catmod","catmod0","catmod1","catmod2","MM.avail","MM","mm","allhis"),"PBStool",tenv=.PBStoolEnv)

	## ------------------------------------------------------
	## Terminate here if all you want are the modern landings
	## ------------------------------------------------------
	.flush.cat("-----Finished collecting the historical and modern landings-----\n\n")
	if (!reconstruct) return(list(catmod0=catmod0, catmod1=catmod1, catmod2=catmod2, catmod=catmod)) 
#browser();return()

	## ===================
	## 3. Calculate ratios
	## ===================

	## ----------------------------------------
	## Extract catch (ctab) for reference catch
	## ----------------------------------------
	.flush.cat("Start calculating ratios:\n")
	if (isThere("refyrs") && !is.null(refyrs) ) 
		ctab = catmod[as.character(refyrs),,,,drop=FALSE] 
	else ctab=catmod

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
				tmpdat$TAR = tmpdat$RFA
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
				tmpda1$TAR = tmpda1$RFA
				tmpda2 = phvdat[is.element(phvdat$fid,5),]
				tmpda2$TAR = tmpda2$RFA
				kfld = intersect(intersect(intersect(names(tmpda1),names(tmpda2)),names(phfdat)),names(fosdat))
				rawdat = rbind(tmpda1[,kfld],tmpda2[,kfld],phfdat[is.element(phfdat$fid,5),kfld],fosdat[is.element(fosdat$fid,5),kfld])
			}
		}
		rawdat$TRF    = rawdat$ORF + rawdat$POP
		rawdat$aindex = .createIDs(rawdat,aflds)
		RAWDAT[[kk]]  = refdat = rawdat
		refdat$year   = as.numeric(substring(refdat$date,1,4))
		refdat        = refdat[is.element(refdat$year,refyrs),]
		#refdat = refdat[refdat[[orfSpp]]>0 & !is.na(refdat[[orfSpp]]),]
		#refdat$ratio = refdat$landed / refdat[[orfSpp]]
		REFDAT[[kk]]  = refdat
	}
	## --------------------------------------------------
	## Get rid of reference data not using reference gear
	## --------------------------------------------------
	if ( !is.null(refgear) ) {  ## refgear must be a named list (named by fid)
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
	## -----------------------------------------------
	## Get rid of reference data not in reference area
	## -----------------------------------------------
	if ( !is.null(refarea) ){
		.flush.cat("Qualifying ref data by reference area ...\n")
		#aflds = c("major","minor","locality")
		kkk = names(refarea)
		for (kk in kkk) {
			k = as.numeric(kk)
			if (!file.exists(refarea[[kk]])) next
			kareas = read.csv(refarea[[kk]])
			if (!all(aflds %in% names(kareas)))
			showError(paste0("User-specified file `",refarea[[kk]],"' does not have the required fields\n",deparse(aflds)))
			kareas$aindex = .createIDs(kareas,aflds)
			refdat = REFDAT[[kk]]
			refdat = refdat[is.element(refdat$aindex,kareas$aindex),]
			REFDAT[[kk]]=refdat
		}
	}
	## --------------------------------------------------------------------
	## Replace elements of the reference array with modified reference data
	## --------------------------------------------------------------------
	if ( !is.null(refarea) || !is.null(refgear) ) {
		if (is.null(refgear)) kkk1 = "" else kkk1 = names(refgear)
		if (is.null(refarea)) kkk2 = "" else kkk2 = names(refarea)
		ctab.orig = ctab
		ii =as.character(refyrs)
		for (kk in dimnames(catmod)$fid) {
			if ( !kk %in% kkk1 & !kk %in% kkk2 ) next
			k = as.numeric(kk)
			for (ll in dimnames(catmod)$catch) {
				CTdat = crossTab(refdat,c("year","major"),ll)
				LLdat = convCT(CTdat)[ii,]
				jj = dimnames(LLdat)[[2]]
				ctab[ii,jj,kk,ll] = LLdat
				#jj0 = setdiff(major,jj)
				jj0 = setdiff(MM,jj)
				if (ll=="landed" && length(jj0)>0)
					ctab[ii,as.character(jj0),kk,ll] = 0
			}
		}
	}
#browser();return()

	## ------------------------------
	## Catch reference summary tables
	## ------------------------------
	.flush.cat("Calculating catch reference summary tables ...\n")
	cref  = ctab[,mm,,,drop=FALSE]               ## catch reference (start with ctab which is a subset of catmod using reference years)
	catMF = apply(cref,2:4,sum,na.rm=TRUE)       ## total catch by major and fid
	catYM = apply(cref,c(1:2,4),sum,na.rm=TRUE)  ## total catch by year and major (NOT USED CURRENTLY)
	catYF = apply(cref,c(1,3:4),sum,na.rm=TRUE)  ## total catch by year and fid   (NOT USED CURRENTLY)
	if (saveinfo)
		packList(c("cref","catYM","catMF","catYF"),"PBStool",tenv=.PBStoolEnv)

	## ----------------------------------------------------------
	## alpha - Proportion RRF caught in a major area for each fid
	## ----------------------------------------------------------
	.flush.cat("Calculating alpha (prop RRF caught in major area for each fid) ...\n")
	alpha=apply(catMF[,,"landed"],2,function(x){
		if (all(x==0)) rep(0,length(x)) else x/sum(x)}) ## columns (fids) sum to 1
	dimnames(alpha) = dimnames(catMF)[1:2]
	if (ascii.tables)
		write.csv(alpha,file=paste0("./tables/alpha-",strSpp,"_",orfSpp,"-",numdate,".csv"))
	if (diagnostics){
		plotData(alpha,"alpha (fishery in major)",col=clrs.fishery,type="bars")
		plotData(t(alpha),"alpha (major in fishery)",col=clrs.major[mm],type="bars")
	}

	## ------------------------------------------------------------
	## beta - Proportion RRF caught in H&L fisheries for each major
	## ------------------------------------------------------------
	.flush.cat("Calculating beta (prop RRF caught in H&L fisheries for each major) ...\n")
	dnam=intersect(c("2","4","5"),dimnames(alpha)[[2]]) ## dimnames for H&L
	beta=t(apply(catMF[,dnam,"landed",drop=FALSE],1,function(x){
		if (all(x==0)) rep(0,length(x)) else x/sum(x)})) ## columns (fids) sum to 1
	dimnames(beta)[[2]]=dnam; names(dimnames(beta)) = c("major","fid")
	if (ascii.tables)
		write.csv(beta,file=paste0("./tables/beta-",strSpp,"_",orfSpp,"-",numdate,".csv"))
	if (diagnostics){
		plotData(beta,"beta (fishery in major)",col=clrs.fishery,type="bars")
		plotData(t(beta),"beta (major in fishery)",col=clrs.major[mm],type="bars")
	}

	## -------------------------------------
	## Ratio RRF catch to other catch (rtar)
	## -------------------------------------
	.flush.cat("Calculating gamma (ratio of RRF to a larger group like ORF) ...\n")
	rtar=list()
	## -------------------------
	## Use cref instead of catMF
	## -------------------------
	for (ll in c("POP","ORF","TRF","TAR")) {  ## calculated for main base groups but gamma only uses `orfSpp'
		## --------------------------------------------------------------------
		## If use binomial-gamma for estimating RRF/ORF or RRF/TRF (2014-10-02)
		## --------------------------------------------------------------------
		if (useBG) {
			if (ll=="POP") .flush.cat("   using binomial-gamma to estimate gamma.\n")
			getPMRrat = function(x) {
				if (length(x)==0) pmr = 0 #c(n=0,p=NA,mu=NA,rho=NA)
				else              pmr = calcPMR(x)
				return(pmr)
			}
			getBGrat = function(x) {
				rat = mean(sampBG(n=1e5,p=x[2],mu=x[3],rho=x[4]))
				return(rat)
			}
			rmat = array(0,dim=c(length(mm),length(fid)), dimnames=list(major=mm,fid=fid))
			for (k in fid) {
				kk = as.character(k)
				refdat = REFDAT[[kk]]
				if (k==2) refdat$fid[is.element(refdat$fid,7)] = 2
				refdat = refdat[is.element(refdat$major,mm) & is.element(refdat$fid,k),]
				refdat = refdat[refdat[[ll]]>0 & !is.na(refdat[[ll]]),]
				if (nrow(refdat)==0) next
				refdat$ratio = refdat$landed / refdat[[ll]]
				#refrat = crossTab(refdat,c("major","fid"),"ratio",getBGrat)
				refpmr = crossTab(refdat,c("major","fid"),"ratio",getPMRrat)
				refpmr = convCT(refpmr)
				refmat = t(t(apply(refpmr,1,getBGrat)))
				dimnames(refmat)[[2]] = kk
				#reffid = intersect(kk,dimnames(refmat)[[2]]) # redundant
				#if (length(reffid)==0) next
				#refmat = refmat[,reffid,drop=FALSE]
#print(c(ll,k)); print(refmat)
				rmat[dimnames(refmat)[[1]],dimnames(refmat)[[2]]] = refmat
			}
		}
		else if (strat.gamma) {
			rmat = array(0,dim=c(length(mm),length(fid)), dimnames=list(major=mm,fid=fid))
			for (k in fid) {
				kk = as.character(k)
				refdat = REFDAT[[kk]]
				refdat = refdat[is.element(refdat$year,refyrs),]
				#if (k==2) refdat$fid[is.element(refdat$fid,7)] = 2
				refdat = refdat[is.element(refdat$major,mm),]
#if (k==2 && ll=="POP"){browser();return()}
				refdat = refdat[refdat[[ll]]>0 & !is.na(refdat[[ll]]),]  ## maybe only do this for halibut:TAR
				if (nrow(refdat)==0) {agamma=NULL; next }
				refdat$dzone = ceiling(refdat$fdep/depbin)*depbin
				refdat$dzone[is.na(refdat$dzone)] = 0
				## ---------------------------------------------------------
				## need at least 10% of records to contain depth information
				## ---------------------------------------------------------
				if (!all(refdat$dzone==0) && length(refdat$dzone[refdat$dzone>0])/nrow(refdat) > 0.1) 
					refdat = refdat[refdat$dzone>0 & !is.na(refdat$dzone),]
				else refdat$dzone = rep(99,nrow(refdat))
				nrefc = apply(crossTab(refdat,c("year","major","dzone"),ll,function(x){length(x[x>0])}),3,sum)
.flush.cat(c(k,ll,nrefc,"\n"))
				## -------------------------------------------------------------------
				## only use depth zones with at least 10 non-zero discard observations
				## -------------------------------------------------------------------
				urefc = nrefc[nrefc>=ifelse(k==3,3,ifelse(k==4,4,10))] 
#if (k==2 && ll=="ORF") {browser();return()}
				if (length(urefc)==0) {agamma=NULL; next }
.flush.cat(c(k,ll,urefc,"\n"))
				refdat = refdat[is.element(refdat$dzone,names(urefc)),]
				dnum  = crossTab(refdat,c("year","major","dzone"),"landed",function(x){if(all(is.na(x))) 0 else sum(x,na.rm=TRUE)/1000.})
				dden  = crossTab(refdat,c("year","major","dzone"),ll,function(x){if(all(is.na(x))) 0 else sum(x,na.rm=TRUE)/1000.})
				dnos  = crossTab(refdat,c("year","major","dzone"),ll,length)
				snos  = apply(dnos,2,sum)        ## stratify by year and depth
				pnos  = dnos
				for (p in 1:dim(dnos)[3]){
					#if (any(dim(dnos)[1:2]==1))
					pdnos = matrix(as.vector(dnos[,,p]),nrow=dim(dnos)[1],ncol=dim(dnos)[2],dimnames=dimnames(dnos)[1:2])
					ztemp = apply(pdnos,1,function(x){x/snos})
					ztemp[!is.finite(ztemp)] = 0
					pnos[,,p] = t(ztemp)
				}
				zgamma = dnum/dden
				zgamma[!is.finite(zgamma)] = 0   ## temporay artificially set rate
				pgamma = zgamma * pnos           ## weight each gamma by the proportion of observations per area
				agamma = apply(pgamma,2,sum)     ## derive the gamma vector by area
				rmat[names(agamma),kk] = agamma
			}
		}
		else { ## original method
			if (ll=="POP") .flush.cat(paste0("   calculating mean of annual ratios (",strSpp,"/",orfSpp,")\n"))
			z0  = cref[,,,"landed"]==0
			z1  = cref[,,,ll]==0
			z2  = z0&z1
			rtmp = cref[,,,"landed"]/cref[,,,ll]
			## order is important here (process equalities, zero-denominator, then zero-numerator)
			rtmp[z2]=0; rtmp[!z2&z1]=1; rtmp[!z2&!z1&z0]=0 
			rmat = apply(rtmp,2:3,mean)
		}
		rtar[[ll]] = rmat
	}
	## -------------------------------------------------------------
	## gamma - Ratio of RRF to a larger group (e.g., other rockfish)
	## -------------------------------------------------------------
	rfac  = rtar[[orfSpp]]
	gamma = rfac[mm,,drop=FALSE]  ## use only specified majors
#browser();return()

	## ----------------------------------------------------------------------------------
	## Special trawl calculations by Paul Starr for YTR based on Brian Mose's suggestions
	## ----------------------------------------------------------------------------------
	if (strSpp=="418" && !is.null(list(...)$pjs) && list(...)$pjs) {
		.flush.cat("   adjusting gamma for Yellowtail based on PJS fixed ratios.\n")
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
	## ---------------------------------------------------------------------------
	## Explicit gamma ratios for YYR/ORF (5ABCD) from the Halibut fishery provided
	## by Chris Sporer (PHMA) and deemed appropriate by their caucus
	## ---------------------------------------------------------------------------
	if (strSpp=="442" && !is.null(list(...)$phma) && list(...)$phma) {
		if (orfSpp=="ORF") {
			gamma.phma = matrix(c(0.25,0.25,0.375,0.3),nrow=4,ncol=1,dimnames=list(major=5:8,fid=2))
			gamma[row.names(gamma.phma),colnames(gamma.phma)] = gamma.phma
		}
	}
	if (ascii.tables)
		write.csv(gamma,file=paste0("./tables/gamma-",strSpp,"_",orfSpp,"-",numdate,".csv")) #;return(gamma)
#browser();return()

	if (diagnostics){
		plotData(gamma,"gamma (fishery in major)",col=clrs.fishery,type="bars")
		plotData(t(gamma),"gamma (major in fishery)",col=clrs.major[mm],type="bars")
	}

	## -----------------------------------------------------------------
	## delta - Discard rate of landed species per ORF from observer logs
	## -----------------------------------------------------------------
	.flush.cat("Calculating delta (discard rate of RRF per target) ...\n")
	assign("PBStool",ttget(PBStool))  ## remember global collection object because 'calcRatio' overwrites it
	drSpp = list(c("discard","landed"),c("discard","TAR"),c("discard","ORF"))
	drate = sapply(drSpp,function(x){paste(x,collapse=":")})   ## discard ratio description
	drN   = length(drSpp)                                      ## number of discard rates
	dfac  = array(0,dim=c(length(mm),length(fid),drN+1),
		dimnames=list(major=mm,fid=fid,rate=c(drate,"dr")))
	ologs=as.list(rep(NA,length(fid))); names(ologs)=fid
	DRATES = PNOS = list()
	for (k in fid) {
		if (k==0) next  ## just in case
		kk=as.character(k); jj=dimnames(dfac)[[1]]; j=as.numeric(jj)
		if (k==1) {
			dyrs=1997:2006
			if (file.exists("phtOlogs.rda")) {
				.flush.cat("   loading trawl observer logs from `phtOlogs.rda';\n")
				load("phtOlogs.rda")
			} else {
				## ------------------------------------------
				## Query observer logs only (can take 30 sec)
				## ------------------------------------------
				.flush.cat("   querying trawl observer logs (~30 sec);\n")
				getData("pht_obsORF.sql",dbName="PacHarvest",strSpp=strSpp,path=spath,tenv=penv(),dummy=dyrs)
				phtOlogs = PBSdat; save("phtOlogs",file="phtOlogs.rda")
			}
			discat = phtOlogs
		}
		else if (any(k==c(2:5))) {
			dyrs=2000:2004 #2007:2013
			if (any(k==c(2,4,5))) {
				if (!file.exists("phfOlogs.rda")) {
					## --------------------------------------------------
					## Query observer logs only (for halibut, sched2, ZN)
					## --------------------------------------------------
					.flush.cat("   querying h&l observer logs;\n")
					getData("phhl_fcatORF.sql","PacHarvHL",strSpp=strSpp,path=spath,fisheryid=c(2,4,5),logtype="OBSERVRLOG",tenv=penv())
					#discat=fosdat[is.element(fosdat$fid,k) & is.element(fosdat$log,105),] # fisherlogs (supposed to record all discards, electronic monitoring)
					phfOlogs = PBSdat; save("phfOlogs",file="phfOlogs.rda")
				} else {
					if (k==2){
						.flush.cat("   loading h&l observer logs from `phfOlogs.rda';\n")
						load("phfOlogs.rda")
					}
				}
				discat = phfOlogs[is.element(phfOlogs$fid,k),]
			}
			if (k==3) {
				if (file.exists("phsOlogs.rda") && !isThere("phsOlogs")) {
					.flush.cat("   loading sablefish observer logs from `phsOlogs.rda';\n")
					load("phsOlogs.rda")
				} else {
					## ---------------------------------------
					## Query observer logs only (for sabefish)
					## ---------------------------------------
					.flush.cat("   querying sablefish observer logs;\n")
					getData("phs_scatORF.sql","PacHarvSable",strSpp=strSpp,path=spath,fisheryid=k,logtype="OBSERVRLOG",tenv=penv())
					phsOlogs = PBSdat; save("phsOlogs",file="phsOlogs.rda")
				}
				discat = phsOlogs
			}
		}
		if (nrow(discat)==0) next
		if (!useSM) {
			discat = zapSeamounts(discat)
			if (nrow(discat)==0) next
		}
		discat$year = as.numeric(substring(discat$date,1,4))
		discat = discat[is.element(discat$year,dyrs) & !is.na(discat$year),]
		discat = discat[is.element(discat$major,MM) & !is.na(discat$major),]
		if (nrow(discat)==0) next
		ologs[[kk]] = discat
		DISCAT = discat
#if (k==2) {browser();return()}
		for (d in 1:drN) { ## discard ratio combos 'drSpp'
			discat = DISCAT
			dd=drate[d]
			fldN = drSpp[[d]][1]
			fldD = drSpp[[d]][2]
			if (strat.delta && is.element("fdep",names(discat)) ){ # && length(discat$fdep[!is.na(discat$fdep)])>=10) {
				#discat = discat[discat$fdep <= quantile(discat$fdep,0.95,na.rm=TRUE) & !is.na(discat$fdep),]
				#discat = DISCAT[!is.na(discat$fdep),]
				discat = discat[discat[[fldD]]>0 & !is.na(discat[[fldD]]),]  ## maybe only do this for halibut:TAR
				if (nrow(discat)==0) {DRAT=NULL; next }
				discat$dzone = ceiling(discat$fdep/depbin)*depbin
				discat$dzone[is.na(discat$dzone)] = 0
				## ---------------------------------------------------------
				## need at least 10% of records to contain depth information
				## ---------------------------------------------------------
				if (!all(discat$dzone==0) && length(discat$dzone[discat$dzone>0])/nrow(discat) > 0.1) 
					discat = discat[discat$dzone>0 & !is.na(discat$dzone),]
				else discat$dzone = rep(99,nrow(discat))
				ndisc = apply(crossTab(discat,c("year","major","dzone"),fldD,function(x){length(x[x>0])}),3,sum)
.flush.cat(c(k,dd,ndisc,"\n"))
				## -------------------------------------------------------------------
				## only use depth zones with at least 10 non-zero discard observations
				## -------------------------------------------------------------------
				udisc = ndisc[ndisc>=ifelse(k==3,3,ifelse(k==4,4,10))]
				if (length(udisc)==0) {DRAT=NULL; next }
.flush.cat(c(k,dd,udisc,"\n"))
				discat = discat[is.element(discat$dzone,names(udisc)),]
				dnum  = crossTab(discat,c("year","major","dzone"),fldN,function(x){if(all(is.na(x))) 0 else sum(x,na.rm=TRUE)/1000.})
				dden  = crossTab(discat,c("year","major","dzone"),fldD,function(x){if(all(is.na(x))) 0 else sum(x,na.rm=TRUE)/1000.})
				dnos  = crossTab(discat,c("year","major","dzone"),fldD,length)
				snos  = apply(dnos,2,sum)    ## stratify by year and depth
				pnos  = dnos
				for (p in 1:dim(dnos)[3]){
					pdnos = matrix(as.vector(dnos[,,p]),nrow=dim(dnos)[1],ncol=dim(dnos)[2],dimnames=dimnames(dnos)[1:2])
					ztemp = apply(pdnos,1,function(x){x/snos})
					ztemp[!is.finite(ztemp)] = 0
					pnos[,,p] = t(ztemp)
				}
				zdrat = dnum/dden
				zdrat[!is.finite(zdrat)] = 0 ## temporay artificially set rate
				pdrat = zdrat * pnos         ## weight each discard rate by the proportion of observations per area
				DRAT  = apply(pdrat,2,sum)   ## derive the discard rate vector by area
				PNOS[[kk]][[dd]] = pnos      ## proportion weighting by year, major andf depth zone applied to discard rates (weights sum to 1 for each area)
#if (k==2) {browser();return()}
			} else { ## original method
				dnum = convCT(crossTab(discat,c("year","major"),fldN,sum))
				dden = convCT(crossTab(discat,c("year","major"),fldD,sum))
				DRAT = dnum/dden ## annual discard rates by major
			}
			DRATES[[kk]][[dd]] = DRAT
			drat = rep(0,length(mm)); names(drat)=mm
			if (!is.null(DRAT)){
				if (is.vector(DRAT)) drat0 = DRAT
				else                 drat0 = apply(DRAT,2,mean,na.rm=TRUE) ## mean rate by major
				mmm = intersect(names(drat),names(drat0))
				drat[mmm] = drat0[mmm]
			}
			dfac[names(drat),kk,dd]=drat 
		}
#if (k==3) {browser();return()}
		## --------------------------------------------------------------
		## Assign a default discard rate by fishery
		## Changes here must also be made starting on line 1469 [mirror1]
		## --------------------------------------------------------------
		if (any(k==c(1,5)))
			dfac[jj,kk,"dr"] = dfac[jj,kk,"discard:landed"]
		if (any(k==c(2,3,4)))
			dfac[jj,kk,"dr"] = dfac[jj,kk,"discard:TAR"]
	}
#browser();return()
	dfac[is.na(dfac) | !is.finite(dfac)] = 0; delta = dfac
	assign("PBStool",PBStool,envir=.PBStoolEnv); rm(PBStool) ## restore to .PBStoolEnv and remove local copy
	save("ologs",file=paste0("ologs",strSpp,".rda"))         ## save observerlogs  with discard information
	save("DRATES",file=paste0("DRATES",strSpp,".rda"))       ## save annual discard rates
	save("PNOS",file=paste0("PNOS",strSpp,".rda"))           ## save proportion weightings applied to discard rates
	if (ascii.tables)
		write.csv(delta[,,"dr"],file=paste0("./tables/delta-",strSpp,"_",orfSpp,"-",numdate,".csv")) ## save discard rate used in CR
	if (diagnostics){
		for (rate in dimnames(delta)$rate) {
			rr = sub(":","2",rate)
			plotData(delta[,,rate],paste("delta ",rr," (fishery in major)",sep=""),col=clrs.fishery,type="bars")
			plotData(t(delta[,,rate]),paste("delta ",rr," (major in fishery)",sep=""),col=clrs.major[mm],type="bars")
	}	}
	if (saveinfo)
		packList(c("ctab","alpha","beta","rtar","gamma","delta","dfac","DRATES"),"PBStool",tenv=.PBStoolEnv)
	.flush.cat("-----Finished calculating reference ratios-----\n\n")

#browser();return()

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
#browser();return()

	## -------------------------------------------------------
	## lambda - Proportion of early catch by general gear type
	## -------------------------------------------------------
	.flush.cat("   calculating lambda (prop. early catch by general gear type).\n")
	lambda = array(0,dim=c(dim(major.gear),2),dimnames=list(major=mm,gear=gear,epoch=epoch))
	lambda[,"h&l","prewar"]=0.9; lambda[,"trap","prewar"]=0; lambda[,"trawl","prewar"]=0.1
	lambda[rownames(major.gear),colnames(major.gear),"postwar"] = major.gear
	if (ascii.tables)
		write.csv(lambda,file=paste0("./tables/lambda-",strSpp,"_",orfSpp,"-",numdate,".csv")) ## save lambda used in CR
	if (diagnostics){
		for (epo in dimnames(lambda)$epoch) {
			plotData(lambda[,,epo],paste("lambda ",epo," (gear in major)",sep=""),col=clrs.fishery,type="bars")
			plotData(t(lambda[,,epo]),paste("lambda ",epo," (major in gear)",sep=""),col=clrs.major[mm],type="bars")
	}	}

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
		plotData(apply(ancientRRF,c(1,2),sum),"ancient in major",col=clrs.major[mm])
		plotData(apply(ancientRRF,c(1,3),sum),"ancient in fishery",col=clrs.fishery)
	}
	if (saveinfo)
		packList(c("cobra","lambda","gamma.lambda","ancientRRF"),"PBStool",tenv=.PBStoolEnv)
	.flush.cat("-----Finished allocating ancient rockfish catch by unknown gear-----\n\n")

	## ================================================
	## 5. Reconstruct RRF catch prior to reported catch
	## ================================================

	.flush.cat("Starting reconstruction of RRF ...\n")
	ALLYRS = sort(union(HISYRS,MODYRS)); nyrs=length(ALLYRS)
	MEDYRS = setdiff(HISYRS,ancyrs) ## medieval years not yet used from `orfhis'
	## ---------------------------------------------------------
	## Create the final collection arrays for the reconstruction
	## ---------------------------------------------------------
	sppnewRRF = sppnewORF = array(0,dim=c(nyrs,length(mm),length(fid)+1),
		dimnames=list(year=ALLYRS,major=mm,fid=c(fid,10)))

	## -------------------------------------------------------
	## Add the ancient RRF and ORF caught be unknown gear type
	## -------------------------------------------------------
	sppnewRRF[as.character(ancyrs),mm,as.character(fid)] = ancientRRF[as.character(ancyrs),mm,as.character(fid),"CA"]
	sppnewORF[as.character(ancyrs),mm,as.character(fid)] = ancientORF[as.character(ancyrs),mm,as.character(fid),"CA"]

	## -----------------------------
	## Allocation matrix from K to k
	## -----------------------------
	.flush.cat("   calculating beta.gamma to allocate early catch from fishery K to k.\n")
	BETA = cbind(rep(1,length(mm)),beta[,"2"],rep(1,length(mm)),beta[,c("4","5")])
	dimnames(BETA)[[2]] = fid
	beta.gamma = BETA * gamma  ## Expansion of RRF:ORF from K to k
	names(dimnames(beta.gamma)) = c("major","fid")
	if (ascii.tables)
		write.csv(beta.gamma,file=paste0("./tables/beta.gamma-",strSpp,"_",orfSpp,"-",numdate,".csv")) ## save beta.gamma used in CR
	if (diagnostics){
		plotData(beta.gamma,"beta.gamma (fishery in major)",col=clrs.fishery,type="bars")
		plotData(t(beta.gamma),"beta.gamma (major in fishery)",col=clrs.major[mm],type="bars")
	}

	## ------------------------------------------------------
	## Record years that catch was reconstructed and reported
	## ------------------------------------------------------
	yrs.rec = list()
	for (k in fid) {
		.flush.cat(paste0("Fishery ",k,":\n"))
		kk = as.character(k)
		jj = dimnames(catmod)$major
		#ii = as.character(MEDYRS)
		## -----------------------------------------------------
		## Reconstruct catch history form data with gear type K.
		## Years to estimate landings of 'strSpp' from ORF/TRF
		## -----------------------------------------------------
		repcatRRF = catmod[,jj,kk,"landed"]   ## all reported landed catch of RRF (CA only in DBs)
		repcatORF = catmod[,jj,kk,orfSpp]     ## reported landed catch of the rockfish group used to estimate gamma
		repdisRRF = catmod[,jj,kk,"discard"]  ## all reported discarded catch of RRF
		if (ascii.tables) {
			write.csv(repcatRRF,paste0("./tables/",fidfive[k],"-",strSpp,"-Landed-Dbase-",numdate,".csv"))
			write.csv(repcatORF,paste0("./tables/",fidfive[k],"-",orfSpp,"-Landed-Dbase-",numdate,".csv"))
			write.csv(repdisRRF,paste0("./tables/",fidfive[k],"-",strSpp,"-Discard-Dbase-",numdate,".csv"))
		}
		## ------------------------------------------------------------------
		## Need to compare reconstructed RRF using ORF from orfhis and catmod
		## ------------------------------------------------------------------
		reccatRRFall = reccatORFall = array(0,dim=c(dim(allhis)[1:2],length(nat)), dimnames=c(dimnames(allhis)[1:2],list(nat=nat)))
		attr(reccatRRFall,"fid") = k  ## just to keep track when debugging
		for (nn in nat) {
			## ------------------------------------------------------
			## All reconstructed RRF landed catch from historical ORF
			## Exception: POP which is already reported/estimated
			## ------------------------------------------------------
			#if (strSpp=="396" && k==1) ## trawl only (important)
			if (strSpp=="396" && !is.element(nn,c("CA"))) { ## except domestic catches
				reccatRRFall[,,nn] = t(apply(allhis[,,nn,fshnam[k],"POP"],1,function(x,beta){x*beta},beta=BETA[,kk]))
				## -------------------------------------------------------------------
				## Terrible clunky fudge for early non-domestic POP estimates from ORF
				## -------------------------------------------------------------------
				if (!is.null(useYR1) && useYR1[k]>=1950) {
					iifug = as.character(1950:min(1995,useYR1[k]-1)) ## no records later than 1995 in orfhis
#browser();return()
					reccatRRFall[iifug,,nn] = t(apply(orfhis[iifug,,nn,fshnam[k]],1,function(x,bega){x*bega},bega=beta.gamma[,kk]))
				}
			} else {
				reccatRRFall[,,nn] = t(apply(orfhis[,,nn,fshnam[k]],1,function(x,bega){x*bega},bega=beta.gamma[,kk]))
			}
			reccatORFall[,,nn] = t(apply(orfhis[,,nn,fshnam[k]],1,function(x,beta){x*beta},beta=BETA[,kk])) ## all historical ORF landed catch
		}
		## --------------------------------------------------
		## Reconstructed domestic RRF landings from modern ORF
		## --------------------------------------------------
		reccatRRF2 = t(apply(catmod[,mm,kk,orfSpp],1,function(x,gamm){x*gamm},gamm=gamma[,kk]))
		icom = intersect(dimnames(reccatRRFall)[[1]],dimnames(reccatRRF2)[[1]])
		if (length(icom)>0) {
			reccatRRFraw  = reccatRRFall[,,"CA"]
			reccatRRFcom  = reccatRRFraw[icom,,drop=FALSE]
			reccatRRFcom2 = reccatRRF2[icom,,drop=FALSE]
			## --------------------------------------
			## Is the modern recon > historical recon
			## --------------------------------------
			useMod = reccatRRFcom2 > reccatRRFcom
			## ----------------------------------------
			## Use adjusted modern ORF for Langara Spit
			## ----------------------------------------
			if (k==1 && !useLS)
				useMod[intersect(1983:1993,icom),"9"] = TRUE
			if (any(useMod))
				reccatRRFcom[useMod]  = reccatRRFcom2[useMod]
			reccatRRFall[icom,,"CA"] = reccatRRFcom
		}
		## ------------------------------------------------------------------------------------------------
		## We are not trying to reconstruct ORF for user-specified years.
		## There is an overlap period between orfhis and reported ORF where landings should be similar.
		## Here we take the maximum during the overlap, though this does not affect the RRF reconstruction.
		## ------------------------------------------------------------------------------------------------
		icon = intersect(dimnames(reccatORFall)[[1]],dimnames(repcatORF)[[1]])
		if (length(icon)>0) {
			reccatORFraw = reccatORFall[,,"CA"]
			reccatORFcon = reccatORFraw[icon,,drop=FALSE]
			repcatORFcon = repcatORF[icon,,drop=FALSE]
			## -------------------------------------
			## Is the reported ORF > historical ORF?
			## -------------------------------------
			useNod = repcatORFcon > reccatORFcon
			if (k==1 && !useLS) ## use adjusted modern ORF for Langara Spit
				useNod[intersect(1983:1993,icon),"9"] = TRUE
			if (any(useNod))
				reccatORFcon[useNod]  = repcatORFcon[useNod]
			reccatORFall[icon,,"CA"] = reccatORFcon
		}
#if (k==2) {browser();return()}

		## -----------------------------------------------------------------------------------
		## Add any ancient catch from (trawl,trap,h&l) (1918-1950) (keep separate form MEDYRS)
		## -----------------------------------------------------------------------------------
		iiaa = as.character(ancyrs)
		for (nn in nat) {
			ancientRRF[iiaa,mm,kk,nn] = ancientRRF[iiaa,mm,kk,nn] + reccatRRFall[iiaa,mm,nn]
			ancientORF[iiaa,mm,kk,nn] = ancientORF[iiaa,mm,kk,nn] + reccatORFall[iiaa,mm,nn]
			if (nn %in% natUse) {
				sppnewRRF[iiaa,mm,kk]  = sppnewRRF[iiaa,mm,kk]  + reccatRRFall[iiaa,mm,nn]
				sppnewORF[iiaa,mm,kk]  = sppnewORF[iiaa,mm,kk]  + reccatORFall[iiaa,mm,nn]
			}
		}
#browser();return()

		##*** `sppnewRRF' only contains fishery catch from 1918-1950 at this stage.

		## --------------------------------------------------------------------------------------------
		## Detect modern RRF landings and mesh with reconstructed RRF landings if no `useYR1' specified
		## --------------------------------------------------------------------------------------------
		if (is.null(useYR1)) {
			.flush.cat(paste0("   automatically detecting modern catches of ",strSpp,";\n"))
			irep   = dimnames(repcatRRF)[[1]]
			xrep = as.numeric(irep)
			## ------------------------------------------------------------------------------------
			## Start believing reported catch when it exceeds the 10th percentile of non-zero catch
			## ------------------------------------------------------------------------------------
			x0 = apply(repcatRRF,2,function(y,x){x[y>quantile(y[y>0],0.10)][1]},x=xrep) ## vector of start years for each major
			x0 = pmax(x0,max(ancyrs)+1) ## cannot go back into the ancient years
			x0 = pmin(x0,1996)          ## have to believe all catches from 1996 on
			x0[is.na(x0)] = 1996        ## ditto 
			zrep = sapply(x0,function(x,xrep){xrep>=x},xrep=xrep)
			sppnewRRF[irep,jj,kk][zrep] = repcatRRF[zrep]   ## use automatically detected repcat catch (CA)
			sppnewORF[irep,jj,kk][zrep] = repcatORF[zrep]   ## use automatically detected repcat catch (CA)
			## ----------------------------------------------------------
			## select the reconstructed catch for reported catch not used
			## ----------------------------------------------------------
			irec   = dimnames(reccatRRFall)[[1]]
			xrec = as.numeric(irec)
			## ------------------------------------------------------------------
			## identify reconstructed catch that had no believable reported catch
			## ------------------------------------------------------------------
			#zrec = sapply(x0,function(x,xrec){xrec<x & xrec>rev(ancyrs)[1]},xrec=xrec)  ## years after 1950 and before first reported catch
			zrec = sapply(x0,function(x,xrec){xrec<x & xrec>rev(ancyrs)[1]},xrec=xrec)  ## years after 1950 and before first reported catch
			zall = array(TRUE,dim=c(length(irec),length(jj)))
#browser();return()
			for (nn in natUse){
				zuse = if (nn %in% "CA") zrec else zall
				sppnewRRF[irec,jj,kk][zuse] =  sppnewRRF[irec,jj,kk][zuse] + reccatRRFall[irec,jj,nn][zuse]
				sppnewORF[irec,jj,kk][zuse] =  sppnewORF[irec,jj,kk][zuse] + reccatORFall[irec,jj,nn][zuse]
			}
			yrs.rec[[kk]][["xrec"]] = sapply(as.data.frame(zrec),function(z){xrec[z]},simplify=FALSE)  ## record reconstructed years
			yrs.rec[[kk]][["xrep"]] = sapply(as.data.frame(zrep),function(z){xrep[z]},simplify=FALSE)  ## record reported years
		}
		else {
			## -------------------------------------------------------------------------
			## Needed because estimation from orfSpp cannot occur before 1996 at present
			useYRstart = min(ifelse(is.na(useYR1[k]), useYR1def, useYR1[k]), 1996)
			## -------------------------------------------------------------------------

			## ------------------------------------------------------------------
			## POP trawl catch relatively well known back to 1956
			#if (strSpp=="396" && k==1) useYRstart = 1956
			## YYR H&L catch relatively well known back to 1982
			#if (any(strSpp==c("442","396")) && any(k==c(4,5))) useYRstart = 1982
			## ------------------------------------------------------------------

			useYRS = useYRstart:rev(MODYRS)[1]
			irep   = as.character(useYRS)
			yrs.rec[[kk]][["xrep"]] = irep  ## record reported years
			estYRend = useYRstart - 1
			## ----------------------------------------------------------------------------------------
			## only estimate if useYRstart > 1950 and less than first year of believable reported catch
			## ----------------------------------------------------------------------------------------
			if (estYRend > MEDYRS[1]) { 
				.flush.cat(paste0("   estimating ",strSpp," from  reconstructing from ",MEDYRS[1]," to ",estYRend,";\n"))
				estYRS = MEDYRS[1]:estYRend
				irec   = as.character(estYRS)
				## ---------------------------------------------------------
				## Combine estimated RRF landings with reported RRF landings
				## ---------------------------------------------------------
				yrs.rec[[kk]][["xrec"]] = irec  ## record reconstructed years
				reccatRRF = reccatRRFall[,,"CA"]
				comcatRRF = rbind(reccatRRF[irec,,drop=FALSE],repcatRRF[irep,,drop=FALSE])
				reccatORF = reccatORFall[,,"CA"]
				comcatORF = rbind(reccatORF[irec,,drop=FALSE],repcatORF[irep,,drop=FALSE])
#if (k==1) {browser();return()}
			} else {
				yrs.rec[[kk]][["xrec"]] = "none"  ## record reconstructed years
				comcatRRF = repcatRRF[irep,,drop=FALSE]
				comcatORF = repcatORF[irep,,drop=FALSE]
			}
			iicc = dimnames(comcatRRF)[[1]]
			jjcc = dimnames(comcatRRF)[[2]]
			for (nn in natUse){
				if (nn %in% "CA") {
					sppnewRRF[iicc,jjcc,kk] = sppnewRRF[iicc,jjcc,kk] + comcatRRF[iicc,jjcc,drop=FALSE]
					sppnewORF[iicc,jjcc,kk] = sppnewORF[iicc,jjcc,kk] + comcatORF[iicc,jjcc,drop=FALSE]
				} else {
					iimed = as.character(MEDYRS[1]:1977)  ## Canada's EEZ established in 1977 (allow for some catch in 1977)
					## ----------------------------------------------------------------------
					## Kate Rutherford (2016-11-14):
					## For many years Canadian trawlers landed their fish in Washington state
					## (Blaine, Bellingham). The fish were still caught in Canada.
					## ----------------------------------------------------------------------
					iimed = as.character(MEDYRS)
#browser();return()
					sppnewRRF[iimed,jjcc,kk] = sppnewRRF[iimed,jjcc,kk] + reccatRRFall[iimed,jjcc,nn]
					sppnewORF[iimed,jjcc,kk] = sppnewORF[iimed,jjcc,kk] + reccatORFall[iimed,jjcc,nn]
				}
			}
		}

		recLfileRRF = paste0("./tables/",fidfive[k],"-",strSpp,"-Landed-Recon-",numdate,".csv")
		recLfileORF = paste0("./tables/",fidfive[k],"-",orfSpp,"-Landed-Recon-",numdate,".csv")
		if (ascii.tables) {
			write.csv(sppnewRRF[,,kk],recLfileRRF)
			write.csv(sppnewORF[,,kk],recLfileORF)
			for (recLfile in c(recLfileRRF,recLfileORF)){
				cat("\nYears,start,end\n",file=recLfile,append=TRUE)
				cat(paste0("Ancient,",ancyrs[1],",",rev(ancyrs)[1],"\n"),file=recLfile,append=TRUE)
				cat(paste0("Medieval,",irec[1],",",rev(irec)[1],"\n"),file=recLfile,append=TRUE)
				cat(paste0("Modern,",irep[1],",",rev(irep)[1],"\n"),file=recLfile,append=TRUE)
			}
		}
#browser();return()

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
		discard.regimes = switch( k, ## default regime
			list(inone=ALLYRS[1]:1953, icalc=1954:1995, idata=1996:ALLYRS[nyrs]),  ## trawl (use `landed' for discard rate)
			list(inone=ALLYRS[1]:1985, icalc=1986:2005, idata=2006:ALLYRS[nyrs]),  ## halibut
			list(inone=ALLYRS[1]:1985, icalc=1986:2005, idata=2006:ALLYRS[nyrs]),  ## sablefish
			list(inone=ALLYRS[1]:1985, icalc=1986:2005, idata=2006:ALLYRS[nyrs]),  ## schedule II
			list(inone=ALLYRS[1]:1985, icalc=1986:2005, idata=2006:ALLYRS[nyrs]))  ## ZN rockfish (use `landed' for discard rate)

		## --------------------------
		## Special conditions for YYR
		## --------------------------
		if (strSpp %in% c("442")){
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
			## Changes here must also be made starting on line 1231 [mirror1]
			## ---------------------------------------------------------------
			if (any(k==c(1,5))) {
				icalc = intersect(dimnames(sppnewRRF)[[1]],icalc)
				kcalc = sppnewRRF[,,kk]
			}
			if (any(k==c(2,3,4))) {
				kcalc = catmod[,,kk,"TAR"]                     ## Get TAR, which will be specific to each fishery
				if (k==2 && isThere("cat614")) {               ## use Lynne's IPHC halibut data from Kong (2015)
					zpos = apply(kcalc,1,function(x){all(x>0)}) ## determine when all BC areas have halibut catch
					zyrs = names(zpos)[zpos][1:5]               ## use the first 5 years
					area.hcat = apply(kcalc[zyrs,],2,sum)       ## calculate total catch by area
					parea.hcat = area.hcat/sum(area.hcat)       ## calculate the proportion catch by area
					kcalc = t(sapply(cat614,function(x,p){x*p},p=parea.hcat)) ## expand annual coastwide catch to catch by areas
					icalc = intersect(dimnames(kcalc)[[1]],icalc)
					if (saveinfo) packList(c("area.hcat","parea.hcat"),"PBStool",tenv=.PBStoolEnv)
				} else {
					icalc=intersect(dimnames(catmod)[[1]],icalc)
				}
			}
			kcalc = kcalc[icalc,jj,drop=FALSE] 
			disC  = t(apply(kcalc,1,function(x,dr){x*dr},dr=dr))
#if (k==5) {browser();return()}
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

		## -------------------
		## Discard output file
		## -------------------
		recDfile = paste0("./tables/",fidfive[k],"-",strSpp,"-Discard-Recon-",numdate,".csv")
		disT = disD[idata,jj]
		if (!is.null(disC))
			disT = rbind(disC[icalc,jj],disT)
		if (!all(is.na(inone))) {
			disN=array(0,c(length(inone),length(jj)),dimnames=list(inone,jj)) # construct zero discard matrix in years before regulations
			disT = rbind(disN[inone,jj],disT) #[c(icalc,idata),jj])
		}
#if (k==1) {browser();return()}
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
		recCfile = paste0("./tables/",fidfive[k],"-",strSpp,"-Catch-Recon-",numdate,".csv")
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
			plotData(reccatRRF,paste("reccat for ",fidnam[k]," by major",sep=""),col=clrs.major[mm])
			plotData(repcatRRF,paste("repcat for ",fidnam[k]," by major",sep=""),col=clrs.major[mm])
			plotData(disC,paste("disC for ",fidnam[k]," by major",sep=""),col=clrs.major[mm])
			plotData(disD,paste("disD for ",fidnam[k]," by major",sep=""),col=clrs.major[mm])
			plotData(sppnewRRF[,,kk],paste("sppnew for ",fidnam[k]," by major",sep=""),col=clrs.major[mm])
		}
	}
	attr(sppnewRRF,"yrs.rec") = yrs.rec # years of reconstructed and reported catch
	sppnewRRF[,,"10"] = apply(sppnewRRF[,,as.character(fid)],1:2,sum)  ## sum across fisheries
	
	#data(pmfc,species,envir=penv())
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
		plotData(sppnewRRF[,,"10"],paste("sppnew for ",fidnam[10]," by major",sep=""),col=clrs.major[mm])
		collectFigs(path="./CRdiag",width=5,capskip=-20,is.fnum=TRUE)
	}
	expr=paste("cat",strSpp,"rec=sppnewRRF; save(\"cat",strSpp,"rec\",file=\"cat",strSpp,"rec.rda\")",sep="")
	eval(parse(text=expr))

	if (saveinfo) packList(c("HISYRS","MODYRS","ALLYRS","inone","icalc","idata",
		"disC","disD","sppnewRRF","beta.gamma"),"PBStool",tenv=.PBStoolEnv)

	## ----------------
	## Plot the results
	## ----------------
	.flush.cat("Plotting and reporting reconstruction results ...\n\n")
	if (!any(c(eps,png,wmf)))
		eps = TRUE
#browser();return()
	for (k in fidout) {
		yrs = as.numeric(dimnames(sppnewRRF)$year)
		plotRecon(sppnewRRF,strSpp=strSpp,major=major,fidout=k,years=yrs,eps=eps,png=png,wmf=wmf) # use user-specified major
	}
	fidlab = c("Trawl","Halibut","Sablefish","Dogfish-Lingcod","H&L Rockfish","Sablefish + ZN",
		"Sablefish + Halibut","Dogfish","Lingcod",paste0("Combined Fisheries",ifelse(addGFB," + Surveys","")))
	
	## ---------------------------------------------
	## ADMB catch data file for the combined fishery
	## ---------------------------------------------
	admdat = paste0("./tables/admb-cat",strSpp,"-",numdate,ifelse(useGFM,"","-multiDB"),".dat")
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
	onam=paste0("./tables/Catch-History-",strSpp,"-",numdate,ifelse(useGFM,"","-multiDB"),".csv")  ## output file name
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
		packList(c("plotname","clrs.major","clrs.fishery","fidlab"),"PBStool",tenv=.PBStoolEnv)
		ttget(PBStool)
		save("PBStool",file=paste("PBStool",strSpp,".rda",sep=""))
	}
	if (eps)
		collectFigs(path=".",ext="eps",fout=paste("Catch-Recon-Summary-",strSpp,sep=""),width=6.75,pattern="Catch-History")
	.flush.cat(paste0("-----Finished reconstructing catch for ",species[strSpp,"latin"],"-----\n\n"))
#browser();return()
	invisible(sppnewRRF)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~buildCatch


#plotData-------------------------------2013-01-28
# Plot diagnostic data for catch reconstructions.
#-----------------------------------------------RH
plotData =function(x,description="something",
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
	plotname = paste("CRdiag/pD",pad0(pD,3),"-",gsub(" ","-",description),sep="")
#browser();return()
	if (eps) 
		postscript(file=paste(plotname,".eps",sep = ""), width=6.5,height=5,paper="special")
	else if (wmf && .Platform$OS.type=="windows")
		do.call("win.metafile",list(filename=paste(plotname,".wmf",sep = ""), width=6.5,height=5))
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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotData


#plotRecon------------------------------2015-12-01
# Plot reconstructed catch using barplots stacked by PMFC area.
#-----------------------------------------------RH
plotRecon = function(dat=cat440rec, strSpp="440", major=c(1,3:9), fidout=10, 
   years=1918:2015, xlab=seq(1920,2050,5), yrs.rec=attributes(dat)$yrs.rec,
   shade=FALSE, shadier=FALSE,
   eps=FALSE, png=FALSE, wmf=FALSE, PIN=c(10,5))
{
	if (dev.cur()>1) { oldpar=par(no.readonly=TRUE); on.exit(par(oldpar)) }
	fshnam=c("trawl","h&l","trap",rep("h&l",6),"combined") #general category vector
	fidnam=c("trawl","halibut","sablefish","sched2","zn","sabzn","sabhal","dogfish","lingcod","combined")
	fidlab=c(paste0(c("Trawl","Halibut","Sablefish","Dogfish-Lingcod","H&L Rockfish","Sablefish + ZN",
		"Sablefish + Halibut","Dogfish","Lingcod")," Fishery"),"All Commercial Groundfish Fisheries")
	yy  = as.character(years)
	if (length(dim(dat))==2) ydat = dat[yy,,drop=FALSE]
	else                     ydat = dat[yy,,,drop=FALSE]
	MAJ = as.character(1:10); mm=as.character(major)
	clrs = rep("gainsboro",length(MAJ)); names(clrs)=MAJ
	clrs[as.character(c(1,3:9))]=c("moccasin","blue","lightblue","yellow","orange","red","seagreen","lightgreen")
	mclrs=clrs[mm]
	data(pmfc,species,envir=penv())
	XLAB=dimnames(ydat)[[1]];  xpos=(1:length(XLAB))-.5; zlab=is.element(XLAB,xlab)
	for (i in fidout){
		ii=as.character(i)
		if (length(dim(ydat))==2) idat = t(ydat)
		else idat = t(ydat[,mm,ii])
		plotname=paste(strSpp,"-Catch-History",ifelse(i==10,0,i),"-",fidnam[i],"-years(",min(years),"-",max(years),")-major(",paste(major,collapse=""),")",sep="")
		if (eps)       postscript(file=paste(plotname,".eps",sep=""),width=PIN[1],height=PIN[2],fonts="mono",paper="special")
		else if (png)  png(filename=paste(plotname,".png",sep=""),width=round(100*PIN[1]),height=round(100*PIN[2]),pointsize=16)
		else if (wmf && .Platform$OS.type=="windows")
			do.call("win.metafile",list(filename=paste(plotname,".wmf",sep=""),width=PIN[1],height=PIN[2]))
		else  resetGraph()
		expandGraph(mar=c(3,3.2,.5,.5),mgp=c(1.6,.5,0))
		barplot(idat,col=0,space=0,xaxt="n",yaxt="n",xaxs="i")
		if (shade && !is.null(yrs.rec)) {
			## shade the ancient years (Dominion Bureau of Stats)
			xanc = as.character(1918:1950)
			panc = (1:length(yy))[is.element(yy,xanc)]
			nanc = length(panc)
			if (nanc>1)
				polygon(panc[c(1,1,nanc,nanc)]+c(-1,-1,0,0),c(0,rep(par()$usr[4],2),0),border=FALSE,col=ifelse(shadier,"aliceblue","#FAFCFF"))

			getRange = function(xyrs) {
				if (is.list(xyrs))
					xrange = max(sapply(xyrs,function(x){min(x)})) : min(sapply(xyrs,function(x){max(x)}))
				else
					xrange = xyrs
				#return((xrec))
				return(as.numeric(xrange))
			}
			## shade the reconstructed years
			if (ii=="10") {
				xrec = list()
				for (j in 1:5)
					xrec[[j]] = getRange(yrs.rec[[as.character(j)]][["xrec"]])
				xrec = getRange(xrec)
			} else
				xrec = getRange(yrs.rec[[ii]][["xrec"]])
			prec = (1:length(yy))[is.element(yy,xrec)]
			nrec = length(prec)
			if (nrec>1)
				polygon(prec[c(1,1,nrec,nrec)]+c(-1,-1,0,0),c(0,rep(par()$usr[4],2),0),border=FALSE,col=ifelse(shadier,"lightyellow","ivory"))

			## shade the reported years
			if (ii=="10") {
				xrep = list()
				for (j in 1:5)
					xrep[[j]] = getRange(yrs.rec[[as.character(j)]][["xrep"]])
				xrep = getRange(xrep)
			} else
				xrep = getRange(yrs.rec[[ii]][["xrep"]])
			prep = (1:length(yy))[is.element(yy,xrep)]
			nrep = length(prep)
			if (nrep>1)
				polygon(prep[c(1,1,nrep,nrep)]+c(-1,-1,0,0),c(0,rep(par()$usr[4],2),0),border=FALSE,col=ifelse(shadier,"honeydew","mintcream"))

			## delimit with dashed vertial line
			if (panc[nanc]==(prec[1]-1))
				abline(v=panc[nanc],lty=2)
			if (prec[nrec]==(prep[1]-1))
				abline(v=prec[nrec],lty=2)
			else {
				polygon(c(prec[c(nrec,nrec)],prep[c(1,1)])+c(0,0,-1,-1),c(0,rep(par()$usr[4],2),0),border=FALSE,col=ifelse(shadier,"ivory","white"))
				abline(v=prec[nrec],lty=2)
				abline(v=prep[1]-1,lty=2)
			}
		}
		yaxp=par()$yaxp; yint=yaxp[2]/yaxp[3]; hlin=seq(yint,yaxp[2],yint)
		segments(rep(0,length(hlin)),hlin,rep(par()$usr[2],length(hlin)),hlin,col="gainsboro")
		barplot(idat,col=mclrs,space=0,cex.names=.8,mgp=c(1.5,.5,0),xaxt="n",xaxs="i",border="grey30",add=TRUE,lwd=0.3)
		axis(1,at=xpos[zlab],labels=XLAB[zlab],tick=FALSE,las=3,mgp=c(0,.2,0),cex.axis=.8,hadj=1)
		addLabel(.05,.95,species[strSpp,"latin"],font=3,cex=1,col="#400080",adj=0)
		addLabel(.05,.90,fidlab[i],cex=1.2,col="#400080",adj=0)
		mtext("Year",side=1,line=1.75,cex=1.2)
		mtext("Catch (t)",side=2,line=2,cex=1.3)
		addStrip(.05, 0.85, col=mclrs, lab=pmfc[mm,"gmu"]) ## RH: New function addition (151201)
		if (eps|png|wmf) dev.off()
	}
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotRecon

#surveyCatch----------------------------2015-08-06
# Query GFBioSQL for survey catch and summarise
# catches by year and PMFC area.
#-----------------------------------------------RH
surveyCatch = function(strSpp="396", spath=.getSpath(), gfbdat=NULL)
{
	bigdate = Sys.Date(); numdate = substring(gsub("-","",bigdate),3)
	if (is.null(gfbdat)) {
		if (isThere("PBSdat")) rm(PBSdat) ## remove from current environment
		## GFBioSQL catch summary for surveys
		getData("gfb_rcatORF.sql",dbName="GFBioSQL",strSpp=strSpp,path=spath,,tenv=penv())
		assign("gfbdat",PBSdat)
		save("gfbdat",file="gfbdat.rda")
		if (file.exists("./tables"))
			write.csv(gfbdat,paste0("./tables/Survey-Records-",strSpp,"-",numdate,".csv"),row.names=FALSE)
		#rcat = gfbdat # used for summaries by SSID and SVID down below
		rm(PBSdat) ## just to be safe
	}
	gfbtab = crossTab(gfbdat,c("year","major"),"catKg") # summarise catch (t)
	gfbcat = as.data.frame(gfbtab[,-1])
	row.names(gfbcat) = gfbtab[,1]
	data(pmfc,package="PBSdata",envir=penv())
	names(gfbcat) = pmfc[names(gfbcat),"gmu"]
	save("gfbcat",file="gfbcat.rda")
	if (file.exists("./tables"))
		write.csv(gfbcat,file=paste0("./tables/Catch-Survey-",strSpp,"-(Year-PMFC)-",numdate,".csv"))

	#getFile(gfbdat)
	gfbdat$SVID[is.na(gfbdat$SVID)] = 999
	gfbdat$SSID[is.na(gfbdat$SSID)] = 999

	spp.svid = crossTab(gfbdat,c("SVID","year"),"catKg")
	getData("SURVEY","GFBioSQL")
	survey = PBSdat
	svid = survey[[2]]; names(svid) = survey[[1]]
	svid = c(svid,`999`="No Survey Identified")
	SVID = spp.svid$SVID
	svidcat = spp.svid[,-1]; attr(svidcat,"class") = "data.frame"
	dimnames(svidcat)[[1]] = paste0("SVID ",SVID,": ",svid[as.character(SVID)])
	if (file.exists("./tables"))
		write.csv(svidcat,file=paste0("./tables/Catch-Survey-",strSpp,"-(SVID-Year)-",numdate,".csv"))

	spp.ssid = crossTab(gfbdat,c("SSID","year"),"catKg")
	getData("SURVEY_SERIES","GFBioSQL")
	series = PBSdat
	ssid = series[[2]]; names(ssid) = series[[1]]
	ssid = c(ssid,`999`="No Survey Series Identified")
	SSID = spp.ssid$SSID
	ssidcat = spp.ssid[,-1]; attr(ssidcat,"class") = "data.frame"
	dimnames(ssidcat)[[1]] = paste0("SSID ",SSID,": ",ssid[as.character(SSID)])
	if (file.exists("./tables"))
		write.csv(ssidcat,file=paste0("./tables/Catch-Survey-",strSpp,"-(SSID-Year)-",numdate,".csv"))

	invisible(gfbcat)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~surveyCatch

#zapSeamounts---------------------------2015-04-21
# Remove seamount records use combinations of 
# major, minor, and locality codes.
#-----------------------------------------------RH
zapSeamounts = function(dat) {
	seamounts = t(data.frame(
		heck=c(3,24,11), eickelberg=c(3,24,12), union=c(4,27,5), dellwood=c(5,11,8), bowie=c(9,31,10),
		pratt=c(10,33,6), surveyor=c(10,33,7), durgin=c(10,33,8), murray=c(10,40,4), cowie=c(10,40,5),
		pathfinder=c(11,42,1), morton=c(11,42,2), miller=c(11,42,3), cobb=c(67,67,1), brownbear=c(67,67,2), 
		row.names=c("major","minor","locality"), stringsAsFactors=FALSE))
	fnam = as.character(substitute(dat))
	nSMrec = 0; names(nSMrec) = fnam; tSMcat = nSMrec
	ii = fnam; idat = dat
	if (!all(c("major","minor","locality") %in% names(idat))) { 
		nSMrec[ii] = NA; tSMcat[ii] = NA
	} else {
		for (j in 1:nrow(seamounts)) {
			jj = seamounts[j,]
			seamo = is.element(idat$major,jj[1]) & is.element(idat$minor,jj[2]) & is.element(idat$locality,jj[3])
			if (sum(seamo)==0) next
			nSMrec[ii] = nSMrec[ii] + sum(seamo)
			catflds = names(idat)[is.element(names(idat),c("landed","discard","catKg"))]
			if (length(catflds)>0)
				tSMcat[ii] = tSMcat[ii] + sum(idat[seamo,catflds],na.rm=TRUE)
				idat = idat[!seamo,]
		}
	}
	tSMcat = tSMcat/1000. # convert total catch from seamounts into tonnes
	attr(idat,"nSMrec") = nSMrec
	attr(idat,"tSMcat") = tSMcat
	return(idat)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~zapSeamounts


#===============================================================================
