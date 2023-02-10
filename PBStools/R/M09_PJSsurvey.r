##==============================================================================
## Module 9: PJS Survey Index Series
## ---------------------------------
##  calcBiom........Calculate swept-area biomass estimate and bootstrap within strata.
##  doSynoptic......Get the synoptic file for specified survey and go through PJS machinations.
##  getLabels.......Read in data file and get labels.
##  getLabelends....Get depth ranges from stratum names (methinks).
##  getMonth........Extract month from dates vector.
##  getSpecies......Get species codes from data.
##  getUsability....Qualify data by usability code.
##  getVessel.......Get survey vessel codes.
##  keepAtts........Keep user's attributes after performing an expression such as subsetting a file.
##  plotIndex.......Plot survey index series after bootstrapping.
##  prepGFsurv......Prepare GF surveys using PJS codes.
##  prepGIGhis......Prepare GIG Historical (formerly called BG Reed) survey data.
##  prepHSass.......Prepare Hecate Strait assemblage survey data.
##  prepHSsyn.......Prepare Hecate Strait synoptic survey data.
##  prepQCSsyn......Prepare QCS synoptic survey data.
##  prepNMFStri.....Prepare US National Marine Fisheries Service triennial survey data from WCVI.
##  prepWCHGsyn.....Prepare WCHG (formerly WCQCI) synoptic survey data
##  prepWCVIsyn.....Prepare west coast Vancouver Island synoptic survey data.
##  restratify......Restratify survey by depth.
##  uniqtows........Ensure unique tows occur for each survey, collapse records as needed.
##==============================================================================


## -----------------PJS Notes---------------------
## /* [06 August 2007]
## generic program to prepare survey data: all in one complex file
## use an integer code to designate the survey:
## 1: old Hecate St Survey (note: this program does not prepare the recruited biomass estimate as done for ENL in 2006)
## 2: QC Sound Synoptic
## 3: WCVI Synoptic
## 4: WCVI Shrimp
## 5: Triennial
## 6: Pcod Monitoring
## 7: QCSound Shrimp
## 8: historical GB Reed survey (now includes 1984 Eastward Ho and 1990s Ocean Selector and Frosti - use this for biomass ests)
## 9: WCQCI Synoptic (added Nov 2008)
## 10: Hecate St synoptic survey (added July 2010)
## 11: retrospective GIG (includes GB Reed and misc GIG surveys) (added Nov 2009 - not for biomass)
## 12: retrospective GIG (1995 Ocean Selector and Frosti only - QC Snd synoptic stratification) (added Dec 2009)
## 
## [09 Aug 2007]
## add capacity to process dates
##
## [31 Aug 2007]
## add historical GB Reed surveys
##
## [08 Nov 2007]
## add depint variable to GB Reed surveys
##
## [08 July 2008]
## minor fixes to the triennial survey code (re lats and longs)
##
## [31 Oct 2008]
## consolidated some code and updated program to fit revised GFBioCatEff.mdb
## also fixed exit codes
##
## [02 Nov 2008]
## hardwired time series so that only GB Reed survey trips are included in the gfsurvey file
##
## [09 Nov 2008]
## added code to check for missing weight data when there are numbers caught 
## (and sub with mean weight by year)
## /*Note: this option has been disabled at the suggestion of Norm Olsen (see getweight subroutine)*/
## 
## [10&18 Nov 2008]
## add west coast QCI synoptic survey to this program (survey==9)
## 
## [18-20 Dec 2008]
## changed the approach to data prep to conform to what Norm Olsen is doing, based on a phone call this AM.  
## For calculating distance travelled for a survey tow, he uses the following order of calculation:
## #1: use FE_distance_travelled as default 
##     (Norm says that he and Kate have groomed the data to ensure this was reliable and based on vessel tracks)
## #2: use bottom_contact*speed
## #3: use winch_time*speed
## Norm also uses default speed and doorspreads that are year and survey specific (not stratum specific as I do)
##
## [05 Sep 2009]
## modify getweight subroutine so I can find records where there is a valid species code but no data
##
## [26 Nov 2009]
## 1. reserve a survey code for the hecate st synoptic survey which I will start using next year
## 2. add option to GB Reed prep to process other vessels (GIG retrospective) (updated 02 Dec 2009) 
##
## [12 Dec 2009]:
## 3. survey #8 is now the full GIG retrospective (=survey_series 32).  This is for the 464 tows identified by moi
##    between latitudes 50.9 and 51.6.  It includes the 1967 to 1984 GB Reed surveys, plus the 1984 Eastward Ho, 
##    the 1994 & 1995 Ocean Selector and the 1995 Frosti.  This survey will use the Norm Olsen grouping codes
##  	4. survey #11 will remain the full series (1601 tows for survey_series 21).  Includes all tows from 1965 to 2005,
##    going from the GB Reed to the Viking Storm.  This will also include Mitchell Gully tows, the discarded Southward
##    Ho 1979 survey, and the same surveys as #8.  The difference is that I will create my own stratification for these surveys
## 5. added survey #12 to accommodate survey_series_ID=33.  This is only for the two 1995 Ocean Selector and Frosti surveys
##    so that they can use the same grouping codes as the QC Sound synoptic survey (survey#2 in this software)
## 6. decided that I will reserve all remaining codes from #13 to #19 to WC GF surveys.  Any additional surveys from
##    other areas that will use this software will be given codes beginning at #20
##
## [19 July 2010]: minor bug fixes (none serious)
##
## [14 Dec 2010]: add generic GIG stratum code to survey==11//this is now the survey code to use when analysing this
## 	survey series
##
## [17 Jan 2011]: add compulsory log file 
##
## [02 Apr 2011]: add line of code to exclude 19 target strength acoustic tows in Ocean Selector 1994 GIG survey
## 	(survey codes 8 & 11):  seems that it doesn't matter which code we use to prepare this survey (get the same
## 	result as long as stratum==1)
##
## [24-30 July 2012]: add mask option to code (need to deal with change to "ymd" from "dmy")
## 	add Caledonian 1996 survey to code (survey code=13)
## 	add traps to find out-of-range depth and distance information for synoptic surveys only
## [05 Aug 2012]: 
## 	add GBReed_historic (emphasis on WCVI surveys) (survey_code=14)
## [29 Aug/01 Sep 2012]: 
## 	add historical WCQCI trips (survey_code=15) & 1997 Ocean Selector (survey_code=16)
## 	add restratification subroutine and apply to 2006 WCQCI synoptic survey (as well as surveys 15 & 16)
## 	also changed logic for stratum label file: now a temporary file that doesn't need erasing
## [08 Sep 2012]: added code to tidy up analysis of historical WCQCI surveys
## [10 Sep 2012]: change to a `defaultdoorspread' option instead of 61.6 from Yamanaka et al. 1996
## [16 Nov 2012]: fix bug in getlabelends subroutine (needed to shave off front end of QCSndSyn labels)
## 	also some residual bugs in the two shrimp survey routines in calling the getlabels subroutine (did not
## 	save $stratumlabelfile for these two surveys)
## [17&18 Aug 2013]: add Nordic Pearl to vessel list//identify strata to drop in the NMFS Triennial survey 
## 	(instead of within gfsurvey.ado) with 'missing' (consistent with the Canadian surveys)
## [22 Aug 2013]: add 'depstrat' variable to QCSoundSynoptic survey//hardwire call to $stratumlabelfile 
## [20 Oct 2013]: NOTE TO PJS: consider switching doorspread replacement to use ONLY tows that are valid
## 	(ie: use==1|use==2|use==6, etc....)//also add ARCTIC OCEAN to the vessel list
## [28 Oct 2013]: 
## 	-add code to check for and process situations where there is more than one record per tow
## 	-drop hardwired rescaling of 2010 WCVI synoptic distance-travelled field (no longer needed)
## [03 & 05 Sep 2015]:
## 	-add code to <getlabels> subroutine which replaces -1 with '.' (missing).  Needed to add -1 to the
## 	 ACCESS output files due to limitations in converting from the decimal' numeric type in queries
## 	-also change the default <datemask> option to "dmy" to conform to export specifications in ACCESS
## 	-add code to force dropping of 2014 WCHG survey data (survey not completed and too deficient to use)
## 	-add option to allow dropping of pre-2003 shrimp survey data (convention adopted in GF Section)
## [28 Aug 2016]:
## 	-add trap to processing WCVI shrimp survey to catch situations when the depth fields for trip=67647 
## 	 haven't been properly updated
## 	-change logic on 'attendflag', 'useflag', and 'savefile': all optionally off now
## 	-generalise the collapse for multiple observations per tow to include the shrimp surveys
## [21 Nov 2016]: change default doorspread for the two shrimp surveys to 29.6 m
## [29 Nov 2016]: fix bug with log files: add 'cap log close' before every invocation of a log file
## [29 Nov 2018]: add water haul identification to the preparation of the Triennial survey data
## [23 Aug 2019]: RH converting PJS STATA code to R
## [01 Oct 2019]: RH renaming functions to align with PBS software convention: verbNoun
## */
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~PJS Notes


## calcBiom-----------------------------2022-05-19
##  Calculate swept-area biomass estimate and 
##  bootstrap within strata depending on 'meth':
##  If meth==0, strata comprise 'xvar' (usually 'year') only.
##  If meth==1, strata comprise year and survey stratum.
## -----------------------------------------PJS|RH
calcBiom = function(dat, reps=0, seed=42, meth=1, fix125flag, xvar="year", 
   areavar="area", stratumvar="stratum", towvar="set", postfilename)
{
	## syntax,  Density(string) Reps(int) SEed(int) Meth(int) FIX125flag(int) Xvar(string) /*
	## */ Areavar(string) STratumvar(string) Towvar(string) Postfilename(string) FILename(string)
	##  density units: kg/km^2;  area units: km^2 

	set.seed(seed)
	label = attributes(dat)$label
	## Use records with identifiable strata
	dat = dat[!is.na(dat[,stratumvar]),]
	
	## Check for missing areas (RH 220519)
	if (any(is.na(dat[,areavar]))){
		gcarea = GC$area; names(gcarea) = GC$group
		gcarea = gcarea[!is.na(gcarea)]
		noarea = is.na(dat[,areavar])
		dat[noarea,areavar] = gcarea[as.character(dat$group)][noarea]
		if (any(is.na(dat[,areavar]))) {  ## just remove them
			.flush.cat(paste0("Removing ", sum(is.na(dat[,areavar])), " record(s) with no group-area information.\n\n"))
			dat = dat[!noarea,]
			#stop (paste0("There are still missing areas (km^2) from '", substitute(dat), "'"))
		}
	}

	## Check for surveys with no catch data at all (it does happen!) 
	##   want to exit gracefully because we may be working on a batch situation
	if (sum(dat$density)==0)
		stop(paste0("Survey ", label$survey$number, " has no catch for this species.\nProgram terminated"))

	strata = .su(dat[,stratumvar])
	nstrat = length(strata)
	nnst   = nstrat + 1
	xvars  = .su(dat[,xvar])
	nxvar  = length(xvars)
	#egen byte `gg'=group(`xvar') ??

	if (reps==0)
		.flush.cat("Simple biomass calculation/no bootstrap","\n")
	else
		.flush.cat(paste0("Bootstrap biomass simulation: N reps=",reps), "\n")

	## initialize n-column matrix to hold number of tows in each year & `stratumvar'
	#smat = matrix(0, nrow=nxvar, ncol=nnst)
	smat = array(0, dim=c(nxvar,nnst), dimnames=list(xvars,c(strata,"T")))
	stab = crossTab(dat, c(xvar,stratumvar), xvar, countVec)
	smat[dimnames(stab)[[1]], dimnames(stab)[[2]]] = stab
	tvec = apply(stab,1,sum)
	smat[names(tvec),"T"] = tvec

	dat.need = dat[,c(xvar, stratumvar, areavar, "density")]
	ntows = table(dat.need[,xvar])
	ptows = crossTab(dat.need, xvar, "density", countVec)

	## from IPHC routines
	## When sim!="parametric", 'statistic' must take at least two arguments.
	## The first argument passed will always be the original data.
	## The second will be a vector of indices, frequencies or weights which define the bootstrap sample.
	## I becomes a random index but what is controlling it?
	sweptArea <- function(x, I){
		xx = x[I,]
		xstra = split(x[I,], x[I,stratumvar])
		out   = lapply(xstra, function(s){
			nobs = nrow(s)
			dens = mean(s$density, na.rm=TRUE)
			sdev = if (nobs<=1 || all(is.na(s$density))) 0 else sd(s$density, na.rm=TRUE)
			area = mean(s[,areavar], na.rm=TRUE)
			vari = (sdev^2 * area^2) / nobs
			vari[is.na(vari) | !is.finite(vari)] = 0
			biom = dens * area
			## summary stats
			bio = sum(biom)/1000.
			var = sum(vari)
			nn  = sum(nobs)
			return(data.frame(B=bio, V=var, N=nn))
			return(bio)
		})
		bvn = do.call("rbind", lapply(out, data.frame, stringsAsFactors=FALSE))
		BVN = apply(bvn,2,sum)
		if (reps>0)
			return(BVN["B"])
		else {
			SE  = sqrt(BVN["V"])/1000.
			CV  = SE/BVN["B"]
			CV[is.na(CV) | !is.finite(CV)] = 0
			return(c(B=as.vector(BVN["B"]), V=as.vector(BVN["V"]), SE=as.vector(SE), CV=as.vector(CV), N=as.vector(BVN["N"])))
		}
	}
	## Need to bootstrap annual indices separately when using boot::boot because strata assumes one population (as far as I can tell)
	booty = bootci = list()
	.flush.cat(paste0("Bootstrapping (", reps, " replicates):\n"))
	#xvars=c(2005,2007) ##testing
	for (i in xvars) {
		ii = as.character(i)
		.flush.cat(paste0(ii, ifelse(i==rev(xvars)[1], "\n\n", ", ")))
		idat = dat.need[is.element(dat.need[,xvar], i),]
		if (all(idat$density==0)) next
		if (meth==0)      idat$index = idat[,xvar]
		else if (meth==1) idat$index = paste(idat[,xvar], idat[,stratumvar], sep="-")
		else stop ("No stratify method available for meth>1")
		idat$index = as.factor(idat$index)
		
		if (reps==0) {
			booty[[ii]] = t(as.matrix(sweptArea(idat)))
		} else {
			## https://stats.stackexchange.com/questions/242404/how-does-the-boot-package-in-r-handle-collecting-bootstrap-samples-if-strata-are
			## with strata specified
			booty[[ii]]  <- boot::boot(idat, sweptArea, R=reps, strata=idat$index)
			bootci[[ii]] <- boot::boot.ci(booty[[ii]], type = "bca")
		}
	}
	if (reps==0) {
		analytic = do.call("rbind", lapply(booty, data.frame, stringsAsFactors=FALSE))
		return(list(analytic=analytic, extra=list(ntows=ntows, ptows=ptows)))
	} else {
		return(list(booty=booty, bootci=bootci, extra=list(ntows=ntows, ptows=ptows)))
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~calcBiom


## doSynoptic --------------------------2023-01-26
##  Get the synoptic data for specified survey and
##  go through PJS machinations.
##  Can be applied to non-synoptic surveys.
## -----------------------------------------PJS|RH
doSynoptic = function(dat, survey, logappendflag=TRUE)
{
	logfile = "surveyprepmessages.log"
	if (logappendflag) append=TRUE else FALSE
	
	#ppost1 = c("fe_id", "year", "set", "stratum", "lowbnd", "upbnd", "sdepth", "edepth", "seabird_depth", "using", "depth_problem_tows")
	#ppost2 = c("fe_id", "year", "set", "stratum", "lowdistance", "updistance", "distance", "distance_calc", "speed", "effort", "effort2", "using", "distance_problem_tows")
	uppdistance  = 2.4
	lowdistance  = 0.9

	ttget(stratum)
	vessels = getVessel(dat)

	## Subsetting in R loses attributes (very insane) so need to preserve and add back (sheesh)
	expr = expression (dat[,setdiff(colnames(dat), c("wing", "open"))])  ## drop trip wing open
	dat  = keepAtts(dat, expr)
	dat  = getUsability(dat)

	year    = .su(dat$year)
	nyear   = length(year)
	minyear = min(year)
	maxyear = max(year)

	ugroup  = .su(dat$grouping_code)
	ustrat  = attributes(stratum)$lstrat[as.character(ugroup)]
	Nstrat   = length(ustrat)
	colnames(dat)[grep("grouping_code", colnames(dat))] = "group"

	##Define 'group' based on 'stratum', which was originally 'grouping_code'
	dat$best_depth = coalesce(dat$sdepth, dat$edepth, dat$modal_depth, dat$seabird_depth)
	sgroup = stratum[ustrat]; names(sgroup) = names(ustrat)
	dat$stratum_group = rep(NA, nrow(dat))
	dat$stratum_group[!is.na(dat$group)] = sgroup[as.character(dat$group[!is.na(dat$group)])]

	dat$stratum_depth = rep(NA, nrow(dat))
	zdep = is.element(attributes(stratum)$gstrat,ugroup)
	for (i in 1:sum(zdep)) {
		z = dat$best_depth > as.numeric(attributes(stratum)$labelends$low[zdep][i]) & dat$best_depth <= as.numeric(attributes(stratum)$labelends$upp[zdep][i])
		dat$stratum_depth[z] = stratum[zdep][i]
	}
	dat$stratum = coalesce(dat$stratum_group, dat$stratum_depth)

	if (all(is.na(dat$door))) {
		.flush.cat(paste0(ttcall(surveyname), " has no doorspread values:\n\tusing ", ttcall(defaultdoorspread), " m as default value for all tows."), "\n")
		dat$door = ttcall(defaultdoorspread)
	}

	## Populate missing values of 'distance', 'door', and 'speed' with means
	## May eventually become a primary function if it's useful on a broader scale.
	## Added in loop to factor in vessels over the years (RH 220524)
	fixmiss = function(dat, flds) {
		uvess = .su(dat$vessel)
		for (v in uvess) {
			zv = is.element(dat$vessel,v)
			vdat = dat[zv,]
			for (i in flds) {
				## If missing values, use mean by year and group #stratum (formerly 'grouping_code')
				ibad = vdat[,i]<=0 | is.na(vdat[,i])
				if (all(ibad)) next
				if (any(ibad)) {
					itab = crossTab(vdat, c("year","group"), i, function(x){if (all(is.na(x))) NA else mean(x,na.rm=TRUE)})
					if (all(is.na(itab))) next
					barf = try(diag(itab[as.character(vdat$year[ibad]), as.character(vdat$group[ibad]), drop=FALSE]), silent=T)  ## (debug RH 230126)
					if(inherits( barf, "try-error" )) {
						imean = try(mean(itab, na.rm=TRUE))
						if (inherits(imean, "try-error")){
							browser(); return()
						}
					} else {
						imean = diag(itab[as.character(vdat$year[ibad]), as.character(vdat$group[ibad]), drop=FALSE])
					}
					vdat[,i][ibad] = imean
				} else next
				## If still missing values, use mean by year
				jbad = vdat[,i]<=0 | is.na(vdat[,i])
				if (all(jbad)) next
				if (any(jbad)) {
					jtab = crossTab(vdat, c("year"), i, function(x){if (all(is.na(x))) NA else mean(x,na.rm=TRUE)})
					vdat[,i][jbad] = jtab[as.character(vdat$year[jbad])]
				} else next
				## If still missing values, use overall mean
				kbad = vdat[,i]<=0 | is.na(vdat[,i])
				if (all(kbad)) next
				if (any(kbad)) {
					ktab =  mean(vdat[,i],na.rm=TRUE)
					vdat[,i][kbad] = ktab
				}
				dat[zv,i] = vdat[,i]  ## upload machinations for field i
			}
		}
		return(dat)
	}

	## Does not make sense to avergae distance and effort for non-standardised (synoptic) surveys
	if (survey %in% c("QCSsyn","WCVIsyn","WCHGsyn","HSsyn")) {  ## i.e. synoptic surveys
		dat = fixmiss(dat, c("door","distance","speed","effort"))
	} else {
		dat = fixmiss(dat, c("door","speed"))
	}

	## Check for inconsistencies in depth information
	## (only do this if the strata limits are consistent with what is in the file)
	#if (survey %in% c(2,3,9,10)) {  ## i.e. synoptic surveys
	#if (survey %in% c("QCSsyn","WCVIsyn","WCHGsyn","HSsyn")) {  ## i.e. synoptic surveys
	## Apply to all surveys:
		lodep = as.numeric(attributes(stratum)$labelends$low[zdep])
		updep = as.numeric(attributes(stratum)$labelends$upp[zdep])
		if (length(lodep)!=0 && length(updep)!=0) {
			names(lodep) = names(updep) = ugroup #stratum[zdep] #ustrat[!is.na(stratum)]
			dat$lodep    = lodep[as.character(dat$group)]
			dat$updep    = updep[as.character(dat$group)]
			seabird.dat  = dat[!is.na(dat$seabird_depth),]
			bad.depth    = seabird.dat$seabird_depth < seabird.dat$lodep | seabird.dat$seabird_depth > seabird.dat$updep
			if (any(bad.depth))
				write.csv(dat[bad.depth,c("fe_id", "year", "set", "stratum", "lodep", "updep", "sdepth", "edepth", "seabird_depth")], file=paste0(gsub(" ",".",ttcall(surveyname)),".depth.problems.csv"), row.names=FALSE)
		}
	#}

	## Calculate distance travelled
	dat$distance_calc = dat$speed * dat$effort2
	bad.dist = is.na(dat$distance_calc)
	dat$distance_calc[bad.dist] = dat$speed[bad.dist] * dat$effort[bad.dist]

	## Check for inconsistencies in distance information
	dat$lodist   = rep(lowdistance, nrow(dat))
	dat$updist   = rep(uppdistance, nrow(dat))
	bad.dist = (dat$distance_calc < dat$lodist | dat$distance_calc > dat$updist) & !is.na(dat$distance_calc)
	if (any(bad.dist))
		write.csv(dat[bad.dist,c("fe_id", "year", "set", "stratum", "lodist", "updist", "distance", "distance_calc", "speed", "effort", "effort2")], file=paste0(gsub(" ",".",ttcall(surveyname)),".distance.problems.csv"), row.names=FALSE)

	miss.dist = dat$distance<=0 | is.na(dat$distance)
	if (any(miss.dist)) {
		.flush.cat("Replacing missing distance travelled cells with calculated distance travelled","\n")
		pos.eff = dat$effort>0 & !is.na(dat$effort)
		pos.eff = rep(FALSE,length(pos.eff))  ## disable lm routine for now
		if (any(pos.eff)) {
			pdat    = dat[pos.eff,]
			lm.dist = lm(pdat$distance_calc ~ pdat$effort)  ## a bit tautological if lots of distances calculated as speed * effort above
			lm.dist = lm(pdat$distance ~ pdat$effort) 
			lm.coef = coefficients(lm.dist)
			zpred   = miss.dist & pos.eff
			dat$distance[zpred] = lm.coef[1] + lm.coef[2] * dat$effort[zpred]
		} else {
			dat$distance[miss.dist] = dat$distance_calc[miss.dist]
		}
	}
	## PJS averages speed over all fleets to calculate distance
	miss.speed = dat$speed<=0 | is.na(dat$speed)
	miss.dist  = dat$distance<=0 | is.na(dat$distance)
	if (any(miss.speed) && !all(miss.speed) && any(miss.dist)) {
		.flush.cat("Replacing missing speed with fleetwide average and populate missing distance (speed*effort)","\n")
		mean.speed  = mean(dat$speed[!miss.speed], na.rm=TRUE)
		dat$speed[miss.speed] = mean.speed
		best.effort = coalesce(dat$effort, dat$effort2)
		miss.effort = best.effort<=0 | is.na(best.effort)
		dist.effort   = miss.dist & !miss.effort
		dat$distance[dist.effort] = dat$speed[dist.effort] * dat$effort[dist.effort]
	}
	## Call this routine to ensure that there is only one observation per tow
	#if (survey %in% c(2,3,9,10))  ## i.e. synoptic surveys
	if (survey %in% c("QCSsyn","WCVIsyn","WCHGsyn","HSsyn"))  ## i.e. synoptic surveys
		dat = uniqtows(dat)

	dat$density = dat$weight / (dat$distance * dat$door/1000.)
	dat$density[is.na(dat$density)] = 0

	dat$best_depth = coalesce(dat$seabird_depth, dat$sdepth, dat$edepth)

	## Change a few field names
	colnames(dat)[grep("usability",colnames(dat))]      = "use"
	colnames(dat)[grep("dfo_stat",colnames(dat))]       = "dfo"
	colnames(dat)[grep("seabird_depth",colnames(dat))]  = "seabird"

	ttput(dat)
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~doSynoptic


## getLabels----------------------------2022-05-19
##  Read in data file and get labels.
## -----------------------------------------PJS|RH
getLabels = function(file)
{
	dat = read.csv(file) ## insheet using `file'
	## Process strata for GIG historical (because data have no grouping codes)
	if (grepl("SSID=21",file)) {
		dat = dat[dat$year<=1995 & !is.na(dat$year),]
		gcode = c(185, 186, 187) ## in GFBioSQL's GROUPING table
		lodep = c(120,183,218)
		updep = c(183,218,300)
		areas = c(2122,1199,1746)
		ndeps = length(lodep)
		GCloc = GC
		dat$GROUPING_CODE = dat$area = rep(NA, nrow(dat))  ## restratify all surveys
		for (i in 1:ndeps) {
			z = is.na(dat$GROUPING_CODE) & dat$begin_depth > lodep[i] & dat$begin_depth <= updep[i]
			dat$GROUPING_CODE[z] = gcode[i]
			dat$area[z] = areas[i]
			#GCloc = rbind(GCloc, c(gcode[i], paste0("H",i), round(lodep[i]), round(updep[i]), areas[i]))
		}
		#assign("GC", GCloc, envir=.GlobalEnv)
		dat = dat[!is.na(dat$GROUPING_CODE),]
	}
	
	## Get the strata and grouping codes directly without having to be specific in various other functions (RH 220518)
	ugroup = .su(dat$GROUPING_CODE)
	udeps  = GC[is.element(GC$group,ugroup),c("stratum","mindep","maxdep"),drop=F]; rownames(udeps) = ugroup
	lstrat = apply(udeps,1,function(x){paste0(x[1],":",as.numeric(x[2]),"-",as.numeric(x[3]),"m")})
	ustrat = GC[is.element(GC$group,ugroup),"stratum"]; names(ustrat) = lstrat
	nstrat = length(ustrat)
	gstrat = as.numeric(names(lstrat)); names(gstrat) = lstrat
	stratum = ustrat
	attr(stratum,"gstrat") = gstrat
	attr(stratum,"lstrat") = lstrat
	attr(stratum,"labelends") = getLabelends(labelname=names(stratum))
	ttput(stratum)

	flds = colnames(dat)
	flds[grep("distance_travelled", flds)] = "distance" ## ren distance_travelled distance
	flds[grep("begin_depth", flds)]   = "sdepth"        ## ren begin_depth sdepth
	flds[grep("end_depth", flds)]   = "edepth"          ## ren end_depth edepth
	flds[grep("^time$", flds)]  = "effort"              ## ren time effort (retrieval - deployment)
	flds[grep("^time2$", flds)] = "effort2"             ## ren time2 effort2 (end bottom contact - start bottom contact)
	flds = tolower(flds)
	colnames(dat) = flds
	dat$species = getSpecies(dat$species)
	dat$month   = getMonth(dat$date)
#	run $stratumlabelfilename  ## what does this do?

	if (mean(dat$longitude,na.rm=TRUE) > 0)
		dat$longitude = -dat$longitude

	## Initialize the label list
	#label = list(survey=list(number=ttcall(survey), name=ttcall(surveyname), file=file))
	label = list(survey=list(code=ttcall(survey), name=ttcall(surveyname), file=file))

	label[["fields"]] = list()
	label[["fields"]][["effort"]]   = "[Begin_retrieval]-[End_deployment] (hrs)"
	label[["fields"]][["effort2"]]  = "[End_bottom contact]-[Begin_bottom contact] (hrs)"
	label[["fields"]][["door"]]     = "Doorspread (m)"
	label[["fields"]][["speed"]]    = "Speed (km/h)"
	label[["fields"]][["sdepth"]]   = "Depth at beginning of tow (m)"
	label[["fields"]][["edepth"]]   = "Depth at end of tow (m)"
	label[["fields"]][["distance"]] = "Distance travelled (km)"

	label[["reason"]]  = list(describe="Survey purpose")
	getData("REASON", "GFBioSQL")
	label[["reason"]][["codes"]] = PBSdat
	attr(dat, "label") = label
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~getLabels


## getLabelends-------------------------2019-07-23
##  Get depth ranges from stratum names (methinks)
## -----------------------------------------PJS|RH
getLabelends = function (labelname)
{
	pos1 = pmax(regexpr(":",labelname),0)
	pos2 = regexpr("-",labelname)
	pos3 = regexpr("( ?f?)m$",labelname)
	lows = substring(labelname, pos1+1, pos2-1)
	upps = substring(labelname, pos2+1, pos3-1)
	nstrat = length(labelname)
#browser();return()
	return(list(low=lows, upp=upps, nstrat=nstrat))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~getLabelends


## getMonth-----------------------------2019-07-30
##  Extract month from dates vector, where
##  type 1 = numeric month
##       2 = month abbreviation
##       3 = month name
##       4 = two-character string
## -----------------------------------------PJS|RH
getMonth = function(dates, type=1)
{
	z = dates!="" & !is.na(dates)
	mos = rep(ifelse(type%in%c(1), NA, ""),length(dates))
	if (all(class(dates[z])=="character")) {
		strmos = substring(dates[z], 6, 7)
		nummos = as.numeric(strmos)
		mos[z] = switch(type,
			nummos,
			month.abb[nummos],
			month.name[nummos],
			strmos
		)
	} else
		stop("The 'date' field is not class 'character', revise function 'getMonth' to deal with date classes.")
#browser();return()
	#mos[z] = month.abb[as.numeric(strftime(as.Date(dates[z], "%d/%m/%Y"),"%m"))]
	return(mos)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~getMonth


## getSpecies---------------------------2019-07-17
##  Get species codes from data
##  (also checks for more than one species)
## -----------------------------------------PJS|RH
getSpecies = function(species)
{
	#ici = lenv() ## RH 200716
	#tempfile = paste0(convSlashes(tempdir(),"unix"),"/tmpsurvey.txt")
	N = countVec(species)  ## count if species~=. 
	if (N>0) {
#		save `tmpsurvey' ## wtf?
		if (length(.su(species)) > 1)
			stop ("More than one species code in the file\n\texiting program...")
		#data("spn", package="PBSdata", envir=ici) ## RH 200716
		z = !is.na(species)
		species[z] = pad0(species[z], 3)
		#species[z] = spn[as.character(species[z])]
#		collapse (count) set if species~=., by(species)	
#		tab species
#		if r(r)>1 {
#			di in red "more than one species code in the file" _n "exit program" 
#			exit 998
#		}
#		else {
#			ren species sp
#			merge 1:1 sp using E:\ADO\Groundfish\species_name.dta
#			keep if _m==3
#			global speciesname=common_name[1]
#			global species=sp[1]
#		}
#		use `tmpsurvey', clear
#	} /* if r(N)>0 */
	}
	return(species)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~getSpecies


## getUsability-------------------------2019-08-02
##  Qualify data by usability code.
## -----------------------------------------PJS|RH
getUsability = function(dat, use=c(0,1,2,6))
{
	Nrows = nrow(dat)
	if ("usability" %in% colnames(dat)) {
		useflag = TRUE
		nuse0 = is.na(dat$usability)
		dat$usability[nuse0] = 0
		dat  = dat[is.element(dat$usability, use),]
		nrows = nrow(dat)
	} else
		useflag = FALSE

	label = attributes(dat)$label
#browser();return()
	if (is.null(label)) label = list()
	if (!is.element("usability",names(label))) {
		label[["usability"]] = list()
		label[["usability"]][["describe"]] = "Records' usability for different purposes"
		getData("USABILITY", "GFBioSQL")
		label[["usability"]][["codes"]] = PBSdat
	}
	if (useflag) {
		label[["usability"]][["note"]] = paste0("Of the original ", Nrows, " records, ", sum(nuse0), " were converted to 0 from NA, and ", Nrows-nrows, " were removed after filtering.")
		label[["usability"]][["Nrows"]] = Nrows
		label[["usability"]][["nrows"]] = nrows
		label[["usability"]][["use"]] = use
	} else
		label[["usability"]][["note"]] = paste0("Variable 'usability' not present in this object; therefore,  all ", Nrows, " survey fishing events will be used for analysis without regard to their utility.")
	attr(dat, "label") = label
#browser();return()
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~getUsability


## getVessel----------------------------2019-08-02
##  Get survey vessel codes.
## -----------------------------------------PJS|RH
getVessel = function(dat)
{
	vessel = 1:20
	names(vessel) = c(
		"G. B. REED", 
		"SOUTHWARD HO",
		"EASTWARD HO",
		"OCEAN SELECTOR",
		"FROSTI",
		"VIKING STORM",
		"W. E. RICKER",
		"CHALLENGER",
		"DELIVERANCE",
		"OCEAN KING",
		"PACIFIC TRIDENT",
		"SHARLENE K",
		"SUNNFJORD",
		"CALEDONIAN",
		"BLUE WATERS",
		"SCOTIA BAY",
		"FREE ENTERPRISE NO 1",
		"NEMESIS",
		"NORDIC PEARL",
		"ARCTIC OCEAN"
	)
	label = attributes(dat)$label
	if (is.null(label)) label = list()

	if (!is.element("vessel",names(label))) {
		label[["vessel"]] = list()
		label[["vessel"]][["describe"]] = "PJS vessel codes"
		label[["vessel"]][["codes"]] = vessel
	}
	if ("vessel" %in% colnames(dat)) {
		vv = vessel[dat$vessel]
		if ( any(is.na(vv)) ) {
			vvdiff = setdiff(.su(dat$vessel),names(vessel))
			if (length(vvdiff)>0) {
				addvessel = max(vessel) + 1:length(vvdiff)
				names(addvessel) = vvdiff
				vessel = c(vessel,addvessel)
				vv = vessel[dat$vessel]
				label[["vessel"]][["codes"]] = vessel
				label[["vessel"]][["note"]] = paste0("Added vessels not in PJS list of 20 vessel codes: ", paste0(paste(addvessel,names(addvessel),sep=" = "),collapse="; "))
			}
			vv[is.na(vv)] = 0
		}
		dat$vessel = vv
	} else
		label[["vessel"]][["note"]] = paste0("Variable 'vessel' not present in this object; therefore, vessels, if any, were not converted to codes.")
	attr(dat, "label") = label
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~getVessel


## keepAtts ----------------------------2019-07-23
##  Keep user's attributes after performing an
##  expression such as subsetting a file.
## ---------------------------------------------RH
keepAtts =function(dat, expr, batts=c("names","row.names","class"), extras=list())
{
	unpackList(extras)
	keepatts = attributes(dat)
	keepatts = keepatts[setdiff(names(keepatts), batts)]
	loco = lenv()
#browser();return()
	dat = eval(expr, envir=loco)
	attributes(dat) = c(attributes(dat), keepatts)
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~keepAtts


## plotIndex----------------------------2023-02-09
##  Plot survey index series after bootstrapping.
##  If type=="PJS", plot the series as mean with
##   bias-corrected percentile limits.
## If type=="RH", plot quantile boxes using 
##   0.05, 0.25, 0.5, 0.75, and 0.95 quantiles.
## Now using results from 'boot::boot' & 'boot::boot.ci' (RH 190821)
## -----------------------------------------PJS|RH
plotIndex = function(bootbomb, analytic, type="PJS", surv=ttcall(surveyname),
   png=FALSE, pngres=400, PIN=c(8,7), lang=c("e","f"))
{
	createFdir(lang)
	unpackList(bootbomb)
	reps     = booty[[1]]$R
	years    = as.numeric(names(booty)); 
	nyrs     = length(years)
	yrs.surv = as.numeric(names(extra$ntows))
	nyrs.surv= length(yrs.surv)
	Bboot    = array(NA, dim=c(reps, nyrs), dimnames=list(rep=1:reps, year=years))
	index    = rep(NA,nyrs); names(index)=years
	yCL      = array(NA, dim=c(2, nyrs), dimnames=list(c("lower","upper"), year=years))
	for (i in names(booty)) {
		Bboot[,i] = booty[[i]]$t[,1]
		index[i]  = booty[[i]]$t0
		yCL[,i]   = bootci[[i]]$bca[4:5]
	}
	Qboot = as.list(rep(NA,length(min(years):max(years)))); names(Qboot)=min(years):max(years)
	qboot = lapply(as.data.frame(Bboot),function(x){x})
	Qboot[names(qboot)] = qboot
	
	## Make a table for output
	bbtab = data.frame(year=years, B_anal=index, B_boot=sapply(qboot,mean), B_lower=yCL["lower",], B_upper=yCL["upper",], CV_boot=sapply(qboot,function(x){sd(x)/mean(x)}))
	if (!missing(analytic))
		bbtab = data.frame(bbtab, CV_anal=analytic$analytic[,"CV"])
	write.csv(bbtab, file=paste0("bio.est.tab.", ttcall(survey),".csv"), row.names=FALSE)

	ylim  = c(0,max(yCL))
	ylim[1] = ylim[1]-(diff(ylim)*ifelse(png,0.02,0.01))
#browser();return()
	ayrs  = yrs.surv[1]:rev(yrs.surv)[1]; nayrs = length(ayrs)
	fout = fout.e = paste0(gsub(" ","_",surv),"-",type)
	for (l in lang) {  ## could switch to other languages if available in 'linguaFranca'.
		changeLangOpts(L=l)
		fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
		if (png) png(filename=paste0(fout,".png"), units="in", res=pngres, width=PIN[1], height=PIN[2])
		expandGraph(mar=c(3,3,0.5,0.5), oma=c(0,0,0,0))

		flab = paste0(c(surv, paste0(
			switch(l, 'e'="Mean +ve events: ", 'f'=eval(parse(text=deparse("moyen \u{00E9}v\u{00E9}nements positifs : "))) ),
			round(mean(extra$ptows)), "/", round(mean(extra$ntows))) ), collapse="\n")

		if (type=="PJS") {
			## estimate bias in std. norm deviates
			## https://influentialpoints.com/Training/bootstrap_confidence_intervals.htm#bias
			#yCL = sapply(1:ncol(Bboot), function(i,B,alpha=0.05){
			#	boot  = B[,i]; obs = index[i]; N = nrow(B)
			#	b     = qnorm((sum(boot > obs) + sum(boot==obs)/2) / N)
			#	alpha = 0.05 # 95% limits
			#	z     = qnorm(c(alpha/2, 1-alpha/2)) # Std. norm. limits
			#	p     = pnorm(z-2*b) ## bias-correct & convert to proportions
			#	iCL   = quantile(boot,p=p) # Bias-corrected percentile limits
			#	return(iCL)
			#}, B=Bboot)
			#colnames(yCL) = colnames(Bboot)
			#yCL  = apply(Bboot[-1,],2,quantile,c(0.025,0.975))  ## 95% confidence interval
			xseg = as.vector(sapply(split(years,years),function(x){c(x,x,NA)}))
			yseg = as.vector(rbind(yCL,rep(NA,length(years))))
			staple.width = 0.2
			xstp = as.vector(sapply(split(years,years),function(x,s){c(x-s,x+s,NA,x-s,x+s,NA)}, s=staple.width/2))
			ystp = as.vector(rbind(yCL[c(1,1),], rep(NA,length(years)), yCL[c(2,2),], rep(NA,length(years)) ))
			plot(years, index, type="n", xlim=extendrange(ayrs), ylim=ylim, xlab=linguaFranca("Year",l), ylab=linguaFranca("Relative biomass (t)",l), cex.axis=1.2, cex.lab=1.5)
			axis(1, at=seq(1900, 2100, ifelse(nayrs>30, 5, 1)), tcl=-0.2, labels=FALSE)
			axis(2, at=pretty(ylim, n=15), tcl=-0.2, labels=FALSE)
			lines(xseg,yseg,lwd=3,col="black")
			lines(xstp,ystp,lwd=3,col="black")
			points(years, index, pch=15, col="red", cex=1.2)
			if (length(setdiff(yrs.surv,years))>0){
				yrs.zero = setdiff(yrs.surv,years)
				points(yrs.zero, rep(0,length(yrs.zero)), pch="\327", col="red", cex=1.5)
			}
			text(yrs.surv, rep(par()$usr[3],nyrs.surv), paste0(extra$ptows,"/",extra$ntows), cex=0.8, col="blue", pos=3)
#browser();return()
			#text(years, rep(par()$usr[3],nyrs), extra$ptows, cex=0.8, col="blue", pos=3)
			#text(years, rep(par()$usr[3],nyrs), extra$ptows, cex=0.8, col="blue", adj=c(0.5,-0.2))
			if (sum(index[1:2]) > sum(rev(index)[1:2]))
				addLabel(0.95,0.95, txt=linguaFranca(flab,l), adj=c(1,1), cex=1.2, col="grey30")
			else
				addLabel(0.05,0.95, txt=linguaFranca(flab,l), adj=c(0,1), cex=1.2, col="grey30")
		} else {
			quantBox(Qboot, ylim=ylim, xlab=linguaFranca("Year",l), ylab=linguaFranca("Relative biomass (t)",l), boxwex=0.6, boxfill="aliceblue", whisklty=1, outpch=43, outcex=0.8, outcol="grey60", cex.lab=1.5)
			xpos  = 1:length(Qboot); names(xpos)=names(Qboot)
			Bmean = sapply(Qboot, mean, na.rm=TRUE)
			points(xpos, Bmean, pch=22, col=lucent("blue",1), bg=lucent("cyan",1), cex=1.5)
			points( xpos[as.character(years)], Bboot[1,], pch=15, col="blue", cex=0.8)
		}
		if (png) dev.off()
	}; eop()
	invisible(return(Qboot))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plotIndex


## prepGFsurv---------------------------2022-05-19
##  Prepare GF surveys using PJS codes.
## -----------------------------------------PJS|RH
prepGFsurv = function(file, survey, strSpp, datemask, doorspread=61.6,
   shrimpyear=1975, savefile=TRUE, usevar=FALSE, attended=FALSE,
   spath="C:/Users/haighr/Files/Projects/R/Develop/PBStools/Authors/SQLcode")
{
	if (!file.exists("GC.rda")) {
		getData("SELECT G.GROUPING_CODE AS 'group', ISNULL(G.GROUPING_SPATIAL_ID,'') + ISNULL(G.GROUPING_DEPTH_ID,'') AS 'stratum', G.MIN_DEPTH_M AS 'mindep', G.MAX_DEPTH_M AS 'maxdep', G.AREA_KM2 AS 'area' FROM GROUPING G WHERE G.MIN_DEPTH_M IS NOT NULL AND G.MAX_DEPTH_M IS NOT NULL", dbName="GFBioSQL", type="SQLX")
		GC = PBSdat
		save("GC", file="GC.rda")
	} else {
		load("GC.rda", envir=.GlobalEnv)
	}
	ici = lenv() ## RH 200716
	#syntax, File(string) SUrvey(int) [DAtemask(string) DOorspread(real 61.6) SHrimpyear(int 1975)] /*

	if (missing(file))
		stop("Supply a CSV file name (e.g. 'SSID=16&species=435.csv') saved form a call to 'gfb_survey_data.sql'")
	if (missing(survey))
		stop("Supply a survey code.\n")
	#if (missing(survey))
	#	stop(paste0("Supply a PJS survey number from:\n",
	#"   1: old Hecate St Survey\n",
	#"      (note: this program does not prepare the recruited biomass estimate as done for ENL in 2006)\n",
	#"   2: QC Sound Synoptic\n",
	#"   3: WCVI Synoptic\n",
	#"   4: WCVI Shrimp\n",
	#"   5: NMFS Triennial\n",
	#"   6: Pcod Monitoring\n",
	#"   7: QCSound Shrimp\n",
	#"   8: historical GB Reed survey, now called GIG Historical\n", 
	#"      (includes 1984 Eastward Ho and 1990s Ocean Selector and Frosti -- use this for biomass ests)\n",
	#"   9: WCQCI Synoptic (added Nov 2008), now call WCHG (west coast Haida Gwaii)\n",
	#"  10: Hecate St synoptic survey (added July 2010)\n",
	#"  11: retrospective GIG (includes GB Reed and misc GIG surveys)\n",
	#"      (added Nov 2009 -- not for biomass)\n",
	#"  12: retrospective GIG\n",
	#"      (1995 Ocean Selector and Frosti only -- QC Snd synoptic stratification) (added Dec 2009)"
	#))
	#maxsurvey = 16
	#if (survey < 1 || survey > maxsurvey) {
	#	stop("You have selected an invalid survey code\nProgram ended")
	#}
	if (!missing(strSpp) && !grepl(strSpp,file))
		.flush.cat(paste0("WARNING -- string supplied by strSpp='", strSpp, "' does not appear in file name: '", file, "'."), "\n")
	if ( grepl("species=",file) ) {  ## overide strSpp if this condition is true
			strSpp = substring(file,regexpr("species=",file)+8, regexpr("species=",file)+10)  ## assume PJS standard naming
			.flush.cat(paste0("Argument 'strSpp' set to '", strSpp, "' based on input file name."), "\n\n")
	}
	if (missing(strSpp))
		stop("\n\n!!!!! String species neither supplied by user nor identified in file name.\n\tSupply species HART code to 'strSpp'\n\n")

	data(spn, package="PBSdata", envir=ici) ## RH 200716
	speciesname = spn[strSpp]
	ttput(speciesname)

	#gfbsurvey = c(2, 1, 4, 7, 79, 5, 6, 32, 16, 3, 21, 33); names(gfbsurvey) = 1:12
	gfbsurvey = list('QCSsyn'=1, 'HSass'=2, 'HSsyn'=3, 'WCVIsyn'=c(4,9), 'HSpac'=5, 'QCSshr'=6, 'WCVIshr'=7, 'WCHGsyn'=c(8,16),
		'WCVIlst'=11, 'IPHCll'=14, 'GIGhis'=21, 'HBLLn'=22, 'HBLLs'=36, 'SBFtrap'=42, 'SOGsyn'=45, 'NMFStri'=79)
	ttput(gfbsurvey)

	synflds = c("year", "month", "date", "vessel", "set", "fe_id", "group", "stratum", "major", "dfo", "area", "speed", "effort", "effort2", "distance", "distance_calc", "latitude", "longitude", "sdepth", "edepth", "seabird", "best_depth", "door", "reason", "use", "weight", "number", "density")
	ttput(synflds) ## for use in the various 'prep' survey functions

	assflds = c("year", "month", "date", "vessel", "trip", "set", "group", "stratum", "major", "dfo", "area", "speed", "effort", "distance", "distance_calc", "latitude", "longitude", "sdepth", "edepth", "seabird", "best_depth", "door", "reason", "use", "weight", "number", "cpue", "density")
	ttput(assflds) ## for use in the 'prepHSass' survey function

	triflds = c("year", "month", "date", "vessel", "trip", "set", "group", "stratCA", "stratUS", "stratTT", "areaCA", "areaUS", "areaTT", "speed", "effort", "distance", "latitude", "longitude", "best_depth", "door", "use", "weight", "number", "cpue", "density", "canada")
	ttput(triflds) ## for use in the 'prepNMFStri' survey function

	gigflds = c("year", "month", "date", "vessel", "set", "stratum", "depstrat", "group", "dfo", "area", "speed", "effort", "distance", "latitude", "longitude", "sdepth", "edepth", "door", "reason", "use", "weight", "number", "density")
	ttput(gigflds) ## for use in the 'prepGIGhis' survey function

	fm_to_m   = 1.8288; ttput(fm_to_m)   ## conversion from fathoms to metres
	kn_to_km  = 1.853248                 ## conversion from nautical miles to kilometres
	ft_to_m   = 0.3048                   ## feet to metres
	defaultdoorspread = doorspread; ttput(defaultdoorspread)
	shrimpdropyear    = shrimpyear
	defaultshrimpdoorspread = 29.6/1000  ## in km: to conform with Norm Olsen "best practice"
	#else survey=`survey'
	#drop _all
	#label drop _all

	#filename = as.character(substitute(file))
	if (!file.exists(file)) {
		getData("gfb_survey_data.sql",dbName="GFBioSQL",strSpp=strSpp, survserid=gfbsurvey[[survey]], path=spath)
		write.csv(PBSdat, file=file, row.names=FALSE)
	}

	if (missing(datemask)) datemask = "dmy"
	#else global datemask "`datemask'"
	if (usevar) useflag = 1
	else        useflag = 0
	if (attended) attendflag = ""
	else          attendflag = "notattended"

	#*di in ye "attended: `attended' global attendflag: $attendflag"  ## display?
	.flush.cat(paste0("attended: ", attended, "  attendflag: ", attendflag),"\n")
	
	fnsurv = survey #switch(survey,
		#"HSass",  "QCSsyn", "WCVIsyn", "WCVIshr", "NMFStri",
		#"PACmon", "QCSshr", "GIGhis",  "WCHGsyn", "HSsyn",
		#"GIGhis", "QCSsyn", "1996caledonian", "gbreedcvi", "WQCIhis",
		#"oceanselector")

	#mess = paste0("source(\"prep", fnsurv, ".r\"); dat = prep", fnsurv, "(file=\"", file, "\", survey=", survey, ")")
	mess = paste0("dat = prep", fnsurv, "(file=\"", file, "\", survey=\"", survey, "\")")
	eval(parse(text=mess))

	#qui compress ## qui = quietly
	if (savefile) {
		.flush.cat("Saving survey data file\n")
		mess = paste0(fnsurv, "=dat; save(\"", fnsurv, "\", file=\"", fnsurv, ".rda\")")
		eval(parse(text=mess))
	}
	else .flush.cat("Saving survey data file suppressed\n")
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~prepGFsurv


## prepGIGhis---------------------------2022-05-19
##  Prepare GIG Historical survey data.
##  survey==8  -- using Norm Olsen's grouping codes
##  survey==11 -- using Paul Starr's strata
## -----------------------------------------PJS|RH
prepGIGhis = function(file, survey="GIGhis")
{
	surveyname = "GIG Historical"
	ttput(surveyname)
	ttput(survey)
	ttget(gfbsurvey)

	dat    = getLabels(file)
	ttget(stratum)
	label  = attributes(dat)$label ## keep temporarily just to be safe

	dat   = doSynoptic(dat, survey) ## even though it's not synoptic, function has some standardising routines

	#if (survey==8) { ## use Norm Olsen grouping codes when $survey==8 
	if (all(gfbsurvey[[survey]]==8)) { ## use Norm Olsen grouping codes when $survey==8 
		expr = expression(dat[!is.element(dat$reason,9),]) ## drop 19 tows in 1994 specified as target strength acoustic tows
		dat  = keepAtts(dat, expr)
		#dat$stratum[is.element(dat$reason,9)] = NA  ## drop 19 tows in 1994 specified as target strength acoustic tows
	}
	#else if (survey==11) {  ## define PJS strata for $survey==11: GIG lies within this latitude range
	else if (all(gfbsurvey[[survey]]==21)) {  ## define PJS strata for $survey==11: GIG lies within this latitude range
		#dat$stratum[dat$latitude>=50.9 & dat$latitude<=51.6 & !is.na(dat$latitude)] = 1
		expr = expression(dat[dat$latitude>=50.9 & dat$latitude<=51.6 & !is.na(dat$latitude),])
		dat  = keepAtts(dat, expr)
		#dat$stratum[is.element(dat$year, c(1965,1966,1979))] = NA
		## 1965 & 1966: wide-ranging exploratory; 1979: different design spatially; 1995: random instead of fixed stations
		expr = expression(dat[!is.element(dat$year, c(1965,1966,1979,1995)),])
		dat  = keepAtts(dat, expr)
		#dat$stratum[is.element(dat$reason,9)] = NA                ## drop 19 tows in 1994 specified as target strength acoustic tows
		expr = expression(dat[!is.element(dat$reason,9),]) ## drop 19 tows in 1994 specified as target strength acoustic tows
		dat  = keepAtts(dat, expr)
		#expr = expression(dat[dat$year>2002 & !is.na(dat$year),]) ## drop recent QC Snd synoptic survey Viking Storm tows
		#dat  = keepAtts(dat, expr)
		#dat$stratum2 = rep(0,nrow(dat))
		#dat$stratum2[dat$latitude>=50.9 & dat$latitude<=51.6  & !is.na(dat$latitude)] =1
		## lab var stratum2 "GIG flag: all surveys"
	}

	## The following code has already been dealt with by subfunction 'fixmiss' in 'doSynoptic'
	## For GIG Historical, effort and distance are better represented than door and speed; therefore, just fix the latter two.
	note = list()
	note[["author"]] = "Paul J. Starr"
	#note[["speed_old"]] = c(
	#	"This field is the original speed information: only one year with a mean>11 km/h.",
	#	"Provisionally not used because seems very high and not credible." )
	#dat$door  = ttcall(defaultdoorspread)  ## 61.6 m  vs. dat's 14.3 m
	#dat$speed = rep(NA, nrow(dat))
	#dat$speed = dat$distance / dat$effort

	#dat$density = dat$weight / (dat$distance * dat$door/1000.)
	#dat$density[is.na(dat$density)] = 0

	## Restratify using depint
	#lodep = as.numeric(attributes(depint)$labelends$low)
	#updep = as.numeric(attributes(depint)$labelends$upp)
	#dat$depstrat = sapply(dat$sdepth, function(mm){ zz = mm >= lodep & mm < updep; if(!any(zz)) NA else (1:length(depint))[zz] },simplify=TRUE)
	#dat$depint   = sapply(dat$sdepth, function(mm){ zz = mm >= lodep & mm < updep; if(!any(zz)) NA else depint[zz] },simplify=TRUE)
	### Get rid of the bad usability codes (e.g.9)  and non-GIG tows
	#badstrat = is.na(dat$stratum)
	#dat$depstrat[badstrat] = dat$depint[badstrat] = NA

	#if (survey==11) {
	#if (all(gfbsurvey[[survey]]==21)) {
	#	names(stratum) = c("Valid GIG tow", "", "")
	#	stratum2 = 0:1
	#	names(stratum2) = c("Outside GIG", "GIG tow")
	#}
	if (all(gfbsurvey[[survey]]==8)) {
		note[["GIG survey 8"]] =c(
			"This is a file of the Historical GIG survey data in Queen Charlotte Sound from 'minyear' to 'maxyear'.",
			"All 'ntows' tows in this file are deemed GIG tows within the correct latitude and depth range ('nvess' vessels in this file)"
		)
		note[["stratum"]] = "Generated from grouping code (stratum code generated by Norm Olsen for 'ntows' records in Goose Island Gully: 1967-1995 surveys only)"
		note[["area"]] = "Provided by N Olsen (Dec 2009) for strata 1 to 3, bounded by depth, for the three grouping codes."
	}
	else if (all(gfbsurvey[[survey]]==21)) {
		note[["GIG survey 11"]] =c(
			"This is a file of the Historical GIG GB Reed survey data in Queen Charlotte Sound from 'minyear' to 'maxyear'.",
			"Contains all tows from 'nvess' vessels that have surveyed in QC Sound ('ntows' tows).",
			"Viking Storm tows from the QC Sound Synoptic survey have been dropped."
		)
		note[["stratum"]]  = "set ==1 if lies between 50.9 and 51.6 with a valid usability code (0/1/2/6)."
		note[["stratum2"]] = "GIG flag for all surveys (ignore useflag and bad survey years)."
	}
	note[["catch"]] = paste0("The only catch data are for ", ttcall(speciesname), ".")
	note[["door"]]  = paste0("Assumed value (used ", ttcall(defaultdoorspread), "m = from Yamanaka et al. 1996 Can MS Report 2362)")
	note[["speed"]] = c(
		"Generated from small number of credible 'distance_travelled' records:",
		"speed = distance/time when present,",
		"speed = mean(speed) when missing."
		)
	note[["distance"]] = c(
		paste0("There were ", nrow(dat)-countVec(dat$distance_old), " records where distance==0."),
		"These were changed to mean values by year and stratum using subfunction 'fixmiss' in 'doSynoptic' before all calculations.",
		"Originally PJS calculated field: gen distance = effort*speed if distance==.",
		"Replace missing calculated distance records (ie when time==.) with mean distance for the year."
		)
	note[["depint"]] = c(
		"gen int depint=recode(sdepth, 146,183,219,256,292,329,366,402,440) ",
		"[this preserves 20 fathom intervals from 80 to 240 fathoms and then a catchall from 240 to deepest]"
		)

	## Choose fields for data return
	if (!is.null(ttcall(gigflds))){
		ttget(gigflds)
		outflds = intersect(gigflds, colnames(dat))
	} else {
		.flush.cat("All data fields will be returned.\n\tRun 'prepGFsurv' to get a standard subset of fields.\n\n")
		outflds = colnames(dat)
	}
	expr = expression(dat[,outflds])
	dat  = keepAtts(dat, expr, extras=list(outflds=outflds))

	attr(dat,"label") = label
	attr(dat,"note")  = note
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~prepGIGhis


## prepHSass----------------------------2022-05-17
##  Prepare Hecate Strait assemblage survey data.
## -----------------------------------------PJS|RH
prepHSass = function(file, survey="HSass")
{
	surveyname = "Hecate Strait Assemblage"
	ttput(surveyname)
	ttput(survey)

	stratum = 1:7
	names(stratum) = c("10-19 fm", "20-29 fm", "30-39 fm", "40-49 fm", "50-59 fm", "60-69 fm", "70-79 fm")
	attr(stratum,"labelends") = getLabelends(labelname=names(stratum))
	ttput(stratum)

	dat   = doSynoptic(dat, survey) ## even though it's not synoptic, function has some standardising routines
	label = attributes(dat)$label    ## add later as there is a lot of fiddly manipulation that follows

	## Note: 1984 had two trips operating.  This resets the set numbers and means
	##   that only year and set will determine all the records
	zset = is.element(dat$year,1984) & is.element(dat$trip,38645)
	dat$set[zset] = dat$set[zset] + 89
	dat  = dat[,setdiff(colnames(dat), c("species", "effort2", "wing", "open"))] #"door"
	dat  = dat[dat$year<=2003 & !is.na(dat$year),]

	## Restratify (note stratum boundaries are in metres: 10 fathoms=18.293 m; 20 f=36.586 m; etc...)
	dat$stratum_old = dat$stratum
	lodep = c(18.293, 36.586, 54.879, 73.172, 91.465, 109.758, 128.051)  ## in metres
	updep = c(36.586, 54.879, 73.172, 91.465, 109.758, 128.051, 146.344) ## in metres
	## Depths in SQL extraction from GFBioSQL are in metres
#browser();return()
	#dat$stratum = sapply(dat$sdepth, function(mm){ zz = mm >= lodep & mm < updep; print(zz); if(all(is.na(zz)) || !any(zz)) NA else stratum[zz] },simplify=TRUE)
	dat$stratum = sapply(dat$sdepth, function(mm){ zz = mm >= lodep & mm < updep; if(all(is.na(zz)) || !any(zz)) NA else stratum[zz] },simplify=TRUE)
	## Individual tweaks
	dat$stratum[is.na(dat$stratum) & (dat$sdepth<18.3 & !is.na(dat$sdepth))]  = 1  ## add one tow at 18 m
	dat$stratum[is.na(dat$stratum) & (dat$sdepth<148.3 & !is.na(dat$sdepth))] = 7  ## add two tows at 148 m

	## Overwrite stratum areas
	areas = c(2657, 1651, 908, 828, 912, 792, 612)
	zstra = !is.na(dat$stratum)
	dat$area_old = dat$area
	dat$area     = rep(NA, nrow(dat))
	dat$area[zstra] = areas[dat$stratum[zstra]]

	width  = 43/3280  ## wingspread is 43 ft> convert to kilometres
	speedy = crossTab(dat[is.element(dat$year,2002:2003),,drop=FALSE],c("year"),"speed", mean, na.rm=TRUE)
	zspeed = is.element(dat$year,2002:2003) & is.na(dat$speed)
	if (any(zspeed))
		dat$speed[zspeed] = speedy[as.character(dat$year[zspeed])]
	zdist = (dat$year>=2000 & !is.na(dat$year)) & is.na(dat$distance)
	if (any(zdist))
		dat$distance[zdist] = dat$speed[zdist] * dat$effort[zdist]
	dat$density = dat$weight / (dat$distance * width)
	dat$density[is.na(dat$density) & !is.na(dat$distance) & dat$year>=2000] = 0

	nunits = 1/0.0486
	dat$cpue = nunits * dat$weight / dat$effort  ## convert into density as suggested by Sinclair, page 11, penultimate para
	dat$cpue[is.na(dat$cpue)] = 0

	## The following reproduces what is in the ROL 2005 ResDoc
	dat$door[is.na(dat$door)]   = 43
	dat$speed[is.na(dat$speed)] = 5.1
	dat$distance[is.na(dat$distance)] = dat$speed * dat$effort
	dat$density = dat$weight / (dat$distance * dat$door/1000.)
	dat$density[is.na(dat$density)]   = 0

	note = list()
	note[["author"]] = "Paul J. Starr"
	note[["source"]] = paste0("This is a file of the Hecate Strait survey data from 1984 to 2003. The only catch data are for ", ttcall(speciesname), ".",
		"This file has all the survey tows and reasonably closely resembles the information in Sinclair (PSARC 99-196e)")
	note[["caveat"]] = "I could not reproduce exactly the distribution of tows shown in Table 6 of PSARC 99-196e.  Not sure why: tried basing the definition on the start depth, end depth, and the mean of the start and end depths.  Part of the problem is that the depths are in metres and the depth stratum definition are in fathoms.  I suspect that there is a unit conversion problem here and possibly a precision issue on the conversion.  All work is shown in function 'prepHSass'."
	note[["stratum"]] = "Stratum definition based on the depth at the start of the tow."
	note[["speed"]] = "Filled in 6 missing values in 2002 and 4 missing values in 2003 with mean speed for each year."
	note[["distance"]] = "Filled in missing values in 2002 and 2003 using speed*effort (=time towed). There were no missing distance values in 2000."
	note[["density"]] = "Calculated only from 2000 onward (density=weight/(distance*0.01311 km) where 0.01311=43 ft/3280 ft/km."
	note[["grouping"]] = "Code generated by Norm Olsen.  Conforms exactly to the definition of stratum by PJS"
	note[["stratum"]] = c(note[["stratum"]], "Added one tow at 18 m (just above 10 fathoms) to stratum 1. Added two tows at 148 m (just below 79 fathoms) to stratum 7. This field is missing ('.') for usability codes other than 0/1/6/.")
	note[["grouping"]] = c(note[["grouping"]], paste0("Original grouping code for all tows: ", texThatVec(.su(dat$group),simplify=FALSE)))

	## Choose fields for data return
	if (!is.null(ttcall(assflds))){
		ttget(assflds)
		outflds = intersect(assflds, colnames(dat))
	} else {
		.flush.cat("All data fields will be returned.\n\tRun 'prepGFsurv' to get a standard subset of fields.\n\n")
		outflds = colnames(dat)
	}
	expr = expression(dat[,outflds])
	dat  = keepAtts(dat, expr, extras=list(outflds=outflds))

	attr(dat,"label") = label
	attr(dat,"note")  = note
#browser();return()
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~prepHSass


## prepHSsyn----------------------------2022-05-17
##  Prepare Hecate Strait synoptic survey data.
## -----------------------------------------PJS|RH
prepHSsyn = function(file, survey="HSsyn")
{
	surveyname = "Hecate Strait Synoptic"
	ttput(surveyname)
	ttput(survey)

	dat    = getLabels(file)
	ttget(stratum)
	label  = attributes(dat)$label ## keep temporarily just to be safe
#browser();return()

	#stratum = 1:4
	#names(stratum) = c("10-70m", "70-130m", "130-220m", "220-500m")
	#attr(stratum,"labelends") = getLabelends(labelname=names(stratum))
	#ttput(stratum)

	dat = doSynoptic(dat, survey)

	#for (i in stratum) {  ## why?
	#	z = !is.na(dat$stratum) & is.element(dat$stratum,i)
	#	dat$area[z] = switch(as.numeric(i), 5958, 3011, 2432, 1858)
	#}

	## Choose fields for data return
	if (!is.null(ttcall(synflds))){
		ttget(synflds)
		outflds = intersect(synflds, colnames(dat))
	} else {
		.flush.cat("All data fields will be returned.\n\tRun 'prepGFsurv' to get a standard subset of fields.\n\n")
		outflds = colnames(dat)
	}
	expr = expression(dat[,outflds])
	dat  = keepAtts(dat, expr, extras=list(outflds=outflds))
#browser();return()
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~prepHSsyn


## prepQCSsyn---------------------------2022-05-17
##  Prepare QCS synoptic survey data
## -----------------------------------------PJS|RH
prepQCSsyn = function(file, survey)
{
	ttget(gfbsurvey)
	if (all(gfbsurvey[[survey]]==1))
		surveyname = "QC Sound Synoptic"
	else if (all(gfbsurvey[[survey]]==21))
		surveyname = "GIG Historical"
	ttput(surveyname)
	ttput(survey)

	dat    = getLabels(file)
	ttget(stratum)
	label  = attributes(dat)$label ## keep temporarily just to be safe
#browser();return()

	#stratum = 1:8
	#names(stratum) = c("South:50-125m", "South:125-200m", "South:200-330m", "South:330-500m", "North:50-125m", "North:125-200m", "North:200-330m", "North:330-500m")
	#ttput(stratum)

	#delimit cr
	#lab save stratum using $stratumlabelfilename, replace

	dat = doSynoptic(dat, survey)

	if (all(gfbsurvey[[survey]]==21)) {
		dat$stratum = dat$stratum + 1  ## this won't work because strata are now characters
		if (dat$species=="396") {
			dat$area[dat$stratum==2] = 4148
			dat$area[dat$stratum==3] = 2200
			#notes drop area in 1
			#notes area: POP survey data: area values replaced with restratified POP values
		}
	}

	## Define depth strata
	#depstrat = 1:4
	#names(depstrat) = c("50-125m", "125-200m", "200-330m", "330-500m")
	#attr(depstrat,"labelends") = getLabelends(labelname=names(depstrat))
	#ttput(depstrat)

	#label = attributes(dat)$label
	#label[["depstrat"]] = list(decribe="Depth intervals")
	#label[["depstrat"]][["interval"]] = names(depstrat)
	#attr(dat,"label") = label

	## Populate dat with 'depstrat' -- note not terribly consistent with depth manipulation in 'doSynoptic'
	#lodep = as.numeric(attributes(depstrat)$labelends$low)
	#updep = as.numeric(attributes(depstrat)$labelends$upp)
	#dat$best_depth = coalesce(dat$seabird_depth, dat$sdepth, dat$edepth)  ## now in 'doSynoptic'
	#dat$depstrat = rep(NA, nrow(dat))
	#for (i in 1:length(depstrat)){
	#	ii = depstrat[i]
	#	zz = dat$best_depth>=lodep[i] & dat$best_depth<updep[i] & !is.na(dat$best_depth)
	#	dat$depstrat[zz] = ii
	#}

	## Choose fields for data return
	if (!is.null(ttcall(synflds))){
		ttget(synflds)
		outflds = intersect(synflds, colnames(dat))
	} else {
		.flush.cat("All data fields will be returned.\n\tRun 'prepGFsurv' to get a standard subset of fields.\n\n")
		outflds = colnames(dat)
	}
	expr = expression(dat[,outflds])
	dat  = keepAtts(dat, expr, extras=list(outflds=outflds))
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~prepQCSsyn


## prepNMFStri--------------------------2022-05-17
##  Prepare US National Marine Fisheries Service
##  triennial survey data from WCVI.
## -----------------------------------------PJS|RH
prepNMFStri = function(file, survey="NMFStri")
{
	ici = lenv() ## RH 200716
	surveyname = "NMFS Triennial"
	ttput(surveyname)
	ttput(survey)

	stratum = 475:500 ## GFBio Grouping Code
	## Set strata to the original stratum codes from NMFS Triennial survey
	## Depth strata: 10-19 = shallow (55-183m), 20-29 = mid-depth (184-366m), 30-39 = deep (367-500m)
	## WCVI strata = 19, 29, 39
	stratum = c(10, 11, 12, 17, 18, 19, 27, 28, 29, 30, 31, 32, 37, 38, 39, 50, 51, 52, 121, 122, 123, 124, 131, 132, 133, 134)
	names(stratum) = 475:500 ## name using GFBio grouping codes
	ttput(stratum)

	dat   = doSynoptic(dat, survey) ## even though it's not synoptic, function has some standardising routines
	ttget(stratum)                   ## had attributes added in 'doSynoptic'
	label = attributes(dat)$label    ## add later as there is a lot of fiddly manipulation that follows

	note = list()
	note[["author"]] = "Paul J. Starr"
	note[["code_canada"]] = c(
		"cap drop canad",
		"gen byte canad=.",
		"replace canad=1 if substr(stratum,3,1)==\"N\"",
		"replace canad=. if substr(stratum,2,1)==\"7\"&canad~=.",
		"replace canad=1 if year==1980&(stratum==\"11\"|stratum==\"31\"|stratum==\"51\")",
		"replace canad=1 if year==1983&(stratum==\"12\"|stratum==\"32\"|stratum==\"52\")",
		"replace canad=1 if stratum==\"39\"",
		"replace canad=2 if canad==.",
		"lab def canad 2 \"US waters\" 1 \"Canadian waters\"",
		"lab val canad canad",
		"lab var canad \"US/Canada flag\"",
		"gen byte strat=real(substr(stratum,1,2))",
		"gen byte llen=length(stratum)",
		"gen byte portion=.",
		"replace portion=1 if llen==3&substr(stratum,3,1)==\"S\"",
		"replace portion=2 if llen==3&substr(stratum,3,1)==\"N\"",
		"cap label drop portion",
		"lab def portion 1 \"S\" 2 \"N\"",
		"lab val portion portion",
		"drop llen",
		"lab var portion \"N/S identifier\""
	)

	## Remove survey data prior to 1980
	dat = dat[dat$year>=1980 & !is.na(dat$year),]

	## Delineate Vancouver and Columbia INPFC regions (Columbia below 47.5 deg N)
	inVan     = dat$latitude > 47.5 & !is.na(dat$latitude)
	dat$INPFC = rep("",nrow(dat))
	dat$INPFC[inVan]  = "Vancouver"
	dat$INPFC[!inVan] = "Columbia"

	## GFBio has no information on tows in Canada vs USA other than strata 19, 29, and 39.
	## Use shapefile 'eez.bc' in package PBSdata to delimit tows based on their geographical coordinates
	data("eez.bc", package="PBSdata", envir=ici) ## RH 200716
	dat$EID = 1:nrow(dat)
	dat$X   = dat$longitude
	dat$Y   = dat$latitude
	dat     = as.EventData(dat, projection="LL")
	locdat  = findPolys(dat, eez.bc, includeBdry=1)
	inCan   = is.element(dat$EID, locdat$EID)

	dat$country         = rep("",nrow(dat))
	dat$country[inCan]  = "CAN"
	dat$country[!inCan] = "USA"

	## Define depth strata
	depstrat = 1:3
	names(depstrat) = c("55-183m", "184-366m", "367-500m")
	attr(depstrat,"labelends") = getLabelends(labelname=names(depstrat))
	ttput(depstrat)

	dat$depstrat = rep(NA, nrow(dat))
	lodep = as.numeric(attributes(depstrat)$labelends$low)
	updep = as.numeric(attributes(depstrat)$labelends$upp)
	## Depths in SQL extraction from GFBioSQL are in metres
	dat$depstrat = sapply(dat$best_depth, function(mm){ zz = mm >= lodep & mm < updep; if(!any(zz)) NA else depstrat[zz] },simplify=TRUE)

	## Individual tweaks
	dat$depstrat[is.element(dat$stratum,10:12) & is.element(dat$year,c(1980,1983))] = 1  ## comment: all are 1 already
	dat$depstrat[is.na(dat$depstrat) & is.element(dat$year,c(1980,1983))] = 2            ## comment: no NA depstrats in these years
	dat$depstrat[is.element(dat$stratum,17:19) & dat$year>=1989] = 1                     ## comment: 3 NAs changed to 1
	dat$depstrat[is.element(dat$stratum,20:29) & dat$year>=1989] = 2                     ## comment: all are 2 already
	dat$depstrat[dat$stratum>=30 & dat$year>=1989] = 3                                   ## comment: all are 3 already

	## Create strata for three indices (i) Canada Vancouver, (ii) USA Vancouver, and (iii) Total Vancouver
	dat$stratTT = dat$stratUS = dat$stratCA = NA
	dat$stratCA[inVan & inCan]   = dat$stratum[inVan & inCan]
	dat$stratUS[inVan & !inCan]  = dat$stratum[inVan & !inCan]
	dat$stratTT[inVan] = dat$stratum[inVan]

	## Why would you want to set strata to NA?
	## Notes suggest for biomass estimation (see PJS Table B.5 in RSR 2018 ResDoc)
	in17u = is.element(dat$stratUS, 17)    ## USA
	in18c = is.element(dat$stratCA, 18)    ## CAN
	in27u = is.element(dat$stratUS, 27)    ## USA
	in28c = is.element(dat$stratCA, 28)    ## CAN
	in30s = is.element(dat$stratTT, 37:39) ## ALL
#browser();return()
	dat$stratUS[in17u|in27u] = dat$stratTT[in17u|in27u] = NA
	dat$stratCA[in18c|in28c] = dat$stratTT[in18c|in28c] = NA
	dat$stratCA[in30s] = dat$stratUS[in30s] = dat$stratTT[in30s] = NA

	#dat$strat[is.element(dat$strat, c(17,18,27,28)) & is.element(dat$country,"USA")] = NA
	## PJS code for parsing strata on boundary where portion=1 is 'N' and portion=2 is 'S'
	#replace strat=. if (strat==17|strat==27)&portion==1	
	#replace strat=. if (strat==18|strat==28)&portion==2
	#merge 1:1 year vess haul using D:\Groundfish\2017\Redstripe\data\surveys\WCVITriennial\usadecod.dta, keepusing(usability) nogen
	#replace strat=. if usability==5 ## already handled by 'getUsability' in 'doSynoptic'

	## Need to estimate density (none provided)
	dat$weight[is.na(dat$weight)] = 0
	dat$number[is.na(dat$number)] = 0
	dat$cpue = dat$weight / dat$effort  ## kg/h
	dat$cpue[is.na(dat$cpue)] = 0
	#dat$door[is.na(dat$door)]   = 43  ## all door widths available (as MOUTH_OPENING_WIDTH)
	dat$speed[is.na(dat$speed)] = 5.1  ## no speeds available so set to a constant (using HS Ass)
	dat$distance[is.na(dat$distance)] = dat$speed * dat$effort  ## not really necessary because all distances are available
	dat$density = dat$weight / (dat$distance * dat$door/1000.)
	dat$density[is.na(dat$density)] = 0

	## Stratum definitions used in the 1980 and 1983 surveys were different than
	## those used in subsequent surveys (9166 km^2 / 7399 km^2) = 1.24
	scaleup = is.element(dat$year, c(1980,1983))
	## RH: Not sure that this is needed when stratum areas from earlier time periods have been estimated (see next code segment).
	#dat$density[scaleup] = dat$density[scaleup] * 1.24

	#Stratum areas (from PJS Survey Appendices in rockfish assessments where available and convex hulls if not)
	areaCA = c(0,8948,5317,0,159,8224,0,88,942,0,804,525,0,66,875,0,43,0)
	areaUS = c(3795,2422,0,1033,2123,363,125,787,270,725,1134,0,102,175,0,198,866,0)
	areaTT = c(3795,12948,5317,1033,2282,8587,125,875,1212,725,5481,525,102,241,875,198,7303,0)
	names(areaCA) = names(areaUS) = names(areaTT) = c(10,11,12,17,18,19,27,28,29,30,31,32,37,38,39,50,51,52)

	dat$areaTT = dat$areaUS = dat$areaCA = rep(NA,nrow(dat))
	zCA = inVan & inCan & is.element(dat$stratCA, names(areaCA)) & !is.na(dat$stratCA)
	dat$areaCA[zCA]  = areaCA[as.character(dat$stratCA[zCA])]
	zUS = inVan & !inCan & is.element(dat$stratUS, names(areaUS)) & !is.na(dat$stratUS)
	dat$areaUS[zUS] = areaUS[as.character(dat$stratUS[zUS])]
	zTT = inVan & is.element(dat$stratTT, names(areaTT)) & !is.na(dat$stratTT)
	dat$areaTT[zTT] = areaTT[as.character(dat$stratTT[zTT])]
#browser();return()


	note[["general"]] =c(
		"This is a file of the NMFS triennial survey data from 1980 to 2001.",
		"Use variable 'strat' instead of 'stratum' for biomass estimation." )
	note[["strat"]] = "Strata not used in biomass estimation have been set ==. (missing) -- RH use fields 'stratCA' or 'stratUS'"
	note[["depstrat"]] = "Depth identifier for all tows (not coded for biomass estimation)."
	note[["usability"]] =c(
		"Water hauls identified with usability==5 (removed from biomass estimation).",
		"See Chapter B.4.4 in Appendix B (surveys) of the Redstripe Rockfish stock assessment ResDoc for a discussion of this issue.",
		"See [InfoRecall\\Groundfish2017&2018\\Data preparation notes\\water hauls in the Triennial survey data] for how these tows were identified."
	)

	## Choose fields for data return
	if (!is.null(ttcall(triflds))){
		ttget(triflds)
		outflds = intersect(triflds, colnames(dat))
	} else {
		.flush.cat("All data fields will be returned.\n\tRun 'prepGFsurv' to get a standard subset of fields.\n\n")
		outflds = colnames(dat)
	}
	expr = expression(dat[,outflds])
	dat  = keepAtts(dat, expr, extras=list(outflds=outflds))

	attr(dat,"label") = label
	attr(dat,"note")  = note
#browser();return()
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~prepNMFStri


## prepWCHGsyn--------------------------2022-05-18
##  Prepare WCHG (formerly WCQCI) synoptic survey data
## -----------------------------------------PJS|RH
prepWCHGsyn = function(file, survey="WCHGsyn")
{
	surveyname = "WCHG Synoptic"
	ttput(surveyname)
	ttput(survey)

	dat    = getLabels(file)
	ttget(stratum)
	label  = attributes(dat)$label ## keep temporarily just to be safe

	#stratumWCHG = 1:4
	#names(stratumWCHG) = c("180-330m", "330-500m", "500-800m", "800-1300m")
	#attr(stratumWCHG,"labelends") = getLabelends(labelname=names(stratumWCHG))
	#ttput(stratumWCHG)
   #
	#stratum2006 = 1:5
	#names(stratum2006) = c("150-200m", "200-330m", "330-500m", "500-800m", "800-1300m")
	#attr(stratum2006,"labelends") = getLabelends(labelname=names(stratum2006))
	#ttput(stratum2006)

	expr     = expression(dat[is.element(dat$year, 2006),])
	only2006 = keepAtts(dat, expr)
	#stratum  = stratum2006; ttput(stratum)
	only2006 = doSynoptic(only2006, survey)
	#ttget(stratum) ## doSynoptic adds a grouping code
	#nstrat   = attributes(stratum)$labelends$nstrat
	only2006 = restratify(only2006, strategy=4, "sdepth", "edepth")
	label[["usability"]] = attributes(only2006)$label$usability[c("describe","codes")]
	label[["usability"]][["only2006"]] = attributes(only2006)$label$usability[c("note", "Nrows", "nrows", "use")]
	only2006$stratum = as.numeric(only2006$stratum) - 1
	only2006 = only2006[only2006$stratum >= 1 & !is.na(only2006$stratum),]  ## shallowest stratum not repeated in subsequent years
	only2006$stratum = as.character(only2006$stratum)
#browser();return()

	expr    = expression(dat[!is.element(dat$year, c(2006, 2014)),])
	no2006  = keepAtts(dat, expr)
	#stratum = stratumWCHG; ttput(stratum)
	no2006  = doSynoptic(no2006, survey)
	#ttget(stratum) ## doSynoptic adds a grouping code
	nstrat  = attributes(stratum)$labelends$nstrat
	label[["usability"]][["no2006"]] = attributes(no2006)$label$usability[c("note", "Nrows", "nrows", "use")]
#browser();return()

	intflds  = intersect(colnames(only2006),colnames(no2006))
	dat      = rbind(only2006[,intflds], no2006[,intflds])
	attr(dat, "label") = label

	## Replace stratum areas for 2006 with those from later years
	.flush.cat("Replacing area values in 2006 with 2007+ values", "\n")
	areatab  = crossTab(dat[!is.element(dat$year, 2006),], c("year","group"), "area", mean, na.rm=TRUE)
	areavec  = apply(areatab, 2, function(x){ mean(x[x>0 & !is.na(x)]) } )
	avec2006 = c(399,1266,1090,927,2228); names(avec2006) = 125:129 ## from GFB C_Grouping
	areavec  =  c(areavec, avec2006)

	dat$area[is.element(dat$year, 2006)] = 0
	fix.area = (dat$area==0 | is.na(dat$area)) & !is.na(dat$group)
	dat$area[fix.area] = areavec[as.character(dat$group[fix.area])]
#browser();return()

	## Get rid of deepest stratum (4) -- not fished in 2006 & 2007
	.flush.cat("Removing shallowest stratum(==0: 15-200 m) & deepest stratum (==4: 800-1300 m) [not fished consistently]", "\n\n")
	expr = expression(dat[!is.na(dat$stratum) & !is.element(dat$stratum,c(0,4)),])
	dat  = keepAtts(dat, expr)

	## Choose fields for data return
	if (!is.null(ttcall(synflds))){
		ttget(synflds)
		outflds = intersect(synflds, colnames(dat))
	} else {
		.flush.cat("All data fields will be returned.\n\tRun 'prepGFsurv' to get a standard subset of fields.\n\n")
		outflds = colnames(dat)
	}
	expr = expression(dat[,outflds])
	dat  = keepAtts(dat, expr, extras=list(outflds=outflds))
	#browser();return()
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~prepWCHGsyn


## prepWCVIsyn-------------------------2022-05-17
##  Prepare west coast Vancouver Island synoptic survey data.
## -----------------------------------------PJS|RH
prepWCVIsyn = function(file, survey="WCVIsyn")
{
	surveyname = "WCVI Synoptic"
	ttput(surveyname)
	ttput(survey)

	stratum = 1:4
	names(stratum) = c("50-125m", "125-200m", "200-330m", "330-500m")
	attr(stratum,"labelends") = getLabelends(labelname=names(stratum))
	ttput(stratum)

#browser();return()
	dat = doSynoptic(file, survey)

	## Choose fields for data return
	if (!is.null(ttcall(synflds))){
		ttget(synflds)
		outflds = intersect(synflds, colnames(dat))
	} else {
		.flush.cat("All data fields will be returned.\n\tRun 'prepGFsurv' to get a standard subset of fields.\n\n")
		outflds = colnames(dat)
	}
	expr = expression(dat[,outflds])
	dat  = keepAtts(dat, expr, extras=list(outflds=outflds))
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~prepWCVIsyn


## restratify---------------------------2019-07-29
##  Restratify survey by depth
##  Pass the strategy number and the names of the variables with beginning and end tow depths
##  Arguments:
##  dat       -- survey data file
##  strategy  -- numeric code that chooses depths to use for restratification
##  dbegin    -- character field name for beginning depth of tow
##  dend      -- character field name for ending depth of tow
##  renamevar -- character name for new field
## -----------------------------------------PJS|RH
restratify = function(dat, strategy, dbegin, dend, renamevar)
{
	## Definition of the values to pass for a restratification strategy:
	strategies = c("min(begin_end)", "max(begin_end)", "begin", "end", "mean(begin_end)")
	names(strategies) = 0:4

	if (strategy<0 || strategy>4) {
		stop("Passed strategy is out of range -- restratification exited w/o completion")
	} else {
		sstrategy = as.character(strategy)
		.flush.cat(paste0("Restratification subroutine using strategy: ", strategies[sstrategy]), "\n\n")
	}
	if (strategy==0)      dat$minmax = as.vector(apply(dat[,c(dbegin,dend),drop=FALSE], 1, min, na.rm=TRUE))
	else if (strategy==1) dat$minmax = as.vector(apply(dat[,c(dbegin,dend),drop=FALSE], 1, max, na.rm=TRUE))
	else if (strategy==2) dat$minmax = as.vector(dat[,dbegin])
	else if (strategy==3) dat$minmax = as.vector(dat[,dend])
	else if (strategy==4) dat$minmax = as.vector(apply(dat[,c(dbegin,dend),drop=FALSE], 1, mean, na.rm=TRUE))

	## Restratify based on strategy depth
	ttget(stratum)
	ugroup = .su(dat$group)
	zdep   = is.element(attributes(stratum)$gstrat,ugroup)
	ustrat  = attributes(stratum)$lstrat[as.character(ugroup)]

	lodep  = as.numeric(attributes(stratum)$labelends$low[zdep])
	updep  = as.numeric(attributes(stratum)$labelends$upp[zdep])
#browser();return()
	gcode  = ugroup #attributes(stratum)$grouping_code
	dat$group_old   = dat$group
	dat$group       = sapply(dat$minmax, function(mm){ zz = mm >= lodep & mm < updep; if(!any(zz)) NA else gcode[zz] },simplify=TRUE)
	dat$stratum_old = dat$stratum
	dat$stratum     = sapply(dat$minmax, function(mm){ zz = mm >= lodep & mm < updep; if(!any(zz)) NA else stratum[zdep][zz] },simplify=TRUE)
#browser();return()

	if(!missing(renamevar))
		colnames(dat)[grep("minmax", colnames(dat))] = renamevar

	no.strat = is.na(dat$stratum)
	if (any(no.strat))
		write.csv(dat[no.strat,c(c("fe_id", "year", "set", "major") , dbegin, dend, c("group_old", "group", "stratum_old", "stratum")),drop=F], file=paste0(gsub(" ",".",ttcall(surveyname)),".restrat.drops.csv"), row.names=FALSE)
#browser();return()

#	if "$ifuse"~="" {
#		qui gen byte `tmpstrat'=1 $ifuse
#		di in ye _n "tagging stratum variable so that only tows which $ifuse are !=."
#		replace stratum=. if `tmpstrat'==.
#	}
#	di _n(2)
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~restratify


## uniqtows ----------------------------2019-07-24
##  Ensure unique tows occur for each survey,
##  collapse records as needed.
## -----------------------------------------PJS|RH
uniqtows = function(dat)
{
	## check for more than 1 observation per tow:
	dat$tow = paste(dat$year, dat$set, sep="-")
	utows    = .su(dat$tow)
	if (length(utows) != nrow(dat)) {
		ntws = table(dat$tow)
		.flush.cat(paste0("WARNING!!  Up to ", max(ntws), " observations for some tows in this data set:"), "\n\n")
		dupe.tows = names(ntws)[ntws>1]
		dupe.recs = dat[is.element(dat$tow, dupe.tows),]
		print(dupe.recs[,c("year", "set", "date", "stratum", "sdepth", "edepth", "door", "distance", "weight"), drop=FALSE])
		list.recs = split(dupe.recs, dupe.recs$tow)
		poop = t(  ## transpose this shit (will be a matrix)
		sapply(list.recs, function(X) {
			doop = sapply(X,function(x){ length(.su(x))>1 })
			poop = X[1,,drop=FALSE]
			if (doop["weight"]) {
				weight = sum(X[,"weight"], na.rm=TRUE)
				poop[,"weight"] = weight
			}
			## could check for other duplicates and apply other functions
			return(poop)
		}) ) 

		expr1 = expression(dat[!is.element(dat$tow, dupe.tows),])
		good  = keepAtts(dat, expr1, extras=list(dupe.tows=dupe.tows))
		poop  = as.data.frame(poop)
		## https://stackoverflow.com/questions/27361081/r-assign-or-copy-column-classes-from-a-data-frame-to-another
		poop[] <- mapply(FUN = as, poop, sapply(good,class), SIMPLIFY = FALSE)
		.flush.cat("\nThese are revised observations for the duplicate tows in this data set:\n")
		print(poop[,c("year", "set", "date", "stratum", "sdepth", "edepth", "door", "distance", "weight"), drop=FALSE])

		expr2 = expression(rbind(dat, as.data.frame(poop)))
		dnew  = keepAtts(good, expr2, extras=list(poop=poop))

		expr3 = expression(dat[order(dat$tow),])
		dat   = keepAtts(dnew, expr3)
#browser();return()
	}
	expr = expression(dat[,setdiff(colnames(dat), "tow")])
	dat  = keepAtts(dat, expr)
	return(dat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~uniqtows


## ---------STATA (still to be processed)---------

#cap prog drop prepwcvishrimp
#prog def prepwcvishrimp
#
#	tempvar tmpstrat
#
#	syntax, file(string)
#	cap log close
#	log using surveyprepmessages.log, replace
#
#	qui {
#		lab def 
#	stratum = 1:2
#	names(stratum) = c("124","125")
#		lab save stratum using $stratumlabelfilename, replace
#		getlabels `file'
#		drop if year<$shrimpdropyear						/* capacity to conform to GF Section standard */
#		count if trip==67467&sdepth~=.
#		if r(N)==0 {
#			di in red _n(2) "Forgot to update the depth observations in trip==67467!!" _n /*
#				*/ _col(6) "They are all missing! Run the update query and re-extract the data" _n /*
#				*/ _col(6) "Program ended!!"
#			exit 9999
#		}
#		drop open wing effort2
#		lab var door "Doorspread opening size (km)"
#		gen int stratum=1 if grouping_code==112
#		replace stratum=2 if grouping_code==113
#		drop if stratum<1|stratum>2
#		lab val stratum stratum
#		/* the following are dropped tows from Sinclair & Starr (2002) */
#		replace stratum=1 if year==1973&set==47	
#		replace stratum=1 if year==1988&set==68	
#		replace stratum=. if year==2000&set==9	
#		replace stratum=. if year==1973&set==58	
#		replace stratum=. if year==1973&set==59	
#		replace stratum=. if year==1973&set==60	
#		replace stratum=. if year==1973&set==61	
#		replace stratum=. if year==1973&set==62	
#		replace stratum=. if year==1973&set==63	
#		replace stratum=. if year==1973&set==78	
#		replace stratum=. if year==1975&set==61	
#		replace stratum=. if year==1975&set==62	
#		replace stratum=. if year==1975&set==63	
#		replace stratum=. if year==1975&set==64	
#		replace stratum=. if year==1975&set==65	
#		replace stratum=. if year==1975&set==66	
#		replace stratum=. if year==1976&set==80	
#		replace stratum=. if year==1997&set==13	
#		replace stratum=. if year==1997&set==14	
#		replace stratum=. if year==1997&set==15	
#		replace stratum=. if year==1998&set==88	
#		replace stratum=. if year==1976&set==9	
#		replace stratum=. if year==1999&set==128	
#		replace stratum=. if year==1999&set==129	
#		replace stratum=. if year==2000&set==116	
#		replace stratum=. if year==2002&set==49	
#		replace stratum=. if year==2001&set==50	
#		getusability
#		gen byte `tmpstrat'=1 $ifuse
#		replace stratum=. if `tmpstrat'==.
#/*
#		/* this code has been suppressed */
#		count if open==.
#		local nopen=r(N)
#		if `nopen'>0 {
#			nois di in ye _n(2) "missing wingspread values by year:"
#			nois tab year if open==.
#			nois di in ye _n "replacing missing wingspread values with constant=10.6 m"
#			nois replace open=10.6 if open==.
#			notes open: replaced `nopen' missing values for this variable with constant=10.6 m
#		}
#*/
#		nois {
#			di in ye _n "replacing all doorspread values with constant=" $defaultshrimpdoorspread*1000 " m"
#			replace door=$defaultshrimpdoorspread
#		}
#		sum year
#		global minyear=r(min)
#		global maxyear=r(max)
#		replace stratum=. if sdepth>160		/* by agreement */
#		replace stratum=. if month>8			/* need to include Aug tows if 1987 is to be included */
#		nois uniqtows door
#		gen density2=weight/(distance*door)
#		replace density2=0 if density2==.
#		getweight
#		global surveyname "WCVI shrimp trawl survey"
#		getnotes 1
#	} /* qui */
#
#	notes: this file has only survey tows for areas 124 & 125 which are less than 160 m in depth /*
#		*/ (corresponds to strat2 in previous analyses)
#	notes stratum: this is the revised stratum definition established by Sinclair & Starr in their 2003 PSARC document /*
#		*/ on WCVI pcod
#	notes distance: this value generated by Shellfish Section: represents time on bottom as determined from /*
#		*/ bottom contact and measurement from lats and longs (phone call w/ J. Bootillier [23 Oct 2006])
#	notes distance: checked that this value is consistently provided in km (rather than knots): [24/09/2007]
#	notes door: replaced all doorspreads for this variable with constant=$defaultshrimpdoorspread km
#
#	order year month date vessel trip set stratum group dfo area speed effort dist latitude longit /*
#		*/ sdep edep door reason $use weight number density
#
#end def
#
#
#
#cap prog drop preppcodmon
#prog def preppcodmon
#
#	tempvar gg
#
#	syntax, file(string)
#
#	qui {
#		#delimit ;
#	stratum = 6:10
#	names(stratum) = c("Pcod TBR", "Pcod WR", "Pcod SG", "Pcod RI", "Pcod HS")
#		;
#		#delimit cr
#		lab save stratum using $stratumlabelfilename, replace
#		getlabels `file'
#		drop species trip vessel effort2 distance wing door open
#		getusability
#		egen byte stratum=group(grouping month) $ifuse
#		local wing=13.8/1000						/* assume constant wingspread of 13.8 m */
#		gen density=weight/(speed*effort*`wing')			/* checked & all speed~=.&wing ~=.&effort~=. */
#		replace density=0 if density==.			
#		lab val grouping stratum
#		lab var strat "Month/Area Stratum
#
#	} /* qui */
#
#	notes: this is a file of the Pacific cod monitoring survey from 2002 to 2004.  /*
#		*/The only catch data are for $speciesname.
#	notes density: assume constant wingspread for this survey: 13.8 m
#	notes density: density=weight/(speed*time*0.0138)  = kg/km^2  (calculated distance)
#	notes stratum: generated from grouping code (stratum code generated by Norm Olsen) & month of capture
#		*/ This field is missing ('.') for usability codes other than 0/1/2/6/.
#	notes grouping: original grouping code for all tows
#	$usenote
#	if "$usenote2"~="" {
#		$usenote2
#	}
#
#	order year month set stratum grouping area dfo speed effort sdep edep $use weight number density	
#
#end def
#
#
#cap prog drop prepqcshrimp
#prog def prepqcshrimp
#
#	tempvar gg
#
#	syntax, file(string)
#	cap log close
#	log using surveyprepmessages.log, replace
#
#	qui {
#	stratum = 1:3
#	names(stratum) = c("109", "110", "111")
#		lab save stratum using $stratumlabelfilename, replace
#		getlabels `file'
#		drop if year==1998							/* agreed to drop this year */
#		drop if year<$shrimpdropyear						/* capacity to truncate to GF section standard */
#		nois {
#			di in ye _n "replacing all doorspread values with constant=" $defaultshrimpdoorspread*1000 " m"
#			replace door=$defaultshrimpdoorspread
#		}
#		keep if grouping==109|grouping==110|grouping==111  		/* rest is junk: see 1999QCSndshrimploc.dta 
#												   for info on sets missing grouping codes in 1999 */		
#		drop wing trip effort2
#		lab var open "Net opening size (m)"
#		getusability
#		sum group
#		gen byte stratum=group-(r(min)-1) $ifuse
#		lab val stratum stratum
#		replace stratum=. if stratum==3					/* get rid of stratum=111 but leave in data set */
#
#		sum year
#		global maxyear=r(max)
#		global minyear=r(min)
#		sum speed
#		local meanspeed=r(mean)							/* get mean speed before substitution */
#		nois di in ye _n(2) "missing speed values by year:"
#		nois tab year if speed==.|speed==0
#		global nspeed=r(N)
#		forv yy=$minyear/$maxyear {
#			sum speed if year==`yy'
#			replace speed=r(mean) if (speed==.|speed==0)&year==`yy'
#		}
#		forv yy=$minyear/$maxyear {
#			tab speed if year==`yy'
#			if r(N)==0 {
#				replace speed=`meanspeed' if year==`yy'
#				local badyear "`badyear' `yy'"
#			}
#		}
#
#		/* Note: distance is in kilometres [updated & checked 24/09/2007] */
#		nois di in ye _n(2) "missing distance or distance==0 values by year:"
#		nois tab y if distance==0|distance==.
#		local ndist=r(N)	
#		replace distance=speed*effort if distance==0|distance==.
#		nois uniqtows door
#		gen density=weight/(distance*door)		
#		replace density=0 if density==.			
#		getweight
#		global surveyname "Queen Charlotte Sound shrimp trawl survey"
#		getnotes 1
#	} /* qui */
#
#	notes distance: this value generated by Shellfish Section: represents time on bottom as determined from /*
#		*/ bottom contact and measurement from lats and longs (phone call w/ J. Bootillier [23 Oct 2006])
#	notes distance: checked that this value is consistently provided in km (rather than knots): [24/09/2007]
#	notes distance: `ndist' tows use a calculated distance (speed*effort) because distance==0 (not credible)|distance=='.'
#	notes density: use same doorspread as used by Norm Olsen: density=weight/(distance*$defaultshrimpdoorspread) [kg/km^2]
#	notes door: this variable filled with value=$defaultshrimpdoorspread (default value used by Norm Olsen). This was used /*
#		*/ in the density calculation
#	notes speed: missing values for this field filled in with mean for survey year (`nspeed' tows)
#	notes speed: the following years have no speed data at all (included in above totals): `badyear' .  /*
#		*/  The overall mean (taken before substitution) was used for these years
#	notes stratum: generated from grouping code (stratum code generated by Norm Olsen) /*
#		*/ Only strata allowed are 109/110.  Stratum 111 has been set='.' as of 2007 due to its location and /*
#		*/ and lack of groundfish species.  Dropped all records with missing strata or WCVI groupings.  /*
#		*/ This field is missing ('.') for usability codes other than 0/1/2/6/.  
#
#	order year month date vessel set stratum group dfo area speed effort dist latitude longit /*
#		*/ sdep edep door open reason $use weight number density
#
#
#end def
#
#
#cap prog drop prep1996caledonian
#prog def prep1996caledonian
#
#	syntax, file(string)
#
#	global surveyname "1996 Caledonian WCVI"
#
#	#delimit ;
#	stratum = 1:4
#	names(stratum) = c("50-125m", "125-200m", "200-330m", "330-500m")
#	;
#	#delimit cr
#	lab save stratum using $stratumlabelfilename, replace
#
#	getlabelends stratum
#	dosynoptic `file'
#
#	qui {
#		replace stratum=.
#		replace strat=2 if sdepth<200
#		replace strat=3 if sdepth>=200&sdep<330
#		replace strat=4 if strat==.
#	}
#
#	run $stratumlabelfilename
#	lab val stratum stratum
#	lab var strat Stratum
#
#	notes drop door
#	notes door: assumed value (used $defaultdoorspread m=from Yamanaka et al. 1996 Can MS Report 2362)
#	notes drop stratum
#	notes stratum: conformed to WCVI synoptic stratum definitions based on starting depth: replace strat=2 /*
#		*/ if sdepth<200; replace strat=3 if sdepth>=200 &sdep<330; replace strat=4 if strat==.
#
#	order year month date vessel set stratum group dfo area speed effort distance distance_calc latitude longit /*
#		*/ sdep edep door reason $use weight number density
#
#end def
#
#cap prog drop prepgbreedwcvi
#prog def prepgbreedwcvi
#
#	/* this section is directed towards GB Reed surveys that worked the WCVI.  All other tows will be suppressed.
#	   I propose to use the WCVI synoptic survey depth strata and ignore the areal strata (for the time being)
#	   All other tows will have stratum==. (note that there are no Norm Olsen grouping codes with these data)
#
#	   I am also going to hardwire special code for the 1972 survey because the date fields have been screwed up
#	   in the original data.
#
#	   [19 Aug 2013]: started to move to a more general approach for Silvergray.  Allow for all tows between 50m
#		and 500m.  This code is unfinished and needs to be considered carefully before using it.
#	*/
#
#	tempvar gg
#	tempfile hold 1972 missing
#
#	syntax, file(string) 
#
#	qui {
#		#delimit ;
#	stratum = 1:4
#	names(stratum) = c("50-124m", "125-199m", "200-329m", "330-500m")
#		;
#		#delimit cr
#		lab save stratum using $stratumlabelfilename, replace
#		global surveyname "GB Reed Historic: WCVI"
#
#		getlabelends stratum
#		getlabels `file'
#		egen byte `gg'=group(year)
#		tab `gg'
#		local nr=r(r)
#
#		/* generate my own stratum definitions: */
#		gen byte stratum=.
#		forv nz=1/$nstrat {
#			replace stratum=`nz' if sdepth>=${low`nz'}&sdepth<${upp`nz'}
#		}
#		/* step through out-of-range tows and assign to a stratum if edepth or seabird_depth allows */
#		save `hold'
#		keep if stratum==.
#		save `missing'
#		use `hold'
#		drop if stratum==.
#		save `hold', replace
#		use `missing'
#		sort year fe_id
#		count
#		forv nn=1/`r(N)' {
#			foreach deptype in edepth seabird {
#				local strat=stratum[`nn']
#				if `strat'==. {
#					local newdepth=`deptype'[`nn']
#					forv nz=1/$nstrat {
#						if `newdepth'>=${low`nz'}&`newdepth'<${upp`nz'} {
#							replace stratum=`nz' in `nn'
#							continue, break
#						}
#					}
#				}
#			}
#		}
#		save `missing', replace
#		use `hold'
#		append using `missing'
#		replace stratum=. if major~=3&major~=4
#		*replace stratum=. if longit>-125.8
#		lab val stratum stratum
#		lab var strat Stratum	
#
#		/* replace wonky 1972 data */
#		save `hold', replace
#		keep if year==1972
#		save `1972'
#		use `hold'
#		drop if year==1972
#		save `hold', replace
#		insheet using D:\Groundfish\2012\POP\data\surveys\WCVI\GBReed_historic\trip67450_allfe.csv, clear
#		ren fe_begin_deployment_time dd
#		ren fe_end_retrieval_time dd2
#		ren fishing_event_id fe_id
#		keep fe_id dd dd2
#		ddate dd, gen(date) mask("ymd") flag(0)
#		foreach ddz in dd dd2 {
#			local cc=`cc'+1
#			ttime `ddz', flag(0)  gen(tt`cc') 
#		}
#		gen effort=tt2-tt1
#		drop tt2 tt1 dd dd2
#		merge 1:1 fe_id using `1972', update
#		save `1972', replace
#		use `hold'
#		append using `1972'
#
#		getvessel
#		drop species trip wing effort2 open
#		getusability
#		replace stratum=. if (use!=0&use!=1&use!=2&use!=6)
#
#		count
#		local ntows=`r(N)'
#		tab vess
#		local nvess=`r(r)'
#		replace door=$defaultdoorspread							
#		replace speed=speed*$kn_to_km if year==1972
#		notes speed: all speeds in 1972 converted to km/h from what appears to have been nm/h (average near 3)
#		lab var distance "Distance travelled
#		count if distance==0
#		local ndist0=`r(N)'
#		count if distance~=.&distance>0
#		local ndist1=`r(N)'
#		replace distance=. 					 		/* will only use calculated distance travelled */
#
#		nois {
#			di in ye _n(2) "missing speed values by year:"
#			tab year if speed==.|speed==0
#			global nspeed=r(N)
#			di in ye _n(2) "mean speed values used by year:"
#			table year, c(n speed mean speed) row
#		}
#		sum speed, meanonly
#		local bigspeed=`r(mean)'
#		forv gr=1/`nr' {
#			if `gr'==1 {
#				sum year if `gg'==1, meanonly
#				local minyear=r(mean)
#			}
#			else if `gr'==`nr' {
#				sum year if `gg'==`nr', meanonly
#				local maxyear=r(mean)
#			}
#			sum speed if `gg'==`gr', meanonly
#			if `r(N)'~=0 {
#				replace speed=`r(mean)' if (speed==.|speed==0)&`gg'==`gr'
#			}
#			else {
#				replace speed=`bigspeed' if (speed==.|speed==0)&`gg'==`gr'
#			}
#		}
#
#		run $stratumlabelfilename
#		gen int area=1082 if stratum==1
#		replace area=499 if stratum==2
#		replace area=587 if stratum==3
#		replace distance=speed*effort if distance==.			/* calculate distance travelled */
#		gen density=weight/(distance*door/1000)
#		replace density=0 if density==.
#	} /* qui */
#
#	notes: this is a file of the Historical GIG GB Reed survey data, with emphasis on WCVI, from `minyear' to `maxyear'.
#	notes stratum: uses depth stratum definitions from WCVI synoptic survey.  Only valid with a valid usability code /*
#		*/ (0/1/2/6) and if Major==3C or Major==3D
#
#	notes: The only catch data are for $speciesname
#	notes door: assumed value (used $defaultdoorspread m=from Yamanaka et al. 1996 Can MS Report 2362)
#	notes speed: speed=mean(speed) for survey year when missing.  Replace $nspeed values in total file
#	notes distance: there were `ndist0' records where distance==0.  These were changed to '.' before all calculations
#	notes distance: there were `ndist1' records where distance~=0&distance~=.  These were changed to '.' before all /*
#		*/ calculations because the values did not look credible and in most instances were incompatible with the /*
#		*/ time towed and a speed of around 5-6 km/h
#	notes distance: calculated field: gen distance (km) =effort (h) *speed (km/h)
#	notes density: gen density=weight/(distance*door/1000)
#	notes density: replace density=0 if density==.
#	$usenote
#	if "$usenote2"~="" {
#		$usenote2
#	}
#	$note1
#	$note2
#
#	drop _m seabird 
#	foreach vvar in group area {
#		confirm exist variable `vvar'
#		if _rc==0 local `vvar' "`vvar'"
#	}
#	order year month date vessel fe_id set stratum major `group' dfo `area' speed effort dist latitude longit /*
#		*/ sdep edep door reason $use weight number density
#
#end def
#	
#cap prog drop prephistoricalwcqci
#prog def prephistoricalwcqci
#
#	/* this section is directed towards a range of surveys that worked the WCQCI.  All other tows will be suppressed.
#	   I will use the WCQCI synoptic survey depth strata and ignore the areal strata (for the time being)
#	   All other tows will have stratum==. (note that there are no Norm Olsen grouping codes with these data)
#	   [30 Aug 2012: put 1997 Caledonian survey into its own prep file apart from these surveys
#	*/
#
#	tempvar gg
#	tempfile keeptrip hold missing
#
#	syntax, file(string) 
#
#	/* these are all the valid trips for this category */
#	local triplist "25394 65806 60606 60607 65646 65366 65826 69347 58580 53020 30864"
#
#	local bluewaters=87.7
#	local scotiabay=48.5
#	local wericker=54.4
#
#	qui {
#		global surveyname "WCQCI Historical"
#
#		#delimit ;
#	stratum = 1:4
#	names(stratum) = c("180-330m", "330-500m", "500-800m", "800-1300m")
#		;
#		#delimit cr
#		lab save stratum using $stratumlabelfilename, replace
#
#		getlabelends stratum
#		getlabels `file'
#		/* get rid of extra trips in the data set */
#		save `hold'
#		foreach ttrip in `triplist' {
#			use `hold', clear
#			local ntrip=`ntrip'+1
#			count if trip==`ttrip'
#			if `r(N)'>0 {
#				keep if trip==`ttrip'
#				if `ntrip'==1 save `keeptrip'
#				else {
#					append using `keeptrip'
#					save `keeptrip', replace
#				}
#			}
#		}	
#		use `keeptrip'
#					
#		count
#		local ntows=`r(N)'
#		tab vess
#		local nvess=`r(r)'
#		replace door=$defaultdoorspread
#		replace door=`bluewaters' if trip==60606|trip==65646
#		replace door=`scotiabay' if trip==60607
#		replace door=`wericker' if trip==53020|trip==30864							
#		lab var distance "Distance travelled
#		count if distance==0
#		local ndist0=`r(N)'
#		count if distance~=.&distance>0
#		local ndist1=`r(N)'
#		replace distance=. if distance==0
#		count if distance==.
#		local ndist2=`r(N)'
#		sum speed if year==1985
#		local mean85speed=`r(mean)'
#		local N85=`r(N)'
#		replace speed=speed/$kn_to_km if year==1985			/* these look wrong so I am correcting here arbitrarily */
#	} /* qui */	
#	
#	cap log close
#	log using surveyprepmessages.log, replace
#	di in ye _n(2) "missing speed values by year:"
#	tab year if speed==.|speed==0
#	global nspeed=r(N)
#	di in ye _n(2) "mean speed values used by year:"
#	table year, c(n speed mean speed) row
#
#	qui {
#		sum speed, meanonly
#		local bigspeed=`r(mean)'
#		egen byte `gg'=group(year)
#		tab `gg'
#		local nr=r(r)
#		forv gr=1/`nr' {
#			if `gr'==1 {
#				sum year if `gg'==1, meanonly
#				local minyear=r(mean)
#			}
#			else if `gr'==`nr' {
#				sum year if `gg'==`nr', meanonly
#				local maxyear=r(mean)
#			}
#			sum speed if `gg'==`gr', meanonly
#			if `r(N)'~=0 replace speed=`r(mean)' if (speed==.|speed==0)&`gg'==`gr'
#			else replace speed=`bigspeed' if (speed==.|speed==0)&`gg'==`gr'
#		}
#	} /* qui */
#
#	/* generate my own stratum definitions: */
#	getusability
#	restratify 4 sdepth edepth meandepth
#	run $stratumlabelfilename
#	lab val stratum stratum
#	lab var strat Stratum	
#	getvessel
#	drop species wing effort2 open
#	cap log close
#	log using surveyprepmessages.log, append
#	di in ye _n "replace stratum=. if year~=1979&year~=1993&year~=1996"
#	replace stratum=. if year~=1979&year~=1993&year~=1996
#	di in ye "replace stratum=. if latitude<54|longitude>-133"
#	replace stratum=. if latitude<54|longitude>-133
#	di in ye "replace stratum=. if stratum==3"
#	replace stratum=. if stratum==3				/* get rid of 500-800m stratum */
#
#	di in ye _n "gen int area=808 if stratum==1"
#	gen int area=808 if stratum==1
#	di in ye "replace area=648 if stratum==2"
#	replace area=648 if stratum==2
#	di in ye "replace distance=speed*effort if distance==."
#	replace distance=speed*effort if distance==.			/* calculate distance travelled */
#	di in ye "gen density=weight/(distance*door/1000)"
#	gen density=weight/(distance*door/1000)
#	di in ye "replace density=0 if density==."
#	replace density=0 if density==.
#	drop seabird 
#	di in ye "replace density=. if distance==."
#	replace dens=.  if dist==.
#
#	notes: this is a file of the Historical WC Queen Chrlotte Islands survey data from `minyear' to `maxyear'.
#	notes stratum: uses depth stratum definitions from WCQCI synoptic survey, without using the deepest (800-1300 m) stratum. /*
#		*/ (no tows this deep anyway). Tows only valid with a valid usability code (0/1/2/6)
#	notes stratum: [30 Aug 2012]: now only valid for tows >54N and west of 133W.  /*
#		*/ Only valid for trips 53020 (1993) 30864 (1996) 60606 (1979) 60607 (1979) 65806 (1979). /*
#		*/ Dropped 500-800m stratum (only 3 tows in this stratum after previous exclusions)
#	notes stratum: rename this variable to {it}stratum{reset} to export the 'valid' tows to {bf}R{reset} for area N of 54N and W of 133W
#	notes: The only catch data are for $speciesname
#	notes area: areas for stratum 1 and stratum 2 (N of 54N and W of 133W) obtained from Norm Olsen (email dated 3 Sept 2012)
#	notes door: no doorspread values in the original data: assumed value (used $defaultdoorspread m=from Yamanaka et al. 1996 Can MS Report 2362)
#	notes speed: speed=mean(speed) for survey year when missing.  Replace $nspeed values in total file
#	notes speed: `N85' records where mean speed=`mean85speed' in 1985 have been divided by the kn_to_km factor=$kn_to_km to convert to km/hr
#	notes distance: there were `ndist0' records where distance==0.  These were changed to '.' before all calculations
#	notes distance: kept value in db when field was not NULL (there were `ndist1' records for this)
#	notes distance: calculated field: gen distance (km) =effort (h) *speed (km/h) when distance==.  (there were `ndist2' records for this)
#	notes density: gen density=weight/(distance*door/1000)
#	notes density: replace density=0 if density==.
#	$usenote
#	if "$usenote2"~="" $usenote2
#	$note1
#	$note2
#
#	notes dist: ==. for set==900 in 1977 (no associated effort)
#	notes dens: ==. for set==900 in 1977 (no associated effort)
#
#	qui {
#		gen byte usable=1 if use==0|use==1|use==2|use==6
#		replace usab=. if sdep==.&edep==.
#		replace usab=. if lat==.|longit==.
#		notes usab: gen byte usable=1 if use==0|use==1|use==2|use==6
#		notes usab: replace usab=. if sdep==.&edep==.
#		notes usab: replace usab=. if lat==.|long==.
#		notes usable: rename this variable to {it}stratum{reset} to export the 'valid' tows to {bf}R{reset} for all of 5E
#		replace meandepth=sdep if meand==.
#		notes meand: gen meandepth=rowmean(sdep+edep)
#		notes meand: replace meandepth=sdep if meand==.
#		ren stratu stratum54N
#		gen str5 tripl=string(trip)
#		drop trip
#		run tripl
#		encode tripl, gen(trip) lab(tripl)
#		drop tripl
#		notes trip: coded trip: order is by year
#	}
#
#	order year month date vessel trip fe_id set stratum major group dfo area speed effort dist latitude longit /*
#		*/ sdep edep door reason $use weight number density
#
#	di _n(2) "{ul on}Notes{reset}:"
#	notes
#	di in ye _n(2) "Table by year and stratum, showing missing values" _c
#	tab year stratum, mis
#	di _n(2)
#	
#	
#end def
#
#cap prog drop prep1997oceanselector
#prog def prep1997oceanselector
#
#	syntax, file(string)
#
#	global surveyname "1997 Ocean Selector WCQCI"
#
#	#delimit ;
#	stratum = 1:4
#	names(stratum) =c("180-330m", "330-500m", "500-800m", "800-1300m")
#	;
#	#delimit cr
#	lab save stratum using $stratumlabelfilename, replace
#
#	dosynoptic `file'
#
#	restratify 4 sdepth edepth
#
#	run $stratumlabelfilename
#	lab val stratum stratum
#	lab var strat Stratum
#
#	replace area=1128 if stratum==1
#	replace area=1044 if stratum==2
#	replace area=960 if stratum==3
#	replace area=2248 if stratum==4
#
#	cap log close
#	log using surveyprepmessages.log, append
#	di in ye _n(2) "Distribution of tows by stratum (before dropping deepest stratum==4)" _c
#	tab stratum year, mis
#	di in ye _n(2) "Removing deepest stratum (==4: 800-1300 m) [not fished consistently among survey years]"
#	replace stratum=. if stratum==4									/* get rid of deepest stratum (not fished in 2006 & 2007) */
#	di in ye "Removing two tows near Anthony Island (S of 53N)"
#	replace stratum=. if latitude<53
#	di _n(2)
#	log close
#
#	notes drop door
#	notes door: assumed value=$defaultdoorspread m
#	notes drop stratum
#	notes stratum: restratified to conform to WCQCI 2007+ depth strata
#
#	order year month date vessel set stratum group dfo area speed effort distance distance_calc latitude longit /*
#		*/ sdep edep door reason $use weight number density
#
#end def
#
#
#cap prog drop getweight
#prog def getweight
#
#/* 	Norm Olsen suggested that the missing weight data records are really situations where the fish were
#	too small to weigh.  Therefore, substituting the mean weight from all fish is probably biased.  I
#	have discontinued using this subroutine but have kept it for reference
#	
#	*below is the deleted code:*
#	tempfile survey meanwgt
#
#	qui {
#		save `survey'
#		collapse (sum) wei numb if wei~=.&wei>0&numb~=.&numb>0,by(year)
#		gen meanwgt=wei/numb
#		sort year
#		drop wei numb
#		save `meanwgt'
#		use `survey'
#		sort year
#		merge year using `meanwgt'
#		nois {
#			di in ye _n "Replacing missing weight data with mean weight for survey_year"
#			replace wei=numb*meanwgt if wei==.&numb~=.
#			di in ye _n(2) "Mean weight by survey_year for non-zero weight/number combinations" _c
#			table year, c(mean meanwgt)
#		}
#		drop meanwgt _merge
#	} /* qui */
#[end deleted code] 
#*/ 
#
#	count if weight==.&number~=.&stratum~=.
#	local nmissingwgt=r(N)
#	count if weight==.&numb==.&species~=.&stratum~=.
#	local nmissingwgt2=r(N)
#
#	if `nmissingwgt'>0|`nmissingwgt2'>0 {
#		nois {
#			di in re _n "WARNING!: " _n "There are `nmissingwgt' valid tows with numbers caught but no weight data" 
#			di in re "There are `nmissingwgt2' valid tows with no weight or numbers caught but with valid species code"_n(2)
#		}
#		global note1 "notes: this dataset did NOT replace the missing weight data with the mean weight by survey year (Norm Olsen pers comm). There were `nmissingwgt' valid tows where weight was missing but there was a valid fish count and `nmissingwgt2' valid tows where both weight and numbers were missing but there was a valid species code"
#	}
#	if `nmissingwgt'>0 {
#		global note2 "notes weight: there are `nmissingwgt' tows where there are no weight data but numbers were recorded.  These are likely to be very small fish that could not be weighed accurately (N. Olsen pers comm)"
#	}
#	if `nmissingwgt2'>0 {
#		global note3 "notes weight: there are `nmissingwgt2' tows where there are NO weight or numbers data were recorded AND there was a valid species code associated with the set.  It is not known what these records represent.  This message is only here to alert me to the presence of these records"
#	}
#	drop species
#
#end def
#'
#
#cap prog drop getnotes
#prog def getnotes
#
#	args synopticflag
#
#	notes: this is a file of the $surveyname data from $minyear to $maxyear.  /*
#		*/The only catch data are for $speciesname.
#	notes density: doorspread-based: =weight/(distance*door)  = kg/km^2
#	if `synopticflag'==0 {
#		notes distance: this field populated with vessel track information by Norm Olsen and Kate R/should be used as default
#		notes distance_calc: calculated field: gen distance_calc=effort2*speed /*
#			*/ (effort field used when effort2 field is missing: $neffort2 tows)
#		notes speed: missing values for this field filled in with mean for survey year ($nspeed tows)
#		notes door: missing values filled in with mean doorspread for year/stratum combination ($ndoor tows)
#	}
#	notes stratum: generated from grouping code (stratum code generated by Norm Olsen) /*
#		*/ This field is missing ('.') for usability codes other than 0/1/2/6/.
#	notes grouping: original grouping code for all tows
#	$usenote
#	if "$usenote2"~="" {
#		$usenote2
#	}
#	$note1
#	$note2
#	$note3
#
#end def
