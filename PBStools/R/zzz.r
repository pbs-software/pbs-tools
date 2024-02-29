# Taking cue from Roger Bivand's maptools:
.PBStoolEnv <- new.env(FALSE, parent=globalenv())  # be sure to exportPattern("^\\.PBS") in NAMESPACE

.onAttach <- function(lib, pkg)
{
	pkg_info = utils::sessionInfo( package="PBStools" )$otherPkgs$PBStools
	if( is.character( pkg_info$Packaged ) )
		pkg_date <- strsplit( pkg_info$Packaged, " " )[[1]][1]
	else
		pkg_date  <- date()

	userguide_path <- system.file( "doc/PBStools-UG.pdf", package = "PBStools" )
	year <- substring(date(),nchar(date())-3,nchar(date()))

	packageStartupMessage("
-----------------------------------------------------------
PBS Tools ", pkg_info$Version, " -- Copyright (C) 2007-",year," Fisheries and Oceans Canada

A somewhat complete user guide 'PBStools-UG.pdf' is located at 
", userguide_path, "

Packaged on ", pkg_date, "
Pacific Biological Station, Nanaimo

All available PBS packages can be found at
https://github.com/pbs-software

We may not be big, but we're small.
-----------------------------------------------------------

")
}
.onUnload <- function(libpath) {
	rm(.PBStoolEnv)
}

# No Visible Bindings
# ===================
if(getRversion() >= "2.15.1") utils::globalVariables(names=c(
	".onClose",".PBSmod",".runModHelperQuit",
	"A","A.SAR","acut","adat","Afile","age","agg","AID","amax","aModel","areas","assflds","autoA","autoD","avgPA","avgRP","avgTS","aVal",
	"B","b0","b5","barcol","barf","bathy","bbonly","Bboot","bcol","bdat","Best","bgsamp","bgtab","bh","bio440","Bmpd","Bobs","boot","boot.ci","bootci","booty","boxpars","boxwidth","Bqnt","Bt","bta","Btot","bugsData",
	"C1","C2","case","cast","cat440rec","catch","Cbar","Cbox","Ccat","ccol","cdata","Ceq","chn1","chn2","clara","clen","Clin","clwd","Cnum","cobs","coefficients","compo","Cpoi","cpuTime","cthin","ctot","Ctrd","currentMCMC","currentProj","currentProj2","Cval",
	"dat","DATA","datt","dattoRlist","dBars","dbName","ddat","defaultdoorspread","diaDir","disA","dist","DLIM",
	"edat","eez.bc","elapTime","empinf","eN","eps","evals","extra",
	"fdir","FID","figDir","fillBars","firstSerial","fminE","fminS","fnam","Fout","formalArgs","fosspp","fqtName","Ft","ftime","func",
	"g","GC","gear","get_comments","gfbdat","gfbsurvey","gigflds","glmfit","globo","gma.popymr","group","Gv2",
	"habSpp", "hadley","hclust","hide0","HSreg","HStrc",
	"idxD","idxL","idxM","importRes","inone","IRRmin","is.pred","isobath","iters",
	"k","K",
	"lang","las","lastSerial","lin","lmres","locality.plus","loess.smooth","ltype","lwd",
	"m","major","major.pop","mar","mata","maxit","mdbName","MDL","melt.data.frame","method","mfrow","mgp","minor","mmor","modelCheck","modelCompile","modelData","modelGenInits","modelUpdate","modT","mpd","msd","mu",
	"nage","Nat","Nboot","nc","nC","nd","nepacLL","nepacLLhigh","nper","nRP","nspec","nT","nyr",
	"obs","oma","orats","orfhistory","out",
	"pa.obs","pa.raw","parVec","path","pathN","PBSdat","PBStool","PBSmin","Pfig","pfout.e","phi","pi.obs","pix","pjsa","PLIM","pmfc","PMFC","pmon","pnam","poi","pop.age","pop.pmr.qcss","popa","powr","prefix","pset","psize","Pstart","pwd","pyr",
	"q2","qboxplot","qq","qtName","qu","quants",
	"rate","rates","redo.currentMCMC","reltol","repN","rho","RkB","Roff","RPmax","RPmin","ryrs",
	"s1","s2","samplesGetFirstChain","samplesGetLastChain","samplesHistory","samplesSet","SB","Scat","Sdat","sdate","seepa","seepi","set","setNames","sex","SG","SGdat","showC","showD","showE","showH","showL","showM","showQ","SID","sigma","so","species","speciesname","spn","spp","srfa","srfs","SSB","ssid","SSID","steptol","sthin","stock","storageID","strArea","strat","stratum","stratum2006","strGear","strSpp","strYear","stype","survey","surveyname","surveys","synflds",
	"t0","tabDir","tau","tcl","tdate","theta","tran","triflds","truehist","trusted","tstar","ttype","type","type.convert",
	"Ugear","uid","Umajor","Usex","Usrfa","Usrfs","Ustype","Uttype",
	"VBdat","Vend","vessel","Vstart",
	"wgta","wgtat","wmf","wted",
	"x","xavgRP","xavgTS","xfun","XLIM","xlsName","xobs","xtable","xwid","xy","XYopt",
	"y","year","years","yfld","yfun","yinf","Yinf","ylim","YLIM","Ymax","Ymin","Ymod","ypos","yr","ystar","ytck",
	"z","Z","za","zero","zfill","Zlim","zsho","zstar","zstn"
	), package="PBStools")


##quantBox------------------------------2018-04-03
## Redefine boxplot to show quantiles (RH 150910)
## From Jake Bowers, Dept of Political Science, UC-Berkeley
## https://stat.ethz.ch/pipermail/r-help/2001-November/016817.html
##----------------------------------------------RH
local(envir=.PBStoolEnv,expr={
	myboxplot.stats <- function (x, coef=NULL, do.conf=TRUE, do.out=TRUE)
	{
		nna <- !is.na(x)
		n <- sum(nna)
		if (!exists("quants5"))
			quants5 = c(0.05,0.25,0.50,0.75,0.95)
		stats <- quantile(x, quants5, na.rm=TRUE) ## one day figure out how to make this dynamic
		iqr <- diff(stats[c(2, 4)])
		out <- x < stats[1] | x > stats[5]
		conf <- if (do.conf)
			stats[3] + c(-1.58, 1.58) * diff(stats[c(2, 4)])/sqrt(n)
		list(stats=stats, n=n, conf=conf, out=x[out & nna])
	}
	boxcode = deparse(boxplot.default)
	boxcode = gsub("boxplot\\.stats","ttcall(myboxplot.stats)",boxcode)
	eval(parse(text=c("qboxplot=",boxcode)))
})
## Shortcuts for RH:
assign("so", function(x, p="tools", ddir="C:/Users/haighr/Files/Projects/R/Develop/"){
	source(paste0(ddir, "PBS", p, "/Authors/Rcode/develop/", x)) }, envir=.PBStoolEnv) ## source
assign("qu", function(x, p="tools", ddir="C:/Users/haighr/Files/Projects/R/Develop/", ...){
	do.call("getData", args=list(fqtName=x, path=paste0(ddir, "PBS", p, "/Authors/SQLcode"), ...)) }, envir=.PBStoolEnv) ## query

