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

A complete user guide 'PBStools-UG.pdf' is located at 
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
	"A","acut","adat","Afile","age","agg","AID","amax","aModel","areas","autoA","autoD","aVal",
	"B","b0","b5","barcol","bathy","bbonly","Bboot","bcol","bdat","Best","bgsamp","bgtab","bh","bio440","Bobs","boot","boot.ci","boxpars","boxwidth","Bqnt","Bt","bta","Btot","bugsData",
	"C1","C2","case","cast","cat440rec","catch","Cbar","Cbox","Ccat","ccol","cdata","Ceq","chn1","chn2","clara","clen","Clin","clwd","Cnum","cobs","Cpoi","cpuTime","cthin","ctot","Ctrd","Cval",
	"dat","DATA","datt","dBars","dbName","ddat","diaDir","disA","DLIM",
	"edat","elapTime","empinf","eN","eps","evals",
	"FID","figDir","fillBars","firstSerial","fminE","fminS","fnam","Fout","formalArgs","fqtName","Ft","ftime","func",
	"g","gear","gfbdat","glmfit","gma.popymr","group","Gv2",
	"hadley","hide0","HSreg","HStrc",
	"idxD","idxL","idxM","inone","IRRmin","is.pred","isobath","iters",
	"k","K",
	"lang","las","lastSerial","lin","lmres","locality.plus","ltype","lwd",
	"m","major","mar","mata","maxit","mdbName","MDL","melt.data.frame","method","mfrow","mgp","minor","mmor","modelCheck","modelCompile","modelData","modelGenInits","modelUpdate","modT","mpd","mu",
	"nage","Nat","Nboot","nc","nC","nd","nepacLL","nepacLLhigh","nper","nRP","nspec","nT","nyr",
	"obs","oma","orats","orfhistory","out",
	"pa.obs","pa.raw","parVec","path","pathN","PBSdat","PBStool","PBSmin","Pfig","pfout.e","phi","pi.obs","pix","pjsa","PLIM","pmfc","PMFC","pmon","pnam","poi","pop.age","pop.pmr.qcss","popa","powr","prefix","pset","psize","Pstart","pwd","pyr",
	"q2","qboxplot","qq","qtName","quants",
	"rate","rates","reltol","repN","rho","RkB","Roff","RPmax","RPmin","ryrs",
	"s1","s2","samplesGetFirstChain","samplesGetLastChain","samplesHistory","samplesSet","SB","Scat","Sdat","sdate","seepa","seepi","set","sex","SG","SGdat","showC","showD","showE","showH","showL","showM","showQ","SID","sigma","species","spn","spp","srfa","srfs","SSB","steptol","sthin","storageID","strArea","strat","strGear","strSpp","strYear","stype","surveys",
	"t0","tabDir","tau","tcl","tdate","theta","tran","truehist","trusted","tstar","ttype","type",
	"Ugear","uid","Umajor","Usex","Usrfa","Usrfs","Ustype","Uttype",
	"VBdat","Vend","vessel","Vstart",
	"wgta","wgtat","wmf","wted",
	"x","xfun","XLIM","xlsName","xobs","xtable","xwid","xy","XYopt",
	"y","year","years","yfld","yfun","yinf","Yinf","ylim","YLIM","Ymax","Ymin","Ymod","ypos","yr","ystar","ytck",
	"z","Z","za","zero","zfill","Zlim","zsho","zstar","zstn"
	), package="PBStools")

##quantBox------------------------------2018-04-03
## Redefine boxplot to show quantiles (RH 150910)
## http://r.789695.n4.nabble.com/Box-plot-with-5th-and-95th-percentiles-instead-of-1-5-IQR-problems-implementing-an-existing-solution-td3456123.html
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
		list(stats = stats, n = n, conf = conf, out = x[out & nna])
	}
	boxcode = deparse(boxplot.default)
	boxcode = gsub("boxplot\\.stats","ttcall(myboxplot.stats)",boxcode)
	eval(parse(text=c("qboxplot=",boxcode)))
})


