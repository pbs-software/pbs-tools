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
http://code.google.com/p/pbs-software/

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
	"A","acut","adat","Afile","age","agg","AID","amax","areas","autoA","autoD","aVal",
	"B","b0","b5","barcol","bathy","bbonly","Bboot","bcol","bdat","Best","bgsamp","bgtab","bh","bio440","Bobs","Bqnt","Bt","bta","Btot","bugsData",
	"C1","C2","case","cat440rec","catch","Cbar","Cbox","Ccat","ccol","cdata","Ceq","chn1","chn2","clen","Clin","clwd","Cnum","cobs","Cpoi","cpuTime","cthin","ctot","Ctrd","Cval",
	"dat","datt","dBars","dbName","ddat","disA","DLIM",
	"elapTime","eN","eps","evals",
	"fillBars","firstSerial","fminE","fminS","fnam","Fout","fqtName","Ft","ftime","func",
	"g","glmfit","gma.popymr","Gv2",
	"hide0","HSreg","HStrc",
	"idxD","idxL","idxM","IRRmin","is.pred","isobath","iters",
	"k",
	"las","lastSerial","lin","lmres","ltype","lwd",
	"m","major","mar","mata","maxit","mdbName","MDL","method","mfrow","mgp","mmor","modelCheck","modelCompile","modelData","modelGenInits","modelUpdate","modT","mpd","mu",
	"nage","Nat","Nboot","nc","nC","nd","nepacLL","nepacLLhigh","nper","nRP","nspec","nT","nyr",
	"obs","oma","orfhistory",
	"pa.obs","pa.raw","parVec","path","pathN","PBSdat","PBStool","PBSmin","Pfig","phi","pi.obs","pix","pjsa","PLIM","pmfc","pmon","pnam","poi","pop.age","pop.pmr.qcss","popa","powr","prefix","pset","psize","Pstart","pwd","pyr",
	"q2","qq","qtName","quants",
	"rate","rates","reltol","repN","rho","RkB","Roff","RPmax","RPmin","ryrs",
	"s1","s2","samplesGetFirstChain","samplesGetLastChain","samplesHistory","samplesSet","SB","Scat","Sdat","sdate","seepa","seepi","set","sex","SG","SGdat","showC","showD","showE","showH","showL","showQ","SID","sigma","species","spn","spp","SSB","steptol","sthin","storageID","strArea","strat","strGear","strSpp","strYear","stype","surveys",
	"t0","tcl","tdate","theta","tran","trusted","tstar","ttype","type",
	"Ugear","uid","Umajor","Usex","Usrfa","Usrfs","Ustype","Uttype",
	"VBdat","Vend","vessel","Vstart",
	"wgta","wgtat","wmf","wted",
	"x","xfun","XLIM","xlsName","xobs","xwid","xy","XYopt",
	"y","year","years","yfld","yfun","yinf","ylim","YLIM","ypos","yr","ystar","ytck",
	"z","Z","za","zero","zfill","Zlim","zsho","zstar","zstn"
	), package="PBStools")

