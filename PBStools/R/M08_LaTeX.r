## =============================================================================
## Module 8 : LaTeX Crap
## ---------------------
##  collectFigs.....Collect encapsulated postscript figures into one document;
##  formatCatch.....Format numeric table so that entries display N significnat figures;
##  makeLTH.........Make a longtable header for printing an xtable;
##  splitTab........Split a long data table into side-by-side pieces for printing in LaTeX;
##  texArray........Flatten and format an array for latex output;
##  texThatVec......Convert a vector to a phrase 'x, y, and z'.
##==============================================================================

#collectFigs----------------------------2016-10-17
# Collect encapsulated postscript figures into one document.
#-----------------------------------------------RH
collectFigs <- function(path=".", ext="eps", is.fnum=FALSE, 
   fout="collectFigs", width=6.5, capskip=0, pattern=NULL, show.output=FALSE)
{
	cwd = getwd(); on.exit(setwd(cwd))
	suffix = paste("\\.",ext,"$",sep="")
	figs = list.files(path=path,pattern=suffix)
	if (!is.null(pattern) && pattern!="")
		figs = findPat(pattern,figs)
	figs = sort(figs); Nfigs = length(figs)
	fnam = fcap = gsub(suffix,"",figs)
	if (is.fnum)  # figure number already in file name
		fcap = sub("-",": ",fcap) 
	#fcap = gsub("-"," ",fcap)
	fcap = gsub("&","\\\\&",fcap)

	# Start build tex file
	ftex = c(
	"\\documentclass[12pt]{article}",
	"\\usepackage[top=0.8in, bottom=0.7in, left=1in, right=1in]{geometry} %  page margins",
	"\\usepackage{graphicx}",
	"\\usepackage[font=sf,labelfont=bf,labelsep=period,justification=raggedright]{caption}",
	"\\usepackage{hyperref}",
	paste0("\\captionsetup[figure]{position=bottom,skip=",capskip,"pt}"),
	"\\DeclareCaptionTextFormat{bookmark}{\\belowpdfbookmark{Fig \\thefigure. #1}{\\thefigure}#1}",
	"\\captionsetup{textformat=bookmark}",
	"\\makeatletter\\setlength{\\@fptop}{0pt}\\makeatother  %remove whitespace at top of page",
	"",
	"\\newcommand\\pbsfig[2]{ % filename is #1, text is #2",
	"  \\begin{figure}[t!]",
	"  \\begin{center}",
	paste0("  \\includegraphics[width=",width,"in,height=",width,"in,keepaspectratio=TRUE]{{#1}.eps}\\\\"),
	"  \\end{center}",
	"  \\caption{#2 }",
	"  \\label{fig:#1}",
	"  \\end{figure} }",
	"",
	"\\begin{document}"
	)
	for (i in 1:Nfigs) {
		ftex = c(ftex,paste("\\pbsfig{",fnam[i],"}{",fcap[i],"}",sep=""))
		if (i%%6==0) ftex = c(ftex, "\\clearpage")
	}
	ftex = c(ftex, "\\end{document}")
	writeLines(ftex,paste(path,"/",fout,".tex",sep=""))
	setwd(path)
	debris = list.files(pattern=paste("^",fout,sep=""))
	debris = setdiff(debris,paste(fout,".tex",sep=""))
	if (length(debris)>0)  file.remove(debris)
	#err = system(command=paste("latex ",fout,".tex",sep=""), wait=TRUE, show.output.on.console=show.output)
#browser();return()
	err = system2("latex",args=c(paste0(fout,".tex")), wait=TRUE, stdout=show.output)
		if (err>0) stop("===== First latex call failed =====")
	#err = system(command=paste("latex ",fout,".tex",sep=""),wait=TRUE,show.output.on.console=show.output)
	err = system2("latex",args=c(paste0(fout,".tex")), wait=TRUE, stdout=show.output)
		if (err>0) stop("===== Second latex call failed =====")
	# Create a ps file from a dvi file
	#err = system(command=paste("dvips ",fout,".dvi",sep=""),wait=TRUE,show.output.on.console=show.output)
	err = system2("dvips",args=c(paste0(fout,".dvi")), wait=TRUE, stdout=show.output)
		if (err>0) stop("===== The dvips call failed =====")
	# Create a pdf from a ps file
	#err = system(command=paste("ps2pdf ",fout,".ps",sep=""),wait=TRUE,show.output.on.console=show.output)
	err = system2("ps2pdf",args=c(paste0(fout,".ps")), wait=TRUE, stdout=show.output)
		if (err>0) stop("===== The ps2pdf call failed =====")
	invisible(ftex)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~collectFigs


## formatCatch--------------------------2023-07-24
## Format a numeric table so that each cell 
## displays N significant figures in character format.
## Arguments:
##  dat  = dataframe/matrix of raw catch numbers
##  N    = number of significant digits
##  X    = numeric vector of columns to exclude
##  zero = character replacement for zero-values
##  na   = character replacement for NAs
##  K    = big mark separator (see 'format')
##  exInt = exclude integers from manipulation to signicant digits
##  use.round = if TRUE, round catches to N decimal places.
## ---------------------------------------------RH
formatCatch <- function(dat, N=3, X=0, zero="0", na="---",
   K=",", exInt=TRUE, use.round=FALSE, scipen=20)
{
	scipen.old = options()$scipen
	on.exit(options(scipen=scipen.old))
	options(scipen=scipen)

	makeCmat  <- function(x,colname="Y") {
		matrix(x,ncol=1,dimnames=list(names(x),colname)) }
	makeRmat  <- function(x,rowname="Y") {
		matrix(x,nrow=1,dimnames=list(rowname,names(x))) }

	if (is.vector(dat)) {
		#datnam = as.character(substitute(dat))
		dat = matrix(dat,ncol=1); #colnames(dat)=datnam
	}
	dnam = dimnames(dat)
	if (is.null(dnam)) {
		dnam=list(paste("R",1:nrow(dat),sep=""),paste("C",1:ncol(dat),sep=""))
		dimnames(dat) = dnam
	}
	if (length(dnam[[2]])!=length(.su(dnam[[2]]))) {
		dnam[[2]] = paste("C",1:ncol(dat),".",dnam[[2]],sep="")
		dimnames(dat) = dnam
	}
	# When user wants to convert all columns
	if (is.null(X) || all(X==0)) X = -(1:ncol(dat))
#browser();return()
	# Check for character columns and exclude
	strcol = sapply(dat[,-(X)],is.character)
	if (any(strcol)) X = grep(TRUE,strcol)  ## (RH 230731)
	
	# Detect, remember, and remove negative signs
	nadat  = is.na(dat[,-(X),drop=FALSE])
	if(use.round) {
		cdat = dat[,-(X),drop=FALSE] 
		#zdat = dat[,-(X),drop=FALSE]==0 & !is.na(dat[,-(X),drop=FALSE])
		zdat = cdat==0 & !is.na(cdat)
		## Attempt to deal with mixtures of catches (>0) and probabilities (<0) in decision tables (RH 230724)
		z1000 = abs(cdat) >= 1000.
		if (any(z1000)) {
			big.mark = ifelse(is.null(options()$big.mark), ",", options()$big.mark)
			c1000 = formatC(round(cdat[z1000],0), digits=0, format="fg", big.mark=big.mark)  ## converts big numbers to integers
		}
		#cdat[,-(X)][!nadat] = show0(round(cdat[,-(X)][!nadat],N), N, add2int=!exInt)
		cdat[!nadat] = show0(round(cdat[!nadat],N), N, add2int=!exInt)
#browser();return()

		## Re-label small values that look like 0 when rounded and big (100%) values that look like 1
		zsmall = dat[,-(X)]>0 & round(dat[,-(X)],N)==0 & !is.na(dat[,-(X)])
		zbig   = dat[,-(X)]<1 & round(dat[,-(X)],N)==1 & !is.na(dat[,-(X)])
		#if (any(zsmall)) cdat[,-(X)][zsmall] = paste0("<",10^(-N))
		#if (any(zbig))   cdat[,-(X)][zbig]   = paste0(">",1-10^(-N))
		#cdat[,-(X)][zdat|nadat] = zero  ## temporarily overwrite NAs as zeroes but next line adjust this.
		#cdat[,-(X)][nadat]      = na
		if (any(zsmall)) cdat[zsmall] = paste0("<",10^(-N))
		if (any(zbig))   cdat[zbig]   = paste0(">",1-10^(-N))
		cdat[zdat|nadat] = zero  ## temporarily overwrite NAs as zeroes but next line adjust this.
		cdat[nadat]      = na
		## final check for NAs (usually in excluded columns X)
		if (any(is.na(cdat)))
			cdat[is.na(cdat)] = na
		if (any(z1000))
			cdat[z1000] = c1000
		dat[,-(X)] = cdat
#browser();return()
		return(dat)  ## ends 'formatCatch if using 'use.round'
	}
	
	negdat = dat[,-(X),drop=FALSE] < 0 & !nadat
#browser();return()
	dat[,-(X)][negdat] = -dat[,-(X)][negdat]
	if (class(dat)[1]=="cast_df") attr(dat,"class") = "data.frame"

	flat=FALSE
	if (nrow(dat)==1) flat=TRUE
	cdat = apply(dat,2,function(x){xx=as.character(x); xxx=sub("^\\s+|\\s+$","",xx); return(xxx)}) ## (RH 210427) get rid of leading and trailing spaces
	if (flat)
		cdat = makeRmat(cdat,rownames(dat))
	cdat = as.data.frame(cdat); dimnames(cdat) = dnam
	for (i in dnam[[2]][-(X)]) {
		idat = cdat[,i]
		istr = strsplit(as.character(idat),split="\\.")
		inum = sapply(istr, function(x,N,K,zero,na){
			if (is.na(x[1])) return(na)
			x1 = format(round(as.numeric(x[1])),big.mark=K,trim=TRUE,scientific=FALSE)
			X1 = format(as.numeric(paste(x,collapse=".")),digits=N,big.mark=K,trim=TRUE,scientific=FALSE)
			if (x1=="0" && X1=="0") return(zero) ## (RH 151113) catch instances when 0 has been changed to "0.00000" by as.character
			n2 = N - nchar(x[1])
			if (length(x)==1) {
				if (x[1]=="0")  ival = zero
				else if(exInt)  ival = format(as.numeric(x[1]), big.mark=K)
				else if (n2>=1) ival = format(as.numeric(gsub(",","",x1)),nsmall=n2,big.mark=K,trim=TRUE,scientific=FALSE)
				else ival = x1
			}
			else {
				if (nchar(x[1])>=N) ival = X1
				else {
					if (x1=="0") {
						## exclude leading zeroes using 'clipVector' from 'PBSmodelling' package
						x2ex0 = paste(clipVector(strsplit(x[2],split="")[[1]],"0",1),collapse="")
						n20   = nchar(x[2])-nchar(x2ex0) ## number of leading zeroes
						if (N-nchar(x2ex0) > 0 )
							x[2] = paste(x[2],paste(rep(0,N-nchar(x2ex0)+1),collapse=""),sep="")
						n2 = N + n20 ## adjusted for leading zeroes
					}
					else {
						n2 = N - nchar(x[1])
					}
					x[2] = paste0(x[2],paste(rep(0,N+1),collapse="")) ## pad with extra zeroes as a precautionary measure
					x2 = pad0(round(as.numeric(substring(x[2],1,n2+2))/100),n2)
					if (as.numeric(x2) %in% 10^(0:10)) ival = format(as.numeric(gsub(",","",X1)),nsmall=n2-as.numeric(is.element(as.numeric(x1),10^(1:10)-1)),big.mark=K,trim=TRUE,scientific=FALSE)
					else ival = paste(x1,x2,sep=".")
				}
			}
			return(ival)
		},N=N,K=K,zero=zero,na=na)
		cdat[[i]] = inum
	}
	# Resore the negative sign
	cdat[,-(X)][negdat] = paste("-",cdat[,-(X)][negdat],sep="")
#browser();return()
	return(cdat)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~formatCatch


#makeLTH--------------------------------2016-10-24
# Make a longtable header for Sweave, source:
# http://tex.stackexchange.com/questions/41067/caption-for-longtable-in-sweave?rq=1
#-----------------------------------------------RH
makeLTH <- function(xtab.table, table.caption, table.label, struts=FALSE, add.continue = FALSE)
{
	longtable.header <- paste(
		paste("\\caption{", table.caption, "}",sep = "", collapse = ""),
		paste("\\label{", table.label, "}\\\\ ",sep = "", collapse = ""),
		"\\hline\\\\[-2.2ex] ",
		attr(xtab.table, "names")[1],
		paste(" & ",attr(xtab.table, "names")[2:length(attr(xtab.table, "names"))],collapse="",sep=""),
		paste0(ifelse(struts," \\Tstrut\\Bstrut ",""),"\\\\[0.2ex]\\hline\\\\[-1.5ex] "),
		"\\endfirsthead ",
		if (add.continue)
			paste0("\\multicolumn{",ncol(xtab.table),"}{l}{{\\tablename\\ \\thetable{} -- continued from previous page}}\\\\ "),
		"\\hline ",
		attr(xtab.table, "names")[1],
		paste(" & ",attr(xtab.table, "names")[2:length(attr(xtab.table, "names"))],collapse="",sep=""),
		paste0(ifelse(struts," \\Tstrut\\Bstrut ",""),"\\\\[0.2ex]\\hline\\\\[-1.5ex] "),
		"\\endhead ",
		"\\hline\\\\[-2.2ex] ",
		if (add.continue)
			paste("\\multicolumn{",ncol(xtab.table),"}{r}{{\\footnotesize \\emph{Continued on next page}}}\\\\ ",sep=""),
		#"\\hline \\endfoot ",
		"\\endfoot ","\\hline \\endlastfoot ",collapse = "")
	return(longtable.header)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~makeLTH


#splitTab-------------------------------2016-10-18
# Split a long data table into side-by-side pieces
# for printing in LaTeX.
#-----------------------------------------------RH
splitTab <- function(tab, np=3, row.names=TRUE, row.label="row", row.numeric=FALSE)
{
	## Deal with the rows
	nr = nrow(tab)
	nb = ceiling(nr/np)
	er = nb * (1:np)
	br = er - nb + 1
	er[np] = nr

	## Deal with the columns
	nc = ncol(tab)
	cnam = dimnames(tab)[[2]]
	if(row.names){
		nc = nc + 1
		cnam = c(row.label,cnam)
	}
	nC = nc * np
	ec = nc * (1:np)
	bc = ec - nc + 1

	## Create a new data frame repository and populate
	newtab = as.data.frame(array(NA,dim=c(nb,nC),dimnames=list(1:nb,rep(cnam,np))),check.names=FALSE,stringsAsFactors=FALSE)
	for (i in 1:np) {
		irow = br[i]:er[i]
		icol = bc[i]:ec[i]
		itab  = tab[irow,]
		if (row.names) {
			inam  = rownames(itab)
			if (row.numeric)
				inam = as.numeric(inam)
			newtab[1:nrow(itab),icol] = data.frame(inam,itab)
		} else {
			newtab[1:nrow(itab),icol] = data.frame(itab)
		}
	}
	return(newtab)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~splitTab


## texArray-----------------------------2025-01-03
## Flatten and format an array for latex output.
## ---------------------------------------------RH
texArray <- function(x, table.caption="My table", table.label="tab:mytable",
   strSpp=NULL, sigdig=3, zero="---", exInt=TRUE, use.round=FALSE, 
   collab=NULL, dash.delim=NULL, tablewidth=6.5,
   rm.empty=FALSE, start.page=1, ignore.col=NULL,
   select.rows=NULL, use.row.names=FALSE, name.row.names="row",
   add.header.column=FALSE, new.header=NULL, outnam="mytable", 
   alignHRC=c("l","l","r"), italics.file=NULL, uscore=" ", ...)
{
	if (!is.array(x) && !is.matrix(x) && !is.data.frame(x)) stop("input an array or a matrix")
	N = dim(x)
	L = length(N)
	fn.select.rows <- function(x,r) {  # select rows from 2-dimensional table
		if (is.null(r)) return(x)
		if (is.numeric(r))
			s = x[r,]
		else
			s = x[is.element(rownames(x),r),]
		return(s)
	}
	for (i in 1:L) {
		if (is.null(dimnames(x)[[i]])) 
			dimnames(x)[[i]] = paste("R",1:N[i],sep="")
		if (is.null(names(dimnames(x))[i]) || is.na(names(dimnames(x))[i]) )
			names(dimnames(x))[i] = paste("D",i,sep="")
	}
	if (L==2) { #2-dimensional (flat) table
		add.header.column=FALSE
		is.fac=sapply(x,function(x){any("factor" %in% class(x))})
		if (any(is.fac)) {
			no.fac = as.data.frame(apply(x[,is.fac,drop=FALSE],2,as.numeric))
			xnum = as.data.frame(array(0,dim=dim(x),dimnames=dimnames(x)))
			xnum[,!is.fac] = x[,!is.fac]
			xnum[,is.fac]  = no.fac
			x = xnum
		}
		poo = fn.select.rows(x,select.rows)
		goo = poo
#browser();return()
	} ## end 2D table
	else {
		Z = N[3:L]; names(Z) = 3:L
		Zsort = sort(Z)
		LL = length(Zsort) # temporary
		Dsort = sapply(Zsort,function(x){1:x},simplify=FALSE)
		if (LL==1) {
			foo = matrix(Dsort[[1]],ncol=1)
			colnames(foo)="3"
		} else {
			mess = character()
			for (i in 1:LL) {
				mess = c(mess,paste("d",i," = rep(Dsort[[",i,"]],",ifelse(i==1,"each=",""),"prod(Zsort[-",i,"]))",sep=""))
			}
			mess = c(mess, paste("foo = cbind(",paste("d",1:LL,collapse=",",sep=""),")",sep=""))
			mess = c(mess, paste("foo = foo[order(",paste("foo[,",1:LL,"]",collapse=",",sep=""),"),]",sep=""))
			eval(parse(text=paste(mess,collapse=";")))
			colnames(foo) = names(Zsort)
			foo = foo[,order(colnames(foo))]
		}
		if (add.header.column) {
			if (!is.null(new.header)) {
				c1 = new.header
#browser();return()
			} else {
				here = lenv()
				data("pmfc", package="PBSdata", envir=here)
				dnams = dimnames(x)[as.numeric(names(Zsort))]
				dlist = sapply(names(dnams),function(d,dd){paste(d,dd[[d]],sep=":")},dd=dnams,simplify=FALSE)
				names(dlist) = names(Zsort)
				for (i in names(dlist)){ 
					dtmp=dlist[[i]][foo[,i]]
					if (i==names(dlist)[1]) doo=matrix(dtmp,ncol=1) else doo=cbind(doo,dtmp)
				}
				c1 = apply(doo,1,paste,collapse=" ")
				c1 = gsub("ttype:14","Com",c1,fixed=TRUE)
				c1 = gsub("ttype:1|4","Com",c1,fixed=TRUE)
				c1 = gsub("ttype:23","R/S",c1,fixed=TRUE)
				c1 = gsub("ttype:2|3","R/S",c1,fixed=TRUE)
				c1 = gsub("ttype:1","Com.Unob",c1,fixed=TRUE)
				c1 = gsub("ttype:2","Surv.Res",c1,fixed=TRUE)
				c1 = gsub("ttype:3","Surv.Cht",c1,fixed=TRUE)
				c1 = gsub("ttype:4","Comm.Obs",c1,fixed=TRUE)
				c1 = gsub("area:major_","PMFC ",c1,fixed=TRUE)
				c1 = gsub("area:major","PMFC ",c1,fixed=TRUE)
				lablist = strsplit(c1," ")
				c1 = sapply(lablist,function(x){
					if (any(x=="PMFC")){
						z=grep("|",x,fixed=TRUE)
						if(length(z)>0)
							p=pmfc[strsplit(x[z],"|",fixed=TRUE)[[1]],"gmu"]
						else{
							z=grep("PMFC",x)+1
							p=pmfc[strsplit(x[z],"")[[1]],"gmu"]
						}
#browser(	);return()
						labmat=matrix(unlist(strsplit(p,"")),ncol=2,byrow=TRUE)
						labor=split(labmat[,2],labmat[,1])
						x[z]=paste(paste(names(labor),sapply(labor,paste,collapse=""),sep=""),collapse="+")
						paste(x,collapse=" ")
					} else {
						x
					}
				})
				c1 = gsub("PMFC ","",c1,fixed=TRUE)
				c1 = gsub("3CD+5ABCDE","Coast",c1,fixed=TRUE)
				c1 = gsub("stat:Ntid","Number of trips",gsub("stat:Scat","Sample catch (t)",gsub("stat:Fcat","Fisheries catch (t)",c1)))
			}
		}  ## end if loop (add.header.column)
		for (k in 1:nrow(foo)) {
			koo = foo[k,]
			xoo = paste("x[,,",paste(koo,collapse=","),"]",sep="")
			poo = eval(parse(text=xoo))
			poo = fn.select.rows(poo,select.rows)
			#if (!is.null(select.rows)){
			#	if (is.numeric(select.rows))
			#		poo = poo[select.rows,]
			#	else
			#		poo = poo[is.element(rownames(poo),select.rows),]
			#}
			if (!is.null(dim(poo)))
				rownames(poo)=paste(k,rownames(poo),sep="-")
			#bad = apply(poo,1,function(x){all(is.na(x))})
			#poo = poo[!bad,drop=FALSE,exact=TRUE]
			if (k==1) goo=poo
			else      goo = rbind(goo,poo)
		}  ## end k loop (foo)
	}  ## end 3D+ table

	#----START making Latex crap-----------------------------
	#texout = paste("LenWt-",strSpp,".tex",sep="")
	texout = paste(outnam,ifelse(is.null(strSpp),"","-"),strSpp,".tex",sep="")
	texmess = c(
		"\\documentclass[11pt]{book}",
		"\\usepackage[top=1in, bottom=1in, left=1in, right=1in]{geometry}",
		"\\usepackage[format=plain, indention=0.5cm, labelsep=period, font={normalsize}, justification=raggedright, singlelinecheck=false]{caption}",
		"\\captionsetup[table]{position=above, skip=5pt}",
		"\\usepackage{amsmath}",
		"\\raggedright",
		"\\newcommand\\Tstrut{\\rule{0pt}{2.6ex}}       % top strut for table row",
		"\\newcommand\\Bstrut{\\rule[-1.1ex]{0pt}{0pt}} % bottom strut for table row",
		"\\usepackage{longtable,array} % need array when specifying a ragged right column",
		"\\setlength{\\LTleft}{0pt}     % left justify longtable",
		paste("\\setcounter{page}{", start.page, "}  % set the page numbering", sep=""),
		"\\usepackage{arydshln} % dashed lines in tables (load after other shit)",
		"\\usepackage[T1]{fontenc}",
		"\\usepackage[scaled=1.00]{uarial}",
		"\\begin{document}",
		"\\renewcommand{\\familydefault}{ua1}",
		"\\renewcommand{\\rmdefault}{ua1}",
		"\\usefont{\\encodingdefault}{\\familydefault}{\\seriesdefault}{\\shapedefault}\\small",
		"\\setlength{\\tabcolsep}{0pt}"
	)
	writeLines(texmess,texout)

	if (!requireNamespace("xtable", quietly=TRUE)) stop("`xtable` package is required")
	if (is.null(collab)) collab = colnames(goo)
	else if (length(collab)==ncol(goo)) colnames(goo) = collab
	else stop("Length of `collab` does not match number of columns")
	#goonum = sapply(goo,function(x){grepl("numeric",class(x))}) 
	goonum = sapply(goo, is.numeric)
	if (any(goonum)) {
		if (all(goonum)) X=0
		else X = grep(FALSE,goonum)
		if (!is.null(ignore.col))
			X = union(X[X!=0],ignore.col)
		goo = formatCatch(goo, N=sigdig, zero=zero, X=X, use.round=use.round, exInt=exInt)
	}
	colnames(goo)=collab ## sapply can screw up colnames if they are not unique

	if (use.row.names) {
		rows = dimnames(goo)[[1]]
		if (L>2) { ## higher than two-dimensional array
			rows = sapply(strsplit(rows,"-"),function(x){paste(x[-1],sep="",collapse="-")})
			rows = gsub("Females","F",gsub("Males","M",rows))
		}
		goo = data.frame(rows,goo,check.names=FALSE,stringsAsFactors=FALSE)
		dimnames(goo)[[2]][1] = name.row.names
	}
	if (add.header.column) {
		Ngroups = prod(Zsort)
		NrecsNg = nrow(goo)/Ngroups  ## number records per group
		NrecsNg  = nrow(goo)/Ngroups
		description = as.vector(matrix(c(c1,rep("",Ngroups*(NrecsNg-1))),ncol=Ngroups,byrow=TRUE))
		goo = data.frame(description,goo,check.names=FALSE,stringsAsFactors=FALSE)
		if (!is.null(new.header))
			colnames(goo)[1] = as.character(substitute(new.header))  ## attempt to avoid blank colnames (RH 230724)
		else 
			colnames(goo)[1] = "group"  ## CSAP does not like blank header names
		if (use.row.names) names(goo)[2] = name.row.names
	}
	ncol = dim(goo)[[2]] - as.numeric(use.row.names) - as.numeric(add.header.column)
	if (rm.empty) {
		keep = apply(goo[,(ncol(goo)-ncol+1):ncol(goo),drop=FALSE],1,function(x){any(!x%in%zero & !x%in%"---")})
		goo = goo[keep,,drop=FALSE]  ## remove 'empty' rows
		keep = apply(goo,2,function(x){any(!x%in%zero & !x%in%"---" & !is.na(x))})
		goo = goo[,keep,drop=FALSE]  ## remove 'empty' columns
		if (exists("description")) {
			new.head.pos = !duplicated(rep(1:Ngroups,each=NrecsNg)[keep])
			goo[new.head.pos,1] = c1
		}
	}
	if (is.null(dash.delim) && L>2 && all(N>1)) {
		rowflag = sapply(strsplit(rownames(goo),"-"),function(x){as.numeric(x[1])})
		rows.line = grep(1,diff(rowflag))
	} else if (!is.null(dash.delim))
		rows.line = (1:nrow(goo))[is.element(substring(rownames(goo),ifelse(L>2,3,1)),dash.delim)]
	else
		rows.line=NULL
	rows.line = setdiff(rows.line,nrow(goo)) # don't draw a dashed line along the last row
#browser();return()  ##*****TO HERE

	ncol     = dim(goo)[[2]] - as.numeric(add.header.column) - as.numeric(use.row.names)
	## Length of align is one greater than ncol(x) if x is a data.frame
	#tabalign = paste0("c",ifelse(add.header.column, alignHRC[1], ""), ifelse(use.row.names, alignHRC[2], ""), paste(rep(alignHRC[3], ncol), collapse=""))
	tabalign = paste0(alignHRC[1], ifelse(add.header.column, alignHRC[1], ""), ifelse(use.row.names, alignHRC[2], ""), paste(rep(alignHRC[3], ncol), collapse=""))
	fldsize  = newsize = c(0,apply(goo,2,function(x){if (all(is.na(x))) 1 else max(nchar(x),na.rm=TRUE)})) # due to extra column nonsense
	heads    = gsub("\\\\newline(_)?"," ",gsub("\\s+","_",colnames(goo)))
	hdrsize  = sapply(strsplit(heads,split="[[:space:]]"),function(x){xx = nchar(x); if (length(xx)==0) 0 else(max(xx))})
	#hdrsize  = sapply(strsplit(heads,split="[[:space:][:punct:]]"),function(x){xx = nchar(x); if (length(xx)==0) 0 else(max(xx))})
	#fldsize  = newsize = c(0,apply(goo,2,function(x){if (all(is.na(x))) 0.1 else max(strwidth(x,font=1,units="in"),na.rm=TRUE)})) # due to extra column nonsense
	#hdrsize  = strwidth(colnames(goo),font=1,units="in")

	colnames(goo) = gsub("\\s+"," ",gsub("\\_",uscore,colnames(goo))) ## (RH 210420)
	goo = as.data.frame(sapply(goo,function(x){gsub("\\_",uscore,x)}))

	## Italicise words from italics.file, if supplied
	if (!is.null(italics.file) && file.exists(italics.file)){
		itals = unlist(read.table(italics.file,header=FALSE))
		icols = apply(goo,2,function(x){length(findPat(itals,x))>1})
		if (any(icols)) {
			ii = (1:ncol)[icols]
			for (j in ii) {
				jtals = itals[sapply(sapply(itals,findPat,goo[,j]),length)>0]
				jtmp = goo[,j]
				for (k in jtals){
					jtmp = gsub(k,paste0("\\\\textit{",k,"}"),jtmp)
				}
				goo[,j] = jtmp
			}
		}
	}

#browser();return()
	xtab = xtable::xtable(goo,align=tabalign)
	longtable.header = makeLTH(xtab,table.caption,table.label)
	if (!is.null(list(...)$add.to.row)) {
		add.to.row = list(...)$add.to.row
	} else if (length(rows.line)==0) {
		add.to.row = list(pos = list(-1, nrow(xtab)), command = c( longtable.header, "%"))
	} else {
		add.to.row = list(pos = list(-1, rows.line, nrow(xtab)), command = c( longtable.header, "\\hdashline[0.5pt/2pt]", "%"))
	}
	#if (!is.null(add.to.row)) {
	#	ADD.TO.ROW[["pos"]] = c(add.to.row[["pos"]], ADD.TO.ROW[["pos"]])
	#	ADD.TO.ROW[["command"]] = c(add.to.row[["command"]], ADD.TO.ROW[["command"]]) }

	print(xtab, 
		file = texout, append=TRUE,
		floating = FALSE, # longtable never floats
		hline.after = NULL,
		#add.to.row = list(pos = list(-1, nrow(xtab)), command = c(longtable.header, "%")),
		add.to.row = add.to.row,
		#add.to.row = list(pos = list(-1, rows.sara, nrow(xtab)), command = c(longtable.header, "\\rowcolor{rowclr}", "%")),
		include.rownames = FALSE,
		include.colnames = FALSE,
		type = "latex",
		tabular.environment = "longtable",
		sanitize.text.function = function(x){x},
		math.style.negative = FALSE
	)
	cat("\\end{document}\n",file=texout,append=TRUE)
	texfile = readLines(texout)
	ltdelim=grep("\\{longtable}",texfile)
	texfile[ltdelim[1]] = sub("\\{longtable}","{longtable}[c]",texfile[ltdelim[1]])

	## New (2017-05-10) by RH
	## \newcolumntype{L}[1]{>{\raggedright\let\newline\\\arraybackslash\hspace{0pt}}p{#1}}
	## \newcolumntype{C}[1]{>{\centering\let\newline\\\arraybackslash\hspace{0pt}}p{#1}}
	## \newcolumntype{R}[1]{>{\raggedleft\let\newline\\\arraybackslash\hspace{0pt}}p{#1}}
	## \begin{longtable}{L{1.5in}L{2.2in}C{0.55in}C{1.1in}C{0.7in}C{0.7in}L{1.5in}}
	LCR <- function(align, width, units="in") {
		if (!any(align==c("L","C","R"))) stop("Choose alignment 'L', 'C', or 'R'")
		mess = paste0(">{\\\\",
			ifelse(align=="L","raggedright",ifelse(align=="C","centering","raggedleft")),
			"\\\\let\\\\newline\\\\\\\\\\\\arraybackslash\\\\hspace{0pt}}p{",
			width, units, "}")
		return(mess)
	}
	fldalign = toupper(strsplit(tabalign,"")[[1]][-1])

	## RH (2017-05-18) Adjust column widths to fit data and allow for minimum width before allocating to table width
	colsize  = pmax(fldsize[-1],hdrsize)        ## compare nchar in data column and column header
	fldprop  = colsize/sum(colsize)             ## calculate colsize as a proportion
	minprop  = pmax(fldprop,1/ncol(goo))        ## make sure some fields are not too small by considering # columns as a proportion
	adjprop  = minprop/sum(minprop)             ## standardise the adjusted column proportions
	fldwidth = round(adjprop * tablewidth,2)    ## allocate column dimensions as proportion of table width
#browser();return()

	#bigmess  = paste0(sapply(1:length(fldwidth), function(N, a, w) { LCR(align=fldalign[N], width=fldwidth[N]) } ),collapse="")
	bigmess  = paste0(sapply(1:length(fldwidth), function(N) { LCR(align=fldalign[N], width=fldwidth[N]) } ),collapse="")
#browser();return()

	texfile[ltdelim[1]] = gsub(substring(tabalign,2,nchar(tabalign)),bigmess,texfile[ltdelim[1]])
	writeLines(texfile,sub("\\.tex","+.tex",texout))
	## set column separator to 0 because code has already optimized spacing for textwidth (RH 211102)
	tabfile=c("\\setlength{\\tabcolsep}{0pt}",texfile[ltdelim[1]:ltdelim[2]])
	invisible(list(goo=goo,texfile=texfile,tabfile=tabfile))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~texArray


## texThatVec-------------------------2018-05-07
##  Convert a vector to a phrase 'x, y, and z'
## ---------------------------------------------RH
texThatVec <- function(vec, simplify=TRUE)
{
	warn = options()$warn
	on.exit(options(warn=warn))
	options(warn=-1)
	if (length(vec)==1) return(paste0(vec))
	if (simplify) {
		uvec = .su(vec)
		if (is.character(uvec) && any(is.na(as.numeric(uvec))))
			cvec = uvec
		else {
			uvec = as.numeric(uvec)
			## User: A5C1D2H2I1M1N2O1R2T1 (140719)
			## https://stackoverflow.com/questions/24837401/find-consecutive-values-in-vector-in-r
			lvec = split(uvec,cumsum(c(1, diff(uvec) != 1)))
			cvec = sapply(lvec, function(x) {
				if (length(x)==1) return(paste0(x))
				else return(paste0(x[1],"-",rev(x)[1]))
			})
		}
	} else cvec = paste0(vec)
	texvec = if (length(cvec)>=2)
		paste0(paste0(cvec[1:(length(cvec)-1)], collapse=", "), ifelse(length(cvec)>2,",",""), " and ", rev(cvec)[1])
		else cvec
	return(texvec)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~texThatVec

