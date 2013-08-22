#===============================================================================
# Module 8 : tex Crap
# -------------------
#  collectFigs    Collect encapsulated postscript figures into one document.
#  formatCatch    Format numeric table so that entries display N significnat figures.
#  makeLHT        Make a longtable header for printing an xtable
#  texArray       Flatten and format an array for latex output.
#===============================================================================


#collectFigs----------------------------2013-07-19
#  Collect encapsulated postscript figures into one document.
#-----------------------------------------------RH
collectFigs = function(path=".", ext="eps", is.fnum=FALSE, 
   fout="collectFigs", width=6, capskip=0, pattern=NULL) {
	cwd = getwd(); on.exit(setwd(cwd))
	suffix = paste("\\.",ext,"$",sep="")
	figs = list.files(path=path,pattern=suffix)
	if (!is.null(pattern) && pattern!="")
		figs = findPat(pattern,figs)
#browser();return()
	figs = sort(figs); Nfigs = length(figs)
	fnam = fcap = gsub(suffix,"",figs)
	if (is.fnum)  # figure number already in file name
		fcap = sub("-",": ",fcap) 
	fcap = gsub("-"," ",fcap)
	fcap = gsub("&","\\\\&",fcap)

	# Start build tex file
	ftex = c(
	"\\documentclass[12pt]{article}",
	"\\usepackage[top=0.8in, bottom=0.7in, left=1in, right=1in]{geometry} %  page margins",
	"\\usepackage{epsfig}",
	"\\usepackage[font=sf,labelfont=bf,labelsep=period,justification=raggedright]{caption}",
	"\\usepackage{hyperref}",
	paste("\\captionsetup[figure]{position=bottom,skip=",capskip,"pt}",sep=""),
	"\\DeclareCaptionTextFormat{bookmark}{\\belowpdfbookmark{Fig \\thefigure. #1}{\\thefigure}#1}",
	"\\captionsetup{textformat=bookmark}",
	"",
	"\\newcommand\\pbsfig[2]{ % filename is #1, text is #2",
	"  \\begin{figure}[ht!]",
	"  \\begin{center}",
	paste("  \\epsfxsize=",width,"in",sep=""),
	"  \\epsfbox{#1.eps}",
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
	err = shell(cmd=paste("latex ",fout,".tex",sep=""),wait=TRUE)
		if (err>0) stop("===== First latex call failed =====")
	err = shell(cmd=paste("latex ",fout,".tex",sep=""),wait=TRUE)
		if (err>0) stop("===== Second latex call failed =====")
	# Create a ps file from a dvi file
	err = shell(cmd=paste("dvips ",fout,".dvi",sep=""),wait=TRUE)
		if (err>0) stop("===== The dvips call failed =====")
	# Create a pdf from a ps file
	err = shell(cmd=paste("ps2pdf ",fout,".ps",sep=""),wait=TRUE)
		if (err>0) stop("===== The ps2pdf call failed =====")
	invisible(ftex)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~collectFigs


#formatCatch----------------------------2013-08-20
# Format a numeric table so that each cell 
# displays N significnat figures in character format.
# Arguments:
#  dat  = dataframe/matrix of raw catch numbers
#  N    = number of significant digits
#  X    = numeric vector of columns to exclude
#  zero = character replacement for zero-values
#  na   = character replacement for NAs
#  K    = big mark separator (see 'format')
#-----------------------------------------------RH
formatCatch = function(dat,N=3,X=0,zero="0",na="---",K=",")
{
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
	
	# Detect, remember, and remove negative signs
	nadat  = is.na(dat[,-(X)])
	negdat = dat[,-(X)] < 0 & !nadat
	dat[,-(X)][negdat] = -dat[,-(X)][negdat]

	cdat = apply(dat,2,as.character)
	cdat = as.data.frame(cdat); dimnames(cdat) = dnam
	for (i in dnam[[2]][-(X)]) {
		idat = dat[,i]
		istr = strsplit(as.character(idat),split="\\.")
		inum = sapply(istr, function(x,N,K,zero,na){
			if (is.na(x[1])) return(na)
			x1 = format(round(as.numeric(x[1])),big.mark=K,trim=TRUE,scientific=FALSE)
			X1 = format(as.numeric(paste(x,collapse=".")),digits=N,big.mark=K,trim=TRUE,scientific=FALSE)
			n2 = N - nchar(x[1])
#browser(); return()
			if (length(x)==1) {
				if (x[1]=="0") ival=zero
				else if (n2>=1) ival = format(as.numeric(gsub(",","",x1)),nsmall=n2,big.mark=K,trim=TRUE,scientific=FALSE)
				else ival = x1
			}
			else {
				if (nchar(x[1])>=N) ival = X1
				else {
					if (x1=="0") {
						# exclude leading zeroes using 'clipVector' from 'PBSmodelling' package
						x2ex0 = paste(clipVector(strsplit(x[2],split="")[[1]],"0",1),collapse="")
						n20   = nchar(x[2])-nchar(x2ex0) # number of leading zeroes
						if (N-nchar(x2ex0) > 0 )
							x[2] = paste(x[2],paste(rep(0,N-nchar(x2ex0)+1),collapse=""),sep="")
						n2 = N + n20 # adjusted for leading zeroes
					}
					else {
						n2 = N - nchar(x[1])
					}
					x[2] = paste(x[2],paste(rep(0,N+1),collapse=""),sep="") # pad with extra zeroes as a precautionary measure
#browser();return()
#if (x[1]=="10" & x[2]=="000") {browser();return() }
					x2 = pad0(round(as.numeric(substring(x[2],1,n2+2))/100),n2)
					if (as.numeric(x2) %in% 10^(0:10)) ival = format(as.numeric(gsub(",","",X1)),nsmall=n2-as.numeric(is.element(as.numeric(x1),10^(1:10)-1)),big.mark=K,trim=TRUE,scientific=FALSE)
					#if (x2=="10") ival = format(as.numeric(gsub(",","",X1)),nsmall=n2,big.mark=K,trim=TRUE,scientific=FALSE)
					#show0(as.numeric(gsub(",","",X1)),n2,add2int=TRUE)
					else ival = paste(x1,x2,sep=".")
				}
			}
			return(ival)
		},N=N,K=K,zero=zero,na=na)
		cdat[[i]] = inum
	}
	# Resore the negative sign
	cdat[,-(X)][negdat] = paste("-",cdat[,-(X)][negdat],sep="")
	return(cdat)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~formatCatch


#makeLTH--------------------------------2013-07-24
# Make a longtable header for Sweave, source:
# http://tex.stackexchange.com/questions/41067/caption-for-longtable-in-sweave?rq=1
#-----------------------------------------------RH
makeLTH <- function(xtab.table, table.caption, table.label) {
	longtable.header <- paste(
		paste("\\caption{", table.caption, "}",sep = "", collapse = ""),
		paste("\\label{", table.label, "}\\\\ ",sep = "", collapse = ""),
		"\\hline\\\\[-2.2ex] ",
		attr(xtab.table, "names")[1],
		paste(" & ",attr(xtab.table, "names")[2:length(attr(xtab.table, "names"))],collapse="",sep=""),
		"\\\\[0.2ex]\\hline ",
		"\\endfirsthead ",
		paste("\\multicolumn{",ncol(xtab.table),"}{l}{{\\tablename\\ \\thetable{} -- continued from previous page}}\\\\ ",sep = ""),
		"\\hline ",
		attr(xtab.table, "names")[1],
		paste(" & ",attr(xtab.table, "names")[2:length(attr(xtab.table, "names"))],collapse="",sep=""),
		"\\\\[0.2ex]\\hline ",
		"\\endhead ",
		"\\hline\\\\[-2.2ex] ",
		paste("\\multicolumn{",ncol(xtab.table),"}{r}{{\\footnotesize \\emph{Continued on next page}}}\\\\ ",sep=""),
		#"\\hline \\endfoot ",
		"\\endfoot ","\\hline \\endlastfoot ",collapse = "")
	return(longtable.header)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~makeLTH


#texArray-------------------------------2013-08-09
# Flatten and format an array for latex output.
#-----------------------------------------------RH
texArray =function(x, table.caption="My table", table.label="tab:mytable",
	strSpp=NULL, sigdig=3, zero="---", collab=NULL, dash.delim=NULL, rm.empty=TRUE,
	start.page=1,
	select.rows=NULL, use.row.names=FALSE, name.row.names="row",
	add.header.column=FALSE, outnam="mytable")
{
	if (!is.array(x) && !is.matrix(x) && !is.data.frame(x)) stop("input an array or a matrix")
	N = dim(x)
	L = length(N)
	for (i in 1:L) {
		if (is.null(dimnames(x)[[i]])) 
			dimnames(x)[[i]] = paste("R",1:N[i],sep="")
		if (is.null(names(dimnames(x))[i]) || is.na(names(dimnames(x))[i]) )
			names(dimnames(x))[i] = paste("D",i,sep="")
	}
#browser();return()
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
		goo = x
	}
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
			data(pmfc,envir=lenv())
			dnams = dimnames(x)[as.numeric(names(Zsort))]
			dlist = sapply(names(dnams),function(d,dd){paste(d,dd[[d]],sep=":")},dd=dnams,simplify=FALSE)
#browser();return()
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
			c1 = gsub("area:major_","PMFC ",c1,fixed=TRUE)
			c1 = gsub("area:major","PMFC ",c1,fixed=TRUE)
			lablist = strsplit(c1," ")
			c1 = sapply(lablist,function(x){
				if (any(x=="PMFC")){
					z=grep("|",x,fixed=TRUE)
					if(length(z)>0)
						p=pmfc[strsplit(x[z],"|",fixed=T)[[1]],"gmu"]
					else{
						z=grep("PMFC",x)+1
						p=pmfc[strsplit(x[z],"")[[1]],"gmu"]
					}
#browser();return()
					labmat=matrix(unlist(strsplit(p,"")),ncol=2,byrow=TRUE)
					labor=split(labmat[,2],labmat[,1])
					x[z]=paste(paste(names(labor),sapply(labor,paste,collapse=""),sep=""),collapse="+")
					paste(x,collapse=" ")
				}
				else x }
			)
#browser();return()
			c1 = gsub("PMFC ","",c1,fixed=TRUE)
			c1 = gsub("3CD+5ABCDE","Coast",c1,fixed=TRUE)
			c1 = gsub("stat:Ntid","Number of trips",gsub("stat:Scat","Sample catch (t)",gsub("stat:Fcat","Fisheries catch (t)",c1)))
		}
#browser();return()
		for (k in 1:nrow(foo)) {
			koo = foo[k,]
			xoo = paste("x[,,",paste(koo,collapse=","),"]",sep="")
			poo = eval(parse(text=xoo))
#browser();return()
			if (!is.null(select.rows)){
				if (is.numeric(select.rows))
					poo = poo[select.rows,]
				else
					poo = poo[is.element(rownames(poo),select.rows),]
			}
#browser();return()
			rownames(poo)=paste(k,rownames(poo),sep="-")
			#bad = apply(poo,1,function(x){all(is.na(x))})
			#poo = poo[!bad,drop=FALSE,exact=TRUE]
			if (k==1) goo=poo
			else      goo = rbind(goo,poo)
		}
	}
#browser();return()
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
		"%\\newcommand\\tstrut{\\rule{0pt}{2.4ex}}       % top strut for table row",
		"%\\newcommand\\bstrut{\\rule[-1.0ex]{0pt}{0pt}} % bottom strut for table row",
		"\\usepackage{longtable,array} % need array when specifying a ragged right column",
		"\\setlength{\\LTleft}{0pt}     % left justify longtable",
		paste("\\setcounter{page}{", start.page, "}  % set the page numbering", sep=""),
		"\\usepackage{arydshln} % dashed lines in tables (load after other shit)",
		"\\begin{document}",
		"% Some Arial thing I presume: (from resDocSty.sty by AME)",
		"\\renewcommand{\\familydefault}{phv}",
		"\\renewcommand{\\rmdefault}{phv}",
		"\\usefont{\\encodingdefault}{\\familydefault}{\\seriesdefault}{\\shapedefault}\\small"
	)
	writeLines(texmess,texout)
	
	require(xtable)
	if (is.null(collab)) collab = colnames(goo)
	else if (length(collab)==ncol(goo)) colnames(goo) = collab
	else stop("Length of `collab` does not match number of columns")
	goo = formatCatch(goo,N=sigdig,zero=zero)
	if (use.row.names) {
		rows = dimnames(goo)[[1]]
#browser();return()
		if (length(N)>2) {
			rows = sapply(strsplit(rows,"-"),function(x){paste(x[-1],sep="",collapse="-")})
			rows = gsub("Females","F",gsub("Males","M",rows))
		}
		goo = data.frame(rows,goo,check.names=FALSE,stringsAsFactors=FALSE)
		dimnames(goo)[[2]][1] = name.row.names
	}
	if (add.header.column) {
		Ngroups = prod(Zsort)
		Nsexes  = nrow(goo)/Ngroups
		description = as.vector(matrix(c(c1,rep("",Ngroups*(Nsexes-1))),ncol=Ngroups,byrow=TRUE))
#browser();return()
		goo = data.frame(description,goo,check.names=FALSE,stringsAsFactors=FALSE)
		names(goo)[1] = ""
		if (use.row.names) names(goo)[2] = name.row.names
	}
	ncol = dim(goo)[[2]] - as.numeric(use.row.names) - as.numeric(add.header.column)
	if (rm.empty) {
		keep = apply(goo[(ncol(goo)-ncol+1):ncol(goo)],1,function(x){any(!x%in%zero & !x%in%"---")})
		goo = goo[keep,]
	}
	if (is.null(dash.delim) && L>2) {
		rowflag = sapply(strsplit(rownames(goo),"-"),function(x){as.numeric(x[1])})
		rows.line = grep(1,diff(rowflag))
	} else if (!is.null(dash.delim))
		rows.line = (1:nrow(goo))[is.element(substring(rownames(goo),ifelse(L>2,3,1)),dash.delim)]
	else
		rows.line=NULL
	rows.line = setdiff(rows.line,nrow(goo)) # don't draw a dashed line along the last row
#browser();return()
	xtab = xtable(goo,align=paste("c",ifelse(add.header.column,"l",""),ifelse(use.row.names,"c",""),paste(rep("r",ncol),collapse=""),sep=""))
	longtable.header=makeLTH(xtab,table.caption,table.label)
#colnames(goo)=rep("",dim(goo)[2])
	print(xtab, 
		file = texout, append=TRUE,
		floating = FALSE, # longtable never floats
		hline.after = NULL,
		#add.to.row = list(pos = list(-1, nrow(xtab)), command = c(longtable.header, "%")),
		add.to.row = {if (length(rows.line)==0) list(pos = list(-1, nrow(xtab)), command = c( longtable.header, "%"))
			else list(pos = list(-1, rows.line, nrow(xtab)), command = c( longtable.header, "\\hdashline[.4pt/1pt]", "%"))},
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
	return(list(goo=goo,texfile=texfile))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~texArray
