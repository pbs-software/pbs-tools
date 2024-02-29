\name{trend}
\alias{trend}
\alias{.trend.booty}
\alias{.trend.funky}
\alias{.trend.getSQLspp}
\alias{.trend.trendy}

\title{GUI: Display Boxplots of CPUE and the Trend Line}
\description{
  Display boxplots of catch or CPUE grouped by year. A trend line is fit through 
  the annual index points summarized by a user-specified summary function (e.g., 
  \code{mean}). The interactive GUI also allows the generation and display of 
  bootstrapped estimates of slope \emph{b} and annual rate \emph{r}.
}
\usage{
trend(strSpp="442", fqtName="gfb_iphc.sql", dbName="GFBioSQL",
   spath=NULL, type="SQL", ioenv=.GlobalEnv, hnam=NULL)

.trend.booty()
.trend.funky()
.trend.getSQLspp(spp=NULL)
.trend.trendy()
}
\arguments{
  \item{strSpp, spp}{\code{character} -- string specifying the code for a species that appears in the
    target database }
  \item{fqtName}{\code{character} -- string specifying the name of a local file (f), an SQL query 
    file (q), or an MDB table (t)}
  \item{dbName}{\code{character} -- string specifying the name of an SQL DSN for an ODBC call to 
    the SQL Server database \code{DFBCV9TWVASP001}, or an MDB database}
  \item{spath}{\code{character} -- string specifying the path to the file \code{fqtName}}
  \item{type}{\code{character} -- string specifying the file type: \code{"SQL"}, \code{"MDB"}, or \code{"FILE"}}
  \item{ioenv}{\code{environment} -- input/output environment for function input data and output results.}
  \item{hnam}{\code{character} -- string specifying the name of a history file}
}
\details{
  The function \code{trend()} creates an interactive GUI that can be used
  to display boxplots, trend lines, and bootstrapped estimates of change. 

  \tabular{ll}{
  \bold{GUI Left - Main Control:}  \tab \cr
  \code{Window}      \tab Button to display the window description file.\cr
  \code{SQL}         \tab Button to display the SQL code.\cr
  \code{R code}      \tab Button to display the R code used to generate \code{trend}.\cr
  \code{Spp}         \tab Species code in target data object or SQL Server database.\cr
  \code{SQL}         \tab Button to run Sql code to get the species data instead of specifying a data object.\cr
  \code{Obj}         \tab Name of data file/object.\cr
  \code{IPHC}        \tab Example IPHC SSA longline data objects in package: \cr
                     \tab    \code{iphc.rbr}, \code{iphc.rer}, \code{iphc.yyr}.\cr
  \code{path}        \tab Path to the SQL code or data object.\cr
  \code{sql}         \tab Button to set path to the library's \code{sql} directory.\cr
  \code{cwd}         \tab Button to set path to the current working directory.\cr
  \code{File/Query/Table} \tab Name of (i) data file, (ii) SQL code file or MDB query, or (iii) MDB table.\cr
  \code{DB/DSN}      \tab Database name on the SQL server \code{DFBCV9TWVASP001}.\cr
  \code{type}        \tab Type of file: \code{"SQL"}, \code{"MDB"}, or \code{"FILE"}.\cr
  \code{GET}         \tab Button to run the code specified on this line of the GUI.\cr
  \code{Trusted}     \tab Check if using a trusted DFO identity.\cr
  \code{SQL User ID} \tab SQL user ID assigned by the database server administrators.\cr
  \code{password}    \tab Password assigned by the database server administrators.\cr
  \code{History}     \tab History widget for storing GUI settings.\cr
  \code{TREND}       \tab Button to calculate and display the trend based on GUI choices.\cr
  \code{boots}       \tab Number of bootstrap replicates.\cr
  \code{BOOT}        \tab Button to calculate and display bootstrapped parameters.\cr
  \code{wmf}         \tab Check box to send TREND or BOOT to a \code{.wmf} file. \cr \cr
  \bold{GUI Right - Settings:} \tab \cr
  \code{Years}       \tab Specify years either singly (e.g. \code{2000}), as a range (e.g. \code{2000:2003}),\cr
                     \tab    as a group (e.g. \code{1996,2000,2006}), or as a sequence (e.g. \code{seq(1997,2007,2)})\cr
  \code{Func}        \tab Summary function to represent annual index points in trend line estimation.\cr
  \code{Exclude stations} \tab Exclude stations that never catch the target species.\cr
  \code{Include zeroes} \tab Include zero-values in the calculation of the index \cr
                     \tab   (need to add a small value if using log transformations).\cr
  \code{Add value}   \tab Add a constant value to the data points.\cr
  \code{Transforms}  \tab Transformation of data (\code{log}, \code{log2}, \code{log10}, or no transformation.\cr
  \code{Show barplot} \tab Show the proportions-zero barplot.\cr
  \code{Indices}     \tab Choose an index to plot (CPUE, Catch, or Effective skates).\cr
  \code{Colours}     \tab Choose colours for: \cr
                     \tab    boxplots, line connecting index points, least squares trend line, \cr
                     \tab    index points, bars showing proportion zeroes, and number of \cr
                     \tab    data points used in the index calculation.\cr
  }
}
\references{
  Obradovich, S.G., Yamanaka, K.L., Cooke, K., Lacko, L.C. and Dykstra, C. (2008)
  Summary of non-halibut catch from the Standardized Stock Assessment Survey 
  conducted by the International Pacific Halibut Commission in British 
  Columbia from June 4 to July 7, 2007. \emph{Canadian Technical Report 
  of Fisheries and Aquatic Sciences} \bold{2807}, x + 83 p.

  Schnute, J., Haigh, R., Krishka, B., Sinclair, A. and Starr, P. (2004)
  The British Columbia longspine thornyhead fishery: analysis of survey and 
  commercial data (1996--2003). \emph{Canadian Science Advisory Secretariat, 
  Research Document} \bold{2004/059}, 75 pp.
}
\author{ 
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-26}\cr
}
\seealso{
  In package \pkg{PBStools}:\cr
    \code{\link[PBStools]{getData}},
    \code{\link[PBStools]{quantBox}},
    \code{\link[PBStools]{simBGtrend}},
    \code{\link[PBStools]{SQLcode}}\cr
  In package \pkg{PBSdata}:\cr
    \code{\link[PBSdata]{iphc.rbr}}\cr
  In package \pkg{stats}:\cr
    \code{\link[stats]{lm}}
}
\keyword{hplot}
\concept{M04_Survey}

