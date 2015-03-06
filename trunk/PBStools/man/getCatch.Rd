\name{getCatch}
\alias{getCatch}
\title{
  Get Catch for \code{weightBio} Function
}
\description{
  Extract catch records for a species from various SQL databases and 
  combine them into one commercial catch data frame and one survey catch
  data frame.
}
\usage{
getCatch(strSpp="396", dbs=c("gfb","gfc","pht","fos"),
   sql=FALSE, sqlpath=.getSpath(), proBio=FALSE, ioenv=.GlobalEnv)
}
\arguments{
  \item{strSpp}{character string specifying a species code.}
  \item{dbs}{character string vector of 3-letter codes specifying DFO databases.
    Currently, only the following are allowed: \cr
    \code{"gfb"} = \code{GFBioSQL} (survey catch data), \cr
    \code{"gfc"} = \code{GFCatch} (primarily trawl data), \cr
    \code{"pht"} = \code{PacHarvest} (trawl data only), \cr
    \code{"fos"} = \code{GFFOS} (all fisheries).}
  \item{sql}{logical: if \code{TRUE}, grab the pre-queried catch files stored as binaries.}
  \item{sqlpath}{string denoting the path to the series of SQL queries that gather catch.}
  \item{proBio}{logical: if \code{TRUE} run the function \code{processBio} on 
    the combined dataset to add fields: \cr
    \code{srfa} = slope rockfish assessment areas, \cr
    \code{srfs} = slope rockfish assessment subareas or gullies, \cr
    \code{popa} = Pacific ocean perch areas.}
  \item{ioenv}{input/output environment for function input data and output results.}
}
\details{
  This function provides a quick method of extracting contiguous datasets
  and combining them into one data object. 
  The primary use for this dataset is to create the catch input for the function \code{weightBio}.
  Currently, \code{getCatch} only queries four databases:
  \code{GFBioSQL}, \code{GFCatch}, \code{PacHarvest} and \code{GFFOS}. 
  The hook and line databases are messy and not contiguous.
  For a comprehensive catch reconstruction, see \code{buildCatch}.

  The fields returned by \code{getCatch} for commercial catch are:
  \tabular{lrl}{
    \code{EID}      \tab integer   \tab \code{\link[PBSmapping]{EventData}} field,\cr
    \code{X}        \tab numeric   \tab longitude (\eqn{^\circ}{deg}W),\cr
    \code{Y}        \tab numeric   \tab latitude (\eqn{^\circ}{deg}N),\cr
    \code{FID}      \tab integer   \tab fishery identifier:\cr
                    \tab           \tab 1=trawl, 2=halibut, 3=sablefish, 4=dogfish/lingcod, 5=H&L rockfish,\cr
    \code{TID}      \tab integer   \tab trip identifier,\cr
    \code{FEID}     \tab integer   \tab fishing event identifier,\cr
    \code{hail}     \tab integer   \tab hail number used by vessels,\cr
    \code{set}      \tab integer   \tab set number within a trip/hail,\cr
    \code{log}      \tab integer   \tab log type code or data source,\cr
    \code{date}     \tab POSIXt    \tab comercial fishing event date,\cr
    \code{year}     \tab integer   \tab calendar year,\cr
    \code{major}    \tab integer   \tab PMFC major area code,\cr
    \code{minor}    \tab integer   \tab PMFC minor area code,\cr
    \code{locality} \tab integer   \tab fishing locality code,\cr
    \code{pfma}     \tab integer   \tab Pacific Fishery Management Areas,\cr
    \code{pfms}     \tab integer   \tab Pacific Fishery Management Subareas,\cr
    \code{depth}    \tab numeric   \tab fishing depth (m),\cr
    \code{vessel}   \tab numeric   \tab vessel registration number,\cr
    \code{gear}     \tab integer   \tab gear code,\cr
    \code{success}  \tab integer   \tab trawl success code,\cr
    \code{effort}   \tab numeric   \tab fishing event effort (min),\cr
    \code{catKg}    \tab numeric   \tab fishing event catch (kg),\cr
    \code{pdis}     \tab numeric   \tab proportion of species total catch discarded in fishing event,\cr
    \code{pcat}     \tab numeric   \tab proportion of species total catch to fishing event total catch.
  }
  The fields returned by \code{getCatch} for survey catch are:
  \tabular{lrl}{
    \code{TID}      \tab integer   \tab trip identifier,\cr
    \code{FEID}     \tab integer   \tab fishing event identifier,\cr
    \code{set}      \tab integer   \tab set number within a trip,\cr
    \code{CID}      \tab integer   \tab catch identifier,\cr
    \code{SSID}     \tab integer   \tab survey series identifier,\cr
    \code{SVID}     \tab integer   \tab survey identifier,\cr
    \code{OI}       \tab integer   \tab original index (1) or not (0) for survey series,\cr
    \code{GC}       \tab integer   \tab grouping code,\cr
    \code{block}    \tab character \tab block designator,\cr
    \code{date}     \tab POSIXt    \tab survey fishing event date,\cr
    \code{year}     \tab integer   \tab calendar year,\cr
    \code{major}    \tab integer   \tab PMFC major area code,\cr
    \code{minor}    \tab integer   \tab PMFC minor area code,\cr
    \code{locality} \tab integer   \tab fishing locality code,\cr
    \code{spp}      \tab character \tab species Hart code,\cr
    \code{wt}       \tab numeric   \tab species catch (kg) in fishing event,\cr
    \code{pcs}      \tab integer   \tab species catch (pieces) in fishing events,\cr
    \code{catKg}    \tab numeric   \tab species catch (kg) -- if \code{wt} not reported, use \code{pcs} converted to kg.
  }
}
\value{
  Returns a list object of survey catch records \code{Scat} and commercial 
  catch records \code{Ccat} resulting from the merge of queries to 
  various DFO databases. 
  
  The results are also written automatically to system
  binary files \code{Scat123.wB.rda, Ccat123.wB.rda}, where \code{123} specifies
  the species code and \code{wB} specifies creation for use in the function
  \code{weightBio}.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link[PBStools]{weightBio}}, \code{\link[PBStools]{getData}},
  \code{\link[PBStools]{buildCatch}}, \code{\link[PBStools]{processBio}} \cr
  Available SQL queries: \code{\link[PBStools]{SQLcode}}
}
\keyword{manip}
\keyword{database}
\keyword{datagen}

