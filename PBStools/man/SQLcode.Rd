\name{SQLcode}
\alias{SQLcode}
\concept{SQL}
\concept{code}
\title{Info: SQL Code in PBStools}
\description{
  \pkg{PBStools} contains numerous SQL code files to query DFO databases.
  The SQL code is executed by the function \code{getData}, which relies on the R
  package \pkg{RODBC}. The code files are found in the package library
  \code{..\\library\\PBStools\\sql}.

  \bold{Note:} \cr
  The location of the SQL code files on a user's system can be determined by specifying
  \code{.getSpath()}, which returns a path string invisibly.
}
\details{
  The Fisheries and Oceans Canada (DFO) databases used by groundfish personnel
  comprise the following: 
  \tabular{lclc}{
    \bold{Database}     \tab \bold{code} \tab \bold{Fishery}                     \tab \bold{Dates} \cr
    \code{GFBioSQL}     \tab \code{gfb}  \tab Research, surveys, biology, JV hake catch, etc.   \tab 1946--2011 \cr
    \code{GFCatch}      \tab \code{gfc}  \tab Commercial fisheries data          \tab 1954--1975 \cr
    \code{GFFOS}        \tab \code{fos}  \tab Commercial fisheries (all sectors) \tab 2002--2016 \cr
    \code{PacHarv3}     \tab \code{ph3}  \tab Commercial sales slips data        \tab 1982--1994 \cr
    \code{PacHarvest}   \tab \code{pht}  \tab Commercial trawl fisheries         \tab 1996--2007 \cr
    \code{PacHarvHL}    \tab \code{phhl} \tab Commercial hook & line fisheries   \tab 1984--2006 \cr
    \code{PacHarvSable} \tab \code{phs}  \tab Commercial sablefish fishery       \tab 1987--2006 
  }
  Further details of the DFO databases can be found in Haigh and Yamanaka (2011).
}
\section{GFBioSQL}{
  The \code{GFBioSQL} database is an SQL copy of the Oracle database \code{GFBIO}. The SQL version
  is housed on the SQL server \code{DFBCV9TWVASP001} located at the Pacific Biological Station (PBS).
  The database contains biological sample and specimen information, as well as catch from 
  fishing activity not housed in any other database (e.g., JV Hake, research, surveys, foreign fleets).

  \tabular{ll}{
    \bold{SQL queries:} \tab \cr
    \code{gfb_ageing_summary.sql}       \tab Norm's ageing datasheet summary. \cr
    \code{gfb_age_request.sql}          \tab Summarize and choose otoliths taken but not aged.\cr
    \code{gfb_age_request_b21b22.sql}   \tab Same as previous but using summary tables as source.\cr
    \code{gfb_bio.sql}                  \tab Specimen biological data for a target species.\cr
    \code{gfb_bio_b21b22.sql}           \tab Same as previous but using summary tables as source.\cr
    \code{gfb_boot.sql}                 \tab Bootstrapped survey indices from boot tables.\cr
    \code{gfb_bycatch.sql}              \tab Annual fish group catches as bycatch to target species' depth range.\cr
    \code{gfb_catch_records.sql}        \tab Research survey catch for \code{\link[PBStools]{getCatch}} and \code{\link[PBStools]{weightBio}}.\cr
    \code{gfb_clara_survey.sql}         \tab Get survey catch from GFBioSQL for clara analysis.\cr
    \code{gfb_concat_rows_example.sql}  \tab Concatenate values when number of items is small and known upfront.\cr
    \code{gfb_fishwt.sql}               \tab Mean weight of species sampled from commercial and research trips.\cr
    \code{gfb_fos_age_request.sql}      \tab Otoliths taken but not aged; \cr
                                        \tab show only those that can be identified by \code{GFFOS}' \code{TRIP_ID}.\cr
    \code{gfb_fos_catch.sql}            \tab Get commercial catch from \code{GFFOS}; \cr
                                        \tab match trip IDs in \code{GFBioSQL} & \code{GFFOS}.\cr
    \code{gfb_fos_tid.sql}              \tab Match \code{GFBioSQL}'s \code{TRIP_ID}s with those in \code{GFFOS}.\cr
    \code{gfb_gfb_catch.sql}            \tab Research survey catch for \code{\link[PBStools]{requestAges}}.\cr
    \code{gfb_iphc.sql}                 \tab IPHC survey data for target species.\cr
    \code{gfb_iphc_v01.sql}             \tab Precursor to previous query. \cr
    \code{gfb_iphc_hook_data.sql}       \tab IPHC longline hook data (for AME's IPHC index calculation).\cr
    \code{gfb_iphc_set_data.sql}        \tab IPHC longline set data (for AME's IPHC index calculation).\cr
    \code{gfb_iphc_skate_data.sql}      \tab IPHC longline skate data (for AME's IPHC index calculation).\cr
    \code{gfb_jcatORF.sql}              \tab Joint-venture hake fishery catch up to \& including 2006 only,\cr
                                        \tab (GFFOS captures J-V hake catch thereafter).\cr
    \code{gfb_mean_weight.sql}          \tab Calculate mean weight of a fish species for converting discarded pieces to kg in GFFOS.\cr
    \code{gfb_pht_catch.sql}            \tab Get commercial catch from \code{PacHarvest}; \cr
                                        \tab match trip IDs in \code{GFBioSQL} & \code{GFFOS}.\cr
    \code{gfb_pmr.sql}                  \tab Population parameters (\eqn{p}, \eqn{\mu}, \eqn{\rho}) for target species in survey strata.\cr
    \code{gfb_rcatORF.sql}              \tab Research survey catch for catch reconstructon.\cr
    \code{gfb_shrimp_trawl.sql}         \tab Get shrimp trawl catch GFBioSQL for clara analysis.\cr
    \code{gfb_survey_activity.sql}      \tab Summarise the annual survey catch for specified species.\cr
    \code{gfb_survey_catch.sql}         \tab Survey catch and density by species.\cr
    \code{gfb_survey_stratum.sql}       \tab Survey stratum information.\cr
    \code{gfb_tasu.sql}                 \tab Specimen biological data for LL surveys in Tasu, Flamingo, Triangle, Brooks.\cr
    \code{gfb_YrWtAge.sql}              \tab Year, weight, age data for target species.\cr
    \code{meanSppWt.sql}                \tab Table of mean species weight calculated using `gfb_mean_weight.sql',\cr
                                        \tab which emulates PJS algorithm for GFBIO data.\cr
  }
}

\section{GFCatch}{
  \code{GFCatch} was originally a FORTRAN data compilation but is now housed on the SQL server
  \code{DFBCV9TWVASP001}. This database contains catch and effort data from three sources: logbooks
  (skippers, onboard observers), landing records (sales slips or validation records), and 
  anecdotal information (Rutherford 1999).

  \tabular{ll}{
    \bold{SQL queries:} \tab \cr
    \code{gfc_catch_fyear.sql}          \tab Catch of target species by fishing year.\cr
    \code{gfc_catch_records.sql}        \tab Species catch by tow (zero-catch records excluded).\cr
    \code{gfc_fcatORF.sql}              \tab \code{GFCatch} landings for rockfish reconstruction.\cr
    \code{gfc_glm.sql}                  \tab Catch/effort data for a target species; used in a GLM analysis.\cr
  }
}
\section{GFFOS}{
  This database is a subset of the Fishery Operations System (\code{FOS}), an Oracle database 
  on the server \code{ORADEV} in Vancouver. The subset \code{GFFOS} (Groundfish \code{FOS}) comprises Views
  to \code{FOS} that make the data more useful to stock assessment personnel, and is housed on 
  the Oracle server \code{GFSH} located at PBS, Nanaimo. The Oracle version has largely been sidelined for
  an SQL version of \code{GFFOS} on the SQL server \code{DFBCV9TWVASP001}
  (now on the ``cloud'', but more likely in someone's leaky basement).

  \tabular{ll}{
    \bold{SQL queries:} \tab \cr
    \code{fos_bycatch.sql}              \tab Annual fish group catches as bycatch to target species' depth range.\cr
    \code{fos_catch_records.sql}        \tab Species catch by tow (zero-catch records excluded).\cr
    \code{fos_catch_records.ora}        \tab Oracle version of previous query.\cr
    \code{fos_checklist_catch.sql}      \tab Used for table of catches in DFO's 'checklist' nonsense.\cr
    \code{fos_fcatORF.sql}              \tab Species landings format for catch history reconstructions.\cr
    \code{fos_feid_catch.sql}           \tab Fishing event catch.\cr
    \code{fos_fishwt.sql}               \tab Mean weight for every species with catch and count info.\cr
    \code{fos_glm.sql}                  \tab Catch/effort data for a target species; used in a GLM analysis.\cr
    \code{fos_map_density.sql}          \tab Catch/effort data for a target species; used to map spatial density.\cr
    \code{fos_mcatORF.sql}              \tab Catch gathered from multiple DBs and housed in \code{GF_MERGED_CATCH}.\cr
    \code{fos_sectors.sql}              \tab Trips rationalised by sector and fishery ID.\cr
    \code{fos_sectors.ora}              \tab Oracle version of previous query.\cr
    \code{fos_tid_catch.sql}            \tab Summarise \code{GFFOS} catch (kg) by trip ID, date, \cr
                                        \tab PMFC major and PMFC minor for a specified species.\cr
    \code{fos_vcatORF.sql}              \tab Query catch from the merged catch table \code{GF_D_OFFICIAL_FE_CATCH}.\cr
    \code{fos_vcatORF.ora}              \tab Oracle version of previous query.\cr
    \code{fos_vess_year_trip.sql}       \tab Tally number of years and trips/year for each vessel.\cr
  }
}
\section{PacHarv3}{
  This Oracle database is commonly called \code{PacHarv3}; however, it's real name is 
  \code{HARVEST_V2_0} and it sits on the Oracle server \code{ORAPROD}. It contains sales slip
  data from landings records by trip. The data tables are numerous and the linkages obscure.
  Fortunately, there exists a summary table called \code{CATCH_SUMMARY}.

  \tabular{ll}{
    \bold{SQL queries:} \tab \cr
    \code{ph3_fcatORF.sql}              \tab Species landings format for catch history reconstructions.\cr
    \code{ph3_fcatORF_v01.sql}          \tab First version of `ph3_fcatORF.sql'.\cr
    \code{ph3_fcatORF_v02.sql}          \tab Second version of `ph3_fcatORF.sql'.\cr
    \code{ph3_fcatORF_v03.sql}          \tab Third version of `ph3_fcatORF.sql'.\cr
    \code{ph3_orfhistory.sql}           \tab Catch table for other rockfish used in catch reconstructions.\cr
    \code{ph3_target.sql}               \tab Determine target species by year, month, area, and gear.\cr
  }
}
\section{PacHarvest}{
  The \code{PacHarvest} database (sometimes referred to as \code{PacHarvTrawl}) sits on the 
  SQL server \code{DFBCV9TWVASP001} and houses observer and fisher log catch and effort 
  information for most trawl fishing events from Feb 1996 to Mar 2007.
  Details of the database can be found in Schnute et al. (1999).

  \tabular{ll}{
    \bold{SQL queries:} \tab \cr
    \code{pht_bycatch.sql}              \tab Annual fish group catches as bycatch to target species' depth range.\cr
    \code{pht_catch_fyear.sql}          \tab Catch of target species by fishing year using observer & fisher logs.\cr
    \code{pht_catch_gullies_cyear.sql}  \tab Catch of target species by calendar year in QCS gullies.\cr
    \code{pht_catch_gullies_fyear.sql}  \tab Catch of target species by fishing year in QCS gullies.\cr
    \code{pht_catch_records.sql}        \tab Species catch by tow (zero-catch records excluded).\cr
    \code{pht_clara.sql}                \tab Catch data for use in \code{clara} clustering algorithm.\cr
    \code{pht_clara_groundfish.sql}     \tab Groundfish catch data for use in \code{clara} clustering algorithm.\cr
    \code{pht_clara_groups.sql}         \tab Catch data by group for use in \code{clara} clustering algorithm.\cr
    \code{pht_clara_taxa.sql}           \tab Catch data by taxon for use in \code{clara} clustering algorithm.\cr
    \code{pht_concurrent.sql}           \tab Top 20 species caught concurrently with a target species.\cr
    \code{pht_effort.sql}               \tab Trawl effort used for background histogram in depth preference.\cr
    \code{pht_fdep.sql}                 \tab Catch of target species used to determine depth of capture.\cr
    \code{pht_fdep_pjs.sql}             \tab As for \code{pht_fdep.sql}, but vessels present \eqn{\ge} 3 years and \eqn{\ge} 5 trips/y.\cr
    \code{pht_glm.sql}                  \tab Catch/effort data for a target species; used in a GLM analysis.\cr
    \code{pht_map_catch.sql}            \tab Catch data for a target species; used to map spatial density..\cr
    \code{pht_map_density.sql}          \tab Catch/effort data for a target species; used to map spatial density.\cr
    \code{pht_obsORF.sql}               \tab Observer landed catch of a target species, POP, and ORF (other rockfish).\cr
    \code{pht_tcatORF.sql}              \tab Trawl landings of target species, POP, & ORF (other rockfish).\cr
    \code{pht_vess_year_trip.sql}       \tab Tally number of years and trips/year for each vessel.\cr
  }
}
\section{PacHarvHL}{
  The \code{PacHarvHL} database comprises hook and line records from fisher log data 
  (1986--2006 for ZN license, 2001-2006 for Schedule II), observer log data (1999--2004 for ZN
  and Halibut, 2001--2004 for Schedule II), and the Dockside Monitoring Program 
  (1995--2005 for ZN, 1996--2005 for Schedule II, and 1991-2005 for Halibut). 
  This database sits on the SQL server \code{DFBCV9TWVASP001}. 
  See Haigh and Richards (1997) for some of the early history.

  \tabular{ll}{
    \bold{SQL queries:} \tab \cr
    \code{phhl_concurrent.sql}          \tab Top 20 species caught concurrently with a target species.\cr
    \code{phhl_districts.sql}           \tab Calculate proportion of PMFC major areas in each historical district.\cr
    \code{phhl_fcatch_fyear.sql}        \tab Fisherlog catch of target species by fishing year & PMFC area.\cr
    \code{phhl_fcatORF.sql}             \tab Fisherlog catch of target species, POP, & ORF (other rockfish).\cr
    \code{phhl_hcatch_fyear.sql}        \tab Halibut bycatch of target species by fishing year & PMFC area.\cr
    \code{phhl_hcatORF.sql}             \tab Halibut bycatch of target species, POP, & ORF (other rockfish).\cr
    \code{phhl_map_density.sql}         \tab Catch/effort data for a target species; used to map spatial density.\cr
    \code{phhl_ocatORF.sql}             \tab Offical (landed) catch of target species, POP, & ORF (other rockfish).\cr
    \code{phhl_vcatch_fyear.sql}        \tab Verified landings of target species by fishing year & PMFC area.\cr
    \code{phhl_vcatORF.sql}             \tab Verified landings of target species, POP, & ORF (other rockfish).\cr
  }
}
\section{PacHarvSable}{
  The database \code{PacHarvSable} (on the SQL server \code{DFBCV9TWVASP001}) includes commercial trap and 
  longline Sablefish catch and effort records from fisher logbooks (1990--2006), observer logbooks
  (2000--2005), and the Dockside Monitoring Program (1995--2002).

  \tabular{ll}{
    \bold{SQL queries:} \tab \cr
    \code{phs_scatORF.sql}              \tab Official landings of target species, POP, & ORF (other rockfish).
  }
}
\author{
  Rowan Haigh & Norm Olsen \cr
  Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\references{
  Haigh, R., and Richards, L.J. (1997) 
  A relational database for hook and line rockfish logbook data. 
  \emph{Canadian Manuscript Report of Fisheries and Aquatic Sciences} \bold{2408}: vi + 46 p.
  
  Haigh, R., and Yamanaka, K.L. (2011) 
  Catch history reconstruction for rockfish (\emph{Sebastes} spp.) caught in British Columbia coastal waters. 
  \emph{Canadian Technical Report of Fisheries and Aquatic Sciences} \bold{2943}: viii + 124 p.
  
  Rutherford, K.L. (1999) 
  A brief history of GFCatch (1954-1995), the groundfish catch and effort database at the Pacific Biological Station. 
  \emph{Canadian Technical Report of Fisheries and Aquatic Sciences} \bold{2299}. v + 66 p.
  
  Schnute, J.T., Olsen, N., and Haigh, R. (1999) 
  Slope rockfish assessment for the west coast of Canada in 1998. 
  \emph{Canadian Stock Assessment Secretariat, Research Document} \bold{99/184}, 104 p.
}
\seealso{
  \code{\link[PBStools]{PBStools}} for package functions and data.\cr
  \code{\link[PBStools]{getData}} to execute the SQL queries.\cr
  \code{\link[PBStools]{ServerParlance}} for differences between SQL Server and Oracle database organisation.
}
\keyword{interface}
\keyword{connection}
\keyword{database}

