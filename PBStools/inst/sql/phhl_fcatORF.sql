-- Last modified by RH (2015-04-23)
-- PacHarvHL query for fisherlog catch (KG) of a target species, POP, and ORF (other rockfish)
-- fisheryid: 2=Halibut, 3=Sablefish, 4=Schdeule II, 5=ZN, 
--            6=Sablefish+ZN, 7=Sablefish+Halibut, 8=Dogfish, 9=Lingcod

DECLARE @logtype VARCHAR(10)
SET @logtype   = @logtypeval

-- declare a temp table for discard codes... (see fos_vcatORF.sql for an example)

SET NOCOUNT ON -- prevents timeout errors

-- Event info on FID and licence option
SELECT 
  'hail_in' = FE.OBFL_HAIL_IN_NO,
  'set_no'  = FE.OBFL_SET_NO,
  'log'     = FE.OBFL_LOG_TYPE_CDE,
  'fid'     = FE.OBFL_FISHERY_ID,
  'lic'     = IsNull(RTRIM(LTRIM(VH.vrec_ext_lic_option_cde)),''),
  'date'    = CAST( COALESCE(FE.OBFL_START_DT,FE.Start_FE,FE.OBFL_END_DT,FE.OBFL_LOADED_DT) AS smalldatetime),
  'major'   = IsNull(FE.OBFL_MAJOR_STAT_AREA_CDE,0),
  'minor'   = IsNull(FE.OBFL_MINOR_STAT_AREA_CDE,0),
  'locality'   = IsNull(FE.OBFL_LOCALITY_CDE,0),
  'region'  = CASE FE.OBFL_MAJOR_STAT_AREA_CDE 
    WHEN 9 THEN '5E'
    WHEN 8 THEN '5D'
    WHEN 7 THEN '5C'
    WHEN 6 THEN '5B'
    WHEN 5 THEN '5A'
    WHEN 4 THEN '3D'
    WHEN 3 THEN '3C'
    WHEN 1 THEN '4B'
    ELSE '' END,
  'fdep' = CASE
    WHEN FE.Fishing_Depth IS NOT NULL AND FE.Fishing_Depth>=0 THEN FE.Fishing_Depth
    WHEN FE.OBFL_START_BOTTOM_DTH IS NOT NULL AND FE.OBFL_START_BOTTOM_DTH>=0 AND
         FE.OBFL_END_BOTTOM_DTH IS NOT NULL AND FE.OBFL_END_BOTTOM_DTH>=0 THEN
         (FE.OBFL_START_BOTTOM_DTH + FE.OBFL_END_BOTTOM_DTH) / 2.
    WHEN FE.OBFL_START_BOTTOM_DTH IS NOT NULL AND FE.OBFL_START_BOTTOM_DTH>=0 THEN FE.OBFL_START_BOTTOM_DTH
    WHEN FE.OBFL_END_BOTTOM_DTH IS NOT NULL AND FE.OBFL_END_BOTTOM_DTH>=0 THEN FE.OBFL_END_BOTTOM_DTH
    ELSE 0. END,
  'eff'  = FE.Duration
  INTO #Events
  FROM
    B5_Validation_Header VH RIGHT OUTER JOIN
    B3_Fishing_Events FE ON
    FE.OBFL_HAIL_IN_NO = VH.vrec_hail_in_no
  WHERE
    FE.OBFL_LOG_TYPE_CDE IN (@logtype)

-- Compile the catch stats for target, POP, ORF, TAR
SELECT 
  E.hail_in, E.set_no, E.log, E.fid, E.lic, 
  E.date, E.major, E.minor, E.locality, E.region, E.fdep, E.eff,
  'landed' = Sum( CASE 
    WHEN FC.OBFL_SPECIES_CDE IN (@sppcode) AND FC.OBFL_CATCH_UTILIZATION_CDE NOT IN (5,6,8,9,22,23,24,27,28)
    THEN IsNull(FC.OBFL_EST_WEIGHT,0)
    ELSE 0 END ),
  'discard' = Sum( CASE 
    WHEN FC.OBFL_SPECIES_CDE IN (@sppcode) AND FC.OBFL_CATCH_UTILIZATION_CDE IN (5,6,8,9,22,23,24,27,28) -- discard codes
    THEN IsNull(FC.OBFL_EST_WEIGHT,0)
    ELSE 0 END ),
  'POP' = Sum( CASE 
    WHEN FC.OBFL_SPECIES_CDE IN ('396') AND FC.OBFL_CATCH_UTILIZATION_CDE NOT IN (5,6,8,9,22,23,24,27,28)
    THEN IsNull(FC.OBFL_EST_WEIGHT,0)
    ELSE 0 END ),
  'ORF' = Sum(   -- all rockfish other than POP
    CASE WHEN FC.OBFL_SPECIES_CDE IN (@orfcode) AND FC.OBFL_CATCH_UTILIZATION_CDE NOT IN (5,6,8,9,22,23,24,27,28)
    THEN IsNull(FC.OBFL_EST_WEIGHT,0)
    -- CASE WHEN FC.OBFL_SPECIES_CDE <> '396' AND S.Rockfish=1 THEN IsNull(FC.OBFL_EST_WEIGHT,0)
    ELSE 0 END ),
  'TAR' = Sum( CASE  -- target landings reference for discard calculations
    WHEN E.fid=2 AND FC.OBFL_SPECIES_CDE IN ('614') AND FC.OBFL_CATCH_UTILIZATION_CDE NOT IN (5,6,8,9,22,23,24,27,28)
      THEN IsNull(FC.OBFL_EST_WEIGHT,0)
    WHEN E.fid=4 AND E.lic='D' AND FC.OBFL_SPECIES_CDE IN ('042','044') AND FC.OBFL_CATCH_UTILIZATION_CDE NOT IN (5,6,8,9,22,23,24,27,28)
      THEN IsNull(FC.OBFL_EST_WEIGHT,0)
    WHEN E.fid=4 AND E.lic='L' AND FC.OBFL_SPECIES_CDE IN ('467') AND FC.OBFL_CATCH_UTILIZATION_CDE NOT IN (5,6,8,9,22,23,24,27,28)
      THEN IsNull(FC.OBFL_EST_WEIGHT,0)
    WHEN E.fid=4 AND E.lic NOT IN ('D','L') AND FC.OBFL_SPECIES_CDE IN ('042','044','467') AND FC.OBFL_CATCH_UTILIZATION_CDE NOT IN (5,6,8,9,22,23,24,27,28)
      THEN IsNull(FC.OBFL_EST_WEIGHT,0)
    WHEN E.fid=5 AND FC.OBFL_SPECIES_CDE IN ('424','407','431','433','442') AND FC.OBFL_CATCH_UTILIZATION_CDE NOT IN (5,6,8,9,22,23,24,27,28)
      THEN IsNull(FC.OBFL_EST_WEIGHT,0)
    ELSE 0 END)
  INTO #Catch
  FROM 
    #Events E RIGHT OUTER JOIN
    -- (PacHarvest.dbo.C_Species S RIGHT OUTER JOIN
    B4_Catches FC ON  -- B4 Catch is in kg (Lisa Lacko)
    -- FC.OBFL_SPECIES_CDE = S.SPECIES_CDE) ON
    E.hail_in = FC.OBFL_HAIL_IN_NO AND
    E.set_no = FC.OBFL_SET_NO AND
    E.log = FC.OBFL_LOG_TYPE_CDE
  WHERE
    FC.OBFL_LOG_TYPE_CDE IN (@logtype) AND
    E.fid IN (@fisheryid)
  GROUP BY 
    E.hail_in, E.set_no, E.log, E.fid, E.lic,
    E.date, E.major, E.minor, E.locality, E.region, E.fdep, E.eff

-- Create an events table based on unique hails
SELECT 
  C.fid, C.lic,
  --'date' = CAST( COALESCE(C.date, T.OBFL_OFFLOAD_DT) AS smalldatetime),
  'date' = CONVERT(char(10),COALESCE(C.date, T.OBFL_OFFLOAD_DT),20),
  C.major, C.minor, C.locality, C.region, 
  'fdep' =  CAST(ROUND(C.fdep,7) AS NUMERIC(15,7)),
  'cfv' = CAST(IsNull(T.OBFL_VSL_CFV_NO,'0') AS VARCHAR(6)),
  C.eff,
  'landed'  = CAST(ROUND(C.landed,7) AS NUMERIC(15,7)),   --/2.20459,
  'discard' = CAST(ROUND(C.discard,7) AS NUMERIC(15,7)), --/2.20459,
  'POP' = CAST(ROUND(C.POP,7) AS NUMERIC(15,7)), --/2.20459,
  'ORF' = CAST(ROUND(C.ORF,7) AS NUMERIC(15,7)), --/2.20459,
  'TAR' = CAST(ROUND(C.TAR,7) AS NUMERIC(15,7))  --/2.20459
FROM 
    B2_Trips T RIGHT OUTER JOIN
    #Catch C ON 
   T.OBFL_HAIL_IN_NO = C.hail_in
WHERE 
  --COALESCE(C.fid,E.OBFL_FISHERY_ID,T.OBFL_FISHERY_ID)=@fisheryid AND
  (C.landed>0 OR C.discard>0 OR C.POP>0 OR C.ORF>0 OR C.TAR>0)

-- SELECT * FROM #Catch
-- getData("phhl_fcatORF.sql","PacHarvHL",strSpp="424",fisheryid=4)
-- getData("phhl_fcatORF.sql","PacHarvHL",strSpp="424",fisheryid=5)
-- qu("phhl_fcatORF.sql",dbName="PacHarvHL",strSpp="418",logtype="FISHERLOG") --- fisherlog catch
-- qu("phhl_fcatORF.sql",dbName="PacHarvHL",strSpp="442",logtype="OBSERVRLOG",fisheryid=4) --- observer log catch


