-- PacHarvSable query for landed catch (KG) of a target species, POP, and ORF (other rockfish)
-- Last modified: 2014-10-15

DECLARE @logtype VARCHAR(10)  --, @fisheryid VARCHAR(1)
SET @logtype   = @logtypeval
-- SET @fisheryid = @fisheryidval -- default = any string of zero or more characters

SET NOCOUNT ON -- prevents timeout errors

-- compile the landed catch stats for target, POP, & ORF
SELECT 
  -- OC.OBFL_HAIL_IN_NO, OC.OBFL_SET_NO,
  'fid'   = IsNull(T.FISHERY_ID,3),
  'gear'  = CASE OC.GEAR WHEN 'TRAP' THEN 2 WHEN 'LONGLINE' THEN 5 ELSE 0 END,
  --'date' = CAST(OC.OFFLOAD_DT AS smalldatetime),
  'date'  = CONVERT(char(10),OC.OFFLOAD_DT,20),
  'major' = IsNull(OC.MAJOR_STAT_AREA_CDE,0),
  'minor' = IsNull(OC.MINOR_STAT_AREA_CDE,0),
  'locality' = IsNull(OC.LOCALITY_CDE,0),
  'cfv' = CAST(COALESCE(OC.VRN,T.VSL_CFV_NO,'0') AS VARCHAR(6)),
  'fdep' = ISNULL(E.END_BOTTOM_DEPTH,NULL),
  -- 'eff' = Sum(IsNull(OC.Duration,0)),
  'landed' = Sum( CASE OC.SPECIES_CODE
    WHEN @sppcode THEN IsNull(OC.LANDED,0)
    ELSE 0 END ),
  'discard' = Sum( CASE OC.SPECIES_CODE
    WHEN @sppcode THEN IsNull(OC.DISCARDED,0)
    ELSE 0 END ),
  'POP' = Sum( CASE OC.SPECIES_CODE
    WHEN '396' THEN IsNull(OC.LANDED,0)
    ELSE 0 END ),
  'ORF' = Sum(  -- all rockfish other than POP
    --CASE WHEN OC.SPECIES_CODE <> '396' AND S.Rockfish = 1 THEN IsNull(OC.LANDED,0)
    CASE WHEN OC.SPECIES_CODE IN (@orfcode) THEN IsNull(OC.LANDED,0)
    ELSE 0 END ),
  'TAR' = Sum(   -- target landings reference for discard calculations
    CASE WHEN OC.SPECIES_CODE IN ('454','455') THEN IsNull(OC.LANDED,0)
   ELSE 0 END)
INTO #Catch
FROM 
  -- PacHarvest.dbo.C_Species S RIGHT OUTER JOIN
  B2_Trips T RIGHT OUTER JOIN
  (B3_Fishing_Events E RIGHT OUTER JOIN 
  D_Official_Catch OC ON
    OC.FISHING_EVENT_ID = E.FISHING_EVENT_ID ) ON
    E.TRIP_ID = T.TRIP_ID  -- ) ON
    -- OC.SPECIES_CODE = S.SPECIES_CDE
WHERE
  IsNull(T.FISHERY_ID,3) IN (@fisheryid) AND
  -- CAST(IsNull(T.FISHERY_ID,3) AS VARCHAR(1)) LIKE @fisheryid AND
  OC.LOG_TYPE_CDE LIKE @logtype
GROUP BY 
  OC.HAIL_IN_NO, OC.SET_NO,
  IsNull(T.FISHERY_ID,3),
  CASE OC.GEAR WHEN 'TRAP' THEN 2 WHEN 'LONGLINE' THEN 5 ELSE 0 END,
  CAST(OC.OFFLOAD_DT AS smalldatetime),
  IsNull(OC.MAJOR_STAT_AREA_CDE,0),
  IsNull(OC.MINOR_STAT_AREA_CDE,0),
  IsNull(OC.LOCALITY_CDE,0),
  CAST(COALESCE(OC.VRN,T.VSL_CFV_NO,'0') AS VARCHAR(6)),
  ISNULL(E.END_BOTTOM_DEPTH,NULL)

SELECT * FROM #Catch C
  WHERE
    (C.landed>0 OR C.discard>0 OR C.POP>0 OR C.ORF>0 OR C.TAR>0)

-- getData("phs_scatORF.sql","PacHarvSable",strSpp="424",fisheryid=3) -- logtype default is 'FISHERLOG'
-- qu("phs_scatORF.sql",dbName="PacHarvSable",strSpp="418",fisheryid=3,logtype="FISHERLOG")
-- qu("phs_scatORF.sql",dbName="PacHarvSable",strSpp="442",fisheryid=3,logtype="OBSERVRLOG")

