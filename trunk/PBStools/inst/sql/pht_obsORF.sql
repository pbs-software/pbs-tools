-- Last modified by RH (2015-04-23)
-- PacHarvest query for landed catch of a target species, POP, and ORF (other rockfish)
-- This shortened form initially took longer to execute than `pht_obsORF_long.sql' until Norm suggested changes in the WHERE clause

SET NOCOUNT ON  -- prevents timeout errors

SELECT 
  IsNull(FE.OBFL_FISHERY_ID,1) AS fid,
  CONVERT(char(10),COALESCE(FE.Start_FE, FE.OBFL_START_DT, FE.OBFL_END_DT, OC.OFFLOAD_DATE), 20) AS [date],
  COALESCE(FE.OBFL_MAJOR_STAT_AREA_CDE, OC.MAJOR_STAT_AREA, 0) AS major,
  COALESCE(FE.OBFL_MINOR_STAT_AREA_CDE, OC.MINOR_STAT_AREA, 0) AS minor,
  COALESCE(FE.OBFL_LOCALITY_CDE, OC.LOCALITY_CDE, 0) AS locality,
  (CASE
    WHEN FE.Fishing_Depth IS NULL OR FE.Fishing_Depth<=0 THEN NULL
    ELSE CAST(ROUND(FE.Fishing_Depth,7) AS NUMERIC(15,7)) END) AS fdep,
  landed = Sum(CASE
    WHEN MC.SPECIES_CODE IN (@sppcode) THEN MC.LANDED
    ELSE 0 END ),
  discard = Sum(CASE
    WHEN MC.SPECIES_CODE IN (@sppcode) THEN MC.DISCARDED
    ELSE 0 END ),
  POP = Sum(CASE   -- landings of Pacific Ocean Perch (POP)
    WHEN MC.SPECIES_CODE IN ('396') THEN MC.LANDED
    ELSE 0 END ),
  ORF = Sum(CASE   -- landings of all rockfish other than POP
    WHEN MC.SPECIES_CODE IN (@orfcode) THEN MC.LANDED
    ELSE 0 END ),
  TAR = Sum(CASE   -- target landings reference for discard calculations
    WHEN MC.SPECIES_CODE IN (@trfcode) THEN MC.LANDED
   ELSE 0 END)
--INTO #OBSLOGS
FROM 
  (B3_Fishing_Events FE INNER JOIN 
  D_Merged_Catches MC ON
    FE.OBFL_HAIL_IN_NO = MC.HAIL_IN_NO AND
    FE.OBFL_SET_NO = MC.SET_NO) INNER JOIN
  D_Official_Catch OC ON
    OC.HAIL_IN_NO = MC.HAIL_IN_NO AND 
    OC.SET_NO = MC.SET_NO AND 
    OC.SPECIES_CODE = MC.SPECIES_CODE
WHERE 
  FE.OBFL_LOG_TYPE_CDE IN ('OBSERVRLOG') AND
  --YEAR(COALESCE(FE.Start_FE, FE.OBFL_START_DT, FE.OBFL_END_DT, OC.OFFLOAD_DATE)) IN (@dummy)
  (YEAR(FE.Start_FE) IN (@dummy) OR
   YEAR(FE.OBFL_START_DT) IN (@dummy) OR
   YEAR(FE.OBFL_END_DT) IN (@dummy) OR
   YEAR(OC.OFFLOAD_DATE) IN (@dummy))
GROUP BY 
  FE.OBFL_HAIL_IN_NO, FE.OBFL_SET_NO,
  IsNull(FE.OBFL_FISHERY_ID,1),
  CONVERT(char(10),COALESCE(FE.Start_FE, FE.OBFL_START_DT, FE.OBFL_END_DT, OC.OFFLOAD_DATE), 20),
  COALESCE(FE.OBFL_MAJOR_STAT_AREA_CDE, OC.MAJOR_STAT_AREA, 0),
  COALESCE(FE.OBFL_MINOR_STAT_AREA_CDE, OC.MINOR_STAT_AREA, 0),
  COALESCE(FE.OBFL_LOCALITY_CDE, OC.LOCALITY_CDE, 0),
  (CASE
    WHEN FE.Fishing_Depth IS NULL OR FE.Fishing_Depth<=0 THEN NULL
    ELSE CAST(ROUND(FE.Fishing_Depth,7) AS NUMERIC(15,7)) END)

--SELECT * FROM #OBSLOGS OL
--WHERE 
-- (OL.landed>0 OR OL.discard>0 OR OL.POP>0 OR OL.ORF>0 OR OL.TAR>0)

-- getData("pht_obsORF.sql","PacHarvest",strSpp="442")
-- qu("pht_obsORF.sql",dbName="PacHarvest",strSpp="442",dummy=1997:2006)
