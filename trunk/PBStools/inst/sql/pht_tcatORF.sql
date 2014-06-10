-- Last modified by RH (2014-06-06)
-- PacHarvest query for landed catch of a target species, POP, and ORF (other rockfish)
SET NOCOUNT ON  -- prevents timeout errors

SELECT 
  MC.HAIL_IN_NO, 
  MC.SET_NO, 
  OC.OFFLOAD_DATE,
  OC.MAJOR_STAT_AREA,
  OC.MINOR_STAT_AREA,
  landed = Sum(CASE
    WHEN MC.SPECIES_CODE IN (@sppcode) THEN MC.LANDED
    ELSE 0 END ),
  discard = Sum(CASE
    WHEN MC.SPECIES_CODE IN (@sppcode) THEN MC.DISCARDED
    ELSE 0 END ),
  POP = Sum(CASE
    WHEN MC.SPECIES_CODE IN ('396') THEN MC.LANDED
    ELSE 0 END ),
  ORF = Sum(CASE   -- landings of all rockfish other than POP
    WHEN MC.SPECIES_CODE IN (@orfcode) THEN MC.LANDED
    ELSE 0 END ),
  TAR = Sum(CASE   -- target landings reference for discard calculations
    WHEN MC.SPECIES_CODE IN (@trfcode) THEN MC.LANDED
   ELSE 0 END)
  INTO #Catch
  FROM 
    D_Merged_Catches MC LEFT OUTER JOIN
    D_Official_Catch OC ON
    MC.HAIL_IN_NO = OC.HAIL_IN_NO AND 
    MC.SET_NO = OC.SET_NO AND 
    MC.SPECIES_CODE = OC.SPECIES_CODE
  GROUP BY 
    MC.HAIL_IN_NO, MC.SET_NO,
    OC.OFFLOAD_DATE, OC.MAJOR_STAT_AREA, OC.MINOR_STAT_AREA

SELECT 
  'fid' = IsNull(E.OBFL_FISHERY_ID,1),
  'gear' = IsNull(E.OBFL_GEAR_SUBTYPE_CDE,0),
  'log' = CASE WHEN E.OBFL_LOG_TYPE_CDE='OBSERVRLOG' THEN 1 ELSE 2 END,
  CONVERT(char(10),COALESCE(E.Start_FE, E.OBFL_START_DT, E.OBFL_END_DT, C.OFFLOAD_DATE), 20) AS [date],
  COALESCE(E.OBFL_MAJOR_STAT_AREA_CDE, C.MAJOR_STAT_AREA, 0) AS major,
  COALESCE(E.OBFL_MINOR_STAT_AREA_CDE, C.MINOR_STAT_AREA, 0) AS minor,
  --IsNull(E.Fishing_Depth, Null) AS fdep, 
  (CASE
    WHEN E.Fishing_Depth IS NULL OR E.Fishing_Depth<=0 THEN NULL
    ELSE CAST(ROUND(E.Fishing_Depth,7) AS NUMERIC(15,7)) END) AS fdep,
--  T.OBFL_VSL_CFV_NO AS cfv, 
  IsNull(E.Duration, Null) AS eff, 
  C.landed,
  C.discard,
  C.POP,
  C.ORF,
  C.TAR
  FROM 
--    B2_Trips T RIGHT OUTER JOIN 
    B3_Fishing_Events E RIGHT OUTER JOIN 
    #Catch C ON 
      E.OBFL_HAIL_IN_NO = C.HAIL_IN_NO AND 
      E.OBFL_SET_NO = C.SET_NO --) ON 
      --T.OBFL_HAIL_IN_NO = E.OBFL_HAIL_IN_NO 
  WHERE 
    -- E.OBFL_GEAR_SUBTYPE_CDE=1 AND 
    -- IsNull(E.OBFL_FE_SUCCESS_CDE,0) in (0,1) AND
    (C.landed>0 OR C.discard>0 OR C.POP>0 OR C.ORF>0 OR C.TAR>0)

-- getData("pht_tcatORF.sql","PacHarvest",strSpp="415")

