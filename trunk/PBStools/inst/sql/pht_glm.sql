SET NOCOUNT ON 

SELECT 
  N.OBFL_HAIL_IN_NO as hail_in, 
  N.OBFL_SET_NO as set_no, 
  catch = Sum( CASE 
    WHEN M.SPECIES_CODE IN (@sppcode) THEN M.LANDED + M.DISCARDED
    ELSE 0 END ),
  bycatch = Sum( CASE 
    WHEN M.SPECIES_CODE IN (@tarcode) THEN M.LANDED + M.DISCARDED
    ELSE 0 END )INTO #Catch
FROM 
  D_Merged_Catches M RIGHT OUTER JOIN
  B3_Fishing_Events N ON
  N.OBFL_HAIL_IN_NO = M.HAIL_IN_NO AND 
  N.OBFL_SET_NO = M.SET_NO
GROUP BY 
  N.OBFL_HAIL_IN_NO, N.OBFL_SET_NO

SELECT -- Top 20
  'year'  = IsNull(Year(COALESCE(E.Start_FE,E.OBFL_START_DT,E.OBFL_END_DT,T.OBFL_DEPARTURE_DT,T.OBFL_OFFLOAD_DT)),9999),
  'month' = IsNull(Month(COALESCE(E.Start_FE,E.OBFL_START_DT,E.OBFL_END_DT,T.OBFL_DEPARTURE_DT,T.OBFL_OFFLOAD_DT)),0),
  'depth' = CAST(ROUND(CASE
    WHEN E.Fishing_Depth > 0 AND E.Fishing_Depth Is Not Null THEN E.Fishing_Depth
    WHEN E.OBFL_MIN_DTH >0 AND E.OBFL_MIN_DTH Is Not Null AND E.OBFL_MAX_DTH >0 AND E.OBFL_MAX_DTH Is Not Null THEN 
      (E.OBFL_MIN_DTH + E.OBFL_MAX_DTH) / 2.
    ELSE 0 END ,7) AS NUMERIC(12,7)),
  'major' = IsNull(E.OBFL_MAJOR_STAT_AREA_CDE,0),
  'minor' = IsNull(E.OBFL_MINOR_STAT_AREA_CDE,0),
  'locality' = IsNull(E.OBFL_LOCALITY_CDE,0),
  'gear'  = IsNull(E.OBFL_GEAR_SUBTYPE_CDE,0),
  'latitude' = CAST(ROUND(COALESCE(E.OBFL_START_LATITUDE, E.OBFL_END_LATITUDE),7) AS NUMERIC(11,7)),
  --'latitude' = COALESCE(E.OBFL_START_LATITUDE, E.OBFL_END_LATITUDE),
  'vessel' = IsNull(T.OBFL_VSL_CFV_NO,0),
  'effort' = IsNull(E.Duration, 0), 
  C.catch, C.bycatch
FROM 
  B2_Trips T RIGHT OUTER JOIN 
  (B3_Fishing_Events E LEFT OUTER JOIN 
  #Catch C ON 
  E.OBFL_HAIL_IN_NO = C.hail_in AND 
  E.OBFL_SET_NO = C.set_no ) ON 
  T.OBFL_HAIL_IN_NO = E.OBFL_HAIL_IN_NO 
WHERE 
  -- IsNull(E.OBFL_GEAR_SUBTYPE_CDE,0) IN (1) AND 
  IsNull(E.OBFL_FE_SUCCESS_CDE,0) IN (0,1)AND
  --(E.Start_FE>='1996-04-01' AND E.Start_FE<='2007-03-31') AND
  -- COALESCE(E.OBFL_START_LATITUDE, E.OBFL_END_LATITUDE) IS NOT NULL AND
  -- IsNull(T.OBFL_VSL_CFV_NO,0) > 0 AND
  ISNULL(E.Duration, 0) > 0

-- getData("pht_glm.sql","PacHarvest",strSpp="453",tarSpp="455")

