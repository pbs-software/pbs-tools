-- Norm Olsen's query (greatly modified) to get top N percentage catch
SET NOCOUNT ON

SELECT
  E.OBFL_HAIL_IN_NO, E.OBFL_SET_NO
INTO #SETS
FROM 
  B3_Fishing_Events E
  INNER JOIN D_Merged_Catches C ON
    E.OBFL_HAIL_IN_NO = C.HAIL_IN_NO AND
    E.OBFL_SET_NO = C.SET_NO
WHERE 
  C.SPECIES_CODE IN (@sppcode) AND
  E.OBFL_LOG_TYPE_CDE IN ('OBSERVRLOG') AND   -- flag for consideration
  COALESCE(E.OBFL_GEAR_SUBTYPE_CDE,1) IN (@dummy) AND
  E.OBFL_MAJOR_STAT_AREA_CDE IN (@major) AND
  COALESCE(
    NULLIF(E.Fishing_Depth,0), NULLIF(E.OBFL_START_BOTTOM_DTH,0),
    NULLIF(E.OBFL_END_BOTTOM_DTH,0), NULLIF(E.OBFL_MID_BOTTOM_DTH,0),
    NULLIF(E.OBFL_MIN_DTH,0), NULLIF(E.OBFL_MAX_DTH,0), 0) BETWEEN @mindep AND @maxdep

SELECT --TOP 100
  C.SPECIES_CODE, C.LANDED, C.DISCARDED
INTO #PHTCAT
FROM
  D_Merged_Catches C
  INNER JOIN #SETS S ON
    S.OBFL_HAIL_IN_NO = C.HAIL_IN_NO AND
    S.OBFL_SET_NO = C.SET_NO 
WHERE
  C.SPECIES_CODE NOT IN ('004','848','849','999','XXX')

SELECT * INTO #FOSCAT
FROM OPENQUERY(GFSH,
'SELECT
  C.SPECIES_CODE,
  NVL(C.LANDED_ROUND_KG,0) AS LANDED,
  COALESCE(C.TOTAL_RELEASED_ROUND_KG,
    (NVL(C.SUBLEGAL_RELEASED_COUNT,0) + NVL(C.LEGAL_RELEASED_COUNT,0)) * FW.MNWT, 0) +  -- released
    (NVL(C.SUBLEGAL_LICED_COUNT,0) + NVL(C.LEGAL_LICED_COUNT,0)) * FW.MNWT +            -- liced
    (NVL(C.SUBLEGAL_BAIT_COUNT,0) + NVL(C.LEGAL_BAIT_COUNT,0)) * FW.MNWT AS DISCARDED
FROM 
  GFFOS.GF_D_OFFICIAL_FE_CATCH C
  INNER JOIN (SELECT
  OC.TRIP_ID, OC.FISHING_EVENT_ID
  FROM
    GFFOS.GF_D_OFFICIAL_FE_CATCH OC
  WHERE 
    OC.SPECIES_CODE  IN (@~sppcode) AND
    OC.DATA_SOURCE_CODE IN (106) AND   -- flag for consideration
    OC.FISHERY_SECTOR IN (''GROUNDFISH TRAWL'') AND
    COALESCE(OC.MAJOR_STAT_AREA_CODE,''00'') IN (@major) AND
    COALESCE(OC.BEST_DEPTH_FM,OC.START_DEPTH_FM,0)*1.8288 BETWEEN @mindep AND @maxdep AND
    (CASE WHEN
      (OC.GEAR_SUBTYPE IN (''HARD BOTTOM TRAWL'',''SOFT BOTTOM TRAWL'') OR   -- bottom
      (OC.GEAR_SUBTYPE IN (''UNSPECIFIED'') AND 
      OC.TRIP_CATEGORY NOT IN (''OPT A - HAKE QUOTA (GULF)'',''OPT A - HAKE QUOTA (SHORESIDE)'',''OPT A - HAKE QUOTA (JV)'')))
      THEN 1
    WHEN
      (OC.GEAR_SUBTYPE IN (''MIDWATER TRAWL'') OR   -- midwater
      (OC.GEAR_SUBTYPE IN (''UNSPECIFIED'') AND 
      OC.TRIP_CATEGORY IN (''OPT A - HAKE QUOTA (GULF)'',''OPT A - HAKE QUOTA (SHORESIDE)'',''OPT A - HAKE QUOTA (JV)'')))
    THEN 3 ELSE 0 END) IN (@dummy) ) SETS ON
    C.TRIP_ID = SETS.TRIP_ID AND
    C.FISHING_EVENT_ID = SETS.FISHING_EVENT_ID
  INNER JOIN GFFOS.MEAN_SPECIES_WEIGHT_VW FW ON -- FISH WEIGHTS FW
    C.SPECIES_CODE = FW.SPP
  WHERE
    --ROWNUM <= 100 AND
    C.SPECIES_CODE NOT IN (''004'',''848'',''849'',''999'',''XXX'') ')

SELECT * 
INTO #ALLCAT
FROM #PHTCAT 
  UNION ALL SELECT * FROM #FOSCAT

DECLARE @total AS FLOAT
SET @total = (SELECT 
  SUM(AC.LANDED + AC.DISCARDED) AS TOTAL
  FROM  #ALLCAT AC )

SELECT TOP @top 
  SP.SPECIES_CDE AS code,
  SP.SPECIES_DESC AS spp,
  SUM(AC.LANDED + AC.DISCARDED)/1e6 AS catKt,
  SUM(AC.LANDED + AC.DISCARDED) / @total * 100 AS pct
FROM
  #ALLCAT AC
  INNER JOIN C_Species SP ON
    AC.SPECIES_CODE = SP.SPECIES_CDE
GROUP BY SP.SPECIES_CDE, SP.SPECIES_DESC
ORDER BY SUM(AC.LANDED + AC.DISCARDED) / @total DESC

-- data(species); fish=species$code[species$fish]
-- mess=paste("getData(\"pht_concurrent.sql\",\"PacHarvest\",strSpp=c(\"",paste(fish,collapse="\",\""),"\"),mindep=330,maxdep=500,dummy=1)",sep="") -- all fish
-- eval(parse(text=mess))

-- getData("pht_concurrent.sql","PacHarvest",strSpp="424",mindep=49,maxdep=101,dummy=1)  -- dummy essential to make query work
-- getData("pht_concurrent.sql","PacHarvest",strSpp="396",major=5:7,mindep=70,maxdep=441,dummy=1) -- QCS (567)

