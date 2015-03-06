-- Get catch-effort data for mapping density (2015-01-06)
SET NOCOUNT ON -- prevent timeout errors

SELECT 
  N.OBFL_HAIL_IN_NO as hail_in, 
  N.OBFL_SET_NO as set_no, 
  catch = Sum( CASE M.SPECIES_CODE
    WHEN @sppcode THEN M.LANDED + M.DISCARDED
    ELSE 0 END )
INTO #Catch
FROM 
  D_Merged_Catches M RIGHT OUTER JOIN
  B3_Fishing_Events N ON
  N.OBFL_HAIL_IN_NO = M.HAIL_IN_NO AND 
  N.OBFL_SET_NO = M.SET_NO
GROUP BY 
  N.OBFL_HAIL_IN_NO, N.OBFL_SET_NO

SELECT --TOP 100
  CONVERT(integer,E.OBFL_FISHERY_ID) AS fid,
  COALESCE(-E.OBFL_START_LONGITUDE, -E.OBFL_END_LONGITUDE, 0)AS X, 
  COALESCE(E.OBFL_START_LATITUDE, E.OBFL_END_LATITUDE, 0) AS Y, 
  ISNULL(-E.OBFL_END_LONGITUDE,0) AS X2, 
  ISNULL(E.OBFL_END_LATITUDE,0) AS Y2, 
  CASE
    WHEN E.OBFL_MAJOR_STAT_AREA_CDE IN (1) THEN '4B'
    WHEN E.OBFL_MAJOR_STAT_AREA_CDE IN (3) THEN '3C'
    WHEN E.OBFL_MAJOR_STAT_AREA_CDE IN (4) THEN '3D'
    WHEN E.OBFL_MAJOR_STAT_AREA_CDE IN (5) THEN '5A'
    WHEN E.OBFL_MAJOR_STAT_AREA_CDE IN (6) THEN '5B'
    WHEN E.OBFL_MAJOR_STAT_AREA_CDE IN (7) THEN '5C'
    WHEN E.OBFL_MAJOR_STAT_AREA_CDE IN (8) THEN '5D'
    WHEN E.OBFL_MAJOR_STAT_AREA_CDE IN (9) THEN '5E'
    ELSE '00' END AS PMFC,
  ISNULL(E.OBFL_DFO_MGMT_AREA_CDE,0) AS PFMA,
  ISNULL(E.OBFL_DFO_MGMT_SUBAREA_CDE,0) AS PFMS,
  ISNULL(E.Fishing_Depth,0) AS fdep,
  ISNULL(E.OBFL_GEAR_SUBTYPE_CDE,0) AS gear,
  CONVERT(smalldatetime,CONVERT(char(10),COALESCE(E.Start_FE, T.OBFL_OFFLOAD_DT, T.OBFL_DEPARTURE_DT), 20)) AS [date], 
  T.OBFL_VSL_CFV_NO AS cfv, 
  ISNULL(E.Duration, 0) AS eff, 
  CONVERT(real,C.catch) AS @sppcode 
INTO #PHTMAP
FROM 
  B2_Trips T 
  RIGHT OUTER JOIN B3_Fishing_Events E ON
    T.OBFL_HAIL_IN_NO = E.OBFL_HAIL_IN_NO 
  LEFT OUTER JOIN #Catch C ON 
    E.OBFL_HAIL_IN_NO = C.hail_in AND 
    E.OBFL_SET_NO = C.set_no
WHERE 
  IsNull(E.OBFL_FE_SUCCESS_CDE,0) IN (0,1) AND
  ISNULL(E.Fishing_Depth,0) <= 1600 AND
  ISNULL(E.Duration, 0) <= 24*60 AND
  COALESCE(-E.OBFL_START_LONGITUDE, -E.OBFL_END_LONGITUDE) IS NOT NULL AND
  COALESCE(E.OBFL_START_LATITUDE, E.OBFL_END_LATITUDE) IS NOT NULL

SELECT * INTO #FOSPAM
FROM OPENQUERY(GFSH,'
-- Query species catch from GFFOS on GFSH
SELECT
  CC.FID AS \"fid\",
  NVL(CC.X,0) AS X, NVL(CC.Y,0) AS Y,
  NVL(CC.X2,0) AS X2, NVL(CC.Y2,0) AS Y2,
  NVL(CC.PMFC,''00'') AS PMFC,
  NVL(CC.PFMA,0) AS PFMA,
  NVL(CC.PFMS,0) AS PFMS,
  NVL(CC.depth,0) AS \"fdep\",
  CC.GEAR AS \"gear\",
  CC.Edate AS \"date\",
  --TO_DATE(CC.Edate,''YYYY-MM-DD'') AS \"date\",
  CC.VRN AS \"cfv\",
  CC.effort AS \"eff\",
  CC.landed + CC.released + CC.liced + CC.bait AS \"catch\"
FROM 
  (SELECT
    OC.TRIP_ID,
    OC.FISHING_EVENT_ID,
    (CASE
      WHEN OC.FISHERY_SECTOR IN (''GROUNDFISH TRAWL'') THEN 1
      WHEN OC.FISHERY_SECTOR IN (''HALIBUT'',''HALIBUT AND SABLEFISH'') THEN 2
      WHEN OC.FISHERY_SECTOR IN (''SABLEFISH'') THEN 3
      WHEN OC.FISHERY_SECTOR IN (''LINGCOD'',''SPINY DOGFISH'') THEN 4
      WHEN OC.FISHERY_SECTOR IN (''ROCKFISH INSIDE'',''ROCKFISH OUTSIDE'') THEN 5
      ELSE 0 END) AS FID,
    (CASE
      WHEN OC.GEAR IN (''TRAWL'') AND OC.GEAR_SUBTYPE NOT IN (''MIDWATER TRAWL'') THEN 1
      WHEN OC.GEAR IN (''TRAP'') THEN 2
      WHEN OC.GEAR IN (''TRAWL'') AND OC.GEAR_SUBTYPE IN (''MIDWATER TRAWL'') THEN 3
      WHEN OC.GEAR IN (''HOOK AND LINE'') THEN 4
      WHEN OC.GEAR IN (''LONGLINE'') THEN 5
      WHEN OC.GEAR IN (''LONGLINE OR HOOK AND LINE'',''TRAP OR LONGLINE OR HOOK AND LINE'') THEN 8
      ELSE 0 END) AS GEAR,
    TO_CHAR(OC.BEST_DATE,''YYYY-MM-DD'') AS Edate,
    NVL(OC.VESSEL_REGISTRATION_NUMBER,0) AS VRN,
    (CASE
      WHEN OC.MAJOR_STAT_AREA_CODE IN (''01'') THEN ''4B''
      WHEN OC.MAJOR_STAT_AREA_CODE IN (''03'') THEN ''3C''
      WHEN OC.MAJOR_STAT_AREA_CODE IN (''04'') THEN ''3D''
      WHEN OC.MAJOR_STAT_AREA_CODE IN (''05'') THEN ''5A''
      WHEN OC.MAJOR_STAT_AREA_CODE IN (''06'') THEN ''5B''
      WHEN OC.MAJOR_STAT_AREA_CODE IN (''07'') THEN ''5C''
      WHEN OC.MAJOR_STAT_AREA_CODE IN (''08'') THEN ''5D''
      WHEN OC.MAJOR_STAT_AREA_CODE IN (''09'') THEN ''5E''
      ELSE ''00'' END) AS PMFC,
    NVL(to_number(OC.DFO_STAT_AREA_CODE),0) AS PFMA,
    NVL(OC.DFO_STAT_SUBAREA_CODE,0) AS PFMS,
    Avg(NVL(-OC.START_LONGITUDE,NULL)) AS X,
    Avg(NVL(OC.START_LATITUDE,NULL)) AS Y,
    Avg(NVL(-OC.END_LONGITUDE,NULL)) AS X2, 
    Avg(NVL(OC.END_LATITUDE,NULL)) AS Y2,
    Avg(COALESCE(OC.BEST_DEPTH_FM,OC.START_DEPTH_FM,NULL) * 6. * 0.3048) AS depth,  -- convert fathoms to metres
    Sum(CASE
      WHEN OC.SPECIES_CODE IN (@~sppcode) THEN
        NVL(OC.LANDED_ROUND_KG,0)
      ELSE 0 END) AS landed,
    Sum(CASE
      WHEN OC.SPECIES_CODE IN (@~sppcode) THEN
        COALESCE(OC.TOTAL_RELEASED_ROUND_KG,
        (NVL(OC.SUBLEGAL_RELEASED_COUNT,0) + NVL(OC.LEGAL_RELEASED_COUNT,0)) * FW.mnwt, 0)
      ELSE 0 END) AS released,
    Sum(CASE
      WHEN OC.SPECIES_CODE IN (@~sppcode) THEN
        (NVL(OC.SUBLEGAL_LICED_COUNT,0) + NVL(OC.LEGAL_LICED_COUNT,0)) * FW.mnwt
      ELSE 0 END) AS liced,
    Sum(CASE
      WHEN OC.SPECIES_CODE IN (@~sppcode) THEN
        (NVL(OC.SUBLEGAL_BAIT_COUNT,0) + NVL(OC.LEGAL_BAIT_COUNT,0)) * FW.mnwt
      ELSE 0 END) AS bait,
    Avg(
      CASE WHEN OC.START_DATE IS NOT NULL AND OC.END_DATE IS NOT NULL THEN
      TO_NUMBER(OC.END_DATE - OC.START_DATE) * 24. * 60. ELSE 0 END) AS effort,   --effort in minutes
    Sum(
      NVL(OC.LANDED_ROUND_KG,0) +
      COALESCE(OC.TOTAL_RELEASED_ROUND_KG,
        (NVL(OC.SUBLEGAL_RELEASED_COUNT,0) + NVL(OC.LEGAL_RELEASED_COUNT,0)) * FW.mnwt, 0) +
      (NVL(OC.SUBLEGAL_LICED_COUNT,0) + NVL(OC.LEGAL_LICED_COUNT,0)) * FW.mnwt +
      (NVL(OC.SUBLEGAL_BAIT_COUNT,0) + NVL(OC.LEGAL_BAIT_COUNT,0)) * FW.mnwt
      ) AS totcat
  FROM GFFOS.GF_D_OFFICIAL_FE_CATCH OC INNER JOIN
    GFFOS.MEAN_SPECIES_WEIGHT_VW FW ON
      OC.SPECIES_CODE = FW.spp
  WHERE
    OC.FISHERY_SECTOR IN (''GROUNDFISH TRAWL'') OR 
    (OC.FISHERY_SECTOR NOT IN (''GROUNDFISH TRAWL'') AND NVL(OC.DATA_SOURCE_CODE,0) NOT IN (106,107))
  GROUP BY
    OC.TRIP_ID, OC.FISHING_EVENT_ID,
    (CASE
      WHEN OC.FISHERY_SECTOR IN (''GROUNDFISH TRAWL'') THEN 1
      WHEN OC.FISHERY_SECTOR IN (''HALIBUT'',''HALIBUT AND SABLEFISH'') THEN 2
      WHEN OC.FISHERY_SECTOR IN (''SABLEFISH'') THEN 3
      WHEN OC.FISHERY_SECTOR IN (''LINGCOD'',''SPINY DOGFISH'') THEN 4
      WHEN OC.FISHERY_SECTOR IN (''ROCKFISH INSIDE'',''ROCKFISH OUTSIDE'') THEN 5
      ELSE 0 END),
    (CASE
      WHEN OC.GEAR IN (''TRAWL'') AND OC.GEAR_SUBTYPE NOT IN (''MIDWATER TRAWL'') THEN 1
      WHEN OC.GEAR IN (''TRAP'') THEN 2
      WHEN OC.GEAR IN (''TRAWL'') AND OC.GEAR_SUBTYPE IN (''MIDWATER TRAWL'') THEN 3
      WHEN OC.GEAR IN (''HOOK AND LINE'') THEN 4
      WHEN OC.GEAR IN (''LONGLINE'') THEN 5
      WHEN OC.GEAR IN (''LONGLINE OR HOOK AND LINE'',''TRAP OR LONGLINE OR HOOK AND LINE'') THEN 8
      ELSE 0 END),
    TO_CHAR(OC.BEST_DATE,''YYYY-MM-DD''),
    NVL(OC.VESSEL_REGISTRATION_NUMBER,0),
    (CASE
      WHEN OC.MAJOR_STAT_AREA_CODE IN (''01'') THEN ''4B''
      WHEN OC.MAJOR_STAT_AREA_CODE IN (''03'') THEN ''3C''
      WHEN OC.MAJOR_STAT_AREA_CODE IN (''04'') THEN ''3D''
      WHEN OC.MAJOR_STAT_AREA_CODE IN (''05'') THEN ''5A''
      WHEN OC.MAJOR_STAT_AREA_CODE IN (''06'') THEN ''5B''
      WHEN OC.MAJOR_STAT_AREA_CODE IN (''07'') THEN ''5C''
      WHEN OC.MAJOR_STAT_AREA_CODE IN (''08'') THEN ''5D''
      WHEN OC.MAJOR_STAT_AREA_CODE IN (''09'') THEN ''5E''
      ELSE ''00'' END),
    NVL(to_number(OC.DFO_STAT_AREA_CODE),0),
    NVL(OC.DFO_STAT_SUBAREA_CODE,0)
  ) CC
WHERE
  --ROWNUM <= 100 AND
  CC.Edate IS NOT NULL AND
  NVL(CC.effort,0) > 1 AND NVL(CC.effort,0) <= 24*60 AND
  NVL(CC.depth,0) <= 1600 AND
  NVL(CC.X,0) + NVL(CC.X2,0) < 0 AND
  NVL(CC.Y,0) + NVL(CC.Y2,0) > 0

')

SELECT 
  CONVERT(integer, FP.fid) AS fid,
  FP.X, FP.Y, FP.X2, FP.Y2, FP.PMFC, FP.PFMA, FP.PFMS, FP.fdep, FP.gear,
  CONVERT(smalldatetime, FP.date, 20) AS [date],
  FP.cfv, FP.eff, 
  CONVERT(real,FP.catch) AS @sppcode
INTO #FOSMAP
FROM #FOSPAM FP
WHERE FP.date>='2006' and FP.date <= CONVERT(CHAR(10),CURRENT_TIMESTAMP,20)

SELECT * INTO #ALLMAP FROM #PHTMAP 
  UNION ALL SELECT * FROM #FOSMAP

SELECT *,
  'GMA' = CASE
    WHEN AM.PFMA IN (21,23,24,121,123) OR
          (AM.PFMA IN (124) AND AM.PFMS IN (1,2,3)) OR
          (AM.PFMA IN (125) AND AM.PFMS IN (6)) THEN '3C'
    WHEN AM.PFMA IN (25,26,126) OR
          (AM.PFMA IN (27) AND AM.PFMS IN (2,3,4,5,6,7,8,9,10,11)) OR
          (AM.PFMA IN (124) AND AM.PFMS IN (4)) OR
          (AM.PFMA IN (125) AND AM.PFMS IN (1,2,3,4,5)) OR
          (AM.PFMA IN (127) AND AM.PFMS IN (1,2)) THEN '3D'
    WHEN AM.PFMA IN (13,14,15,16,17,18,19,20,28,29) OR
          (AM.PFMA IN (12) AND AM.PFMS NOT IN (14)) THEN '4B'
    WHEN AM.PFMA IN (11,111) OR
          (AM.PFMA IN (12) AND AM.PFMS IN (14)) OR
          (AM.PFMA IN (27) AND AM.PFMS IN (1)) OR
          (AM.PFMA IN (127) AND AM.PFMS IN (3,4)) OR
          (AM.PFMA IN (130) AND AM.PFMS IN (1)) THEN '5A'
    WHEN AM.PFMA IN (6,106) OR
          (AM.PFMA IN (2) AND AM.PFMS BETWEEN 1 AND 19) OR
          (AM.PFMA IN (102) AND AM.PFMS IN (2)) OR
          (AM.PFMA IN (105) AND AM.PFMS IN (2)) OR
          (AM.PFMA IN (107) AND AM.PFMS IN (1)) OR
          (@sppcode IN ('396','440') AND AM.PFMA IN (102) AND AM.PFMS IN (3)) OR
          -- note: these next lines identify tows in the four-sided polygon SW of Cape St. James
          (@sppcode IN ('396','440') AND COALESCE(AM.X,AM.X2,NULL) IS NOT NULL AND COALESCE(AM.Y,AM.Y2,NULL) IS NOT NULL AND
            -- top, right, bottom, left (note: X already negative)
            COALESCE(AM.Y,AM.Y2)  <= 52.33333 AND
            COALESCE(AM.X,AM.X2)  <= ((COALESCE(AM.Y,AM.Y2)+29.3722978)/(-0.6208634)) AND
            COALESCE(AM.Y,AM.Y2)  >= (92.9445665+(0.3163707*COALESCE(AM.X,AM.X2))) AND
            COALESCE(AM.X,AM.X2)  >= ((COALESCE(AM.Y,AM.Y2)+57.66623)/(-0.83333)) ) THEN '5C'
    WHEN AM.PFMA IN (7,8,9,10,108,109,110) OR
          (@sppcode NOT IN ('396') AND AM.PFMA IN (102) AND AM.PFMS IN (3)) OR
          (AM.PFMA IN (107) AND AM.PFMS IN (2,3)) OR
          (AM.PFMA IN (130) AND AM.PFMS IN (2)) OR
          (AM.PFMA IN (130) AND AM.PFMS IN (3) AND
            COALESCE(AM.Y,AM.Y2,99)<=51.93333) THEN '5B'
    WHEN AM.PFMA IN (3,4,5,103,104) OR
          (AM.PFMA IN (1) AND AM.PFMS IN (2,3,4,5)) OR
          (AM.PFMA IN (101) AND AM.PFMS BETWEEN 4 AND 10) OR
          (AM.PFMA IN (102) AND AM.PFMS IN (1)) OR
          (AM.PFMA IN (105) AND AM.PFMS IN (1)) THEN '5D'
    WHEN AM.PFMA IN (142) OR
          (AM.PFMA IN (1) AND AM.PFMS IN (1)) OR
          (AM.PFMA IN (2) AND AM.PFMS BETWEEN 31 AND 100) OR
          (AM.PFMA IN (101) AND AM.PFMS IN (1,2,3)) OR
          (AM.PFMA IN (130) AND AM.PFMS IN (3) AND 
            COALESCE(AM.Y,AM.Y2,0)>51.93333) THEN '5E'
    ELSE '00' END
FROM #ALLMAP AM

--getData("pht_map_density.sql","PacHarvest",strSpp="396")
-- getData("fos_map_density.sql","GFFOS",strSpp="394",server="GFSH",type="ORA",trusted=F)

