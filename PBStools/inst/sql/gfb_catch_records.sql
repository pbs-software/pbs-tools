-- Research survey catch for 'getCatch', 'weightBio', and 'requestAges'.
-- This query is no longer duplicated as 'gfb_gfb_catch.sql', formerly use by 'requestAges'.
-- Last revised: RH 2021-03-01

SET NOCOUNT ON  -- prevents timeout errors

-- =====ASSOCIATE TRIPS WITH SURVEYS=====

-- Identify surveys by original index only (see gfb_bio.sql, RH 181219) - some restrats may need to be added over time
SELECT
  S.SURVEY_SERIES_ID,
  S.SURVEY_ID
INTO #ORIGINAL_SURVEYS
FROM SURVEY S
WHERE
  (S.ORIGINAL_IND='Y' AND S.SURVEY_ID NOT IN (79)) -- Exclude original stratification for 2006 WCHG Synoptic (use SVID 123)
  OR
  (S.ORIGINAL_IND='N' AND S.SURVEY_SERIES_ID IN (21) AND S.SURVEY_ID BETWEEN 91 AND 104) -- Special case for GIG Historical
ORDER BY
  S.SURVEY_SERIES_ID, S.SURVEY_ID

-- Merge tables to get TRIP_ID, SURVEY_ID, and SURVEY_SERIES_ID (see gfb_bio.sql, RH 181219)
-- Added TRIP_ACTIVITY_CODE (RH 190807)
SELECT
  TS.TRIP_ID,
  TAC.ACTIVITY_CODE,               -- 190624: Trip activity code might provide a clue if survey ID is missing (MS)
  CASE
    WHEN OS.SURVEY_SERIES_ID IS NULL THEN 999
    WHEN OS.SURVEY_SERIES_ID IN (6,7) THEN 670          -- Shrimp trawl surveys
    WHEN OS.SURVEY_SERIES_ID IN (35,41,42,43) THEN 350  -- Sablefish surveys
    WHEN OS.SURVEY_SERIES_ID BETWEEN 82 AND 87 THEN 820 -- Jig surveys
    WHEN OS.SURVEY_ID IN (130) THEN 40                  -- HBLL South survey
    WHEN OS.SURVEY_ID IN (131) THEN 39                  -- HBLL North survey
    WHEN OS.SURVEY_SERIES_ID IN (10,21) THEN 21         -- GIG historical
    ELSE OS.SURVEY_SERIES_ID END AS SURVEY_SERIES_ID,
  MAX(OS.SURVEY_ID) AS SURVEY_ID
INTO #TripSurvSer
FROM 
  TRIP_ACTIVITY TAC RIGHT OUTER JOIN
  TRIP T INNER JOIN
  (#ORIGINAL_SURVEYS OS INNER JOIN
  TRIP_SURVEY TS ON
    OS.SURVEY_ID = TS.SURVEY_ID) ON
    T.TRIP_ID = TS.TRIP_ID ON
    T.TRIP_ID = TAC.TRIP_ID
--WHERE T.TRIP_ID IN (10921,62066)
GROUP BY
  TS.TRIP_ID,
  TAC.ACTIVITY_CODE,
  CASE
    WHEN OS.SURVEY_SERIES_ID IS NULL THEN 999
    WHEN OS.SURVEY_SERIES_ID IN (6,7) THEN 670          -- Shrimp trawl surveys
    WHEN OS.SURVEY_SERIES_ID IN (35,41,42,43) THEN 350  -- Sablefish surveys
    WHEN OS.SURVEY_SERIES_ID BETWEEN 82 AND 87 THEN 820 -- Jig surveys
    WHEN OS.SURVEY_ID IN (130) THEN 40                  -- HBLL South survey
    WHEN OS.SURVEY_ID IN (131) THEN 39                  -- HBLL North survey
    WHEN OS.SURVEY_SERIES_ID IN (10,21) THEN 21         -- GIG historical
    ELSE OS.SURVEY_SERIES_ID END


-- Gather earliest GROUPING_CODE by FISHING_EVENT_ID (changed 180129 to match gfb_bio.sql)
-- This is likely to match the original series SSID
-- Not ideal because some fishing events are used in numerous re-stratifications. (e.g., FEID=886052)
SELECT *
INTO #FishEventGroup
FROM
  (SELECT *,
    ROW_NUMBER() OVER (PARTITION BY LEG.FISHING_EVENT_ID ORDER BY LEG.GROUPING_CODE ASC) AS RN
  FROM FISHING_EVENT_GROUPING LEG) AS FEG
WHERE FEG.RN = 1
ORDER BY FEG.FISHING_EVENT_ID


-- =====BEST VALUES FROM FISHING EVENTS=====
-- Derive `Best` values from B02 Fishing Events
SELECT --TOP 20
  B02.TRIP_ID,
  B02.FISHING_EVENT_ID,
  B02.FE_MAJOR_LEVEL_ID,
  'Best_Long' = CASE
    WHEN B02.FE_START_LONGITUDE_DEGREE > 0 AND B02.FE_START_LONGITUDE_DEGREE IS NOT NULL AND 
      B02.FE_START_LONGITUDE_MINUTE IS NOT NULL AND B02.FE_END_LONGITUDE_DEGREE > 0 AND 
      B02.FE_END_LONGITUDE_DEGREE IS NOT NULL AND B02.FE_END_LONGITUDE_MINUTE IS NOT NULL THEN
      ((B02.FE_START_LONGITUDE_DEGREE + (B02.FE_START_LONGITUDE_MINUTE / 60.0)) + (B02.FE_END_LONGITUDE_DEGREE + (B02.FE_END_LONGITUDE_MINUTE / 60.0))) / 2.0
    WHEN B02.FE_START_LONGITUDE_DEGREE > 0 AND B02.FE_START_LONGITUDE_DEGREE IS NOT NULL AND B02.FE_START_LONGITUDE_MINUTE IS NOT NULL THEN
      B02.FE_START_LONGITUDE_DEGREE + (B02.FE_START_LONGITUDE_MINUTE / 60.0)
    WHEN B02.FE_END_LONGITUDE_DEGREE > 0 AND B02.FE_END_LONGITUDE_DEGREE IS NOT NULL AND B02.FE_END_LONGITUDE_MINUTE IS NOT NULL THEN
      B02.FE_END_LONGITUDE_DEGREE + (B02.FE_END_LONGITUDE_MINUTE / 60.0)
    ELSE NULL END,
  'Best_Lat' = CASE
    WHEN B02.FE_START_LATTITUDE_DEGREE > 0 AND B02.FE_START_LATTITUDE_DEGREE IS NOT NULL AND 
      B02.FE_START_LATTITUDE_MINUTE IS NOT NULL AND B02.FE_END_LATTITUDE_DEGREE > 0 AND 
      B02.FE_END_LATTITUDE_DEGREE IS NOT NULL AND B02.FE_END_LATTITUDE_MINUTE IS NOT NULL THEN
      ((B02.FE_START_LATTITUDE_DEGREE + (B02.FE_START_LATTITUDE_MINUTE / 60.0)) + (B02.FE_END_LATTITUDE_DEGREE + (B02.FE_END_LATTITUDE_MINUTE / 60.0))) / 2.0
    WHEN B02.FE_START_LATTITUDE_DEGREE > 0 AND B02.FE_START_LATTITUDE_DEGREE IS NOT NULL AND B02.FE_START_LATTITUDE_MINUTE IS NOT NULL THEN
      B02.FE_START_LATTITUDE_DEGREE + (B02.FE_START_LATTITUDE_MINUTE / 60.0)
    WHEN B02.FE_END_LATTITUDE_DEGREE > 0 AND B02.FE_END_LATTITUDE_DEGREE IS NOT NULL AND B02.FE_END_LATTITUDE_MINUTE IS NOT NULL THEN
      B02.FE_END_LATTITUDE_DEGREE + (B02.FE_END_LATTITUDE_MINUTE / 60.0)
    ELSE NULL END,
  'Best_Depth' = CASE
    WHEN B02.GEAR_CODE IN (0,1,2,4,5,8,11,13,14,16) THEN  -- bottom gear
      COALESCE(B02.FE_BEGINNING_BOTTOM_DEPTH, B02.FE_END_BOTTOM_DEPTH, B02.FE_MODAL_BOTTOM_DEPTH, B02.FE_MIN_BOTTOM_DEPTH, B02.FE_MAX_BOTTOM_DEPTH,
        B02.FE_BEGINNING_GEAR_DEPTH, B02.FE_END_GEAR_DEPTH, B02.FE_MODAL_GEAR_DEPTH, B02.FE_MIN_GEAR_DEPTH, B02.FE_MAX_GEAR_DEPTH, 
        B02.FE_BEGIN_CAPTURE_DEPTH, B02.FE_END_CAPTURE_DEPTH, B02.FE_MODAL_CAPTURE_DEPTH, B02.FE_MIN_CAPTURE_DEPTH, B02.FE_MAX_CAPTURE_DEPTH,
        B02.FE_BEGIN_TARGET_DEPTH, B02.FE_END_TARGET_DEPTH, B02.FE_MODAL_TARGET_DEPTH, B02.FE_MIN_TARGET_DEPTH, B02.FE_MAX_TARGET_DEPTH)
    WHEN B02.GEAR_CODE IN (3,6,7,9,10,12,17,18,22) THEN  -- midwater gear
      COALESCE(B02.FE_BEGINNING_GEAR_DEPTH, B02.FE_END_GEAR_DEPTH, B02.FE_MODAL_GEAR_DEPTH, B02.FE_MIN_GEAR_DEPTH, B02.FE_MAX_GEAR_DEPTH,
        B02.FE_BEGIN_CAPTURE_DEPTH, B02.FE_END_CAPTURE_DEPTH, B02.FE_MODAL_CAPTURE_DEPTH, B02.FE_MIN_CAPTURE_DEPTH, B02.FE_MAX_CAPTURE_DEPTH, 
        B02.FE_BEGIN_TARGET_DEPTH, B02.FE_END_TARGET_DEPTH, B02.FE_MODAL_TARGET_DEPTH, B02.FE_MIN_TARGET_DEPTH, B02.FE_MAX_TARGET_DEPTH,
        B02.FE_BEGINNING_BOTTOM_DEPTH, B02.FE_END_BOTTOM_DEPTH, B02.FE_MODAL_BOTTOM_DEPTH, B02.FE_MIN_BOTTOM_DEPTH, B02.FE_MAX_BOTTOM_DEPTH)
    ELSE 0 END
INTO #BestEvents
FROM 
  B02_FISHING_EVENT B02
WHERE B02.FE_SUB_LEVEL_ID IS NULL  -- FISHING_EVENT_ID REPEATED MANY TIMES FOR HOOKS AND TRAPS IF NOT NULL (STUPID IDEA)


-- ===== Tie everything together =====
SELECT --TOP 20
  T.TRIP_ID AS TID,
  E.FISHING_EVENT_ID AS FEID,
  E.FE_MAJOR_LEVEL_ID AS [set],
  C.CATCH_ID AS CID,                                     -- B03_CATCH
  'SSID' = CASE                                             -- SURVEY
    WHEN TSS.SURVEY_SERIES_ID IS NULL OR TSS.SURVEY_SERIES_ID IN (0) THEN (CASE
      WHEN E.BLOCK_DESIGNATION IN ('TASU','FLAMINGO') THEN 22
      WHEN E.BLOCK_DESIGNATION IN ('TRIANGLE','BROOKS') THEN 36
      ELSE TSS.SURVEY_SERIES_ID END)
    ELSE TSS.SURVEY_SERIES_ID END,
  'SVID' = TSS.SURVEY_ID,
  'AC'   = TSS.ACTIVITY_CODE,
  --'OI' = TSS.ORIGINAL_IND, --CASE
    --WHEN TSS.ORIGINAL_IND IN ('Y') THEN 'TRUE'
    --ELSE 'FALSE' END,
  'GC'   = CASE                                             -- SURVEY_GROUPING
    WHEN E.GROUPING_CODE IS NOT NULL THEN E.GROUPING_CODE
    --WHEN TSSG.GROUPING_CODE IS NOT NULL THEN TSSG.GROUPING_CODE
    WHEN E.BLOCK_DESIGNATION IN ('TASU','FLAMINGO') THEN (CASE
      WHEN BB.Best_Depth BETWEEN 20 AND 70 THEN 321
      WHEN BB.Best_Depth BETWEEN 70.001 AND 150 THEN 322
      WHEN BB.Best_Depth BETWEEN 150.001 AND 260 THEN 323
      ELSE NULL END)
    WHEN E.BLOCK_DESIGNATION IN ('TRIANGLE','BROOKS') THEN (CASE
      WHEN BB.Best_Depth BETWEEN 20 AND 70 THEN 324
      WHEN BB.Best_Depth BETWEEN 70.001 AND 150 THEN 325
      WHEN BB.Best_Depth BETWEEN 150.001 AND 260 THEN 326
      ELSE NULL END)
    ELSE NULL END,
  E.BLOCK_DESIGNATION AS block,
  CONVERT(smalldatetime,CONVERT(char(10),T.TRIP_START_DATE,20)) AS [date], 
  Year(T.TRIP_START_DATE) AS [year],
  IsNull(E.MAJOR_STAT_AREA_CODE,0) AS major,
  IsNull(E.MINOR_STAT_AREA_CODE,0) AS minor,
  ISNULL(-BB.Best_Long,0) As X,
  ISNULL(BB.Best_Lat,0) As Y,
  IsNull(E.LOCALITY_CODE,0) AS locality,
  ISNULL(BB.Best_Depth,0) As depth,
  ISNULL(E.GEAR_CODE,0) AS gear,
  C.SPECIES_CODE AS spp, 
  C.CATCH_WEIGHT AS wt, 
  C.CATCH_COUNT AS pcs, 
  COALESCE(C.CATCH_WEIGHT, C.CATCH_COUNT * F.fishwt, 0) AS catKg
INTO #B01B03
FROM 
  #TripSurvSer TSS INNER JOIN
  (#BestEvents BB RIGHT OUTER JOIN
  (B01_TRIP T INNER JOIN 
  (B02_FISHING_EVENT E INNER JOIN 
  (B02L3_Link_Fishing_Event_Catch L INNER JOIN
  (B03_CATCH C INNER JOIN 
  (SELECT
    C.SPECIES_CODE,
    Avg(C.CATCH_WEIGHT) AS mnwt, 
    Avg(C.CATCH_COUNT) AS mnct, 
    Count(C.CATCH_WEIGHT) AS n, 
    Avg(C.CATCH_WEIGHT/C.CATCH_COUNT) AS fishwt
    FROM B03_CATCH C
    WHERE
      C.SPECIES_CODE IN (@sppcode) AND 
      C.CATCH_WEIGHT > 0 AND
      C.CATCH_COUNT > 1
    GROUP BY 
      C.SPECIES_CODE ) F ON 
    C.SPECIES_CODE = F.SPECIES_CODE ) ON
    L.CATCH_ID = C.CATCH_ID) ON 
    E.TRIP_ID = L.TRIP_ID AND
    E.FISHING_EVENT_ID = L.FISHING_EVENT_ID) ON
    T.TRIP_ID = E.TRIP_ID ) ON
    T.TRIP_ID = BB.TRIP_ID AND
    E.FISHING_EVENT_ID = BB.FISHING_EVENT_ID AND
    E.FE_MAJOR_LEVEL_ID = BB.FE_MAJOR_LEVEL_ID ) ON
    TSS.TRIP_ID = T.TRIP_ID
WHERE
  C.SPECIES_CODE IN (@sppcode) AND 
  T.TRIP_SUB_TYPE_CODE IN (2,3) AND
  E.FE_SUB_LEVEL_ID IS NULL  -- FISHING_EVENT_ID REPEATED MANY TIMES FOR HOOKS AND TRAPS IF NOT NULL (STUPID IDEA)

-- Add in missing Gouping Codes to main table B01B03
UPDATE #B01B03
SET #B01B03.GC = COALESCE( #B01B03.GC,
   (SELECT #FishEventGroup.GROUPING_CODE 
    FROM #FishEventGroup
    WHERE #FishEventGroup.FISHING_EVENT_ID = #B01B03.FEID) )

SELECT BS.*
FROM #B01B03 BS
ORDER BY
  BS.TID, BS.FEID, BS.CID, BS.SSID, BS.SVID


--getData("gfb_catch_records.sql","GFBioSQL",strSpp="401",as.is=c(rep(F,14),T,rep(F,3)))
--qu("gfb_catch_records.sql",dbName="GFBioSQL",strSpp="607",as.is=c(rep(F,14),T,rep(F,3)))
--qu("gfb_catch_records.sql",dbName="GFBioSQL",strSpp="439",as.is=c(rep(F,16),T,rep(F,3)))
--qu("gfb_catch_records.sql",dbName="GFBioSQL",strSpp="417",as.is=c(rep(FALSE,15),TRUE,rep(FALSE,3)))
--qu("gfb_catch_records.sql",dbName="GFBioSQL",strSpp="440",as.is=c(rep(FALSE,15),TRUE,rep(FALSE,3))) -- YMR (201020, 210125)
--qu("gfb_catch_records.sql",dbName="GFBioSQL",strSpp="437",as.is=c(rep(FALSE,17),TRUE,rep(FALSE,3))) -- CAR (211130, 220506)
--qu("gfb_catch_records.sql",dbName="GFBioSQL",strSpp="396",as.is=c(rep(FALSE,17),TRUE,rep(FALSE,3))) -- POP (210301, 221021, 230201, 230501)
--qu("gfb_catch_records.sql",dbName="GFBioSQL",strSpp="607",as.is=c(rep(FALSE,17),TRUE,rep(FALSE,3))) -- PEL (230925)
--qu("gfb_catch_records.sql",dbName="GFBioSQL",strSpp="435",as.is=c(rep(FALSE,15),TRUE,rep(FALSE,3))) -- BOR (191016, 240105)
--qu("gfb_catch_records.sql",dbName="GFBioSQL",strSpp="418",as.is=c(rep(FALSE,15),TRUE,rep(FALSE,3))) -- YTR (240208, 240315)
--qu("gfb_catch_records.sql",dbName="GFBioSQL",strSpp="394",as.is=c(rep(FALSE,15),TRUE,rep(FALSE,3))) -- RER (200228, 240501)
--qu("gfb_catch_records.sql",dbName="GFBioSQL",strSpp="228",as.is=c(rep(FALSE,15),TRUE,rep(FALSE,3))) -- WAP (241128)
--qu("gfb_catch_records.sql",dbName="GFBioSQL",strSpp="405",as.is=c(rep(FALSE,15),TRUE,rep(FALSE,3))) -- SGR (241119, 250414)

