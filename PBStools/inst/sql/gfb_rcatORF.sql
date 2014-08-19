-- Research survey catch for catch reconstructon. (last modified 2014-07-08)

SET NOCOUNT ON  -- prevents timeout errors

-- =====ASSOCIATE TRIPS WITH SURVEYS=====
-- Merge tables to get TRIP_ID, SURVEY_ID, and SURVEY_SERIES_ID
SELECT
  TS.TRIP_ID, 
  TS.SURVEY_ID, 
  IsNull(S.SURVEY_SERIES_ID,0) AS SURVEY_SERIES_ID,
  CASE 
    WHEN S.ORIGINAL_IND='Y' THEN 1
    WHEN S.ORIGINAL_IND='N' THEN 2
    ELSE 3 END AS ORIGINAL_IND
INTO #TempTripSurv
FROM 
  SURVEY S RIGHT OUTER JOIN
  TRIP_SURVEY TS ON
  S.SURVEY_ID = TS.SURVEY_ID 

-- TRIP_ID can be associated with more than one SURVEY_ID, which can include the original index or not.
-- Choose one SURVEY_ID per TRIP_ID, preferably the orginal index.
-- When the orginal is not avialable, choose a non-original index.
SELECT
  TS.TRIP_ID, 
  TS.SURVEY_ID, 
  TS.SURVEY_SERIES_ID,
  TS.ORIGINAL_IND
INTO #TripSurvSer
FROM
  (SELECT *,
    ROW_NUMBER() OVER (PARTITION BY TTS.TRIP_ID ORDER BY TTS.ORIGINAL_IND, TTS.SURVEY_ID) AS RN
  FROM #TempTripSurv TTS) AS TS
WHERE TS.RN = 1
ORDER BY TS.TRIP_ID

-- =====COLLECT CATCH=====
SELECT
  T.TRIP_ID AS TID,
  E.FISHING_EVENT_ID AS FEID,
  FE_MAJOR_LEVEL_ID AS [set],
  CONVERT(smalldatetime,CONVERT(char(10),T.TRIP_START_DATE,20)) AS [date], 
  Year(T.TRIP_START_DATE) AS [year],
  IsNull(E.MAJOR_STAT_AREA_CODE,0) AS major,
  IsNull(E.MINOR_STAT_AREA_CODE,0) AS minor,
  IsNull(E.LOCALITY_CODE,0) AS locality,
  C.CATCH_ID AS CID, 
  TSS.SURVEY_ID AS SVID,
  TSS.SURVEY_SERIES_ID AS SSID,
  C.SPECIES_CODE AS spp, 
  C.CATCH_WEIGHT AS wt, 
  C.CATCH_COUNT AS pcs, 
  COALESCE(C.CATCH_WEIGHT, C.CATCH_COUNT * F.fishwt, 0) AS catKg
FROM 
  #TripSurvSer TSS RIGHT OUTER JOIN
  B01_TRIP T INNER JOIN 
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
    T.TRIP_ID = E.TRIP_ID ON
    TSS.TRIP_ID = T.TRIP_ID
WHERE
  C.SPECIES_CODE IN (@sppcode) AND 
  T.TRIP_SUB_TYPE_CODE IN (2,3) AND 
  E.FE_SUB_LEVEL_ID IS NULL  -- FISHING_EVENT_ID REPEATED MANY TIMES FOR HOOKS AND TRAPS IF NOT NULL (STUPID IDEA)

--getData("gfb_rcatORF.sql","GFBioSQL",strSpp="442")


