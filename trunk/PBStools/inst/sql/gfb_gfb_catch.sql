-- Research survey catch for 'requestAges()'. (2010-07-27)
-- Duplicated as 'gfb_catch_records.sql' for 'getCatch()' and 'weightBio'.

SET NOCOUNT ON  -- prevents timeout errors

SELECT
  CASE 
    WHEN S.SURVEY_SERIES_ID=0 THEN 99
    ELSE S.SURVEY_SERIES_ID END AS SURVEY_SERIES_ID,
  S.SURVEY_ID,
  S.ORIGINAL_IND
  INTO #SeriesSurvey
  FROM SURVEY S  -- keys = SVID (survey id) & SSID (survey series id)
  WHERE 
    S.SURVEY_ID IN (@surveyid) AND S.SURVEY_SERIES_ID IN (@survserid) AND S.ORIGINAL_IND IN (@originid)
  ORDER BY
    S.SURVEY_SERIES_ID, S.SURVEY_ID

SELECT
  TS.TRIP_ID,
  SS.*
  INTO #TripSeriesSurvey
  FROM 
    TRIP_SURVEY TS INNER JOIN
    #SeriesSurvey SS ON
      TS.SURVEY_ID = SS.SURVEY_ID

-- One event can have multiple groupings due to restratification schemes
SELECT
  SG.SURVEY_ID,
  FEG.GROUPING_CODE,
  FEG.FISHING_EVENT_ID
  INTO #FEGroups
  FROM 
    SURVEY_GROUPING SG INNER JOIN
    FISHING_EVENT_GROUPING FEG ON 
      SG.GROUPING_CODE = FEG.GROUPING_CODE
  ORDER BY SG.SURVEY_ID, FEG.GROUPING_CODE

SELECT
  T.TRIP_ID AS TID,
  E.FISHING_EVENT_ID AS FEID,
  FE_MAJOR_LEVEL_ID AS [set],
  TSS.SURVEY_SERIES_ID AS SSID,
  TSS.SURVEY_ID AS SVID,
  CASE
    WHEN TSS.ORIGINAL_IND IN ('Y') THEN 'TRUE'
    ELSE 'FALSE' END AS OI,
  FEGC.GROUPING_CODE AS [group],
  CONVERT(smalldatetime,CONVERT(char(10),T.TRIP_START_DATE,20)) AS [date], 
  Year(T.TRIP_START_DATE) AS [year],
  IsNull(E.MAJOR_STAT_AREA_CODE,0) AS major,
  IsNull(E.MINOR_STAT_AREA_CODE,0) AS minor,
  IsNull(E.LOCALITY_CODE,0) AS locality,
  C.CATCH_ID AS CID, 
  C.SPECIES_CODE AS spp, 
  C.CATCH_WEIGHT AS wt, 
  C.CATCH_COUNT AS pcs, 
  COALESCE(C.CATCH_WEIGHT, C.CATCH_COUNT * F.fishwt, 0) AS catKg
  --TEG.GROUPING_DESC AS strata
FROM 
  #FEGroups FEGC INNER JOIN
  (#TripSeriesSurvey TSS INNER JOIN
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
    TSS.TRIP_ID = T.TRIP_ID) ON
    TSS.SURVEY_ID = FEGC.SURVEY_ID AND
    FEGC.FISHING_EVENT_ID = E.FISHING_EVENT_ID
WHERE
  C.SPECIES_CODE IN (@sppcode) AND 
  T.TRIP_SUB_TYPE_CODE IN (2,3)

--getData("gfb_gfb_catch.sql","GFBioSQL",strSpp="396",noLogicals=F)


