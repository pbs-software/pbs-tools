-- SQL code to get IPHC skate records (not used for IPHC index calculation) from GFBioSQL
-- Last modified: 2015-03-03

SET NOCOUNT ON -- prevents timeout errors

SELECT
  YEAR(T.TRIP_START_DATE) AS [year],
  FE.TRIP_ID,
  FE.FISHING_EVENT_ID,
  FE.FE_PARENT_EVENT_ID,
  FE.FE_MAJOR_LEVEL_ID,
  FE.FE_SUB_LEVEL_ID,
  FE.FE_MINOR_LEVEL_ID
FROM
  (TRIP_SURVEY TS INNER JOIN 
  SURVEY S ON 
    TS.SURVEY_ID = S.SURVEY_ID) INNER JOIN 
  (B02_FISHING_EVENT FE INNER JOIN 
  B01_TRIP T ON 
    FE.TRIP_ID = T.TRIP_ID) ON 
    TS.TRIP_ID = T.TRIP_ID
WHERE
  FE.FE_MINOR_LEVEL_ID Is Not Null AND
  S.SURVEY_SERIES_ID IN (14)
ORDER BY
  Year(T.TRIP_START_DATE),
  FE.TRIP_ID,
  FE.FISHING_EVENT_ID

--qu("gfb_iphc_skates.sql",dbName="GFBioSQL",strSpp="442")
--getData("gfb_iphc_skates.sql",dbName="GFBioSQL",strSpp="442",path="C:/Users/haighr/Files/Projects/R/Develop/PBStools/Authors/SQLcode/")

