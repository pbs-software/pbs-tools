-- SQL code to get IPHC longline skate data (for AME's IPHC index calculation) from GFBioSQL
-- Last modified: 2015-03-04

SET NOCOUNT ON -- prevents timeout errors

SELECT
  YEAR(T.TRIP_START_DATE) AS [year],
  FE.TRIP_ID,
  FE.FISHING_EVENT_ID,
  FE.FE_PARENT_EVENT_ID,
  FE.BLOCK_DESIGNATION,
  FE.FE_MAJOR_LEVEL_ID,
  FE.FE_SUB_LEVEL_ID,
  FE.FE_MINOR_LEVEL_ID,
  C.SPECIES_CODE,
  C.CATCH_WEIGHT,
  C.CATCH_COUNT,
  S.SURVEY_SERIES_ID,
  LS.LGLSP_HOOK_COUNT,
  LS.LGLSP_HOOKS_SET_COUNT,
  LS.SKATE_COUNT,
  ES.EFFECTIVE_SKATE,
  C.SPECIES_CATEGORY_CODE
FROM
  (((TRIP_SURVEY TS INNER JOIN 
  SURVEY S ON 
    TS.SURVEY_ID = S.SURVEY_ID) INNER JOIN 
  (((B02_FISHING_EVENT FE INNER JOIN 
  B02L3_Link_Fishing_Event_Catch L23 ON 
    FE.FISHING_EVENT_ID = L23.FISHING_EVENT_ID) INNER JOIN
  B03_CATCH C ON 
    L23.CATCH_ID = C.CATCH_ID) INNER JOIN 
  B01_TRIP T ON 
    FE.TRIP_ID = T.TRIP_ID) ON 
    TS.TRIP_ID = T.TRIP_ID) LEFT OUTER JOIN 
  LONGLINE_SPECS LS ON
    FE.FISHING_EVENT_ID = LS.FISHING_EVENT_ID) LEFT OUTER JOIN 
  IPHC_EFFECTIVE_SKATE ES ON 
    FE.FISHING_EVENT_ID = ES.FISHING_EVENT_ID
WHERE
  FE.FE_MINOR_LEVEL_ID Is Null AND
  S.SURVEY_SERIES_ID IN (14)
ORDER BY
  YEAR(T.TRIP_START_DATE),
  FE.TRIP_ID,
  FE.FISHING_EVENT_ID,
  FE.FE_MAJOR_LEVEL_ID,
  C.SPECIES_CODE

--qu("gfb_iphc_skate_data.sql",dbName="GFBioSQL",strSpp="442")
--getData("gfb_iphc_skate_data.sql",dbName="GFBioSQL",strSpp="442",path="C:/Users/haighr/Files/Projects/R/Develop/PBStools/Authors/SQLcode/")

