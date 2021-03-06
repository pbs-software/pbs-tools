-- Revised SQL code by Norm Olsen to grab IPHC survey data from GFBioSQL (2011-09-28)
SET NOCOUNT ON -- prevents timeout errors

SELECT A.year,
   A.BLOCK_DESIGNATION AS station,
   @sppcode AS spp,
   MAX(ISNULL(A.EFFECTIVE_SKATE,0)) AS effsk,
   SUM(ISNULL(B.pcs,0)) AS pcs,
   SUM(ISNULL(B.pcs,0)) / MAX(A.EFFECTIVE_SKATE) AS cpue
FROM (
   SELECT T.TRIP_ID,
      YEAR(TRIP_START_DATE) AS year,
      FE.FISHING_EVENT_ID,
      BLOCK_DESIGNATION,
      EFFECTIVE_SKATE
   FROM SURVEY S
      INNER JOIN TRIP_SURVEY TS ON
      S.SURVEY_ID = TS.SURVEY_ID
      INNER JOIN TRIP T ON
      TS.TRIP_ID = T.TRIP_ID
      INNER JOIN FISHING_EVENT FE ON
      T.TRIP_ID = FE.TRIP_ID
      INNER JOIN IPHC_EFFECTIVE_SKATE ES ON
      FE.FISHING_EVENT_ID = ES.FISHING_EVENT_ID
   WHERE SURVEY_SERIES_ID = 14 AND FE_PARENT_EVENT_ID IS NULL) A
LEFT JOIN (
   SELECT T.TRIP_ID,
      FE.FISHING_EVENT_ID,
      YEAR(TRIP_START_DATE) AS [year], 
      BLOCK_DESIGNATION AS station,
      C.SPECIES_CODE AS spp,
      ISNULL(CATCH_COUNT,0) AS pcs
   FROM SURVEY S
      INNER JOIN TRIP_SURVEY TS ON
      S.SURVEY_ID = TS.SURVEY_ID
      INNER JOIN TRIP T ON
      TS.TRIP_ID = T.TRIP_ID
      INNER JOIN FISHING_EVENT FE ON
      T.TRIP_ID = FE.TRIP_ID
      INNER JOIN FISHING_EVENT_CATCH FEC ON
      FE.FISHING_EVENT_ID = FEC.FISHING_EVENT_ID
      INNER JOIN CATCH C ON 
      FEC.CATCH_ID = C.CATCH_ID
   WHERE SPECIES_CODE = @sppcode AND SURVEY_SERIES_ID = 14 AND FE_SUB_LEVEL_ID IS NULL) B ON
A.TRIP_ID = B.TRIP_ID AND A.FISHING_EVENT_ID = B.FISHING_EVENT_ID
GROUP BY A.year, BLOCK_DESIGNATION


