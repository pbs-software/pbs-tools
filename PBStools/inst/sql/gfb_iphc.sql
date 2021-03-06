-- SQL code (originally bt by NO) to grab IPHC survey data from GFBioSQL
-- Code revised by RH for BA to include other factors like 'depth' (2012-07-02)

SET NOCOUNT ON -- prevents timeout errors

SELECT 
  T.TRIP_ID,
  FE.FISHING_EVENT_ID,
  YEAR(T.TRIP_START_DATE) AS tyear, 
  COALESCE(FE.FE_MODAL_BOTTOM_DEPTH,FE.FE_MIN_BOTTOM_DEPTH,FE.FE_MAX_BOTTOM_DEPTH,0) AS depth,
  FE.BLOCK_DESIGNATION,
  C.SPECIES_CODE,
  SUM(ISNULL(C.CATCH_COUNT,0)) AS pcs
INTO #SCAT
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
WHERE SPECIES_CODE = @sppcode AND SURVEY_SERIES_ID = 14 AND FE_SUB_LEVEL_ID IS NULL
GROUP BY
  T.TRIP_ID,
  FE.FISHING_EVENT_ID,
  YEAR(T.TRIP_START_DATE), 
  COALESCE(FE.FE_MODAL_BOTTOM_DEPTH,FE.FE_MIN_BOTTOM_DEPTH,FE.FE_MAX_BOTTOM_DEPTH,0),
  FE.BLOCK_DESIGNATION,
  C.SPECIES_CODE

SELECT
  T.TRIP_ID,
  FE.FISHING_EVENT_ID,
  YEAR(T.TRIP_START_DATE) AS tyear,
  COALESCE(FE.FE_MODAL_BOTTOM_DEPTH,FE.FE_MIN_BOTTOM_DEPTH,FE.FE_MAX_BOTTOM_DEPTH,0) AS depth,
  FE.BLOCK_DESIGNATION,
  ES.EFFECTIVE_SKATE
INTO #EFF
FROM SURVEY S
  INNER JOIN TRIP_SURVEY TS ON
    S.SURVEY_ID = TS.SURVEY_ID
  INNER JOIN TRIP T ON
    TS.TRIP_ID = T.TRIP_ID
  INNER JOIN FISHING_EVENT FE ON
    T.TRIP_ID = FE.TRIP_ID
  INNER JOIN IPHC_EFFECTIVE_SKATE ES ON
    FE.FISHING_EVENT_ID = ES.FISHING_EVENT_ID
WHERE SURVEY_SERIES_ID = 14 AND FE_PARENT_EVENT_ID IS NULL

SELECT
   A.tyear AS [year],
   A.BLOCK_DESIGNATION AS station,
   @sppcode AS spp,
   A.depth,
   ISNULL(A.EFFECTIVE_SKATE,0) AS effsk,
   isnull(B.pcs,0) AS pcs,
   CASE
     WHEN ISNULL(A.EFFECTIVE_SKATE,0) IN (0) THEN NULL
     WHEN isnull(B.pcs,0) IN (0) THEN 0
   ELSE B.pcs/A.EFFECTIVE_SKATE END  AS cpue
FROM
  #EFF A LEFT JOIN
  #SCAT B ON
    A.TRIP_ID = B.TRIP_ID AND
    A.FISHING_EVENT_ID = B.FISHING_EVENT_ID AND
    A.tyear = B.tyear AND
    A.depth = B.depth AND
    A.BLOCK_DESIGNATION = B.BLOCK_DESIGNATION


 --getData("gfb_iphc2.sql","GFBioSQL",strSpp="467")




