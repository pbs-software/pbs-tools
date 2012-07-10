--GFBioSQL: summarise the annual survey catch for specified species
SET NOCOUNT ON

SELECT 
  C.SPECIES_CODE,
  AVG(C.CATCH_WEIGHT) AS mnwt, 
  AVG(C.CATCH_COUNT) AS mnct, 
  COUNT(C.CATCH_WEIGHT) AS n,
  AVG(ROUND(C.CATCH_WEIGHT/C.CATCH_COUNT,5)) AS fishwt
INTO #Mean_Fish_Weight
FROM B03_CATCH C
WHERE 
  C.SPECIES_CODE IN (@sppcode) AND 
  C.CATCH_WEIGHT > 0 AND 
  C.CATCH_COUNT > 1
GROUP BY
  C.SPECIES_CODE

SELECT 
  A.ACTIVITY_CODE AS activity, 
  A.ACTIVITY_DESC AS survey,
  Year(T.TRIP_START_DATE) AS [year],
  C.SPECIES_CODE AS spp,
  SUM(CASE
    WHEN C.CATCH_WEIGHT IS NOT NULL THEN C.CATCH_WEIGHT
    WHEN C.CATCH_COUNT IS NOT NULL THEN C.CATCH_COUNT * FW.fishwt
    ELSE 0 END) / 1000. AS catT
FROM
  C_Activity A
  INNER JOIN TRIP_ACTIVITY TA ON 
    A.ACTIVITY_CODE = TA.ACTIVITY_CODE 
  INNER JOIN B01_TRIP T ON
    TA.TRIP_ID = T.TRIP_ID
  INNER JOIN B02_FISHING_EVENT E ON
    T.TRIP_ID = E.TRIP_ID
  INNER JOIN B02L3_Link_Fishing_Event_Catch LEC ON
    E.TRIP_ID = LEC.TRIP_ID AND
    E.FISHING_EVENT_ID = LEC.FISHING_EVENT_ID
  INNER JOIN B03_CATCH C ON
    LEC.CATCH_ID = C.CATCH_ID
  INNER JOIN #Mean_Fish_Weight FW ON
    C.SPECIES_CODE = FW.SPECIES_CODE
WHERE 
  E.FE_SUB_LEVEL_ID Is Null AND 
  C.SPECIES_CODE IN (@sppcode) AND
  (CASE WHEN C.CATCH_WEIGHT IS NOT NULL THEN C.CATCH_WEIGHT
    WHEN C.CATCH_COUNT IS NOT NULL THEN C.CATCH_COUNT * FW.fishwt
    ELSE 0 END) > 0
GROUP BY 
  A.ACTIVITY_CODE, A.ACTIVITY_DESC,
  Year(T.TRIP_START_DATE), C.SPECIES_CODE

-- getData("gfb_survey_activity.sql","GFBioSQL",strSpp="396")
-- restab = crossTab(PBSdat,c("survey","year"),"catT",sum,margins=T)
-- act=c(sapply(split(PBSdat$activity,PBSdat$survey),unique),all=100)
-- dimnames(restab)[[1]]=act[restab$survey]


