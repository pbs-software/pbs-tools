-- Research survey catch for catch reconstructon. (2013-08-20)

SET NOCOUNT ON  -- prevents timeout errors

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
  C.SPECIES_CODE AS spp, 
  C.CATCH_WEIGHT AS wt, 
  C.CATCH_COUNT AS pcs, 
  COALESCE(C.CATCH_WEIGHT, C.CATCH_COUNT * F.fishwt, 0) AS catKg
FROM 
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
    T.TRIP_ID = E.TRIP_ID
WHERE
  C.SPECIES_CODE IN (@sppcode) AND 
  T.TRIP_SUB_TYPE_CODE IN (2,3)

--getData("gfb_rcatORF.sql","GFBioSQL",strSpp="405")


