-- GFBioSQL query for fish group catches (2011-09-29)
SET NOCOUNT ON  -- prevents timeout errors

SELECT
  FE.TRIP_ID, FE.FISHING_EVENT_ID,
  COALESCE(FE.FE_MODAL_BOTTOM_DEPTH, FE.FE_MODAL_GEAR_DEPTH, FE.FE_MODAL_CAPTURE_DEPTH, FE.FE_MODAL_TARGET_DEPTH, 0) AS Zmod,
  COALESCE(FE.FE_BEGINNING_BOTTOM_DEPTH, FE.FE_BEGINNING_GEAR_DEPTH, FE.FE_BEGIN_CAPTURE_DEPTH, FE.FE_BEGIN_TARGET_DEPTH, 0) AS Zbeg,
  COALESCE(FE.FE_END_BOTTOM_DEPTH, FE.FE_END_GEAR_DEPTH, FE.FE_END_CAPTURE_DEPTH, FE.FE_END_TARGET_DEPTH, 0) AS Zend,
  COALESCE(FE.FE_MIN_BOTTOM_DEPTH, FE.FE_MIN_GEAR_DEPTH, FE.FE_MIN_CAPTURE_DEPTH, FE.FE_MIN_TARGET_DEPTH, 0) AS Zmin,
  COALESCE(FE.FE_MAX_BOTTOM_DEPTH, FE.FE_MAX_GEAR_DEPTH, FE.FE_MAX_CAPTURE_DEPTH, FE.FE_MAX_TARGET_DEPTH, 0) AS Zmax
INTO #DEPTHS
FROM
  B02_FISHING_EVENT FE

SELECT --TOP 1000
  T.TRIP_ID AS TID,
  T.TRIP_SUB_TYPE_CODE AS ttype,
  E.FISHING_EVENT_ID AS FEID,
  CONVERT(smalldatetime,CONVERT(char(10),T.TRIP_START_DATE,20)) AS [date], 
  Year(T.TRIP_START_DATE) AS [year],
  IsNull(E.MAJOR_STAT_AREA_CODE,0) AS major,
  IsNull(E.MINOR_STAT_AREA_CODE,0) AS minor,
  IsNull(E.LOCALITY_CODE,0) AS locality,
  CASE 
    WHEN D.Zmod > 0 THEN D.Zmod
    WHEN D.Zbeg > 0 AND D.Zend > 0 THEN (D.Zbeg + D.Zend) / 2.
    WHEN D.Zbeg > 0 THEN D.Zbeg
    WHEN D.Zend > 0 THEN D.Zend
    WHEN D.Zmin > 0 AND D.Zmax > 0 THEN (D.Zmin + D.Zmax) / 2.
    WHEN D.Zmin > 0 THEN D.Zmin
    WHEN D.Zmax > 0 THEN D.Zmax
    ELSE 0 END AS depth,
  E.GEAR_CODE AS gear,
  C.CATCH_ID AS CID, 
  C.SPECIES_CODE AS spp, 
  C.CATCH_WEIGHT AS wt, 
  C.CATCH_COUNT AS pcs, 
  COALESCE(C.CATCH_WEIGHT, C.CATCH_COUNT * F.fishwt, 0) AS catKg
INTO #SppCatch
FROM 
  #DEPTHS D INNER JOIN
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
    --C.SPECIES_CODE IN (@sppcode) AND 
    C.CATCH_WEIGHT > 0 AND
    C.CATCH_COUNT > 1
  GROUP BY 
    C.SPECIES_CODE ) F ON 
    C.SPECIES_CODE = F.SPECIES_CODE ) ON
    L.CATCH_ID = C.CATCH_ID) ON 
    E.TRIP_ID = L.TRIP_ID AND
    E.FISHING_EVENT_ID = L.FISHING_EVENT_ID) ON
    T.TRIP_ID = E.TRIP_ID ) ON
    E.TRIP_ID = D.TRIP_ID AND
    E.FISHING_EVENT_ID = D.FISHING_EVENT_ID
WHERE
  --C.SPECIES_CODE IN (@sppcode) AND 
  (T.TRIP_SUB_TYPE_CODE IN (2,3) OR T.TRIP_SUB_TYPE_CODE BETWEEN 5 AND 14)

SELECT
  --SC.TID, SC.FEID, SC.year, SC.major, SC.depth, 
  'year'     = IsNull(SC.year,9999),
  'POP'      = Sum(CASE WHEN SC.spp IN ('396')       THEN SC.catKg ELSE 0 END )/1000.,
  'rockfish' = Sum(CASE WHEN SC.spp IN (@orfcode)    THEN SC.catKg ELSE 0 END )/1000.,
  'turbot'   = Sum(CASE WHEN SC.spp IN ('602')       THEN SC.catKg ELSE 0 END )/1000.,
  'flatfish' = Sum(CASE WHEN SC.spp IN (@offcode)    THEN SC.catKg ELSE 0 END )/1000.,
  'hake'     = Sum(CASE WHEN SC.spp IN ('224','225') THEN SC.catKg ELSE 0 END )/1000.,
  'sharks'   = Sum(CASE WHEN SC.spp IN (@ssscode)    THEN SC.catKg ELSE 0 END )/1000.,
  'other'    = Sum(CASE WHEN SC.spp IN (@alfcode) AND 
    SC.spp NOT IN ('396',@orfcode,'602',@offcode,'224','225',@ssscode) THEN SC.catKg ELSE 0 END )/1000.
--INTO #GroupCatch
FROM 
  #SppCatch SC
WHERE
  COALESCE(CONVERT(INT,SC.major), 0) IN (@major) AND 
  COALESCE(SC.depth, 0) BETWEEN @mindep AND @maxdep AND
  COALESCE(SC.gear,  1) IN (@dummy)
GROUP BY 
  IsNull(SC.year,9999)
ORDER BY 
  IsNull(SC.year,9999)

--select * from #SppCatch


-- Note: @dummy refers to GFBioSQL gear_code: 1=bottom trawl, 6=midwater trawl
--getData("gfb_bycatch.sql","GFBioSQL",strSpp="396",major=5:7,mindep=76,maxdep=444,dummy=6)


