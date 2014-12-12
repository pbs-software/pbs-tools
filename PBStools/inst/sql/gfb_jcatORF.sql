-- Query GFB catch for joint-venture hake fishery catch up to 
-- and including 2006 only (GFFOS captures this catch thereafter).
-- Last modified: 2014-11-03
SET NOCOUNT ON 

-- Kate Rutherford's query (modified) for bycatch from the joint-venture hake fishery
SELECT -- TOP 10
  '1' AS 'fid',
  CASE
    WHEN FE.GEAR_CODE IN (1,8,11,16)      THEN 1 -- bottom trawl (1=bottom trawl, 8=unknown trawl, 11=shrimp trawl, 16=twin beam trawl)
    WHEN FE.GEAR_CODE IN (2,13)           THEN 2 -- trap (2=trap, 13=prawn trap)
    WHEN FE.GEAR_CODE IN (3,6,9,14,17,18) THEN 3 -- midwater trawl (3=gillnet, 6=midwater trawl, 9=seine, 14=tucker trawl, 17=hand seine, 18=purse seine)
    WHEN FE.GEAR_CODE IN (4,7,10,12)      THEN 4 -- hook and line (4=handline, 7=troll, 10=jig, 12=rod & reel)
    WHEN FE.GEAR_CODE IN (5)              THEN 5 -- longline
    ELSE 0 END AS 'gear',
  T.TRIP_START_DATE AS 'date', 
  T.TRIP_SUB_TYPE_CODE AS 'ttype', 
  FE.MAJOR_STAT_AREA_CODE AS 'major',
  FE.MINOR_STAT_AREA_CODE AS 'minor',
  FE.LOCALITY_CODE AS 'locality',
  --C.SPECIES_CATEGORY_CODE AS 'scat', -- 4=discarded
  landed = Sum(CASE
    WHEN C.SPECIES_CODE IN (@sppcode) AND C.SPECIES_CATEGORY_CODE NOT IN (4) THEN ISNULL(C.CATCH_WEIGHT,0)
    ELSE 0 END ),
  discard = Sum(CASE
    WHEN SPECIES_CODE IN (@sppcode) AND C.SPECIES_CATEGORY_CODE IN (4) THEN ISNULL(C.CATCH_WEIGHT,0)
    ELSE 0 END ),
  POP = Sum(CASE
    WHEN SPECIES_CODE IN ('396') THEN ISNULL(C.CATCH_WEIGHT,0)
    ELSE 0 END ),
  ORF = Sum(CASE   -- landings of all rockfish other than POP
    WHEN SPECIES_CODE IN (@orfcode) THEN  ISNULL(C.CATCH_WEIGHT,0)
    ELSE 0 END ),
  TAR = Sum(CASE   -- target landings reference for discard calculations
    WHEN SPECIES_CODE IN (@trfcode) THEN  ISNULL(C.CATCH_WEIGHT,0)
  ELSE 0 END)
INTO #JVHAKE
FROM
  ((((TRIP T INNER JOIN FISHING_EVENT FE ON 
    T.TRIP_ID = FE.TRIP_ID) INNER JOIN
  FISHING_EVENT_CATCH FEC ON 
    FE.FISHING_EVENT_ID = FEC.FISHING_EVENT_ID) INNER JOIN 
 CATCH C ON 
    FEC.CATCH_ID = C.CATCH_ID) INNER JOIN 
  GEAR G ON 
    FE.GEAR_CODE = G.GEAR_CODE) INNER JOIN 
  MAJOR_STAT_AREA MSA ON 
    FE.MAJOR_STAT_AREA_CODE = MSA.MAJOR_STAT_AREA_CODE
WHERE
  T.TRIP_SUB_TYPE_CODE IN (5,7,8,9,10,12) -- 5=Canada, 7=Polish, 8-9=Russian, 10=Polish, 12=Japanese
  AND Year(T.TRIP_START_DATE) <= 2006
--  AND C.SPECIES_CODE IN (@sppcode)
GROUP BY 
  CASE
    WHEN FE.GEAR_CODE IN (1,8,11,16)      THEN 1 -- bottom trawl (1=bottom trawl, 8=unknown trawl, 11=shrimp trawl, 16=twin beam trawl)
    WHEN FE.GEAR_CODE IN (2,13)           THEN 2 -- trap (2=trap, 13=prawn trap)
    WHEN FE.GEAR_CODE IN (3,6,9,14,17,18) THEN 3 -- midwater trawl (3=gillnet, 6=midwater trawl, 9=seine, 14=tucker trawl, 17=hand seine, 18=purse seine)
    WHEN FE.GEAR_CODE IN (4,7,10,12)      THEN 4 -- hook and line (4=handline, 7=troll, 10=jig, 12=rod & reel)
    WHEN FE.GEAR_CODE IN (5)              THEN 5 -- longline
    ELSE 0 END,
  T.TRIP_START_DATE, 
  T.TRIP_SUB_TYPE_CODE,
  FE.MAJOR_STAT_AREA_CODE,
  FE.MINOR_STAT_AREA_CODE,
  FE.LOCALITY_CODE
--  C.SPECIES_CATEGORY_CODE

SELECT * 
FROM #JVHAKE JV
WHERE
  JV.landed>0 OR JV.discard>0 OR JV.POP>0 OR JV.ORF>0 OR JV.TAR>0


-- getData("gfb_jcatORF.sql","GFBioSQL",strSpp="418")
-- qu("gfb_jcatORF.sql",dbName="GFBioSQL",strSpp="418")



