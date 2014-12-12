-- Query FOS catch from the merged catch table GF_D_OFFICIAL_FE_CATCH
-- Query species catch from GFFOS on SVBCPBSGFIIS
-- Last modified: 2014-12-12
SET NOCOUNT ON 

-- Mean species weight calculated using `gfb_mean_weight.sql', which emulates PJS algorithm for GFBIO data
DECLARE @MEAN_WEIGHT TABLE (SPECIES_CODE VARCHAR(5), MNWT REAL)
INSERT INTO @MEAN_WEIGHT VALUES
  ('222', 1.759289),   -- ttype=c(1,4), gear=1
  ('396', 0.8536203),  -- ttype=c(1,4), gear=1
  ('401', 1.724491),   -- ttype=c(1,4), gear=1, major=3:9
  ('405', 1.916324),   -- ttype=c(1,4), gear=1
  ('418', 1.45),       -- Paul Starr conversion for 2014 YTR assessment
  ('442', 3.529231),   -- ttype=c(1,4), gear=5, major=3:9
  ('602', 1.124977),   -- ttype=c(1,4), gear=1, major=3:9
  ('621', 0.5346079)   -- ttype=c(1,4), gear=1
  --('222', 1.596781),  -- ttype=c(1,2,3,4), gear=1
  --('401', 1.453529),  -- ttype=c(1,2,3,4), gear=1
  --('405', 1.787811),  -- ttype=c(1,2,3,4), gear=1
  --('418', 1.484357),  -- ttype=c(1,2,3,4), gear=1
  --('621', 0.458456)   -- ttype=c(1,2,3,4), gear=1

-- Usage: SELECT FW.MEAN_WEIGHT FROM @MEAN_WEIGHT FW WHERE FW.SPECIES_CODE IN('222')

-- MEAN SPECIES WEIGHT MW
--SELECT
--  MW.spp as \"spp\",
--  Avg(CASE
--    WHEN MW.unit='PND' THEN MW.mnwt/2.20459  -- standardises mean species weight in kg
--    ELSE MW.mnwt END) as \"mnwt\",
--  Sum(MW.n) as \"n\"
--INTO #MEAN_WEIGHT
--FROM
--  (SELECT  -- inline view calculates empirical mean weights for species (in lbs and/or kg)
--    C.SPECIES_CODE AS spp,
--    C.WEIGHT_UNIT_CODE AS unit,
--    Sum(C.CATCH_WEIGHT) AS wt,
--    Sum(C.CATCH_COUNT) AS n,
--    ISNULL(Avg(C.CATCH_WEIGHT/C.CATCH_COUNT),0) AS mnwt
--    FROM GF_FE_CATCH C
--    WHERE
--      (C.CATCH_WEIGHT>0 And C.CATCH_WEIGHT Is Not Null) AND 
--      (C.CATCH_COUNT>1 And C.CATCH_COUNT Is Not Null) AND    -- only calculate means from records with more than one fish
--      C.WEIGHT_UNIT_CODE IN ('PND','KGM')
--    GROUP BY C.SPECIES_CODE, C.WEIGHT_UNIT_CODE) MW
--GROUP BY MW.spp
--ORDER BY MW.spp

-- Gather catch of RRF, POP, ORF, TAR
SELECT -- TOP 20
  OC.TRIP_ID,
  OC.FISHING_EVENT_ID,
  Sum(CASE
    WHEN OC.SPECIES_CODE IN (@sppcode) THEN ISNULL(OC.LANDED_ROUND_KG,0)
    ELSE 0 END) AS landed,
  Sum(CASE
    WHEN OC.SPECIES_CODE IN (@sppcode) THEN
      COALESCE(OC.TOTAL_RELEASED_ROUND_KG,
      (ISNULL(OC.SUBLEGAL_RELEASED_COUNT,0) + ISNULL(OC.LEGAL_RELEASED_COUNT,0)) * ISNULL(FW.MNWT,1), 0)
    ELSE 0 END) AS released,
  Sum(CASE
    WHEN OC.SPECIES_CODE IN (@sppcode) THEN
      (ISNULL(OC.SUBLEGAL_LICED_COUNT,0) + ISNULL(OC.LEGAL_LICED_COUNT,0)) * ISNULL(FW.MNWT,1)
    ELSE 0 END) AS liced,
  Sum(CASE
    WHEN OC.SPECIES_CODE IN (@sppcode) THEN
      (ISNULL(OC.SUBLEGAL_BAIT_COUNT,0) + ISNULL(OC.LEGAL_BAIT_COUNT,0)) * ISNULL(FW.MNWT,1)
    ELSE 0 END) AS bait,
  SUM(CASE
    WHEN OC.SPECIES_CODE IN ('396') THEN ISNULL(OC.LANDED_ROUND_KG,0)
    ELSE 0 END) AS POP,
  SUM(CASE  -- all rockfish other than POP
    WHEN OC.SPECIES_CODE IN (@orfcode) THEN ISNULL(OC.LANDED_ROUND_KG,0)
    ELSE 0 END) AS ORF,
  SUM(CASE  -- target landings reference for discard calculations
    WHEN OC.FISHERY_SECTOR IN ('GROUNDFISH TRAWL') AND OC.SPECIES_CODE IN (@trfcode)
      THEN ISNULL(OC.LANDED_ROUND_KG,0)
    WHEN OC.FISHERY_SECTOR IN ('HALIBUT','HALIBUT AND SABLEFISH') AND OC.SPECIES_CODE IN ('614')
      THEN ISNULL(OC.LANDED_ROUND_KG,0)
    WHEN OC.FISHERY_SECTOR IN ('SABLEFISH') AND OC.SPECIES_CODE IN ('454','455')
      THEN ISNULL(OC.LANDED_ROUND_KG,0)
    WHEN OC.FISHERY_SECTOR IN ('SPINY DOGFISH') AND OC.SPECIES_CODE IN ('042','044')
      THEN ISNULL(OC.LANDED_ROUND_KG,0)
    WHEN OC.FISHERY_SECTOR IN ('LINGCOD') AND OC.SPECIES_CODE IN ('467')
      THEN ISNULL(OC.LANDED_ROUND_KG,0)
    WHEN OC.FISHERY_SECTOR IN ('ROCKFISH INSIDE','ROCKFISH OUTSIDE') AND OC.SPECIES_CODE IN ('424','407','431','433','442')
      THEN ISNULL(OC.LANDED_ROUND_KG,0)
    ELSE 0 END) AS TAR
INTO #CATCH_CORE
FROM 
  GF_D_OFFICIAL_FE_CATCH OC LEFT OUTER JOIN
  @MEAN_WEIGHT FW ON
    OC.SPECIES_CODE = FW.SPECIES_CODE
GROUP BY OC.TRIP_ID, OC.FISHING_EVENT_ID 

-- Combine event information with catch
SELECT
  (CASE
    WHEN FC.FISHERY_SECTOR IN ('GROUNDFISH TRAWL') THEN 1
    WHEN FC.FISHERY_SECTOR IN ('HALIBUT','HALIBUT AND SABLEFISH') THEN 2
    WHEN FC.FISHERY_SECTOR IN ('SABLEFISH') THEN 3
    WHEN FC.FISHERY_SECTOR IN ('LINGCOD','SPINY DOGFISH') THEN 4
    WHEN FC.FISHERY_SECTOR IN ('ROCKFISH INSIDE','ROCKFISH OUTSIDE') THEN 5
    ELSE 0 END) AS \"fid\",
  FC.FISHERY_SECTOR AS \"sector\",
  CASE
    WHEN FC.GEAR IN ('TRAWL') AND FC.GEAR_SUBTYPE NOT IN ('MIDWATER TRAWL') THEN 1
    WHEN FC.GEAR IN ('TRAP') THEN 2
    WHEN FC.GEAR IN ('TRAWL') AND FC.GEAR_SUBTYPE IN ('MIDWATER TRAWL') THEN 3
    WHEN FC.GEAR IN ('HOOK AND LINE') THEN 4
    WHEN FC.GEAR IN ('LONGLINE') THEN 5
    WHEN FC.GEAR IN ('LONGLINE OR HOOK AND LINE','TRAP OR LONGLINE OR HOOK AND LINE') THEN 8
    ELSE 0 END AS \"gear\",
  ISNULL(FC.DATA_SOURCE_CODE,0) AS \"log\",
  --TO_DATE(TO_CHAR(FC.BEST_DATE,'YYYY-MM-DD'),'YYYY-MM-DD') as \"date\",
  CONVERT(VARCHAR(10),FC.BEST_DATE,20) as \"date\",
  ISNULL(FC.MAJOR_STAT_AREA_CODE,'0') AS \"major\",
  ISNULL(FC.MINOR_STAT_AREA_CODE,'0') AS \"minor\",
  ISNULL(FC.LOCALITY_CODE,'0') AS \"locality\",
  CC.landed AS \"landed\",
  CC.released + CC.liced + CC.bait AS \"discard\",
  CC.POP, CC.ORF, CC.TAR
INTO #CATCH_EVENTS
FROM 
  GF_D_OFFICIAL_FE_CATCH FC INNER JOIN
  #CATCH_CORE CC ON
    FC.TRIP_ID = CC.TRIP_ID AND
    FC.FISHING_EVENT_ID = CC.FISHING_EVENT_ID 
WHERE 
  FC.SPECIES_CODE IN (@sppcode) AND 
  (FC.FISHERY_SECTOR IN ('GROUNDFISH TRAWL') OR 
  (FC.FISHERY_SECTOR NOT IN ('GROUNDFISH TRAWL') AND ISNULL(FC.DATA_SOURCE_CODE,0) NOT IN (106,107)))
ORDER BY FC.TRIP_ID, FC.FISHING_EVENT_ID

SELECT * 
FROM #CATCH_EVENTS ORF
WHERE
  ORF.\"landed\">0 OR ORF.\"discard\">0 OR ORF.POP>0 OR ORF.ORF>0 OR ORF.TAR>0

-- getData("fos_vcatORF.sql","GFFOS",strSpp="442")
-- qu("fos_vcatORF.sql",dbName="GFFOS",strSpp="418")



