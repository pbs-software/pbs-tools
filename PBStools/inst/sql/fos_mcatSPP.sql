-- Query catch gathered from multiple DBs and housed in GF_MERGED_CATCH
-- Query species catch from GFFOS on SVBCPBSGFIIS
-- Modified from 'fos_mcatORF' to replace catch summaries used by function 'weightBio'.
-- Last modified: 2019-08-07 (RH)
SET NOCOUNT ON 

-- Mean species weight calculated using `gfb_mean_weight.sql', which emulates PJS algorithm for GFBIO data
@INSERT('meanSppWt.sql')  -- getData now inserts the specified SQL file assuming it's on the `path' specified in `getData'
-- Usage: SELECT FW.MEAN_WEIGHT FROM @MEAN_WEIGHT FW WHERE FW.SPECIES_CODE IN('222')

-- Gather catch of SPP, FISHCAT
SELECT --TOP 200
  MC.DATABASE_NAME,
  MC.TRIP_ID,
  MC.FISHING_EVENT_ID,
  --ISNULL(L.LOCALITY_DESCRIPTION,'MOOSE COUNTY') AS LOCAL_NAME,
  Sum(CASE
    WHEN MC.SPECIES_CODE IN (@sppcode) THEN COALESCE(NULLIF(MC.LANDED_KG,0), NULLIF(MC.LANDED_PCS*ISNULL(FW.MNWT,1),0), 0)
    ELSE 0 END) AS SPPLAND,
  Sum(CASE
    WHEN MC.SPECIES_CODE IN (@sppcode) THEN COALESCE(NULLIF(MC.DISCARDED_KG,0), NULLIF(MC.DISCARDED_PCS*ISNULL(FW.MNWT,1),0), 0)
    ELSE 0 END) AS SPPDISC,
  SUM(COALESCE(NULLIF(MC.LANDED_KG,0), NULLIF(MC.LANDED_PCS*ISNULL(FW.MNWT,1),0), 0) + 
    COALESCE(NULLIF(MC.DISCARDED_KG,0), NULLIF(MC.DISCARDED_PCS*ISNULL(FW.MNWT,1),0), 0)) AS FISHCAT  -- 'meanSppWt.sql' now has FISH field
INTO #CATCH_CORE
FROM 
  GF_MERGED_CATCH MC INNER JOIN
  @MEAN_WEIGHT FW ON
    MC.SPECIES_CODE = FW.SPECIES_CODE
WHERE FW.FISH IN (1)
GROUP BY MC.DATABASE_NAME, MC.TRIP_ID, MC.FISHING_EVENT_ID


-- Collect area information (which can differ within a fishing event depending on the species)
-- This madness significantly increases the running time of the query!!!
SELECT --top 100
  MC.DATABASE_NAME, MC.TRIP_ID, MC.FISHING_EVENT_ID,
  ISNULL(MC.MAJOR_STAT_AREA_CODE,'0') AS MAJOR,
  ISNULL(MC.MINOR_STAT_AREA_CODE,'0') AS MINOR,
  ISNULL(MC.LOCALITY_CODE,'0') AS LOCALITY,
  COUNT(ISNULL(MC.MAJOR_STAT_AREA_CODE,'0')) OVER (PARTITION BY MC.DATABASE_NAME, MC.TRIP_ID, MC.FISHING_EVENT_ID, ISNULL(MC.MAJOR_STAT_AREA_CODE,'0')) AS COUNT_MAJOR,
  COUNT(ISNULL(MC.MINOR_STAT_AREA_CODE,'0')) OVER (PARTITION BY MC.DATABASE_NAME, MC.TRIP_ID, MC.FISHING_EVENT_ID, ISNULL(MC.MINOR_STAT_AREA_CODE,'0')) AS COUNT_MINOR,
  COUNT(ISNULL(MC.LOCALITY_CODE,'0')) OVER (PARTITION BY MC.DATABASE_NAME, MC.TRIP_ID, MC.FISHING_EVENT_ID, ISNULL(MC.LOCALITY_CODE,'0')) AS COUNT_LOCALITY
INTO #COUNT_DRACULA
FROM 
  GF_MERGED_CATCH MC
--WHERE MC.TRIP_ID IN ('20905002','20905004','20905009','20605028','100053')
ORDER BY MC.DATABASE_NAME, MC.TRIP_ID, MC.FISHING_EVENT_ID

-- Collect a unique major area for the fishing event (most frequently used, other than 0)
SELECT CD.DATABASE_NAME, CD.TRIP_ID, CD.FISHING_EVENT_ID, CD.MAJOR, MAX(CD.COUNT_MAJOR) AS N_MAJOR
INTO #TOP_MAJOR
FROM #COUNT_DRACULA CD
GROUP BY CD.DATABASE_NAME, CD.TRIP_ID, CD.FISHING_EVENT_ID, CD.MAJOR
ORDER BY CD.DATABASE_NAME, CD.TRIP_ID, CD.FISHING_EVENT_ID, CD.MAJOR DESC, N_MAJOR ASC

SELECT T1.DATABASE_NAME, T1.TRIP_ID, T1.FISHING_EVENT_ID, T1.MAJOR
INTO #UNIQUE_MAJOR
FROM (SELECT ROW_NUMBER() OVER(PARTITION BY TM.DATABASE_NAME, TM.TRIP_ID, TM.FISHING_EVENT_ID
  ORDER BY TM.DATABASE_NAME, TM.TRIP_ID, TM.FISHING_EVENT_ID) AS RN, * FROM #TOP_MAJOR TM) T1 
WHERE T1.RN<=1

-- Collect a unique minor area for the fishing event (most frequently used, other than 0)
SELECT CD.DATABASE_NAME, CD.TRIP_ID, CD.FISHING_EVENT_ID, CD.MINOR, MAX(CD.COUNT_MINOR) AS N_MINOR
INTO #TOP_MINOR
FROM #COUNT_DRACULA CD
GROUP BY CD.DATABASE_NAME, CD.TRIP_ID, CD.FISHING_EVENT_ID, CD.MINOR
ORDER BY CD.DATABASE_NAME, CD.TRIP_ID, CD.FISHING_EVENT_ID, CD.MINOR DESC, N_MINOR ASC

SELECT T2.DATABASE_NAME, T2.TRIP_ID, T2.FISHING_EVENT_ID, T2.MINOR
INTO #UNIQUE_MINOR
FROM (SELECT ROW_NUMBER() OVER(PARTITION BY TM.DATABASE_NAME, TM.TRIP_ID, TM.FISHING_EVENT_ID
  ORDER BY TM.DATABASE_NAME, TM.TRIP_ID, TM.FISHING_EVENT_ID) AS RN, * FROM #TOP_MINOR TM) T2 
WHERE T2.RN<=1

-- Collect a unique localities for the fishing event (most frequently used, other than 0)
SELECT CD.DATABASE_NAME, CD.TRIP_ID, CD.FISHING_EVENT_ID, CD.LOCALITY, MAX(CD.COUNT_LOCALITY) AS N_LOCALITY
INTO #TOP_LOCALITY
FROM #COUNT_DRACULA CD
GROUP BY CD.DATABASE_NAME, CD.TRIP_ID, CD.FISHING_EVENT_ID, CD.LOCALITY
ORDER BY CD.DATABASE_NAME, CD.TRIP_ID, CD.FISHING_EVENT_ID, CD.LOCALITY DESC, N_LOCALITY ASC

SELECT T3.DATABASE_NAME, T3.TRIP_ID, T3.FISHING_EVENT_ID, T3.LOCALITY
INTO #UNIQUE_LOCALITY
FROM (SELECT ROW_NUMBER() OVER(PARTITION BY TM.DATABASE_NAME, TM.TRIP_ID, TM.FISHING_EVENT_ID
  ORDER BY TM.DATABASE_NAME, TM.TRIP_ID, TM.FISHING_EVENT_ID) AS RN, * FROM #TOP_LOCALITY TM) T3 
WHERE T3.RN<=1

-- Tie the unique major, minor, and locality areas together
SELECT
  UMAJ.DATABASE_NAME, UMAJ.TRIP_ID, UMAJ.FISHING_EVENT_ID,
  UMAJ.MAJOR AS MAJOR_STAT_AREA_CODE,
  UMIN.MINOR AS MINOR_STAT_AREA_CODE,
  ULOC.LOCALITY AS LOCALITY_CODE
INTO #AREAS
FROM 
  (#UNIQUE_MAJOR UMAJ INNER JOIN
  #UNIQUE_MINOR UMIN ON
    UMAJ.DATABASE_NAME = UMIN.DATABASE_NAME AND
    UMAJ.TRIP_ID = UMIN.TRIP_ID AND
    UMAJ.FISHING_EVENT_ID = UMIN.FISHING_EVENT_ID) INNER JOIN
  #UNIQUE_LOCALITY ULOC ON
    UMAJ.DATABASE_NAME = ULOC.DATABASE_NAME AND
    UMAJ.TRIP_ID = ULOC.TRIP_ID AND
    UMAJ.FISHING_EVENT_ID = ULOC.FISHING_EVENT_ID

-- Collect event information
SELECT --top 100
  MC.DATABASE_NAME,
  MC.TRIP_ID,
  MC.FISHING_EVENT_ID,
  (CASE
    WHEN MC.FISHERY_SECTOR IN ('GROUNDFISH TRAWL','JOINT VENTURE TRAWL') THEN 1
    WHEN MC.FISHERY_SECTOR IN ('HALIBUT','HALIBUT AND SABLEFISH','K/L') THEN 2
    WHEN MC.FISHERY_SECTOR IN ('SABLEFISH') THEN 3
    WHEN MC.FISHERY_SECTOR IN ('LINGCOD','SPINY DOGFISH','SCHEDULE II') THEN 4
    WHEN MC.FISHERY_SECTOR IN ('ROCKFISH INSIDE','ROCKFISH OUTSIDE','ZN','K/ZN') THEN 5
    WHEN MC.FISHERY_SECTOR IN ('FOREIGN') THEN 9
    ELSE 0 END) AS \"fid\",
  MC.FISHERY_SECTOR AS \"sector\",
  CASE
    WHEN MC.GEAR IN ('BOTTOM TRAWL','UNKNOWN TRAWL') THEN 1
    WHEN MC.GEAR IN ('TRAP') THEN 2
    WHEN MC.GEAR IN ('MIDWATER TRAWL') THEN 3
    WHEN MC.GEAR IN ('HOOK AND LINE') THEN 4
    WHEN MC.GEAR IN ('LONGLINE') THEN 5
    WHEN MC.GEAR IN ('LONGLINE OR HOOK AND LINE','TRAP OR LONGLINE OR HOOK AND LINE') THEN 8
    ELSE 0 END AS \"gear\",
  CASE
    WHEN ISNULL(MC.VESSEL_NAME,'UNKNOWN') IN ('VIKING ENTERPRISE','OSPREY NO 1','RAW SPIRIT','NORTHERN ALLIANCE','VIKING ALLIANCE') THEN 1
    ELSE 0 END AS \"ftype\", -- freezer vessel
  CASE
    WHEN MC.FISHING_EVENT_ID IN ('0') THEN 3   -- unidentified fishing events should all be DMP
    WHEN MC.LOG_TYPE IN ('OBSERVER LOG') THEN 1
    WHEN MC.LOG_TYPE IN ('FISHER LOG') THEN 2
    WHEN MC.LOG_TYPE IN ('DMP') THEN 3
    WHEN MC.LOG_TYPE IN ('UNKNOWN') THEN 0
    ELSE 0 END AS \"log\",
  --CONVERT(VARCHAR(10),COALESCE(MC.FE_START_DATE, MC.FE_END_DATE, MC.TRIP_START_DATE, MC.TRIP_END_DATE),20) as \"date\",
  CONVERT(VARCHAR(10),MC.BEST_DATE,20) as \"date\",
  CASE WHEN MC.BEST_DATE IS NULL THEN '9999' ELSE CAST(YEAR(MC.BEST_DATE) AS CHAR(4)) END AS \"year\",
  ISNULL(A.MAJOR_STAT_AREA_CODE,'0') AS \"major\",
  ISNULL(A.MINOR_STAT_AREA_CODE,'0') AS \"minor\",
  ISNULL(A.LOCALITY_CODE,'0') AS \"locality\",
  --CC.LOCAL_NAME, -- for debugging locality names
  ISNULL(MC.LONGITUDE,'0') AS \"X\",
  ISNULL(MC.LATITUDE,'0') AS \"Y\",
  ISNULL(MC.BEST_DEPTH,0) AS \"fdep\",
  ISNULL(DATEDIFF(n,MC.FE_START_DATE,MC.FE_END_DATE),0) AS \"eff\"
INTO #EVENT_CORE
FROM 
  GF_MERGED_CATCH MC INNER JOIN 
  #AREAS A ON
    MC.DATABASE_NAME = A.DATABASE_NAME AND
    MC.TRIP_ID = A.TRIP_ID AND
    MC.FISHING_EVENT_ID = A.FISHING_EVENT_ID
--WHERE
  -- Langara Spit (Norm's choice based on the word "Langara" appearing in locality name)
  --(( MC.MAJOR_STAT_AREA_CODE IN (8) AND MC.MINOR_STAT_AREA_CODE IN (3) AND MC.LOCALITY_CODE IN (3) ) OR
  --( MC.MAJOR_STAT_AREA_CODE IN (9) AND MC.MINOR_STAT_AREA_CODE IN (35) AND MC.LOCALITY_CODE IN (1,2,4,5,6,7) ))
GROUP BY
  MC.DATABASE_NAME,
  MC.TRIP_ID,
  MC.FISHING_EVENT_ID,
  (CASE
    WHEN MC.FISHERY_SECTOR IN ('GROUNDFISH TRAWL','JOINT VENTURE TRAWL') THEN 1
    WHEN MC.FISHERY_SECTOR IN ('HALIBUT','HALIBUT AND SABLEFISH','K/L') THEN 2
    WHEN MC.FISHERY_SECTOR IN ('SABLEFISH') THEN 3
    WHEN MC.FISHERY_SECTOR IN ('LINGCOD','SPINY DOGFISH','SCHEDULE II') THEN 4
    WHEN MC.FISHERY_SECTOR IN ('ROCKFISH INSIDE','ROCKFISH OUTSIDE','ZN','K/ZN') THEN 5
    WHEN MC.FISHERY_SECTOR IN ('FOREIGN') THEN 9
    ELSE 0 END),
  MC.FISHERY_SECTOR,
  CASE
    WHEN MC.GEAR IN ('BOTTOM TRAWL','UNKNOWN TRAWL') THEN 1
    WHEN MC.GEAR IN ('TRAP') THEN 2
    WHEN MC.GEAR IN ('MIDWATER TRAWL') THEN 3
    WHEN MC.GEAR IN ('HOOK AND LINE') THEN 4
    WHEN MC.GEAR IN ('LONGLINE') THEN 5
    WHEN MC.GEAR IN ('LONGLINE OR HOOK AND LINE','TRAP OR LONGLINE OR HOOK AND LINE') THEN 8
    ELSE 0 END,
  CASE
    WHEN ISNULL(MC.VESSEL_NAME,'UNKNOWN') IN ('VIKING ENTERPRISE','OSPREY NO 1','RAW SPIRIT','NORTHERN ALLIANCE','VIKING ALLIANCE') THEN 1
    ELSE 0 END,
  CASE
    WHEN MC.FISHING_EVENT_ID IN ('0') THEN 3   -- unidentified fishing events should all be DMP
    WHEN MC.LOG_TYPE IN ('OBSERVER LOG') THEN 1
    WHEN MC.LOG_TYPE IN ('FISHER LOG') THEN 2
    WHEN MC.LOG_TYPE IN ('DMP') THEN 3
    WHEN MC.LOG_TYPE IN ('UNKNOWN') THEN 0
    ELSE 0 END,
  CONVERT(VARCHAR(10),MC.BEST_DATE,20),
  CASE WHEN MC.BEST_DATE IS NULL THEN '9999' ELSE CAST(YEAR(MC.BEST_DATE) AS CHAR(4)) END,
  ISNULL(A.MAJOR_STAT_AREA_CODE,'0'),
  ISNULL(A.MINOR_STAT_AREA_CODE,'0'),
  ISNULL(A.LOCALITY_CODE,'0'),
  --CC.LOCAL_NAME, -- for debugging locality names
  ISNULL(MC.LONGITUDE,'0'),
  ISNULL(MC.LATITUDE,'0'),
  ISNULL(MC.BEST_DEPTH,0),
  ISNULL(DATEDIFF(n,MC.FE_START_DATE,MC.FE_END_DATE),0)


-- Combine event information with catch
SELECT
  EC.DATABASE_NAME AS db,
  EC.fid AS FID,
  EC.TRIP_ID AS TID,
  EC.FISHING_EVENT_ID AS FEID,
  -- EC.hail, EC.set, -- not available in GF_MERGED_CATCH
  EC.log,
  EC.X, EC.Y,
  EC.date,
  EC.year,
  EC.major,
  EC.minor,
  EC.locality,
  --EC.pfma, EC.pfma, -- available but not used for anything
  EC.fdep AS depth,
  -- EC.vessel,       -- available but not used for anything
  EC.ftype,           -- freezer vessel or not
  EC.gear,
  -- EC.success,      -- not available in GF_MERGED_CATCH
  EC.eff AS effort,
  CC.SPPLAND + CC.SPPDISC AS catKg,
  CC.FISHCAT AS fishKg,
  CASE
    WHEN CC.SPPDISC = 0 THEN 0
    ELSE CC.SPPDISC/(CC.SPPLAND+CC.SPPDISC)
    END AS \"pdis\",  -- proportion discarded
  CASE
    WHEN CC.FISHCAT = 0 THEN 0
    ELSE (CC.SPPLAND+CC.SPPDISC)/CC.FISHCAT
    END AS \"pcat\"   -- proportion of total catch
--INTO #CATCH_EVENTS
FROM 
  #EVENT_CORE EC INNER JOIN
  #CATCH_CORE CC ON
    EC.DATABASE_NAME = CC.DATABASE_NAME AND
    EC.TRIP_ID = CC.TRIP_ID AND
    EC.FISHING_EVENT_ID = CC.FISHING_EVENT_ID 
WHERE
  CC.SPPLAND>0 OR CC.SPPDISC>0
  --CC.FISHCAT>0
  --CC.SPPLAND>0 OR CC.SPPDISC>0 OR CC.POP>0 OR CC.ORF>0 OR CC.TAR>0
--WHERE 
--  MC.SPECIES_CODE IN (@sppcode) --AND 
  --(MC.FISHERY_SECTOR IN ('GROUNDFISH TRAWL') OR 
  --(MC.FISHERY_SECTOR NOT IN ('GROUNDFISH TRAWL') AND ISNULL(MC.DATA_SOURCE_CODE,0) NOT IN (106,107)))
ORDER BY EC.TRIP_ID, EC.FISHING_EVENT_ID

-- qu("fos_mcatSPP.sql",dbName="GFFOS",strSpp="435")  -- BOR Bocaccio (190705)

