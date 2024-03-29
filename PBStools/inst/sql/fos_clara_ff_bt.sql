-- Query to determine catch weight of RFF compared to TFF for Gabe Andrews
SELECT --TOP 100
  MC.LONGITUDE AS X,
  MC.LATITUDE AS Y,
  MC.BEST_DEPTH AS Z,
  CONVERT(SMALLDATETIME,CONVERT(char(10),MC.BEST_DATE, 20)) AS 'date',
  CONVERT(INTEGER, ISNULL(MC.MAJOR_STAT_AREA_CODE,'00')) AS major,
  CONVERT(INTEGER, ISNULL(MC.MINOR_STAT_AREA_CODE,'00')) AS minor,
  ISNULL(MC.LOCALITY_CODE,0) AS locality,
  CASE WHEN MC.VESSEL_REGISTRATION_NUMBER IS NULL OR VESSEL_REGISTRATION_NUMBER=0 THEN ISNULL(MC.VESSEL_NAME,'UNKNOWN')
    ELSE MC.VESSEL_REGISTRATION_NUMBER END AS vessel,

  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('595') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- LEF Left-eyed flounders
    ELSE 0 END) as LEF,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('596') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- PAD Pacific Sanddab
    ELSE 0 END) as PAD,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('598') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- SPD Speckled Sanddab
    ELSE 0 END) as SPD,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('599') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- RIF Right-eyed flounders
    ELSE 0 END) as REF,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('602') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- ARF Arrowtooth Flounder
    ELSE 0 END) as ARF,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('604') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- RSL Roughscale Sole
    ELSE 0 END) as RSL,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('605') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- DEL Deepsea Sole
    ELSE 0 END) as DEL,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('607') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- PEL Petrale Sole
    ELSE 0 END) as PEL,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('610') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- RXL Rex Sole
    ELSE 0 END) as RXL,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('612') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- FHL Flathead Sole
    ELSE 0 END) as FHL,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('614') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- PAH Pacific Halibut
    ELSE 0 END) as PAH,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('617') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- FLL Forkline Sole
    ELSE 0 END) as FLL,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('619') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- BUL Butter Sole
    ELSE 0 END) as BUL,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('621') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- ROL Rock Sole
    ELSE 0 END) as ROL,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('622') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- NRL Northern Rock Sole
    ELSE 0 END) as NRL,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('623') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- YFL Yellowfin Sole
    ELSE 0 END) as YFL,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('625') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- SLL Slender Sole
    ELSE 0 END) as SLL,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('626') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- DOL Dover Sole
    ELSE 0 END) as DOL,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('628') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- ENL English Sole
    ELSE 0 END) as ENL,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('631') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- STF Starry Flounder
    ELSE 0 END) as STF,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('633') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- COL C-o Sole
    ELSE 0 END) as COL,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('635') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- CUL Curlfin Sole
    ELSE 0 END) as CUL,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('636') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- SAL Sand Sole
    ELSE 0 END) as SAL,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('638') THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)  -- GLH Greenland Halibut
    ELSE 0 END) as GLH
FROM 
  GF_MERGED_CATCH MC
WHERE
  Year(MC.BEST_DATE) BETWEEN 1996 AND 2024
  AND MC.LONGITUDE IS NOT NULL
  AND MC.LATITUDE IS NOT NULL
  AND MC.BEST_DEPTH IS NOT NULL
  AND MC.MAJOR_STAT_AREA_CODE IN ('03','04','05','06','07','08','09')
  --AND MC.GEAR IN ('MIDWATER TRAWL')
  AND MC.GEAR IN ('BOTTOM TRAWL', 'UNKNOWN TRAWL')
  --AND MC.GEAR IN ('BOTTOM TRAWL', 'MIDWATER TRAWL', 'UNKNOWN TRAWL')
GROUP BY
  MC.LONGITUDE,
  MC.LATITUDE,
  MC.BEST_DEPTH,
  CONVERT(SMALLDATETIME,CONVERT(char(10),MC.BEST_DATE, 20)),
  CONVERT(INTEGER, ISNULL(MC.MAJOR_STAT_AREA_CODE,'00')),
  CONVERT(INTEGER, ISNULL(MC.MINOR_STAT_AREA_CODE,'00')),
  ISNULL(MC.LOCALITY_CODE,0),
  CASE WHEN MC.VESSEL_REGISTRATION_NUMBER IS NULL OR VESSEL_REGISTRATION_NUMBER=0 THEN ISNULL(MC.VESSEL_NAME,'UNKNOWN')
    ELSE MC.VESSEL_REGISTRATION_NUMBER END

--qu("fos_clara_ff_bt.sql", dbName="GFFOS", strSpp="999")
