-- Query to determine catch weight of RRF compared to TRF for statement in TOR (started with WWR 2019)
SELECT
  Year(MC.BEST_DATE) AS 'YEAR',
  SUM(CASE
    WHEN MC.SPECIES_CODE IN (@sppcode) THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)
    ELSE 0 END)/1000. as RRF,
  SUM(CASE
    WHEN S.Rockfish IN (1) THEN ISNULL(MC.LANDED_KG,0) + ISNULL(MC.DISCARDED_KG,0)
    ELSE 0 END)/1000. as TRF
FROM 
  GF_MERGED_CATCH MC INNER JOIN
  PacHarvest.dbo.C_Species S ON 
  MC.SPECIES_CODE = S.SPECIES_CDE
WHERE
  Year(MC.BEST_DATE) BETWEEN 1996 AND 2019
  --AND MC.GEAR IN ('BOTTOM TRAWL', 'MIDWATER TRAWL', 'UNKNOWN TRAWL')
  AND MC.MAJOR_STAT_AREA_CODE IN ('03','04','05','06','07','08','09')
  AND S.Rockfish IN (1)
GROUP BY
  Year(MC.BEST_DATE)

-- qu("fos_rrf_trf_catch.sql",dbName="GFFOS",strSpp="417") -- Widow Rockfish (WWR: 190307)
-- qu("fos_rrf_trf_catch.sql",dbName="GFFOS",strSpp="435") -- Bocaccio (BOR: 190705)
-- qu("fos_rrf_trf_catch.sql",dbName="GFFOS",strSpp="394") -- Rougheye Rockfish (RER: 191210)

