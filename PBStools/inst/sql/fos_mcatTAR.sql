-- How much TAR is caught be catch recon FIDs?
SET NOCOUNT ON 

SELECT
  (CASE
    WHEN MC.FISHERY_SECTOR IN ('GROUNDFISH TRAWL','JOINT VENTURE TRAWL') THEN 1
    WHEN MC.FISHERY_SECTOR IN ('HALIBUT','HALIBUT AND SABLEFISH','K/L') THEN 2
    WHEN MC.FISHERY_SECTOR IN ('SABLEFISH') THEN 3
    WHEN MC.FISHERY_SECTOR IN ('LINGCOD','SPINY DOGFISH','SCHEDULE II') THEN 4
    WHEN MC.FISHERY_SECTOR IN ('ROCKFISH INSIDE','ROCKFISH OUTSIDE','ZN','K/ZN') THEN 5
    WHEN MC.FISHERY_SECTOR IN ('FOREIGN') THEN 9
    ELSE 0 END) AS FID,
  Year(MC.BEST_DATE) AS [YEAR],
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('614') THEN MC.LANDED_KG ELSE 0 END)/1000. AS PAH,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('454','455') THEN MC.LANDED_KG ELSE 0 END)/1000. AS SBF,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('042','044') THEN MC.LANDED_KG ELSE 0 END)/1000. AS DOG,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('467') THEN MC.LANDED_KG ELSE 0 END)/1000. AS LIN,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('396') THEN MC.LANDED_KG ELSE 0 END)/1000. AS POP,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN (@orfcode) THEN MC.LANDED_KG ELSE 0 END)/1000. AS ORF,
  SUM(CASE
    WHEN MC.SPECIES_CODE IN ('424','407','431','433','442') THEN MC.LANDED_KG ELSE 0 END)/1000. AS IRF
FROM
  GF_MERGED_CATCH MC
WHERE
  --MC.SPECIES_CODE IN (@sppcode) AND 
  Year(MC.BEST_DATE) >= 1996
GROUP BY
  (CASE
    WHEN MC.FISHERY_SECTOR IN ('GROUNDFISH TRAWL','JOINT VENTURE TRAWL') THEN 1
    WHEN MC.FISHERY_SECTOR IN ('HALIBUT','HALIBUT AND SABLEFISH','K/L') THEN 2
    WHEN MC.FISHERY_SECTOR IN ('SABLEFISH') THEN 3
    WHEN MC.FISHERY_SECTOR IN ('LINGCOD','SPINY DOGFISH','SCHEDULE II') THEN 4
    WHEN MC.FISHERY_SECTOR IN ('ROCKFISH INSIDE','ROCKFISH OUTSIDE','ZN','K/ZN') THEN 5
    WHEN MC.FISHERY_SECTOR IN ('FOREIGN') THEN 9
    ELSE 0 END),
    Year(MC.BEST_DATE)
ORDER BY
  (CASE
    WHEN MC.FISHERY_SECTOR IN ('GROUNDFISH TRAWL','JOINT VENTURE TRAWL') THEN 1
    WHEN MC.FISHERY_SECTOR IN ('HALIBUT','HALIBUT AND SABLEFISH','K/L') THEN 2
    WHEN MC.FISHERY_SECTOR IN ('SABLEFISH') THEN 3
    WHEN MC.FISHERY_SECTOR IN ('LINGCOD','SPINY DOGFISH','SCHEDULE II') THEN 4
    WHEN MC.FISHERY_SECTOR IN ('ROCKFISH INSIDE','ROCKFISH OUTSIDE','ZN','K/ZN') THEN 5
    WHEN MC.FISHERY_SECTOR IN ('FOREIGN') THEN 9
    ELSE 0 END),
    Year(MC.BEST_DATE)

--qu("fos_mcatTAR.sql",dbName="GFFOS", strSpp="467")
