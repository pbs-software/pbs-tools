SET NOCOUNT ON 

SELECT
  MC.FISHERY_SECTOR AS SECTOR, 
  Year(MC.BEST_DATE) AS 'YEAR', 
  MC.VESSEL_REGISTRATION_NUMBER AS VRN
INTO #VRN_SPP
FROM
  GF_MERGED_CATCH MC
WHERE
   MC.SPECIES_CODE IN (@sppcode)
GROUP BY
  MC.FISHERY_SECTOR, 
  Year(MC.BEST_DATE), 
  MC.VESSEL_REGISTRATION_NUMBER
HAVING
  Year(MC.BEST_DATE) Between 2018 And 2022
ORDER BY
  MC.FISHERY_SECTOR,
  Year(MC.BEST_DATE)

SELECT
  VS.SECTOR,
  VS.YEAR,
  COUNT(VS.VRN) AS N_VESSELS
FROM #VRN_SPP VS
GROUP BY
  VS.SECTOR,
  VS.YEAR
ORDER BY
  VS.SECTOR,
  VS.YEAR
