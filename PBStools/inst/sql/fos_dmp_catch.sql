--DMP Records for GFFOS
SET NOCOUNT ON 

-- Get Trip catches by species groups
SELECT 
  Year(OH.OFFLOAD_DATE) AS [year],
  OH.TRIP_ID,
  CONVERT(VARCHAR(10),OH.OFFLOAD_DATE,20) AS \"date\",
  --OH.OFFLOAD_DATE as [date],
  OH.GEAR As gear,
  SUM(CASE
    WHEN OD.SPECIES_CODE IN (@sppcode) THEN COALESCE(OD.OFFLOAD_WEIGHT * OD.WEIGHT_CONVERSION_FACTOR / 2.20459, 0)
    ELSE 0 END) AS landed,
  discard = 0,
  SUM(CASE
    WHEN OD.SPECIES_CODE IN ('396') THEN COALESCE(OD.OFFLOAD_WEIGHT * OD.WEIGHT_CONVERSION_FACTOR / 2.20459, 0)
    ELSE 0 END) AS POP,
  SUM(CASE
    WHEN OD.SPECIES_CODE IN (@orfcode) THEN COALESCE(OD.OFFLOAD_WEIGHT * OD.WEIGHT_CONVERSION_FACTOR / 2.20459, 0)
    ELSE 0 END) AS ORF, 
  SUM(CASE  -- target landings reference for discard calculations (very vague at the TRIP level
    WHEN OH.GEAR IN ('TRAWL') AND OD.SPECIES_CODE IN (@trfcode)
      THEN COALESCE(OD.OFFLOAD_WEIGHT * OD.WEIGHT_CONVERSION_FACTOR / 2.20459, 0)
      ELSE 0 END) AS TAR1,
  SUM(CASE
      WHEN OH.GEAR IN ('TRAP OR LONGLINE OR HOOK AND L', 'LONGLINE', 'undefined', 'LONGLINE OR HOOK AND LINE') AND OD.SPECIES_CODE IN ('614')
      THEN COALESCE(OD.OFFLOAD_WEIGHT * OD.WEIGHT_CONVERSION_FACTOR / 2.20459, 0)
      ELSE 0 END) AS TAR2,
  SUM(CASE
    WHEN OH.GEAR IN ('TRAP','TRAP OR LONGLINE OR HOOK AND L') AND OD.SPECIES_CODE IN ('454','455')
      THEN COALESCE(OD.OFFLOAD_WEIGHT * OD.WEIGHT_CONVERSION_FACTOR / 2.20459, 0)
      ELSE 0 END) AS TAR3,
  SUM(CASE
    WHEN OH.GEAR IN ('TRAP OR LONGLINE OR HOOK AND L', 'LONGLINE', 'undefined', 'LONGLINE OR HOOK AND LINE') AND OD.SPECIES_CODE IN ('042','044','467')
      THEN COALESCE(OD.OFFLOAD_WEIGHT * OD.WEIGHT_CONVERSION_FACTOR / 2.20459, 0)
      ELSE 0 END) AS TAR4,
  SUM(CASE
    WHEN OH.GEAR IN ('TRAP OR LONGLINE OR HOOK AND L', 'LONGLINE', 'undefined', 'LONGLINE OR HOOK AND LINE') AND OD.SPECIES_CODE IN ('424','407','431','433','442')
      THEN COALESCE(OD.OFFLOAD_WEIGHT * OD.WEIGHT_CONVERSION_FACTOR / 2.20459, 0)
      ELSE 0 END) AS TAR5
--INTO #TRIP_CATCH
FROM
  GF_OFFLOAD_HEADER OH INNER JOIN
  GF_OFFLOAD_DETAIL OD ON
    OH.OFFLOAD_ID = OD.OFFLOAD_ID
WHERE
  OH.TRIP_ID IN (74206,74212,74804,80726,80852) AND --for testing
  --OH.GEAR NOT IN ('TRAWL') AND 
  OH.OFFLOAD_DATE BETWEEN CONVERT(DATETIME, '2007-01-01', 102) AND CONVERT(DATETIME, '2012-12-31', 102)
GROUP BY
  Year(OH.OFFLOAD_DATE),
  OH.TRIP_ID,
  CONVERT(VARCHAR(10),OH.OFFLOAD_DATE,20),
  --OH.OFFLOAD_DATE,
  OH.GEAR
ORDER BY
  Year(OH.OFFLOAD_DATE),
  OH.TRIP_ID


--qu("fos_dmp_catch.sql", dbName="GFFOS", strSpp="435")
