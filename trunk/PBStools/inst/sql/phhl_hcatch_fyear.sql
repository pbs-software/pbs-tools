-- PacHarvHL query for validation catch (LBS) of a halibut, POP, and ORF (other rockfish)
-- note: validation tables do not contain information at the tow level.
-- fisheryid: 2=Halibut, 3=Sablefish, 4=Schdeule II, 5=ZN (takes long time to execute), 
--            6=Sablefish+ZN, 7=Sablefish+Halibut, 8=Dogfish, 9=Lingcod

SET NOCOUNT ON -- prevents timeout errors

SELECT
  VS.vrec_hail_in_no,
  VS.vrec_species_cde AS 'spp',
  CASE
    WHEN RTRIM(LTRIM(VA.vrec_mgmt_catch_area)) IN ('4B','SG','12','13','14','15','16','17','18','19','20',
        '28','29','29A','29B','29C','29D','29E') THEN 1
    WHEN RTRIM(LTRIM(VA.vrec_mgmt_catch_area)) IN ('4A','C') THEN 2
    WHEN RTRIM(LTRIM(VA.vrec_mgmt_catch_area)) IN ('3C','21','22','23','24','121','122','123','124') THEN 3
    WHEN RTRIM(LTRIM(VA.vrec_mgmt_catch_area)) IN ('3D','WC','25','26','27','125','126','127') THEN 4
    WHEN RTRIM(LTRIM(VA.vrec_mgmt_catch_area)) IN ('5A','10','11','111') THEN 5
    WHEN RTRIM(LTRIM(VA.vrec_mgmt_catch_area)) IN ('5B','CC','7','8','9','107','108','109','110','130','30') THEN 6
    WHEN RTRIM(LTRIM(VA.vrec_mgmt_catch_area)) IN ('5C','6','102','106','2E','2B-EAST') THEN 7
    WHEN RTRIM(LTRIM(VA.vrec_mgmt_catch_area)) IN ('5D','PR','NC','1','3','4','5','101','103','104','105','2A-EAST') THEN 8
    WHEN RTRIM(LTRIM(VA.vrec_mgmt_catch_area)) IN ('5E','QC','2','141','142','2W','2A-WEST','2B-WEST') THEN 9
    ELSE 0 END AS 'major',
  CAST(VA.vrec_weight_percentage as FLOAT)/100. AS 'pcat',
  VS.vrec_landed_weight AS 'lbs',
  (CAST(VA.vrec_weight_percentage as FLOAT)/100. * VS.vrec_landed_weight)/2.20459 AS 'catKg'
INTO #Catch
FROM
  B6_Validation_Species VS INNER JOIN  -- B6 Catch is in lbs (Lisa Lacko)
  B7_Validation_Areas VA ON
    VS.vrec_hail_in_no  = VA.vrec_hail_in_no AND
    VS.vrec_species_cde = VA.vrec_species_cde
--select * from #Catch

-- summarize catch by year (rows) and pmfc major area (columns), i.e., crosstab
SELECT 
   CC.fyear,
   SUM(CASE major WHEN 3 THEN (CC.catKg) END) / 1000. AS '3C', 
   SUM(CASE major WHEN 4 THEN (CC.catKg) END) / 1000. AS '3D', 
   SUM(CASE major WHEN 1 THEN (CC.catKg) END) / 1000. AS '4B', 
   SUM(CASE major WHEN 5 THEN (CC.catKg) END) / 1000. AS '5A', 
   SUM(CASE major WHEN 6 THEN (CC.catKg) END) / 1000. AS '5B', 
   SUM(CASE major WHEN 7 THEN (CC.catKg) END) / 1000. AS '5C', 
   SUM(CASE major WHEN 8 THEN (CC.catKg) END) / 1000. AS '5D', 
   SUM(CASE major WHEN 9 THEN (CC.catKg) END) / 1000. AS '5E', 
   SUM(CASE WHEN CC.major NOT IN (1,3,4,5,6,7,8,9) THEN (CC.catKg) END) / 1000. AS 'UNK' 
FROM 
   (SELECT
      CAST(CASE 
         WHEN Year(VH.vrec_offload_dt) < 1997 OR Month(VH.vrec_offload_dt) > 3 THEN Year(VH.vrec_offload_dt) 
         WHEN Year(VH.vrec_offload_dt) = 1997 AND Month(VH.vrec_offload_dt) <= 3 THEN '19967' 
         WHEN Year(VH.vrec_offload_dt) > 1997 AND Month(VH.vrec_offload_dt) <= 3 THEN Year(VH.vrec_offload_dt)-1 
         WHEN VH.vrec_offload_dt IS NULL THEN '9999' END AS VARCHAR) AS fyear, 
      C.catKg, 
      C.major
    FROM
      B5_Validation_Header VH INNER JOIN
      #Catch C ON
    VH.vrec_hail_in_no = C.vrec_hail_in_no
    WHERE 
      C.spp IN (@sppcode) AND
      VH.vrec_fishery_id IN (2,7)  ) AS CC
GROUP BY fyear 
ORDER BY fyear

-- getData("phhl_hcatch_fyear.sql","PacHarvHL",strSpp="394")

