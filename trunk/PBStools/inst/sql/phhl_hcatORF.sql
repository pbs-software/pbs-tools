-- PacHarvHL query for validation catch (LBS) of halibut, POP, and ORF (other rockfish)
-- note: validation tables do not contain information at the tow level.
-- fisheryid: 2=Halibut, 3=Sablefish, 4=Schdeule II, 5=ZN, 
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
  CASE
    WHEN (RTRIM(LTRIM(VA.vrec_mgmt_catch_area)) IN ('142') AND RTRIM(LTRIM(VA.vrec_mgmt_subarea)) IN ('1')) OR
         (RTRIM(LTRIM(VA.vrec_mgmt_catch_area)) IN ('2') AND 
          RTRIM(LTRIM(VA.vrec_mgmt_subarea)) IN ('31','32','33','34','35','36','37','38','39','40','41','42','43','44','45','46','47'))
      THEN 34
    ELSE 0 END AS 'minor',
  CAST(VA.vrec_weight_percentage as FLOAT)/100. AS 'pcat',
  VS.vrec_landed_weight AS 'lbs',
  (CAST(VA.vrec_weight_percentage as FLOAT)/100. * VS.vrec_landed_weight)/2.20459 AS 'catKg'
INTO #Catch
FROM
  B6_Validation_Species VS INNER JOIN  -- B6 Catch is in lbs (Lisa Lacko)
  B7_Validation_Areas VA ON
    VS.vrec_hail_in_no  = VA.vrec_hail_in_no AND
    VS.vrec_species_cde = VA.vrec_species_cde
--WHERE
--  VS.vrec_hail_in_no IN (20905035,20905071) -- for testing purposes

SELECT
  --VH.vrec_hail_in_no,
  VH.vrec_fishery_id AS 'fid',
  VH.vrec_ext_lic_option_cde AS 'lic',
  --IsNull(VH.vrec_offload_dt,VH.vrec_departure_dt) AS 'date',
  CONVERT(char(10),IsNull(VH.vrec_offload_dt,VH.vrec_departure_dt),20) AS 'date',
  C.major,
  C.minor,
  landed = Sum(CASE
    WHEN C.spp IN (@sppcode) THEN IsNull(C.catKg,0)
    ELSE 0 END ),
  discard = 0,
  -- target landings reference for discard calculations
  POP = Sum( CASE
    WHEN C.spp IN ('396') THEN IsNull(C.catKg,0)
    ELSE 0 END ),
  ORF = Sum(CASE -- all rockfish other than POP
    WHEN C.spp IN (@orfcode)
    THEN IsNull(C.catKg,0) ELSE 0 END ),
  PAH = Sum(CASE -- Pacific halibut
    WHEN C.spp IN ('614') 
    THEN IsNull(C.catKg,0) ELSE 0 END),
  SBF = Sum(CASE  -- Sablefish
    WHEN C.spp IN ('454','455') 
    THEN IsNull(C.catKg,0) ELSE 0 END),
  DOG = Sum(CASE  -- Spiny dogfish
    WHEN C.spp IN ('042','044') 
    THEN IsNull(C.catKg,0) ELSE 0 END),
  LIN = Sum(CASE  -- Lingcod
    WHEN C.spp IN ('467') 
    THEN IsNull(C.catKg,0) ELSE 0 END),
  RFA = Sum(CASE
    WHEN C.spp IN ('424','407','431','433','442')
    THEN IsNull(C.catKg,0) ELSE 0 END)
INTO #Hcat
FROM
  B5_Validation_Header VH INNER JOIN
  #Catch C ON
    VH.vrec_hail_in_no = C.vrec_hail_in_no
WHERE 
	VH.vrec_fishery_id IN (2,7)
GROUP BY
  VH.vrec_hail_in_no,
  VH.vrec_fishery_id,
  VH.vrec_ext_lic_option_cde,
  IsNull(VH.vrec_offload_dt,VH.vrec_departure_dt),
  C.major,
  C.minor

SELECT * FROM #Hcat H
WHERE H.landed>0 OR H.POP>0 OR H.ORF>0 OR H.PAH>0 OR H.SBF>0 OR H.DOG>0 OR H.LIN>0 OR H.RFA>0

-- getData("phhl_hcatORF.sql","PacHarvHL",strSpp="442")


