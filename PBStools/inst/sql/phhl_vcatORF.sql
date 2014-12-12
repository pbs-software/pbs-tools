-- Last modified by RH (2014-10-15)
-- PacHarvHL query for validation catch (LBS) of a target species, POP, and ORF (other rockfish)
-- Note: validation tables do not contain information at the tow level.
-- fisheryid: 2=Halibut, 3=Sablefish, 4=Schdeule II, 5=ZN, 
--            6=Sablefish+ZN, 7=Sablefish+Halibut, 8=Dogfish, 9=Lingcod

SET NOCOUNT ON -- prevents timeout errors

-- hails can fish in more than one PMFC major
SELECT 
  E.OBFL_HAIL_IN_NO AS phhl_hail, 
  IsNull(E.OBFL_MAJOR_STAT_AREA_CDE,0) AS phhl_major, 
  IsNull(E.OBFL_MINOR_STAT_AREA_CDE,0) AS phhl_minor, 
  IsNull(E.OBFL_LOCALITY_CDE,0) AS phhl_locality, 
  Count(E.OBFL_SET_NO) AS phhl_mset 
INTO #Msets
FROM B3_Fishing_Events E 
GROUP BY 
  E.OBFL_HAIL_IN_NO,
  IsNull(E.OBFL_MAJOR_STAT_AREA_CDE,0), 
  IsNull(E.OBFL_MINOR_STAT_AREA_CDE,0), 
  IsNull(E.OBFL_LOCALITY_CDE,0)
ORDER BY 
  E.OBFL_HAIL_IN_NO

SELECT 
  E.OBFL_HAIL_IN_NO AS phhl_hail, 
  Count(E.OBFL_SET_NO) AS phhl_nset 
INTO #Nsets
FROM B3_Fishing_Events E
GROUP BY 
  E.OBFL_HAIL_IN_NO
ORDER BY 
  E.OBFL_HAIL_IN_NO

SELECT
  M.*, 
  N.phhl_nset, 
  CAST(M.phhl_mset as FLOAT)/CAST(N.phhl_nset as FLOAT) AS 'phhl_pset',
  VS.vrec_species_cde AS 'spp',
  VS.vrec_landed_weight AS 'lbs',
  CAST(M.phhl_mset as FLOAT)/CAST(N.phhl_nset as FLOAT) * VS.vrec_landed_weight/2.20459 AS 'catKg'
INTO #Catch
FROM 
  (#Msets M INNER JOIN 
  #Nsets N ON
    M.phhl_hail = N.phhl_hail) INNER JOIN
  B6_Validation_Species VS ON  -- B6 Catch is in lbs (Lisa Lacko)
  N.phhl_hail = VS.vrec_hail_in_no
--WHERE N.phhl_hail IN (20607554,20607872)

SELECT
  --VH.vrec_hail_in_no,
  VH.vrec_fishery_id AS 'fid',
  VH.vrec_ext_lic_option_cde AS 'lic',
  --IsNull(VH.vrec_offload_dt,VH.vrec_departure_dt) AS 'date',
  CONVERT(char(10),IsNull(VH.vrec_offload_dt,VH.vrec_departure_dt),20) AS 'date',
  C.phhl_major AS 'major',
  C.phhl_minor AS 'minor',
  C.phhl_locality AS 'locality',
  landed = CAST(ROUND(Sum(CASE
    WHEN C.spp IN (@sppcode) THEN IsNull(C.catKg,0)
    ELSE 0 END ),7) AS NUMERIC(15,7)),
  discard = 0,
  -- target landings reference for discard calculations
  POP = CAST(ROUND(Sum( CASE
    WHEN C.spp IN ('396') THEN IsNull(C.catKg,0)
    ELSE 0 END ),7) AS NUMERIC(15,7)),
  ORF = CAST(ROUND(Sum(CASE -- all rockfish other than POP
    WHEN C.spp IN (@orfcode)
    THEN IsNull(C.catKg,0) ELSE 0 END ),7) AS NUMERIC(15,7)),
  PAH = CAST(ROUND(Sum(CASE -- Pacific halibut
    WHEN C.spp IN ('614') 
    THEN IsNull(C.catKg,0) ELSE 0 END),7) AS NUMERIC(15,7)),
  SBF = CAST(ROUND(Sum(CASE  -- Sablefish
    WHEN C.spp IN ('454','455') 
    THEN IsNull(C.catKg,0) ELSE 0 END),7) AS NUMERIC(15,7)),
  DOG = CAST(ROUND(Sum(CASE  -- Spiny dogfish
    WHEN C.spp IN ('042','044') 
    THEN IsNull(C.catKg,0) ELSE 0 END),7) AS NUMERIC(15,7)),
  LIN = CAST(ROUND(Sum(CASE  -- Lingcod
    WHEN C.spp IN ('467') 
    THEN IsNull(C.catKg,0) ELSE 0 END),7) AS NUMERIC(15,7)),
  RFA = CAST(ROUND(Sum(CASE
    WHEN C.spp IN ('424','407','431','433','442')
    THEN IsNull(C.catKg,0) ELSE 0 END),7) AS NUMERIC(15,7))
INTO #Vcat
FROM
  B5_Validation_Header VH INNER JOIN
  #Catch C ON
    VH.vrec_hail_in_no = C.phhl_hail
WHERE 
	VH.vrec_fishery_id IN (@fisheryid)
GROUP BY
  VH.vrec_hail_in_no,
  VH.vrec_fishery_id,
  VH.vrec_ext_lic_option_cde,
  IsNull(VH.vrec_offload_dt,VH.vrec_departure_dt),
  C.phhl_major,
  C.phhl_minor,
  C.phhl_locality
SELECT * FROM #Vcat V
WHERE V.landed>0 OR V.POP>0 OR V.ORF>0 OR V.PAH>0 OR V.SBF>0 OR V.DOG>0 OR V.LIN>0 OR V.RFA>0

-- getData("phhl_vcatORF.sql","PacHarvHL",strSpp="442",fisheryid=5)
-- qu("phhl_vcatORF.sql",dbName="PacHarvHL",strSpp="418")   ### validated (DMP) catch



