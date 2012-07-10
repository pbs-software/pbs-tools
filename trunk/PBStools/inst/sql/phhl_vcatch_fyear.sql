-- PacHarvHL query for validation catch (LBS) of a target species, POP, and ORF (other rockfish)
-- Note: validation tables do not contain information at the tow level.
-- fisheryid: 2=Halibut, 3=Sablefish, 4=Schdeule II, 5=ZN (takes long time to execute), 
--            6=Sablefish+ZN, 7=Sablefish+Halibut, 8=Dogfish, 9=Lingcod

SET NOCOUNT ON -- prevents timeout errors

-- hails can fish in more than one PMFC major
SELECT 
  E.OBFL_HAIL_IN_NO AS phhl_hail, 
  IsNull(E.OBFL_MAJOR_STAT_AREA_CDE,0) AS phhl_major, 
  IsNull(E.OBFL_MINOR_STAT_AREA_CDE,0) AS phhl_minor, 
  Count(E.OBFL_SET_NO) AS phhl_mset 
INTO #Msets
FROM B3_Fishing_Events E 
GROUP BY 
  E.OBFL_HAIL_IN_NO, E.OBFL_MAJOR_STAT_AREA_CDE, E.OBFL_MINOR_STAT_AREA_CDE
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
      C.phhl_major AS major
    FROM 
      B5_Validation_Header VH INNER JOIN
      #Catch C ON
        VH.vrec_hail_in_no = C.phhl_hail  
    WHERE C.spp IN (@sppcode) ) AS CC
GROUP BY fyear 
ORDER BY fyear


-- getData("phhl_vcatch_fyear.sql","PacHarvHL",strSpp="394",fisheryid=4)


