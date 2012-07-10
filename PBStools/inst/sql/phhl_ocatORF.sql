-- PacHarvHL query for official catch landings (LBS) of a target species, POP, and ORF (other rockfish)
-- note: D_OFFICIAL_CATCH only contains catch (LBS) for SchII and ZN.
-- fisheryid: 2=Halibut, 3=Sablefish, 4=Schdeule II, 5=ZN (takes long time to execute), 
--            6=Sablefish+ZN, 7=Sablefish+Halibut, 8=Dogfish, 9=Lingcod

SET NOCOUNT ON -- prevents timeout errors

-- compile the landings for target, POP, & ORF
SELECT 
  'fid' = CAST(OC.OBFL_FISHERY_ID AS VARCHAR(1)),
    --CASE WHEN OC.OBFL_FISHERY_ID=4 AND OC.LICENCE_OPTION='D' THEN 8
    --WHEN OC.OBFL_FISHERY_ID=4 AND OC.LICENCE_OPTION='L' THEN 9
    --ELSE OC.OBFL_FISHERY_ID END AS VARCHAR(1)),
  'lic' = RTRIM(LTRIM(IsNull(OC.LICENCE_OPTION,''))),
  'date' = CAST(OC.Offload_Dt AS smalldatetime),
  'major' = CASE OC.MAJOR_AREA
    WHEN '5E' THEN 9
    WHEN '5D' THEN 8
    WHEN '5C' THEN 7
    WHEN '5B' THEN 6
    WHEN '5A' THEN 5
    WHEN '3D' THEN 4
    WHEN '3C' THEN 3
    WHEN '4B' THEN 1
    ELSE 0 END,
  'minor' = CASE
    WHEN OC.OBFL_DFO_MGMT_AREA_CDE IN (142) AND OC.OBFL_DFO_MGMT_SUBAREA_CDE IN (1) THEN 34 -- for POP in Anthony Island
    ELSE 0 END,
  'cfv' = CAST(IsNull(OC.VRN,'0') AS VARCHAR(5)),
  'eff' = Sum(IsNull(OC.Duration,0)),
  'landed' = Sum( CASE OC.OBFL_SPECIES_CDE
    WHEN @sppcode THEN IsNull(OC.CATCH_LBS,0)
    ELSE 0 END )/2.20459,
  'discard' = Sum( CASE OC.OBFL_SPECIES_CDE
    WHEN @sppcode THEN IsNull(OC.DISCARDED_LBS,0)
    ELSE 0 END )/2.20459,
  'POP' = Sum( CASE OC.OBFL_SPECIES_CDE
    WHEN '396' THEN IsNull(OC.CATCH_LBS,0)
    ELSE 0 END )/2.20459,
  'ORF' = Sum(   -- all rockfish landings other than POP
    -- CASE WHEN OC.OBFL_SPECIES_CDE <> '396' AND S.Rockfish=1 THEN IsNull(OC.CATCH_LBS,0)
    CASE WHEN OC.OBFL_SPECIES_CDE IN (@orfcode) THEN IsNull(OC.CATCH_LBS,0)
    ELSE 0 END )/2.20459,
  'TAR' = Sum( CASE  -- target landings reference for discard calculations
    WHEN OC.OBFL_FISHERY_ID=4 AND OC.LICENCE_OPTION='D' AND OC.OBFL_SPECIES_CDE IN ('042','044') THEN IsNull(OC.CATCH_LBS,0)
    WHEN OC.OBFL_FISHERY_ID=4 AND OC.LICENCE_OPTION='L' AND OC.OBFL_SPECIES_CDE IN ('467') THEN IsNull(OC.CATCH_LBS,0)
    WHEN OC.OBFL_FISHERY_ID=4 AND OC.LICENCE_OPTION NOT IN ('D','L') AND OC.OBFL_SPECIES_CDE IN ('042','044','467') THEN IsNull(OC.CATCH_LBS,0)
    WHEN OC.OBFL_FISHERY_ID=5 AND OC.OBFL_SPECIES_CDE IN ('424','407','431','433','442') THEN IsNull(OC.CATCH_LBS,0)
    ELSE 0 END)/2.20459
  INTO #Catch
  FROM 
    D_OFFICIAL_CATCH OC 
  WHERE
    OC.OBFL_FISHERY_ID IN (@fisheryid)
  GROUP BY 
    OC.OBFL_HAIL_IN_NO, OC.OBFL_SET_NO,
    CAST(OC.OBFL_FISHERY_ID AS VARCHAR(1)),
    --CAST(CASE 
    --  WHEN OC.OBFL_FISHERY_ID=4 AND OC.LICENCE_OPTION='D' THEN 8
    --  WHEN OC.OBFL_FISHERY_ID=4 AND OC.LICENCE_OPTION='L' THEN 9
    --  ELSE OC.OBFL_FISHERY_ID END AS VARCHAR(1)),
    RTRIM(LTRIM(IsNull(OC.LICENCE_OPTION,''))),
    CAST(OC.Offload_Dt AS smalldatetime),
    CASE OC.MAJOR_AREA
      WHEN '5E' THEN 9
      WHEN '5D' THEN 8
      WHEN '5C' THEN 7
      WHEN '5B' THEN 6
      WHEN '5A' THEN 5
      WHEN '3D' THEN 4
      WHEN '3C' THEN 3
      WHEN '4B' THEN 1
      ELSE 0 END,
    CASE
      WHEN OC.OBFL_DFO_MGMT_AREA_CDE IN (142) AND OC.OBFL_DFO_MGMT_SUBAREA_CDE IN (1) THEN 34 -- for POP in Anthony Island
      ELSE 0 END,
    CAST(IsNull(OC.VRN,'0') AS VARCHAR(5))

SELECT * FROM #Catch C
  WHERE
    (C.landed>0 OR C.discard>0 OR C.POP>0 OR C.ORF>0 OR C.TAR>0)

-- SELECT * FROM #Catch
-- getData("phhl_ocatORF.sql","PacHarvHL",strSpp="424",fisheryid=4)
-- getData("phhl_ocatORF.sql","PacHarvHL",strSpp="424",fisheryid=5)

