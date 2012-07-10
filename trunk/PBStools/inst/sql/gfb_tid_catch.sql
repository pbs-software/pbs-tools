-- Deprecated (use gfb_pht_catch.sql)
-- Get commercial catch from PacHarvest; match trip IDs in GFBioSQL & FOS.  (2010-02-08)
SET NOCOUNT ON  -- prevents timeout errors

-- Get GFB Trip from Hail-Vessel-Date combo
SELECT
  --CAST(IsNull(SA.HAIL_IN_NO,'0') AS INT) AS hail, 
  SA.HAIL_IN_NO AS gfb_hail, 
  V.CFV_NUM AS gfb_cfv, 
  ISNULL(SA.TRIP_START_DATE, SA.TRIP_END_DATE) AS gfb_date, 
  SA.TRIP_ID AS gfb_tid
  INTO 
    #GFB_HVD
  FROM
    B21_Samples SA INNER JOIN 
    C_Vessels V ON 
    SA.VESSEL_ID = V.VESSEL_ID AND 
    SA.SUFFIX = V.SUFFIX
  WHERE
    SA.HAIL_IN_NO Is Not NULL
  GROUP BY
    SA.HAIL_IN_NO, 
    V.CFV_NUM, 
    ISNULL(SA.TRIP_START_DATE, SA.TRIP_END_DATE), 
    SA.TRIP_ID

-- Get FOS Trip from Hail-Vessel-Date combo
SELECT * INTO #FOS_HVD
  FROM OPENQUERY(ORADEV,
  'SELECT 
    CAST(H.HAIL_NUMBER as VARCHAR(20)) AS fos_hail, 
    T.VESSEL_REGISTRATION_NUMBER AS fos_cfv, 
    NVL(T.TRIP_START_DATE,T.TRIP_END_DATE) AS fos_date, 
    T.TRIP_ID AS fos_tid
  FROM 
    GFFOS.GF_TRIP T INNER JOIN 
    GFFOS.GF_HAIL_NUMBER H ON 
    T.TRIP_ID = H.TRIP_ID
  GROUP BY
    CAST(H.HAIL_NUMBER as VARCHAR(20)), 
    T.VESSEL_REGISTRATION_NUMBER, 
    NVL(T.TRIP_START_DATE,T.TRIP_END_DATE), 
    T.TRIP_ID
  ')

-- Get joint TIDs
SELECT
  GFB.gfb_tid  AS TID_gfb,
  IsNull(FOS.fos_tid,0)  AS TID_fos,
  GFB.gfb_hail AS hail,
  GFB.gfb_cfv  AS cfv,
  GFB.gfb_date AS \"date\"
  INTO #TIDS
  FROM
    #GFB_HVD GFB LEFT OUTER JOIN
    #FOS_HVD FOS ON
      GFB.gfb_hail = FOS.fos_hail AND
      GFB.gfb_cfv  = FOS.fos_cfv  AND
      GFB.gfb_date = FOS.fos_date

-- Get PHT Catches
SELECT 
  T.OBFL_HAIL_IN_NO AS hail,
  CAST(T.OBFL_VSL_CFV_NO as INT) AS cfv,
  IsNull(T.OBFL_DEPARTURE_DT,T.OBFL_OFFLOAD_DT) AS \"date\",
  IsNull(FE.OBFL_MAJOR_STAT_AREA_CDE,0) AS major,
  IsNull(FE.OBFL_MINOR_STAT_AREA_CDE,0) AS minor,
  MC.SPECIES_CODE AS spp,
  -- SC.util as \"util\",
  Sum(MC.RETAINED+MC.DISCARDED) AS catKg
INTO #PHT
FROM
  PacHarvest.dbo.B2_Trips T RIGHT OUTER JOIN
  (PacHarvest.dbo.B3_Fishing_Events FE RIGHT OUTER JOIN
  PacHarvest.dbo.D_Merged_Catches MC ON
    FE.OBFL_HAIL_IN_NO = MC.HAIL_IN_NO AND
    FE.OBFL_SET_NO = MC.SET_NO) ON
    T.OBFL_HAIL_IN_NO = FE.OBFL_HAIL_IN_NO
WHERE
  MC.SPECIES_CODE IN (@sppcode)
GROUP BY
  T.OBFL_HAIL_IN_NO,
  CAST(T.OBFL_VSL_CFV_NO as INT),
  IsNull(T.OBFL_DEPARTURE_DT,T.OBFL_OFFLOAD_DT),
  IsNull(FE.OBFL_MAJOR_STAT_AREA_CDE,0),
  IsNull(FE.OBFL_MINOR_STAT_AREA_CDE,0),
  MC.SPECIES_CODE

SELECT
  IsNull(TID.TID_gfb,0) AS TID_gfb,
  IsNull(TID.TID_fos,0) AS TID_fos,
  PHT.date,
  PHT.major,
  PHT.minor,
  PHT.spp,
  Sum(PHT.catKg) AS catKg
FROM
  #TIDS TID RIGHT OUTER JOIN
  #PHT  PHT ON
    TID.hail = PHT.hail AND
    TID.cfv  = PHT.cfv  AND
    TID.date = PHT.date
WHERE 
  PHT.date IS NOT NULL
GROUP BY
  IsNull(TID.TID_gfb,0),
  IsNull(TID.TID_fos,0),
  PHT.date,
  PHT.major,
  PHT.minor,
  PHT.spp


-- getData("gfb_fos_tid.sql","GFBioSQL",strSpp="439")

