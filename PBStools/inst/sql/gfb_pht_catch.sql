-- Get commercial catch from PacHarvest; match trip IDs in GFBioSQL & GFFOS.  (2010-02-08)
SET NOCOUNT ON  -- prevents timeout errors

-- Get GFB Trip ID for selected species from Hail-Vessel-Date combo
SELECT
  --CAST(IsNull(SA.HAIL_IN_NO,'0') AS INT) AS hail, 
  SA.HAIL_IN_NO AS gfb_hail, 
  V.CFV_NUM AS gfb_cfv, 
  CONVERT(char(10),ISNULL(SA.TRIP_START_DATE, SA.TRIP_END_DATE),20) AS gfb_date, 
  CONVERT(char(7),ISNULL(SA.TRIP_START_DATE, SA.TRIP_END_DATE),20) AS gfb_yrmo, 
  SA.TRIP_ID AS gfb_tid
  INTO 
    #GFB_HVD
  FROM
    B21_Samples SA INNER JOIN 
    C_Vessels V ON 
    SA.VESSEL_ID = V.VESSEL_ID AND 
    SA.SUFFIX = V.SUFFIX
  WHERE
    SA.SPECIES_CODE IN (@sppcode) AND
    SA.HAIL_IN_NO Is Not NULL
  GROUP BY
    SA.HAIL_IN_NO, 
    V.CFV_NUM, 
    convert(char(10),ISNULL(SA.TRIP_START_DATE, SA.TRIP_END_DATE),20), 
    convert(char(7),ISNULL(SA.TRIP_START_DATE, SA.TRIP_END_DATE),20), 
    SA.TRIP_ID

-- Get FOS Trip from Hail-Vessel-Date combo
SELECT * INTO #FOS_HVD
  FROM OPENQUERY(GFSH,
  'SELECT 
    CAST(HT.HAIL_NUMBER as VARCHAR(20)) AS fos_hail, 
    T.VESSEL_REGISTRATION_NUMBER AS fos_cfv, 
    TO_CHAR(NVL(T.TRIP_START_DATE,T.TRIP_END_DATE),''YYYY-MM-DD'') AS fos_date,
    T.TRIP_ID AS fos_tid
  FROM 
    GFFOS.GF_TRIP T RIGHT OUTER JOIN 
    (SELECT 
      CAST(H.HAIL_NUMBER as VARCHAR(20)) AS HAIL_NUMBER, 
      MIN(H.TRIP_ID) AS TRIP_ID
    FROM 
      GFFOS.GF_HAIL_NUMBER H
    GROUP BY
      CAST(H.HAIL_NUMBER as VARCHAR(20)) ) HT ON
    T.TRIP_ID = HT.TRIP_ID
  ')

-- Get FOS Trip from Hail-Vessel-YrMo combo
SELECT * INTO #FOS_HVYM
  FROM OPENQUERY(GFSH,
  'SELECT 
    CAST(HT.HAIL_NUMBER as VARCHAR(20)) AS fos_hail, 
    T.VESSEL_REGISTRATION_NUMBER AS fos_cfv, 
    TO_CHAR(NVL(T.TRIP_START_DATE,T.TRIP_END_DATE),''YYYY-MM'') AS fos_yrmo,
    T.TRIP_ID AS fos_tid
  FROM 
    GFFOS.GF_TRIP T RIGHT OUTER JOIN 
    (SELECT 
      CAST(H.HAIL_NUMBER as VARCHAR(20)) AS HAIL_NUMBER, 
      MIN(H.TRIP_ID) AS TRIP_ID
    FROM 
      GFFOS.GF_HAIL_NUMBER H
    GROUP BY
      CAST(H.HAIL_NUMBER as VARCHAR(20)) ) HT ON
    T.TRIP_ID = HT.TRIP_ID
  ')

-- Get joint TIDs
SELECT
  GFB.gfb_tid  AS TID_gfb,
  COALESCE(FOS1.fos_tid,FOS2.fos_tid,0)  AS TID_fos,
  GFB.gfb_hail AS hail,
  GFB.gfb_cfv  AS cfv,
  GFB.gfb_date AS \"date\",
  GFB.gfb_yrmo AS yrmo
  INTO #TIDS
  FROM
    #FOS_HVYM FOS2 RIGHT OUTER JOIN
    (#FOS_HVD FOS1 RIGHT OUTER JOIN
    #GFB_HVD GFB ON
      GFB.gfb_hail = FOS1.fos_hail AND
      GFB.gfb_cfv  = FOS1.fos_cfv  AND
      GFB.gfb_date = FOS1.fos_date) ON
      GFB.gfb_hail = FOS2.fos_hail AND
      GFB.gfb_cfv  = FOS2.fos_cfv  AND
      GFB.gfb_yrmo = FOS2.fos_yrmo

-- Get PHT Catches
SELECT 
  T.OBFL_HAIL_IN_NO AS hail,
  CAST(T.OBFL_VSL_CFV_NO as INT) AS cfv,
  CONVERT(char(10),IsNull(T.OBFL_DEPARTURE_DT,T.OBFL_OFFLOAD_DT),20) AS \"date\",
  CONVERT(char(7),IsNull(T.OBFL_DEPARTURE_DT,T.OBFL_OFFLOAD_DT),20) AS yrmo,
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
  --IsNull(T.OBFL_DEPARTURE_DT,T.OBFL_OFFLOAD_DT),
  CONVERT(char(10),IsNull(T.OBFL_DEPARTURE_DT,T.OBFL_OFFLOAD_DT),20),
  CONVERT(char(7),IsNull(T.OBFL_DEPARTURE_DT,T.OBFL_OFFLOAD_DT),20),
  IsNull(FE.OBFL_MAJOR_STAT_AREA_CDE,0),
  IsNull(FE.OBFL_MINOR_STAT_AREA_CDE,0),
  MC.SPECIES_CODE

SELECT
  IsNull(TID.TID_gfb,0) AS TID_gfb,
  IsNull(TID.TID_fos,0) AS TID_fos,
  PHT.hail, PHT.cfv,
  CONVERT(smalldatetime,PHT.date) AS [date],
  PHT.major,
  PHT.minor,
  PHT.spp,
  Sum(CONVERT(real,PHT.catKg)) AS catKg
FROM
  #TIDS TID RIGHT OUTER JOIN  -- needs to be a right join to get all catch records for species
  #PHT  PHT ON
    TID.hail = PHT.hail AND
    TID.cfv  = PHT.cfv  AND
    TID.yrmo = PHT.yrmo
    --TID.date = PHT.date
WHERE 
  PHT.date IS NOT NULL
GROUP BY
  IsNull(TID.TID_gfb,0),
  IsNull(TID.TID_fos,0),
  PHT.hail, PHT.cfv,
  CONVERT(smalldatetime,PHT.date),
  PHT.major,
  PHT.minor,
  PHT.spp


-- getData("gfb_pht_catch.sql","GFBioSQL",strSpp="440")

