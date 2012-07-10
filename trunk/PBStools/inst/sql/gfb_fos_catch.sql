-- Get commercial catch from GFFOS and try to match TIDs in GFBioSQL. (2010-06-16)
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
  GFB.gfb_date AS \"date\"
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

-- Get FOS trip catches of strSpp
SELECT * INTO #FOS_CAT
  FROM OPENQUERY(GFSH,
  'SELECT 
  T.TRIP_ID as \"TID_fos\",
  CAST(HS.HAIL_NUMBER as VARCHAR(20)) AS \"hail\", 
  T.VESSEL_REGISTRATION_NUMBER AS \"cfv\",
  TO_CHAR(NVL(T.TRIP_START_DATE,T.TRIP_END_DATE),''YYYY-MM-DD'') as \"date\",
  NVL(A.MAJOR_STAT_AREA_CODE,0) as \"major\",
  NVL(A.MINOR_STAT_AREA_CODE,0) as \"minor\",
  SC.spp as \"spp\",
  NVL(Sum(SC.sppcat),0) as \"catKg\"
FROM 
-- HAIL & SET HS
  (SELECT
  FE.TRIP_ID,
  FE.FISHING_EVENT_ID,
  NVL(HT.HAIL_NUMBER,0) AS HAIL_NUMBER,
  NVL(FE.SET_NUMBER,9999) AS SET_NUMBER,
  NVL(TS.SUCCESS_CODE,0) AS SUCCESS_CODE
  FROM
  (SELECT
    CAST(H.HAIL_NUMBER as VARCHAR(20)) AS HAIL_NUMBER, 
    MIN(H.TRIP_ID) AS TRIP_ID
    FROM 
      GFFOS.GF_HAIL_NUMBER H
    GROUP BY
      CAST(H.HAIL_NUMBER as VARCHAR(20)) ) HT RIGHT OUTER JOIN
  GFFOS.GF_FISHING_EVENT FE ON
  HT.TRIP_ID = FE.TRIP_ID
  LEFT OUTER JOIN GFFOS.GF_FE_TRAWL_SPECS TS
  ON FE.FISHING_EVENT_ID = TS.FISHING_EVENT_ID
  ORDER BY FE.TRIP_ID, FE.FISHING_EVENT_ID) HS INNER JOIN --RIGHT OUTER JOIN

  (GFFOS.GF_TRIP T INNER JOIN 
  (GFFOS.GF_FE_DERIVED_AREA A RIGHT OUTER JOIN
  (GFFOS.GF_FISHING_EVENT FE RIGHT OUTER JOIN 
  -- SC species catch (specified 'strSpp')
  (SELECT 
    C.FISHING_EVENT_ID,
    C.SPECIES_CODE As spp,
    NVL(C.UTILIZATION_CODE,0) AS util,
    SUM(CASE
      WHEN C.CATCH_WEIGHT Is Not Null THEN 
      DECODE(C.WEIGHT_UNIT_CODE,
        ''PND'', C.CATCH_WEIGHT/2.20459,  -- PND (lbs) occurs exclusively
        ''KGM'', C.CATCH_WEIGHT,
        ''MET'', C.CATCH_WEIGHT*1000.,
        ''IPT'', C.CATCH_WEIGHT*2240./2.20459,
        ''STN'', C.CATCH_WEIGHT*2000./2.20459, 0)
      WHEN C.CATCH_WEIGHT Is Null And C.CATCH_COUNT>0 THEN C.CATCH_COUNT*NVL(FW.mnwt,1.) -- if mean weight missing use 1 kg
      ELSE 0 END) AS sppcat
    FROM 
      GFFOS.GF_FE_CATCH C INNER JOIN

-- GFFOS.GF_FE_CATCH mean species weight (kg) from observations where CATCH_COUNT > 1
    GFFOS.MEAN_SPECIES_WEIGHT_VW FW ON

    C.SPECIES_CODE=FW.spp
    WHERE 
      --C.FISHING_EVENT_ID IN (740916,740724,740868,743260,745230,982515) AND -- simply to speed testing
      C.SPECIES_CODE IN ('@sppcode')
    GROUP BY C.FISHING_EVENT_ID, C.SPECIES_CODE, NVL(C.UTILIZATION_CODE,0) ) SC ON
    FE.FISHING_EVENT_ID = SC.FISHING_EVENT_ID ) ON
    FE.FISHING_EVENT_ID = A.FISHING_EVENT_ID  ) ON
    T.TRIP_ID = FE.TRIP_ID ) ON
      T.TRIP_ID = HS.TRIP_ID AND
      FE.FISHING_EVENT_ID = HS.FISHING_EVENT_ID
WHERE SC.sppcat>0
GROUP BY 
  T.TRIP_ID, 
  CAST(HS.HAIL_NUMBER as VARCHAR(20)), 
  T.VESSEL_REGISTRATION_NUMBER,
  TO_CHAR(NVL(T.TRIP_START_DATE,T.TRIP_END_DATE),''YYYY-MM-DD''),
  NVL(A.MAJOR_STAT_AREA_CODE,0),
  NVL(A.MINOR_STAT_AREA_CODE,0),
  SC.spp --, SC.util
ORDER BY
  TO_CHAR(NVL(T.TRIP_START_DATE,T.TRIP_END_DATE),''YYYY-MM-DD''),
  NVL(A.MAJOR_STAT_AREA_CODE,0)
  ')

SELECT
  IsNull(TID.TID_gfb,0) AS TID_gfb,
  FCAT.TID_fos,
  FCAT.hail, FCAT.cfv,
  CONVERT(smalldatetime,FCAT.date) as \"date\",
  FCAT.major,
  FCAT.minor,
  FCAT.spp,
  SUM(CONVERT(real,FCAT.catKg)) AS catKg
FROM
  #TIDS TID RIGHT OUTER JOIN  -- needs to be a right join to get all catch records for species
  #FOS_CAT FCAT ON
    TID.TID_fos = FCAT.TID_fos AND
    TID.hail = FCAT.hail
    --TID.cfv  = FCAT.cfv  AND
    --TID.date = FCAT.date
GROUP BY
  IsNull(TID.TID_gfb,0),
  FCAT.TID_fos,
  FCAT.hail, FCAT.cfv,
  CONVERT(smalldatetime,FCAT.date),
  FCAT.major,
  FCAT.minor,
  FCAT.spp


--getData("gfb_fos_catch.sql","GFBioSQL",strSpp="440")
--select * from #FOS_CAT

