-- Match GFB's TRIP_IDs with those in FOS
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
    T.TRIP_START_DATE AS fos_date, 
    T.TRIP_ID AS fos_tid
  FROM 
    GFFOS.GF_TRIP T INNER JOIN 
    GFFOS.GF_HAIL_NUMBER H ON 
    T.TRIP_ID = H.TRIP_ID
  WHERE
    T.TRIP_START_DATE Is Not NULL
  ')

-- Get joint TIDs
SELECT
  GFB.gfb_tid AS TID_gfb,
  FOS.fos_tid AS TID_fos
  INTO #TIDS
  FROM
    #GFB_HVD GFB INNER JOIN
    #FOS_HVD FOS ON
      GFB.gfb_hail = FOS.fos_hail AND
      GFB.gfb_cfv  = FOS.fos_cfv  AND
      GFB.gfb_date = FOS.fos_date

--SELECT TOP 20 * FROM #TIDS


