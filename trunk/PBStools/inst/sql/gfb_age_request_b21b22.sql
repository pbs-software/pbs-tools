-- Query GFBioSQL for otoliths taken but not aged (2011-10-06)
-- Show only those that can be identified by FOS TRIP_ID

SET NOCOUNT ON  -- prevents timeout errors

-- Concatenating values when the number of items is small and known upfront 
-- http://www.projectdmx.com/tsql/rowconcatenate.aspx
SELECT SAMPLE_ID,
  MAX(CASE WHEN seq IN (1) THEN STORAGE_CONTAINER_ID ELSE '' END ) +
  MAX(CASE WHEN seq IN (2) THEN '+' + STORAGE_CONTAINER_ID ELSE '' END) +
  MAX(CASE WHEN seq IN (3) THEN '+' + STORAGE_CONTAINER_ID ELSE '' END) +
  MAX(CASE WHEN seq IN (4) THEN '+' + STORAGE_CONTAINER_ID ELSE '' END) AS TRAYS
INTO #Unique_Storage
FROM (
  SELECT 
    SC1.SAMPLE_ID, SC1.STORAGE_CONTAINER_ID,
    (SELECT COUNT(*) 
      FROM SAMPLE_COLLECTED SC2
      WHERE SC2.SAMPLE_ID = SC1.SAMPLE_ID
        AND SC2.STORAGE_CONTAINER_ID <= SC1.STORAGE_CONTAINER_ID )
  FROM SAMPLE_COLLECTED SC1 ) D ( SAMPLE_ID, STORAGE_CONTAINER_ID, seq )
GROUP BY SAMPLE_ID


SELECT 
  SA.TRIP_ID AS TID_gfb, 
  CASE
    WHEN ISNUMERIC(SUBSTRING(LTRIM(RTRIM(SA.TRIP_COMMENT)),15,6))=1 THEN 
      CAST(SUBSTRING(LTRIM(RTRIM(SA.TRIP_COMMENT)),15,6) AS REAL)
    ELSE NULL END AS TID_fos,
  SA.FISHING_EVENT_ID AS FEID,
  ISNULL(CAST(SA.HAIL_IN_NO AS INT),0) AS hail, 
  V.VESSEL_NAME AS vessel, 
  V.CFV_NUM AS cfv, 
  --ISNULL(SA.TRIP_START_DATE, SA.TRIP_END_DATE) AS tdate, 
  CONVERT(char(10),ISNULL(SA.TRIP_START_DATE, SA.TRIP_END_DATE),20) AS gfb_date,
  CONVERT(char(7),ISNULL(SA.TRIP_START_DATE, SA.TRIP_END_DATE),20) AS gfb_yrmo,
  SA.TRIP_SUB_TYPE_CODE AS ttype, 
  SA.SRF_Area AS srfa, 
  SA.MAJOR_STAT_AREA_CODE AS major, 
  SA.MINOR_STAT_AREA_CODE AS minor, 
  SA.FE_MAJOR_LEVEL_ID AS 'set', 
  SA.CATCH_WEIGHT AS catchKg, 
  SA.SPECIES_CODE AS spp, 
  SA.N_Lengths AS Nlen, 
  SA.N_Weights AS Nwgt, 
  SA.N_Ages_Collected AS Noto, 
  SA.N_Aged AS Naged, -- as reported in the Samples table
  SUM(CASE WHEN SP.AGEING_METHOD_CODE IN (3) THEN 1 ELSE 0 END) as Nbba,               -- count broken & burnt ages
  SUM(IsNull(SP.AGEING_METHOD_CODE,0)) as T_ameth,                                     -- sum the ageing method codes
  SUM(CASE WHEN SP.AGEING_METHOD_CODE BETWEEN 0 AND 16 THEN 1 ELSE 0 END) as N_ameth,  -- count the ageing method codes
  SA.SAMPLE_ID AS SID, 
  SA.SAMPLE_TYPE_CODE AS stype, 
  CONVERT(char(10),ISNULL(SA.SAMPLE_DATE, SA.SAMPLE_DATE),20) AS sdate,
  IsNull(SP.SPECIMEN_SERIAL_PREFIX,'') AS prefix, 
  MIN(ISNULL(SP.SPECIMEN_SERIAL_NUMBER,2e9)) AS firstSerial, 
  MAX(ISNULL(SP.SPECIMEN_SERIAL_NUMBER,0))   AS lastSerial, 
  IsNull(US.TRAYS,'Unk') AS storageID
INTO #GFB_Otoliths
FROM 
  #Unique_Storage US RIGHT OUTER JOIN 
  (((B21_Samples SA INNER JOIN 
  B22_Specimens SP ON 
    SA.SAMPLE_ID = SP.SAMPLE_ID) INNER JOIN
  B05a_Specimen_Collected SCA ON
    SCA.SAMPLE_ID = SA.SAMPLE_ID AND
    SCA.SPECIMEN_ID = SP.SPECIMEN_ID) INNER JOIN
  C_Vessels V ON 
    SA.VESSEL_ID = V.VESSEL_ID AND 
    SA.SUFFIX = V.SUFFIX) ON
    US.SAMPLE_ID = SA.SAMPLE_ID
WHERE 
-- To activate the following lines, remove '--'
--  ISNULL(Year(SA.TRIP_START_DATE),0) > 0 AND
--  ISNULL(CAST(SA.HAIL_IN_NO AS INT),0) > 0 AND
--  ISNULL(V.CFV_NUM,0) > 0 AND
--  Year(SA.TRIP_START_DATE) IN (2009) AND 
--  SA.TRIP_SUB_TYPE_CODE IN (1,4) AND 
--  (SA.MAJOR_STAT_AREA_CODE IN ('05','06','07') OR 
--    (SA.MAJOR_STAT_AREA_CODE IN ('09') AND SA.MINOR_STAT_AREA_CODE IN ('34') )) AND 
  SA.SPECIES_CODE IN (@sppcode) AND
  SCA.COLLECTED_ATTRIBUTE_CODE IN (20) AND  -- otoliths collected
  SA.N_Ages_Collected > 0 --AND
  --(SP.AGEING_METHOD_CODE Is Null OR SP.AGEING_METHOD_CODE IN (3)) -- either not aged or aged via broken-burnt (cannot exclude NAs when others are not 3)
  --(ISNULL(SA.N_Ages_Collected,0) - ISNULL(SA.N_Aged,0)) > 2
GROUP BY 
  SA.TRIP_ID,
  CASE
    WHEN ISNUMERIC(SUBSTRING(LTRIM(RTRIM(SA.TRIP_COMMENT)),15,6))=1 THEN 
      CAST(SUBSTRING(LTRIM(RTRIM(SA.TRIP_COMMENT)),15,6) AS REAL)
    ELSE NULL END,
  SA.FISHING_EVENT_ID,
  ISNULL(CAST(SA.HAIL_IN_NO AS INT),0), 
  V.VESSEL_NAME, 
  V.CFV_NUM, 
  --ISNULL(SA.TRIP_START_DATE, SA.TRIP_END_DATE), 
  CONVERT(char(10),ISNULL(SA.TRIP_START_DATE, SA.TRIP_END_DATE),20),
  CONVERT(char(7),ISNULL(SA.TRIP_START_DATE, SA.TRIP_END_DATE),20),
  SA.TRIP_SUB_TYPE_CODE, 
  SA.SRF_Area, 
  SA.MAJOR_STAT_AREA_CODE, 
  SA.MINOR_STAT_AREA_CODE, 
  SA.FE_MAJOR_LEVEL_ID, 
  SA.CATCH_WEIGHT, 
  SA.SPECIES_CODE, 
  SA.N_Lengths, 
  SA.N_Weights, 
  SA.N_Ages_Collected, 
  SA.N_Aged, 
  SA.SAMPLE_ID, 
  SA.SAMPLE_TYPE_CODE, 
  CONVERT(char(10),ISNULL(SA.SAMPLE_DATE, SA.SAMPLE_DATE),20),
  SP.SPECIMEN_SERIAL_PREFIX, 
  US.TRAYS

-- Get FOS Trip from Hail-Vessel-Date combo
SELECT * INTO #FOS_HVD
  FROM OPENQUERY(GFSH,
  'SELECT 
    HT.HAIL_NUMBER AS fos_hail, 
    T.VESSEL_REGISTRATION_NUMBER AS fos_cfv, 
    TO_CHAR(NVL(T.TRIP_START_DATE,T.TRIP_END_DATE),''YYYY-MM-DD'') AS fos_date,
    T.TRIP_ID AS fos_tid
  FROM 
    GFFOS.GF_TRIP T RIGHT OUTER JOIN 
    (SELECT 
      H.HAIL_NUMBER, 
      MIN(H.TRIP_ID) AS TRIP_ID
    FROM 
      GFFOS.GF_HAIL_NUMBER H
    GROUP BY
      H.HAIL_NUMBER ) HT ON
    T.TRIP_ID = HT.TRIP_ID
  ')

-- Get FOS Trip from Hail-Vessel-YrMo combo
SELECT * INTO #FOS_HVYM
  FROM OPENQUERY(GFSH,
  'SELECT 
    HT.HAIL_NUMBER AS fos_hail, 
    T.VESSEL_REGISTRATION_NUMBER AS fos_cfv, 
    TO_CHAR(NVL(T.TRIP_START_DATE,T.TRIP_END_DATE),''YYYY-MM'') AS fos_yrmo,
    T.TRIP_ID AS fos_tid
  FROM 
    GFFOS.GF_TRIP T RIGHT OUTER JOIN 
    (SELECT 
      H.HAIL_NUMBER, 
      MIN(H.TRIP_ID) AS TRIP_ID
    FROM 
      GFFOS.GF_HAIL_NUMBER H
    GROUP BY
      H.HAIL_NUMBER ) HT ON
    T.TRIP_ID = HT.TRIP_ID
  ')

SELECT 
  GFB.TID_gfb,
  COALESCE(GFB.TID_fos,FOS1.fos_tid,FOS2.fos_tid,0)  AS TID_fos,
  --GFB.hail, GFB.cfv, GFB.gfb_date
  GFB.FEID, GFB.hail, GFB.[set], GFB.vessel, GFB.cfv,
  CONVERT(smalldatetime,GFB.gfb_date) AS tdate,
  GFB.ttype, GFB.srfa, GFB.major, GFB.minor, GFB.spp, GFB.catchKg,
  GFB.SID, GFB.Nlen, GFB.Nwgt, GFB.Noto, GFB.Naged, GFB.Nbba,
  CASE WHEN GFB.N_ameth IN (0) THEN 0 ELSE GFB.T_ameth / GFB.N_ameth END AS ameth, -- mean ageing method (flags samnples with mixtures of ageing method)
  GFB.stype, CONVERT(smalldatetime,GFB.sdate) AS sdate,
  GFB.prefix, GFB.firstSerial, GFB.lastSerial, GFB.storageID
  --INTO #Dump
  FROM
    #FOS_HVYM FOS2 RIGHT OUTER JOIN
    (#FOS_HVD FOS1 RIGHT OUTER JOIN
    #GFB_Otoliths GFB ON
      GFB.hail  = FOS1.fos_hail AND
      GFB.cfv   = FOS1.fos_cfv  AND
      GFB.gfb_date = FOS1.fos_date) ON
      GFB.hail  = FOS2.fos_hail AND
      GFB.cfv   = FOS2.fos_cfv  AND
      GFB.gfb_yrmo = FOS2.fos_yrmo
  ORDER BY
  CONVERT(smalldatetime,GFB.gfb_date)

--select * from #GFB_Otoliths

-- getData("gfb_age_request.sql",dbName="GFBioSQL",strSpp="396")

