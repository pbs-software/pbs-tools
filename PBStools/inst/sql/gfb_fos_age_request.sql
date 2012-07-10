-- Query GFBioSQL for otoliths taken but not aged
-- Show only those that can be identified by FOS TRIP_ID

SET NOCOUNT ON  -- prevents timeout errors

SELECT
  SC.SAMPLE_ID,
  MIN(SC.STORAGE_CONTAINER_ID) AS IDA,
  MAX(SC.STORAGE_CONTAINER_ID) AS IDB
INTO #Temp_Storage
FROM SAMPLE_COLLECTED SC
GROUP BY SC.SAMPLE_ID

SELECT
  TS.SAMPLE_ID,
  TS.IDA + CASE 
    WHEN TS.IDA=TS.IDB THEN ''
    ELSE '-' + TS.IDB END AS STORAGE_CONTAINER_ID
  INTO #Unique_Storage
  FROM #Temp_Storage TS

-- Get FOS Trip from Hail-Vessel-Date combo
SELECT * INTO #Hail_Vessel_Date
  FROM OPENQUERY(ORADEV,
  'SELECT 
    H.HAIL_NUMBER, 
    T.VESSEL_REGISTRATION_NUMBER, 
    T.TRIP_START_DATE, 
    T.TRIP_ID
  FROM 
    GFFOS.GF_TRIP T INNER JOIN 
    GFFOS.GF_HAIL_NUMBER H ON 
    T.TRIP_ID = H.TRIP_ID
  ')
-- SELECT TOP 10 * FROM #Hail_Vessel_Date

SELECT 
  SA.TRIP_ID AS TID_gfb, 
  CAST(SA.HAIL_IN_NO AS INT) AS hail, 
  V.VESSEL_NAME AS vessel, 
  V.CFV_NUM AS cfv, 
  SA.TRIP_START_DATE AS tdate, 
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
  SA.N_Aged AS Naged, 
  SA.SAMPLE_ID AS SID, 
  SA.SAMPLE_TYPE_CODE AS stype, 
  SA.SAMPLE_DATE AS sdate, 
  SP.SPECIMEN_SERIAL_PREFIX AS prefix, 
  MIN(ISNULL(SP.SPECIMEN_SERIAL_NUMBER,2e9)) AS firstSerial, 
  MAX(ISNULL(SP.SPECIMEN_SERIAL_NUMBER,0))   AS lastSerial, 
  US.STORAGE_CONTAINER_ID AS storageID
INTO #GFB_Otoliths
FROM 
  #Unique_Storage US RIGHT OUTER JOIN 
  ((B21_Samples SA INNER JOIN 
  B22_Specimens SP ON 
    SA.SAMPLE_ID = SP.SAMPLE_ID) INNER JOIN
  C_Vessels V ON 
    SA.VESSEL_ID = V.VESSEL_ID AND 
    SA.SUFFIX = V.SUFFIX) ON
    US.SAMPLE_ID = SA.SAMPLE_ID
WHERE 
  ISNULL(Year(SA.TRIP_START_DATE),0) > 0 AND
  ISNULL(CAST(SA.HAIL_IN_NO AS INT),0) > 0 AND
  ISNULL(V.CFV_NUM,0) > 0 AND
-- To activate the following lines, remove '--'
--  Year(SA.TRIP_START_DATE) IN (2009) AND 
--  SA.TRIP_SUB_TYPE_CODE IN (1,4) AND 
--  (SA.MAJOR_STAT_AREA_CODE IN ('05','06','07') OR 
--    (SA.MAJOR_STAT_AREA_CODE IN ('09') AND SA.MINOR_STAT_AREA_CODE IN ('34') )) AND 
  SA.SPECIES_CODE IN (@sppcode) AND 
  SA.N_Ages_Collected > 0 AND
  (ISNULL(SA.N_Ages_Collected,0) - ISNULL(SA.N_Aged,0)) > 2
GROUP BY SA.TRIP_ID, 
  CAST(SA.HAIL_IN_NO AS INT), 
  V.VESSEL_NAME, 
  V.CFV_NUM, 
  SA.TRIP_START_DATE, 
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
  SA.SAMPLE_DATE, 
  SP.SPECIMEN_SERIAL_PREFIX, 
  US.STORAGE_CONTAINER_ID

SELECT 
  FOS.TRIP_ID AS TID_fos, 
  GFB.*
FROM 
  #Hail_Vessel_Date FOS INNER JOIN 
  #GFB_Otoliths GFB ON 
    FOS.HAIL_NUMBER = GFB.hail AND
    FOS.VESSEL_REGISTRATION_NUMBER = GFB.cfv AND 
    FOS.TRIP_START_DATE = GFB.tdate
ORDER BY
  GFB.tdate


-- getData("gfb_fos_age_request.sql",dbName="GFBioSQL",strSpp="396",type="SQL")

