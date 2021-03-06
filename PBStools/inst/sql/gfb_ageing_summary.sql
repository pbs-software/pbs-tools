-- Norm's ageing datasheet summary (2016-03-07)
--CREATE PROCEDURE [dbo].[proc_ageing_summary3] @species_code VARCHAR(3),  @start_year SMALLINT, @end_year SMALLINT, @include_aged BIT
--AS

SET NOCOUNT ON

DECLARE @num_aged AS INT, @start_year AS SMALLINT, @end_year AS SMALLINT
--IF @include_aged = 0
--   SET @num_aged = 0
--ELSE
  SET @num_aged   = 1000000
  SET @start_year = 2001
  SET @end_year   = 2003

SELECT YEAR(TRIP_START_DATE) AS Year,
      MAX(ACTIVITY_DESC) AS Activity,
   TRIP_SUB_TYPE_DESC AS TripType,
   CONVERT(VARCHAR(12),
      ISNULL(ISNULL(ISNULL(FE_END_DEPLOYMENT_TIME, 
      FE_BEGIN_DEPLOYMENT_TIME), FE_BEGIN_RETRIEVAL_TIME), 
      FE_END_RETRIEVAL_TIME), 100) AS CaptureDate,
   VESSEL_NAME AS Vessel,
   SUBSTRING(MAJOR_STAT_AREA_NAME, 1, 2) AS Area,
   FE_MAJOR_LEVEL_ID AS [Set],
   S.SAMPLE_ID AS SampleId,
   CONVERT(VARCHAR(12),
      MAX(SAMPLE_DATE), 100) AS SampleDate,
   MAX(SMC.STORAGE_CONTAINER_ID) AS Bin,
   MAX(SC.STORAGE_CONTAINER_ID) AS Tray,
   MAX(SP.SPECIMEN_SERIAL_PREFIX) AS Prefix,
   MIN(SP.SPECIMEN_SERIAL_NUMBER) AS FirstSerial,
   MAX(SP.SPECIMEN_SERIAL_NUMBER) AS LastSerial,
   COUNT(SP.SPECIMEN_ID) AS NumAges,
   SUM(CASE WHEN ISNULL(SP.SPECIMEN_AGE,0) > 0 THEN 1 ELSE 0 END) AS NumAged
FROM TRIP T
   INNER JOIN FISHING_EVENT FE ON
   T.TRIP_ID = FE.TRIP_ID
   INNER JOIN FISHING_EVENT_CATCH FEC ON
   FE.FISHING_EVENT_ID = FEC.FISHING_EVENT_ID
   INNER JOIN CATCH C ON
   FEC.CATCH_ID = C.CATCH_ID
   INNER JOIN CATCH_SAMPLE CS ON
   C.CATCH_ID = CS.CATCH_ID
   INNER JOIN SAMPLE S ON
   CS.SAMPLE_ID = S.SAMPLE_ID
   INNER JOIN SPECIMEN SP ON
   S.SAMPLE_ID = SP.SAMPLE_ID
   INNER JOIN (
      SELECT * FROM SPECIMEN_COLLECTED
      WHERE COLLECTED_ATTRIBUTE_CODE IN (20,21,22,23,24,25,27,28) AND ISNULL(SPECIMEN_COLLECTED_IND, 'Y') = 'Y') SC ON
   SP.SPECIMEN_ID = SC.SPECIMEN_ID
   INNER JOIN VESSEL V ON
   T.VESSEL_ID = V.VESSEL_ID AND T.SUFFIX = V.SUFFIX
   INNER JOIN MAJOR_STAT_AREA M ON
   FE.MAJOR_STAT_AREA_CODE = M.MAJOR_STAT_AREA_CODE
   INNER JOIN TRIP_SUB_TYPE TST ON
   T.TRIP_SUB_TYPE_CODE = TST.TRIP_SUB_TYPE_CODE
   LEFT JOIN (
      SELECT SAMPLE_ID, MAX(STORAGE_CONTAINER_ID) AS STORAGE_CONTAINER_ID
      FROM SAMPLE_COLLECTED
      GROUP BY SAMPLE_ID) SMC ON
   S.SAMPLE_ID = SMC.SAMPLE_ID
   LEFT JOIN TRIP_ACTIVITY TA ON
   T.TRIP_ID = TA.TRIP_ID
   LEFT JOIN ACTIVITY A ON
   TA.ACTIVITY_CODE = A.ACTIVITY_CODE
WHERE SPECIES_CODE = @sppcode AND
   YEAR(ISNULL(ISNULL(ISNULL(FE_END_DEPLOYMENT_TIME, 
      FE_BEGIN_DEPLOYMENT_TIME), FE_BEGIN_RETRIEVAL_TIME), 
      FE_END_RETRIEVAL_TIME)) BETWEEN @start_year AND @end_year
GROUP BY YEAR(TRIP_START_DATE),
   TRIP_SUB_TYPE_DESC,
   ISNULL(ISNULL(ISNULL(FE_END_DEPLOYMENT_TIME, 
      FE_BEGIN_DEPLOYMENT_TIME), FE_BEGIN_RETRIEVAL_TIME), 
      FE_END_RETRIEVAL_TIME),
   VESSEL_NAME,
   MAJOR_STAT_AREA_NAME,
   FE_MAJOR_LEVEL_ID,
   S.SAMPLE_ID
HAVING SUM(ISNULL(SP.SPECIMEN_AGE,0)) <= @num_aged AND
   COUNT(SP.SPECIMEN_ID) > 0
ORDER BY YEAR(TRIP_START_DATE),
   ISNULL(ISNULL(ISNULL(FE_END_DEPLOYMENT_TIME, 
      FE_BEGIN_DEPLOYMENT_TIME), FE_BEGIN_RETRIEVAL_TIME), 
      FE_END_RETRIEVAL_TIME),
   VESSEL_NAME,
   MAJOR_STAT_AREA_NAME,
   FE_MAJOR_LEVEL_ID,
   S.SAMPLE_ID

--GO
-- getData("gfb_ageing_summary.sql","GFBioSQL",strSpp="453")



