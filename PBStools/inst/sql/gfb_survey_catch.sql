-- Originally a PROCEDURE 'proc_species_catch_matrix' by Norm Olsen
-- @sid = GFBio Survey_Id (from SURVEY table); @mds = mean doorspread (meters);
-- @msog = mean speed over ground (kmh)
-- CREATE PROCEDURE [dbo].[proc_species_catch_matrix] @sid INTEGER, @spp VARCHAR(3), @mds FLOAT, @msog FLOAT
-- AS
   SET NOCOUNT ON
   DECLARE @doors FLOAT, @speed FLOAT, @scon AS FLOAT --, @surveyid INTEGER
   SET @scon = 16.66667 -- conversion from km/h to m/min
   --SET @surveyid = @surveyidval
   SET @doors    = @doorsval
   SET @speed    = @speedval

   -- Convert speed over ground to m/min from km/h
   SET @speed = @speed * @scon

   -- Fishing event attributes for good (USABILITY_CODE = 1,2,6) sets:
   SELECT S.SURVEY_ID,
      TRS.TRIP_ID,
      YEAR(T.TRIP_START_DATE) AS [YEAR],
      FE.FISHING_EVENT_ID,
      FEG.GROUPING_CODE,
      ISNULL(DATEDIFF(MI,FE_BEGIN_BOTTOM_CONTACT_TIME,
         FE_END_BOTTOM_CONTACT_TIME),
         DATEDIFF(MI,FE_END_DEPLOYMENT_TIME,
         FE_BEGIN_RETRIEVAL_TIME)) AS DURATION_MIN,
      NULLIF(FE_DISTANCE_TRAVELLED,0) * 1000.0 AS TOW_LENGTH_M, -- Convert to meters
      ISNULL(NULLIF(TS.TRLSP_DOORSPREAD,0),@doors) AS DOORSPREAD_M,
      ISNULL(NULLIF(TS.TRLSP_SPEED,0) * @scon, @speed) AS SPEED_MPM
   INTO #SETS
   FROM SURVEY S
      INNER JOIN TRIP_SURVEY TRS ON
      S.SURVEY_ID = TRS.SURVEY_ID
      INNER JOIN TRIP T ON
      TRS.TRIP_ID = T.TRIP_ID
      INNER JOIN FISHING_EVENT FE ON
      T.TRIP_ID = FE.TRIP_ID
      LEFT JOIN TRAWL_SPECS TS ON
      FE.FISHING_EVENT_ID = TS.FISHING_EVENT_ID
      INNER JOIN FISHING_EVENT_GROUPING FEG ON
      FE.FISHING_EVENT_ID = FEG.FISHING_EVENT_ID
      INNER JOIN SURVEY_GROUPING SG ON
      S.SURVEY_ID = SG.SURVEY_ID AND
      SG.GROUPING_CODE = FEG.GROUPING_CODE
   WHERE S.SURVEY_ID IN (@surveyid) AND ISNULL(USABILITY_CODE,1) IN (1, 2, 6)

   -- Cross-product:
   SELECT FISHING_EVENT_ID, @sppcode AS SPECIES_CODE
   INTO #SETS_FISH
   FROM #SETS

   -- STEP 2: Get the catch for the sets/fish cross product
   --         Zero-weight sets are included
   SELECT SF.FISHING_EVENT_ID,
      SF.SPECIES_CODE,
      ISNULL(CATCH_WEIGHT,0) AS CATCH_WEIGHT,
      ISNULL(CATCH_COUNT,0) AS CATCH_COUNT
   INTO #CATCH
   FROM #SETS_FISH SF
      LEFT OUTER JOIN (
         SELECT S.FISHING_EVENT_ID,
            C.SPECIES_CODE,
            CATCH_WEIGHT,
            CATCH_COUNT
         FROM #SETS S
            INNER JOIN FISHING_EVENT_CATCH FEC ON
            S.FISHING_EVENT_ID = FEC.FISHING_EVENT_ID
            INNER JOIN CATCH C ON
            FEC.CATCH_ID = C.CATCH_ID) C
      ON SF.FISHING_EVENT_ID = C.FISHING_EVENT_ID AND
      SF.SPECIES_CODE = C.SPECIES_CODE

   -- STEP 3 (final): Put it all together

   -- Total time to execute is about 40 sec
   SELECT S.SURVEY_ID,
      S.TRIP_ID,
      S.[YEAR],
      S.FISHING_EVENT_ID,
      S.GROUPING_CODE,
      S.DURATION_MIN,
      S.DOORSPREAD_M,
      S.SPEED_MPM,
      C.CATCH_WEIGHT,
      ISNULL(C.CATCH_WEIGHT / (S.TOW_LENGTH_M * S.DOORSPREAD_M),
         ISNULL(C.CATCH_WEIGHT / 
         (S.DOORSPREAD_M * (S.SPEED_MPM * S.DURATION_MIN)), 0)) AS DENSITY_KGPM2,
      C.CATCH_COUNT,
      C.CATCH_COUNT / 
         (S.DOORSPREAD_M * (S.SPEED_MPM * S.DURATION_MIN)) AS DENSITY_PCPM2
   FROM #SETS S
      INNER JOIN #CATCH C ON
      S.FISHING_EVENT_ID = C.FISHING_EVENT_ID

-- getData("gfb_survey_catch.sql","GFBioSQL",strSpp="396",surveyid=3)

