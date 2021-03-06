-- Get Norm's bootstrap results
-- Modified 2014-05-08 to deal with R-3.1.0's handling of floats and precision
SET NOCOUNT ON

SELECT
  S.SURVEY_SERIES_ID AS serID,
  TS.SURVEY_ID AS survID,
  BH.BOOT_ID AS bootID,
  T.TRIP_START_DATE AS [date],
  Year(T.TRIP_START_DATE) AS [year],
  BH.RUN_DATE AS runDate,
  BH.RUN_DESC AS runDesc,
  BH.DEFAULT_DOORSPREAD AS defDoor,
  BH.DEFAULT_SPEED AS defSpeed,
  CAST(ROUND(BD.BIOMASS,7) AS NUMERIC(20,7)) AS biomass,
  CAST(ROUND(BD.BOOT_MEAN,7) AS NUMERIC(20,7)) AS bootMean,
  CAST(ROUND(BD.BOOT_MEDIAN,7) AS NUMERIC(20,7)) AS bootMedian,
  CAST(ROUND(BD.BOOT_LOWER_CI,7) AS NUMERIC(20,7)) AS bootLoCI,
  CAST(ROUND(BD.BOOT_UPPER_CI,7) AS NUMERIC(20,7)) AS bootHiCI,
  CAST(ROUND(BD.BOOT_RE,7) AS NUMERIC(20,7)) AS bootRE,
  CAST(ROUND(BD.CATCH_WEIGHT,7) AS NUMERIC(20,7)) AS catchWt,
  BD.NUM_SETS AS numSets,
  BD.NUM_POS_SETS AS numPosSets
FROM 
  B01_TRIP T INNER JOIN 
  TRIP_SURVEY TS ON 
    T.TRIP_ID = TS.TRIP_ID INNER JOIN
  SURVEY S ON
    TS.SURVEY_ID = S.SURVEY_ID INNER JOIN
  BOOT_HEADER BH ON 
    TS.SURVEY_ID = BH.SURVEY_ID INNER JOIN
  BOOT_DETAIL BD ON 
    BH.BOOT_ID = BD.BOOT_ID
WHERE 
  BD.SPECIES_CODE IN (@sppcode)
ORDER BY
  S.SURVEY_SERIES_ID, TS.SURVEY_ID, BH.BOOT_ID

-- getData("gfb_boot.sql","GFBioSQL",strSpp="451")

