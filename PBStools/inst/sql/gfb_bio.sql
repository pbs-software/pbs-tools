-- Get specimen biological data from GFBioSQL (last revised 2025-04-23)
-- Note: Not all users can access the Schnute overlay for table names.

SET NOCOUNT ON

---------------------------SPECIMEN QUERIES---------------------------

--------------------------------------------------
-- EXISTENCE_CODE includes genetic results for REBS, among assorted micellaneous observations
-- NOTE: Any one specimen can be tagged with multiple EXISTENCE_CODEs
-- Relevant genetic codes:
-- 16 = ROUGHEYE/BLACKSPOTTED - ROUGHEYE (USER GUESS)
-- 17 = ROUGHEYE/BLACKSPOTTED - BLACKSPOTTED (USER GUESS)
-- 18 = SMA6 & MTSNP COI_496 RESOLVED GENETIC ID: BLACKSPOTTED ROCKFISH
-- 19 = SMA6 & MTSNP COI_496 RESOLVED GENETIC ID: ROUGHEYE ROCKFISH
-- 20 = SMA6 & MTSNP COI_496 RESOLVED GENETIC ID: HYBRID (RE/BS) F1+ (first generation or later)
-- 21 = SMA6 & MTSNP COI_496 RESOLVED GENETIC ID: HYBRID (RE/BS) F2+ (second generation or later)
-- 22 = SMA6 & MTSNP COI_496 RESOLVED GENETIC ID: FAILURE
-- 23 = SMA6: BLACKSPOTTED/BLACKSPOTTED - NUCLEAR DNA MARKER USMA6 ALLELES ARE BLACKSPOTTED ROCKFISH
-- 24 = SMA6: ROUGHEYE/ROUGHEYE - NUCLEAR DNA MARKER USMA6 ALLELES ARE ROUGHEYE ROCKFISH
-- 25 = SMA6: BLACKSPOTTED/ROUGHEYE - NUCLEAR DNA MARKER USMA6 ARE BLACKSPOTTED ROCKFISH AND ROUGHEYE ROCKFISH
-- 26 = SMA6: NO AMPLIFICATION OF LOCUS (FAILURE)
-- 27 = MTSNP: BLACKSPOTTED - MITOCHONDRIAL DNA SINGLE NUCLEOTIDE PLYOMORPHISM COI_496_C: BLACKSPOTTED ROCKFISH
-- 28 = MTSNP: ROUGHEYE - MITOCHONDRIAL DNA SINGLE NUCLEOTIDE POLYMORPHISM COI_496_T: ROUGHEYE ROCKFISH
-- 29 = MTSNP: NO AMPLIFICATION OF LOCUS (FAILURE)
-- 31 = ROUGHEYE/BLACKSPOTTED - HYBRID (USER GUESS)
--------------------------------------------------
SELECT --TOP 40
  SAMPLE_ID,
  SPECIMEN_ID,
  MAX(CASE WHEN seq IN (1) THEN CAST(EXISTENCE_ATTRIBUTE_CODE AS varchar) ELSE '' END ) +
  MAX(CASE WHEN seq IN (2) THEN '|' + CAST(EXISTENCE_ATTRIBUTE_CODE AS varchar) ELSE '' END) +
  MAX(CASE WHEN seq IN (3) THEN '|' + CAST(EXISTENCE_ATTRIBUTE_CODE AS varchar) ELSE '' END) +
  MAX(CASE WHEN seq IN (4) THEN '|' + CAST(EXISTENCE_ATTRIBUTE_CODE AS varchar) ELSE '' END) +
  MAX(CASE WHEN seq IN (5) THEN '|' + CAST(EXISTENCE_ATTRIBUTE_CODE AS varchar) ELSE '' END) +
  MAX(CASE WHEN seq IN (6) THEN '|' + CAST(EXISTENCE_ATTRIBUTE_CODE AS varchar) ELSE '' END) +
  MAX(CASE WHEN seq IN (7) THEN '|' + CAST(EXISTENCE_ATTRIBUTE_CODE AS varchar) ELSE '' END) +
  MAX(CASE WHEN seq IN (8) THEN '|' + CAST(EXISTENCE_ATTRIBUTE_CODE AS varchar) ELSE '' END) AS EXISTENCE_CODE
INTO #GenSpp
FROM
  (SELECT 
      SE1.SAMPLE_ID, SE1.SPECIMEN_ID, SE1.EXISTENCE_ATTRIBUTE_CODE,
    (SELECT COUNT(*) 
      FROM SPECIMEN_EXISTENCE SE2
      WHERE SE2.SAMPLE_ID = SE1.SAMPLE_ID AND SE2.SPECIMEN_ID = SE1.SPECIMEN_ID
        AND SE2.EXISTENCE_ATTRIBUTE_CODE <= SE1.EXISTENCE_ATTRIBUTE_CODE )
  FROM SPECIMEN_EXISTENCE SE1 ) D ( SAMPLE_ID, SPECIMEN_ID, EXISTENCE_ATTRIBUTE_CODE, seq )
GROUP BY SAMPLE_ID, SPECIMEN_ID

-- FOS TIDs that are associated with GFB TIDs (there can be more than 1) (RH 190812)
-- Need to collapse FOSTID because there can be more than one (extreme: 8 FOS TIDs for GFB TID = 84531)
-- Concatenating values when the number of items is small and known upfront 
-- http://www.projectdmx.com/tsql/rowconcatenate.aspx
SELECT TRIP_ID,
  MAX(CASE WHEN seq IN (1) THEN CAST(FOS_TRIP_ID AS varchar) ELSE '' END ) +
  MAX(CASE WHEN seq IN (2) THEN '|' + CAST(FOS_TRIP_ID AS varchar) ELSE '' END) +
  MAX(CASE WHEN seq IN (3) THEN '|' + CAST(FOS_TRIP_ID AS varchar) ELSE '' END) +
  MAX(CASE WHEN seq IN (4) THEN '|' + CAST(FOS_TRIP_ID AS varchar) ELSE '' END) +
  MAX(CASE WHEN seq IN (5) THEN '|' + CAST(FOS_TRIP_ID AS varchar) ELSE '' END) +
  MAX(CASE WHEN seq IN (6) THEN '|' + CAST(FOS_TRIP_ID AS varchar) ELSE '' END) +
  MAX(CASE WHEN seq IN (7) THEN '|' + CAST(FOS_TRIP_ID AS varchar) ELSE '' END) +
  MAX(CASE WHEN seq IN (8) THEN '|' + CAST(FOS_TRIP_ID AS varchar) ELSE '' END) AS FOS_TRIP_ID
INTO #FOSTID
FROM
  (SELECT 
    FT1.TRIP_ID, FT1.FOS_TRIP_ID,
    (SELECT COUNT(*) 
      FROM FOS_TRIP FT2
      WHERE FT2.TRIP_ID = FT1.TRIP_ID
        AND FT2.FOS_TRIP_ID <= FT1.FOS_TRIP_ID )
  FROM FOS_TRIP FT1 ) D ( TRIP_ID, FOS_TRIP_ID, seq )
GROUP BY TRIP_ID

-- Collect values from B01 to B05, possibly modified by genetic species (GS) update
SELECT -- TOP 1000
  TAC.ACTIVITY_CODE,               -- 190624: Trip activity code might provide a clue if survey ID is missing (MS)
  B01.VESSEL_ID,
  B01.TRIP_ID,
  COALESCE(FT.FOS_TRIP_ID, '') AS FOS_TRIP_ID,  -- 190717: Trip ID in GFFOS to link GFBioSQL and GFFOS (NO).
  B02.FISHING_EVENT_ID,
  B03.CATCH_ID,
  B04.SAMPLE_ID,
  B05.SPECIMEN_ID,
  B02.GROUPING_CODE,
  B02.REASON_CODE,
  COALESCE(B02.BLOCK_DESIGNATION, '') AS BLOCK_DESIGNATION,
  B01.HAIL_IN_NO,
  B02.FE_MAJOR_LEVEL_ID,
  B02.FE_SUB_LEVEL_ID,             -- 180914: Only populated for gear types 2 (trap) and 4 (handline)
  B02.FE_MINOR_LEVEL_ID,           -- 180914: Only populated for spp 455 in gear 2 (sablefish in trap)
  B01.TRIP_SUB_TYPE_CODE,
  B04.SAMPLE_TYPE_CODE,
  B01.TRIP_START_DATE,
  B01.TRIP_END_DATE,
  B04.SAMPLE_DATE,
  B05.SPECIMEN_SEX_CODE,
  B05.MATURITY_CODE,
  B05.SPECIMEN_AGE,
  B05.AGEING_METHOD_CODE,
  B02.GEAR_CODE,
  B02.FE_DISTANCE_TRAVELLED,
  B02.MAJOR_STAT_AREA_CODE,
  B02.MINOR_STAT_AREA_CODE,
  B02.LOCALITY_CODE,
  B02.DFO_STAT_AREA_CODE,
  B02.DFO_STAT_SUBAREA_CODE,
 'Best_Long' = CASE
    WHEN B02.FE_START_LONGITUDE_DEGREE > 0 AND B02.FE_START_LONGITUDE_DEGREE IS NOT NULL AND 
      B02.FE_START_LONGITUDE_MINUTE IS NOT NULL AND B02.FE_END_LONGITUDE_DEGREE > 0 AND 
      B02.FE_END_LONGITUDE_DEGREE IS NOT NULL AND B02.FE_END_LONGITUDE_MINUTE IS NOT NULL THEN
      ((B02.FE_START_LONGITUDE_DEGREE + (B02.FE_START_LONGITUDE_MINUTE / 60.0)) + (B02.FE_END_LONGITUDE_DEGREE + (B02.FE_END_LONGITUDE_MINUTE / 60.0))) / 2.0
    WHEN B02.FE_START_LONGITUDE_DEGREE > 0 AND B02.FE_START_LONGITUDE_DEGREE IS NOT NULL AND B02.FE_START_LONGITUDE_MINUTE IS NOT NULL THEN
      B02.FE_START_LONGITUDE_DEGREE + (B02.FE_START_LONGITUDE_MINUTE / 60.0)
    WHEN B02.FE_END_LONGITUDE_DEGREE > 0 AND B02.FE_END_LONGITUDE_DEGREE IS NOT NULL AND B02.FE_END_LONGITUDE_MINUTE IS NOT NULL THEN
      B02.FE_END_LONGITUDE_DEGREE + (B02.FE_END_LONGITUDE_MINUTE / 60.0)
    ELSE NULL END,
  'Best_Lat' = CASE
    WHEN B02.FE_START_LATTITUDE_DEGREE > 0 AND B02.FE_START_LATTITUDE_DEGREE IS NOT NULL AND 
      B02.FE_START_LATTITUDE_MINUTE IS NOT NULL AND B02.FE_END_LATTITUDE_DEGREE > 0 AND 
      B02.FE_END_LATTITUDE_DEGREE IS NOT NULL AND B02.FE_END_LATTITUDE_MINUTE IS NOT NULL THEN
      ((B02.FE_START_LATTITUDE_DEGREE + (B02.FE_START_LATTITUDE_MINUTE / 60.0)) + (B02.FE_END_LATTITUDE_DEGREE + (B02.FE_END_LATTITUDE_MINUTE / 60.0))) / 2.0
    WHEN B02.FE_START_LATTITUDE_DEGREE > 0 AND B02.FE_START_LATTITUDE_DEGREE IS NOT NULL AND B02.FE_START_LATTITUDE_MINUTE IS NOT NULL THEN
      B02.FE_START_LATTITUDE_DEGREE + (B02.FE_START_LATTITUDE_MINUTE / 60.0)
    WHEN B02.FE_END_LATTITUDE_DEGREE > 0 AND B02.FE_END_LATTITUDE_DEGREE IS NOT NULL AND B02.FE_END_LATTITUDE_MINUTE IS NOT NULL THEN
      B02.FE_END_LATTITUDE_DEGREE + (B02.FE_END_LATTITUDE_MINUTE / 60.0)
    ELSE NULL END,
  'Best_Depth' = CASE
    WHEN B02.GEAR_CODE IN (0,1,2,5) THEN
      COALESCE(B02.FE_BEGINNING_BOTTOM_DEPTH, B02.FE_END_BOTTOM_DEPTH, B02.FE_MODAL_BOTTOM_DEPTH, B02.FE_MIN_BOTTOM_DEPTH, B02.FE_MAX_BOTTOM_DEPTH,
        B02.FE_BEGINNING_GEAR_DEPTH, B02.FE_END_GEAR_DEPTH, B02.FE_MODAL_GEAR_DEPTH, B02.FE_MIN_GEAR_DEPTH, B02.FE_MAX_GEAR_DEPTH, 
        B02.FE_BEGIN_CAPTURE_DEPTH, B02.FE_END_CAPTURE_DEPTH, B02.FE_MODAL_CAPTURE_DEPTH, B02.FE_MIN_CAPTURE_DEPTH, B02.FE_MAX_CAPTURE_DEPTH,
        B02.FE_BEGIN_TARGET_DEPTH, B02.FE_END_TARGET_DEPTH, B02.FE_MODAL_TARGET_DEPTH, B02.FE_MIN_TARGET_DEPTH, B02.FE_MAX_TARGET_DEPTH)
    WHEN B02.GEAR_CODE IN (6) THEN
      COALESCE(B02.FE_BEGINNING_GEAR_DEPTH, B02.FE_END_GEAR_DEPTH, B02.FE_MODAL_GEAR_DEPTH, B02.FE_MIN_GEAR_DEPTH, B02.FE_MAX_GEAR_DEPTH,
        B02.FE_BEGIN_CAPTURE_DEPTH, B02.FE_END_CAPTURE_DEPTH, B02.FE_MODAL_CAPTURE_DEPTH, B02.FE_MIN_CAPTURE_DEPTH, B02.FE_MAX_CAPTURE_DEPTH, 
        B02.FE_BEGIN_TARGET_DEPTH, B02.FE_END_TARGET_DEPTH, B02.FE_MODAL_TARGET_DEPTH, B02.FE_MIN_TARGET_DEPTH, B02.FE_MAX_TARGET_DEPTH,
        B02.FE_BEGINNING_BOTTOM_DEPTH, B02.FE_END_BOTTOM_DEPTH, B02.FE_MODAL_BOTTOM_DEPTH, B02.FE_MIN_BOTTOM_DEPTH, B02.FE_MAX_BOTTOM_DEPTH)
    ELSE 0 END,
  B02.FE_BOTTOM_WATER_TEMPERATURE,
  --COALESCE(GS.GENETIC_CODE, B03.SPECIES_CODE) AS SPECIES_CODE,
  B03.SPECIES_CODE AS SPECIES_CODE_OBS,
  COALESCE(GS.EXISTENCE_CODE, '') AS EXIST_CODE,  -- 200107: RH added existence attribute code for REBS genetics
  --ISNULL(GS.GENETICS,0) AS GENETICS,
  B03.SPECIES_CATEGORY_CODE,
  B04.SAMPLE_SOURCE_CODE,          -- 190626: sample source code used in conjunction with species category code to determine unsorted and keepers ( Forrest et. al (2015)
  B03.CATCH_VERIFICATION_CODE,
  B03.CATCH_WEIGHT,
  B03.CATCH_COUNT
INTO #B01B05
FROM 
  TRIP_ACTIVITY TAC RIGHT OUTER JOIN
  #FOSTID FT RIGHT OUTER JOIN
  B01_TRIP B01 INNER JOIN
  B02_FISHING_EVENT B02 INNER JOIN
  B02L3_Link_Fishing_Event_Catch L1 INNER JOIN
  B03_CATCH B03 INNER JOIN
  B03L4_Link_Catch_Sample L2 INNER JOIN
  B04_SAMPLE B04 INNER JOIN
  B05_SPECIMEN B05 LEFT OUTER JOIN
  #GenSpp GS ON
    GS.SAMPLE_ID = B05.SAMPLE_ID AND
    GS.SPECIMEN_ID = B05.SPECIMEN_ID ON
    B05.SAMPLE_ID = B04.SAMPLE_ID ON
    B04.SAMPLE_ID = L2.SAMPLE_ID ON
    L2.CATCH_ID = B03.CATCH_ID ON
    B03.CATCH_ID = L1.CATCH_ID ON
    L1.FISHING_EVENT_ID = B02.FISHING_EVENT_ID ON
    B02.TRIP_ID = B01.TRIP_ID ON
    B01.TRIP_ID = FT.TRIP_ID ON
    B01.TRIP_ID = TAC.TRIP_ID
WHERE 
  --COALESCE(GS.GENETIC_CODE, B03.SPECIES_CODE) IN (@sppcode)
  B03.SPECIES_CODE IN (@sppcode)
  AND B02.MAJOR_STAT_AREA_CODE IN (1,3,4,5,6,7,8,9,11,71,72,73,74,75,76,77) -- change back to '@major' after revised 'getData' makes its way into package PBStools
  --AND B02.FE_SUB_LEVEL_ID IS NULL  -- FISHING_EVENT_ID REPEATED MANY TIMES FOR HOOKS AND TRAPS IF NOT NULL (STUPID IDEA)

-- Gather earliest GROUPING_CODE by FISHING_EVENT_ID (changed 180129 to match gfb_catch_records.sql)
-- This is likely to match the original series SSID
-- Not ideal because some fishing events are used in numerous re-stratifications. (e.g., FEID=886052)
SELECT *
INTO #FishEventGroup
FROM
  (SELECT *,
    ROW_NUMBER() OVER (PARTITION BY LEG.FISHING_EVENT_ID ORDER BY LEG.GROUPING_CODE ASC) AS RN
  FROM FISHING_EVENT_GROUPING LEG) AS FEG
WHERE FEG.RN = 1
ORDER BY FEG.FISHING_EVENT_ID

-- Add in missing Gouping Codes to main skeleton table B01B05
UPDATE #B01B05
SET GROUPING_CODE = COALESCE( #B01B05.GROUPING_CODE,
   (SELECT #FishEventGroup.GROUPING_CODE 
    FROM #FishEventGroup
    WHERE #FishEventGroup. FISHING_EVENT_ID = #B01B05. FISHING_EVENT_ID) )

--SELECT * FROM #B01B05


-- Collect specimen IDs for ONLY strSpp (to speed up some later queries)
SELECT --TOP 40
  BB.TRIP_ID,
  BB.FISHING_EVENT_ID,
  BB.CATCH_ID,
  BB.SAMPLE_ID,
  BB.SPECIMEN_ID
INTO #onlySPID
FROM
  #B01B05 BB

--SELECT * FROM #onlySPID

-- Collect fishing event IDs for ONLY strSpp specimens (to speed up some later queries)
SELECT --TOP 40
  CC.TRIP_ID,
  CC.FISHING_EVENT_ID
INTO #onlyFEID
FROM #onlySPID CC
GROUP BY
  CC.TRIP_ID,
  CC.FISHING_EVENT_ID

--SELECT * FROM #onlyFEID


-- Collect trip IDs for ONLY strSpp specimens (to speed up some later queries)
SELECT --TOP 40
  DD.TRIP_ID
INTO #onlyTID
FROM #onlyFEID DD
GROUP BY
  DD.TRIP_ID

--SELECT * FROM #onlyTID

-- Get 'collected attribute' of specimens
SELECT --TOP 40
  SC.SAMPLE_ID,
  SC.SPECIMEN_ID,
  'notavail'  = AVG (CASE WHEN SC.NOT_AVAILABLE_REASON_CODE IS NOT NULL THEN SC.NOT_AVAILABLE_REASON_CODE ELSE ISNULL(SC.NOT_AVAILABLE_REASON_CODE,0) END), -- not different over third key field COLLECTED_ATTRIBUTE_CODE
  'otoliths'  = SUM(CASE WHEN SC.COLLECTED_ATTRIBUTE_CODE IN (20) THEN 1 ELSE 0 END),
  'scales'    = SUM(CASE WHEN SC.COLLECTED_ATTRIBUTE_CODE IN (21) THEN 1 ELSE 0 END),
  'fins'      = SUM(CASE WHEN SC.COLLECTED_ATTRIBUTE_CODE IN (22,23,27,28) THEN 1 ELSE 0 END),
  'genetics'  = SUM(CASE WHEN SC.COLLECTED_ATTRIBUTE_CODE IN (4,6,30) THEN 1 ELSE 0 END),
  'gonads'    = SUM(CASE WHEN SC.COLLECTED_ATTRIBUTE_CODE IN (1) THEN 1 ELSE 0 END),
  'stomachs'  = SUM(CASE WHEN SC.COLLECTED_ATTRIBUTE_CODE IN (2) THEN 1 ELSE 0 END)
INTO #SpecAtts
FROM 
  B05a_Specimen_Collected SC INNER JOIN
  #onlySPID SPID ON
  SPID.SAMPLE_ID = SC.SAMPLE_ID AND
  SPID.SPECIMEN_ID = SC.SPECIMEN_ID
GROUP BY
  SC.SAMPLE_ID, SC.SPECIMEN_ID

--SELECT * FROM #SpecAtts

-- Get the appropriate factor to convert lengths to cm and weight to kg
SELECT --TOP 40
  -- Need three key fields
  SM.SAMPLE_ID,
  SM.SPECIMEN_ID,
  SM.MORPHOMETRICS_ATTRIBUTE_CODE,
  FACTOR = CASE
    WHEN SM.MORPHOMETRICS_UNIT_CODE IN (1,7,14) THEN 1.0  -- centimetre
    WHEN SM.MORPHOMETRICS_UNIT_CODE IN (2,13) THEN 0.1    -- millimetre
    WHEN SM.MORPHOMETRICS_UNIT_CODE IN (11) THEN 0.1      -- decimetre
    WHEN SM.MORPHOMETRICS_UNIT_CODE IN (4) THEN 1.0       -- kilogram
    WHEN SM.MORPHOMETRICS_UNIT_CODE IN (3) THEN 0.001     -- gram
    WHEN SM.MORPHOMETRICS_UNIT_CODE IN (8) THEN 0.000001  -- milligram
    WHEN SM.MORPHOMETRICS_UNIT_CODE IN (10) THEN 0.0001   -- decigram
    WHEN SM.MORPHOMETRICS_UNIT_CODE IN (9) THEN 0.01      -- dekagram
    WHEN SM.MORPHOMETRICS_UNIT_CODE IN (12) THEN 0.1      -- hectogram
    WHEN SM.MORPHOMETRICS_UNIT_CODE IN (5) THEN 0.453592  -- pound
    WHEN SM.MORPHOMETRICS_UNIT_CODE IN (6) THEN 0.0283495 -- ounce
    ELSE 0 END
INTO #FactorMorpho
FROM 
  B05d_Specimen_Morphometrics SM INNER JOIN
  #onlySPID AS SPID ON
    SPID.SAMPLE_ID = SM.SAMPLE_ID AND
    SPID.SPECIMEN_ID = SM.SPECIMEN_ID
WHERE
  SM.MORPHOMETRICS_ATTRIBUTE_CODE IN (1,2,4,6,10)

--SELECT * FROM #FactorMorpho

-- Deteremine the Best Length and Round Weight for Specimens (causing errors)
SELECT --TOP 40
  SM.SAMPLE_ID,
  SM.SPECIMEN_ID,
  'Best_Length' = SUM(COALESCE(
    (CASE WHEN SM.MORPHOMETRICS_ATTRIBUTE_CODE IN (4) THEN SM.SPECIMEN_MORPHOMETRICS_VALUE * FM.FACTOR ELSE NULL END),  -- total length
    (CASE WHEN SM.MORPHOMETRICS_ATTRIBUTE_CODE IN (2) THEN SM.SPECIMEN_MORPHOMETRICS_VALUE * FM.FACTOR ELSE NULL END),  -- standard length
    (CASE WHEN SM.MORPHOMETRICS_ATTRIBUTE_CODE IN (1) THEN SM.SPECIMEN_MORPHOMETRICS_VALUE * FM.FACTOR ELSE NULL END),  -- fork length
    (CASE WHEN SM.MORPHOMETRICS_ATTRIBUTE_CODE IN (6) THEN SM.SPECIMEN_MORPHOMETRICS_VALUE * FM.FACTOR ELSE NULL END),0)), -- third dorsal length
  'Round_Weight'  = SUM(COALESCE(
    (CASE WHEN SM.MORPHOMETRICS_ATTRIBUTE_CODE IN (10) THEN SM.SPECIMEN_MORPHOMETRICS_VALUE * FM.FACTOR ELSE NULL END),0))  -- ***** this line was originally faulty
INTO #BestMorpho
FROM 
  B05d_Specimen_Morphometrics SM  INNER JOIN
  #FactorMorpho FM ON
    SM.SAMPLE_ID = FM.SAMPLE_ID AND
    SM.SPECIMEN_ID = FM.SPECIMEN_ID AND
    SM.MORPHOMETRICS_ATTRIBUTE_CODE = FM.MORPHOMETRICS_ATTRIBUTE_CODE
GROUP BY
  SM.SAMPLE_ID,
  SM.SPECIMEN_ID

--SELECT * FROM #BestMorpho

-- Trawl Specs (returns same # records as B02_FISHING_EVENT)
SELECT
  B01.TRIP_ID,
  B02.FISHING_EVENT_ID,
  'DISTANCE'  = CASE
    WHEN B02.FE_DISTANCE_TRAVELLED IS NOT NULL THEN B02.FE_DISTANCE_TRAVELLED
    WHEN IsNull(B01.TRIP_SUB_TYPE_CODE,0) IN (1,4) THEN 20 ELSE 1.75 END,
  'DOORSPREAD'  = IsNull(B02e.TRLSP_DOORSPREAD,64.4),       -- TRAWL_SPECS
  'USABILITY'   = IsNull(B02e.USABILITY_CODE,0)             -- TRAWL_SPECS
INTO #TSpecs
FROM
  #onlyFEID FE INNER JOIN
  (B01_TRIP B01 INNER JOIN
  (B02_FISHING_EVENT B02 LEFT OUTER JOIN
  B02e_Trawl_Specs B02e ON
  B02.FISHING_EVENT_ID = B02e.FISHING_EVENT_ID) ON
  B01.TRIP_ID = B02.TRIP_ID) ON
  FE.TRIP_ID = B01.TRIP_ID AND
  FE.FISHING_EVENT_ID = B02.FISHING_EVENT_ID
WHERE B02.FE_SUB_LEVEL_ID IS NULL  -- FISHING_EVENT_ID REPEATED MANY TIMES FOR HOOKS AND TRAPS IF NOT NULL (STUPID IDEA)

--SELECT * FROM #TSpecs

-- =====ASSOCIATE TRIPS WITH SURVEYS=====

-- Identify surveys by original index only (RH 181219) - some restrats may need to be added over time
SELECT
  S.SURVEY_SERIES_ID,
  S.SURVEY_ID
INTO #ORIGINAL_SURVEYS
FROM SURVEY S
WHERE
  (S.ORIGINAL_IND='Y' AND S.SURVEY_ID NOT IN (79)) -- Exclude original stratification for 2006 WCHG Synoptic (use SVID 123)
  OR
  (S.ORIGINAL_IND='N' AND S.SURVEY_SERIES_ID IN (21) AND S.SURVEY_ID BETWEEN 91 AND 104) -- Special case for GIG Historical
ORDER BY
  S.SURVEY_SERIES_ID, S.SURVEY_ID

-- Merge tables to get TRIP_ID, SURVEY_ID, and SURVEY_SERIES_ID (RH 181219)
SELECT
  TS.TRIP_ID,
  CASE
    WHEN OS.SURVEY_SERIES_ID IS NULL THEN 999
    WHEN OS.SURVEY_SERIES_ID IN (6,7) THEN 670          -- Shrimp trawl surveys
    WHEN OS.SURVEY_SERIES_ID IN (35,41,42,43) THEN 350  -- Sablefish surveys
    WHEN OS.SURVEY_SERIES_ID BETWEEN 82 AND 87 THEN 820 -- Jig surveys
    WHEN OS.SURVEY_ID IN (130) THEN 40                  -- HBLL South survey
    WHEN OS.SURVEY_ID IN (131) THEN 39                  -- HBLL North survey
    WHEN OS.SURVEY_SERIES_ID IN (10,21) THEN 21         -- GIG historical
    ELSE OS.SURVEY_SERIES_ID END AS SURVEY_SERIES_ID,
  MAX(OS.SURVEY_ID) AS SURVEY_ID
INTO #TripSurvSer
FROM 
  #onlyTID T INNER JOIN
  --TRIP T INNER JOIN
  (#ORIGINAL_SURVEYS OS INNER JOIN
  TRIP_SURVEY TS ON
    OS.SURVEY_ID = TS.SURVEY_ID) ON
    T.TRIP_ID = TS.TRIP_ID
--WHERE T.TRIP_ID IN (10921,62066)
GROUP BY
  TS.TRIP_ID,
  CASE
    WHEN OS.SURVEY_SERIES_ID IS NULL THEN 999
    WHEN OS.SURVEY_SERIES_ID IN (6,7) THEN 670          -- Shrimp trawl surveys
    WHEN OS.SURVEY_SERIES_ID IN (35,41,42,43) THEN 350  -- Sablefish surveys
    WHEN OS.SURVEY_SERIES_ID BETWEEN 82 AND 87 THEN 820 -- Jig surveys
    WHEN OS.SURVEY_ID IN (130) THEN 40                  -- HBLL South survey
    WHEN OS.SURVEY_ID IN (131) THEN 39                  -- HBLL North survey
    WHEN OS.SURVEY_SERIES_ID IN (10,21) THEN 21         -- GIG historical
    ELSE OS.SURVEY_SERIES_ID END


-- ===== Tie everything together =====
SELECT 
  'AC'      = ISNULL(AA.ACTIVITY_CODE,0),                      -- TRIP_ACTIVITY
  'VID'     = ISNULL(AA.VESSEL_ID,0),                          -- B01_Trip
  'TID'     = AA.TRIP_ID,                                      -- B01_TRIP
  'FOSTID'  = AA.FOS_TRIP_ID,                                  -- FOS_TRIP
  'FEID'    = AA.FISHING_EVENT_ID,                             -- B02_FISHING_EVENT
  'CID'     = AA.CATCH_ID,                                     -- B03_CATCH
  'SID'     = AA.SAMPLE_ID,                                    -- B04_SAMPLE
  'SPID'    = AA.SPECIMEN_ID,                                  -- B05_SPECIMEN
  'SVID'    = TSS.SURVEY_ID,                                   -- TRIP_SURVEY
  'SSID'    = CASE                                             -- SURVEY
    WHEN TSS.SURVEY_SERIES_ID IS NULL OR TSS.SURVEY_SERIES_ID IN (0) THEN (CASE
      WHEN AA.BLOCK_DESIGNATION IN ('TASU','FLAMINGO') THEN 22
      WHEN AA.BLOCK_DESIGNATION IN ('TRIANGLE','BROOKS') THEN 36
      ELSE TSS.SURVEY_SERIES_ID END)
    ELSE TSS.SURVEY_SERIES_ID END,
  'GC'      = CASE                                             -- SURVEY_GROUPING
    WHEN AA.GROUPING_CODE IS NOT NULL THEN AA.GROUPING_CODE
    WHEN AA.BLOCK_DESIGNATION IN ('TASU','FLAMINGO') THEN (CASE
      WHEN AA.Best_Depth BETWEEN 20 AND 70 THEN 321
      WHEN AA.Best_Depth BETWEEN 70.001 AND 150 THEN 322
      WHEN AA.Best_Depth BETWEEN 150.001 AND 260 THEN 323
      ELSE NULL END)
    WHEN AA.BLOCK_DESIGNATION IN ('TRIANGLE','BROOKS') THEN (CASE
      WHEN AA.Best_Depth BETWEEN 20 AND 70 THEN 324
      WHEN AA.Best_Depth BETWEEN 70.001 AND 150 THEN 325
      WHEN AA.Best_Depth BETWEEN 150.001 AND 260 THEN 326
      ELSE NULL END)
    ELSE NULL END,
  'RC'      = ISNULL(AA.REASON_CODE,0),                       -- B02_FISHING_EVENT
  'block'   = AA.BLOCK_DESIGNATION,                           -- B02_FISHING_EVENT
  'hail'    = IsNull(AA.HAIL_IN_NO,0),                        -- B01_Trip
  'set'     = IsNull(AA.FE_MAJOR_LEVEL_ID,0),                 -- B02_Fishing_Event (primary set,   e.g. tow,    in FEID)
  'sset'    = IsNull(AA.FE_SUB_LEVEL_ID,0),                   -- B02_Fishing_Event (secondary set, e.g. skates, in FEID)
  'ssset'   = ISNULL(AA.FE_MINOR_LEVEL_ID,0),                 -- B02_Fishing_Event (tertiary  set, e.g. hooks,  in FEID)
  'ttype'   = IsNull(AA.TRIP_SUB_TYPE_CODE,0),                -- B01_Trip
  'stype'   = IsNull(AA.SAMPLE_TYPE_CODE,0),                  -- B04_Sample
  'ftype'   = CASE WHEN AA.VESSEL_ID IN (568,569,592,595,608,609,1727) THEN 1 ELSE 0 END,  -- B01_Trip (freezer trawl vessels)
  -- From Scott Buchanan (221201)
  --Vessel Name =c('VIKING ENTERPRISE', 'OSPREY NO 1', 'NORTHERN ALLIANCE', 'RAW SPIRIT', 'PACIFIC LEGACY NO. 1', 'SUNDEROEY', 'VIKING ALLIANCE')
  --VRN = c(310913, 310988, 312275, 312405, 313334, 313464, 313224)
  --VESSEL_ID = c(568, 569, 592, 595, 608, 609, 1727)
  'date'    = CASE 
    WHEN AA.SAMPLE_DATE Is Null Or 
         AA.TRIP_END_DATE-AA.SAMPLE_DATE < 0 Or 
         AA.TRIP_START_DATE-AA.SAMPLE_DATE > 0 THEN 
      convert(smalldatetime,convert(varchar(10),AA.TRIP_END_DATE,20),20)        -- B01_Trip
    ELSE convert(smalldatetime,convert(varchar(10),AA.SAMPLE_DATE,20),20) END,  -- B04_Sample
  'year'    = Year(CASE 
    WHEN AA.SAMPLE_DATE Is Null Or AA.TRIP_END_DATE-AA.SAMPLE_DATE<0 Or
    AA.TRIP_START_DATE-AA.SAMPLE_DATE>0 THEN AA.TRIP_END_DATE -- B01_Trip
    ELSE AA.SAMPLE_DATE END),                                 -- B04_Sample
  -- 'spp'  = AA.SPECIES_CODE,                                -- SPECIMEN_EXISTENCE (genetically determined) or B03_CATCH
  'spp'     = AA.SPECIES_CODE_OBS,                            -- B03_CATCH (species before genetics, if any)
  'exist'   = AA.EXIST_CODE,                                  -- SPECIMEN_EXISTENCE (field = EXISTENCE_ATTRIBUTE_CODE)
  -- 'gene'    = AA.GENETICS,                                 -- was genetics performed (determined in 'processBio.r')
  'sex'     = IsNull(AA.SPECIMEN_SEX_CODE,0),                 -- B05_Specimen
  'mat'     = IsNull(AA.MATURITY_CODE,0),                     -- B05_Specimen
  -- sometimes otoliths have been aged but they do not appear in the SPECIMEN_COLLECTED table
  -- RH 180102 - cannot assume these are otoliths (also, the statement above may no longer be TRUE)
  --'oto'   = CASE
  --            WHEN AA.SPECIMEN_AGE IS NOT NULL THEN 1
  --            ELSE IsNull(TT.otoliths,0) END,               -- B05a_Specimen_Collected
  'oto'     = IsNull(TT.otoliths,0),                          -- B05a_Specimen_Collected
  'fin'     = IsNull(TT.fins,0),                              -- B05a_Specimen_Collected (may be more than one type of fin)
  'scale'   = IsNull(TT.scales,0),                            -- B05a_Specimen_Collected (may be more than one type of scale)
  'uage'    = CASE
              WHEN AA.SPECIMEN_AGE IS NOT NULL AND IsNull(TT.otoliths,0)=0 AND 
              IsNull(TT.fins,0)=0 AND IsNull(TT.scales,0)=0 THEN 1
              ELSE 0 END,                                     -- B05a_Specimen_Collected (this condition may never exist)
  --'narc'  = CASE 
  --            WHEN AA.SPECIMEN_AGE IS NOT NULL OR ISNULL(TT.notavail,0) = 0 THEN NULL
  --            ELSE TT.notavail END,                         -- B05a_Specimen_Collected
  'narc'    = CASE 
              WHEN ISNULL(TT.notavail,0) = 0 THEN NULL
              ELSE TT.notavail END,                           -- B05a_Specimen_Collected
  'age'     = AA.SPECIMEN_AGE,                                -- B05_Specimen
  'ameth'   = CASE WHEN AA.SPECIMEN_AGE Is Null THEN NULL ELSE IsNull(AA.AGEING_METHOD_CODE,0) END,  -- B05_Specimen
  'len'     = CASE WHEN IsNull(BM.Best_Length,0)=0 THEN NULL ELSE BM.Best_Length END,    -- B05d_Specimen_Morphometrics
  'wt'      = CASE WHEN IsNull(BM.Round_Weight,0)=0 THEN NULL ELSE BM.Round_Weight END,  -- B05d_Specimen_Morphometrics
  'scat'    = IsNull(AA.SPECIES_CATEGORY_CODE,0),             -- B03_Catch
  'ssrc'    = AA.SAMPLE_SOURCE_CODE,                          -- B04_Sample
  'cver'    = IsNull(AA.CATCH_VERIFICATION_CODE,0),           -- B03_Catch
  'fdep'    = CASE WHEN AA.Best_Depth=0 THEN NULL ELSE AA.Best_Depth END,  -- B02_FISHING_EVENT
  'bwt'     = CASE WHEN AA.FE_BOTTOM_WATER_TEMPERATURE>30 THEN NULL
    ELSE AA.FE_BOTTOM_WATER_TEMPERATURE END,                  -- B02_Fishing_Event
  'gear'    = IsNull(AA.GEAR_CODE,0),                         -- B02_Fishing_Event
  'use'     = IsNull(TSP.USABILITY,0),                        -- B02e_Trawl_Specs
  'dist'    = IsNull(TSP.DISTANCE,0),                         -- B02e_Trawl_Specs
  'door'    = IsNull(TSP.DOORSPREAD,0),                       -- B02e_Trawl_Specs
  'major'   = IsNull(AA.MAJOR_STAT_AREA_CODE,0),              -- B02_Fishing_Event
  'minor'   = IsNull(AA.MINOR_STAT_AREA_CODE,0),              -- B02_Fishing_Event
  'locality' = IsNull(AA.LOCALITY_CODE,0),                    -- B02_Fishing_Event
  'X'       = IsNull(-AA.Best_Long, Null),                    -- B02_FISHING_EVENT
  'Y'       = IsNull(AA.Best_Lat, Null),                      -- B02_FISHING_EVENT
  CASE
    WHEN AA.MAJOR_STAT_AREA_CODE IN (1) THEN '4B'
    WHEN AA.MAJOR_STAT_AREA_CODE IN (3) THEN '3C'
    WHEN AA.MAJOR_STAT_AREA_CODE IN (4) THEN '3D'
    WHEN AA.MAJOR_STAT_AREA_CODE IN (5) THEN '5A'
    WHEN AA.MAJOR_STAT_AREA_CODE IN (6) THEN '5B'
    WHEN AA.MAJOR_STAT_AREA_CODE IN (7) THEN '5C'
    WHEN AA.MAJOR_STAT_AREA_CODE IN (8) THEN '5D'
    WHEN AA.MAJOR_STAT_AREA_CODE IN (9) THEN '5E'
    WHEN AA.MAJOR_STAT_AREA_CODE IN (71) THEN '3CD'
    WHEN AA.MAJOR_STAT_AREA_CODE IN (72) THEN '3D5A'
    WHEN AA.MAJOR_STAT_AREA_CODE IN (73) THEN '5AB'
    WHEN AA.MAJOR_STAT_AREA_CODE IN (74) THEN '5BC'
    WHEN AA.MAJOR_STAT_AREA_CODE IN (75) THEN '5CD'
    WHEN AA.MAJOR_STAT_AREA_CODE IN (76) THEN '5DE'
    WHEN AA.MAJOR_STAT_AREA_CODE IN (77) THEN '5BE'
    ELSE '0' END AS PMFC,                                     -- B02_Fishing_Event
  ISNULL(AA.DFO_STAT_AREA_CODE,'0') AS PFMA,                  -- B02_Fishing_Event
  ISNULL(AA.DFO_STAT_SUBAREA_CODE,0) AS PFMS,                 -- B02_Fishing_Event
  CASE 
    WHEN AA.DFO_STAT_AREA_CODE IN ('21','23','24','121','123') OR
          (AA.DFO_STAT_AREA_CODE IN ('124') AND AA.DFO_STAT_SUBAREA_CODE IN (1,2,3)) OR
          (AA.DFO_STAT_AREA_CODE IN ('125') AND AA.DFO_STAT_SUBAREA_CODE IN (6)) THEN '3C'
    WHEN AA.DFO_STAT_AREA_CODE IN ('25','26','126') OR
          (AA.DFO_STAT_AREA_CODE IN ('27') AND AA.DFO_STAT_SUBAREA_CODE IN (2,3,4,5,6,7,8,9,10,11)) OR
          (AA.DFO_STAT_AREA_CODE IN ('124') AND AA.DFO_STAT_SUBAREA_CODE IN (4)) OR
          (AA.DFO_STAT_AREA_CODE IN ('125') AND AA.DFO_STAT_SUBAREA_CODE IN (1,2,3,4,5)) OR
          (AA.DFO_STAT_AREA_CODE IN ('127') AND AA.DFO_STAT_SUBAREA_CODE IN (1,2)) THEN '3D'
    WHEN AA.DFO_STAT_AREA_CODE IN ('13','14','15','16','17','18','19','20','28','29') OR
          (AA.DFO_STAT_AREA_CODE IN ('12') AND AA.DFO_STAT_SUBAREA_CODE NOT IN (14)) THEN '4B'
    WHEN AA.DFO_STAT_AREA_CODE IN ('11','111') OR
          (AA.DFO_STAT_AREA_CODE IN ('12') AND AA.DFO_STAT_SUBAREA_CODE IN (14)) OR
          (AA.DFO_STAT_AREA_CODE IN ('27') AND AA.DFO_STAT_SUBAREA_CODE IN (1)) OR
          (AA.DFO_STAT_AREA_CODE IN ('127') AND AA.DFO_STAT_SUBAREA_CODE IN (3,4)) OR
          (AA.DFO_STAT_AREA_CODE IN ('130') AND AA.DFO_STAT_SUBAREA_CODE IN (1)) THEN '5A'
    WHEN AA.DFO_STAT_AREA_CODE IN ('6','106') OR
          (AA.DFO_STAT_AREA_CODE IN ('2') AND AA.DFO_STAT_SUBAREA_CODE BETWEEN 1 AND 19) OR
          (AA.DFO_STAT_AREA_CODE IN ('102') AND AA.DFO_STAT_SUBAREA_CODE IN (2)) OR
          (AA.DFO_STAT_AREA_CODE IN ('105') AND AA.DFO_STAT_SUBAREA_CODE IN (2)) OR
          (AA.DFO_STAT_AREA_CODE IN ('107') AND AA.DFO_STAT_SUBAREA_CODE IN (1)) OR
          --(@sppcode IN ('396','440') AND AA.DFO_STAT_AREA_CODE IN ('102') AND AA.DFO_STAT_SUBAREA_CODE IN (3)) OR
          (('396' IN (@sppcode) OR '440' IN (@sppcode)) AND AA.DFO_STAT_AREA_CODE IN ('102') AND AA.DFO_STAT_SUBAREA_CODE IN (3)) OR
-- note: these four lines identify tows in the four-sided polygon SW of Cape St. James
          --(@sppcode IN ('396','440') AND AA.Best_Long IS NOT NULL AND AA.Best_Lat IS NOT NULL AND
          (('396' IN (@sppcode) OR '440' IN (@sppcode)) AND AA.Best_Long IS NOT NULL AND AA.Best_Lat IS NOT NULL AND
            -- top, right, bottom, left
            AA.Best_Lat   <= 52.33333 AND
            -AA.Best_Long <= ((AA.Best_Lat+29.3722978)/(-0.6208634)) AND
            AA.Best_Lat   >= (92.9445665+(-AA.Best_Long*0.3163707)) AND
            -AA.Best_Long >= ((AA.Best_Lat+57.66623)/(-0.83333)) ) THEN '5C'
    WHEN AA.DFO_STAT_AREA_CODE IN ('7','8','9','10','108','109','110') OR
          --(@sppcode NOT IN ('396','440') AND AA.DFO_STAT_AREA_CODE IN ('102') AND AA.DFO_STAT_SUBAREA_CODE IN (3)) OR
          (('396' NOT IN (@sppcode) AND '440' NOT IN (@sppcode)) AND AA.DFO_STAT_AREA_CODE IN ('102') AND AA.DFO_STAT_SUBAREA_CODE IN (3)) OR
          (AA.DFO_STAT_AREA_CODE IN ('107') AND AA.DFO_STAT_SUBAREA_CODE IN (2,3)) OR
          (AA.DFO_STAT_AREA_CODE IN ('130') AND AA.DFO_STAT_SUBAREA_CODE IN (2)) OR
          (AA.DFO_STAT_AREA_CODE IN ('130') AND AA.DFO_STAT_SUBAREA_CODE IN (3) AND
            COALESCE(AA.Best_Lat,99)<=51.93333) THEN '5B'
    WHEN AA.DFO_STAT_AREA_CODE IN ('3','4','5','103','104') OR
          (AA.DFO_STAT_AREA_CODE IN ('1') AND AA.DFO_STAT_SUBAREA_CODE IN (2,3,4,5)) OR
          (AA.DFO_STAT_AREA_CODE IN ('101') AND AA.DFO_STAT_SUBAREA_CODE BETWEEN 4 AND 10) OR
          (AA.DFO_STAT_AREA_CODE IN ('102') AND AA.DFO_STAT_SUBAREA_CODE IN (1)) OR
          (AA.DFO_STAT_AREA_CODE IN ('105') AND AA.DFO_STAT_SUBAREA_CODE IN (1)) THEN '5D'
    WHEN AA.DFO_STAT_AREA_CODE IN ('142') OR
          (AA.DFO_STAT_AREA_CODE IN ('1') AND AA.DFO_STAT_SUBAREA_CODE IN (1)) OR
          (AA.DFO_STAT_AREA_CODE IN ('2') AND AA.DFO_STAT_SUBAREA_CODE BETWEEN 31 AND 100) OR
          (AA.DFO_STAT_AREA_CODE IN ('101') AND AA.DFO_STAT_SUBAREA_CODE IN (1,2,3)) OR
          (AA.DFO_STAT_AREA_CODE IN ('130') AND AA.DFO_STAT_SUBAREA_CODE IN (3) AND 
            COALESCE(AA.Best_Lat,0)>51.93333) THEN '5E'
    ELSE '0' END AS GMA,                                                        -- B02_Fishing_Event
  'catch'   = AA.CATCH_WEIGHT,                                                    -- B03_Catch
  'nfish'   = AA.CATCH_COUNT,                                                     -- B03_Catch
  'density' = (CASE -- this calculation is sensitive to zero-values 
    WHEN AA.CATCH_WEIGHT IS NULL OR TSP.DISTANCE IS NULL OR TSP.DISTANCE<=0 
      OR TSP.DOORSPREAD IS NULL OR TSP.DOORSPREAD<=0 THEN NULL
    ELSE CAST(ROUND(AA.CATCH_WEIGHT / (TSP.DISTANCE*TSP.DOORSPREAD),7) AS NUMERIC(15,7)) END) -- (kg/(km*m)) = (kg/1000)/(km * (m/1000)) = t/km^2
INTO #BIOSPP
FROM 
  #B01B05 AA 
  LEFT OUTER JOIN
  #TripSurvSer TSS  ON
    AA.TRIP_ID = TSS.TRIP_ID
  LEFT OUTER JOIN 
  #TSpecs TSP  ON
    AA.TRIP_ID = TSP.TRIP_ID AND
    AA.FISHING_EVENT_ID = TSP.FISHING_EVENT_ID
  LEFT OUTER JOIN
  #BestMorpho BM  ON
    AA.SAMPLE_ID = BM.SAMPLE_ID AND
    AA.SPECIMEN_ID = BM.SPECIMEN_ID
  LEFT OUTER JOIN
  #SpecAtts TT  ON
    AA.SAMPLE_ID = TT.SAMPLE_ID AND
    AA.SPECIMEN_ID = TT.SPECIMEN_ID
--WHERE 
--  AA.SPECIES_CODE IN (@sppcode) AND
--  AA.MAJOR_STAT_AREA_CODE IN (@major)

--SELECT * FROM #BIOSPP

SELECT 
  BS.*,
  'area'  = G.AREA_KM2 --IsNull(G.AREA_KM2,0)   -- GROUPING
INTO #GFBBIO
FROM
  #BIOSPP BS LEFT OUTER JOIN
  GROUPING G ON
    BS.GC = G.GROUPING_CODE
ORDER BY
  BS.TID, BS.FEID, BS.CID, BS.SID, BS.SPID

-- Update SURVEY_SERIES_ID from GROUPING table (RH 181212):
--   sablefish/shrimp SSIDs not always identified correctly to this point in the query.
-- Note: If SSID has been automatically selected to be the first ORIGINAL_INDEX of multiple original indices
--   and GROUPING_CODE in GROUPING is NULL, e.g., because REASON_CODE=21 (exploratory FEID),
--   then misidentified SSID (e.g., 41) will not be updated to actual SSID (e.g., 43).
-- There is no easy work around at this point, short of a specific SQL patch for odd cases (not implemented).
--UPDATE #GFBBIO
--SET SSID = COALESCE(
--   (SELECT G.SURVEY_SERIES_ID
--    FROM GROUPING G
--    WHERE G.GROUPING_CODE = #GFBBIO.GC),
--    #GFBBIO.SSID )


SELECT * FROM #GFBBIO

-- getData("gfb_bio.sql","GFBioSQL",strSpp="442")
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="442") -- Yelloweye Rockfish
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="439") -- Redstripe Rockfish
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp=c("394","425"))  -- won't work: incompatible w/ @sppcode NOT IN ('396','440')
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="417") -- Widow Rockfish (WWR: )
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="410") -- Darkblotched Rockfish (180813)
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="602") -- Arrowtooth Flounder (180911)  -- with freezer trawl vessels flagged
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="435") -- Bocaccio (BOR: 180912, 181120, 181206, 181212, 181219 | for 2019 assess)
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="417") -- Widow Rockfish (WWR: 180627, 180803, 181012, 181107, 181120, 181207, 181219, 190321 | for 2019 assess)
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="425") -- Blackspotted Rockfish (BSR: 190110, 190114, 190128, 190717 | for Vania|Sean)
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="394") -- Rougheye Rockfish (RER: 190110, 190114, 190128, 190717 | for Vania|Sean)
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="435") -- Bocaccio (BOR: 190107, 190620, 190709, 190812, 190822, 190903, 191003, 190107, 190620, 190709, 190812, 190822, 190903, 191003 | for 2019 assess)
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="394") -- Rougheye Rockfish (RER: 191015, 191220, 200106 | for 2020 assess)
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="425") -- Blackspotted Rockfish (BSR: 191015, 191220, 200106 | for 2020 assess)
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="610") -- Rex Sole (RXL: 200123 | for 2020 MPF)
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp=c("394","425")) -- Rougheye/Blackspotted (REBS: 200107, 200228, 200316 [2020 WP])
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="437") -- Canary Rockfish (CAR: 200720, 200903 | for PJS)
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="440") -- Yellowmouth Rockfish (YMR: 200720, 200903, 200928, 210120, 210208 [2021 WP])
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="435") -- Bocaccio (BOR: 211014 [2021 SR])
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="225") -- Pacific Hake (PAK: 211210 | for PJS)
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="396") -- Pacific Ocean Perch (POP: 210217, 211021, 221021 | for PJS)
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="453") -- Longspine Thornyhead (LST: 220309 | for RH)
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="437") -- Canary Rockfish (CAR: 200720, 200903, 211013, 211123, 220502 [2022 WP])
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="602") -- Arrowtooth Flounder (ARF: 180619, 180829, 180911, 200908, 200914, 201007, 230112 | for 2022 assess)
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="401") -- Redbanded Rockfish (RBR: 230411 | for SCL)
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="396") -- Pacific Ocean Perch (POP: 230117, 230428 [2023 WP])
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="607") -- Petrale Sole (PEL: 230925 [MM])
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="435") -- Bocaccio (BOR: 230117, 240102 [2024 SR])
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="418") -- Yellowtail Rockfish (180531, 200910, 240208, 240315, 240320 [2024 WP])
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="009") -- Rougheye Rockfish (240731) -- no records
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="044") -- Spiny Dogfish (DOG: 241024 [DF])
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="228") -- Walleye Pollock (WAP: 241125 [DH])
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="405") -- Silvergray Rockfish (SGR: 240731, 241108, 250414, 250423 [2025 WP])
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="425") -- Blackspotted Rockfish (BSR 240731, 240904, 250501 [NF])
-- qu("gfb_bio.sql",dbName="GFBioSQL",strSpp="394") -- Rougheye Rockfish (RER: 240731, 240904, 250501 [NF])

-- DF=Dee Finn, DH=Dana Haggarty, MM=Mackenzie Mazur, NF=Nick Fisch, PJS=Paul Starr, RH=Rowan Haigh, SR=Science Response, WP=working paper
