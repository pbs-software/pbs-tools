-- Get specimen biological data from GFBioSQL (2011-09-12)
SET NOCOUNT ON

-- Get survey ID if it exists
SELECT
  TS.TRIP_ID, TS.SURVEY_ID, SG.GROUPING_CODE
INTO #Link
FROM 
  SURVEY S INNER JOIN 
  (TRIP_SURVEY TS INNER JOIN 
  SURVEY_GROUPING SG ON 
  TS.SURVEY_ID = SG.SURVEY_ID) ON 
  S.SURVEY_ID = TS.SURVEY_ID 
WHERE S.ORIGINAL_IND='Y' 

-- Get default survey ID if surveys in B21_Samples are missing GROUPING_CODE
SELECT
	TS.TRIP_ID,
	MIN(TS.SURVEY_ID) AS SVID_DEF
INTO  #Unilink
FROM  TRIP_SURVEY TS
GROUP BY TS.TRIP_ID
ORDER BY TS.TRIP_ID

-- Get 'collected attribute' of specimens
SELECT
  SC.SAMPLE_ID,
  SC.SPECIMEN_ID,
  'otoliths'  = SUM(CASE WHEN SC.COLLECTED_ATTRIBUTE_CODE IN (20) THEN 1 ELSE 0 END),
  'scales'    = SUM(CASE WHEN SC.COLLECTED_ATTRIBUTE_CODE IN (21) THEN 1 ELSE 0 END),
  'fins'      = SUM(CASE WHEN SC.COLLECTED_ATTRIBUTE_CODE IN (22,23,27,28) THEN 1 ELSE 0 END),
  'genetics'  = SUM(CASE WHEN SC.COLLECTED_ATTRIBUTE_CODE IN (4,6,30) THEN 1 ELSE 0 END),
  'gonads'    = SUM(CASE WHEN SC.COLLECTED_ATTRIBUTE_CODE IN (1) THEN 1 ELSE 0 END),
  'stomachs'  = SUM(CASE WHEN SC.COLLECTED_ATTRIBUTE_CODE IN (2) THEN 1 ELSE 0 END)
INTO #Atts
FROM 
  B05a_Specimen_Collected SC 
GROUP BY
  SC.SAMPLE_ID, SC.SPECIMEN_ID

SELECT 
  'SPID' = SP.SPECIMEN_ID,                                  -- B05_Specimen
  'SID'  = SA.SAMPLE_ID,                                    -- B04_Sample
  'TID'  = SA.TRIP_ID,                                      -- B01_Trip
  'FEID' = SA.FISHING_EVENT_ID,                             -- B02_Fishing_Event
  'SVID' = COALESCE(L.SURVEY_ID,U.SVID_DEF,0),
  'group'= COALESCE(SA.GROUPING_CODE,L.GROUPING_CODE,0),    -- B02_Fishing_Event
  'hail' = IsNull(SA.HAIL_IN_NO,0),                         -- B01_Trip
  'set'  = SA.FE_MAJOR_LEVEL_ID,                            -- B02_Fishing_Event
  'subset' = IsNull(SA.FE_SUB_LEVEL_ID,1),                  -- B02_Fishing_Event
  'ttype' = IsNull(SA.TRIP_SUB_TYPE_CODE,0),                -- B01_Trip
  'stype' = IsNull(SA.SAMPLE_TYPE_CODE,0),                  -- B04_Sample
  'date' = CASE 
    WHEN SA.SAMPLE_DATE Is Null Or 
         SA.TRIP_END_DATE-SA.SAMPLE_DATE < 0 Or 
         SA.TRIP_START_DATE-SA.SAMPLE_DATE > 0 THEN 
      convert(smalldatetime,convert(varchar(10),SA.TRIP_END_DATE,20),20)        -- B01_Trip
    ELSE convert(smalldatetime,convert(varchar(10),SA.SAMPLE_DATE,20),20) END,  -- B04_Sample
  'year' = Year(CASE 
    WHEN SA.SAMPLE_DATE Is Null Or SA.TRIP_END_DATE-SA.SAMPLE_DATE<0 Or
    SA.TRIP_START_DATE-SA.SAMPLE_DATE>0 THEN SA.TRIP_END_DATE                   -- B01_Trip
    ELSE SA.SAMPLE_DATE END),                                                   -- B04_Sample
  'sex'   = IsNull(SP.SPECIMEN_SEX_CODE,0),                 -- B05_Specimen
  'mat'   = IsNull(SP.MATURITY_CODE,0),                     -- B05_Specimen
  'oto'   = IsNull(A.otoliths,0),
  'age'   = SP.SPECIMEN_AGE,                                -- B05_Specimen
  'ameth' = CASE WHEN SP.SPECIMEN_AGE Is Null THEN NULL ELSE IsNull(SP.AGEING_METHOD_CODE,0) END,  -- B05_Specimen
  'len'   = CASE WHEN SP.Best_Length=0 THEN NULL ELSE SP.Best_Length END,       -- B22_Specimens
  'wt'    = CASE WHEN SP.Round_Weight=0 THEN NULL ELSE SP.Round_Weight END,     -- B22_Specimens
  'fdep'  = CASE WHEN SA.Best_Depth=0 THEN NULL ELSE SA.Best_Depth END,         -- B21_Samples
  'gear'  = IsNull(SA.GEAR_CODE,0),                         -- B02_Fishing_Event
  'major' = IsNull(SA.MAJOR_STAT_AREA_CODE,0),              -- B02_Fishing_Event
  'minor' = IsNull(SA.MINOR_STAT_AREA_CODE,0),              -- B02_Fishing_Event
  'locality' = IsNull(SA.LOCALITY_CODE,0),                  -- B02_Fishing_Event
  'X' = IsNull(-SA.Best_Long, Null),                        -- B21_Samples
  'Y' = IsNull(SA.Best_Lat, Null),                          -- B21_Samples
  CASE
    WHEN SA.MAJOR_STAT_AREA_CODE IN (1) THEN '4B'
    WHEN SA.MAJOR_STAT_AREA_CODE IN (3) THEN '3C'
    WHEN SA.MAJOR_STAT_AREA_CODE IN (4) THEN '3D'
    WHEN SA.MAJOR_STAT_AREA_CODE IN (5) THEN '5A'
    WHEN SA.MAJOR_STAT_AREA_CODE IN (6) THEN '5B'
    WHEN SA.MAJOR_STAT_AREA_CODE IN (7) THEN '5C'
    WHEN SA.MAJOR_STAT_AREA_CODE IN (8) THEN '5D'
    WHEN SA.MAJOR_STAT_AREA_CODE IN (9) THEN '5E'
    ELSE '0' END AS PMFC,                                   -- B02_Fishing_Event
  ISNULL(DFO_STAT_AREA_CODE,'0') AS PFMA,                   -- B02_Fishing_Event
  ISNULL(DFO_STAT_SUBAREA_CODE,0) AS PFMS,                  -- B02_Fishing_Event
  CASE 
    WHEN SA.DFO_STAT_AREA_CODE IN ('21','23','24','121','123') OR
          (SA.DFO_STAT_AREA_CODE IN ('124') AND SA.DFO_STAT_SUBAREA_CODE IN (1,2,3)) OR
          (SA.DFO_STAT_AREA_CODE IN ('125') AND SA.DFO_STAT_SUBAREA_CODE IN (6)) THEN '3C'
    WHEN SA.DFO_STAT_AREA_CODE IN ('25','26','126') OR
          (SA.DFO_STAT_AREA_CODE IN ('27') AND SA.DFO_STAT_SUBAREA_CODE IN (2,3,4,5,6,7,8,9,10,11)) OR
          (SA.DFO_STAT_AREA_CODE IN ('124') AND SA.DFO_STAT_SUBAREA_CODE IN (4)) OR
          (SA.DFO_STAT_AREA_CODE IN ('125') AND SA.DFO_STAT_SUBAREA_CODE IN (1,2,3,4,5)) OR
          (SA.DFO_STAT_AREA_CODE IN ('127') AND SA.DFO_STAT_SUBAREA_CODE IN (1,2)) THEN '3D'
    WHEN SA.DFO_STAT_AREA_CODE IN ('13','14','15','16','17','18','19','20','28','29') OR
          (SA.DFO_STAT_AREA_CODE IN ('12') AND SA.DFO_STAT_SUBAREA_CODE NOT IN (14)) THEN '4B'
    WHEN SA.DFO_STAT_AREA_CODE IN ('11','111') OR
          (SA.DFO_STAT_AREA_CODE IN ('12') AND SA.DFO_STAT_SUBAREA_CODE IN (14)) OR
          (SA.DFO_STAT_AREA_CODE IN ('27') AND SA.DFO_STAT_SUBAREA_CODE IN (1)) OR
          (SA.DFO_STAT_AREA_CODE IN ('127') AND SA.DFO_STAT_SUBAREA_CODE IN (3,4)) OR
          (SA.DFO_STAT_AREA_CODE IN ('130') AND SA.DFO_STAT_SUBAREA_CODE IN (1)) THEN '5A'
    WHEN SA.DFO_STAT_AREA_CODE IN ('6','106') OR
          (SA.DFO_STAT_AREA_CODE IN ('2') AND SA.DFO_STAT_SUBAREA_CODE BETWEEN 1 AND 19) OR
          (SA.DFO_STAT_AREA_CODE IN ('102') AND SA.DFO_STAT_SUBAREA_CODE IN (2)) OR
          (SA.DFO_STAT_AREA_CODE IN ('105') AND SA.DFO_STAT_SUBAREA_CODE IN (2)) OR
          (SA.DFO_STAT_AREA_CODE IN ('107') AND SA.DFO_STAT_SUBAREA_CODE IN (1)) OR
          (@sppcode IN ('396','440') AND SA.DFO_STAT_AREA_CODE IN ('102') AND SA.DFO_STAT_SUBAREA_CODE IN (3)) OR
-- note: these four lines identify tows in the four-sided polygon SW of Cape St. James
          (@sppcode IN ('396','440') AND SA.Best_Long IS NOT NULL AND SA.Best_Lat IS NOT NULL AND
            -- top, right, bottom, left
            SA.Best_Lat   <= 52.33333 AND
            -SA.Best_Long <= ((SA.Best_Lat+29.3722978)/(-0.6208634)) AND
            SA.Best_Lat   >= (92.9445665+(-SA.Best_Long*0.3163707)) AND
            -SA.Best_Long >= ((SA.Best_Lat+57.66623)/(-0.83333)) ) THEN '5C'
    WHEN SA.DFO_STAT_AREA_CODE IN ('7','8','9','10','108','109','110') OR
          (@sppcode NOT IN ('396','440') AND SA.DFO_STAT_AREA_CODE IN ('102') AND SA.DFO_STAT_SUBAREA_CODE IN (3)) OR
          (SA.DFO_STAT_AREA_CODE IN ('107') AND SA.DFO_STAT_SUBAREA_CODE IN (2,3)) OR
          (SA.DFO_STAT_AREA_CODE IN ('130') AND SA.DFO_STAT_SUBAREA_CODE IN (2)) OR
          (SA.DFO_STAT_AREA_CODE IN ('130') AND SA.DFO_STAT_SUBAREA_CODE IN (3) AND
            COALESCE(SA.Best_Lat,99)<=51.93333) THEN '5B'
    WHEN SA.DFO_STAT_AREA_CODE IN ('3','4','5','103','104') OR
          (SA.DFO_STAT_AREA_CODE IN ('1') AND SA.DFO_STAT_SUBAREA_CODE IN (2,3,4,5)) OR
          (SA.DFO_STAT_AREA_CODE IN ('101') AND SA.DFO_STAT_SUBAREA_CODE BETWEEN 4 AND 10) OR
          (SA.DFO_STAT_AREA_CODE IN ('102') AND SA.DFO_STAT_SUBAREA_CODE IN (1)) OR
          (SA.DFO_STAT_AREA_CODE IN ('105') AND SA.DFO_STAT_SUBAREA_CODE IN (1)) THEN '5D'
    WHEN SA.DFO_STAT_AREA_CODE IN ('142') OR
          (SA.DFO_STAT_AREA_CODE IN ('1') AND SA.DFO_STAT_SUBAREA_CODE IN (1)) OR
          (SA.DFO_STAT_AREA_CODE IN ('2') AND SA.DFO_STAT_SUBAREA_CODE BETWEEN 31 AND 100) OR
          (SA.DFO_STAT_AREA_CODE IN ('101') AND SA.DFO_STAT_SUBAREA_CODE IN (1,2,3)) OR
          (SA.DFO_STAT_AREA_CODE IN ('130') AND SA.DFO_STAT_SUBAREA_CODE IN (3) AND 
            COALESCE(SA.Best_Lat,0)>51.93333) THEN '5E'
    ELSE '0' END AS GMA,                                                        -- B02_Fishing_Event
  'Tbw' = CASE WHEN SA.FE_BOTTOM_WATER_TEMPERATURE>30 THEN NULL
    ELSE SA.FE_BOTTOM_WATER_TEMPERATURE END,                                    -- B02_Fishing_Event
  'catch' = SA.CATCH_WEIGHT                                                     -- B03_Catch
FROM 
  #Unilink U RIGHT OUTER JOIN
  (#Link L RIGHT OUTER JOIN 
  (B21_Samples SA INNER JOIN 
  (B22_Specimens SP LEFT OUTER JOIN 
  #Atts A ON
    A.SAMPLE_ID = SP.SAMPLE_ID AND
    A.SPECIMEN_ID = SP.SPECIMEN_ID) ON 
    SA.SAMPLE_ID = SP.SAMPLE_ID) ON
    L.TRIP_ID = SA.TRIP_ID AND
    L.GROUPING_CODE = SA.GROUPING_CODE) ON
    U.TRIP_ID = SA.TRIP_ID
WHERE 
  SP.SPECIES_CODE IN (@sppcode) AND
  SA.MAJOR_STAT_AREA_CODE IN (@major)

-- getData("gfb_bio.sql","GFBioSQL",strSpp="415")


