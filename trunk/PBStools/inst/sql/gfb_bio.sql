-- Get specimen biological data from GFBioSQL (2012-08-03)
SET NOCOUNT ON

-- Collect unadulterated values from B01 to B05
SELECT 
  B01.TRIP_ID,
  B02.FISHING_EVENT_ID,
  B03.CATCH_ID,
  B04.SAMPLE_ID,
  B05.SPECIMEN_ID,
  B02.GROUPING_CODE,
  B01.HAIL_IN_NO,
  B02.FE_MAJOR_LEVEL_ID,
  B02.FE_SUB_LEVEL_ID,
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
  B02.FE_BOTTOM_WATER_TEMPERATURE,
  B03.SPECIES_CODE,
  B03.CATCH_WEIGHT
INTO #B01B05
FROM 
  B01_TRIP B01 INNER JOIN
  B02_FISHING_EVENT B02 INNER JOIN
  B02L3_Link_Fishing_Event_Catch L1 INNER JOIN
  B03_CATCH B03 INNER JOIN
  B03L4_Link_Catch_Sample L2 INNER JOIN
  B04_SAMPLE B04 INNER JOIN
  B05_SPECIMEN B05 ON
    B05.SAMPLE_ID = B04.SAMPLE_ID ON
    B04.SAMPLE_ID = L2.SAMPLE_ID ON
    L2.CATCH_ID = B03.CATCH_ID ON
    B03.CATCH_ID = L1.CATCH_ID ON
    L1.FISHING_EVENT_ID = B02.FISHING_EVENT_ID ON
    B02.TRIP_ID = B01.TRIP_ID
WHERE 
  B03.SPECIES_CODE IN (@sppcode) AND
  B02.MAJOR_STAT_AREA_CODE IN (@major)

-- Collect the derived values from B21 and B22 that are not easily reproduced from scratch
SELECT 
  SA.TRIP_ID,
  SA.FISHING_EVENT_ID,
  SA.CATCH_ID,
  SA.SAMPLE_ID,
  SP.SPECIMEN_ID,
  SA.Best_Long,
  SA.Best_Lat,
  SA.Best_Depth,
  SP.Best_Length,
  SP.Round_Weight
INTO #B21B22
FROM 
  B21_Samples SA INNER JOIN 
  B22_Specimens SP ON
    SA.SAMPLE_ID = SP.SAMPLE_ID
WHERE 
  SP.SPECIES_CODE IN (@sppcode) AND
  SA.MAJOR_STAT_AREA_CODE IN (@major)

-- Get survey information by trip (TRIP_ID and GROUPING_CODE are key fields)
SELECT
  TS.TRIP_ID, 
  TS.SURVEY_ID, 
  IsNull(S.SURVEY_SERIES_ID,0) AS SURVEY_SERIES_ID,
  IsNull(SG.GROUPING_CODE,0) AS GROUPING_CODE,
  IsNull(G.AREA_KM2,0) AS AREA_KM2
INTO #TripSurv
FROM 
  SURVEY S RIGHT OUTER JOIN 
  (TRIP_SURVEY TS LEFT OUTER JOIN 
  (SURVEY_GROUPING SG INNER JOIN
  GROUPING G ON
  SG.GROUPING_CODE = G.GROUPING_CODE) ON
  TS.SURVEY_ID = SG.SURVEY_ID) ON 
  S.SURVEY_ID = TS.SURVEY_ID 
WHERE S.ORIGINAL_IND='Y' 

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
INTO #SpecAtts
FROM 
  B05a_Specimen_Collected SC 
GROUP BY
  SC.SAMPLE_ID, SC.SPECIMEN_ID

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
  B01_TRIP B01 INNER JOIN
  (B02_FISHING_EVENT B02 LEFT OUTER JOIN
  B02e_Trawl_Specs B02e ON
  B02.FISHING_EVENT_ID = B02e.FISHING_EVENT_ID) ON
  B01.TRIP_ID = B02.TRIP_ID

-- Tie everything together
SELECT 
  'TID'  = AA.TRIP_ID,                                      -- B01_TRIP
  'FEID' = AA.FISHING_EVENT_ID,                             -- B02_FISHING_EVENT
  'CID'  = AA.CATCH_ID,                                     -- B03_CATCH
  'SID'  = AA.SAMPLE_ID,                                    -- B04_SAMPLE
  'SPID' = AA.SPECIMEN_ID,                                  -- B05_SPECIMEN
  'SVID' = IsNull(TSG.SURVEY_ID,0),                         -- TRIP_SURVEY
  'SSID' = IsNull(TSG.SURVEY_SERIES_ID,0),                  -- SURVEY
  'GC'   = COALESCE(AA.GROUPING_CODE,TSG.GROUPING_CODE,0),  -- SURVEY_GROUPING
  'hail' = IsNull(AA.HAIL_IN_NO,0),                         -- B01_Trip
  'set'  = AA.FE_MAJOR_LEVEL_ID,                            -- B02_Fishing_Event
  'subset' = IsNull(AA.FE_SUB_LEVEL_ID,1),                  -- B02_Fishing_Event
  'ttype' = IsNull(AA.TRIP_SUB_TYPE_CODE,0),                -- B01_Trip
  'stype' = IsNull(AA.SAMPLE_TYPE_CODE,0),                  -- B04_Sample
  'date' = CASE 
    WHEN AA.SAMPLE_DATE Is Null Or 
         AA.TRIP_END_DATE-AA.SAMPLE_DATE < 0 Or 
         AA.TRIP_START_DATE-AA.SAMPLE_DATE > 0 THEN 
      convert(smalldatetime,convert(varchar(10),AA.TRIP_END_DATE,20),20)        -- B01_Trip
    ELSE convert(smalldatetime,convert(varchar(10),AA.SAMPLE_DATE,20),20) END,  -- B04_Sample
  'year' = Year(CASE 
    WHEN AA.SAMPLE_DATE Is Null Or AA.TRIP_END_DATE-AA.SAMPLE_DATE<0 Or
    AA.TRIP_START_DATE-AA.SAMPLE_DATE>0 THEN AA.TRIP_END_DATE                   -- B01_Trip
    ELSE AA.SAMPLE_DATE END),                                                   -- B04_Sample
  'sex'   = IsNull(AA.SPECIMEN_SEX_CODE,0),                 -- B05_Specimen
  'mat'   = IsNull(AA.MATURITY_CODE,0),                     -- B05_Specimen
  'oto'   = IsNull(TT.otoliths,0),
  'age'   = AA.SPECIMEN_AGE,                                -- B05_Specimen
  'ameth' = CASE WHEN AA.SPECIMEN_AGE Is Null THEN NULL ELSE IsNull(AA.AGEING_METHOD_CODE,0) END,  -- B05_Specimen
  'len'   = CASE WHEN BB.Best_Length=0 THEN NULL ELSE BB.Best_Length END,       -- B22_Specimens
  'wt'    = CASE WHEN BB.Round_Weight=0 THEN NULL ELSE BB.Round_Weight END,     -- B22_Specimens
  'fdep'  = CASE WHEN BB.Best_Depth=0 THEN NULL ELSE BB.Best_Depth END,         -- B21_Samples
  'gear'  = IsNull(AA.GEAR_CODE,0),                         -- B02_Fishing_Event
  'use'   = IsNull(TSP.USABILITY,0),                        -- B02e_Trawl_Specs
  'dist'  = IsNull(TSP.DISTANCE,0),                         -- B02e_Trawl_Specs
  'door'  = IsNull(TSP.DOORSPREAD,0),                       -- B02e_Trawl_Specs
  'area'  = IsNull(TSG.AREA_KM2,0),                         -- GROUPING
  'major' = IsNull(AA.MAJOR_STAT_AREA_CODE,0),              -- B02_Fishing_Event
  'minor' = IsNull(AA.MINOR_STAT_AREA_CODE,0),              -- B02_Fishing_Event
  'locality' = IsNull(AA.LOCALITY_CODE,0),                  -- B02_Fishing_Event
  'X' = IsNull(-BB.Best_Long, Null),                        -- B21_Samples
  'Y' = IsNull(BB.Best_Lat, Null),                          -- B21_Samples
  CASE
    WHEN AA.MAJOR_STAT_AREA_CODE IN (1) THEN '4B'
    WHEN AA.MAJOR_STAT_AREA_CODE IN (3) THEN '3C'
    WHEN AA.MAJOR_STAT_AREA_CODE IN (4) THEN '3D'
    WHEN AA.MAJOR_STAT_AREA_CODE IN (5) THEN '5A'
    WHEN AA.MAJOR_STAT_AREA_CODE IN (6) THEN '5B'
    WHEN AA.MAJOR_STAT_AREA_CODE IN (7) THEN '5C'
    WHEN AA.MAJOR_STAT_AREA_CODE IN (8) THEN '5D'
    WHEN AA.MAJOR_STAT_AREA_CODE IN (9) THEN '5E'
    ELSE '0' END AS PMFC,                                   -- B02_Fishing_Event
  ISNULL(DFO_STAT_AREA_CODE,'0') AS PFMA,                   -- B02_Fishing_Event
  ISNULL(DFO_STAT_SUBAREA_CODE,0) AS PFMS,                  -- B02_Fishing_Event
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
          (@sppcode IN ('396','440') AND AA.DFO_STAT_AREA_CODE IN ('102') AND AA.DFO_STAT_SUBAREA_CODE IN (3)) OR
-- note: these four lines identify tows in the four-sided polygon SW of Cape St. James
          (@sppcode IN ('396','440') AND BB.Best_Long IS NOT NULL AND BB.Best_Lat IS NOT NULL AND
            -- top, right, bottom, left
            BB.Best_Lat   <= 52.33333 AND
            -BB.Best_Long <= ((BB.Best_Lat+29.3722978)/(-0.6208634)) AND
            BB.Best_Lat   >= (92.9445665+(-BB.Best_Long*0.3163707)) AND
            -BB.Best_Long >= ((BB.Best_Lat+57.66623)/(-0.83333)) ) THEN '5C'
    WHEN AA.DFO_STAT_AREA_CODE IN ('7','8','9','10','108','109','110') OR
          (@sppcode NOT IN ('396','440') AND AA.DFO_STAT_AREA_CODE IN ('102') AND AA.DFO_STAT_SUBAREA_CODE IN (3)) OR
          (AA.DFO_STAT_AREA_CODE IN ('107') AND AA.DFO_STAT_SUBAREA_CODE IN (2,3)) OR
          (AA.DFO_STAT_AREA_CODE IN ('130') AND AA.DFO_STAT_SUBAREA_CODE IN (2)) OR
          (AA.DFO_STAT_AREA_CODE IN ('130') AND AA.DFO_STAT_SUBAREA_CODE IN (3) AND
            COALESCE(BB.Best_Lat,99)<=51.93333) THEN '5B'
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
            COALESCE(BB.Best_Lat,0)>51.93333) THEN '5E'
    ELSE '0' END AS GMA,                                                        -- B02_Fishing_Event
  'temp' = CASE WHEN AA.FE_BOTTOM_WATER_TEMPERATURE>30 THEN NULL
    ELSE AA.FE_BOTTOM_WATER_TEMPERATURE END,                                    -- B02_Fishing_Event
  'catch' = AA.CATCH_WEIGHT,                                                    -- B03_Catch
  'density' = (CASE -- this calculation is sensitive to zero-values 
    WHEN AA.CATCH_WEIGHT IS NULL OR TSP.DISTANCE IS NULL OR TSP.DISTANCE<=0 
      OR TSP.DOORSPREAD IS NULL OR TSP.DOORSPREAD<=0 THEN NULL
    ELSE 1000.*AA.CATCH_WEIGHT / (TSP.DISTANCE*TSP.DOORSPREAD) END)
FROM 
  #SpecAtts TT RIGHT OUTER JOIN
  (#TripSurv TSG RIGHT OUTER JOIN
  (#B21B22 BB RIGHT OUTER JOIN 
  (#B01B05 AA LEFT OUTER JOIN
  #TSpecs TSP ON
    TSP.TRIP_ID = AA.TRIP_ID AND
    TSP.FISHING_EVENT_ID = AA.FISHING_EVENT_ID) ON
    AA.TRIP_ID = BB.TRIP_ID AND
    AA.FISHING_EVENT_ID = BB.FISHING_EVENT_ID AND
    AA.CATCH_ID = BB.CATCH_ID AND
    AA.SAMPLE_ID = BB.SAMPLE_ID AND
    AA.SPECIMEN_ID = BB.SPECIMEN_ID) ON
    TSG.TRIP_ID = AA.TRIP_ID AND
    TSG.GROUPING_CODE = AA.GROUPING_CODE) ON
    TT.SAMPLE_ID = AA.SAMPLE_ID AND
    TT.SPECIMEN_ID = AA.SPECIMEN_ID
WHERE 
  AA.SPECIES_CODE IN (@sppcode) AND
  AA.MAJOR_STAT_AREA_CODE IN (@major)

-- getData("gfb_bio.sql","GFBioSQL",strSpp="415")


