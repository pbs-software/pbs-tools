-- Calculate mean weight of a fish species from GFBioSQL 
-- for converting discarded pieces to kg in GFFOS.
-- Last modified: 2014-12-12
-- =====================================
-- Start PJS calculations of mean weight
-- =====================================
-- PJS Algorithm Step 2 - calculate mean weight by sex for each sample (~= tow)
-- PJS Algorithm Step 3 - calculate mean weight by sex and trip from samples on the same trip, weighted by tow weight of RRF
-- PJS Algorithm Step 4 - calculate mean weight by sex and quarter, weighted by trip catch of RRF
-- PJS Algorithm Step 6 - calculate single mean weight for all years (equal weight for each year)


SET NOCOUNT ON

-- Create data table for calculating mean weight from lengths
-- ==========================================================
-- Create data table for calculating mean weight from lengths
SELECT -- TOP 100
  B21.GEAR_CODE,
  B22.SPECIES_CODE,
  B22.SPECIMEN_SEX_CODE,
  B22.BEST_LENGTH,
  B22.ROUND_WEIGHT,
  -- additional fields not used in length-weight fit
  B21.TRIP_ID,
  YEAR(B21.TRIP_START_DATE) AS TRIP_YEAR,
  CASE 
    WHEN MONTH(B21.TRIP_START_DATE) IN (1,2,3) THEN 1
    WHEN MONTH(B21.TRIP_START_DATE) IN (4,5,6) THEN 2
    WHEN MONTH(B21.TRIP_START_DATE) IN (7,8,9) THEN 3
    WHEN MONTH(B21.TRIP_START_DATE) IN (10,11,12) THEN 4
    ELSE 0 END AS TRIP_QUARTER,
  B21.FISHING_EVENT_ID,
  B21.SAMPLE_ID
INTO #data_table
FROM
  B22_Specimens B22 INNER JOIN
  B21_Samples B21 ON
    B21.SAMPLE_ID = B22.SAMPLE_ID
WHERE
  B21.TRIP_SUB_TYPE_CODE IN (1,4,5) AND
  B21.MAJOR_STAT_AREA_CODE IN (@major) AND
  B21.GEAR_CODE IN (1,2,4,5,6) AND
  B21.SPECIES_CATEGORY_CODE IN (1) AND
  B21.SAMPLE_TYPE_CODE IN (1,2,6,7) AND
  YEAR(B21.TRIP_START_DATE) >= 2007 AND
  B22.SPECIES_CODE IN (@sppcode) AND
  B22.SPECIMEN_SEX_CODE IN (1,2)

-- Subset the data table for length-weight analyis
SELECT
  dt.GEAR_CODE,
  dt.SPECIES_CODE,
  dt.SPECIMEN_SEX_CODE,
  LOG(dt.BEST_LENGTH) AS x,
  LOG(dt.ROUND_WEIGHT) AS y
INTO #fish_table
FROM #data_table dt
WHERE
  dt.BEST_LENGTH > 0.0 AND
  dt.ROUND_WEIGHT > 0.0

-- Start linear regression queries
-- ===============================
SELECT
  ft.GEAR_CODE,
  ft.SPECIES_CODE,
  ft.SPECIMEN_SEX_CODE, 
  AVG(ft.x)  AS xmean,
  AVG(ft.y)  AS ymean
INTO #mean_estimates
FROM #fish_table ft
GROUP BY
  ft.GEAR_CODE,
  ft.SPECIES_CODE,
  ft.SPECIMEN_SEX_CODE
ORDER BY
  ft.GEAR_CODE,
  ft.SPECIES_CODE,
  ft.SPECIMEN_SEX_CODE

SELECT
  ft.GEAR_CODE,
  ft.SPECIES_CODE,
  ft.SPECIMEN_SEX_CODE, 
  CASE WHEN COUNT(*)=1 OR SUM(SQUARE(ft.x - me.xmean))=0 THEN 1 
    ELSE SQRT(SUM(SQUARE(ft.x - me.xmean)) / (COUNT(*) - 1)) END  AS xstdev,
  CASE WHEN COUNT(*)=1 THEN 0
    ELSE SQRT(SUM(SQUARE(ft.y - me.ymean)) / (COUNT(*) - 1)) END  AS ystdev
INTO #stdev_estimates
FROM #fish_table ft
INNER JOIN 
  #mean_estimates  me ON
    me.GEAR_CODE = ft.GEAR_CODE AND
    me.SPECIES_CODE = ft.SPECIES_CODE AND
    me.SPECIMEN_SEX_CODE = ft.SPECIMEN_SEX_CODE
GROUP BY
  ft.GEAR_CODE,
  ft.SPECIES_CODE,
  ft.SPECIMEN_SEX_CODE
--  me.xmean, me.ymean
ORDER BY
  ft.GEAR_CODE,
  ft.SPECIES_CODE,
  ft.SPECIMEN_SEX_CODE

-- increases numerical stability
SELECT
  ft.GEAR_CODE,
  ft.SPECIES_CODE,
  ft.SPECIMEN_SEX_CODE, 
  (ft.x - me.xmean) / se.xstdev   AS xstd,
  CASE se.ystdev WHEN 0 THEN 0 ELSE (ft.y - me.ymean) / se.ystdev END AS ystd
INTO #standardized_data
FROM 
  #fish_table ft 
  INNER JOIN #stdev_estimates se ON 
    se.GEAR_CODE = ft.GEAR_CODE AND
    se.SPECIES_CODE = ft.SPECIES_CODE AND
    se.SPECIMEN_SEX_CODE = ft.SPECIMEN_SEX_CODE
  INNER JOIN #mean_estimates  me ON 
    me.GEAR_CODE = ft.GEAR_CODE AND
    me.SPECIES_CODE = ft.SPECIES_CODE AND
    me.SPECIMEN_SEX_CODE = ft.SPECIMEN_SEX_CODE

SELECT
  sd.GEAR_CODE,
  sd.SPECIES_CODE,
  sd.SPECIMEN_SEX_CODE,
  COUNT(*) AS n,
  CASE WHEN SUM(sd.xstd * sd.xstd) = 0 THEN 0
    ELSE SUM(sd.xstd * sd.ystd) / (COUNT(*) - 1) END  AS betastd
INTO #standardized_beta_estimates
FROM #standardized_data sd
GROUP BY
  sd.GEAR_CODE,
  sd.SPECIES_CODE,
  sd.SPECIMEN_SEX_CODE

SELECT
  sb.GEAR_CODE AS gear,
  sb.SPECIES_CODE AS spp,
  sb.SPECIMEN_SEX_CODE as sex,
  sb.n,
  me.ymean - me.xmean * sb.betastd * se.ystdev / se.xstdev  AS alpha,
  sb.betastd * se.ystdev / se.xstdev                        AS beta
INTO #coefficients
FROM 
  #standardized_beta_estimates sb
  INNER JOIN #stdev_estimates se ON 
    se.GEAR_CODE = sb.GEAR_CODE AND
    se.SPECIES_CODE = sb.SPECIES_CODE AND
    se.SPECIMEN_SEX_CODE = sb.SPECIMEN_SEX_CODE
  INNER JOIN #mean_estimates me ON
    me.GEAR_CODE = sb.GEAR_CODE AND
    me.SPECIES_CODE = sb.SPECIES_CODE AND
    me.SPECIMEN_SEX_CODE = sb.SPECIMEN_SEX_CODE
ORDER BY
  sb.GEAR_CODE,
  sb.SPECIES_CODE,
  sb.SPECIMEN_SEX_CODE

SELECT --TOP 100
  dt.*,
  EXP(cf.alpha + cf.beta * LOG(dt.BEST_LENGTH)) AS CALC_WEIGHT
INTO #calc_table
FROM
  #data_table dt INNER JOIN
  #coefficients cf ON
    dt.GEAR_CODE = cf.gear AND
    dt.SPECIES_CODE = cf.spp AND
    dt.SPECIMEN_SEX_CODE = cf.sex
WHERE
  dt.BEST_LENGTH > 0.0

-- Get the Trips and Fishing Events relevant RRF to data
-- !!! This means you can only run one species at a time. !!!
-- !!! Adding other species increases the incidence of    !!!
-- !!! non-relevant RRF catch in the weighting function.  !!!
SELECT
  ct.TRIP_ID, ct.FISHING_EVENT_ID
INTO #TRIP_EVENTS
FROM #calc_table ct
GROUP BY ct.TRIP_ID, ct.FISHING_EVENT_ID

-- Gather catches by Species
-- =========================
SELECT --TOP 20
  YEAR(B01.TRIP_START_DATE) AS TRIP_YEAR,
  CASE 
    WHEN MONTH(B01.TRIP_START_DATE) IN (1,2,3) THEN 1
    WHEN MONTH(B01.TRIP_START_DATE) IN (4,5,6) THEN 2
    WHEN MONTH(B01.TRIP_START_DATE) IN (7,8,9) THEN 3
    WHEN MONTH(B01.TRIP_START_DATE) IN (10,11,12) THEN 4
    ELSE 0 END AS TRIP_QUARTER,
  B01.TRIP_ID,
  B02.FISHING_EVENT_ID,
  B03.CATCH_ID,
  B03.SPECIES_CODE,
  COALESCE(B03.CATCH_WEIGHT,B03.CATCH_COUNT,0) AS CATCH_WEIGHT,  -- assume for general purposes that each fish weighs 1kg
  ISNULL(B03.SPECIES_CATEGORY_CODE,0) AS SPECIES_CATEGORY_CODE,
  ISNULL(B03.CATCH_VERIFICATION_CODE,0) AS CATCH_VERIFICATION_CODE
INTO #catch_species
FROM 
  #TRIP_EVENTS B00 INNER JOIN    -- !!! This means you can only run one species at a time !!!
  B01_TRIP B01 INNER JOIN
  B02_FISHING_EVENT B02 INNER JOIN
  B02L3_Link_Fishing_Event_Catch L1 INNER JOIN
  B03_CATCH B03 ON
    B03.CATCH_ID = L1.CATCH_ID ON
    L1.FISHING_EVENT_ID = B02.FISHING_EVENT_ID ON
    B02.TRIP_ID = B01.TRIP_ID ON
    B01.TRIP_ID = B00.TRIP_ID AND
    B02.FISHING_EVENT_ID = B00.FISHING_EVENT_ID
WHERE
  B03.SPECIES_CODE IN (@sppcode) AND
  B02.FE_SUB_LEVEL_ID IS NULL  -- FISHING_EVENT_ID REPEATED MANY TIMES FOR HOOKS AND TRAPS IF NOT NULL (STUPID IDEA)

-- Gather catches by Trip
SELECT
  cs.TRIP_YEAR,
  cs.TRIP_QUARTER,
  cs.TRIP_ID,
  cs.SPECIES_CODE,
  SUM(cs.CATCH_WEIGHT) AS CATCH_TRIP
INTO #catch_trip
FROM #catch_species cs
GROUP BY 
  cs.TRIP_YEAR,
  cs.TRIP_QUARTER,
  cs.TRIP_ID,
  cs.SPECIES_CODE

SELECT ct.TRIP_YEAR, ct.TRIP_QUARTER, ct.SPECIES_CODE,
   SUM(ct.CATCH_TRIP) AS CATCH_TRIP_TOTAL
INTO #catch_trip_total
FROM #catch_trip ct
GROUP BY ct.TRIP_YEAR, ct.TRIP_QUARTER, ct.SPECIES_CODE

SELECT
  ct.TRIP_YEAR,
  ct.TRIP_QUARTER,
  ct.TRIP_ID,
  ct.SPECIES_CODE,
  ct.CATCH_TRIP,
  ct.CATCH_TRIP / ctt.CATCH_TRIP_TOTAL AS TRIP_WEIGHT
INTO #catch_trip_weights
FROM
  #catch_trip ct INNER JOIN
  #catch_trip_total ctt ON
    ct.TRIP_YEAR = ctt.TRIP_YEAR AND
    ct.TRIP_QUARTER = ctt.TRIP_QUARTER AND
    ct.SPECIES_CODE = ctt.SPECIES_CODE

-- Gather catches by Fishing Event (tow)
SELECT
  cs.TRIP_YEAR,
  cs.TRIP_QUARTER,
  cs.TRIP_ID,
  cs.FISHING_EVENT_ID,
  cs.SPECIES_CODE,
  SUM(cs.CATCH_WEIGHT) AS CATCH_EVENT
INTO #catch_event
FROM #catch_species cs
GROUP BY 
  cs.TRIP_YEAR,
  cs.TRIP_QUARTER,
  cs.TRIP_ID,
  cs.FISHING_EVENT_ID,
  cs.SPECIES_CODE

SELECT ce.TRIP_YEAR, ce.TRIP_QUARTER, ce.TRIP_ID, ce.SPECIES_CODE,
   SUM(ce.CATCH_EVENT) AS CATCH_EVENT_TOTAL
INTO #catch_event_total
FROM #catch_event ce
GROUP BY ce.TRIP_YEAR, ce.TRIP_QUARTER, ce.TRIP_ID, ce.SPECIES_CODE

SELECT
  ce.TRIP_YEAR, ce.TRIP_QUARTER, ce.TRIP_ID,
  ce.FISHING_EVENT_ID,
  ce.SPECIES_CODE,
  ce.CATCH_EVENT,
  --cet.CATCH_EVENT_TOTAL,
  ce.CATCH_EVENT / cet.CATCH_EVENT_TOTAL AS EVENT_WEIGHT
INTO #catch_event_weights
FROM
  #catch_event ce INNER JOIN
  #catch_event_total cet ON
    ce.TRIP_YEAR = cet.TRIP_YEAR AND
    ce.TRIP_QUARTER = cet.TRIP_QUARTER AND
    ce.TRIP_ID = cet.TRIP_ID AND
    ce.SPECIES_CODE = cet.SPECIES_CODE

-- Start PJS calculations of mean weight
-- =====================================

-- PJS Algorithm Step 2 - calculate mean weight by sex for each sample (~= tow)
SELECT
  ct.GEAR_CODE,
  ct.SPECIES_CODE,
  ct.SPECIMEN_SEX_CODE,
  ct.TRIP_YEAR,
  ct.TRIP_QUARTER,
  ct.TRIP_ID,
  ct.FISHING_EVENT_ID,
  ct.SAMPLE_ID,
  AVG(ct.CALC_WEIGHT) AS MEAN_SAMPLE_WEIGHT
INTO #mean_sample_weight
FROM #calc_table ct
GROUP BY
  ct.GEAR_CODE,
  ct.SPECIES_CODE,
  ct.SPECIMEN_SEX_CODE,
  ct.TRIP_YEAR,
  ct.TRIP_QUARTER,
  ct.TRIP_ID,
  ct.FISHING_EVENT_ID,
  ct.SAMPLE_ID

-- PJS Algorithm Step 3 - calculate mean weight by sex and trip from samples on the same trip, weighted by tow weight of RRF
SELECT
  msw.GEAR_CODE,
  msw.SPECIES_CODE,
  msw.SPECIMEN_SEX_CODE,
  msw.TRIP_YEAR,
  msw.TRIP_QUARTER,
  msw.TRIP_ID,
  --msw.MEAN_SAMPLE_WEIGHT,
  --cew.EVENT_WEIGHT,
  SUM(msw.MEAN_SAMPLE_WEIGHT * cew.EVENT_WEIGHT) AS MEAN_TRIP_WEIGHT
INTO #mean_trip_weight
FROM
  #mean_sample_weight msw INNER JOIN
  #catch_event_weights cew ON
    msw.TRIP_ID = cew.TRIP_ID AND
    msw.FISHING_EVENT_ID = cew.FISHING_EVENT_ID AND
    msw.SPECIES_CODE = cew.SPECIES_CODE
GROUP BY
  msw.GEAR_CODE,
  msw.SPECIES_CODE,
  msw.SPECIMEN_SEX_CODE,
  msw.TRIP_YEAR,
  msw.TRIP_QUARTER,
  msw.TRIP_ID

-- PJS Algorithm Step 4 - calculate mean weight by sex and quarter, weighted by trip catch of RRF
SELECT
  mtw.GEAR_CODE,
  mtw.SPECIES_CODE,
  mtw.SPECIMEN_SEX_CODE,
  mtw.TRIP_YEAR,
  mtw.TRIP_QUARTER,
  SUM(mtw.MEAN_TRIP_WEIGHT * ctw.TRIP_WEIGHT) AS MEAN_QUARTER_WEIGHT
INTO #mean_quarter_weight
FROM
  #mean_trip_weight mtw INNER JOIN
  #catch_trip_weights ctw ON
    mtw.TRIP_YEAR = ctw.TRIP_YEAR AND
    mtw.TRIP_QUARTER = ctw.TRIP_QUARTER AND
    mtw.TRIP_ID = ctw.TRIP_ID AND
    mtw.SPECIES_CODE = ctw.SPECIES_CODE
GROUP BY
  mtw.GEAR_CODE,
  mtw.SPECIES_CODE,
  mtw.SPECIMEN_SEX_CODE,
  mtw.TRIP_YEAR,
  mtw.TRIP_QUARTER

-- PJS Algorithm Step 5 - calculate mean weight by year, averaging across sex and quarter (without weights)
SELECT
  mqw.GEAR_CODE,
  mqw.SPECIES_CODE,
  mqw.TRIP_YEAR,
  AVG(mqw.MEAN_QUARTER_WEIGHT) AS MEAN_YEAR_WEIGHT
INTO #mean_year_weight
FROM #mean_quarter_weight mqw
GROUP BY
  mqw.GEAR_CODE,
  mqw.SPECIES_CODE,
  mqw.TRIP_YEAR

-- PJS Algorithm Step 6 - calculate single mean weight for all years (equal weight for each year)
SELECT
  myw.GEAR_CODE,
  myw.SPECIES_CODE,
  AVG(myw.MEAN_YEAR_WEIGHT)/1000. AS MEAN_WEIGHT -- in kg
INTO #mean_weight
FROM #mean_year_weight myw
GROUP BY
  myw.GEAR_CODE,
  myw.SPECIES_CODE

--SELECT * FROM #calc_table
--SELECT * FROM #mean_sample_weight
--SELECT * FROM #catch_trip_weights
--SELECT * FROM #mean_quarter_weight
SELECT * FROM #mean_weight

--qu("lm.sql",dbName="GFBioSQL",strSpp=c("222","401","405","418","602","621"))
--qu("gfb_mean_weight.sql",dbName="GFBioSQL",strSpp="442",major=3:9)
--qu("gfb_mean_weight.sql",dbName="GFBioSQL",strSpp="440",major=3:9)
--qu("gfb_mean_weight.sql",dbName="GFBioSQL",strSpp="228",major=1)
--qu("gfb_mean_weight.sql",dbName="GFBioSQL",strSpp="439",major=3:9)
--qu("gfb_mean_weight.sql",dbName="GFBioSQL",strSpp="410",major=3:9)
--qu("gfb_mean_weight.sql",dbName="GFBioSQL",strSpp="417",major=3:9)
--qu("gfb_mean_weight.sql",dbName="GFBioSQL",strSpp="435",major=3:9)  -- BOR (180912, 190705)
--qu("gfb_mean_weight.sql",dbName="GFBioSQL",strSpp="437",major=3:9)  -- CAR (180622, 200720)
--qu("gfb_mean_weight.sql",dbName="GFBioSQL",strSpp="440",major=3:9)  -- YMR (180925, 200720)
--qu("gfb_mean_weight.sql",dbName="GFBioSQL",strSpp="453",major=3:4)  -- LST (220309)
