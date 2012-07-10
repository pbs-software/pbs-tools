-- Catch data from GFCatch for use in a GLM
SET NOCOUNT ON 

-- Get Trip_ID and possible links to GFCatch's trip table
SELECT 
  BT.TRIP_ID,
  BT.TRIP_END_DATE,
  'Trip_Length'=CAST(BT.TRIP_END_DATE-BT.TRIP_START_DATE as INT)
INTO #GFBGFC
FROM GFBioSQl.dbo.B01_TRIP BT

SELECT 
  TT.*,
  BCT.TRIP_ID
INTO #Trips
FROM 
  B1_Trips TT LEFT OUTER JOIN 
  #GFBGFC BCT ON 
    TT.Date = BCT.TRIP_END_DATE AND
    TT.Trip_Length = BCT.Trip_Length

-- Create a temporary table with species catch and total catch
SELECT 
  CC.Trip, 
  CC.Event, 
  SPPCAT = Sum( CASE 
    WHEN CC.Species IN (@sppcode) THEN CC.Catch/2.20459  -- convert lbs to kg
    ELSE 0 END ),
  SPPDIS = Sum( CASE 
    WHEN CC.Species IN (@sppcode) AND CC.Utilization>1 THEN CC.Catch/2.20459  -- convert lbs to kg
    ELSE 0 END ),
  TARCAT = Sum( CASE  -- target or bycatch
    WHEN CC.Species IN (@tarcode) THEN CC.Catch/2.20459  -- convert lbs to kg
    ELSE 0 END ),
  TOTCAT = Sum(CC.Catch/2.20459)  -- convert lbs to kg
INTO #Catch
FROM 
  B3_Catch CC
GROUP BY 
  CC.Trip, CC.Event

-- Link all catch events above to fishing events
SELECT -- TOP 20
  IsNull(T.TRIP_ID,0) AS TID,
  CASE 
    WHEN T.Source IN (1,2) THEN 1
    WHEN T.Source IN (3,4) THEN 5
    WHEN T.Source IN (5,6) THEN 3
    ELSE 0 END AS FID,
  CASE T.Log_Source   -- Change to codes used by FOS
    WHEN 1 THEN 105   -- fishing log
    WHEN 2 THEN 106   -- at-sea observer log
    WHEN 3 THEN 1     -- observer
    ELSE 0 END AS [log],
  ISNULL(T.Date,Null) AS [date], 
  ISNULL(Year(T.Date),9999) AS [year],
  IsNull(Month(T.Date),0) AS [month],
  COALESCE(E.Avg_Depth,E.Min_Depth,E.Max_Depth)*1.8288 AS depth, -- convert fathoms to metres
  IsNull(E.Major_Area,0)  AS major,
  IsNull(E.Minor_Area,0)  AS minor,
  IsNull(E.Locality,0)    AS locality,
  ISNULL(E.Gear,0) AS gear, -- need original gear codes for rollups by location, gear and depth
--  CASE
--    WHEN E.Gear IN (1,2,3,4,5,9) THEN 1
--    WHEN E.Gear IN (6,7,8) THEN 3
--    ELSE 0 END AS gear,
  ISNULL(COALESCE(E.Lat_Deg_Start+E.Lat_Min_Start/60,E.Lat_Deg_End+E.Lat_Min_End/60),Null) AS latitude, 
  IsNull(T.Vessel,0) AS vessel,
  ISNULL(E.Time,0)*60. AS effort, -- convert hours to minutes
  C.SPPCAT AS catch,              -- landed + discard
--  C.SPPDIS AS discard,            -- discarded
--  C.SPPCAT/C.TOTCAT AS pcat,      -- proportion of total catch
  C.TARCAT AS bycatch,            -- proportion of total catch
  CASE WHEN T.Source IN (1,3,5) AND E.Time>0 THEN 'TRUE' ELSE 'FALSE' END AS use4cpue
FROM 
  #Trips T RIGHT OUTER JOIN 
  (B2_Events E RIGHT OUTER JOIN 
  #Catch C ON
  E.Trip = C.Trip AND 
  E.Event = C.Event ) ON 
  T.Trip = E.Trip 
WHERE
  C.TOTCAT > 0 AND  -- select only positive total catch
  E.Time > 0 
ORDER BY
  T.Date --C.Trip, C.Event

-- getData("gfc_glm.sql","GFCatch",strSpp="440",tarSpp="396",noLogicals=FALSE,path=.getSpath())

