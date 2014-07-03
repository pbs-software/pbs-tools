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
  SPPCAT = Sum( CASE CC.Species
    WHEN @sppcode THEN CC.Catch/2.20459  -- convert lbs to kg
    ELSE 0 END ),
  SPPDIS = Sum( CASE 
    WHEN CC.Species=@sppcode AND CC.Utilization>1 THEN CC.Catch/2.20459  -- convert lbs to kg
    ELSE 0 END ),
  TOTCAT = Sum(CC.Catch/2.20459)  -- convert lbs to kg
INTO #Catch
FROM 
  B3_Catch CC
GROUP BY 
  CC.Trip, CC.Event

-- Link all catch events above to fishing events
SELECT 
  CASE 
    WHEN T.Source IN (1,2) THEN 1
    WHEN T.Source IN (3,4) THEN 5
    WHEN T.Source IN (5,6) THEN 3
    ELSE 0 END AS FID,
  IsNull(T.TRIP_ID,0) AS TID,
  'FEID'=0,
  C.Trip AS hail,
  C.Event AS [set],
  CASE T.Log_Source   -- Change to codes used by FOS
    WHEN 1 THEN 105   -- fishing log
    WHEN 2 THEN 106   -- at-sea observer log
    WHEN 3 THEN 1     -- observer
   ELSE 0 END AS [log],
  ISNULL(COALESCE(-(E.Lon_Deg_Start+E.Lon_Min_Start/60),-(E.Lon_Deg_End+E.Lon_Min_End/60)),Null) AS X, 
  ISNULL(COALESCE(E.Lat_Deg_Start+E.Lat_Min_Start/60,E.Lat_Deg_End+E.Lat_Min_End/60),Null) AS Y, 
  ISNULL(T.Date,Null) AS [date], 
  ISNULL(Year(T.Date),9999) AS [year],
  IsNull(E.Major_Area,0) AS major,
  IsNull(E.Minor_Area,0) AS minor,
  IsNull(E.Locality,0)   AS locality,
  pfma=0,
  pfms=0,
  COALESCE(E.Avg_Depth,E.Min_Depth,E.Max_Depth)*1.8288 AS depth, -- convert fathoms to metres
  IsNull(T.Vessel,0) AS vessel,
  CASE 
    WHEN E.gear IN (1,2,3,4,5,6,9) THEN 1  -- bottom trawl
    WHEN E.gear IN (8) THEN 3              -- midwater trawl
    ELSE 0 END AS gear,
  success=0,
  ISNULL(E.Time,0)*60. AS effort,  -- convert hours to minutes
  C.SPPCAT AS catKg,
  C.SPPDIS/C.SPPCAT AS pdis,  -- proportion discarded
  C.SPPCAT/C.TOTCAT AS pcat   -- proportion of total catch
FROM 
  #Trips T RIGHT OUTER JOIN 
  (B2_Events E RIGHT OUTER JOIN 
  #Catch C ON
  E.Trip = C.Trip AND 
  E.Event = C.Event ) ON 
  T.Trip = E.Trip 
WHERE
  C.SPPCAT > 0  -- select only positive catches for the chosen species
ORDER BY
  C.Trip, C.Event

-- getData("gfc_catch_records.sql","GFCatch",strSpp="401")

