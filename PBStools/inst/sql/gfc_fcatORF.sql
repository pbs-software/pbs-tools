-- Query GFCatch landings for rockfish reconstruction.
-- Last modified: 2014-10-15

SET NOCOUNT ON  -- prevents timeout errors

-- Create a temporary table with species catch and total catch
SELECT 
  CC.Trip, 
  CC.Event, 
  Sum(CASE
    WHEN CC.Species IN (@sppcode) AND CC.Utilization NOT IN (4,6)
    THEN CC.Catch
    ELSE 0 END)/2.20459 AS \"landed\",
  Sum(CASE
    WHEN CC.Species IN (@sppcode) AND CC.Utilization IN (4,6)
    THEN CC.Catch
    ELSE 0 END)/2.20459 AS \"discard\",
  Sum(CASE
    WHEN CC.Species IN ('396')    AND CC.Utilization NOT IN (4,6)
    THEN CC.Catch
    ELSE 0 END)/2.20459 AS POP,
  Sum(CASE
    WHEN CC.Species IN (@orfcode) AND CC.Utilization NOT IN (4,6)
    THEN CC.Catch
    ELSE 0 END)/2.20459 AS ORF,
  Sum(CASE
    WHEN CC.Species IN ('614')    AND CC.Utilization NOT IN (4,6)
    THEN CC.Catch
    ELSE 0 END)/2.20459 AS PAH,
  Sum(CASE
    WHEN CC.Species IN ('454','455')  AND CC.Utilization NOT IN (4,6)
    THEN CC.Catch
    ELSE 0 END)/2.20459 AS SBF,
  Sum(CASE
    WHEN CC.Species IN ('042','044','467')  AND CC.Utilization NOT IN (4,6)
    THEN CC.Catch
    ELSE 0 END)/2.20459 AS DOG,
  Sum(CASE
    WHEN CC.Species IN ('424','407','431','433','442')  AND CC.Utilization NOT IN (4,6)
    THEN CC.Catch
    ELSE 0 END)/2.20459 AS RFA
  INTO #Catch
  FROM 
    B3_Catch CC
  GROUP BY 
    CC.Trip, CC.Event

-- Link all catch events above to fishing events
SELECT 
  'fid' = (CASE 
    WHEN T.Source IN (1,2) THEN 1
    WHEN T.Source IN (3,4) THEN 5
    WHEN T.Source IN (5,6) THEN 3
    ELSE 0 END),
  'log'  = (CASE
    WHEN T.Log_Source IN (2,3) THEN 1  -- observer logs
    WHEN T.Log_Source IN (1) THEN 2    -- fisher logs
    ELSE 0 END),
  'gear' = ISNULL(E.Gear,0),
  --'date' = ISNULL(T.Date,Null), 
  'date' = CONVERT(char(10),ISNULL(T.Date,Null),20), 
  'major' = IsNull(E.Major_Area,0),
  'minor' = IsNull(E.Minor_Area,0),
  'locality' = IsNull(E.Locality,0),
  C.landed, C.discard, 
  C.POP, C.ORF, C.PAH, C.SBF, C.DOG, C.RFA
FROM 
  B1_Trips T RIGHT OUTER JOIN 
  (B2_Events E RIGHT OUTER JOIN 
  #Catch C ON
  E.Trip = C.Trip AND 
  E.Event = C.Event ) ON 
  T.Trip = E.Trip 
  WHERE
    C.\"landed\">0 OR C.\"discard\">0 OR C.POP>0 OR C.ORF>0 OR C.PAH>0 OR C.SBF>0 OR C.DOG>0 OR C.RFA>0


-- getData("gfc_fcatORF.sql","GFCatch",strSpp="396")
-- qu("gfc_fcatORF.sql",dbName="GFCatch",strSpp="418")


