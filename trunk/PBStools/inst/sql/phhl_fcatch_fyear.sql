-- PacHarvHL query to summarize fisherlog catch by PMFC area
-- fisheryid: 2=Halibut, 3=Sablefish, 4=Schdeule II, 5=ZN
--            6=Sablefish+ZN, 7=Sablefish+Halibut, 8=Dogfish, 9=Lingcod

DECLARE @logtype VARCHAR(10)
SET @logtype   = @logtypeval

SET NOCOUNT ON -- prevents timeout errors

-- create an events table based on unique hails
SELECT
  E.OBFL_HAIL_IN_NO, E.OBFL_LOG_TYPE_CDE, E.OBFL_SET_NO,
  CAST( COALESCE(T.OBFL_OFFLOAD_DT,E.OBFL_START_DT,E.Start_FE,E.OBFL_END_DT,E.OBFL_LOADED_DT) AS smalldatetime) AS Date,
  E.OBFL_MAJOR_STAT_AREA_CDE as major, 
  RM = CASE E.OBFL_MAJOR_STAT_AREA_CDE 
    WHEN 1 THEN '4B' 
    WHEN 3 THEN '3C' 
    WHEN 4 THEN '3D' 
    WHEN 5 THEN '5A' 
    WHEN 6 THEN '5B' 
    WHEN 7 THEN '5C' 
    WHEN 8 THEN '5D' 
    WHEN 9 THEN '5E' 
    ELSE 'UNK' END 
INTO #Events 
FROM 
  B2_Trips T RIGHT OUTER JOIN 
  B3_Fishing_Events E ON
  T.OBFL_HAIL_IN_NO = E.OBFL_HAIL_IN_NO AND 
  T.OBFL_LOG_TYPE_CDE = E.OBFL_LOG_TYPE_CDE 
WHERE 
  E.OBFL_LOG_TYPE_CDE IN (@logtype) AND
  E.OBFL_FISHERY_ID IN (@fisheryid)

-- summarize catch by year (rows) and pmfc major area (columns), i.e., crosstab
SELECT 
  CC.fyear, 
  SUM(CASE RM WHEN '3C' THEN (CC.catch) END) / 1000 AS '3C', 
  SUM(CASE RM WHEN '3D' THEN (CC.catch) END) / 1000 AS '3D', 
  SUM(CASE RM WHEN '4B' THEN (CC.catch) END) / 1000 AS '4B', 
  SUM(CASE RM WHEN '5A' THEN (CC.catch) END) / 1000 AS '5A', 
  SUM(CASE RM WHEN '5B' THEN (CC.catch) END) / 1000 AS '5B', 
  SUM(CASE RM WHEN '5C' THEN (CC.catch) END) / 1000 AS '5C', 
  SUM(CASE RM WHEN '5D' THEN (CC.catch) END) / 1000 AS '5D', 
  SUM(CASE RM WHEN '5E' THEN (CC.catch) END) / 1000 AS '5E', 
  SUM(CASE RM WHEN 'UNK' THEN (CC.catch) END) / 1000 AS 'UNK' 
FROM 
  (SELECT
    CAST(CASE 
      WHEN Year(V.Date) < 1997 OR Month(V.Date) > 3 THEN Year(V.Date) 
      WHEN Year(V.Date) = 1997 AND Month(V.Date) <= 3 THEN '19967' 
      WHEN Year(V.Date) > 1997 AND Month(V.Date) <= 3 THEN Year(V.Date)-1 
      WHEN V.Date IS NULL THEN '9999' END AS VARCHAR) AS fyear, 
    C.OBFL_SPECIES_CDE, V.major, 
    C.OBFL_EST_WEIGHT AS catch, 
    V.RM
  FROM 
    #Events V RIGHT OUTER JOIN 
    B4_Catches C ON 
    V.OBFL_HAIL_IN_NO = C.OBFL_HAIL_IN_NO AND 
    V.OBFL_LOG_TYPE_CDE = C.OBFL_LOG_TYPE_CDE AND 
    V.OBFL_SET_NO = C.OBFL_SET_NO
  WHERE 
    C.OBFL_SPECIES_CDE = @sppcode ) AS CC
GROUP BY fyear 
ORDER BY fyear

-- getData("phhl_fcatch_fyear.sql","PacHarvHL",strSpp="394",fisheryid=4)

