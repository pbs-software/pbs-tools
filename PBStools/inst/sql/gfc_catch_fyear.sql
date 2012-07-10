SET NOCOUNT ON

SELECT 
  CC.fyear, 
  SUM(CASE RM WHEN '3C' THEN CC.Catch END) / 2204.59 AS '3C', 
  SUM(CASE RM WHEN '3D' THEN CC.Catch END) / 2204.59 AS '3D', 
  SUM(CASE RM WHEN '4B' THEN CC.Catch END) / 2204.59 AS '4B', 
  SUM(CASE RM WHEN '5A' THEN CC.Catch END) / 2204.59 AS '5A', 
  SUM(CASE RM WHEN '5B' THEN CC.Catch END) / 2204.59 AS '5B', 
  SUM(CASE RM WHEN '5C' THEN CC.Catch END) / 2204.59 AS '5C', 
  SUM(CASE RM WHEN '5D' THEN CC.Catch END) / 2204.59 AS '5D', 
  SUM(CASE RM WHEN '5E' THEN CC.Catch END) / 2204.59 AS '5E', 
  SUM(CASE RM WHEN 'UNK' THEN CC.Catch END) / 2204.59 AS 'UNK' 
FROM 
  (SELECT
    CAST(CASE 
       WHEN Year(T.Date) < 1997 OR Month(T.Date) > 3 THEN Year(T.Date) 
       WHEN Year(T.Date) = 1997 AND Month(T.Date) <= 3 THEN '19967' 
       WHEN Year(T.Date) > 1997 AND Month(T.Date) <= 3 THEN Year(T.Date)-1 
       WHEN T.Date IS NULL THEN '9999' END AS VARCHAR) AS fyear, 
    E.Major_Area, 
    C.Catch, 
    RM = CASE 
      WHEN E.Major_Area = 1 THEN '4B' 
      WHEN E.Major_Area = 3 THEN '3C' 
      WHEN E.Major_Area = 4 THEN '3D' 
      WHEN E.Major_Area = 5 THEN '5A' 
      WHEN E.Major_Area = 6 THEN '5B' 
      WHEN E.Major_Area = 7 THEN '5C' 
      WHEN E.Major_Area = 8 THEN '5D' 
      WHEN E.Major_Area = 9 THEN '5E' 
      ELSE 'UNK' END 
  FROM 
    B1_Trips T RIGHT OUTER JOIN
    (B2_Events E RIGHT OUTER JOIN 
    B3_Catch C ON 
    E.Trip = C.Trip AND 
    E.Event = C.Event) ON 
    T.Trip = E.Trip
  WHERE 
    C.SPECIES = @sppcode) AS CC
GROUP BY fyear 
ORDER BY fyear

