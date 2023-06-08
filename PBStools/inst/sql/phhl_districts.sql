-- Calculate proportion of PMFC major areas in each historical district. (2010-07-13)
SET NOCOUNT ON -- prevents timeout errors

SELECT
  HYD.Year AS 'year',
  HAC.Area AS 'region',
  CASE 
    WHEN HAC.Area IN ('28','29','29A','29B','29C','29D','29E') THEN 'D1'
    WHEN HAC.Area IN ('1','2','3','4','5','6','7','8','9','10','2E','2W','30') THEN 'D2'
    WHEN HAC.Area IN ('11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27') THEN 'D3'
    ELSE 'D0' END AS 'district',
  CASE
    WHEN HAC.Area IN ('12','13','14','15','16','17','18','19','20','28','29','29A','29B','29C','29D','29E') THEN 1
    WHEN HAC.Area IN ('C') THEN 2
    WHEN HAC.Area IN ('21','22','23','24') THEN 3
    WHEN HAC.Area IN ('25','26','27') THEN 4
    WHEN HAC.Area IN ('10','11') THEN 5
    WHEN HAC.Area IN ('7','8','9','30') THEN 6
    WHEN HAC.Area IN ('6','2E') THEN 7
    WHEN HAC.Area IN ('1','3','4','5') THEN 8
    WHEN HAC.Area IN ('2','2W') THEN 9
    ELSE 0 END AS 'major',
  HAC.[Round_Wt(lbs)]/2.20459 AS 'catKg'
INTO #Catch
FROM
  B21_Historic_Year_Details HYD INNER JOIN
  B22_Historic_Area_Catch HAC ON
    HYD.Id = HAC.Id
WHERE
  --HYD.Year IN ('1953','1954','1955')
  HYD.Year IN ('1951','1952')

SELECT
  C.district,
  Sum(C.catKg) AS 'dcatKg'
INTO #Dcatch
FROM #Catch C
GROUP BY C.district

SELECT
  C.district,
  C.major,
  Sum(C.catKg) / Avg(D.dcatKg) AS 'dmajor'
FROM
  #Catch  C INNER JOIN
  #Dcatch D ON
    C.district = D.district
GROUP BY
  C.district, C.major
ORDER BY
  C.district, C.major

-- qu("phhl_districts.sql",dbName="PacHarvHL",strSpp="389")



