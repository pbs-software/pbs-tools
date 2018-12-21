--https://stackoverflow.com/questions/13953134/convert-access-transform-pivot-query-to-sql-server
SET NOCOUNT ON

DECLARE @query AS NVARCHAR(MAX);
DECLARE @cols AS NVARCHAR(MAX)

SELECT @cols = STUFF((SELECT distinct ',' + QUOTENAME(AGEING_METHOD_DESC)
  FROM AGEING_METHOD
  FOR XML PATH(''), TYPE ).value('.', 'NVARCHAR(MAX)') ,1,1,'')

--TRANSFORM Max(GFB_B05_SPECIMEN.SPECIMEN_AGE) AS MaxOfSPECIMEN_AGE
SELECT --TOP 1000
  B03.SPECIES_CODE,
  S.Common_Name,
  AM.AGEING_METHOD_DESC,
  B05.SPECIMEN_AGE
--  MAX(B05.SPECIMEN_AGE) AS MAX_AGE
INTO #fornow
FROM 
  PacHarvest.dbo.C_Species S INNER JOIN
  B03_CATCH B03 INNER JOIN 
  B03L4_Link_Catch_Sample L34 INNER JOIN
  B04_SAMPLE B04 INNER JOIN
  B05_SPECIMEN B05 INNER JOIN
  AGEING_METHOD AM
    ON AM.AGEING_METHOD_CODE = B05.AGEING_METHOD_CODE
    ON B05.SAMPLE_ID = B04.SAMPLE_ID
    ON B04.SAMPLE_ID = L34.SAMPLE_ID
    ON L34.CATCH_ID  = B03.CATCH_ID
    ON B03.SPECIES_CODE = S.SPECIES_CDE
WHERE
  B05.SPECIMEN_AGE Is Not Null AND 
  (S.Rockfish=1 OR
  S.SPECIES_CDE IN ('228'))

--select(@cols) --can see the variable selection

SET @query = '
  SELECT * from #fornow F
  PIVOT (MAX(F.SPECIMEN_AGE) for F.AGEING_METHOD_DESC IN ('+@cols+')) P'

EXECUTE(@query)
--Seems you have to build a query and then execute it. 
--Expansion of @cols does not appear to work otherwise

-- @cols :
--[ANAL FIN],[Dorsal Fin XS],[DORSAL SPINE],[LENGTH],[Otolith (Unknown)],[Otolith Broken and Baked (Break and Bake)],[Otolith Broken and Burnt],[Otolith Burnt and Thin Sectioned],[Otolith Surface Only],[Otolith Thin Sectioned],[PECTORAL FIN],[PELVIC FIN],[PRE-OPERCULAR],[SCALE],[Surface And Broken/Burnt],[UNKNOWN FIN],[Unknown],[VERTEBRAE]

--PIVOT (MAX(F.SPECIMEN_AGE) for F.AGEING_METHOD_DESC IN ([Otolith Broken and Baked (Break and Bake)],[Otolith Broken and Burnt])) P



