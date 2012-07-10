-- Mean weight of species sampled from commerical and research trips.
SET NOCOUNT ON

SELECT 
  CAST(SP.SPECIES_CODE AS CHAR(3)) AS spp,
  (CASE 
    WHEN SA.TRIP_SUB_TYPE_CODE IN (1,4) THEN 'Com'
    WHEN SA.TRIP_SUB_TYPE_CODE IN (2,3) THEN 'Res'
   ELSE 'Unk' END) AS ttype,
  --SUM(SP.Round_Weight) AS totwt, 
  AVG(SP.Round_Weight)/1000. AS mnwt,
  COUNT(SP.Round_Weight) AS n 
FROM 
  B21_Samples SA INNER JOIN 
  B22_Specimens SP ON 
  SA.SAMPLE_ID = SP.SAMPLE_ID 
WHERE 
  SA.SAMPLE_TYPE_CODE IN (1,2,6,7) AND
  IsNull(SP.Round_Weight,0)>0 AND
  SP.SPECIES_CODE IN (@sppcode)  -- comment this line (and 'AND' above) out to get all species
GROUP BY SP.SPECIES_CODE,
  CASE 
    WHEN SA.TRIP_SUB_TYPE_CODE IN (1,4) THEN 'Com'
    WHEN SA.TRIP_SUB_TYPE_CODE IN (2,3) THEN 'Res'
    ELSE 'Unk' END
ORDER BY SP.SPECIES_CODE,
  CASE 
    WHEN SA.TRIP_SUB_TYPE_CODE IN (1,4) THEN 'Com'
    WHEN SA.TRIP_SUB_TYPE_CODE IN (2,3) THEN 'Res'
    ELSE 'Unk' END

-- getData("gfb_fishwt.sql",dbName="GFBioSQL",strSpp="424")


