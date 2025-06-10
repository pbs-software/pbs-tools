-- Mean weight of species sampled from commerical and research trips.
-- Last modified 250528
SET NOCOUNT ON

SELECT --TOP 100
  CAST(SP.SPECIES_CODE AS VARCHAR(3)) AS spp,
  (CASE 
    WHEN SA.TRIP_SUB_TYPE_CODE IN (1,4) THEN 'Com'
    WHEN SA.TRIP_SUB_TYPE_CODE IN (2,3) THEN 'Res'
   ELSE 'Unk' END) AS ttype,
  --SUM(SP.Round_Weight) AS totwt, 
  CAST(AVG(SP.Round_Weight/1000.) AS REAL) AS mnwt,  -- need to average wts/1000; averaging wts and then dividing by 1000 causes conversion issues
  COUNT(SP.Round_Weight) AS n 
FROM 
  B21_Samples SA INNER JOIN 
  B22_Specimens SP ON 
  SA.SAMPLE_ID = SP.SAMPLE_ID 
WHERE 
  SA.SAMPLE_TYPE_CODE IN (1,2,6,7)
  AND IsNull(SP.Round_Weight,0) >= 1   -- for some reason >0 includes 0 values in B22
  --AND SP.SPECIES_CODE IN (@sppcode)  -- comment this line out to get all species
GROUP BY
  CAST(SP.SPECIES_CODE AS VARCHAR(3)),
  CASE 
    WHEN SA.TRIP_SUB_TYPE_CODE IN (1,4) THEN 'Com'
    WHEN SA.TRIP_SUB_TYPE_CODE IN (2,3) THEN 'Res'
    ELSE 'Unk' END
ORDER BY
  CAST(SP.SPECIES_CODE AS VARCHAR(3)),
  CASE 
    WHEN SA.TRIP_SUB_TYPE_CODE IN (1,4) THEN 'Com'
    WHEN SA.TRIP_SUB_TYPE_CODE IN (2,3) THEN 'Res'
    ELSE 'Unk' END

-- qu("gfb_fishwt.sql",dbName="GFBioSQL",strSpp="044",as.is=c(T,F,F,F))

