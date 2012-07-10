SET NOCOUNT ON

SELECT 
  Year(SA.SAMPLE_DATE) AS [year],
  SP.Round_Weight AS wt,
  SP.SPECIMEN_AGE AS age,
  SA.MAJOR_STAT_AREA_CODE AS major, 
  SA.SRF_Area AS srfa 
FROM 
  B21_Samples SA INNER JOIN 
  B22_Specimens SP ON 
  SA.SAMPLE_ID = SP.SAMPLE_ID
WHERE 
  SP.Round_Weight Is Not Null AND 
  SP.SPECIMEN_AGE Is Not Null AND 
  SA.SPECIES_CODE=@sppcode 

