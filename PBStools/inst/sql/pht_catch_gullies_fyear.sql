SET NOCOUNT ON

SELECT 
  CC.fyear, 
  SUM(CASE RM WHEN 'GS' THEN (CC.LANDED + CC.DISCARDED) END) / 1000 AS 'GS', 
  SUM(CASE RM WHEN 'MI' THEN (CC.LANDED + CC.DISCARDED) END) / 1000 AS 'MI', 
  SUM(CASE RM WHEN 'MR' THEN (CC.LANDED + CC.DISCARDED) END) / 1000 AS 'MR', 
  SUM(CASE RM WHEN 'UNK' THEN (CC.LANDED + CC.DISCARDED) END) / 1000 AS 'UNK' 
FROM 
  (SELECT
    cast(CASE WHEN E.Fishing_Year = '97' THEN '19967' 
      WHEN (E.Fishing_Year IS NULL) 
      THEN '9999' ELSE E.Fishing_Year END AS VARCHAR) AS fyear, 
    E.SRF_Subarea, 
    C.SPECIES_CODE, 
    C.LANDED, C.DISCARDED, 
    RM = CASE E.SRF_Subarea 
      WHEN 'GS' THEN 'GS' 
      WHEN 'MI' THEN 'MI' 
      WHEN 'MR' THEN 'MR' 
      ELSE 'UNK' END 
  FROM 
    B3_Fishing_Events E RIGHT OUTER JOIN 
    D_Merged_Catches C ON 
    E.OBFL_HAIL_IN_NO = C.HAIL_IN_NO AND 
    E.OBFL_SET_NO = C.SET_NO 
  WHERE 
    C.SPECIES_CODE = @sppcode AND 
    E.SRF_Subarea IS NOT NULL) AS CC 
GROUP BY fyear 
ORDER BY fyear;
