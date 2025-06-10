SET NOCOUNT ON 

SELECT  -- inline view calculates empirical mean weights for species (in lbs and/or kg)
  C.SPECIES_CODE AS spp,
  --C.WEIGHT_UNIT_CODE AS unit,
  ISNULL(C.UTILIZATION_CODE,0) AS ucode,
  ISNULL(U.UTILIZATION_DESCRIPTION,'Unknown') AS udesc,
  SUM(CASE 
    WHEN C.WEIGHT_UNIT_CODE IN ('PND') THEN C.CATCH_WEIGHT/2.20459
    ELSE C.CATCH_WEIGHT END) AS Swt, -- sum of weights
  SUM(C.CATCH_COUNT) AS Sn,          -- sum of numbers (of specimens)
  SUM(CASE 
    WHEN C.WEIGHT_UNIT_CODE IN ('PND') THEN C.CATCH_WEIGHT/2.20459
    ELSE C.CATCH_WEIGHT END) /  SUM(C.CATCH_COUNT) AS Swt_Sn, -- sum of weights over sum of numbers
  AVG(CASE 
    WHEN C.WEIGHT_UNIT_CODE IN ('PND') THEN (C.CATCH_WEIGHT/2.20459) / C.CATCH_COUNT
    ELSE CAST(ROUND(C.CATCH_WEIGHT,7) AS NUMERIC(15,7)) / CAST(ROUND(C.CATCH_COUNT,7) AS NUMERIC(15,7)) END) AS M_wt_n  -- average of ratios wt/n
  FROM
    GF_FE_CATCH C LEFT OUTER JOIN
    UTILIZATION U ON 
      C.UTILIZATION_CODE = U.UTILIZATION_CODE
  WHERE
    (C.CATCH_WEIGHT>0 And C.CATCH_WEIGHT Is Not Null)
    AND (C.CATCH_COUNT>1 And C.CATCH_COUNT Is Not Null)  -- only calculate means from records with more than one fish
    AND C.WEIGHT_UNIT_CODE IN ('PND','KGM')
    --AND C.SPECIES_CODE IN (@sppcode)  -- comment this out to get all species
  GROUP BY C.SPECIES_CODE, ISNULL(C.UTILIZATION_CODE,0), ISNULL(U.UTILIZATION_DESCRIPTION,'Unknown') --, C.WEIGHT_UNIT_CODE
  ORDER BY C.SPECIES_CODE, ISNULL(C.UTILIZATION_CODE,0), ISNULL(U.UTILIZATION_DESCRIPTION,'Unknown') --, C.WEIGHT_UNIT_CODE

--qu("fos_fishwt.sql",dbName="GFFOS",strSpp=c("222","401","405","418","602","621"))
