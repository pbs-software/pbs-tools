-- 2010-03-24 RH
-- Now appears as a View table called 'GFFOS.MEAN_SPECIES_WEIGHT_VW' on the Oracle server GFSH (contact Norm Olsen).
SELECT
  MW.spp as \"spp\",
  Avg(CASE
    WHEN MW.unit='PND' THEN MW.mnwt/2.20459  -- standardises mean species weight in kg
    ELSE MW.mnwt END) as \"mnwt\",
  Sum(MW.n) as \"n\"
FROM
  (SELECT  -- inline view calculates empirical mean weights for species (in lbs and/or kg)
    C.SPECIES_CODE AS spp,
    C.WEIGHT_UNIT_CODE AS unit,
    Sum(C.CATCH_WEIGHT) AS wt,
    Sum(C.CATCH_COUNT) AS n,
    NVL(Avg(C.CATCH_WEIGHT/C.CATCH_COUNT),0) AS mnwt
    FROM @table.GF_FE_CATCH C
    WHERE
      (C.CATCH_WEIGHT>0 And C.CATCH_WEIGHT Is Not Null) AND 
      (C.CATCH_COUNT>1 And C.CATCH_COUNT Is Not Null) AND    -- only calculate means from records with more than one fish
      C.WEIGHT_UNIT_CODE IN ('PND','KGM')
    GROUP BY C.SPECIES_CODE, C.WEIGHT_UNIT_CODE) MW
GROUP BY MW.spp
ORDER BY MW.spp
;
-- getData("fos_fishwt.sql",dbName="GFFOS",strSpp="424",server="ORADEV",type="ORA",trusted=F)
-- getData("fos_fishwt.sql",dbName="GFFOS",strSpp="424",server="GFSH",type="ORA",trusted=F)

