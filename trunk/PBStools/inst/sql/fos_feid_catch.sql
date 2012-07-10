SELECT 
  FE.FISHING_EVENT_ID as FEID,
  SC.spp as \"spp\",
  SC.util as \"util\",
  Sum(SC.sppcat) as \"catKg\"
FROM 
  @table.GF_FISHING_EVENT FE RIGHT OUTER JOIN 
  -- SC species catch (specified 'strSpp')
  (SELECT 
    C.FISHING_EVENT_ID,
    C.SPECIES_CODE As spp,
    NVL(C.UTILIZATION_CODE,0) AS util,
    SUM(CASE
      WHEN C.CATCH_WEIGHT Is Not Null THEN 
      DECODE(C.WEIGHT_UNIT_CODE,
        'PND', C.CATCH_WEIGHT/2.20459,  -- PND (lbs) occurs exclusively
        'KGM', C.CATCH_WEIGHT,
        'MET', C.CATCH_WEIGHT*1000.,
        'IPT', C.CATCH_WEIGHT*2240./2.20459,
        'STN', C.CATCH_WEIGHT*2000./2.20459, 0)
      WHEN C.CATCH_WEIGHT Is Null And C.CATCH_COUNT>0 THEN C.CATCH_COUNT*NVL(FW.mnwt,1.) -- if mean weight missing use 1 kg
      ELSE 0 END) AS sppcat
    FROM 
      @table.GF_FE_CATCH C INNER JOIN

-- GFFOS.GF_FE_CATCH mean species weight (kg) from observations where CATCH_COUNT > 1
  (SELECT
    MW.spp as spp,
    Avg(CASE
      WHEN MW.unit='PND' THEN MW.mnwt/2.20459
      ELSE MW.mnwt END) as mnwt
    FROM
      (SELECT  -- inline view calculates empirical mean weights for species (in lbs and/or kg)
         FC.SPECIES_CODE AS spp,
         FC.WEIGHT_UNIT_CODE AS unit,
         NVL(Avg(FC.CATCH_WEIGHT/FC.CATCH_COUNT),0) AS mnwt
         FROM @table.GF_FE_CATCH FC
         WHERE
           (FC.CATCH_WEIGHT>0 And FC.CATCH_WEIGHT Is Not Null) AND 
           (FC.CATCH_COUNT>1 And FC.CATCH_COUNT Is Not Null) AND 
           FC.WEIGHT_UNIT_CODE IN ('PND','KGM')
         GROUP BY FC.SPECIES_CODE, FC.WEIGHT_UNIT_CODE) MW
    GROUP BY MW.spp ) FW ON

    C.SPECIES_CODE=FW.spp
    WHERE 
      --C.FISHING_EVENT_ID IN (740916,740724,740868,743260,745230,982515) AND -- simply to speed testing
      C.SPECIES_CODE IN (@sppcode) --,@trfcode,'042','044','454','455','467','614')
    GROUP BY C.FISHING_EVENT_ID, C.SPECIES_CODE, NVL(C.UTILIZATION_CODE,0) ) SC ON
    FE.FISHING_EVENT_ID = SC.FISHING_EVENT_ID
WHERE SC.sppcat>0
GROUP BY FE.FISHING_EVENT_ID, SC.spp, SC.util
;

-- getData("fos_feid_catch.sql","GFFOS",strSpp="415",server="ORADEV",type="ORA",trusted=F)


