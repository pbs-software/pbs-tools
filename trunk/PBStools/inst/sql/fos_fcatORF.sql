-- Query FOS catch from the observer/fisherlog catch table GF_FE_CATCH
SELECT * FROM (
SELECT
  --SC.FISHING_EVENT_ID,
  NVL(TS.FID,0) AS \"fid\",
  TS.SECTOR AS \"sector\",
  E.DATA_SOURCE_CODE AS \"log\",
  NVL(E.START_DATE,E.END_DATE) AS \"date\",
  NVL(A.MAJOR_STAT_AREA_CODE,0) AS \"major\",
  NVL(A.MINOR_STAT_AREA_CODE,0) AS \"minor\",
  SUM ( CASE 
    WHEN SC.spp IN (@sppcode) AND SC.util IN (0,57,58,60,62,66,69)  -- retained codes in GFFOS
    THEN SC.sppcat ELSE 0 END) AS \"landed\",
  SUM ( CASE
    WHEN SC.spp IN (@sppcode) AND SC.util NOT IN (0,57,58,60,62,66,69) 
    THEN NVL(SC.sppcat,0) ELSE 0 END ) AS \"discard\",
  SUM ( CASE
    WHEN SC.spp = '396' AND SC.util IN (57,58,60,62,66,69) 
    THEN NVL(SC.sppcat,0) ELSE 0 END ) AS \"POP\",
  SUM ( CASE  -- all rockfish other than POP
    WHEN SC.spp IN (@orfcode) AND SC.util IN (57,58,60,62,66,69) 
    THEN NVL(SC.sppcat,0) ELSE 0 END ) AS \"ORF\",
  SUM ( CASE  -- target landings reference for discard calculations
    WHEN TS.FID=1 AND SC.spp IN ('424','407','431','433','442') AND
      SC.util IN (57,58,60,62,66,69)    THEN NVL(SC.sppcat,0)
    WHEN TS.FID=2 AND SC.spp IN ('614') AND
      SC.util IN (57,58,60,62,66,69)    THEN NVL(SC.sppcat,0)
    WHEN TS.FID=3 AND SC.spp IN ('454','455') AND
      SC.util IN (57,58,60,62,66,69)    THEN NVL(SC.sppcat,0)
    WHEN TS.FID=4 AND TS.SECTOR='Spiny Dogfish' AND SC.spp IN ('042','044') AND
      SC.util IN (57,58,60,62,66,69)    THEN NVL(SC.sppcat,0)
    WHEN TS.FID=4 AND TS.SECTOR='Lingcod' AND SC.spp IN ('467') AND
      SC.util IN (57,58,60,62,66,69)    THEN NVL(SC.sppcat,0)
    WHEN TS.FID=5 AND SC.spp IN ('424','407','431','433','442') AND
      SC.util IN (57,58,60,62,66,69)    THEN NVL(SC.sppcat,0)
    ELSE 0 END) AS \"TAR\"

  FROM
  (SELECT -- Norm's query (modified) to categorize trips into sectors
  TF.TRIP_ID,
  COUNT(TRIP_ID) AS NFID,
  MIN(CASE
    WHEN TF.FISHERY_CODE IN (24) THEN 5
    WHEN TF.FISHERY_CODE IN (21)       AND TF.FISHERY_CODE NOT IN (24) THEN 2
    WHEN TF.FISHERY_CODE IN (52,57,59) AND TF.FISHERY_CODE NOT IN (24,21) THEN 4
    WHEN TF.FISHERY_CODE IN (17)       AND TF.FISHERY_CODE NOT IN (24,21,52,57,59) THEN 3
    WHEN TF.FISHERY_CODE IN (23)       AND TF.FISHERY_CODE NOT IN (24,21,52,57,59,17) THEN 1
    ELSE 0 END) AS FID,
  (CASE 
      WHEN COUNT(TF.TRIP_ID) >= 2 THEN INITCAP(MIN(F.FISHERY_NAME)) || ' & ' || INITCAP(MAX(F.FISHERY_NAME)) 
      ELSE INITCAP(MIN(F.FISHERY_NAME)) END) || --'' || 
     (CASE MIN(FS.FISHERY_SUBTYPE_DESCRIPTION) 
      WHEN 'Unspecified' THEN '' 
      ELSE ' ' || MIN(FS.FISHERY_SUBTYPE_DESCRIPTION) END) AS SECTOR
  FROM 
    @table.GF_TRIP_FISHERY TF
    INNER JOIN @table.FISHERY F ON
    TF.FISHERY_CODE = F.FISHERY_CODE
    INNER JOIN @table.FISHERY_SUBTYPE FS ON
    TF.FISHERY_SUBTYPE_CODE = FS.FISHERY_SUBTYPE_CODE
  WHERE 
    TF.FISHERY_CODE IN (17, 21, 23, 24, 52, 57, 59) AND -- Retain these fisheries
    TF.TRIP_TYPE_CODE NOT IN (18, 12813, 5669) -- Omit IPHC, research, and seamount trips
  GROUP BY 
    TF.TRIP_ID ) TS RIGHT OUTER JOIN

  (@table.GF_FE_DERIVED_AREA A RIGHT OUTER JOIN
  (@table.GF_FISHING_EVENT E RIGHT OUTER JOIN

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
      WHEN C.CATCH_WEIGHT Is Null And C.CATCH_COUNT>0 THEN C.CATCH_COUNT*NVL(FW.MNWT,1.) -- if mean weight missing use 1 kg
      ELSE 0 END) AS sppcat
    FROM 
      @table.GF_FE_CATCH C INNER JOIN

-- GFFOS.GF_FE_CATCH mean species weight (kg) from observations where CATCH_COUNT > 1
  GFFOS.MEAN_SPECIES_WEIGHT_VW FW
  ON  C.SPECIES_CODE = FW.SPP
    WHERE 
      --C.FISHING_EVENT_ID IN (740916,740724,740868,743260,745230,982515) AND -- simply to speed testing
      C.SPECIES_CODE IN (@sppcode,@trfcode,'042','044','454','455','467','614')
    GROUP BY C.FISHING_EVENT_ID, C.SPECIES_CODE, NVL(C.UTILIZATION_CODE,0) ) SC ON

  SC.FISHING_EVENT_ID = E.FISHING_EVENT_ID ) ON
  E.FISHING_EVENT_ID = A.FISHING_EVENT_ID  ) ON
  E.TRIP_ID = TS.TRIP_ID
  WHERE
    TS.FID = 1 OR (TS.FID <> 1 AND E.DATA_SOURCE_CODE NOT IN (106,107))
  GROUP BY
    SC.FISHING_EVENT_ID, NVL(TS.FID,0), TS.SECTOR,
    E.DATA_SOURCE_CODE, NVL(E.START_DATE,E.END_DATE),
    NVL(A.MAJOR_STAT_AREA_CODE,0),
    NVL(A.MINOR_STAT_AREA_CODE,0)
) ORF
WHERE
  ORF.\"landed\">0 OR ORF.\"discard\">0 OR ORF.POP>0 OR ORF.ORF>0 OR ORF.TAR>0
;

-- getData("fos_fcatORF.sql",dbName="GFFOS",strSpp="415",server="GFSH",type="ORA",trusted=F)

