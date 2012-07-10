-- GFFOS query for landed and discarded catch in tows catching a specific species (2011-02-14)
-- Used for table of catches in DFO's 'checklist' nonsense.
-- Query uses the merged catch table GF_D_OFFICIAL_FE_CATCH with official retained catch
-- User must specify date intervals by changing code on line 27 below.

SELECT
  OC.SPECIES_CODE AS SPP,
  SUM(NVL(OC.LANDED_ROUND_KG,0))/1000. AS LANDED_T,           -- official retained catch
  SUM(NVL(OC.BEST_RETAINED_ROUND_KG,0))/1000. AS RETAINED_T,  -- best estimate of fishing event catch retained at sea
  SUM(COALESCE(OC.TOTAL_RELEASED_ROUND_KG,
    (NVL(OC.SUBLEGAL_RELEASED_COUNT,0) + NVL(OC.LEGAL_RELEASED_COUNT,0)) * FW.MNWT, 0) +  -- released
    (NVL(OC.SUBLEGAL_LICED_COUNT,0) + NVL(OC.LEGAL_LICED_COUNT,0)) * FW.MNWT +            -- liced
    (NVL(OC.SUBLEGAL_BAIT_COUNT,0) + NVL(OC.LEGAL_BAIT_COUNT,0)) * FW.MNWT)/1000. AS DISCARDED_T
FROM 
  @table.MEAN_SPECIES_WEIGHT_VW FW INNER JOIN
  (@table.GF_D_OFFICIAL_FE_CATCH OC INNER JOIN
  (SELECT
    FOC.TRIP_ID,
    FOC.FISHING_EVENT_ID
    FROM @table.GF_D_OFFICIAL_FE_CATCH FOC
    WHERE FOC.SPECIES_CODE IN (@sppcode)
    GROUP BY FOC.TRIP_ID, FOC.FISHING_EVENT_ID ) FE ON
    OC.TRIP_ID = FE.TRIP_ID AND
    OC.FISHING_EVENT_ID = FE.FISHING_EVENT_ID) ON
  FW.SPP = OC.SPECIES_CODE
WHERE 
  OC.SPECIES_CODE NOT IN ('004','848','849','999','XXX') AND 
  TO_DATE(TO_CHAR(OC.BEST_DATE,'YYYY-MM-DD'),'YYYY-MM-DD') BETWEEN TO_DATE('2010-04-01', 'YYYY-MM-DD') AND TO_DATE('2011-03-31', 'YYYY-MM-DD') AND
  (CASE WHEN
    (OC.GEAR_SUBTYPE IN ('HARD BOTTOM TRAWL','SOFT BOTTOM TRAWL') OR   -- bottom
    (OC.GEAR_SUBTYPE IN ('UNSPECIFIED') AND 
     OC.TRIP_CATEGORY NOT IN ('OPT A - HAKE QUOTA (GULF)','OPT A - HAKE QUOTA (SHORESIDE)','OPT A - HAKE QUOTA (JV)')))
  THEN 1
    WHEN
    (OC.GEAR_SUBTYPE IN ('MIDWATER TRAWL') OR   -- midwater
    (OC.GEAR_SUBTYPE IN ('UNSPECIFIED') AND 
     OC.TRIP_CATEGORY IN ('OPT A - HAKE QUOTA (GULF)','OPT A - HAKE QUOTA (SHORESIDE)','OPT A - HAKE QUOTA (JV)')))
  THEN 3 ELSE 0 END) IN (@dummy) 
GROUP BY
  OC.SPECIES_CODE
ORDER BY
  -SUM(NVL(OC.LANDED_ROUND_KG,0))
;

-- Note: @dummy refers to GFFOS gear_subtype: 1=bottom trawl, 3=midwater trawl
-- getData("fos_checklist_catch.sql","GFFOS",strSpp="440",server="GFSH",type="ORA",trusted=F,uid="haighr",pwd="",dummy=0:3)

