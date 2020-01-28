-- Query PacHarv3's CATCH_SUMMARY_TABLE for a crossTab in R
-- YPAGS = Year, Period, Area, Gear, Species
SELECT 
  CS.STP_SPER_YR AS \"year\",
  CS.STP_SPER_PERIOD_CDE AS \"period\",
  CS.SFA_MSFA_MIDSIZE_FA_CDE AS \"area\",
  CS.GR_GEAR_CDE AS \"gear\",
  CS.SP_SPECIES_CDE AS \"spp\",
  Sum(CS.CATSUM_ROUND_LBS_WT)/2204.59 AS \"sumcat\", --tonnes
  Count(CS.CATSUM_ROUND_LBS_WT) AS \"numcat\"
FROM
  @table.CATCH_SUMMARY CS
WHERE
  CS.STP_SPER_YR <= 1995
GROUP BY 
  CS.STP_SPER_YR,
  CS.STP_SPER_PERIOD_CDE,
  CS.SFA_MSFA_MIDSIZE_FA_CDE,
  CS.GR_GEAR_CDE,
  CS.SP_SPECIES_CDE

-- qu("ph3_catch_YPAGS.sql",dbName="HARVEST_V2_0",strSpp="000",server="ORAPROD",type="ORA",trusted=FALSE, uid="haighr", pwd="haighr")
