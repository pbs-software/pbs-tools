-- Determine target species by year, month, area, and gear.

SELECT
    ZC.STP_SPER_YR,
    ZC.STP_SPER_PERIOD_CDE,
    ZC.SFA_MSFA_MIDSIZE_FA_CDE,
    ZC.GR_GEAR_CDE,
    ZC.SumCat,
    MIN(ZC.SP_SPECIES_CDE) AS Target
  FROM
    (SELECT 
      CS.STP_SPER_YR,
      CS.STP_SPER_PERIOD_CDE,
      CS.SFA_MSFA_MIDSIZE_FA_CDE,
      CS.GR_GEAR_CDE,
      CS.SP_SPECIES_CDE,
      Sum(CS.CATSUM_ROUND_LBS_WT) AS SumCat
    FROM
      @table.CATCH_SUMMARY CS
    GROUP BY 
      CS.STP_SPER_YR, CS.STP_SPER_PERIOD_CDE,
      CS.SFA_MSFA_MIDSIZE_FA_CDE, CS.GR_GEAR_CDE, CS.SP_SPECIES_CDE) ZC -- Species catch by year, period, and area

  INNER JOIN
    (SELECT
      TC.STP_SPER_YR,
      TC.STP_SPER_PERIOD_CDE,
      TC.SFA_MSFA_MIDSIZE_FA_CDE,
      TC.GR_GEAR_CDE,
      Max(TC.SumCat) AS MaxCat
    FROM
      (SELECT 
        YC.STP_SPER_YR,
        YC.STP_SPER_PERIOD_CDE,
        YC.SFA_MSFA_MIDSIZE_FA_CDE,
        YC.GR_GEAR_CDE,
        YC.SP_SPECIES_CDE,
        Sum(YC.CATSUM_ROUND_LBS_WT) AS SumCat
      FROM
        @table.CATCH_SUMMARY YC
      GROUP BY 
        YC.STP_SPER_YR, YC.STP_SPER_PERIOD_CDE,
        YC.SFA_MSFA_MIDSIZE_FA_CDE, YC.GR_GEAR_CDE, YC.SP_SPECIES_CDE) TC   -- total catch
    GROUP BY 
      TC.STP_SPER_YR, TC.STP_SPER_PERIOD_CDE,
      TC.SFA_MSFA_MIDSIZE_FA_CDE, TC.GR_GEAR_CDE) MC ON                     -- maximum catch

    ZC.STP_SPER_YR = MC.STP_SPER_YR AND
    ZC.STP_SPER_PERIOD_CDE = MC.STP_SPER_PERIOD_CDE AND
    ZC.SFA_MSFA_MIDSIZE_FA_CDE = MC.SFA_MSFA_MIDSIZE_FA_CDE AND
    ZC.GR_GEAR_CDE = MC.GR_GEAR_CDE AND
    ZC.SumCat = MC.MaxCat
  WHERE
    ZC.STP_SPER_YR <= 1995
    --ZC.STP_SPER_YR IN (1995) AND
    --ZC.STP_SPER_PERIOD_CDE IN ('070') AND
    --ZC.SFA_MSFA_MIDSIZE_FA_CDE IN (9021)
  GROUP BY
    ZC.STP_SPER_YR,
    ZC.STP_SPER_PERIOD_CDE,
    ZC.SFA_MSFA_MIDSIZE_FA_CDE,
    ZC.GR_GEAR_CDE,
    ZC.SumCat
  ORDER BY
    ZC.STP_SPER_YR,
    ZC.STP_SPER_PERIOD_CDE,
    ZC.SFA_MSFA_MIDSIZE_FA_CDE,
    ZC.GR_GEAR_CDE


-- qu("ph3_target.sql",dbName="HARVEST_V2_0",strSpp="000",server="ORAPROD",type="ORA",trusted=FALSE, uid="haighr", pwd="haighr")

