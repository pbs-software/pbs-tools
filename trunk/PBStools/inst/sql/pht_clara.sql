SET NOCOUNT ON 

SELECT
  MC.HAIL_IN_NO, MC.SET_NO,
  CAST(SUM(ISNULL(MC.LANDED,0) + ISNULL(MC.DISCARDED,0)) AS INT) AS Tcat
  INTO #TCAT
  FROM 
    D_Merged_Catches MC
    INNER JOIN C_Species S ON
    MC.SPECIES_CODE = S.SPECIES_CDE
  WHERE 
    S.Fish = 1 AND
    S.Code3 IS NOT NULL
  GROUP BY 
    MC.HAIL_IN_NO, MC.SET_NO 

SELECT *
  INTO #FCAT
  FROM #TCAT T
  WHERE T.Tcat > 0

SELECT
  E.OBFL_HAIL_IN_NO, E.OBFL_SET_NO, 
  COALESCE(-E.OBFL_START_LONGITUDE, -E.OBFL_END_LONGITUDE)AS X, 
  COALESCE(E.OBFL_START_LATITUDE, E.OBFL_END_LATITUDE) AS Y,
  E.Fishing_Depth AS depth
  INTO #Events 
  FROM 
    B3_Fishing_Events E 
    INNER JOIN #FCAT F ON
    E.OBFL_HAIL_IN_NO = F.HAIL_IN_NO AND
    E.OBFL_SET_NO = F.SET_NO
  WHERE 
    COALESCE(-E.OBFL_START_LONGITUDE, -E.OBFL_END_LONGITUDE) IS NOT NULL AND
    COALESCE(E.OBFL_START_LATITUDE, E.OBFL_END_LATITUDE) IS NOT NULL

SELECT
  V.X, V.Y,
  CAST(AVG(V.depth) AS INT) AS Z,
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'ABS' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'ABS',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'ARF' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'ARF',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'AUR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'AUR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'BBE' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'BBE',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'BDS' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'BDS',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'BFU' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'BFU',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'BIS' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'BIS',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'BKR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'BKR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'BOR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'BOR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'BUL' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'BUL',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'CAR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'CAR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'CHM' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'CHM',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'CHR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'CHR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'COL' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'COL',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'CPR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'CPR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'CUL' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'CUL',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'DBR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'DBR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'DEL' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'DEL',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'DKR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'DKR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'DOG' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'DOG',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'DOL' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'DOL',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'ENL' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'ENL',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'EUN' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'EUN',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'FHL' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'FHL',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'GSR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'GSR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'HQR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'HQR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'KEG' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'KEG',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'LEF' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'LEF',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'LIN' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'LIN',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'LNS' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'LNS',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'LST' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'LST',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'PAC' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'PAC',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'PAD' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'PAD',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'PAF' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'PAF',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'PAH' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'PAH',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'PAK' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'PAK',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'PAT' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'PAT',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'PEL' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'PEL',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'PER' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'PER',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'POP' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'POP',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'PSL' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'PSL',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'PSS' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'PSS',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'QBR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'QBR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'RAT' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'RAT',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'RBR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'RBR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'RBU' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'RBU',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'RER' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'RER',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'RIF' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'RIF',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'ROL' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'ROL',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'ROR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'ROR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'RSR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'RSR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'RTR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'RTR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'RTS' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'RTS',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'RXL' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'RXL',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'SAL' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'SAL',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'SBF' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'SBF',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'SCO' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'SCO',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'SCR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'SCR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'SGR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'SGR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'SKR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'SKR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'SLL' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'SLL',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'SNR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'SNR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'SPS' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'SPS',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'SSS' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'SSS',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'SST' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'SST',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'STF' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'STF',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'STS' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'STS',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'TFU' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'TFU',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'TIR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'TIR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'VMR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'VMR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'WAP' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'WAP',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'WOE' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'WOE',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'WWR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'WWR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'XXR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'XXR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'XXT' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'XXT',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'YFL' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'YFL',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'YMR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'YMR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'YTR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'YTR',
  ISNULL(CAST(SUM(CASE S.Code3 WHEN 'YYR' THEN (MC.LANDED + MC.DISCARDED) END) AS INT),0) AS 'YYR' 
  FROM 
    #Events V 
    INNER JOIN D_Merged_Catches MC ON
    V.OBFL_HAIL_IN_NO = MC.HAIL_IN_NO AND
    V.OBFL_SET_NO = MC.SET_NO 
    INNER JOIN C_SPECIES S ON 
    MC.SPECIES_CODE = S.SPECIES_CDE
  WHERE 
    S.Fish = 1 AND
    S.Code3 IS NOT NULL
  GROUP BY
    V.X, V.Y
 
