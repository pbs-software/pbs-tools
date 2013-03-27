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
    S.Fish = 1 OR
    S.Invertebrate =1 
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
  ISNULL(E.Fishing_Depth,0) AS depth
  INTO #Events 
  FROM 
    B3_Fishing_Events E 
    INNER JOIN #FCAT F ON
    E.OBFL_HAIL_IN_NO = F.HAIL_IN_NO AND
    E.OBFL_SET_NO = F.SET_NO
  WHERE 
    COALESCE(-E.OBFL_START_LONGITUDE, -E.OBFL_END_LONGITUDE) IS NOT NULL AND
    COALESCE(E.OBFL_START_LATITUDE, E.OBFL_END_LATITUDE) IS NOT NULL

SELECT --TOP 1000
  CAST(V.X AS REAL) AS X,
  CAST(V.Y AS REAL) AS Y,
  CAST(AVG(V.depth) AS INT) AS Z,
  CAST(SUM(CASE WHEN MC.SPECIES_CODE IN (@t10code) THEN (MC.LANDED + MC.DISCARDED) ELSE 0 END) AS INT) AS 'Rockfish',
  CAST(SUM(CASE WHEN MC.SPECIES_CODE IN (@t11code) THEN (MC.LANDED + MC.DISCARDED) ELSE 0 END) AS INT) AS 'Lingcod',
  CAST(SUM(CASE WHEN MC.SPECIES_CODE IN ('221','222') THEN (MC.LANDED + MC.DISCARDED) ELSE 0 END) AS INT) AS 'Pacific_Cod',
  CAST(SUM(CASE WHEN MC.SPECIES_CODE IN (@t12code) AND MC.SPECIES_CODE NOT IN ('614') THEN (MC.LANDED + MC.DISCARDED) ELSE 0 END) AS INT) AS 'Flatfish',
  CAST(SUM(CASE WHEN MC.SPECIES_CODE IN ('614') THEN (MC.LANDED + MC.DISCARDED) ELSE 0 END) AS INT) AS 'Halibut',
  CAST(SUM(CASE WHEN MC.SPECIES_CODE IN ('228') THEN (MC.LANDED + MC.DISCARDED) ELSE 0 END) AS INT) AS 'Pollock',
  CAST(SUM(CASE WHEN MC.SPECIES_CODE IN ('224','225') THEN (MC.LANDED + MC.DISCARDED) ELSE 0 END) AS INT) AS 'Hake',
  CAST(SUM(CASE WHEN MC.SPECIES_CODE IN ('454','455') THEN (MC.LANDED + MC.DISCARDED) ELSE 0 END) AS INT) AS 'Sablefish',
  CAST(SUM(CASE WHEN MC.SPECIES_CODE IN (@t02code) THEN (MC.LANDED + MC.DISCARDED) ELSE 0 END) AS INT) AS 'Sharks_Rays',
  CAST(SUM(CASE WHEN MC.SPECIES_CODE IN (@t03code,@t05code) THEN (MC.LANDED + MC.DISCARDED) ELSE 0 END) AS INT) AS 'Herring_Anchovies_Smelts',
  CAST(SUM(CASE WHEN MC.SPECIES_CODE IN (@t09code) THEN (MC.LANDED + MC.DISCARDED) ELSE 0 END) AS INT) AS 'Mackerels_Tunas'
INTO #PHT_CLARA
FROM 
  #Events V 
  INNER JOIN D_Merged_Catches MC ON
  V.OBFL_HAIL_IN_NO = MC.HAIL_IN_NO AND
  V.OBFL_SET_NO = MC.SET_NO 
GROUP BY
  CAST(V.X AS REAL),
  CAST(V.Y AS REAL)

SELECT * INTO #FOS_CLARA
FROM OPENQUERY(GFSH,'
SELECT
  CAST(OC.LON AS REAL) AS X,
  CAST(OC.LAT AS REAL) AS Y,
  CAST(AVG(NVL(OC.BEST_DEPTH_FM,0)*1.8288) AS INTEGER) AS Z,
  CAST(SUM(CASE WHEN OC.SPECIES_CODE IN (@~t10code) THEN 
    (NVL(OC.LANDED_ROUND_KG,0)) +           -- official retained catch
    (COALESCE(OC.TOTAL_RELEASED_ROUND_KG,
    (NVL(OC.SUBLEGAL_RELEASED_COUNT,0) + NVL(OC.LEGAL_RELEASED_COUNT,0)) * FW.MNWT, 0) +  -- released
    (NVL(OC.SUBLEGAL_LICED_COUNT,0) + NVL(OC.LEGAL_LICED_COUNT,0)) * FW.MNWT +            -- liced
    (NVL(OC.SUBLEGAL_BAIT_COUNT,0) + NVL(OC.LEGAL_BAIT_COUNT,0)) * FW.MNWT)
    ELSE 0 END) AS INTEGER) AS \"Rockfish\",
  CAST(SUM(CASE WHEN OC.SPECIES_CODE IN (@~t11code) THEN 
    (NVL(OC.LANDED_ROUND_KG,0)) +           -- official retained catch
    (COALESCE(OC.TOTAL_RELEASED_ROUND_KG,
    (NVL(OC.SUBLEGAL_RELEASED_COUNT,0) + NVL(OC.LEGAL_RELEASED_COUNT,0)) * FW.MNWT, 0) +  -- released
    (NVL(OC.SUBLEGAL_LICED_COUNT,0) + NVL(OC.LEGAL_LICED_COUNT,0)) * FW.MNWT +            -- liced
    (NVL(OC.SUBLEGAL_BAIT_COUNT,0) + NVL(OC.LEGAL_BAIT_COUNT,0)) * FW.MNWT)
    ELSE 0 END) AS INTEGER) AS \"Lingcod\",
  CAST(SUM(CASE WHEN OC.SPECIES_CODE IN (''221'',''222'') THEN 
    (NVL(OC.LANDED_ROUND_KG,0)) +           -- official retained catch
    (COALESCE(OC.TOTAL_RELEASED_ROUND_KG,
    (NVL(OC.SUBLEGAL_RELEASED_COUNT,0) + NVL(OC.LEGAL_RELEASED_COUNT,0)) * FW.MNWT, 0) +  -- released
    (NVL(OC.SUBLEGAL_LICED_COUNT,0) + NVL(OC.LEGAL_LICED_COUNT,0)) * FW.MNWT +            -- liced
    (NVL(OC.SUBLEGAL_BAIT_COUNT,0) + NVL(OC.LEGAL_BAIT_COUNT,0)) * FW.MNWT)
    ELSE 0 END) AS INTEGER) AS \"Pacific_Cod\",
  CAST(SUM(CASE WHEN OC.SPECIES_CODE IN (@~t12code) AND OC.SPECIES_CODE NOT IN (''614'') THEN 
    (NVL(OC.LANDED_ROUND_KG,0)) +           -- official retained catch
    (COALESCE(OC.TOTAL_RELEASED_ROUND_KG,
    (NVL(OC.SUBLEGAL_RELEASED_COUNT,0) + NVL(OC.LEGAL_RELEASED_COUNT,0)) * FW.MNWT, 0) +  -- released
    (NVL(OC.SUBLEGAL_LICED_COUNT,0) + NVL(OC.LEGAL_LICED_COUNT,0)) * FW.MNWT +            -- liced
    (NVL(OC.SUBLEGAL_BAIT_COUNT,0) + NVL(OC.LEGAL_BAIT_COUNT,0)) * FW.MNWT)
    ELSE 0 END) AS INTEGER) AS \"Flatfish\",
  CAST(SUM(CASE WHEN OC.SPECIES_CODE IN (''614'') THEN 
    (NVL(OC.LANDED_ROUND_KG,0)) +           -- official retained catch
    (COALESCE(OC.TOTAL_RELEASED_ROUND_KG,
    (NVL(OC.SUBLEGAL_RELEASED_COUNT,0) + NVL(OC.LEGAL_RELEASED_COUNT,0)) * FW.MNWT, 0) +  -- released
    (NVL(OC.SUBLEGAL_LICED_COUNT,0) + NVL(OC.LEGAL_LICED_COUNT,0)) * FW.MNWT +            -- liced
    (NVL(OC.SUBLEGAL_BAIT_COUNT,0) + NVL(OC.LEGAL_BAIT_COUNT,0)) * FW.MNWT)
    ELSE 0 END) AS INTEGER) AS \"Halibut\",
  CAST(SUM(CASE WHEN OC.SPECIES_CODE IN (''228'') THEN 
    (NVL(OC.LANDED_ROUND_KG,0)) +           -- official retained catch
    (COALESCE(OC.TOTAL_RELEASED_ROUND_KG,
    (NVL(OC.SUBLEGAL_RELEASED_COUNT,0) + NVL(OC.LEGAL_RELEASED_COUNT,0)) * FW.MNWT, 0) +  -- released
    (NVL(OC.SUBLEGAL_LICED_COUNT,0) + NVL(OC.LEGAL_LICED_COUNT,0)) * FW.MNWT +            -- liced
    (NVL(OC.SUBLEGAL_BAIT_COUNT,0) + NVL(OC.LEGAL_BAIT_COUNT,0)) * FW.MNWT)
    ELSE 0 END) AS INTEGER) AS \"Pollock\",
  CAST(SUM(CASE WHEN OC.SPECIES_CODE IN (''224'',''225'') THEN 
    (NVL(OC.LANDED_ROUND_KG,0)) +           -- official retained catch
    (COALESCE(OC.TOTAL_RELEASED_ROUND_KG,
    (NVL(OC.SUBLEGAL_RELEASED_COUNT,0) + NVL(OC.LEGAL_RELEASED_COUNT,0)) * FW.MNWT, 0) +  -- released
    (NVL(OC.SUBLEGAL_LICED_COUNT,0) + NVL(OC.LEGAL_LICED_COUNT,0)) * FW.MNWT +            -- liced
    (NVL(OC.SUBLEGAL_BAIT_COUNT,0) + NVL(OC.LEGAL_BAIT_COUNT,0)) * FW.MNWT)
    ELSE 0 END) AS INTEGER) AS \"Hake\",
  CAST(SUM(CASE WHEN OC.SPECIES_CODE IN (''454'',''455'') THEN 
    (NVL(OC.LANDED_ROUND_KG,0)) +           -- official retained catch
    (COALESCE(OC.TOTAL_RELEASED_ROUND_KG,
    (NVL(OC.SUBLEGAL_RELEASED_COUNT,0) + NVL(OC.LEGAL_RELEASED_COUNT,0)) * FW.MNWT, 0) +  -- released
    (NVL(OC.SUBLEGAL_LICED_COUNT,0) + NVL(OC.LEGAL_LICED_COUNT,0)) * FW.MNWT +            -- liced
    (NVL(OC.SUBLEGAL_BAIT_COUNT,0) + NVL(OC.LEGAL_BAIT_COUNT,0)) * FW.MNWT)
    ELSE 0 END) AS INTEGER) AS \"Sablefish\",
  CAST(SUM(CASE WHEN OC.SPECIES_CODE IN (@~t02code) THEN 
    (NVL(OC.LANDED_ROUND_KG,0)) +           -- official retained catch
    (COALESCE(OC.TOTAL_RELEASED_ROUND_KG,
    (NVL(OC.SUBLEGAL_RELEASED_COUNT,0) + NVL(OC.LEGAL_RELEASED_COUNT,0)) * FW.MNWT, 0) +  -- released
    (NVL(OC.SUBLEGAL_LICED_COUNT,0) + NVL(OC.LEGAL_LICED_COUNT,0)) * FW.MNWT +            -- liced
    (NVL(OC.SUBLEGAL_BAIT_COUNT,0) + NVL(OC.LEGAL_BAIT_COUNT,0)) * FW.MNWT)
    ELSE 0 END) AS INTEGER) AS \"Sharks_Rays\",
  CAST(SUM(CASE WHEN OC.SPECIES_CODE IN (@~t03code,@~t05code) THEN 
    (NVL(OC.LANDED_ROUND_KG,0)) +           -- official retained catch
    (COALESCE(OC.TOTAL_RELEASED_ROUND_KG,
    (NVL(OC.SUBLEGAL_RELEASED_COUNT,0) + NVL(OC.LEGAL_RELEASED_COUNT,0)) * FW.MNWT, 0) +  -- released
    (NVL(OC.SUBLEGAL_LICED_COUNT,0) + NVL(OC.LEGAL_LICED_COUNT,0)) * FW.MNWT +            -- liced
    (NVL(OC.SUBLEGAL_BAIT_COUNT,0) + NVL(OC.LEGAL_BAIT_COUNT,0)) * FW.MNWT)
    ELSE 0 END) AS INTEGER) AS \"Herring_Anchovies_Smelts\",
  CAST(SUM(CASE WHEN OC.SPECIES_CODE IN (@~t09code) THEN 
    (NVL(OC.LANDED_ROUND_KG,0)) +           -- official retained catch
    (COALESCE(OC.TOTAL_RELEASED_ROUND_KG,
    (NVL(OC.SUBLEGAL_RELEASED_COUNT,0) + NVL(OC.LEGAL_RELEASED_COUNT,0)) * FW.MNWT, 0) +  -- released
    (NVL(OC.SUBLEGAL_LICED_COUNT,0) + NVL(OC.LEGAL_LICED_COUNT,0)) * FW.MNWT +            -- liced
    (NVL(OC.SUBLEGAL_BAIT_COUNT,0) + NVL(OC.LEGAL_BAIT_COUNT,0)) * FW.MNWT)
    ELSE 0 END) AS INTEGER) AS \"Mackerels_Tunas\"
FROM 
  GFFOS.MEAN_SPECIES_WEIGHT_VW FW INNER JOIN
  GFFOS.GF_D_OFFICIAL_FE_CATCH OC ON
    FW.SPP = OC.SPECIES_CODE
WHERE 
  OC.LON IS NOT NULL AND OC.LAT IS NOT NULL AND
  --ROWNUM <= 1000 AND
  OC.SPECIES_CODE IN (@~t10code,@~t11code,''221'',''222'',@~t12code,''614'',''228'',''224'',''225'',''454'',''455'',@~t02code,@~t03code,@~t05code,@~t09code)
GROUP BY
  CAST(OC.LON AS REAL),
  CAST(OC.LAT AS REAL)
')

SELECT  * FROM #PHT_CLARA
--UNION ALL SELECT * FROM #FOS_CLARA


-- getData("pht_clara_groundfish.sql",strSpp="ZZZ")
