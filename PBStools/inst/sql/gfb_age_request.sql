-- Query GFBioSQL for otoliths taken but not aged (2013-02-20)
-- Show only those that can be identified by FOS TRIP_ID

SET NOCOUNT ON  -- prevents timeout errors

-- CONTAINER_ID in SAMPLE_COLLECTED = Bin, CONTAINER_ID in SPECIMEN_COLLECTED = Tray
SELECT
  SAC.SAMPLE_ID,
  SPC.SPECIMEN_ID,
  SP.SPECIMEN_SERIAL_NUMBER,
  SAC.STORAGE_CONTAINER_ID AS SAMPLE_CONTAINER,
  ISNULL(SPC.STORAGE_CONTAINER_ID,CEILING(SP.SPECIMEN_SERIAL_NUMBER/100)*100) AS SPECIMEN_TRAY,
  ISNULL(SAC.STORAGE_CONTAINER_ID,'UNK')+':'+ISNULL(SPC.STORAGE_CONTAINER_ID,CEILING(SP.SPECIMEN_SERIAL_NUMBER/100)*100) AS TRAYS
INTO #Unique_Storage
FROM
  
  SAMPLE_COLLECTED SAC INNER JOIN 
  SPECIMEN_COLLECTED SPC INNER JOIN
  SPECIMEN SP ON
    SP.SAMPLE_ID = SPC.SAMPLE_ID AND
    SP.SPECIMEN_ID = SPC.SPECIMEN_ID ON
    SPC.SAMPLE_ID = SAC.SAMPLE_ID

-- Collect unadulterated values from B01 to B05
SELECT --TOP 100
  B01.TRIP_ID,
  B02.FISHING_EVENT_ID,
  B03.CATCH_ID,
  B04.SAMPLE_ID,
  B05.SPECIMEN_ID,
  B01.TRIP_COMMENT,
  B01.HAIL_IN_NO,
  B01.TRIP_START_DATE,
  B01.TRIP_END_DATE,
  B01.TRIP_SUB_TYPE_CODE,
  V.VESSEL_NAME,
  V.CFV_NUM,
  B02.FE_MAJOR_LEVEL_ID,
  B02.MAJOR_STAT_AREA_CODE,
  B02.MINOR_STAT_AREA_CODE,
  B02.LOCALITY_CODE,
  B02.DFO_STAT_AREA_CODE,
  B02.DFO_STAT_SUBAREA_CODE,
  B02.GROUPING_CODE,
  B03.SPECIES_CODE,
  B03.CATCH_WEIGHT,
  B04.SAMPLE_TYPE_CODE,
  B04.SAMPLE_DATE,
  B05.AGEING_METHOD_CODE,
  B05.SPECIMEN_SERIAL_PREFIX,
  B05.SPECIMEN_SERIAL_NUMBER,
  B05.SPECIMEN_SEX_CODE
INTO #B01B05
FROM 
  VESSEL V INNER JOIN
  B01_TRIP B01 INNER JOIN
  B02_FISHING_EVENT B02 INNER JOIN
  B02L3_Link_Fishing_Event_Catch L1 INNER JOIN
  B03_CATCH B03 INNER JOIN
  B03L4_Link_Catch_Sample L2 INNER JOIN
  B04_SAMPLE B04 INNER JOIN
  B05_SPECIMEN B05 ON
    B05.SAMPLE_ID = B04.SAMPLE_ID ON
    B04.SAMPLE_ID = L2.SAMPLE_ID ON
    L2.CATCH_ID = B03.CATCH_ID ON
    B03.CATCH_ID = L1.CATCH_ID ON
    L1.FISHING_EVENT_ID = B02.FISHING_EVENT_ID ON
    B02.TRIP_ID = B01.TRIP_ID ON
    B01.VESSEL_ID = V.VESSEL_ID AND
    B01.SUFFIX = V.SUFFIX
WHERE 
  B03.SPECIES_CODE IN (@sppcode)

SELECT 
  AA.TRIP_ID AS TID_gfb,                          -- B01_TRIP
  CASE
    WHEN ISNUMERIC(SUBSTRING(LTRIM(RTRIM(AA.TRIP_COMMENT)),15,6))=1 THEN
      CAST(SUBSTRING(LTRIM(RTRIM(AA.TRIP_COMMENT)),15,6) AS REAL)
    ELSE NULL END AS TID_fos,                     -- B01_TRIP
  AA.FISHING_EVENT_ID AS FEID,                    -- B02_FISHING_EVENT
  ISNULL(CAST(AA.HAIL_IN_NO AS INT),0) AS hail,   -- B01_TRIP
  AA.VESSEL_NAME AS vessel,                        -- VESSEL
  AA.CFV_NUM AS cfv,                               -- VESSEL
  CONVERT(char(10),ISNULL(AA.TRIP_START_DATE, AA.TRIP_END_DATE),20) AS gfb_date,     -- B01_TRIP
  CONVERT(char(7),ISNULL(AA.TRIP_START_DATE, AA.TRIP_END_DATE),20) AS gfb_yrmo,      -- B01_TRIP
  AA.TRIP_SUB_TYPE_CODE AS ttype,                 -- B01_TRIP
  --SA.SRF_Area AS srfa, 
  AA.MAJOR_STAT_AREA_CODE AS major,               -- B02_FISHING_EVENT
  AA.MINOR_STAT_AREA_CODE AS minor,               -- B02_FISHING_EVENT
  AA.FE_MAJOR_LEVEL_ID AS 'set',                  -- B02_FISHING_EVENT
  ISNULL(AA.GROUPING_CODE,0) AS 'GC',             -- B02_FISHING_EVENT
  AA.CATCH_WEIGHT AS catchKg,                     -- B03_CATCH
  AA.SPECIES_CODE AS spp,                         -- B03_CATCH
  -- Following from B05_Specimen
  SUM(CASE WHEN SCA.COLLECTED_ATTRIBUTE_CODE IN (20) THEN 1 ELSE 0 END) AS Noto,                                    -- count otoliths collected
  SUM(CASE WHEN SCA.COLLECTED_ATTRIBUTE_CODE IN (20) AND AA.SPECIMEN_SEX_CODE IN (2) THEN 1 ELSE 0 END) AS Foto,    -- Noto females
  SUM(CASE WHEN SCA.COLLECTED_ATTRIBUTE_CODE IN (20) AND AA.SPECIMEN_SEX_CODE IN (1) THEN 1 ELSE 0 END) AS Moto,    -- Noto males
  --SUM(CASE WHEN SCA.COLLECTED_ATTRIBUTE_CODE IN (21) THEN 1 ELSE 0 END) AS Nsca,
  SUM(CASE WHEN AA.AGEING_METHOD_CODE IN (3) THEN 1 ELSE 0 END) as Nbba,                                            -- count broken & burnt ages
  SUM(CASE WHEN AA.AGEING_METHOD_CODE IN (3) AND AA.SPECIMEN_SEX_CODE IN (2) THEN 1 ELSE 0 END) as Fbba,            -- Nbba females
  SUM(CASE WHEN AA.AGEING_METHOD_CODE IN (3) AND AA.SPECIMEN_SEX_CODE IN (1) THEN 1 ELSE 0 END) as Mbba,            -- Nbba females
  SUM(CASE WHEN AA.AGEING_METHOD_CODE IN (1,2,3,4,5,16) THEN 1 ELSE 0 END) as Nage,                                 -- count any otolith aged
  SUM(CASE WHEN AA.AGEING_METHOD_CODE IN (1,2,3,4,5,16) AND AA.SPECIMEN_SEX_CODE IN (2) THEN 1 ELSE 0 END) as Fage, -- Nage females
  SUM(CASE WHEN AA.AGEING_METHOD_CODE IN (1,2,3,4,5,16) AND AA.SPECIMEN_SEX_CODE IN (1) THEN 1 ELSE 0 END) as Mage, -- Nage males
  SUM(CASE WHEN AA.AGEING_METHOD_CODE BETWEEN 0 AND 16 THEN 1 ELSE 0 END) as Xage,     -- count any ageing
  SUM(IsNull(AA.AGEING_METHOD_CODE,0)) as T_ameth,                                     -- sum the ageing method codes
  SUM(CASE WHEN AA.AGEING_METHOD_CODE BETWEEN 0 AND 16 THEN 1 ELSE 0 END) as N_ameth,  -- count the ageing method codes
  AA.SAMPLE_ID AS SID,                            -- B04_SAMPLE
  AA.SAMPLE_TYPE_CODE AS stype,                   -- B04_SAMPLE
  CONVERT(char(10),ISNULL(AA.SAMPLE_DATE, AA.SAMPLE_DATE),20) AS sdate,    -- B04_SAMPLE
  IsNull(AA.SPECIMEN_SERIAL_PREFIX,'') AS prefix,                          -- B05_SPECIMEN
  MIN(ISNULL(AA.SPECIMEN_SERIAL_NUMBER,2e9)) AS firstSerial,               -- B05_SPECIMEN
  MAX(ISNULL(AA.SPECIMEN_SERIAL_NUMBER,0))   AS lastSerial,                -- B05_SPECIMEN
  IsNull(US.TRAYS,'UNK') AS storageID
INTO #GFB_Otoliths
FROM 
  #Unique_Storage US RIGHT OUTER JOIN 
  #B01B05 AA INNER JOIN
  B05a_Specimen_Collected SCA ON
    SCA.SAMPLE_ID = AA.SAMPLE_ID AND
    SCA.SPECIMEN_ID = AA.SPECIMEN_ID ON
    AA.SAMPLE_ID = US.SAMPLE_ID AND
    AA.SPECIMEN_ID = US.SPECIMEN_ID
WHERE 
-- To activate the following lines, remove '--'
--  ISNULL(Year(AA.TRIP_START_DATE),0) > 0 AND
--  ISNULL(CAST(AA.HAIL_IN_NO AS INT),0) > 0 AND
--  ISNULL(V.CFV_NUM,0) > 0 AND
--  Year(AA.TRIP_START_DATE) IN (2009) AND 
--  AA.TRIP_SUB_TYPE_CODE IN (1,4) AND 
--  (AA.MAJOR_STAT_AREA_CODE IN ('05','06','07') OR 
--    (AA.MAJOR_STAT_AREA_CODE IN ('09') AND AA.MINOR_STAT_AREA_CODE IN ('34') )) AND 
  AA.SPECIES_CODE IN (@sppcode) AND
  SCA.COLLECTED_ATTRIBUTE_CODE IN (20)  -- otoliths collected
  -- SA.N_Ages_Collected > 0 --AND
  --(AA.AGEING_METHOD_CODE Is Null OR AA.AGEING_METHOD_CODE IN (3)) -- either not aged or aged via broken-burnt (cannot exclude NAs when others are not 3)
  --(ISNULL(SA.N_Ages_Collected,0) - ISNULL(SA.N_Aged,0)) > 2
GROUP BY 
  AA.TRIP_ID,
  CASE
    WHEN ISNUMERIC(SUBSTRING(LTRIM(RTRIM(AA.TRIP_COMMENT)),15,6))=1 THEN
      CAST(SUBSTRING(LTRIM(RTRIM(AA.TRIP_COMMENT)),15,6) AS REAL)
    ELSE NULL END,                     -- B01_TRIP
  AA.FISHING_EVENT_ID,
  ISNULL(CAST(AA.HAIL_IN_NO AS INT),0),
  AA.VESSEL_NAME,
  AA.CFV_NUM,
  CONVERT(char(10),ISNULL(AA.TRIP_START_DATE, AA.TRIP_END_DATE),20),
  CONVERT(char(7),ISNULL(AA.TRIP_START_DATE, AA.TRIP_END_DATE),20),
  AA.TRIP_SUB_TYPE_CODE,
  --SA.SRF_Area AS srfa, 
  AA.MAJOR_STAT_AREA_CODE,
  AA.MINOR_STAT_AREA_CODE,
  AA.FE_MAJOR_LEVEL_ID,
  ISNULL(AA.GROUPING_CODE,0),
  AA.CATCH_WEIGHT,
  AA.SPECIES_CODE,
  AA.SAMPLE_ID,
  AA.SAMPLE_TYPE_CODE,
  CONVERT(char(10),ISNULL(AA.SAMPLE_DATE, AA.SAMPLE_DATE),20),
  IsNull(AA.SPECIMEN_SERIAL_PREFIX,''),
  IsNull(US.TRAYS,'UNK')

-- Get FOS Trip from Hail-Vessel-Date combo
SELECT * INTO #FOS_HVD
  FROM OPENQUERY(GFSH,
  'SELECT 
    HT.HAIL_NUMBER AS fos_hail, 
    T.VESSEL_REGISTRATION_NUMBER AS fos_cfv, 
    TO_CHAR(NVL(T.TRIP_START_DATE,T.TRIP_END_DATE),''YYYY-MM-DD'') AS fos_date,
    T.TRIP_ID AS fos_tid
  FROM 
    GFFOS.GF_TRIP T RIGHT OUTER JOIN 
    (SELECT 
      H.HAIL_NUMBER, 
      MIN(H.TRIP_ID) AS TRIP_ID
    FROM 
      GFFOS.GF_HAIL_NUMBER H
    GROUP BY
      H.HAIL_NUMBER ) HT ON
    T.TRIP_ID = HT.TRIP_ID
  ')

-- Get FOS Trip from Hail-Vessel-YrMo combo
SELECT * INTO #FOS_HVYM
  FROM OPENQUERY(GFSH,
  'SELECT 
    HT.HAIL_NUMBER AS fos_hail, 
    T.VESSEL_REGISTRATION_NUMBER AS fos_cfv, 
    TO_CHAR(NVL(T.TRIP_START_DATE,T.TRIP_END_DATE),''YYYY-MM'') AS fos_yrmo,
    T.TRIP_ID AS fos_tid
  FROM 
    GFFOS.GF_TRIP T RIGHT OUTER JOIN 
    (SELECT 
      H.HAIL_NUMBER, 
      MIN(H.TRIP_ID) AS TRIP_ID
    FROM 
      GFFOS.GF_HAIL_NUMBER H
    GROUP BY
      H.HAIL_NUMBER ) HT ON
    T.TRIP_ID = HT.TRIP_ID
  ')

SELECT 
  GFB.TID_gfb,
  COALESCE(GFB.TID_fos,FOS1.fos_tid,FOS2.fos_tid,0)  AS TID_fos,
  --GFB.hail, GFB.cfv, GFB.gfb_date
  GFB.FEID, GFB.hail, GFB.[set], GFB.GC, GFB.vessel, GFB.cfv,
  CONVERT(smalldatetime,GFB.gfb_date) AS tdate,
  GFB.ttype, GFB.major, GFB.minor, GFB.spp, GFB.catchKg,
  GFB.SID, GFB.Noto, GFB.Foto, GFB.Moto, GFB.Nbba, GFB.Fbba, GFB.Mbba, GFB.Nage, GFB.Fage, GFB.Mage, GFB.Xage,
  CASE WHEN GFB.N_ameth IN (0) THEN 0 ELSE GFB.T_ameth / GFB.N_ameth END AS ameth, -- mean ageing method (flags samnples with mixtures of ageing method)
  GFB.stype, CONVERT(smalldatetime,GFB.sdate) AS sdate,
  GFB.prefix, GFB.firstSerial, GFB.lastSerial, GFB.storageID
  --INTO #Dump
  FROM
    #FOS_HVYM FOS2 RIGHT OUTER JOIN
    (#FOS_HVD FOS1 RIGHT OUTER JOIN
    #GFB_Otoliths GFB ON
      GFB.hail  = FOS1.fos_hail AND
      GFB.cfv   = FOS1.fos_cfv  AND
      GFB.gfb_date = FOS1.fos_date) ON
      GFB.hail  = FOS2.fos_hail AND
      GFB.cfv   = FOS2.fos_cfv  AND
      GFB.gfb_yrmo = FOS2.fos_yrmo
  ORDER BY
  CONVERT(smalldatetime,GFB.gfb_date)

--select * from #GFB_Otoliths

-- getData("gfb_age_request.sql",dbName="GFBioSQL",strSpp="401")

