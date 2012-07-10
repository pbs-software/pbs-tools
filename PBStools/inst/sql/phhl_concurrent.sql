-- Norm Olsen's query to get top 20 percentage catch
--DECLARE @mindep INT, @maxdep INT 
--SET @mindep = @mindepval
--SET @maxdep = @maxdepval

SET NOCOUNT ON

SELECT E.OBFL_HAIL_IN_NO, E.OBFL_SET_NO, OBFL_MAJOR_STAT_AREA_CDE
  INTO #SETS
  FROM B3_Fishing_Events E
    INNER JOIN D_Official_Catch C ON
    E.OBFL_HAIL_IN_NO = C.OBFL_HAIL_IN_NO AND
    E.OBFL_SET_NO = C.OBFL_SET_NO
  WHERE C.OBFL_SPECIES_CDE = @sppcode AND
    E.OBFL_LOG_TYPE_CDE = 'FISHERLOG' AND
    E.Fishing_Depth BETWEEN @mindep AND @maxdep

DECLARE @total AS FLOAT
SET @total = (SELECT SUM(ISNULL(CATCH_LBS,0) + ISNULL(DISCARDED_LBS,0)) AS TOTAL
  FROM D_Official_Catch C
    INNER JOIN #SETS S ON
    S.OBFL_HAIL_IN_NO = C.OBFL_HAIL_IN_NO AND
    S.OBFL_SET_NO = C.OBFL_SET_NO
  WHERE 
    C.OBFL_SPECIES_CDE NOT IN ('004','848','999','XXX') AND
    S.OBFL_MAJOR_STAT_AREA_CDE IN (@major) )

SELECT TOP @top SPECIES_DESC AS spp,
  SUM(ISNULL(CATCH_LBS,0) + ISNULL(DISCARDED_LBS,0)) / @total * 100 AS pct
  FROM D_Official_Catch C
  INNER JOIN #SETS S ON
    S.OBFL_HAIL_IN_NO = C.OBFL_HAIL_IN_NO AND
    S.OBFL_SET_NO = C.OBFL_SET_NO
    INNER JOIN C_Species SP ON
    C.OBFL_SPECIES_CDE = SP.SPECIES_CDE
  WHERE 
    C.OBFL_SPECIES_CDE NOT IN ('004','848','999','XXX') AND
    S.OBFL_MAJOR_STAT_AREA_CDE IN (@major) 
  GROUP BY SPECIES_DESC
  ORDER BY SUM(ISNULL(CATCH_LBS,0) + ISNULL(DISCARDED_LBS,0)) / @total DESC 

-- getData("phhl_concurrent.sql","PacHarvHL",strSpp="424",mindep=49,maxdep=101)

