-- SQL query (2008-02-28) for IPHC survey data now deprecated. Use 'gfb_iphc.sql'.
SET NOCOUNT ON

SELECT     TOP 100 PERCENT dbo.B01_TRIP.TRIP_ID, dbo.B02_FISHING_EVENT.FE_MAJOR_LEVEL_ID, YEAR(dbo.B01_TRIP.TRIP_START_DATE) AS [year], 
                      dbo.B02_FISHING_EVENT.BLOCK_DESIGNATION AS station, dbo.B03_CATCH.SPECIES_CODE AS spp, dbo.B03_CATCH.CATCH_COUNT AS cnt, 
                      dbo.IPHC_EFFECTIVE_SKATE.EFFECTIVE_SKATE AS effsk, 
                      dbo.B03_CATCH.CATCH_COUNT / dbo.IPHC_EFFECTIVE_SKATE.EFFECTIVE_SKATE AS cpue
INTO #FISH

FROM         dbo.B01_TRIP INNER JOIN
                      dbo.B02_FISHING_EVENT ON dbo.B01_TRIP.TRIP_ID = dbo.B02_FISHING_EVENT.TRIP_ID INNER JOIN
                      dbo.B02L3_Link_Fishing_Event_Catch ON 
                      dbo.B02_FISHING_EVENT.FISHING_EVENT_ID = dbo.B02L3_Link_Fishing_Event_Catch.FISHING_EVENT_ID INNER JOIN
                      dbo.B03_CATCH ON dbo.B02L3_Link_Fishing_Event_Catch.CATCH_ID = dbo.B03_CATCH.CATCH_ID INNER JOIN
                      dbo.IPHC_EFFECTIVE_SKATE ON dbo.B02_FISHING_EVENT.FISHING_EVENT_ID = dbo.IPHC_EFFECTIVE_SKATE.FISHING_EVENT_ID
WHERE dbo.B03_CATCH.SPECIES_CODE = @sppcode
GROUP BY dbo.B01_TRIP.TRIP_ID, dbo.B02_FISHING_EVENT.FE_MAJOR_LEVEL_ID, dbo.B02_FISHING_EVENT.FE_SUB_LEVEL_ID, 
                      YEAR(dbo.B01_TRIP.TRIP_START_DATE), dbo.B02_FISHING_EVENT.BLOCK_DESIGNATION, dbo.B03_CATCH.SPECIES_CODE, 
                      dbo.B03_CATCH.CATCH_COUNT, dbo.IPHC_EFFECTIVE_SKATE.EFFECTIVE_SKATE, 
                      dbo.B03_CATCH.CATCH_COUNT / dbo.IPHC_EFFECTIVE_SKATE.EFFECTIVE_SKATE
HAVING      (dbo.B01_TRIP.TRIP_ID = 52040 OR
                      dbo.B01_TRIP.TRIP_ID = 52041 OR
                      dbo.B01_TRIP.TRIP_ID = 56913 OR
                      dbo.B01_TRIP.TRIP_ID = 56914 OR
                      dbo.B01_TRIP.TRIP_ID = 56915 OR
                      dbo.B01_TRIP.TRIP_ID = 60247 OR
                      dbo.B01_TRIP.TRIP_ID = 60248 OR
                      dbo.B01_TRIP.TRIP_ID = 60249 OR
                      dbo.B01_TRIP.TRIP_ID = 62006 OR
                      dbo.B01_TRIP.TRIP_ID = 62007 OR
                      dbo.B01_TRIP.TRIP_ID = 62008 OR
                      dbo.B01_TRIP.TRIP_ID = 64846 OR
                      dbo.B01_TRIP.TRIP_ID = 64847) AND (dbo.B02_FISHING_EVENT.FE_SUB_LEVEL_ID IS NULL)

SELECT     *
INTO #STATIONS
FROM         IPHC_stations10km

SELECT     A.[Year] AS [year], A.station,  @sppcode as [spp], A.effsk, isnull(B.cnt,0) as pcs, isnull(B.cpue,0) as cpue
FROM         #STATIONS A LEFT OUTER JOIN
                      #FISH B ON B.[Year] = A.[year] AND B.station = A.station
WHERE A.[Year]>2002

