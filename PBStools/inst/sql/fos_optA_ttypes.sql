-- /*
-- *** Once trips go back to using observers, need to set up an end date ***
-- 
-- From April 1 2020, groundfish option A trips were able to hail out without an at-sea observer.
-- To accomodate these data in FOS, the trips were hailed out as hake shoreside trips with a hail-out
-- comment indicating the trip was really a regular option A trip. Some vessels/trips did not include 
-- the hail-out comment, and occasionally they recorded the trip type in the hail-in comments instead.
-- 
-- Regular option A trips are identified using the following criteria:
-- 	- target species are groundfish other than hake
-- 	- target species is hake but the vessel is a freezer boat or a "receiving tank vessel" (RTV)
-- 	- target species is hake but the hail comments indicate option A
-- 
-- Hake shoreside trips are identifed using the following criteria:
-- 	- target species is hake but hail comments do not indicate option A (and the vessel is not
-- 		identified as RTV or a freezer boat)
-- 	- a few trip had to be identified as shoreside manually by inspecting the records in FOS
-- 
-- Note that there were no actual hake shoreside trips in January - March 2020.  Other trip types
-- are not affected.
-- */
SELECT * FROM (
SELECT T.TRIP_ID, T.TRIP_START_DATE, V.VESSEL_REGISTRATION_NUMBER AS VRN, V.VESSEL_NAME,
	C.TRIP_CATEGORY AS REPORTED_TRIP_CATEGORY,
	CASE
		WHEN -- option A trips
			GF.TRIP_ID IS NOT NULL -- target species is other groundfish (not hake)
			OR V.VESSEL_REGISTRATION_NUMBER IN (310913,312275,310988,312405,313224) -- freezer boats
			OR HO.HAIL_OUT_COMMENT LIKE '%RTV%' -- receiving tank vessels
			OR -- various versions of 'option a bottom trawl' in the hail OUT comments
				HO.HAIL_OUT_COMMENT LIKE '%ottom%' OR 
				HO.HAIL_OUT_COMMENT LIKE '%option%' OR 
				HO.HAIL_OUT_COMMENT LIKE '%groun%'
			OR -- various versions of 'option a bottom trawl' in the hail IN comments
				HI.HAIL_IN_COMMENT LIKE '%ottom%' OR 
				HI.HAIL_IN_COMMENT LIKE '%option%' OR 
				HI.HAIL_IN_COMMENT LIKE '%groun%'
			THEN 'OPT A - QUOTA'
		WHEN -- actual shoreside trips 
			T.TRIP_ID IN (399640,400085) -- specific trips that I checked manually
			OR (HAKE.TRIP_ID IS NOT NULL AND HO.HAIL_OUT_COMMENT IS NULL) -- target hake, no hail out comment
			THEN C.TRIP_CATEGORY
		ELSE NULL END AS CORRECTED_TRIP_CATEGORY,
	G.GEAR AS REPORTED_GEAR_TYPE, 
	CASE WHEN HAKE.TRIP_ID IS NOT NULL THEN 'HAKE' WHEN GF.TRIP_ID IS NOT NULL THEN 'OTHER GF' ELSE NULL END AS TARGET_SPECIES,
	HO.HAIL_OUT_COMMENT, HI.HAIL_IN_COMMENT
FROM GFFOS.DBO.GF_TRIP T 
INNER JOIN (-- get the reported trip category plus limit the trips to completed trips with catch
	SELECT TRIP_ID, TRIP_CATEGORY FROM GFFOS.dbo.GF_D_OFFICIAL_FE_CATCH GROUP BY TRIP_ID, TRIP_CATEGORY
	) C ON C.TRIP_ID = T.TRIP_ID
INNER JOIN GFFOS.dbo.VESSEL V ON V.VESSEL_REGISTRATION_NUMBER = T.VESSEL_REGISTRATION_NUMBER
LEFT JOIN (-- hail out comments
	SELECT TRIP_ID, HAIL_NUMBER AS HAIL_OUT, HAIL_COMMENT AS HAIL_OUT_COMMENT
	FROM GFFOS.dbo.GF_HAIL_NUMBER WHERE HAIL_TYPE = 'OUT' AND HAIL_COMMENT IS NOT NULL
	) HO ON HO.TRIP_ID = T.TRIP_ID
LEFT JOIN (-- hail in comments; limit to just the first hail in
	SELECT TRIP_ID, MIN(HAIL_NUMBER) AS HAIL_IN, HAIL_COMMENT AS HAIL_IN_COMMENT
	FROM GFFOS.dbo.GF_HAIL_NUMBER WHERE HAIL_TYPE = 'IN' AND HAIL_COMMENT IS NOT NULL AND TRIP_ID != 399800
	GROUP BY TRIP_ID, HAIL_COMMENT
	) HI ON HI.TRIP_ID = T.TRIP_ID
INNER JOIN (--reported gear type from GFFOS_BUILD trawl specs
	SELECT TRIP_ID,
		CASE WHEN BOTTOM_TRAWL = 0 THEN 'MIDWATER TRAWL' WHEN MIDWATER_TRAWL = 0 THEN 'BOTTOM TRAWL'
			WHEN BOTTOM_TRAWL > 0 AND MIDWATER_TRAWL>0 THEN 'MIX' END AS GEAR
	FROM (-- gear type is by set, but we want to know at a trip level
		SELECT F.TRIP_ID, 
			SUM(CASE WHEN ISNULL(BOTTOM_TOW_IND,0) = 1 THEN 1 ELSE 0 END) AS BOTTOM_TRAWL,
			SUM(CASE WHEN ISNULL(BOTTOM_TOW_IND,0) = 0 THEN 1 ELSE 0 END) AS MIDWATER_TRAWL
		FROM GFFOS.dbo.GF_FISHING_EVENT F 
		LEFT JOIN GFFOS_Build.dbo.GF_FE_TRAWL_SPECS GFB ON F.FISHING_EVENT_ID = GFB.FISHING_EVENT_ID
		GROUP BY F.TRIP_ID
		) A
	) G ON G.TRIP_ID = T.TRIP_ID
-- /*LEFT JOIN GFDataReq.dbo.PANDEMIC_HAKE_TRIPS_VW HT ON HT.TRIP_ID = T.TRIP_ID*/
LEFT JOIN (-- Target species hake
	SELECT F.TRIP_ID FROM GFFOS.dbo.GF_FE_TARGET_SPECIES TS
	INNER JOIN GFFOS.dbo.GF_FISHING_EVENT F ON F.FISHING_EVENT_ID = TS.FISHING_EVENT_ID
	WHERE TS.PRIORITY = 1 AND TS.SPECIES_CODE = '225' GROUP BY F.TRIP_ID
	) HAKE ON HAKE.TRIP_ID = T.TRIP_ID
LEFT JOIN (-- Target species other groundfish (not hake)
	SELECT F.TRIP_ID FROM GFFOS.dbo.GF_FE_TARGET_SPECIES TS
	INNER JOIN GFFOS.dbo.GF_FISHING_EVENT F ON F.FISHING_EVENT_ID = TS.FISHING_EVENT_ID
	WHERE TS.PRIORITY = 1 AND TS.SPECIES_CODE != '225' GROUP BY F.TRIP_ID
	) GF ON GF.TRIP_ID = T.TRIP_ID
WHERE C.TRIP_CATEGORY = 'OPT A - HAKE QUOTA (SHORESIDE)' AND T.TRIP_START_DATE > '2020-04-01'
) A
ORDER BY VESSEL_NAME, TARGET_SPECIES DESC, CORRECTED_TRIP_CATEGORY DESC, REPORTED_GEAR_TYPE, TRIP_ID


--qu("fos_optA_ttypes.sql",dbName="GFFOS",strSpp="ABC")

