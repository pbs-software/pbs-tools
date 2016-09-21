-- Concatenating values when the number of items is small and known upfront 
-- http://stackoverflow.com/questions/8868604/sql-group-concat-function-in-sql-server

SET NOCOUNT ON  -- prevents timeout errors

SELECT SAMPLE_ID,
  MAX(CASE WHEN seq IN (1) THEN STORAGE_CONTAINER_ID ELSE '' END ) +
  MAX(CASE WHEN seq IN (2) THEN '+' + STORAGE_CONTAINER_ID ELSE '' END) +
  MAX(CASE WHEN seq IN (3) THEN '+' + STORAGE_CONTAINER_ID ELSE '' END) +
  MAX(CASE WHEN seq IN (4) THEN '+' + STORAGE_CONTAINER_ID ELSE '' END) AS TRAYS
FROM (
  SELECT 
    SC1.SAMPLE_ID, SC1.STORAGE_CONTAINER_ID,
    (SELECT COUNT(*) 
      FROM SAMPLE_COLLECTED SC2
      WHERE SC2.SAMPLE_ID = SC1.SAMPLE_ID
        AND SC2.STORAGE_CONTAINER_ID <= SC1.STORAGE_CONTAINER_ID )
  FROM SAMPLE_COLLECTED SC1 ) D ( SAMPLE_ID, STORAGE_CONTAINER_ID, seq )
GROUP BY SAMPLE_ID

-- getData("gfb_concat_rows_example.sql","GFBioSQL",strSpp="396")

