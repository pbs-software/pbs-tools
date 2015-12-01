-- Mean species weight calculated using `gfb_mean_weight.sql', which emulates PJS algorithm for GFBIO data
-- Last modified: 2015-10-28 (RH)
DECLARE @MEAN_WEIGHT TABLE (SPECIES_CODE VARCHAR(5), MNWT REAL)
INSERT INTO @MEAN_WEIGHT VALUES
  ('222', 1.759289),   -- ttype=c(1,4), gear=1
  ('396', 0.853620),  -- ttype=c(1,4), gear=1
  ('401', 1.724491),   -- ttype=c(1,4), gear=1, major=3:9
  ('405', 1.916324),   -- ttype=c(1,4), gear=1
  ('417', 1.392816),   -- ttype=c(1,4), gear=1, major=3:9 (queried 151123)
  ('418', 1.45),       -- Paul Starr conversion for 2014 YTR assessment
  ('442', 3.575088),   -- ttype=c(1,4), gear=5, major=3:9 (queried 150409)
  ('450', 0.3947),     -- not enough info in GFBioSQL to prform PJS regression; use mean weight from SST 2015 assessment
  ('451', 0.3947),     -- not enough info in GFBioSQL to prform PJS regression; use mean weight from SST 2015 assessment
  ('453', 0.2),        -- not enough info in GFBioSQL to prform PJS regression; use guesstimate mean weight
  ('602', 1.124977),   -- ttype=c(1,4), gear=1, major=3:9
  ('621', 0.534608)   -- ttype=c(1,4), gear=1

