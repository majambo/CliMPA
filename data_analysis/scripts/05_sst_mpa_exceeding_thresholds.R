library(terra)
library(sf)
library(dplyr)

# assume: ssp245_anomaly and ssp585_anomaly are SpatRaster anomalies (single-layer or multi-model median)
# and mpa_sf is your sf MPA polygons (same CRS as rasters, or reproject)

# ensure same crs
mpa_sf <- st_transform(mpa_sf, crs(ssp245_anomaly))

# helper: percent of raster cells inside polygon above threshold
percent_above <- function(rast, polys, threshold){
  # classify raster as 1 (>threshold) or 0 (<=threshold), NA preserved
  bin <- rast > threshold
  # extract sum of 1s and count of non-NA cells per polygon
  ex_sum <- terra::extract(bin, polys, fun = sum, na.rm=TRUE, ID=TRUE)
  ex_count <- terra::extract(!is.na(rast), polys, fun = sum, na.rm=TRUE, ID=TRUE)  # counts cells with data
  # assemble
  df <- data.frame(ID = ex_sum$ID,
                   above = ex_sum[[2]],
                   cells = ex_count[[2]])
  df$percent <- 100 * df$above / df$cells
  df$percent[is.nan(df$percent)] <- NA
  return(df)
}

p_245_1p5 <- percent_above(ssp245_anomaly, mpa_sf, 1.5)
p_245_2p0 <- percent_above(ssp245_anomaly, mpa_sf, 2.0)

p_585_1p5 <- percent_above(ssp585_anomaly, mpa_sf, 1.5)
p_585_2p0 <- percent_above(ssp585_anomaly, mpa_sf, 2.0)

# Combine into one table with names
mpa_percent <- data.frame(
  MPA_Name = mpa_sf[[name_col]],
  pct_245_gt1.5 = p_245_1p5$percent,
  pct_245_gt2.0 = p_245_2p0$percent,
  pct_586_gt1.5 = p_585_1p5$percent,
  pct_585_gt2.0 = p_585_2p0$percent
)

# Save
write.csv(mpa_percent, "output/mpa_percent_area_exceedance.csv", row.names = FALSE)
print(head(mpa_percent))
