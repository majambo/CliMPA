# === Define decadal years (your stacked anomalies) ===
years_decadal <- c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090)

# === Helper function: Time of Emergence (ToE) ===
# === Helper function: Time of Emergence (ToE) ===
compute_toe_per_mpa <- function(ts_matrix, mpa_polygons, years, threshold = 1.5) {
  toe <- rep(NA, nrow(mpa_polygons))
  
  for (i in 1:nrow(mpa_polygons)) {
    anomalies <- ts_matrix[i, ]
    exceed <- anomalies >= threshold   # include the threshold (≥)
    
    if (any(exceed, na.rm = TRUE)) {
      first_idx <- which(exceed)[1]    # first year where anomaly ≥ threshold
      toe[i] <- years[first_idx]       # assign corresponding year
    }
  }
  return(toe)
}


# === Build decadal time series for each MPA ===
# Absolute SSTs (mean across polygon for each decade)
ssp245_abs <- terra::extract(ssp245_sst, mpa_sf, fun = mean, na.rm = TRUE)
ssp585_abs <- terra::extract(ssp585_sst, mpa_sf, fun = mean, na.rm = TRUE)

# Drop ID column
ssp245_abs_mat <- as.matrix(ssp245_abs[ , -1])
ssp585_abs_mat <- as.matrix(ssp585_abs[ , -1])

# Baseline SST per MPA (already computed earlier as baseline_stats$Mean)
baseline_sst <- baseline_stats$Mean

# === Compute anomalies (projected – baseline) ===
ssp245_ts_mat <- sweep(ssp245_abs_mat, 1, baseline_sst, "-")
ssp585_ts_mat <- sweep(ssp585_abs_mat, 1, baseline_sst, "-")

# === Calculate exceedance probabilities ===
th1 <- 1.5
th2 <- 2.0

pct_exceed_245_1.5 <- rowMeans(ssp245_ts_mat >= th1, na.rm = TRUE) * 100
pct_exceed_245_2.0 <- rowMeans(ssp245_ts_mat >= th2, na.rm = TRUE) * 100
pct_exceed_585_1.5 <- rowMeans(ssp585_ts_mat >= th1, na.rm = TRUE) * 100
pct_exceed_585_2.0 <- rowMeans(ssp585_ts_mat >= th2, na.rm = TRUE) * 100

# === Time of Emergence (ToE) ===
toe245_1.5 <- compute_toe_per_mpa(ssp245_ts_mat, mpa_sf, years_decadal, threshold = th1)
toe245_2.0 <- compute_toe_per_mpa(ssp245_ts_mat, mpa_sf, years_decadal, threshold = th2)
toe585_1.5 <- compute_toe_per_mpa(ssp585_ts_mat, mpa_sf, years_decadal, threshold = th1)
toe585_2.0 <- compute_toe_per_mpa(ssp585_ts_mat, mpa_sf, years_decadal, threshold = th2)

# === Model spread (across decades) ===
spread245 <- apply(ssp245_ts_mat, 1, sd, na.rm = TRUE)
spread585 <- apply(ssp585_ts_mat, 1, sd, na.rm = TRUE)

# === Combine into final risk table ===
risk_table <- data.frame(
  MPA_Name = mpa_sf[[name_col]],
  
  # Baseline
  Baseline_SST = round(baseline_sst, 2),
  
  # Absolute SST projections (mean of all decades)
  Mean_SSP245_SST = round(rowMeans(ssp245_abs_mat, na.rm = TRUE), 2),
  Mean_SSP585_SST = round(rowMeans(ssp585_abs_mat, na.rm = TRUE), 2),
  
  # Anomalies (mean over decades relative to baseline)
  Mean_SSP245_Anom = round(rowMeans(ssp245_ts_mat, na.rm = TRUE), 2),
  Mean_SSP585_Anom = round(rowMeans(ssp585_ts_mat, na.rm = TRUE), 2),
  
  # Risk metrics
  Pct_SSP245_gt1.5 = round(pct_exceed_245_1.5, 1),
  Pct_SSP245_gt2.0 = round(pct_exceed_245_2.0, 1),
  Pct_SSP585_gt1.5 = round(pct_exceed_585_1.5, 1),
  Pct_SSP585_gt2.0 = round(pct_exceed_585_2.0, 1),
  
  ToE_SSP245_1.5 = toe245_1.5,
  ToE_SSP245_2.0 = toe245_2.0,
  ToE_SSP585_1.5 = toe585_1.5,
  ToE_SSP585_2.0 = toe585_2.0,
  
  Spread_SSP245 = round(spread245, 2),
  Spread_SSP585 = round(spread585, 2),
  
  Management_Notes = NA
)

# Save to CSV
write.csv(risk_table, "output/mpa_sst_risk_table.csv", row.names = FALSE)
cat("Combined risk table saved to 'mpa_sst_risk_table.csv'\n")

# Preview
head(risk_table)
