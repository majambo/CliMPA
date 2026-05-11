library(dplyr)

# Total MPAs
total_mpas <- nrow(mpa_results)

# Calculate percentages
mpa_exceedance <- mpa_results %>%
  summarise(
    SSP245_gt1p5 = mean(SST_Anomaly_SSP245 > 1.5, na.rm = TRUE) * 100,
    SSP245_gt2p0 = mean(SST_Anomaly_SSP245 > 2.0, na.rm = TRUE) * 100,
    SSP585_gt1p5 = mean(SST_Anomaly_SSP585 > 1.5, na.rm = TRUE) * 100,
    SSP585_gt2p0 = mean(SST_Anomaly_SSP585 > 2.0, na.rm = TRUE) * 100
  )

print(mpa_exceedance)
