# Calculates pH statistics (mean, min, max, SD) for each MPA polygon across:
#   - Baseline (2000–2018 mean)
#   - SSP1-2.6 end-of-century (2090)
#   - SSP2-4.5 end-of-century (2090)
#   - SSP5-8.5 end-of-century (2090)
# Also computes pH anomaly (change from baseline) per MPA per scenario
# Handles MPAs with NA values using progressive buffer expansion
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)

# File paths 
path <- "data/bioracle/"
ph_baseline <- paste0(path, "ph_mean_baseline.nc")
ph_ssp585   <- paste0(path, "ssp585/ph_ssp585_2020_2100_depthmax_mean.nc")
ph_ssp245   <- paste0(path, "ssp245/ph_ssp245_2020_2100_depthmax_mean.nc")
ph_ssp126   <- paste0(path, "ssp126/ph_ssp126_2020_2100_depthmax_mean.nc")
shapefile   <- "data/mafia/mafia_mpa.gpkg"

mpa_shp     <- "data/wio/wio_mpas.shp"
output_csv  <- "output/mpa_ph_projections.csv"
output_png  <- "output/mpa_ph_anomalies_comparison.png"

# Estimate NA values using progressive buffer expansion
estimate_na_values <- function(raster_layer, mpa_polygons,
                                buffer_distance = 5000, max_buffer = 50000) {
  mpa_values <- terra::extract(raster_layer, mpa_polygons,
                                fun = mean, na.rm = TRUE, ID = TRUE)
  na_indices <- which(is.na(mpa_values[[2]]))

  if (length(na_indices) > 0) {
    cat("Found", length(na_indices),
        "MPAs with NA values. Estimating using nearby pixels...\n")

    for (i in na_indices) {
      current_buffer  <- buffer_distance
      estimated_value <- NA

      while (is.na(estimated_value) && current_buffer <= max_buffer) {
        buffered_mpa    <- st_buffer(mpa_polygons[i, ], dist = current_buffer)
        buffer_values   <- terra::extract(raster_layer, buffered_mpa,
                                          fun = mean, na.rm = TRUE)
        estimated_value <- buffer_values[[1]]

        if (is.na(estimated_value)) {
          current_buffer <- current_buffer + 5000
          cat("  MPA", i, ": No data within",
              current_buffer / 1000, "km, expanding...\n")
        } else {
          cat("  MPA", i, ": Estimated pH",
              round(estimated_value, 3), "using",
              current_buffer / 1000, "km buffer\n")
        }
      }

      if (!is.na(estimated_value)) {
        mpa_values[i, 2] <- estimated_value
      } else {
        cat("  WARNING: Could not estimate value for MPA", i,
            "even with", max_buffer / 1000, "km buffer\n")
      }
    }
  }
  return(mpa_values)
}

# Extract mean, min, max, SD for a raster layer across MPA polygons
get_raster_stats <- function(raster_layer, mpa_polygons, mpa_names) {
  data.frame(
    MPA_Name = mpa_names,
    Mean     = terra::extract(raster_layer, mpa_polygons,
                              fun = mean, na.rm = TRUE, ID = TRUE)[[2]],
    Min      = terra::extract(raster_layer, mpa_polygons,
                              fun = min,  na.rm = TRUE, ID = TRUE)[[2]],
    Max      = terra::extract(raster_layer, mpa_polygons,
                              fun = max,  na.rm = TRUE, ID = TRUE)[[2]],
    SD       = terra::extract(raster_layer, mpa_polygons,
                              fun = sd,   na.rm = TRUE, ID = TRUE)[[2]]
  ) |>
    mutate(Range = Max - Min)
}

# Load data
cat("Loading pH data...\n")

ph_baseline <- rast(ph_baseline)
ph_ssp126   <- rast(ph_ssp126)
ph_ssp245   <- rast(ph_ssp245)
ph_ssp585   <- rast(ph_ssp585)

cat("Loading MPA shapefile...\n")
mpa_sf <- st_read(mpa_shp, quiet = TRUE)
mpa_sf <- st_make_valid(mpa_sf)
cat("Loaded", nrow(mpa_sf), "MPAs\n")
crs(ph_baseline) <- "EPSG:4326"

# Find name column
name_col <- NULL
for (col in c("MPA_NAME","NAME","name","Name","SITE_NAME","Site_Name","NAME_ENG")) {
  if (col %in% names(mpa_sf)) { name_col <- col; break }
}
if (is.null(name_col)) {
  mpa_sf$MPA_ID <- paste0("MPA_", 1:nrow(mpa_sf))
  name_col <- "MPA_ID"
  cat("No name column found — using row numbers.\n")
}
cat("Using column '", name_col, "' for MPA names\n")

# Prepare raster layers
cat("\nPreparing raster layers...\n")

# Baseline: mean across its two time steps (2000, 2010)
# baseline_mean <- mean(ph_baseline, na.rm = TRUE)
baseline_mean <- ph_baseline

# Scenarios: use last layer (2090) as end-of-century
ph_ssp126_eoc <- ph_ssp126[[nlyr(ph_ssp126)]]
ph_ssp245_eoc <- ph_ssp245[[nlyr(ph_ssp245)]]
ph_ssp585_eoc <- ph_ssp585[[nlyr(ph_ssp585)]]

cat("  SSP1-2.6 layers:", nlyr(ph_ssp126), "| using layer", nlyr(ph_ssp126), "(2090)\n")
cat("  SSP2-4.5 layers:", nlyr(ph_ssp245), "| using layer", nlyr(ph_ssp245), "(2090)\n")
cat("  SSP5-8.5 layers:", nlyr(ph_ssp585), "| using layer", nlyr(ph_ssp585), "(2090)\n")

# pH anomalies (change from baseline)
anomaly_126 <- ph_ssp126_eoc - baseline_mean
anomaly_245 <- ph_ssp245_eoc - baseline_mean
anomaly_585 <- ph_ssp585_eoc - baseline_mean

# Extract statistics per MPA
cat("\nExtracting pH statistics per MPA...\n")

# Baseline statistics
cat("  Baseline...\n")
baseline_stats <- get_raster_stats(baseline_mean, mpa_sf, mpa_sf[[name_col]])

# Anomaly extraction with NA buffer handling
cat("  SSP1-2.6 anomaly...\n")
anom_126 <- estimate_na_values(anomaly_126, mpa_sf)

cat("  SSP2-4.5 anomaly...\n")
anom_245 <- estimate_na_values(anomaly_245, mpa_sf)

cat("  SSP5-8.5 anomaly...\n")
anom_585 <- estimate_na_values(anomaly_585, mpa_sf)

# Results table
mpa_results <- data.frame(
  MPA_Name           = mpa_sf[[name_col]],
  Baseline_pH_Mean   = round(baseline_stats$Mean,  4),
  Baseline_pH_SD     = round(baseline_stats$SD,    4),
  Baseline_pH_Min    = round(baseline_stats$Min,   4),
  Baseline_pH_Max    = round(baseline_stats$Max,   4),
  pH_Anomaly_SSP126  = round(anom_126[[2]],4),
  pH_Anomaly_SSP245  = round(anom_245[[2]],4),
  pH_Anomaly_SSP585  = round(anom_585[[2]],4),
  Future_ph_ssp126   = round(baseline_stats$Mean + anom_126[[2]], 4),
  Future_ph_ssp245   = round(baseline_stats$Mean + anom_245[[2]], 4),
  Future_ph_ssp585   = round(baseline_stats$Mean + anom_585[[2]], 4)
)

# SSP5-8.5 vs SSP1-2.6 spread (useful for risk ranking)
mpa_results$SSP585_vs_SSP126_diff <- round(
  mpa_results$pH_Anomaly_SSP585 - mpa_results$pH_Anomaly_SSP126, 4)

# Summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
cat("\nBaseline pH:\n")
cat("  Mean:", round(mean(mpa_results$Baseline_pH_Mean, na.rm = TRUE), 4), "\n")
cat("  Range:", paste(round(range(mpa_results$Baseline_pH_Mean, na.rm = TRUE), 4),
                      collapse = " – "), "\n")

for (scene in c("SSP126","SSP245","SSP585")) {
  col <- paste0("pH_Anomaly_", scene)
  cat("\n", scene, "pH anomaly (2090 vs baseline):\n")
  cat("  Mean:", round(mean(mpa_results[[col]], na.rm = TRUE), 4), "\n")
  cat("  Range:", paste(round(range(mpa_results[[col]], na.rm = TRUE), 4),
                        collapse = " – "), "\n")
}

# NA check
na_counts <- colSums(is.na(mpa_results))
na_counts <- na_counts[na_counts > 0]
if (length(na_counts) > 0) {
  cat("\nRemaining NAs:\n")
  print(na_counts)
} else {
  cat("\nNo remaining NA values.\n")
}


# Results
print(mpa_results)
write.csv(mpa_results, output_csv, row.names = FALSE)

# 7. pH anomaly comparison across scenarios 
plot_data <- mpa_results |>
  select(MPA_Name, pH_Anomaly_SSP126, pH_Anomaly_SSP245, pH_Anomaly_SSP585) |>
  pivot_longer(
    cols      = starts_with("pH_Anomaly"),
    names_to  = "Scenario",
    values_to = "pH_Anomaly"
  ) |>
  mutate(Scenario = case_when(
    Scenario == "pH_Anomaly_SSP126" ~ "SSP1-2.6",
    Scenario == "pH_Anomaly_SSP245" ~ "SSP2-4.5",
    Scenario == "pH_Anomaly_SSP585" ~ "SSP5-8.5"
  ),
  Scenario = factor(Scenario,
                    levels = c("SSP1-2.6","SSP2-4.5","SSP5-8.5")))

p <- ggplot(plot_data, aes(x = Scenario, y = pH_Anomaly, fill = Scenario)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey40", linewidth = 0.5) +
  scale_fill_manual(values = c(
    "SSP1-2.6" = "#1A9641",
    "SSP2-4.5" = "#F5A623",
    "SSP5-8.5" = "#D7191C"
  )) +
  labs(
    title   = "Ocean pH Anomalies by Climate Scenario — WIO MPAs",
    x       = "Scenario",
    y       = "pH Anomaly (2090 vs baseline)",
    caption = "End-of-century (2090) projections relative to 2000–2018 baseline"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "none"
  )

print(p)

ggsave(output_png, plot = p, width = 10, height = 6, dpi = 300, bg = "white")
cat("Plot saved as:", output_png, "\n")
