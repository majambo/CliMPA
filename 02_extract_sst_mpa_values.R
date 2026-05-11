# Load libraries
library(terra)
library(sf)
library(dplyr)

# Function to estimate values for MPAs with NA using nearby pixels
estimate_na_values <- function(raster_layer, mpa_polygons, buffer_distance = 5000, max_buffer = 50000){
  
  # Extract initial values
  mpa_values <- terra::extract(raster_layer, mpa_polygons, fun = mean, na.rm = TRUE, ID = TRUE)
  
  # Identify MPAs with NA values
  na_indices <- which(is.na(mpa_values[[2]]))
  
  if (length(na_indices) > 0) {
    cat("Found", length(na_indices), "MPAs with NA values. Estimating using nearby pixels...\n")
    
    for (i in na_indices) {
      current_buffer <- buffer_distance
      estimated_value <- NA
      
      # Progressively increase buffer distance until we find valid data
      while (is.na(estimated_value) && current_buffer <= max_buffer) {
        # Create buffer around the MPA
        buffered_mpa <- st_buffer(mpa_polygons[i, ], dist = current_buffer)
        
        # Extract values from buffered area
        buffer_values <- terra::extract(raster_layer, buffered_mpa, fun = mean, na.rm = TRUE)
        estimated_value <- buffer_values[[1]]
        
        if (is.na(estimated_value)) {
          current_buffer <- current_buffer + 5000  # Increase buffer by 5km
          cat("  MPA", i, ": No data found within", current_buffer/1000, "km buffer, expanding search...\n")
        } else {
          cat("  MPA", i, ": Estimated value", round(estimated_value, 3), "using", current_buffer/1000, "km buffer\n")
        }
      }
      
      # Update the value
      if (!is.na(estimated_value)) {
        mpa_values[i, 2] <- estimated_value
      } else {
        cat("  WARNING: Could not estimate value for MPA", i, "even with", max_buffer/1000, "km buffer\n")
      }
    }
  }
  
  return(mpa_values)
}

# Function to get comprehensive raster statistics
get_raster_stats <- function(raster_layer, mpa_polygons, mpa_names) {
  
  # Extract multiple statistics
  stats_mean <- terra::extract(raster_layer, mpa_polygons, fun = mean, na.rm = TRUE, ID = TRUE)
  stats_min <- terra::extract(raster_layer, mpa_polygons, fun = min, na.rm = TRUE, ID = TRUE)
  stats_max <- terra::extract(raster_layer, mpa_polygons, fun = max, na.rm = TRUE, ID = TRUE)
  stats_sd <- terra::extract(raster_layer, mpa_polygons, fun = sd, na.rm = TRUE, ID = TRUE)
  
  # Combine statistics
  stats_df <- data.frame(
    MPA_Name = mpa_names,
    Mean = stats_mean[[2]],
    Min = stats_min[[2]],
    Max = stats_max[[2]],
    SD = stats_sd[[2]],
    Range = stats_max[[2]] - stats_min[[2]]
  )
  
  return(stats_df)
}

# Set working directory if needed
# setwd("path/to/your/data")

# Load data with error checking
cat("Loading climate data...\n")

# Load SST data
if (!file.exists("data/bioracle/thetao_mean_baseline.nc")) {
  stop("Baseline SST file not found!")
}
baseline_sst <- rast("data/bioracle/thetao_mean_baseline.nc")

if (!file.exists("data/bioracle/ssp245/thetao_ssp245_2020_2100_depthmax_thetao_mean.nc")) {
  stop("SSP245 SST file not found!")
}
ssp245_sst <- rast("data/bioracle/ssp245/thetao_ssp245_2020_2100_depthmax_thetao_mean.nc")

if (!file.exists("data/bioracle/ssp585/thetao_ssp585_2020_2100_depthmax_thetao_mean.nc")) {
  stop("SSP585 SST file not found!")
}
ssp585_sst <- rast("data/bioracle/ssp585/thetao_ssp585_2020_2100_depthmax_thetao_mean.nc")

# Baseline projection
crs(baseline_sst) <- "EPSG:4326"

# Crop baseline to match future scenarios extent
baseline_sst_wio <- crop(baseline_sst, ext(ssp245_sst))

# Load MPA shapefile
if (!file.exists("data/wio/wio_mpas.shp")) {
  stop("MPA shapefile not found!")
}
mpa_sf <- st_read("data/wio/wio_mpas.shp", quiet = TRUE)
mpa_sf <- st_make_valid(mpa_sf)
# mpa_sf <- st_crop(mpa_sf, st_bbox(ssp245_sst))
# mpa_sf2 <- vect(mpa_sf)

cat("Loaded", nrow(mpa_sf), "MPAs\n")

# Check and standardize column names
available_cols <- names(mpa_sf)
name_col <- NULL

# Look for common name columns
possible_names <- c("MPA_NAME", "NAME", "name", "Name", "SITE_NAME", "Site_Name")
for (col in possible_names) {
  if (col %in% available_cols) {
    name_col <- col
    break
  }
}

if (is.null(name_col)) {
  cat("Warning: No standard name column found. Available columns:", paste(available_cols, collapse = ", "), "\n")
  cat("Using row numbers as identifiers.\n")
  mpa_sf$MPA_ID <- paste0("MPA_", 1:nrow(mpa_sf))
  name_col <- "MPA_ID"
}

cat("Using column '", name_col, "' for MPA names\n")

# Calculate the mean of the baseline period (2000-2019)
cat("Calculating baseline mean...\n")
baseline_mean <- mean(baseline_sst_wio, na.rm = TRUE)

# Extract the end-of-century data (2090) for each future scenario
cat("Processing future scenarios...\n")
n_layers_245 <- nlyr(ssp245_sst)
n_layers_585 <- nlyr(ssp585_sst)

cat("SSP245 has", n_layers_245, "layers\n")
cat("SSP585 has", n_layers_585, "layers\n")

# Use the last layer as end-of-century
ssp245_eoc <- ssp245_sst[[n_layers_245]]
ssp585_eoc <- ssp585_sst[[n_layers_585]]

# Calculate SST anomalies at the end of the century
cat("Calculating anomalies...\n")
ssp245_anomaly <- ssp245_eoc - baseline_mean
ssp585_anomaly <- ssp585_eoc - baseline_mean

# Extract anomaly values for each MPA with estimation for NAs
cat("Extracting values for MPAs...\n")
ssp245_mpa_anomalies <- estimate_na_values(ssp245_anomaly, mpa_sf, buffer_distance = 5000)
ssp585_mpa_anomalies <- estimate_na_values(ssp585_anomaly, mpa_sf, buffer_distance = 5000)

# Get comprehensive statistics for baseline temperatures
cat("Calculating baseline statistics...\n")
baseline_stats <- get_raster_stats(baseline_mean, mpa_sf, mpa_sf[[name_col]])

# Create results data frames
mpa_results <- data.frame(
  MPA_Name = mpa_sf[[name_col]],
  Baseline_SST_Mean = baseline_stats$Mean,
  Baseline_SST_SD = baseline_stats$SD,
  SST_Anomaly_SSP245 = ssp245_mpa_anomalies[[2]],
  SST_Anomaly_SSP585 = ssp585_mpa_anomalies[[2]],
  Future_SST_SSP245 = baseline_stats$Mean + ssp245_mpa_anomalies[[2]],
  Future_SST_SSP585 = baseline_stats$Mean + ssp585_mpa_anomalies[[2]]
)

# Add additional useful columns
mpa_results$SSP585_vs_SSP245_diff <- mpa_results$SST_Anomaly_SSP585 - mpa_results$SST_Anomaly_SSP245

# Summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
cat("Baseline SST (°C):\n")
cat("  Mean:", round(mean(mpa_results$Baseline_SST_Mean, na.rm = TRUE), 2), "\n")
cat("  Range:", round(range(mpa_results$Baseline_SST_Mean, na.rm = TRUE), 2), "\n")

cat("\nSST Anomalies (°C):\n")
cat("SSP245 scenario:\n")
cat("  Mean:", round(mean(mpa_results$SST_Anomaly_SSP245, na.rm = TRUE), 2), "\n")
cat("  Range:", round(range(mpa_results$SST_Anomaly_SSP245, na.rm = TRUE), 2), "\n")

cat("SSP585 scenario:\n")
cat("  Mean:", round(mean(mpa_results$SST_Anomaly_SSP585, na.rm = TRUE), 2), "\n")
cat("  Range:", round(range(mpa_results$SST_Anomaly_SSP585, na.rm = TRUE), 2), "\n")

# Check for remaining NA values
na_count_245 <- sum(is.na(mpa_results$SST_Anomaly_SSP245))
na_count_585 <- sum(is.na(mpa_results$SST_Anomaly_SSP585))

if (na_count_245 > 0 || na_count_585 > 0) {
  cat("\nWarning: Still have NA values after estimation:\n")
  cat("  SSP245:", na_count_245, "MPAs\n")
  cat("  SSP585:", na_count_585, "MPAs\n")
  
  if (na_count_245 > 0) {
    na_mpas_245 <- mpa_results$MPA_Name[is.na(mpa_results$SST_Anomaly_SSP245)]
    cat("  SSP245 MPAs with NA:", paste(na_mpas_245, collapse = ", "), "\n")
  }
  
  if (na_count_585 > 0) {
    na_mpas_585 <- mpa_results$MPA_Name[is.na(mpa_results$SST_Anomaly_SSP585)]
    cat("  SSP585 MPAs with NA:", paste(na_mpas_585, collapse = ", "), "\n")
  }
}

# Display results
cat("\n=== MPA CLIMATE PROJECTIONS ===\n")
print(mpa_results)

# Optional: Save results to CSV
write.csv(mpa_results, "output/mpa_sst_projections.csv", row.names = FALSE)

# Optional: Create a simple plot
if (require(ggplot2, quietly = TRUE)) {
  library(ggplot2)
  
  # Reshape data for plotting
  plot_data <- mpa_results %>%
    select(MPA_Name, SST_Anomaly_SSP245, SST_Anomaly_SSP585) %>%
    tidyr::pivot_longer(cols = c(SST_Anomaly_SSP245, SST_Anomaly_SSP585),
                        names_to = "Scenario", values_to = "Anomaly") %>%
    mutate(Scenario = gsub("SST_Anomaly_", "", Scenario))
  
  p <- ggplot(plot_data, aes(x = Scenario, y = Anomaly, fill = Scenario)) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    labs(title = "SST Anomalies by Climate Scenario",
         x = "Climate Scenario", 
         y = "Temperature Anomaly (°C)",
         caption = "End-of-century projections relative to 2000-2019 baseline") +
    theme_minimal() +
    scale_fill_manual(values = c("SSP245" = "#2E86AB", "SSP585" = "#A23B72"))
  
  print(p)
  
  ggsave("output/mpa_sst_anomalies_comparison.png", plot = p, width = 10, height = 6, dpi = 300)
  cat("Plot saved as 'mpa_sst_anomalies_comparison.png'\n")
}

