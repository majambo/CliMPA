# Extracting Sea Level Anomaly statistics for all WIO MPAs 
# Data from the C3S satellite altimetry gridded monthly product (1993–2023).

library(terra)
library(sf)
library(tidyverse)
library(lubridate)

# File paths 
nc_folder <- "data/slr/gridded/"
mpa_shp   <- "data/wio/wio_mpas.shp"
output_dir <- "output/"

# Load the data
mpa_sf <- st_read(mpa_shp, quiet = TRUE) |> 
  st_make_valid()
mpa_v  <- vect(mpa_sf)

# Find name column
name_col <- NULL
for (col in c("MPA_NAME","NAME","name","Name","SITE_NAME","NAME_ENG")) {
  if (col %in% names(mpa_sf)) { name_col <- col; break }
}
if (is.null(name_col)) {
  mpa_sf$mpa_id <- paste0("MPA_", seq_len(nrow(mpa_sf)))
  name_col <- "mpa_id"
}
message("  ", nrow(mpa_sf), " MPAs loaded | name column: ", name_col)

# Load all monthly SLA files
message("\nLoading SLA rasters...")
nc_files <- list.files(nc_folder, pattern = "\\.nc$",
                       full.names = TRUE) |> sort()

if (length(nc_files) == 0) stop("No .nc files found in: ", nc_folder)
message("  ", length(nc_files), " monthly files found")

# Load SLA layers only (first variable = sla, second = eke)
r_sla <- rast(lapply(nc_files, function(f) {
  r <- rast(f)
  r[[grep("^sla", names(r), value = FALSE)[1]]]
}))

if (is.na(crs(r_sla))) crs(r_sla) <- "EPSG:4326"

# Parse dates
t     <- time(r_sla)
dates <- as.Date(t)
if (all(is.na(dates))) {
  dates <- as.Date(paste0(str_extract(basename(nc_files), "\\d{6}"), "01"),
                   format = "%Y%m%d")
}

# Convert to mm
r_sla <- r_sla * 1000

message("  Date range: ", as.character(min(dates, na.rm = TRUE)),
        " to ", as.character(max(dates, na.rm = TRUE)))
message("  Layers    : ", nlyr(r_sla))

# Define time windows
early_idx  <- which(year(dates) %in% 1993:1997)
recent_idx <- which(year(dates) %in% 2019:2023)

message("\n  Early period  (1993-1997): ", length(early_idx), " months")
message("  Recent period (2019-2023): ", length(recent_idx), " months")

# Extract stats per MPA
extract_mpa_stats <- function(i) {

  mpa_single <- mpa_v[i]
  mpa_name   <- mpa_sf[[name_col]][i]

  # Extract all monthly values for this MPA
  vals <- terra::extract(r_sla, mpa_single,
                         fun = mean, na.rm = TRUE, ID = FALSE)

  if (nrow(vals) == 0 || all(is.na(vals))) {
    # Try with a small buffer for small MPAs that may miss pixels
    mpa_buf <- buffer(mpa_single, width = 25000)  # 25km buffer
    vals    <- terra::extract(r_sla, mpa_buf,
                              fun = mean, na.rm = TRUE, ID = FALSE)
  }

  monthly <- as.numeric(vals[1, ])

  if (all(is.na(monthly))) {
    return(tibble(mpa_name = mpa_name,
                  mean_sla_mm = NA, sd_sla_mm = NA,
                  min_sla_mm = NA, max_sla_mm = NA,
                  range_sla_mm = NA, early_mean_mm = NA,
                  recent_mean_mm = NA, change_mm = NA,
                  n_months_positive = NA, pct_positive = NA))
  }

  tibble(
    mpa_name          = mpa_name,
    mean_sla_mm       = round(mean(monthly, na.rm = TRUE), 2),
    sd_sla_mm         = round(sd(monthly, na.rm = TRUE), 2),
    min_sla_mm        = round(min(monthly, na.rm = TRUE), 2),
    max_sla_mm        = round(max(monthly, na.rm = TRUE), 2),
    range_sla_mm      = round(max(monthly, na.rm = TRUE) - min(monthly, na.rm = TRUE), 2),
    early_mean_mm     = round(mean(monthly[early_idx],   na.rm = TRUE), 2),
    recent_mean_mm    = round(mean(monthly[recent_idx],  na.rm = TRUE), 2),
    change_mm         = round(mean(monthly[recent_idx],  na.rm = TRUE) - mean(monthly[early_idx],  na.rm = TRUE), 2),
    n_months_positive = sum(monthly > 0, na.rm = TRUE),
    pct_positive      = round(mean(monthly > 0, na.rm = TRUE) * 100, 1)
  )
}

# Run extraction for all MPAs
results <- map_dfr(seq_len(nrow(mpa_sf)), function(i) {
  if (i %% 20 == 0) message("  Processing MPA ", i, " of ", nrow(mpa_sf))
  extract_mpa_stats(i)
})

# Save results
out_csv <- file.path(output_dir, "mpa_sla_statistics.csv")
write_csv(results, out_csv)

message("MPAs processed    : ", nrow(results))
message("MPAs with data    : ", sum(!is.na(results$mean_sla_mm)))
message("MPAs with NA      : ", sum(is.na(results$mean_sla_mm)))
message("Mean SLA (all MPAs): ",
        round(mean(results$mean_sla_mm, na.rm = TRUE), 2), " mm")
message("Mean change (recent vs early): ",
        round(mean(results$change_mm, na.rm = TRUE), 2), " mm")

print(results, n = 20)

# Plotting
# Join stats back to sf for mapping
mpa_plot <- mpa_sf |>
  left_join(results, by = setNames("mpa_name", name_col))

# Barplot of SLA change by MPA 
fig_box <- results |>
  filter(!is.na(change_mm)) |>
  mutate(mpa_name = fct_reorder(mpa_name, change_mm)) |>
  ggplot(aes(x = change_mm, y = mpa_name, fill = change_mm)) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "grey40",
             linewidth = 0.5, linetype = "dashed") +
  scale_fill_gradient2(
    low      = "#2C7BB6",
    mid      = "white",
    high     = "#D7191C",
    midpoint = 0,
    guide    = "none"
  ) +
  labs(
    title    = "SLA Change per WIO MPA (Recent vs Early Period)",
    subtitle = "2019-2023 mean minus 1993-1997 mean | Red = rising faster",
    x        = "Change in mean SLA (mm)",
    y        = NULL,
    caption  = "Source: Copernicus C3S satellite-sea-level-global"
  ) +
  theme_minimal(base_size = 9) +
  theme(plot.title       = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.text.y      = element_text(size = 7))

ggsave(file.path(output_dir, "mpa_sla_change_bar.png"),
       fig_box,
       width  = 10,
       height = max(8, nrow(results) * 0.18),
       dpi    = 300,
       bg     = "white")

