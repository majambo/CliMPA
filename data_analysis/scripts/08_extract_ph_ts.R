# Extract pH for MIMP polygon
# MIMP - Mafia Island Marine Park

library(terra)
library(sf)
library(tidyverse)

# Load MIMP boundary 
shapefile <- "data/mafia/mafia_mpa.gpkg"

mimp    <- st_read(shapefile, quiet = TRUE) |> st_make_valid()
mimp_v  <- vect(mimp)

# Extract mean pH over MIMP for all layers in a file 
extract_mimp_ph <- function(nc_file, scenario) {

  r        <- rast(nc_file)
  crs(r) <- "EPSG:4326"
  
  r_masked <- r |> crop(mimp_v) |> mask(mimp_v)

  # Get time values 
  time_vals <- time(r_masked)
  
  if (is.null(time_vals) || all(is.na(time_vals))) {
    # Fall back to layer index if time metadata missing
    years <- seq(2000, by = 10, length.out = nlyr(r_masked))
  } else {
    years <- as.integer(format(as.Date(time_vals), "%Y"))
  }

  # Spatial mean per layer
  means <- global(r_masked, "mean", na.rm = TRUE)[[1]]

  tibble(
    scenario = scenario,
    year     = years,
    ph_mean  = round(means, 4)
  )
}


# Extract all scenarios 
message("Extracting pH for MIMP polygon...")

path <- "data/bioracle/"
ph_baseline <- paste0(path, "ph_mean_baseline.nc")
ph_ssp585   <- paste0(path, "ssp585/ph_ssp585_2020_2100_depthmax_mean.nc")
ph_ssp245   <- paste0(path, "ssp245/ph_ssp245_2020_2100_depthmax_mean.nc")
ph_ssp126  <- paste0(path, "ssp126/ph_ssp126_2020_2100_depthmax_mean.nc")

results <- bind_rows(
  extract_mimp_ph(ph_baseline, "Baseline"),
  extract_mimp_ph(ph_ssp126,   "SSP1-2.6"),
  extract_mimp_ph(ph_ssp245,   "SSP2-4.5"),
  extract_mimp_ph(ph_ssp585,   "SSP5-8.5")
)

# Add change from baseline (mean of baseline years)
baseline_mean <- results |>
  filter(scenario == "Baseline") |>
  summarise(ref = mean(ph_mean)) |>
  pull(ref)

results <- results |>
  mutate(
    ph_change   = round(ph_mean - baseline_mean, 4),
    scenario    = factor(scenario,
                         levels = c("Baseline","SSP1-2.6","SSP2-4.5","SSP5-8.5"))
  )

# Save
output_dir <- "output/"

write_csv(results, paste0(output_dir, "ph_mimp_timeseries.csv"))

# Print summary
message("\n--- pH summary ---")
print(results, n = 50)

message("\nBaseline mean pH (MIMP): ", round(baseline_mean, 4))

# colour scheme
colours <- c(
  "Baseline"  = "#2C7BB6",
  "SSP1-2.6"  = "#1A9641",
  "SSP2-4.5"  = "#F5A623",
  "SSP5-8.5"  = "#D7191C"
)

# Time series plot

ph <- read_csv(paste0(output_dir, "ph_mimp_timeseries.csv"), show_col_types = FALSE) |>
  mutate(scenario = factor(scenario,
                           levels = c("Baseline", "SSP1-2.6", "SSP2-4.5", "SSP5-8.5")))

baseline_ph <- ph |> filter(scenario == "Baseline") |> pull(ph_mean) |> mean()

# Attach baseline point to each SSP line so they start from the same origin
baseline_pt <- ph |>
  filter(scenario == "Baseline") |>
  summarise(year = mean(year), ph_mean = mean(ph_mean), ph_change = 0)

# ssp_with_anchor <- ph |>
#   filter(scenario != "Baseline") |>
#   bind_rows(
#     mutate(baseline_pt, scenario = "SSP1-2.6"),
#     mutate(baseline_pt, scenario = "SSP2-4.5"),
#     mutate(baseline_pt, scenario = "SSP5-8.5")
#   ) |>
#   mutate(scenario = factor(scenario,
#                            levels = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5")))
anchor_rows <- map_dfr(
  c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5"),
  ~ mutate(baseline_pt, scenario = .x)
)

ssp_with_anchor <- ph |>
  filter(scenario != "Baseline") |>
  bind_rows(anchor_rows) |>
  mutate(scenario = factor(scenario,
                           levels = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5")))

fig_ts <- ggplot() +
  # Baseline point
  geom_point(data = ph |> filter(scenario == "Baseline"),
             aes(x = year, y = ph_mean),
             colour = "#2C7BB6", size = 3) +
  # Scenario lines
  geom_line(data  = ssp_with_anchor,
            aes(x = year, y = ph_mean, colour = scenario),
            linewidth = 0.9) +
  geom_point(data = ssp_with_anchor,
             aes(x = year, y = ph_mean, colour = scenario),
             size = 2.2) +
  scale_colour_manual(values = colours[c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5")],
                      name = "Scenario") +
  scale_x_continuous(breaks = seq(2000, 2090, by = 10)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  labs(
    title    = "Ocean pH at Mafia Island Marine Park (2000\u20132090)",
    subtitle = paste("Baseline mean pH:",round(baseline_ph, 3)),
    x = "Year", y = "Mean pH"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(paste0(output_dir, "ph_timeseries.png"),
       fig_ts, width = 10, height = 6, dpi = 300, bg = "white")

# Factsheet values for future outlook if we need the values

ph_ssp <- ph |> filter(scenario != "Baseline")

factsheet <- tribble(
  ~placeholder,               ~value,                                                               ~notes,

  "OA_BASELINE_PH",
  as.character(round(baseline_ph, 3)),
  "Mean pH across MIMP polygon, Bio-Oracle baseline 2000-2018",

  "OA_PH_SSP126_2050",
  as.character(ph_ssp |> filter(scenario == "SSP1-2.6", year == 2050) |> pull(ph_mean)),
  "Bio-Oracle v3.0 SSP1-2.6",

  "OA_PH_SSP585_2050",
  as.character(ph_ssp |> filter(scenario == "SSP5-8.5", year == 2050) |> pull(ph_mean)),
  "Bio-Oracle v3.0 SSP5-8.5",

  "OA_PH_SSP126_2090",
  as.character(ph_ssp |> filter(scenario == "SSP1-2.6", year == 2090) |> pull(ph_mean)),
  "Bio-Oracle v3.0 SSP1-2.6",

  "OA_PH_SSP585_2090",
  as.character(ph_ssp |> filter(scenario == "SSP5-8.5", year == 2090) |> pull(ph_mean)),
  "Bio-Oracle v3.0 SSP5-8.5",

  "OA_DELTA_PH_SSP126_2090",
  as.character(ph_ssp |> filter(scenario == "SSP1-2.6", year == 2090) |> pull(ph_change)),
  "Change from baseline",

  "OA_DELTA_PH_SSP585_2090",
  as.character(ph_ssp |> filter(scenario == "SSP5-8.5", year == 2090) |> pull(ph_change)),
  "Change from baseline"
)

write_csv(factsheet, paste0(output_dir, "ph_factsheet_values.csv"))
print(factsheet)
