# Observed SLR trend from C3S satellite altimetry (1993–2023)
# DATA:Copernicus C3S — Sea level gridded data from satellite observations
# https://cds.climate.copernicus.eu/datasets/satellite-sea-level-global
# Variable : sla (sea level anomaly, metres)
# Reference: 1993–2012 mean
# Resolution: 0.25° x 0.25°, monthly, 1993–2023

library(terra)
library(sf)
library(tidyverse)
library(lubridate)

# File paths 
nc_file    <- "data/slr/sla.nc"
shapefile  <- "data/mafia/mafia_mpa.gpkg"
shapefile <- vect(shapefile)
output_dir <- "output/"

# IPCC AR6 global mean rate 1993–2018 for comparison
ipcc_global_rate <- 3.3   # mm/year

# Load datasets
r <- rast(nc_file)
if (is.na(crs(r))) crs(r) <- "EPSG:4326"

# Find the sea level anomaly variable
sla_names <- names(r)[str_detect(names(r), regex("sla|sea_level|anomaly",
                                                   ignore_case = TRUE))]
if (length(sla_names) == 0) {
  message("Variable names found: ", paste(names(r), collapse = ", "))
  stop("Could not identify SLA variable. Update the variable name manually.")
}

# If multiple variables, select SLA only
r_sla <- r[[sla_names]]

message("  SLA layers : ", nlyr(r_sla))
message("  Resolution : ", paste(res(r_sla), collapse = " x "), " degrees")

# Parse time
t      <- time(r_sla)
dates  <- as.Date(t)
message("  Time range : ", as.character(min(dates)),
        " to ", as.character(max(dates)))

# Extract MIMP time series
raw_vals <- terra::extract(r_sla, shapefile, mean)
sla_vals <- as.numeric(raw_vals[1, which(names(raw_vals) != "ID")])

# Convert metres to mm
sla_mm <- sla_vals * 1000

slr_df <- tibble(
  date   = dates,
  year   = year(dates),
  month  = month(dates),
  sla_mm = sla_mm
) |>
  filter(!is.na(sla_mm)) |>
  arrange(date)

message("  Extracted ", nrow(slr_df), " monthly values")
message("  SLA range : ",
        round(min(slr_df$sla_mm), 1), " to ",
        round(max(slr_df$sla_mm), 1), " mm")

# Annual mean (for cleaner trend fitting)
slr_annual <- slr_df |>
  group_by(year) |>
  summarise(sla_mm = mean(sla_mm, na.rm = TRUE), .groups = "drop") |>
  filter(n() == 1 | TRUE)   # keep all years


# Fit linear trend on monthly data 
# Use decimal year for monthly trend
slr_df <- slr_df |>
  mutate(decimal_year = year + (month - 0.5) / 12)

lm_fit  <- lm(sla_mm ~ decimal_year, data = slr_df)
lm_sum  <- summary(lm_fit)

trend_mm_yr   <- round(coef(lm_fit)["decimal_year"], 2)
trend_se      <- round(coef(lm_sum)["decimal_year", "Std. Error"], 2)
trend_ci_low  <- round(trend_mm_yr - 1.96 * trend_se, 2)
trend_ci_high <- round(trend_mm_yr + 1.96 * trend_se, 2)
r_squared     <- round(lm_sum$r.squared, 3)
total_years   <- as.numeric(max(dates) - min(dates)) / 365.25
total_rise_mm <- round(trend_mm_yr * total_years, 1)

message("  Trend     : ", trend_mm_yr, " mm/year")
message("  95% CI    : ", trend_ci_low, " to ", trend_ci_high, " mm/year")
message("  R²        : ", r_squared)
message("  Total rise: ", total_rise_mm, " mm over ",
        round(total_years, 1), " years")

slr_df <- slr_df |>
  mutate(predicted = predict(lm_fit))

write.csv(slr_df, paste0(output_dir, "sla_ts_stats.csv"))

# Time series plot
fig_ts <- ggplot() +
  
  # Monthly values — light grey
  geom_line(data = slr_df,
            aes(x = date, y = sla_mm),
            colour = "grey70", linewidth = 0.4, alpha = 0.8) +
  # Annual mean — blue
  geom_line(data = slr_annual,
            aes(x = as.Date(paste0(year, "-07-01")), y = sla_mm),
            colour = "#2C7BB6", linewidth = 1.0) +
  geom_point(data = slr_annual,
             aes(x = as.Date(paste0(year, "-07-01")), y = sla_mm),
             colour = "#2C7BB6", size = 2.0) +
  # Linear trend — red
  geom_line(data = slr_df,
            aes(x = date, y = predicted),
            colour = "#D7191C", linewidth = 1.0, linetype = "solid") +
  # # Trend annotation
  # annotate("text",
  #          x     = as.Date("1994-01-01"),
  #          y     = max(slr_df$sla_mm, na.rm = TRUE) * 0.92,
  #          label = paste0(
  #            "Trend: ", trend_mm_yr, " \u00b1 ", trend_se, " mm/year\n",
  #            "95% CI: ", trend_ci_low, "\u2013", trend_ci_high,
  #            " mm/year\n",
  #            "R\u00b2 = ", r_squared, "\n",
  #            "Total rise: ", total_rise_mm, " mm (",
  #            round(total_years), " years)\n",
  #            "IPCC AR6 global mean: ", ipcc_global_rate, " mm/year"
  #          ),
  #          hjust = 0, vjust = 1,
  #          size = 3.0, colour = "#D7191C",
  #          fontface = "italic") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(
    title    = "MIMP Mean Sea Level Anomaly (1993-2023)",
    subtitle = paste0(
      "Monthly SLA (grey) and annual mean (blue) relative to 1993-2012 baseline"
    ),
    x       = "Year",
    y       = "Sea level anomaly (mm)",
    caption = paste0(
      "Source: Copernicus C3S CDS(2018) sea level gridded dataset",
      "(doi:10.24381/cds.4c328c78)"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.caption     = element_text(colour = "grey50", size = 7.5,
                                    hjust = 0),
    panel.grid.minor = element_blank()
  )

out_fig <- file.path(output_dir, "sla_timeseries.png")
ggsave(out_fig, fig_ts, width = 11, height = 6,
       dpi = 300, bg = "white")


# Factsheet placeholder values
factsheet <- tribble(
  ~placeholder,~value,~unit,~notes,
  "SLR_OBSERVED_RATE",
  as.character(trend_mm_yr),
  "mm/year",
  paste0("Linear trend, C3S satellite altimetry at MIMP 1993-2023"),

  "SLR_OBSERVED_RATE_CI",
  paste0(trend_ci_low, " to ", trend_ci_high),
  "mm/year",
  "95% confidence interval",

  "SLR_TOTAL_RISE",
  as.character(total_rise_mm),
  "mm",
  paste0("Total rise 1993-2023 (", round(total_years), " years)"),

  "SLR_GLOBAL_MEAN",
  as.character(ipcc_global_rate),
  "mm/year",
  "IPCC AR6 global mean 1993-2018",

  "SLR_VS_GLOBAL",
  as.character(round(trend_mm_yr - ipcc_global_rate, 2)),
  "mm/year",
  "Local minus global (positive = faster than global)"
)

out_csv <- file.path(output_dir, "sla_factsheet_values.csv")
write_csv(factsheet, out_csv)

write_csv(slr_df, file.path(output_dir, "slr_mimp_timeseries.csv"))

message("  FACTSHEET VALUES — SLR (1993-2023)")
message("================================================")
message("Observed rate (MIMP) : ", trend_mm_yr, " mm/year")
message("95% CI               : ", trend_ci_low,
        " to ", trend_ci_high, " mm/year")
message("Total rise 1993-2023 : ", total_rise_mm, " mm")
message("IPCC AR6 global mean : ", ipcc_global_rate, " mm/year")
message("Local vs global      : ",
        round(trend_mm_yr - ipcc_global_rate, 2), " mm/year")
message("================================================\n")

