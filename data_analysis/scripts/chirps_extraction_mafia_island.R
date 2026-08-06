# Extract CHIRPS v2.0 daily precipitation for Mafia Island
# 1981:2025, Units: (mm/day), Resolution: 0.05° 

library(terra)
library(sf)
library(tidyverse)
library(lubridate)
library(patchwork)

# File paths 
chirps_dir  <- "chirps/netcdf/"
island_gpkg <- "data/mafia_island.gpkg"
output_dir  <- "output/"

# Thresholds
heavy_mm     <- 50    
dry_mm       <- 1     
long_Rains   <- 3:5   
short_Rains  <- 10:12 

# Load mafia island boundary
island_sf <- st_read(island_gpkg, quiet = TRUE) |> st_make_valid()
island_v  <- vect(island_sf)

# List all yearly files
nc_files <- list.files(chirps_dir,
                       pattern = "chirps_prec_\\d{4}\\.nc$",
                       full.names = TRUE) |> sort()

years <- as.integer(str_extract(basename(nc_files), "\\d{4}"))
message("Found ", length(nc_files), " yearly files: ",
        min(years), "\u2013", max(years))

# 3. Extract functionone year at a time
# annual Rainfall total (mm)
# Long Rains total (Mar-May)
# Short Rains total (Oct-Dec)
# heavy Rainfall days per year (days > 50mm/day)
# Consecutive dry days — longest dry spell (days < 1mm)
# seasonality index
# Monthly totals (for climatology plot)

extract_year <- function(nc_file, yr) {
  
  message("  Processing ", yr, "...")
  
  r <- rast(nc_file)
  if (is.na(crs(r))) crs(r) <- "EPSG:4326"
  
  # Crop and mask to island polygon
  r_crop <- crop(r, island_v)
  r_mask <- mask(r_crop, island_v)
  
  # Get dates from time metadata
  dates <- as.Date(time(r_mask))
  
  # Spatial mean per day across island pixels
  daily_vals <- global(r_mask, "mean", na.rm = TRUE)[[1]]
  
  daily_df <- tibble(
    date  = dates,
    year  = year(dates),
    month = month(dates),
    doy   = yday(dates),
    prec  = daily_vals
  )
  
  # annual indices 
  annual <- daily_df |>
    summarise(
      year             = yr,
      # Totals
      annual_mm        = round(sum(prec, na.rm = TRUE), 1),
      long_rains_mm    = round(sum(prec[month %in% long_Rains],  na.rm = TRUE), 1),
      short_rains_mm   = round(sum(prec[month %in% short_Rains], na.rm = TRUE), 1),
      dry_season_mm    = round(sum(prec[!month %in% c(long_Rains, short_Rains)],
                                   na.rm = TRUE), 1),
      # heavy days
      heavy_days       = sum(prec >= heavy_mm, na.rm = TRUE),
      # Wet days
      wet_days         = sum(prec >= dry_mm,   na.rm = TRUE),
      # Dry days
      dry_days         = sum(prec < dry_mm,    na.rm = TRUE),
      # Longest consecutive dry spell
      max_dry_spell    = {
        is_dry <- prec < dry_mm
        rle_dry <- rle(is_dry)
        max(rle_dry$lengths[rle_dry$values], na.rm = TRUE)
      },
      # Peak daily Rainfall
      peak_day_mm      = round(max(prec, na.rm = TRUE), 1),
      peak_day_date    = as.character(date[which.max(prec)]),
      # seasonality index
      seasonality_idx  = round((long_rains_mm + short_rains_mm) / annual_mm, 3)
    )
  
  # Monthly totals
  monthly <- daily_df |>
    group_by(year, month) |>
    summarise(
      ptot_mm   = round(sum(prec, na.rm = TRUE), 1),
      heavy_days = sum(prec >= heavy_mm, na.rm = TRUE),
      wet_days   = sum(prec >= dry_mm,   na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(date = as.Date(paste(year, month, "15", sep = "-")))
  
  list(annual = annual, monthly = monthly)
}

# Extraction across all the years

message("\nExtracting daily indices per year...")

results <- map2(nc_files, years, function(f, yr) {
  tryCatch(
    extract_year(f, yr),
    error = function(e) {
      message("  ERROR in ", yr, ": ", conditionMessage(e))
      NULL
    }
  )
})

# Remove failed years
results <- results[!sapply(results, is.null)]
annual_df  <- map_dfr(results, "annual")
monthly_df <- map_dfr(results, "monthly")

message("\nExtraction complete.")
message("  Years processed: ", nrow(annual_df))
message("  Monthly records: ", nrow(monthly_df))

# Factsheet stats
baseline <- annual_df |> filter(year <= 2010)
recent   <- annual_df |> filter(year >= 2011 & year <= 2025)

# Linear trend on annual totals
lm_annual <- lm(annual_mm ~ year, data = annual_df)
trend_annual <- round(coef(lm_annual)["year"], 2)

# trend on heavy days
lm_heavy <- lm(heavy_days ~ year, data = annual_df)
trend_heavy <- round(coef(lm_heavy)["year"], 2)

stats <- tribble(
  ~placeholder, ~value, ~unit, ~notes,
  
  "Rain_annual_mean",
  as.character(round(mean(annual_df$annual_mm), 0)),
  "mm", "CHIRPS mean annual Rainfall 1981-2025",
  
  "Rain_annual_sd",
  as.character(round(sd(annual_df$annual_mm), 0)),
  "mm", "Standard deviation of annual Rainfall",
  
  "Rain_annual_min",
  as.character(round(min(annual_df$annual_mm), 0)),
  "mm", paste("Driest year:", annual_df$year[which.min(annual_df$annual_mm)]),
  
  "Rain_annual_max",
  as.character(round(max(annual_df$annual_mm), 0)),
  "mm", paste("Wettest year:", annual_df$year[which.max(annual_df$annual_mm)]),
  
  "Rain_baseline_mean",
  as.character(round(mean(baseline$annual_mm), 0)),
  "mm", "WMO baseline 1981-2010",
  
  "Rain_recent_mean",
  as.character(round(mean(recent$annual_mm), 0)),
  "mm", "recent period 2011-2022",
  
  "Rain_trend",
  as.character(trend_annual),
  "mm/year", "Linear trend 1981-2025",
  
  "Rain_long_rains_mean",
  as.character(round(mean(annual_df$long_rains_mm), 0)),
  "mm", "mean Mar-May total 1981-2025",
  
  "Rain_short_rains_mean",
  as.character(round(mean(annual_df$short_rains_mm), 0)),
  "mm", "mean Oct-Dec total 1981-2025",
  
  "Rain_seasonality_idx",
  as.character(round(mean(annual_df$seasonality_idx), 3)),
  "0-1", "Proportion of annual Rainfall in wet seasons",
  
  "Rain_heavy_days_mean",
  as.character(round(mean(annual_df$heavy_days), 1)),
  "days/year", "mean days >= 50mm/day 1981-2025",
  
  "Rain_heavy_days_trend",
  as.character(trend_heavy),
  "days/year", "Linear trend in heavy days 1981-2025",
  
  "Rain_max_dry_spell_mean",
  as.character(round(mean(annual_df$max_dry_spell), 0)),
  "days", "mean longest annual dry spell (days < 1mm)"
)

# Save results
write_csv(annual_df,  file.path(output_dir, "rainfall_mafia_annual.csv"))
write_csv(monthly_df, file.path(output_dir, "rainfall_mafia_monthly.csv"))
write_csv(stats,      file.path(output_dir, "rainfall_mafia_stats.csv"))

# # Summary
# message("mean annual Rainfall : ", round(mean(annual_df$annual_mm), 0), " mm")
# message("trend                : ", trend_annual, " mm/year")
# message("Long Rains mean      : ", round(mean(annual_df$long_Rains_mm), 0), " mm")
# message("Short Rains mean     : ", round(mean(annual_df$short_Rains_mm), 0), " mm")
# message("heavy days (>=50mm)  : ", round(mean(annual_df$heavy_days), 1), " days/year")
# message("heavy days trend     : ", trend_heavy, " days/year")
# message("max dry spell mean   : ", round(mean(annual_df$max_dry_spell), 0), " days")
# message("baseline mean        : ", round(mean(baseline$annual_mm), 0), " mm (1981-2010)")
# message("recent mean          : ", round(mean(recent$annual_mm), 0), " mm (2011-2022)")

# Visualization
col_annual      <- "#2C7BB6"
col_long_rains  <- "#1A9641"
col_short_rains <- "#F5A623"
col_trend       <- "#D7191C"

mimp_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

# Load data 
monthly <- read_csv(file.path(output_dir, "rainfall_mafia_monthly.csv"),
                    show_col_types = FALSE)
annual  <- read_csv(file.path(output_dir, "rainfall_mafia_annual.csv"),
                    show_col_types = FALSE)


# Annual time series
lm_trend  <- lm(annual_mm ~ year, data = annual)
trend_val <- round(coef(lm_trend)["year"], 1)
trend_dir <- ifelse(trend_val > 0, "increasing", "decreasing")

fig_annual <- ggplot(annual, aes(x = year, y = annual_mm)) +
  
  # WMO baseline reference band (1981–2010 mean)
  # geom_hline(
  #   yintercept = mean(annual$annual_mm[annual$year <= 2010]),
  #   linetype = "dashed", colour = "grey50", linewidth = 0.6
  # ) +
  # annotate("text",
  #          x = 1982, y = mean(annual$annual_mm[annual$year <= 2010]) + 15,
  #          label = "1981\u20132010 mean",
  #          colour = "grey50", size = 3, hjust = 0) +
  
  # Annual bars
  geom_col(fill = col_annual, alpha = 0.75, width = 0.8) +
  # Trend line
  geom_smooth(method = "lm", se = TRUE,
              colour = col_trend, fill = col_trend,
              alpha = 0.15, linewidth = 1.0) +
  # Trend annotation
  annotate("text",
           x = 2015, y = max(annual$annual_mm) * 0.95,
           label = paste0("Trend: ", trend_val, " mm/year (",
                          trend_dir, ")"),
           colour = col_trend, size = 3, hjust = 0, fontface = "italic") +
  scale_x_continuous(breaks = seq(1985, 2020, by = 5)) +
  labs(
    title    = "Annual Rainfall \u2014 Mafia Island (1981\u20132022)",
    subtitle = "CHIRPS v2.0 | Spatial mean over island polygon",
    x = "Year", y = "Annual rainfall (mm)",
    caption  = "Source: CHIRPS v2.0"
  ) + mimp_theme

ggsave(file.path(output_dir, "rain_annual_mafia.png"),
       fig_annual, width = 10, height = 6, dpi = 300, bg = "white")

# Monthly climatology, mean seasonal cycle
climatology <- monthly |>
  group_by(month) |>
  summarise(
    mean_mm = round(mean(ptot_mm, na.rm = TRUE), 1),
    sd_mm   = round(sd(ptot_mm,   na.rm = TRUE), 1),
    .groups = "drop"
  ) |>
  mutate(
    month_label = factor(month.abb[month], levels = month.abb),
    season = case_when(
      month %in% 3:5   ~ "Long rains (Mar\u2013May)",
      month %in% 10:12 ~ "Short rains (Oct\u2013Dec)",
      TRUE             ~ "Dry season"
    ),
    season = factor(season, levels = c("Long rains (Mar\u2013May)",
                                       "Short rains (Oct\u2013Dec)",
                                       "Dry season"))
  )

fig_clim <- ggplot(climatology,
                   aes(x = month_label, y = mean_mm, fill = season)) +
  geom_col(alpha = 0.85, width = 0.75) +
  geom_errorbar(aes(ymin = mean_mm - sd_mm,
                    ymax = mean_mm + sd_mm),
                width = 0.3, colour = "grey40", linewidth = 0.5) +
  scale_fill_manual(
    values = c(
      "Long rains (Mar\u2013May)"   = col_long_rains,
      "Short rains (Oct\u2013Dec)"  = col_short_rains,
      "Dry season"                  = "grey70"
    ),
    name = NULL
  ) +
  labs(
    title    = "Mean Monthly Rainfall \u2014 Mafia Island (1981-2022)",
    subtitle = "Error bars = \u00b11 standard deviation",
    x = NULL, y = "Mean monthly rainfall (mm)",
    caption  = "Source: CHIRPS v2.0"
  ) + mimp_theme

ggsave(file.path(output_dir, "mafia_rain_climatology.png"),
       fig_clim, width = 10, height = 6, dpi = 300, bg = "white")

# Long vs. short ts
seasons_long <- annual |>
  select(year, long_rains_mm, short_rains_mm) |>
  pivot_longer(cols = c(long_rains_mm, short_rains_mm),
               names_to  = "season",
               values_to = "rainfall_mm") |>
  mutate(season = recode(season,
                         "long_rains_mm"  = "Long rains (Mar\u2013May)",
                         "short_rains_mm" = "Short rains (Oct\u2013Dec)"
  ))

fig_seasons <- ggplot(seasons_long,
                      aes(x = year, y = rainfall_mm, colour = season)) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  geom_point(size = 1.8, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE,
              linetype = "dashed", linewidth = 0.7) +
  scale_colour_manual(
    values = c(
      "Long rains (Mar\u2013May)"  = col_long_rains,
      "Short rains (Oct\u2013Dec)" = col_short_rains
    ),
    name = NULL
  ) +
  scale_x_continuous(breaks = seq(1985, 2020, by = 5)) +
  labs(
    title    = "Long and Short Rains: Mafia Island (1981-2022)",
    # subtitle = "Dashed lines = linear trends per season",
    x = "Year", y = "Seasonal rainfall total (mm)",
    caption  = "Source: CHIRPS v2.0"
  ) + mimp_theme

ggsave(file.path(output_dir, "mafia_rain_seasons.png"),
       fig_seasons, width = 10, height = 6, dpi = 300, bg = "white")

# Combined figure
fig_combined <- (fig_annual + fig_clim) /
  (fig_seasons + plot_spacer()) +
  plot_annotation(
    title    = "Rainfall at Mafia Island (1981-2022)",
    # subtitle = "CHIRPS v2.0 | Spatial mean over island polygon",
    caption  = "Source: CHIRPS v2.0",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 9,  hjust = 0.5, colour = "grey40"),
      plot.caption  = element_text(size = 8,  hjust = 0,   colour = "grey50")
    )
  )

ggsave(file.path(output_dir, "mafia_rain_combined.png"),
       fig_combined, width = 14, height = 10, dpi = 300, bg = "white")


# Season trends
for (s in c("long_rains_mm", "short_rains_mm", "annual_mm")) {
  lm_s <- lm(reformulate("year", s), data = annual)
  cat(s, ": ", round(coef(lm_s)["year"], 2), " mm/year\n")
}
