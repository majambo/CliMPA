# Bivariate SST & pH Map
library(terra)
library(sf)
library(tidyverse)
library(biscale)
library(ggplot2)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)

# File paths 
path <- "data/bioracle/"

sst_baseline_file <- "data/bioracle/thetao_mean_baseline.nc"
sst_ssp245_file   <- paste0(path, "ssp245/thetao_ssp245_2020_2100_depthmax_thetao_mean.nc")
sst_ssp585_file   <- paste0(path, "ssp585/thetao_ssp585_2020_2100_depthmax_thetao_mean.nc")

ph_baseline_file  <- paste0(path, "ph_mean_baseline.nc")
ph_ssp245_file    <- paste0(path, "ssp245/ph_ssp245_2020_2100_depthmax_mean.nc")
ph_ssp585_file    <- paste0(path, "ssp585/ph_ssp585_2020_2100_depthmax_mean.nc")

wio_mpa_shp  <- "data/wio/wio_mpas.shp"
mafia_shp    <- "data/mafia/mafia_mpa.gpkg"
output_dir   <- "output/bivariates/"

palette     <- "BlueOr"
dim_classes <- 4

# Load data
# MPA vectors
mpa     <- vect(wio_mpa_shp)
mafia_v <- mpa[mpa$MPA_NAME == "Mafia Island"]

# Mafia as sf for ggplot
mafia_sf <- st_read(mafia_shp, quiet = TRUE) |> st_make_valid()

# Define AOI: MIMP bbox + buffer
buffer_deg <- 2.1
mafia_bbox <- st_bbox(mafia_sf)
aoi <- ext(
  mafia_bbox["xmin"] - buffer_deg,
  mafia_bbox["xmax"] + buffer_deg,
  mafia_bbox["ymin"] - buffer_deg,
  mafia_bbox["ymax"] + buffer_deg
)
message("  MIMP AOI: ", paste(round(as.vector(aoi), 3), collapse = ", "))

# Country boundaries from rnaturalearth — cropped to AOI
coast_ext <- c(
  xmin = aoi$xmin,
  xmax = aoi$xmax,
  ymin = aoi$ymin,
  ymax = aoi$ymax
)
countries <- ne_countries(scale = "medium", returnclass = "sf") |>
  st_make_valid() |>
  suppressWarnings(st_crop(coast_ext))

message("  Country boundaries loaded via rnaturalearth")

# SST rasters
sst_base <- rast(sst_baseline_file)
crs(sst_base) <- "EPSG:4326"
sst_245  <- rast(sst_ssp245_file)
sst_585  <- rast(sst_ssp585_file)

# pH rasters
ph_base  <- rast(ph_baseline_file)
crs(ph_base) <- "EPSG:4326"
ph_245   <- rast(ph_ssp245_file)
ph_585   <- rast(ph_ssp585_file)

# Helper functions
get_decade_layer <- function(r, target_year) {
  decades     <- seq(2020, 2090, by = 10)
  layer_index <- which(decades == target_year)
  if (length(layer_index) == 0) stop("Year ", target_year, " not found.")
  r[[layer_index]]
}

prepare_panel_data <- function(sst_layer, ph_layer) {
  sst_wio <- crop(sst_layer, aoi)
  ph_wio  <- crop(ph_layer,  aoi)
  comb        <- c(sst_wio, ph_wio)
  names(comb) <- c("sst", "ph")
  comb_df <- as.data.frame(comb, xy = TRUE)
  bi_class(comb_df, x = sst, y = ph,
           style = "quantile", dim = dim_classes)
}

# Prepare all panel datasets
sst_base_mean <- mean(sst_base, na.rm = TRUE)
ph_base_mean  <- mean(ph_base,  na.rm = TRUE)

message("  Baseline...")
data_baseline <- prepare_panel_data(sst_base_mean, ph_base_mean)

message("  SSP2-4.5 2050...")
data_245_2050 <- prepare_panel_data(get_decade_layer(sst_245, 2050),
                                     get_decade_layer(ph_245,  2050))
message("  SSP2-4.5 2090...")
data_245_2090 <- prepare_panel_data(get_decade_layer(sst_245, 2090),
                                     get_decade_layer(ph_245,  2090))
message("  SSP5-8.5 2050...")
data_585_2050 <- prepare_panel_data(get_decade_layer(sst_585, 2050),
                                     get_decade_layer(ph_585,  2050))
message("  SSP5-8.5 2090...")
data_585_2090 <- prepare_panel_data(get_decade_layer(sst_585, 2090),
                                     get_decade_layer(ph_585,  2090))

# 4. Plot function
make_bivariate_panel <- function(data, panel_title) {

  ggplot() +
    theme_void(base_size = 12) +
    # Bivariate raster
    geom_tile(data    = data,
              mapping = aes(x = x, y = y, fill = bi_class),
              show.legend = FALSE) +
    bi_scale_fill(pal        = palette,
                  dim        = dim_classes,
                  flip_axes  = FALSE,
                  rotate_pal = FALSE) +
    # Country boundaries from rnaturalearth
    geom_sf(data        = countries,
            fill        = "#f5f5f5",
            colour      = "grey50",
            linewidth   = 0.2,
            inherit.aes = FALSE) +
    # Mafia Island MIMP boundary
    geom_sf(data        = mafia_sf,
            fill        = NA,
            colour      = "black",
            linewidth   = 0.7,
            inherit.aes = FALSE) +
    # graticule lines — guaranteed to show regardless of coord system
    geom_hline(
      yintercept = seq(floor(aoi$ymin), ceiling(aoi$ymax), by = 1),
      colour = "grey75", linewidth = 0.1, linetype = "dashed"
    ) +
    geom_vline(
      xintercept = seq(floor(aoi$xmin), ceiling(aoi$xmax), by = 1),
      colour = "grey75", linewidth = 0.1, linetype = "dashed"
    ) +
    coord_sf(
      xlim   = c(aoi$xmin, aoi$xmax),
      ylim   = c(aoi$ymin, aoi$ymax),
      expand = FALSE
    ) +
    scale_x_continuous(
      breaks = seq(floor(aoi$xmin), ceiling(aoi$xmax), by = 1),
      labels = function(x) paste0(abs(x), ifelse(x >= 0, "\u00b0E", "\u00b0W"))
    ) +
    scale_y_continuous(
      breaks = seq(floor(aoi$ymin), ceiling(aoi$ymax), by = 1),
      labels = function(y) paste0(abs(y), ifelse(y >= 0, "\u00b0N", "\u00b0S"))
    ) +
    labs(title = panel_title, x = NULL, y = NULL) +
    theme(
      plot.title       = element_text(hjust = 0.5, face = "bold", size = 11),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(colour = "grey80", linewidth = 0.3,
                                      linetype = "dashed"),
      axis.text.x      = element_text(size = 7, colour = "grey40"),
      axis.text.y      = element_text(size = 7, colour = "grey40"),
      axis.ticks       = element_line(colour = "grey60", linewidth = 0.3)
    )
}

# Shared legend
bivariate_legend <- bi_legend(
  pal        = palette,
  flip_axes  = FALSE,
  rotate_pal = FALSE,
  dim        = dim_classes,
  xlab       = "Higher SST",
  ylab       = "Higher pH",
  size       = 10
)

# 5. Individual plots
message("\nSaving individual plots...")

make_final_plot <- function(panel_data, panel_title, out_file) {

  panel <- make_bivariate_panel(panel_data, panel_title)

  # Place legend to the right of the map using plot_grid
  final <- plot_grid(
    panel,
    bivariate_legend,
    ncol       = 2,
    rel_widths = c(0.80, 0.20)
  )

  ggsave(out_file, final, width = 12, height = 8,
         dpi = 300, bg = "white")
  message("  Saved: ", basename(out_file))
  final
}

plot_baseline <- make_final_plot(
  data_baseline,
  "SST \u00d7 pH \u2014 Baseline (2000-2018)",
  file.path(output_dir, "bivariate_baseline.png")
)
plot_245_2050 <- make_final_plot(
  data_245_2050,
  "SST \u00d7 pH \u2014 SSP2-4.5 (2050)",
  file.path(output_dir, "bivariate_ssp245_2050.png")
)
plot_245_2090 <- make_final_plot(
  data_245_2090,
  "SST \u00d7 pH \u2014 SSP2-4.5 (2090)",
  file.path(output_dir, "bivariate_ssp245_2090.png")
)
plot_585_2050 <- make_final_plot(
  data_585_2050,
  "SST \u00d7 pH \u2014 SSP5-8.5 (2050)",
  file.path(output_dir, "bivariate_ssp585_2050.png")
)
plot_585_2090 <- make_final_plot(
  data_585_2090,
  "SST \u00d7 pH \u2014 SSP5-8.5 (2090)",
  file.path(output_dir, "bivariate_ssp585_2090.png")
)

# 3x3 panel with legend
# Blue   = low SST + high pH: low stress / refugia
# Orange = high SST + low pH; double jeopardy (bleaching + acidification)

message("Building combined figures...")

panel_baseline_p <- make_bivariate_panel(data_baseline,
                      "Baseline (2000-2018)")
panel_245_2050_p <- make_bivariate_panel(data_245_2050, "SSP2-4.5 (2050)")
panel_245_2090_p <- make_bivariate_panel(data_245_2090, "SSP2-4.5 (2090)")
panel_585_2050_p <- make_bivariate_panel(data_585_2050, "SSP5-8.5 (2050)")
panel_585_2090_p <- make_bivariate_panel(data_585_2090, "SSP5-8.5 (2090)")

make_three_panel_figure <- function(panel_2050, panel_2090,
                                     scenario_label, out_file) {

  # 4-panel row: Baseline | 2050 | 2090 | Legend
  four_panels <- plot_grid(
    panel_baseline_p, panel_2050, panel_2090, bivariate_legend,
    nrow       = 1,
    ncol       = 4,
    # labels     = c("a", "b", "c", ""),
    label_size = 9,
    rel_widths = c(1, 1, 1, 0.4)   # legend column narrower than map panels
  )

  title_grob <- ggdraw() +
    draw_label(
      paste0("Mafia Island Marine Park SST \u00d7 pH Bivariate Map ",
             scenario_label),
      fontface = "bold", size = 13, hjust = 0.5
    )

  subtitle_grob <- ggdraw() +
    draw_label(
      "Orange = high SST + low pH | Blue = low SST + high pH",
      size = 9, hjust = 0.5, colour = "grey40"
    )

  caption_grob <- ggdraw() +
    draw_label(
      "Source: Bio-Oracle v3.0" ,
      size = 8, hjust = 0, colour = "grey50"
    )

  # Stack: title / subtitle / panels+legend / caption
  final <- plot_grid(
    title_grob,
    subtitle_grob,
    four_panels,
    caption_grob,
    nrow        = 4,
    rel_heights = c(0.05, 0.04, 0.87, 0.04)
  )

  ggsave(out_file, final, width = 18, height = 7,
         dpi = 300, bg = "white")
  message("  Saved: ", basename(out_file))
  final
}

fig_ssp245 <- make_three_panel_figure(
  panel_2050     = panel_245_2050_p,
  panel_2090     = panel_245_2090_p,
  scenario_label = "SSP2-4.5",
  out_file       = file.path(output_dir, "bivariate_ssp245.png")
)

fig_ssp585 <- make_three_panel_figure(
  panel_2050     = panel_585_2050_p,
  panel_2090     = panel_585_2090_p,
  scenario_label = "SSP5-8.5",
  out_file       = file.path(output_dir, "bivariate_ssp585.png")
)

# Class distribution
panels <- list(
  "Baseline"      = data_baseline,
  "SSP2-4.5 2050" = data_245_2050,
  "SSP2-4.5 2090" = data_245_2090,
  "SSP5-8.5 2050" = data_585_2050,
  "SSP5-8.5 2090" = data_585_2090
)

for (label in names(panels)) {
  cat(label, "\n")
  print(panels[[label]] |> count(bi_class) |> arrange(bi_class))
  cat("\n")
}

message("All done.")
