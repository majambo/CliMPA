# Ocean Acidification 
# Generates maps of: Observed pH baseline (2000–2018 mean) and SSP5-8.5 projected pH in decadal steps (2020–2090)

library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(cowplot)

# File paths 
path <- "data/bioracle/"
ph_baseline <- paste0(path, "ph_mean_baseline.nc")
ph_ssp585   <- paste0(path, "ssp585/ph_ssp585_2020_2100_depthmax_mean.nc")
shapefile   <- "data/mafia/mafia_mpa.gpkg"

output_png  <- "output/ph_spatial_map.png"
output_pdf  <- "output/ph_spatial_map.pdf"

# Load data 
message("Loading data...")

ph_baseline <- rast(ph_baseline)
ph_ssp585   <- rast(ph_ssp585)
mpa_boundary <- st_read(shapefile, quiet = TRUE)
mpa_boundary <- st_make_valid(mpa_boundary)

if (is.na(crs(ph_baseline))) crs(ph_baseline) <- "EPSG:4326"
if (is.na(crs(ph_ssp585)))   crs(ph_ssp585)   <- "EPSG:4326"

message("  Baseline layers : ", nlyr(ph_baseline))
message("  SSP5-8.5 layers : ", nlyr(ph_ssp585))

# Collapse baseline to a single mean layer 
ph_baseline_mean <- mean(ph_baseline, na.rm = TRUE)
names(ph_baseline_mean) <- "ph_mean"

# Define plotting extent (MPA bbox + buffer) 
mpa_bbox <- st_bbox(mpa_boundary)
buffer_degrees <- .1  # degrees around MIMP — adjust if needed

expanded_extent <- ext(
  mpa_bbox["xmin"] - buffer_degrees,
  mpa_bbox["xmax"] + buffer_degrees,
  mpa_bbox["ymin"] - buffer_degrees,
  mpa_bbox["ymax"] + buffer_degrees
)

message("Plotting extent: ",
        paste(round(as.vector(expanded_extent), 3), collapse = ", "))

# Crop to plotting extent 
baseline_crop <- crop(ph_baseline_mean, expanded_extent)
ssp585_crop   <- crop(ph_ssp585,        expanded_extent)

# Shared colour scale across all panels 
all_vals   <- c(values(baseline_crop), values(ssp585_crop))
ph_range   <- range(all_vals, na.rm = TRUE)
ph_midpoint <- mean(ph_range)

message("pH range across all panels: ",
        round(ph_range[1], 3), " – ", round(ph_range[2], 3))

# Plot function (same structure as your SST script) 
make_ph_panel <- function(raster_layer, panel_title) {

  layer_df <- as.data.frame(raster_layer, xy = TRUE)
  fill_col <- names(layer_df)[3]

  ggplot(data = layer_df, aes(x = x, y = y, fill = !!sym(fill_col))) +
    geom_raster() +
    scale_fill_gradient2(
      name     = "pH",
      low      = "#D7191C",    # red = low pH (more acidic)
      mid      = "#FFFFBF",    # yellow = mid
      high     = "#2C7BB6",    # blue = high pH
      midpoint = ph_midpoint,
      limits   = ph_range,
      na.value = "transparent"
    ) +
    geom_sf(data       = mpa_boundary,
            fill       = "transparent",
            color      = "black",
            linewidth  = 0.8,
            inherit.aes = FALSE) +
    labs(title = panel_title, x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(
      legend.position  = "none",
      plot.title       = element_text(size = 11, face = "bold"),
      axis.title       = element_text(size = 9),
      axis.text        = element_text(size = 7)
    )
}

# Build panel list 
message("Building panels...")
plot_list <- list()

# Panel 1: Baseline mean
plot_list[[1]] <- make_ph_panel(baseline_crop, "Baseline (2000-2018 mean)")

# Panels 2–9: SSP5-8.5 decadal steps
for (i in 1:nlyr(ssp585_crop)) {
  t     <- time(ssp585_crop)[i]
  label <- if (!is.null(t) && !is.na(t)) {
    format(as.Date(t), "%Y")
  } else {
    paste0("Step ", i)
  }
  plot_list[[i + 1]] <- make_ph_panel(ssp585_crop[[i]],
                                       paste0("SSP5-8.5 (", label, ")"))
}

message("  Total panels: ", length(plot_list))

# Legend 
legend_data <- as.data.frame(baseline_crop, xy = TRUE)
fill_col    <- names(legend_data)[3]

legend_plot <- ggplot(legend_data,
                      aes(x = x, y = y, fill = !!sym(fill_col))) +
  geom_raster() +
  scale_fill_gradient2(
    name     = "pH",
    low      = "#D7191C",
    mid      = "#FFFFBF",
    high     = "#2C7BB6",
    midpoint = ph_midpoint,
    limits   = ph_range,
    na.value = "transparent"
  ) +
  theme_void() +
  theme(
    legend.position   = "bottom",
    legend.title      = element_text(size = 12, face = "bold"),
    legend.text       = element_text(size = 10),
    legend.key.width  = unit(2, "cm")
  )

legend <- cowplot::get_legend(legend_plot)

# Combine panels 
combined_plot <- patchwork::wrap_plots(plot_list, nrow = 3) +
  plot_annotation(
    title    = "Bio-Oracle v3.0 pH \u2014 Mafia Island Marine Park & WIO",
    subtitle = paste0(
      "Observed baseline (2000-2018) and SSP5-8.5 projections (2020-2090) | ",
      "Blue = higher pH; Red = lower pH | Black outline = MIMP boundary"
    ),
    caption  = "Source: Bio-Oracle v3.0",
    theme    = theme(
      plot.title    = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey40"),
      plot.caption  = element_text(size = 8,  hjust = 0,   colour = "grey50")
    )
  )

final_plot <- combined_plot / legend +
  plot_layout(heights = c(15, 1))

# Save 
message("Saving figures...")

ggsave(output_png, plot = final_plot,
       width = 14, height = 12, dpi = 300, units = "in", bg = "white")

ggsave(output_pdf, plot = final_plot,
       width = 14, height = 12, units = "in")

message("Saved: ", output_png)
message("Saved: ", output_pdf)

print(final_plot)
