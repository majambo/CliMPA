# This script loads SST data from a NetCDF file
# Plots it over a larger region based on the MPA boundary and adds the MPA boundary.

# Load libraries
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(cowplot)

# Load the data
tryCatch({
  
  climate_data <- rast("data/bioracle/ssp585/thetao_ssp585_2020_2100_depthmax_thetao_mean.nc")
  mpa_boundary <- st_read("data/mafia/mafia_mpa.gpkg")
  
  message("Successfully loaded the climate data and MPA boundary.")
  
}, error = function(e) {
  message("Files not found")
  
})

# Create a larger rectangular plotting area based on the MPA boundary
# Get the bounding box of the MPA
mpa_bbox <- st_bbox(mpa_boundary)

# Expand the rectangular domain by adding buffer degrees to all sides
buffer_degrees <- .1  # Adjust this value as needed (e.g., 2, 5, 10 degrees)

expanded_extent <- ext(
  mpa_bbox[1] - buffer_degrees,  # xmin (longitude)
  mpa_bbox[3] + buffer_degrees,  # xmax (longitude) 
  mpa_bbox[2] - buffer_degrees,  # ymin (latitude)
  mpa_bbox[4] + buffer_degrees   # ymax (latitude)
)

# Print extents for debugging
print("Original MPA extent:")
print(mpa_bbox)
print("Expanded plotting extent:")
print(expanded_extent)

# Crop the climate data to the expanded rectangular area
cropped_data <- crop(climate_data, expanded_extent)

temp_range <- range(values(cropped_data), na.rm = TRUE)

# Plot the results for each time step
plot_list <- list()
for (i in 1:nlyr(cropped_data)) {
  
  # Convert the SpatRaster layer to a data frame for plotting with ggplot2
  layer_df <- as.data.frame(cropped_data[[i]], xy = TRUE)
  
  # Get the time label for the plot title
  time_label <- format(time(cropped_data)[i], "%Y-%m-%d")
  
  # Create the plot
  p <- ggplot(data = layer_df, aes(x = x, y = y, fill = !!sym(names(layer_df)[3]))) +
    geom_raster() +
    scale_fill_distiller(
      name = "Temp (°C)",
      palette = "Spectral",
      type = "div",
      limits = temp_range,
      na.value = "transparent"
    ) +
    # Add the original MPA boundary on top
    geom_sf(data = mpa_boundary, fill = "transparent", color = "black", size = 1, inherit.aes = FALSE) +
    labs(
      title = paste("Time:", time_label),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legend from individual plots
      plot.title = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8)
    )
  
  plot_list[[i]] <- p
}

# Create a separate legend plot
legend_plot <- ggplot(data = as.data.frame(cropped_data[[1]], xy = TRUE), 
                      aes(x = x, y = y, fill = !!sym(names(as.data.frame(cropped_data[[1]], xy = TRUE))[3]))) +
  geom_raster() +
  scale_fill_distiller(
    name = "Temperature (°C)",
    palette = "Spectral",
    type = "div",
    limits = temp_range,
    na.value = "transparent"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.key.width = unit(2, "cm")
  )

# Extract just the legend from the legend plot
legend <- cowplot::get_legend(legend_plot)

# Combine all the plots into a single figure with one shared legend
combined_plot <- patchwork::wrap_plots(plot_list, nrow = 2) +
  plot_annotation(
    title = 'Ocean Temperature Around MPA',
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    )
  )

# Add the single legend at the bottom
final_plot <- combined_plot / legend + plot_layout(heights = c(15, 2))
print(final_plot)

# Save the plots as png and pdf
ggsave("output/ocean_temperature_mpa_ssp585.png", plot = final_plot,
       width = 12, height = 10, dpi = 300, units = "in")

ggsave("output/ocean_temperature_mpa_ssp585.pdf", plot = final_plot,
       width = 12, height = 10, units = "in")

message("\nPlotting complete.")

