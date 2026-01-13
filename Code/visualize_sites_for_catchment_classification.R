#--------------------------------
## Create Interactive Map for Manual Catchment Classification
## Visualize sites with topography/elevation data
#--------------------------------

library(tidyverse)
library(sf)
library(leaflet)

# Install elevatr if needed
if (!require(elevatr, quietly = TRUE)) {
  install.packages("elevatr", repos = "https://cloud.r-project.org")
  library(elevatr)
}

# Install raster if needed
if (!require(raster, quietly = TRUE)) {
  install.packages("raster", repos = "https://cloud.r-project.org")
  library(raster)
}

library(RColorBrewer)

#--------------------------------
## Load and prepare site data
#--------------------------------

df <- read.csv("Data/Data_20240408.csv")

# Clean column names (remove BOM character)
names(df)[1] <- "site"

# Extract unique site locations
site_coords <- df %>%
  dplyr::select(site, Latitude, Longitude, Stream, year,
         Burned..B..vs..unburned..U., Wet.or.dry.in.2016) %>%
  # Get unique site locations (some sampled in multiple years)
  group_by(site, Stream) %>%
  summarize(
    Latitude = first(Latitude),
    Longitude = first(Longitude),
    burned = first(Burned..B..vs..unburned..U.),
    n_years = n(),
    years = paste(unique(year), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(!is.na(Latitude), !is.na(Longitude))

cat("Number of unique sites:", nrow(site_coords), "\n")
cat("Latitude range:", range(site_coords$Latitude), "\n")
cat("Longitude range:", range(site_coords$Longitude), "\n")

#--------------------------------
## Create spatial objects
#--------------------------------

# Convert to sf object (WGS84 projection)
sites_sf <- st_as_sf(site_coords,
                     coords = c("Longitude", "Latitude"),
                     crs = 4326)

# Get bounding box with some buffer
bbox <- st_bbox(sites_sf)
bbox_buffered <- bbox + c(-0.05, -0.05, 0.05, 0.05)  # Add ~5km buffer

cat("\nBounding box (buffered):\n")
print(bbox_buffered)

#--------------------------------
## Get elevation data
#--------------------------------

cat("\nDownloading elevation data (this may take a moment)...\n")

# Get elevation raster for the region
# z = zoom level (higher = more detail but slower)
# src = "aws" uses AWS Terrain Tiles (free, good coverage)
elev_raster <- get_elev_raster(sites_sf, z = 11, src = "aws")

cat("Elevation data downloaded.\n")
cat("Elevation range:", range(values(elev_raster), na.rm = TRUE), "meters\n")

#--------------------------------
## Create static ggplot map
#--------------------------------

# Convert raster to data frame for ggplot
elev_df <- as.data.frame(elev_raster, xy = TRUE)
names(elev_df) <- c("x", "y", "elevation")

# Filter out NA values
elev_df <- elev_df %>% filter(!is.na(elevation))

# Create static map
p_static <- ggplot() +
  # Elevation layer
  geom_raster(data = elev_df, aes(x = x, y = y, fill = elevation)) +
  scale_fill_gradientn(
    colors = terrain.colors(100),
    name = "Elevation (m)",
    na.value = "transparent"
  ) +
  # Site points
  geom_sf(data = sites_sf, aes(color = burned), size = 3, alpha = 0.8) +
  scale_color_manual(
    values = c("B" = "red", "U" = "blue"),
    labels = c("B" = "Burned", "U" = "Unburned"),
    name = "Status"
  ) +
  # Add site labels
  geom_sf_text(data = sites_sf, aes(label = site),
               size = 2.5, hjust = -0.1, vjust = -0.5) +
  coord_sf(xlim = c(bbox_buffered["xmin"], bbox_buffered["xmax"]),
           ylim = c(bbox_buffered["ymin"], bbox_buffered["ymax"])) +
  labs(
    title = "Study Sites for Catchment Classification",
    subtitle = "Sites colored by burn status, overlaid on elevation",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10)
  )

print(p_static)

# Save the static map
ggsave("Figures/site_map_elevation.png", p_static,
       width = 12, height = 10, dpi = 300)
cat("\nStatic map saved to: Figures/site_map_elevation.png\n")

#--------------------------------
## Create interactive Leaflet map
#--------------------------------

cat("\nCreating interactive map...\n")

# Prepare popup text for each site
# Extract coordinates back from geometry for popup
coords_for_popup <- st_coordinates(sites_sf)
sites_sf <- sites_sf %>%
  mutate(
    lat_display = round(coords_for_popup[, "Y"], 4),
    lon_display = round(coords_for_popup[, "X"], 4),
    popup_text = paste0(
      "<b>Site:</b> ", site, "<br>",
      "<b>Stream:</b> ", Stream, "<br>",
      "<b>Status:</b> ", ifelse(burned == "B", "Burned", "Unburned"), "<br>",
      "<b>Years sampled:</b> ", years, "<br>",
      "<b>Lat:</b> ", lat_display, "<br>",
      "<b>Lon:</b> ", lon_display
    )
  )

# Create color palette for sites
pal <- colorFactor(
  palette = c("red", "blue"),
  domain = c("B", "U")
)

# Create leaflet map
map_interactive <- leaflet(sites_sf) %>%
  # Base map with topography
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topography") %>%
  addProviderTiles(providers$OpenTopoMap, group = "OpenTopo") %>%
  addProviderTiles(providers$OpenStreetMap, group = "Street Map") %>%
  # Add elevation as hillshade (if available)
  # Site markers
  addCircleMarkers(
    radius = 8,
    color = ~pal(burned),
    fillOpacity = 0.8,
    stroke = TRUE,
    weight = 2,
    popup = ~popup_text,
    label = ~site
  ) %>%
  # Add legend
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~burned,
    title = "Burn Status",
    labels = c("Burned", "Unburned")
  ) %>%
  # Add layer control
  addLayersControl(
    baseGroups = c("Topography", "OpenTopo", "Street Map"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # Add scale bar
  addScaleBar(position = "bottomleft")

# Display interactive map
print(map_interactive)

# Save interactive map as HTML
library(htmlwidgets)
saveWidget(map_interactive,
           "Figures/site_map_interactive.html",
           selfcontained = FALSE)
cat("\nInteractive map saved to: Figures/site_map_interactive.html\n")
cat("(Note: Open this file in a web browser to view the interactive map)\n")

#--------------------------------
## Print site summary for manual classification
#--------------------------------

cat("\n=== SITE SUMMARY FOR CATCHMENT CLASSIFICATION ===\n\n")

# Group by stream name
stream_summary <- site_coords %>%
  group_by(Stream) %>%
  summarize(
    n_sites = n(),
    sites = paste(site, collapse = ", "),
    lat_range = paste(round(min(Latitude), 3), "-", round(max(Latitude), 3)),
    lon_range = paste(round(min(Longitude), 3), "-", round(max(Longitude), 3)),
    .groups = "drop"
  ) %>%
  dplyr::arrange(desc(n_sites))

print(stream_summary)

cat("\n\nNumber of unique stream names:", nrow(stream_summary), "\n")
cat("Total sites:", sum(stream_summary$n_sites), "\n")

# Save stream summary
write.csv(stream_summary, "Data/stream_summary.csv", row.names = FALSE)
cat("\nStream summary saved to: Data/stream_summary.csv\n")

#--------------------------------
## Create a template for manual catchment assignment
#--------------------------------

catchment_template <- site_coords %>%
  dplyr::select(site, Stream, Latitude, Longitude) %>%
  mutate(
    catchment_id = NA_integer_,  # To be filled in manually
    catchment_name = NA_character_,  # To be filled in manually
    notes = NA_character_  # For any special notes
  )

write.csv(catchment_template, "Data/catchment_classification_template.csv",
          row.names = FALSE)
cat("\nCatchment classification template saved to:\n")
cat("  Data/catchment_classification_template.csv\n")
cat("\nFill in the 'catchment_id' and 'catchment_name' columns based on the maps.\n")

cat("\n=== DONE ===\n")
cat("Review the maps and use them to classify sites into catchments.\n")
cat("The Stream column provides initial groupings, but you may want to\n")
cat("combine or split these based on topography and drainage patterns.\n")
