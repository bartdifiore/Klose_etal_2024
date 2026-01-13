#--------------------------------
## Spatial Bubble Plot: Stream Quality and Trout Abundance
## Shows stream quality (color) and trout count (size) on topographic basemap
#--------------------------------

library(tidyverse)
library(sf)
library(raster)
library(elevatr)
library(rstan)
library(terra)
library(geodata)
library(ggnewscale)  # For multiple fill scales

#--------------------------------
## Load model results and data
#--------------------------------

# Load fitted model
fit <- readRDS("Models/stan_model_full_hurdle_fit.rds")

# Load original data
df <- read.csv("Data/Data_20240408.csv")

# Clean column names (remove BOM character if present)
names(df)[1] <- "site"

# Prepare data with model results
df_mod <- df %>%
  mutate(
    # Response variable
    trout_count = as.integer(total.trout),

    # Transform continuous predictors (scaled for modeling)
    max_depth_scaled = as.numeric(scale(Max.depth.m)),
    do_scaled = as.numeric(scale(as.numeric(Point.Minimum.DO.mg.L))),
    conduct_log_scaled = as.numeric(scale(log(Conductivity.uS.cm.))),
    canopy_logit_scaled = as.numeric(scale(qlogis(Average.canopy.cover/100))),
    thermal_scaled = as.numeric(scale(Thermal.index)),
    q_log_scaled = as.numeric(scale(log(`Q.estimate..m3.s.`))),

    # Categorical predictors
    burned_coded = as.numeric(ifelse(`Burned..B..vs..unburned..U.` == "B", 1, 0)),
    wet_coded = as.numeric(ifelse(`Wet.or.dry.in.2016` == "W", 1, 0)),

    # Factor versions for plotting
    burned = `Burned..B..vs..unburned..U.`,
    wet_dry = `Wet.or.dry.in.2016`
  ) %>%
  filter(!is.na(trout_count),
         !is.na(conduct_log_scaled),
         !is.na(max_depth_scaled),
         !is.na(do_scaled),
         !is.na(thermal_scaled),
         !is.na(canopy_logit_scaled),
         !is.na(q_log_scaled),
         !is.na(burned_coded),
         !is.na(wet_coded))

# Extract stream quality estimates
stream_quality_samples <- as.matrix(fit,
  pars = paste0("stream_quality[", 1:nrow(df_mod), "]"))

# Calculate median for each observation
df_mod$stream_quality_median <- apply(stream_quality_samples, 2, median)

# Add spatial coordinates and year information
df_spatial <- df_mod %>%
  mutate(
    site_name = site,
    year = year,
    # Extract base site name by removing year suffix (e.g., "Fox Creek_2016" -> "Fox Creek")
    site_base = str_replace(site, "_\\d{4}$", ""),
    Longitude = Longitude,
    Latitude = Latitude,
    trout_present = trout_count > 0
  ) %>%
  filter(!is.na(Latitude), !is.na(Longitude))

# Identify sites sampled in multiple years (using base site name)
site_year_counts <- df_spatial %>%
  group_by(site_base) %>%
  summarize(
    n_years = n(),
    years = paste(sort(unique(year)), collapse = ", "),
    base_lon = mean(Longitude),
    base_lat = mean(Latitude),
    .groups = "drop"
  )

# Add dodge offset for multi-year sites
dodge_distance <- 0.045  # degrees (~5 km for better visibility and no overlap)

df_spatial <- df_spatial %>%
  left_join(site_year_counts, by = "site_base") %>%
  group_by(site_base) %>%
  mutate(
    year_index = match(year, sort(unique(year))),
    n_years_site = n(),
    # Offset positions for multi-year sites at 45-degree separation
    # 2016 and 2017 are 45 degrees apart from each other
    # Determine if site is in western or eastern part of study area
    is_western = base_lon < median(site_year_counts$base_lon[site_year_counts$n_years > 1]),
    # Set angle based on year and position
    # Western sites: 2016 at 67.5°, 2017 at 112.5° (45° apart, north-facing)
    # Eastern sites: 2016 at -67.5°, 2017 at -112.5° (45° apart, south-facing)
    angle_deg = ifelse(n_years_site > 1,
                      ifelse(is_western,
                            ifelse(year_index == 1, 67.5, 112.5),  # Western: 45° apart, north-facing
                            ifelse(year_index == 1, -67.5, -112.5)), # Eastern: 45° apart, south-facing
                      0),
    # Convert to radians and calculate offsets
    angle_rad = angle_deg * pi / 180,
    lon_offset = ifelse(n_years_site > 1, dodge_distance * cos(angle_rad), 0),
    lat_offset = ifelse(n_years_site > 1, dodge_distance * sin(angle_rad), 0),
    lon_display = base_lon + lon_offset,
    lat_display = base_lat + lat_offset
  ) %>%
  ungroup()

cat("Number of sites with valid data:", nrow(df_spatial), "\n")
cat("Unique site locations:", length(unique(df_spatial$site_name)), "\n")
cat("Sites sampled multiple years:", sum(site_year_counts$n_years > 1), "\n")
cat("Sites with trout:", sum(df_spatial$trout_present), "\n")
cat("Sites without trout:", sum(!df_spatial$trout_present), "\n")

#--------------------------------
## Create spatial objects
#--------------------------------

# Convert to sf object (WGS84 projection)
sites_sf <- st_as_sf(df_spatial,
                     coords = c("Longitude", "Latitude"),
                     crs = 4326)

# Get bounding box with buffer
bbox <- st_bbox(sites_sf)
bbox_buffered <- bbox + c(-0.08, -0.08, 0.08, 0.08)  # Add buffer for display

# Create a slightly larger bbox for downloading elevation (to ensure coverage)
bbox_elev_download <- bbox + c(-0.15, -0.15, 0.15, 0.15)  # Larger buffer for download

cat("\nBounding box (buffered for display):\n")
print(bbox_buffered)
cat("\nBounding box (for elevation download):\n")
print(bbox_elev_download)

#--------------------------------
## Get elevation data
#--------------------------------

cat("\nDownloading elevation data...\n")

# Create a larger bbox polygon for elevation download to ensure full coverage
bbox_elev_poly <- st_as_sf(st_as_sfc(bbox_elev_download, crs = 4326))

# Get elevation raster for the larger region to ensure it covers all rivers
# z = 10 for good detail without being too slow
elev_raster <- get_elev_raster(bbox_elev_poly, z = 10, src = "aws")

cat("Elevation data downloaded.\n")
cat("Elevation range:", range(values(elev_raster), na.rm = TRUE), "meters\n")

#--------------------------------
## Get river/stream data (using multiple sources)
#--------------------------------

cat("\nAttempting to download river data...\n")

has_rivers <- FALSE

# Method 1: Try nhdplusTools for USGS NHD data (best for USA)
if (!has_rivers && !require(nhdplusTools, quietly = TRUE)) {
  cat("Attempting to install nhdplusTools for USGS hydrography data...\n")
  tryCatch({
    install.packages("nhdplusTools", repos = "https://cloud.r-project.org")
    library(nhdplusTools)
  }, error = function(e) {
    cat("Could not install nhdplusTools:", e$message, "\n")
  })
}

if (require(nhdplusTools, quietly = TRUE)) {
  cat("Trying to download NHD flowlines from USGS...\n")
  tryCatch({
    library(nhdplusTools)

    # Convert bbox to polygon for NHD query
    bbox_poly <- st_as_sfc(st_bbox(bbox_buffered, crs = 4326))

    # Download NHD flowlines for the area
    rivers_nhd <- get_nhdplus(AOI = bbox_poly, realization = "flowline")

    if (!is.null(rivers_nhd) && nrow(rivers_nhd) > 0) {
      # Filter to only show streams - use order 2 and above for more detail
      # Clip to the exact bounding box to match elevation layer
      rivers_cropped <- rivers_nhd %>%
        filter(streamorde >= 2) %>%  # Keep streams of order 2 and above for more detail
        st_crop(bbox_buffered) %>%  # Crop to the extent
        st_intersection(st_as_sfc(st_bbox(bbox_buffered, crs = 4326)))  # Strict clip

      cat("NHD river data downloaded:", nrow(rivers_cropped), "features\n")
      has_rivers <- TRUE
    }
  }, error = function(e) {
    cat("Could not download NHD data:", e$message, "\n")
  })
}

# Method 2: Fall back to Natural Earth if NHD fails
if (!has_rivers) {
  cat("Trying Natural Earth rivers as fallback...\n")

  if (!require(rnaturalearth, quietly = TRUE)) {
    install.packages("rnaturalearth", repos = "https://cloud.r-project.org")
    library(rnaturalearth)
  }

  tryCatch({
    # Try 110m scale (broader coverage)
    rivers_global <- ne_download(scale = 110, type = 'rivers_lake_centerlines',
                                 category = 'physical', returnclass = "sf")

    # Crop to study area
    rivers_bbox <- st_as_sfc(st_bbox(bbox_buffered, crs = 4326))
    rivers_cropped <- st_crop(rivers_global, rivers_bbox)

    if (nrow(rivers_cropped) > 0) {
      cat("Natural Earth river data downloaded:", nrow(rivers_cropped), "features\n")
      has_rivers <- TRUE
    } else {
      cat("No rivers found in study area with Natural Earth data.\n")
    }
  }, error = function(e) {
    cat("Could not download Natural Earth data:", e$message, "\n")
  })
}

if (!has_rivers) {
  cat("Map will be created without river layer.\n")
  cat("Note: Rivers may be too small for global datasets in this region.\n")
}

#--------------------------------
## Prepare elevation data for plotting
#--------------------------------

# Convert raster to data frame for ggplot
elev_df <- as.data.frame(elev_raster, xy = TRUE)
names(elev_df) <- c("x", "y", "elevation")

# Filter out NA values and clip to the exact bounding box
elev_df <- elev_df %>%
  filter(!is.na(elevation)) %>%
  filter(x >= bbox_buffered["xmin"], x <= bbox_buffered["xmax"],
         y >= bbox_buffered["ymin"], y <= bbox_buffered["ymax"]) %>%
  mutate(
    is_ocean = elevation < 0,
    elev_land = ifelse(elevation >= 0, elevation, NA)
  )

# Calculate contour intervals
elev_range <- range(elev_df$elevation, na.rm = TRUE)
contour_interval <- round((elev_range[2] - elev_range[1]) / 15, -1)  # ~15 contours
contour_breaks <- seq(
  floor(elev_range[1] / contour_interval) * contour_interval,
  ceiling(elev_range[2] / contour_interval) * contour_interval,
  by = contour_interval
)

cat("\nContour interval:", contour_interval, "meters\n")
cat("Number of contours:", length(contour_breaks), "\n")

# Prepare data for bubble plot with original and display coordinates
df_spatial_plot <- df_spatial %>%
  mutate(
    lon_base = Longitude,
    lat_base = Latitude,
    lon = lon_display,
    lat = lat_display
  )

#--------------------------------
## Create the spatial bubble plot
#--------------------------------

# Create color palette for stream quality
# Brown to green gradient for topography
topo_colors <- colorRampPalette(c(
  "#2d5016",  # dark green (low)
  "#5a7f32",  # medium green
  "#8fbc8f",  # light green
  "#d4c19c",  # tan
  "#a67c52",  # light brown
  "#8b6914",  # brown
  "#654321"   # dark brown (high)
))(100)

# Color palette for stream quality (diverging: red = poor, blue = good)
quality_colors <- colorRampPalette(c(
  "#d73027",  # red (poor quality)
  "#fc8d59",  # orange
  "#fee090",  # yellow
  "#e0f3f8",  # light blue
  "#91bfdb",  # medium blue
  "#4575b4"   # dark blue (good quality)
))(100)

# Create the map
p_bubble <- ggplot() +
  # Ocean layer (elevation < 0) - solid blue
  geom_raster(
    data = elev_df %>% filter(is_ocean),
    aes(x = x, y = y),
    fill = "lightblue3"
  ) +
  # Land elevation layer with topographic colors
  geom_raster(
    data = elev_df %>% filter(!is_ocean),
    aes(x = x, y = y, fill = elev_land)
  ) +
  scale_fill_gradientn(
    colors = topo_colors,
    name = "Elevation (m)",
    na.value = "transparent"
  ) +
  # Add contour lines (land only)
  geom_contour(
    data = elev_df %>% filter(!is_ocean),
    aes(x = x, y = y, z = elev_land),
    breaks = contour_breaks,
    color = "gray40",
    alpha = 0.4,
    linewidth = 0.3
  ) +
  new_scale_fill() +  # Reset fill scale for next layer
  # Add rivers if available
  {
    if (has_rivers && exists("rivers_cropped") && nrow(rivers_cropped) > 0) {
      geom_sf(data = rivers_cropped, color = "dodgerblue", linewidth = 0.8, alpha = 0.6)
    }
  } +
  # Add connecting lines from base location to dodged positions for multi-year sites
  geom_segment(
    data = df_spatial_plot %>% filter(n_years_site > 1),
    aes(x = lon_base, y = lat_base, xend = lon, yend = lat),
    color = "gray30",
    linewidth = 0.6,
    alpha = 0.7
  ) +
  # Add black center point at actual sampling location for multi-year sites
  geom_point(
    data = df_spatial_plot %>%
      filter(n_years_site > 1) %>%
      distinct(site_base, .keep_all = TRUE),
    aes(x = lon_base, y = lat_base),
    shape = 21,
    size = 3,
    fill = "black",
    color = "white",
    stroke = 0.8
  ) +
  # Sites WITHOUT trout (triangles, colored by stream quality)
  geom_point(
    data = df_spatial_plot %>% filter(!trout_present),
    aes(x = lon, y = lat, fill = stream_quality_median),
    shape = 24,  # filled triangle
    size = 4,
    color = "black",
    stroke = 0.5,
    alpha = 0.9
  ) +
  # Sites WITH trout (circles, sized by count, colored by stream quality)
  geom_point(
    data = df_spatial_plot %>% filter(trout_present),
    aes(x = lon, y = lat, size = trout_count, fill = stream_quality_median),
    shape = 21,  # filled circle
    color = "black",
    stroke = 0.5,
    alpha = 0.9
  ) +
  # Add year labels for multi-year sites - positioned to avoid overlap
  geom_text(
    data = df_spatial_plot %>% filter(n_years_site > 1),
    aes(x = lon, y = lat, label = year),
    size = 3,
    fontface = "bold",
    color = "white",
    vjust = -1.8,
    hjust = 0.5
  ) +
  scale_fill_gradientn(
    colors = quality_colors,
    name = "Stream\nQuality",
    limits = range(df_spatial_plot$stream_quality_median),
    guide = guide_colorbar(order = 1)
  ) +
  scale_size_continuous(
    name = "Trout\nCount",
    range = c(3, 12),
    breaks = c(1, 10, 50, 100, 200),
    guide = guide_legend(order = 2)
  ) +
  coord_sf(
    xlim = c(bbox_buffered["xmin"], bbox_buffered["xmax"]),
    ylim = c(bbox_buffered["ymin"], bbox_buffered["ymax"]),
    crs = 4326,
    expand = FALSE  # Don't expand beyond the exact limits
  ) +
  labs(
    title = "Spatial Distribution of Stream Quality and Trout Abundance",
    subtitle = "Circles = sites with trout (size ∝ count); Triangles = sites without trout\nColor indicates latent stream quality (red = poor, blue = good)\nMulti-year sites: black point = location, lines connect to year-specific measurements",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    panel.grid.major = element_line(color = "gray80", linewidth = 0.3),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5)
  )

print(p_bubble)

# Save the map
ggsave("Figures/fig_spatial_stream_quality_trout.png", p_bubble,
       width = 12, height = 10, dpi = 300)
ggsave("Figures/fig_spatial_stream_quality_trout.pdf", p_bubble,
       width = 12, height = 10)

cat("\n=== SPATIAL MAP SAVED ===\n")
cat("Saved to:\n")
cat("  - Figures/fig_spatial_stream_quality_trout.png\n")
cat("  - Figures/fig_spatial_stream_quality_trout.pdf\n")

#--------------------------------
## Summary statistics by spatial region
#--------------------------------

cat("\n=== SPATIAL SUMMARY ===\n\n")
cat("Stream quality range:", round(range(df_spatial_plot$stream_quality_median), 2), "\n")
cat("Trout count range:", range(df_spatial_plot$trout_count), "\n")
cat("Mean stream quality:", round(mean(df_spatial_plot$stream_quality_median), 2), "\n")
cat("Mean trout count (all sites):", round(mean(df_spatial_plot$trout_count), 2), "\n")
cat("Mean trout count (where present):",
    round(mean(df_spatial_plot$trout_count[df_spatial_plot$trout_present]), 2), "\n")
