# Only needed if PROJ cannot find its data directory on this machine.
# Guarded so the script is not tied to a local QGIS install.
if (!nzchar(Sys.getenv("PROJ_LIB"))) {
  qgis_proj <- "/Applications/QGIS.app/Contents/Resources/qgis/proj"
  if (dir.exists(qgis_proj)) Sys.setenv(PROJ_LIB = qgis_proj)
}

library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(ggnewscale)
library(maptiles)
library(here)
library(gifski)
library(grid)

source(here("R", "utils.R"))   # slugify()

# ---- MSA list ---------------------------------------------------------

msa_shapes <- readRDS(
  here::here("data", "processed", "geographies", "msa_geom.rds")
)

# every MSA (this previously read [13:nrow()], a leftover from resuming a
# partial run, which silently skipped the first 12 metros)
msa_list <- msa_shapes$MSA

# ---- 0. Parameters ---------------------------------------------------

years <- 1999:2024

# Developed NLCD classes
developed_classes <- 21:24

# Purple palette: light → dark for 21, 22, 23, 24
purple_pal <- c(
  "21" = "#F3E5F5",  # very light purple
  "22" = "#CE93D8",
  "23" = "#9C27B0",
  "24" = "#4A148C"   # very dark purple
)

# Directory base to hold per-year PNG frames (per MSA subfolders)
frame_root <- here("output", "frames_landcover_gif")
dir.create(frame_root, recursive = TRUE, showWarnings = FALSE)

# Directory for GIF outputs
gif_root <- here("output","gifs")
dir.create(gif_root, recursive = TRUE, showWarnings = FALSE)

# ======================================================================
# LOOP OVER ALL MSAs
# ======================================================================

for (msa_name in msa_list) {
  message("====================================================")
  message("Processing MSA: ", msa_name)
  message("====================================================")
  
  # slug for this MSA (must match your landcover_crops folder names)
  msa_slug <- slugify(msa_name)
  
  # Folder with your cropped NLCD for this MSA (already filtered to extent)
  r_dir <- here("data", "processed", "landcover_crops", msa_slug)
  
  if (!dir.exists(r_dir)) {
    message("  Skipping ", msa_name, " — no directory: ", r_dir)
    next
  }
  
  # Per-MSA frame directory
  frame_dir <- file.path(frame_root, msa_slug)
  dir.create(frame_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ---- 1. Static terrain basemap for this MSA ------------------------
  
  # Use one year to define the extent (e.g. 2013)
  r_first_path <- file.path(r_dir, "nlcd_2013.tif")
  if (!file.exists(r_first_path)) {
    message("  Skipping ", msa_name, " — no nlcd_2013.tif in ", r_dir)
    next
  }
  
  r_first    <- rast(r_first_path)
  r_first_ll <- project(r_first, "EPSG:4326")  # WGS84 for web tiles
  
  # Get terrain-style basemap for that extent
  basemap <- get_tiles(
    x        = r_first_ll,
    provider = "Esri.WorldTerrain",
    zoom     = 9,
    crop     = TRUE
  )
  
  base_df <- as.data.frame(basemap, xy = TRUE)
  
  # Identify RGB bands
  rgb_cols <- setdiff(names(base_df), c("x", "y"))
  rgb_cols <- rgb_cols[1:3]
  
  base_df <- base_df |>
    mutate(
      r   = .data[[rgb_cols[1]]],
      g   = .data[[rgb_cols[2]]],
      b   = .data[[rgb_cols[3]]],
      rgb = rgb(r, g, b, maxColorValue = 255)
    )
  
  ################################################################################
  # Render one PNG frame per year for this MSA ----------------------------------
  ################################################################################
  #
  # A "dev-prep" loop used to sit here. It wrote one nlcd_dev_ll_<year>.tif per
  # MSA-year plus a template_ll_aggr.tif, then nothing ever read them: the frame
  # loop below re-reads the original nlcd_<year>.tif and reprojects from
  # scratch. Removing it dropped ~744MB of write-only output and roughly halved
  # this script's runtime.

  for (yr in years) {
    message("  Rendering frame for year ", yr, " ...")
    
    # Load this year's cropped NLCD for the MSA
    r_path <- file.path(r_dir, sprintf("nlcd_%d.tif", yr))
    if (!file.exists(r_path)) {
      message("    [", msa_name, "] Missing raster for year ", yr, " — skipping this frame.")
      next
    }
    
    r <- rast(r_path)
    
    # Reproject to WGS84 to match basemap
    r_ll <- project(r, "EPSG:4326")
    
    # Downsample to keep size manageable
    r_ll <- terra::aggregate(r_ll, fact = 8, fun = "first")
    
    # Keep only developed classes at raster level
    r_ll_dev <- terra::ifel(r_ll %in% developed_classes, r_ll, NA)
    
    # Raster -> data frame; drop NAs to lighten ggplot load
    df_year <- as.data.frame(r_ll_dev, xy = TRUE, na.rm = TRUE) |>
      rename(value = 3) |>
      mutate(
        value = factor(value, levels = developed_classes)
      )
    
    p <- ggplot() +
      # 1) Basemap
      geom_raster(
        data = base_df,
        aes(x = x, y = y, fill = rgb)
      ) +
      scale_fill_identity() +
      ggnewscale::new_scale_fill() +
      
      # 2) Developed overlay
      geom_raster(
        data  = df_year,
        aes(x = x, y = y, fill = value),
        alpha = 1
      ) +
      scale_fill_manual(
        values       = purple_pal,
        breaks       = c("21", "22", "23", "24"),
        labels       = c("Low Intensity", "", "", "High Intensity"),
        name         = NULL,
        na.value     = NA,
        na.translate = FALSE
      ) +
      coord_equal(expand = 0) +
      labs(x = NULL, y = NULL) +
      theme_void() +
      theme(
        legend.position      = c(0.05, 0.2),
        legend.justification = c(0, 0),
        plot.background      = element_rect(fill = "white", color = NA),
        panel.background     = element_rect(fill = "white", color = NA),
        plot.margin          = margin(0, 0, 0, 0)
      ) +
      annotation_custom(
        grob = textGrob(
          paste("Developed Land:", yr),
          x     = unit(0.05, "npc"),
          y     = unit(0.95, "npc"),
          just  = c("left", "top"),
          gp    = gpar(fontface = "bold", cex = 1.2)
        ),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
      )
    
    frame_path <- file.path(frame_dir, sprintf("frame_%d.png", yr))
    ggsave(
      filename = frame_path,
      plot     = p,
      width    = 8,
      height   = 6,
      dpi      = 150
    )
    
    # explicitly free big objects each iteration
    rm(r, r_ll, r_ll_dev, df_year, p)
    gc()
  }
  
  ################################################################################
  # GIF CREATION FOR THIS MSA (same frames, just looped over all MSAs) ----------
  ################################################################################
  
  png_files <- file.path(frame_dir, sprintf("frame_%d.png", years))
  png_files <- png_files[file.exists(png_files)]
  
  if (length(png_files) == 0) {
    message("  No frames found for ", msa_name, " — skipping GIF.")
    next
  }
  
  gif_path <- file.path(gif_root, paste0(msa_slug, "_developed.gif"))
  
  message("  Creating GIF for ", msa_name, " -> ", gif_path)
  
  gifski::gifski(
    png_files,
    gif_path,
    width  = 800,
    height = 600,
    delay  = 0.3
  )
  
  gc()
}
