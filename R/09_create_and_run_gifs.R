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
library(gtable)

source(here("R", "utils.R"))   # slugify(), msa_frame(), lc_years

# ---- MSA list ---------------------------------------------------------

msa_shapes <- readRDS(
  here::here("data", "processed", "geographies", "msa_geom.rds")
)

# every MSA (this previously read [13:nrow()], a leftover from resuming a
# partial run, which silently skipped the first 12 metros)
msa_list <- msa_shapes$MSA

# Each MSA's frame: a Web Mercator square centred on the app's circle, the same
# one the app's dot map opens to (msa_frame(), utils.R; stored by step 10).
msa_frames <- msa_frame(msa_shapes)

# Optional: render only MSAs whose name contains the text given on the command
# line, e.g. `Rscript R/09_create_and_run_gifs.R Austin`. Useful for checking a
# styling change on one metro instead of the multi-hour full run. With no
# argument, every MSA is rendered.
msa_filter <- commandArgs(trailingOnly = TRUE)
if (length(msa_filter) > 0) {
  msa_list <- msa_list[grepl(msa_filter[1], msa_list, fixed = TRUE)]
  if (length(msa_list) == 0) stop("No MSA name contains '", msa_filter[1], "'.")
  message("Rendering only: ", paste(msa_list, collapse = "; "))
}

# ---- 0. Parameters ---------------------------------------------------

years <- lc_years   # utils.R; step 10 relies on the same order to split the GIFs

# Developed NLCD classes
developed_classes <- 21:24

# Developed-class colours: CARTO's Sunset palette, reversed so density rises as
# colours get lighter.
# Four evenly spaced stops (1, 3, 5, 7) of Sunset's seven. Dense cores glow pale
# yellow against the darker purple suburban fringe, and light colours read best
# over the dark satellite basemap. Sunset's warm hues also echo the TealRose
# rose used in the app.
sunset_pal <- c(
  "21" = "#5C53A5",  # open space: deep purple
  "22" = "#CE6693",  # low intensity: magenta-rose
  "23" = "#F8A07E",  # medium intensity: coral
  "24" = "#F3E79B"   # high intensity: pale yellow
)

# Satellite basemap muting, so the overlay doesn't compete with real built-up
# areas (grey in imagery) or bright fields.
# Each pixel is blended toward its own grey by `imagery_desaturate` (0 = full
# colour, 1 = greyscale), then scaled by `imagery_brightness` (1 = unchanged).
imagery_desaturate <- 0.4
imagery_brightness <- 0.75

mute_rgb <- function(r, g, b, desaturate = imagery_desaturate, brightness = imagery_brightness) {
  lum  <- 0.299 * r + 0.587 * g + 0.114 * b          # perceptual luminance (Rec. 601)
  mute <- function(ch) pmin(255, pmax(0, (ch + desaturate * (lum - ch)) * brightness))
  rgb(mute(r), mute(g), mute(b), maxColorValue = 255)
}

# Plain grey ribbon under the map. Its grey (#303030) is the app's panel grey
# (the darkly theme's .well colour, colors$panel_grey in app/R/constants.R), so
# the ribbon reads as part of the page. Nothing is drawn on it: the app lays
# the Developed Land legend (lc_legend in app/R/constants.R, whose colours must
# match `sunset_pal` above) and the year slider over this band, built like the
# dot map's ribbon beside it, so they can change without re-rendering. The
# metro name is left out too: the app shows it in the header above the image.
ribbon_height <- unit(130, "pt")
ribbon_fill   <- "#303030"

# Frame and GIF size: a square map plus the ribbon beneath it.
# The map is `frame_w_in` inches square and shows exactly the shared frame
# (msa_frame()), like the app's square dot map; the canvas adds the ribbon's
# height below it, so no part of the map is covered. app/R/constants.R
# gif_size must match gif_w_px x gif_h_px, and its gif_ribbon_frac must match
# ribbon_height / frame_w_in.
frame_w_in <- 7.5
frame_h_in <- frame_w_in + convertUnit(ribbon_height, "inches", valueOnly = TRUE)
gif_w_px   <- 800
gif_h_px   <- round(gif_w_px * frame_h_in / frame_w_in)

# The ribbon itself: one grey rectangle filling its row.
ribbon_grob <- function() {
  rectGrob(gp = gpar(fill = ribbon_fill, col = NA))
}

# Attach the ribbon beneath a finished map plot.
# Converts the ggplot to a gtable, inserts a fixed-height row directly under
# the panel, and places the ribbon across the panel's columns so it is exactly
# as wide as the map. The canvas is tall enough for both (frame_h_in), so the
# map stays the full square frame and nothing is covered. Returns a gtable,
# which ggsave() accepts as `plot`.
add_ribbon <- function(p) {
  g     <- ggplotGrob(p)
  panel <- g$layout[g$layout$name == "panel", ]
  g     <- gtable::gtable_add_rows(g, ribbon_height, pos = panel$b)
  gtable::gtable_add_grob(g, ribbon_grob(),
                          t = panel$b + 1, l = panel$l, r = panel$r,
                          name = "ribbon", clip = "on")
}

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
  
  # ---- 1. Static satellite basemap for this MSA ----------------------
  
  # Use one year to define the extent (e.g. 2013)
  r_first_path <- file.path(r_dir, "nlcd_2013.tif")
  if (!file.exists(r_first_path)) {
    message("  Skipping ", msa_name, " — no nlcd_2013.tif in ", r_dir)
    next
  }
  
  r_first <- rast(r_first_path)
  ref_ext <- ext(r_first)

  # Frames are drawn in Web Mercator (EPSG:3857), the projection of the app's
  # dot map beside the GIF, so the two show the same area with the same
  # orientation. (They used to be drawn in NLCD Albers, which is rotated
  # against Mercator by up to ~13 degrees for metros far from -96.)
  # The frame is the shared msa_frame() square; step 02 cut each crop as the
  # Albers rectangle that contains it, so reprojected land cover fills it.
  # The satellite tiles are Web Mercator already, so they need no warping.
  frame     <- msa_frames[msa_frames$MSA == msa_name, ]
  frame_ext <- ext(as.vector(st_bbox(frame))[c(1, 3, 2, 4)])   # xmin, xmax, ymin, ymax

  basemap <- get_tiles(
    x        = frame,
    provider = "Esri.WorldImagery",
    zoom     = 9,
    crop     = TRUE,
    project  = FALSE
  )
  basemap <- terra::crop(basemap, frame_ext)

  # Fixed output grid over the frame, shared by every year, so frames cannot
  # shift. 900 x 900 cells is about the map's pixel size in the GIF; every
  # year's crop is reprojected onto it below.
  template <- rast(frame_ext, nrows = 900, ncols = 900, crs = "EPSG:3857")

  base_df <- as.data.frame(basemap, xy = TRUE)
  
  # Identify RGB bands
  rgb_cols <- setdiff(names(base_df), c("x", "y"))
  rgb_cols <- rgb_cols[1:3]
  
  base_df <- base_df |>
    mutate(
      r   = .data[[rgb_cols[1]]],
      g   = .data[[rgb_cols[2]]],
      b   = .data[[rgb_cols[3]]],
      rgb = mute_rgb(r, g, b)
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

    # Flag crops whose extent differs from the 2013 reference.
    # A mismatched crop still renders on the shared grid, but any area it is
    # missing shows up as lost development, so it should be re-cropped in 02.
    if (!isTRUE(all.equal(as.vector(ext(r)), as.vector(ref_ext)))) {
      warning("[", msa_name, "] nlcd_", yr, ".tif extent differs from 2013; ",
              "re-run R/02_load_transform_and_crop_lc_rasters.R ", yr, ".")
    }

    # Reproject from Albers onto the shared Web Mercator grid.
    # Nearest-neighbour keeps class codes intact (no blending of categories).
    r_ds <- project(r, template, method = "near")

    # Keep only developed classes at raster level
    r_ds_dev <- terra::ifel(r_ds %in% developed_classes, r_ds, NA)

    # Raster -> data frame; drop NAs to lighten ggplot load
    df_year <- as.data.frame(r_ds_dev, xy = TRUE, na.rm = TRUE) |>
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
        values       = sunset_pal,
        na.value     = NA,
        na.translate = FALSE
      ) +
      # lock the view to the frame square; anything past it is clipped
      coord_equal(xlim = frame_ext[1:2], ylim = frame_ext[3:4], expand = FALSE) +
      labs(x = NULL, y = NULL) +
      theme_void() +
      theme(
        # no ggplot legend: the app draws the legend over the ribbon
        legend.position      = "none",
        plot.background      = element_rect(fill = "white", color = NA),
        panel.background     = element_rect(fill = "white", color = NA),
        plot.margin          = margin(0, 0, 0, 0)
      )

    # plain grey ribbon under the map (the app draws the legend and year slider on it)
    p <- add_ribbon(p)

    frame_path <- file.path(frame_dir, sprintf("frame_%d.png", yr))
    ggsave(
      filename = frame_path,
      plot     = p,
      width    = frame_w_in,
      height   = frame_h_in,
      dpi      = 150
    )

    # explicitly free big objects each iteration
    rm(r, r_ds, r_ds_dev, df_year, p)
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
    width  = gif_w_px,
    height = gif_h_px,
    delay  = 0.3
  )

  # Record the frame this GIF was drawn in, next to it.
  # Step 10 compares it with msa_frame() and warns about any GIF drawn in a
  # different (older) frame, since such a GIF won't line up with the app's dot
  # map. Web Mercator metres; 1 m precision is far finer than a GIF pixel.
  write.csv(
    as.data.frame(as.list(round(st_bbox(frame), 1))),
    file.path(gif_root, paste0(msa_slug, "_developed_frame.csv")),
    row.names = FALSE
  )

  gc()
}
