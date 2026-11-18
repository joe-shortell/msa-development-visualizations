library(terra)
library(sf)
library(here)

source(here("R", "utils.R"))   # slugify(), msa_frame()

# --- parameters -------------------------------------------------------
years <- 1999:2024

# Optional: re-crop only the years given on the command line, e.g.
# `Rscript R/02_load_transform_and_crop_lc_rasters.R 2024`. Useful for repairing
# one year's crops without the multi-hour full run.
# Arguments are parsed as integers and must fall within `years`; with no
# argument, every year is processed.
year_filter <- suppressWarnings(as.integer(commandArgs(trailingOnly = TRUE)))
if (length(year_filter) > 0) {
  if (anyNA(year_filter) || !all(year_filter %in% years)) {
    stop("Year arguments must be integers within ", min(years), "-", max(years), ".")
  }
  years <- year_filter
  message("Cropping only: ", paste(years, collapse = ", "))
}

# where the raw NLCD rasters live, and where to put the cropped ones
lc_dir   <- here("data", "raw", "landcover")
out_base <- here("data", "processed", "landcover_crops")

# --- load MSA geography ----------------------------------------------
template_area <- readRDS(
  here("data", "processed", "geographies", "msa_geom.rds")
)

msas <- template_area$MSA   # or template_area$NAME

# Each MSA's GIF frame: a Web Mercator square shared with the app's dot map
# (msa_frame() in utils.R). Crops are cut to cover it once reprojected.
msa_frames <- msa_frame(template_area)

# crop extents, worked out once we see the first NLCD raster's CRS
msa_crop_extents <- NULL

terraOptions(todisk = FALSE)  # encourage in-memory operations (optional)

# developed NLCD classes you want to keep
developed_classes <- 21:24

for (yr in years) {
  message("=== Year ", yr, " ===")
  
  tif_path <- file.path(lc_dir,
                        sprintf("Annual_NLCD_LndCov_%d_CU_C1V1.tif", yr))

  # 1. Load big NLCD raster for this year -----------------------------
  # The .zip archives are no longer kept (each was a duplicate of its own
  # extracted .tif, ~1.3GB apiece). Download the missing year's GeoTIFF from
  # the USGS Annual NLCD collection into data/raw/landcover/ and re-run.
  if (!file.exists(tif_path)) {
    warning("NLCD TIF not found for ", yr, ": ", tif_path, " — skipping year.")
    next
  }

  message("Loading NLCD TIF for ", yr, " ...")
  r <- rast(tif_path)
  
  # 2. Crop extents covering each MSA's Mercator frame (computed once) --------
  # The GIF is drawn in Web Mercator (step 09), but the NLCD is in Albers. The
  # Mercator square becomes a slightly rotated, curved-edged shape in Albers,
  # so each crop is that shape's Albers bounding box: a rectangle that fully
  # contains the frame. Edges are densified before transforming so the curve
  # is followed, and a 2 km margin keeps the frame's edge cells from falling
  # off the crop when step 09 reprojects it.
  if (is.null(msa_crop_extents)) {
    frames_albers <- msa_frames |>
      st_segmentize(1000) |>
      st_transform(st_crs(terra::crs(r)))

    msa_crop_extents <- lapply(seq_len(nrow(frames_albers)), function(i) {
      bb <- st_bbox(frames_albers[i, ])
      terra::ext(bb[["xmin"]] - 2000, bb[["xmax"]] + 2000,
                 bb[["ymin"]] - 2000, bb[["ymax"]] + 2000)
    })
  }
  
  # 3. Crop to each MSA's frame extent, keep only developed classes, save -----
  for (i in seq_along(msas)) {
    msa_name <- msas[i]
    msa_slug <- slugify(msa_name)

    # crop extent for this MSA (same for all years, so frames can't shift)
    lc_crop <- terra::crop(r, msa_crop_extents[[i]])
    
    # keep only developed classes (21–24), preserve their distinct values
    lc_dev <- terra::app(
      lc_crop,
      fun = function(x) ifelse(x %in% developed_classes, x, NA)
    )
    
    # output folder per MSA
    out_dir <- file.path(out_base, msa_slug)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    out_tif <- file.path(out_dir, sprintf("nlcd_%d.tif", yr))
    
    message("  Writing developed-only NLCD for ", msa_name,
            " (", yr, ") → ", out_tif)
    writeRaster(lc_dev, out_tif, overwrite = TRUE)
  }
  
  rm(r); gc()
  message("Finished year ", yr, "\n")
}
