library(terra)
library(sf)
library(here)

source(here("R", "utils.R"))   # slugify()

# --- parameters -------------------------------------------------------
years <- 1999:2024

# where the raw NLCD rasters live, and where to put the cropped ones
lc_dir   <- here("data", "raw", "landcover")
out_base <- here("data", "processed", "landcover_crops")

# --- load MSA geography ----------------------------------------------
template_area <- readRDS(
  here("data", "processed", "geographies", "msa_geom.rds")
)

msas <- template_area$MSA   # or template_area$NAME

# we’ll reproject once, when we see the first NLCD raster
template_area_transformed <- NULL
msa_square_extents <- NULL   # list of square extents per MSA

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
  
  # 2. Transform MSAs to raster CRS (once) and precompute square extents ------
  if (is.null(template_area_transformed)) {
    template_area_transformed <- st_transform(
      template_area,
      st_crs(terra::crs(r))
    )
    
    # precompute one square extent per MSA
    msa_square_extents <- lapply(1:nrow(template_area_transformed), function(i) {
      bb <- terra::ext(template_area_transformed[i, ])
      # bb = c(xmin, xmax, ymin, ymax)
      xmin <- bb[1]; xmax <- bb[2]
      ymin <- bb[3]; ymax <- bb[4]
      
      w <- xmax - xmin
      h <- ymax - ymin
      
      # side of square = max(width, height)
      side <- max(w, h)
      
      # center of bbox
      cx <- (xmin + xmax) / 2
      cy <- (ymin + ymax) / 2
      
      # build square extent centered on MSA bbox center
      terra::ext(
        cx - side / 2, cx + side / 2,
        cy - side / 2, cy + side / 2
      )
    })
  }
  
  # 3. Crop to each MSA *square* bbox, keep only developed classes, save -----
  for (i in seq_along(msas)) {
    msa_name <- msas[i]
    msa_slug <- slugify(msa_name)
    
    # square extent for this MSA (same for all years)
    ext_sq <- msa_square_extents[[i]]
    
    # crop to square around MSA
    lc_crop <- terra::crop(r, ext_sq)
    
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
