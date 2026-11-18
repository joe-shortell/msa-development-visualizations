library(sf)
library(dplyr)
library(here)

source(here("R", "utils.R"))   # slugify(), msa_frame(), lc_years

### build the app's self-contained inputs ---------------------------------------
# The app is deployed as the app/ folder alone, so it cannot reach data/ or
# output/. This step writes everything it reads into app/:
#   app/data/app_data.rds                 one named list, already in EPSG:4326
#   app/www/frames/<slug>/<year>.jpg      each GIF from step 09 split into its
#                                         yearly images, served by Shiny at
#                                         frames/<slug>/<year>.jpg for the
#                                         app's year slider
#   app/www/basemap/relief.jpg            the national map's shaded relief, in
#                                         Albers, laid under its grey land
# It also does the joins and transforms the app used to redo on every launch.

### load ------------------------------------------------------------------------
msa_shapes <- readRDS(here("data", "processed", "geographies", "msa_geom.rds"))
pop_dev <- readRDS(
  here("data", "processed", "combined_population_development_data_lc_classes.rds")
)
dots <- readRDS(here("data", "processed", "dots_bg_change.rds"))
ppd  <- readRDS(here("data", "processed", "dots_ppd.rds"))

### geographies -----------------------------------------------------------------
# gif_file is the only place the app needs slugify(); computing it here keeps
# slugify() defined once, in R/utils.R.
msa_shapes <- msa_shapes |>
  st_transform(4326) |>
  mutate(
    gif_file  = paste0(slugify(MSA), "_developed.gif"),
    frame_dir = slugify(MSA)   # app/www/frames/<frame_dir>/<year>.jpg
  )

st_agr(msa_shapes) <- "constant"   # MSA attributes hold at the centroid
msa_centers <- st_centroid(msa_shapes)

# The MSA view's map frames, from msa_frame() (utils.R), so the app frames its
# dot map exactly as step 09 framed the GIF without its own copy of the rule:
#   frame_*  the view square both maps show (the dot map opens fitted to it)
#   dots_*   the 1.25x dot area steps 06-07 filled with dots (the dot map's
#            pan limit)
# Stored as lng/lat corners. A Web Mercator square is an exact lng/lat
# rectangle (Mercator's grid lines are meridians and parallels), so its
# bounding box loses nothing.
frame_corners <- function(frames, prefix) {
  bb <- t(vapply(seq_len(nrow(frames)), function(i) {
    as.vector(st_bbox(st_transform(frames[i, ], 4326)))
  }, numeric(4)))
  colnames(bb) <- paste0(prefix, c("lng1", "lat1", "lng2", "lat2"))
  as.data.frame(bb)
}
msa_shapes <- cbind(   # sf's cbind keeps the geometry; bind_cols() may not
  msa_shapes,
  frame_corners(msa_frame(msa_shapes), "frame_"),
  frame_corners(msa_frame(msa_shapes, scale = 1.25), "dots_")
)
st_agr(msa_shapes) <- "constant"

# Lower-48 state outlines: the national view's basemap.
# The national view is drawn in Albers Equal Area, where CARTO's Web Mercator
# tiles can't be shown, so the app draws these as vector polygons instead. The
# Census cartographic-boundary file at 1:20m is small and simplified enough for
# a national map; tigris caches the download.
state_shapes <- tigris::states(cb = TRUE, resolution = "20m", year = 2020, progress_bar = FALSE) |>
  filter(!STUSPS %in% c("AK", "HI", "PR", "VI", "GU", "MP", "AS", "DC")) |>
  select(STUSPS)

# Where each state's label sits: a point guaranteed to fall inside the state,
# found in Albers (EPSG:5070) so the result is planar-correct, stored as
# lng/lat columns for leaflet.
st_agr(state_shapes) <- "constant"
label_pts <- state_shapes |>
  st_transform(5070) |>
  st_point_on_surface() |>
  st_transform(4326) |>
  st_coordinates()

state_shapes <- state_shapes |>
  st_transform(4326) |>
  mutate(label_lng = label_pts[, "X"], label_lat = label_pts[, "Y"])

# Canada and Mexico as context land around the lower 48.
# Natural Earth's 1:50m "countries with lakes" layer has the Great Lakes cut
# out, matching the Census state outlines (which stop at the shoreline). It is
# cropped to a box around the lower 48 so the app doesn't carry all of Canada.
neighbor_shapes <- rnaturalearth::ne_download(
  scale = 50, type = "admin_0_countries_lakes", category = "cultural", returnclass = "sf"
) |>
  filter(ADMIN %in% c("Canada", "Mexico")) |>
  select(ADMIN) |>
  st_transform(4326)

sf_use_s2(FALSE)                       # planar crop on lng/lat is fine at this scale
neighbor_shapes <- suppressWarnings(
  st_crop(neighbor_shapes, xmin = -140, ymin = 12, xmax = -50, ymax = 62)
)
sf_use_s2(TRUE)

### shaded relief for the national map ---------------------------------------------
# Terrain under the national map's grey land, from Natural Earth's 1:10m "Gray
# Earth with Shaded Relief and Hypsography" (public domain, ~1.85 km per pixel,
# downloaded by hand into data/raw/natural_earth/GRAY_HR_SR/).
# The app lays this image over its grey land polygons with a multiply blend
# (basemap_css() in app/R/mod_map.R), so it can only darken: mid-greys shade
# the land, and white leaves what's beneath unchanged.
#   1. Crop to the same lng/lat box as neighbor_shapes, the land the map draws.
#   2. Set everything that isn't land (oceans, the Gulf, the Great Lakes, which
#      both shape layers cut out) to white, so the relief never tints water.
#   3. Project to Albers (EPSG:5070, the national map's CRS) at 2 km per pixel,
#      about one pixel per screen pixel at the map's opening zoom.
#   4. Save as a greyscale JPEG in app/www/basemap/, served at basemap/relief.jpg.
# relief_bounds holds the image's corners in EPSG:5070 metres (xmin, ymin,
# xmax, ymax). Unlike the rest of app_data it isn't lng/lat, because the app
# places it with Proj4Leaflet's imageOverlay, which takes projected corners.
relief_src <- here("data", "raw", "natural_earth", "GRAY_HR_SR", "GRAY_HR_SR.tif")
relief <- terra::crop(terra::rast(relief_src), terra::ext(-140, -50, 12, 62))

land <- terra::vect(st_sf(geometry = c(st_geometry(state_shapes), st_geometry(neighbor_shapes))))
relief <- terra::mask(relief, land, updatevalue = 255)

relief <- terra::project(relief, "EPSG:5070", res = 2000, method = "bilinear")
relief_bounds <- as.vector(terra::ext(relief))[c("xmin", "ymin", "xmax", "ymax")]

# The projected box's curved edges fall outside the source (NA): white, too.
relief_px <- terra::as.matrix(relief, wide = TRUE)
relief_px[is.na(relief_px)] <- 255
relief_px <- pmin(pmax(relief_px, 0), 255) / 255

dir.create(here("app", "www", "basemap"), showWarnings = FALSE, recursive = TRUE)
magick::image_read(as.raster(relief_px)) |>
  magick::image_convert(colorspace = "gray") |>
  magick::image_write(here("app", "www", "basemap", "relief.jpg"), format = "jpeg", quality = 85)

### tables ----------------------------------------------------------------------
# Population by MSA and year, for the national map, and the source of its year
# slider's range (1999-2024). pop_dev repeats each MSA-year's population on
# every land class row, so this keeps one row per MSA-year.
msa_pop_dev <- pop_dev |>
  distinct(MSA_GEOID, MSA, year, msa_pop)

dots <- dots |>
  st_transform(4326) |>
  select(MSA, change_cat)

### write -----------------------------------------------------------------------
dir.create(here("app", "data"), showWarnings = FALSE, recursive = TRUE)
saveRDS(
  list(
    msa_shapes  = msa_shapes,
    msa_centers = msa_centers,
    state_shapes = state_shapes,
    neighbor_shapes = neighbor_shapes,
    relief_bounds = relief_bounds,   # EPSG:5070 metres, not lng/lat (see above)
    pop_dev     = pop_dev,
    msa_pop_dev = msa_pop_dev,
    dots        = dots,
    ppd         = ppd,
    lc_years    = lc_years
  ),
  here("app", "data", "app_data.rds")
)

### yearly land cover images ------------------------------------------------------
# Split each MSA's GIF into one JPEG per year, so the app's year slider can
# show any year (a GIF can only play through on its own).
# image_coalesce() turns each GIF frame into a complete picture (GIF frames
# may store only what changed since the previous one). Frame i is year
# lc_years[i], the order step 09 rendered them in; a GIF with a different
# number of frames is skipped with a warning rather than mislabelled.
# JPEG at quality 85 is ~210 KB a frame (~5 MB per MSA). Each MSA's folder is
# cleared first so no frame from an older run survives.
gifs <- here("output", "gifs", msa_shapes$gif_file)
missing <- !file.exists(gifs)
if (any(missing)) {
  warning("No GIF for: ", paste(msa_shapes$MSA[missing], collapse = "; "))
}

# Remove the thin light line along each image's edges.
# Step 09 draws every frame on a white background with the land cover ending
# exactly at the frame's edge, so the outermost pixels are only partly covered
# and white bleeds into them; after gifski shrinks the frame to 800 px this
# leaves a 1-pixel light line down the left and right edges (and in some MSAs
# along the top), plus a light seam where the map meets the grey ribbon. How
# visible it is depends on the land colour at that edge.
# Each of those lines is overwritten with its inner neighbour, so the image
# keeps its exact size and framing (it still lines up with the dot map).
# The map is the square at the top (as tall as the image is wide); the ribbon
# is below it, and the seam is that square's last row and the ribbon's first.
repair_frame_edges <- function(frame) {
  px    <- magick::image_data(frame, "rgb")   # raw array: channel x column x row
  w     <- dim(px)[2]
  map_h <- w
  map   <- seq_len(map_h)
  px[, 1, map] <- px[, 2, map]                # left edge
  px[, w, map] <- px[, w - 1, map]            # right edge
  px[, , 1]    <- px[, , 2]                   # top edge
  px[, , map_h]     <- px[, , map_h - 1]      # seam: last map row <- the map above it
  px[, , map_h + 1] <- px[, , map_h + 2]      # seam: first ribbon row <- the ribbon below it
  magick::image_read(px)
}

frames_root <- here("app", "www", "frames")
for (i in which(!missing)) {
  frames <- magick::image_coalesce(magick::image_read(gifs[i]))
  if (length(frames) != length(lc_years)) {
    warning(msa_shapes$MSA[i], ": GIF has ", length(frames), " frames, expected ",
            length(lc_years), " (", min(lc_years), "-", max(lc_years), "); skipped")
    next
  }
  out_dir <- file.path(frames_root, msa_shapes$frame_dir[i])
  unlink(out_dir, recursive = TRUE)
  dir.create(out_dir, recursive = TRUE)
  for (j in seq_along(lc_years)) {
    magick::image_write(repair_frame_edges(frames[j]), file.path(out_dir, paste0(lc_years[j], ".jpg")),
                        format = "jpeg", quality = 85)
  }
}

# The app used to show the GIFs themselves from app/www/gifs/. Remove that
# regenerable copy so it isn't deployed alongside the frames.
unlink(here("app", "www", "gifs"), recursive = TRUE)

# Check each GIF was drawn in the frame the app's dot map opens on.
# Step 09 writes the frame it drew each GIF in to <gif>_frame.csv; a GIF with
# no such file, or a different frame, predates the current msa_frame() rule
# and won't line up with the dot map beside it. Warn rather than stop, so the
# app still gets the GIFs there are; re-running 08 for those MSAs fixes it.
frames_now <- msa_frame(readRDS(here("data", "processed", "geographies", "msa_geom.rds")))
stale <- vapply(seq_len(nrow(msa_shapes)), function(i) {
  stamp <- sub("\\.gif$", "_frame.csv", gifs[i])
  if (missing[i] || !file.exists(stamp)) return(TRUE)
  drawn <- unlist(read.csv(stamp)[c("xmin", "ymin", "xmax", "ymax")])
  now   <- st_bbox(frames_now[frames_now$MSA == msa_shapes$MSA[i], ])[c("xmin", "ymin", "xmax", "ymax")]
  any(abs(drawn - now) > 1)   # metres
}, logical(1))
if (any(stale)) {
  warning(
    "These GIFs were not drawn in the current view frame and won't line up ",
    "with the dot map; re-run step 09 for: ",
    paste(msa_shapes$MSA[stale], collapse = "; ")
  )
}
