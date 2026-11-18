### shared helpers -------------------------------------------------------------
# Sourced by the pipeline scripts (the app is self-contained and does not).
# Keep this file free of library() calls and side effects.

# Land cover years: one GIF frame per year, in this order.
# Step 09 renders a frame for each and step 10 splits the GIFs back into
# yearly images for the app's year slider, so frame i is always lc_years[i].
lc_years <- 1999:2024

# Convert an MSA name to a folder/file-safe slug.
#
# This is the de-facto join key between three things:
#   - the MSA's NAME string in msa_geom.rds
#   - its raster folder, data/processed/landcover_crops/<slug>/
#   - its animation,    output/gifs/<slug>_developed.gif
#
# It was previously redefined identically in four files. Changing it here
# changes all three at once, which is the point — divergent copies silently
# break raster lookup and the app's GIF panel.
slugify <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

# The map frame for each MSA's view, as a square in Web Mercator.
#
# One framing rule shared by the time-lapse GIF (steps 02, 09), the dots
# (steps 04, 06, 07) and the app's dot map (stored by step 10), so the two maps in
# the MSA view show the same area in the same projection.
#
# The square is centred on the MSA's centroid, computed exactly as step 10
# computes the app's circles (st_centroid() on lng/lat), so the circle that was
# clicked sits in the middle of both maps. Its half-side is the largest
# distance from that centre to any edge of the MSA's bounding box, measured in
# EPSG:3857, so the whole MSA is in view. `scale` enlarges the square about the
# same centre: steps 04, 06 and 07 use scale = 1.25 as the "dot area", which covers the
# app's slightly taller dot-map panel with room to pan.
#
# Returns an sf of square polygons in EPSG:3857 with an MSA column, one row per
# MSA, in the input's order.
msa_frame <- function(msa_geom, scale = 1) {
  ll <- sf::st_transform(msa_geom, 4326)
  sf::st_agr(ll) <- "constant"
  centre <- sf::st_coordinates(sf::st_transform(sf::st_centroid(sf::st_geometry(ll)), 3857))
  merc   <- sf::st_transform(sf::st_geometry(ll), 3857)

  squares <- lapply(seq_along(merc), function(i) {
    bb   <- sf::st_bbox(merc[i])
    cx   <- centre[i, "X"]
    cy   <- centre[i, "Y"]
    half <- scale * max(cx - bb[["xmin"]], bb[["xmax"]] - cx, cy - bb[["ymin"]], bb[["ymax"]] - cy)
    sf::st_polygon(list(rbind(
      c(cx - half, cy - half), c(cx + half, cy - half),
      c(cx + half, cy + half), c(cx - half, cy + half),
      c(cx - half, cy - half)
    )))
  })
  sf::st_sf(MSA = msa_geom$MSA, geometry = sf::st_sfc(squares, crs = 3857))
}

# Postal codes of the states that any of `frames` touches.
#
# Step 06 pulls block-group shapes state by state. Choosing the states from the
# frames, rather than from each MSA's principal cities, covers every block
# group the dot map can show, including those outside the MSA and in
# states no principal city is in (e.g. the Wisconsin side of Minneapolis).
frame_states <- function(frames) {
  states <- tigris::states(cb = TRUE, year = 2020, progress_bar = FALSE)
  states <- sf::st_transform(states, sf::st_crs(frames))
  hit    <- lengths(sf::st_intersects(states, sf::st_union(frames))) > 0
  sort(states$STUSPS[hit])
}

# Census API key, from CENSUS_API_KEY in ~/.Renviron (kept out of the repo).
#
# Deliberately not a project-level .Renviron: R reads only one user .Renviron,
# and one in the project root would shadow ~/.Renviron (which sets PROJ_LIB
# for the raster steps). Fails early with instructions rather than letting a
# Census API call fail mid-run with "Missing Key".
census_key <- function() {
  key <- Sys.getenv("CENSUS_API_KEY")
  if (!nzchar(key)) {
    stop(
      "CENSUS_API_KEY is not set. Add your key to the CENSUS_API_KEY= line in\n",
      "  ~/.Renviron (usethis::edit_r_environ() opens it), then restart R.",
      call. = FALSE
    )
  }
  key
}
