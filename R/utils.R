### shared helpers -------------------------------------------------------------
# Sourced by the pipeline scripts (the app is self-contained and does not).
# Keep this file free of library() calls and side effects.

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
