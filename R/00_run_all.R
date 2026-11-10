### run the full pipeline ------------------------------------------------------
#
# Builds every artifact the Shiny app reads, from raw inputs to finished GIFs.
# Run from the project root (here::here() anchors on msa-growth-map.Rproj).
#
#   Rscript R/00_run_all.R
#
# Requirements:
#   - CENSUS_API_KEY in ~/.Renviron (steps 01 and 04 hit the Census API)
#   - data/raw/landcover/Annual_NLCD_LndCov_<year>_CU_C1V1.tif for 1999-2024
#   - data/raw/crosswalks/nhgis_tr{2000,2020}_tr2010.csv
#
# Runtime warning: steps 02 and 08 are the raster steps and take hours.
# Steps 01, 03-07 and 09 are comparatively quick.
#
# Dependency order (why this sequence):
#   01 ─┬─> 02 ──> 03 ─┐
#       │              ├─> 07 combine   [app input]
#       └─> 04 ──> 05 ─┴─> 06 dots      [app input]
#            02 ─────────> 08 gifs      [app input]
#   06 + 07 + 08 ─────────> 09 app/data, app/www  (what the deployed app reads)

library(here)

steps <- c(
  "01_pull_data_for_nested_geographies.R",
  "02_load_transform_and_crop_lc_rasters.R",
  "03_calculate_annual_landcover_chg_by_metro.R",
  "04_loading_and_harmonizing_tract_data.R",
  "05_joining_geography_to_population_change_by_msa.R",
  "06_creating_dots_for_dot_density_map.R",
  "07_combine_development_and_population_data.R",
  "08_create_and_run_gifs.R",
  "09_build_app_data.R"
)

# check the key up front rather than hours into the run
source(here::here("R", "utils.R"))
invisible(census_key())

for (s in steps) {
  message("\n==================================================")
  message("RUNNING: ", s)
  message("==================================================")
  t0 <- Sys.time()
  source(here::here("R", s), echo = FALSE)
  message("DONE: ", s, " (", round(difftime(Sys.time(), t0, units = "mins"), 1), " min)")
}

message("\nPipeline complete. Launch the app with:")
message("  shiny::runApp(\"app\")")
