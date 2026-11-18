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
#   - data/raw/crosswalks/nhgis_blk{2000,2020}_bg2010.csv (NHGIS block -> 2010
#     block group crosswalks)
#   - internet access: step 05 downloads the Census county population files
#     into data/raw/census_popest/ on its first run
#
# Runtime warning: steps 02 and 09 are the raster steps and take hours, and
# step 04 (block-level Census pulls) about an hour on a first run.
# Steps 01, 03, 05-08 and 10 are comparatively quick.
#
# Dependency order (why this sequence):
#   01 ─┬─> 02 ──> 03 ──────────┐
#       │                       ├─> 08 combine   [app input: national map]
#       └─> 04 ─┬─> 05 popest ──┘
#               └─> 06 ──> 07 dots               [app input: dot map]
#            02 ──────────> 09 gifs              [app input: land cover]
#   07 + 08 + 09 ─────────> 10 app/data, app/www  (what the deployed app reads)
#   (05 reads step 04's cached 2000 census blocks for its Broomfield fill.)

library(here)

steps <- c(
  "01_pull_data_for_nested_geographies.R",
  "02_load_transform_and_crop_lc_rasters.R",
  "03_calculate_annual_landcover_chg_by_metro.R",
  "04_loading_and_harmonizing_block_group_data.R",
  "05_msa_population_from_county_estimates.R",
  "06_joining_geography_to_population_change_by_msa.R",
  "07_creating_dots_for_dot_density_map.R",
  "08_combine_development_and_population_data.R",
  "09_create_and_run_gifs.R",
  "10_build_app_data.R"
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
