library(tidycensus)
library(dplyr)
library(purrr)
library(readr)
library(sf)
library(here)

source(here("R", "utils.R"))   # census_key(), msa_frame()

### population change by 2010 block group ---------------------------------------
# The dot map places one dot per 500 people gained or lost, counted and placed
# by 2010 block group (finer than tracts, coarse enough that 2020's privacy
# noise and dot rounding stay small).
#
# Block-group boundaries changed between censuses, so 2000 and 2020 counts are
# pulled by block (the smallest unit) and summed into 2010 block groups with
# NHGIS crosswalks, whose `weight` is the expected share of each block's
# population in each 2010 block group. 2010 counts are pulled by block group
# directly. Output: one row per 2010 block group with pop_2000/2010/2020 and
# the changes between them.
#
# This is the slow step: the Census API serves blocks one county at a time, so
# it makes ~1,000 county requests per year (about an hour in all). Each
# state-year is cached under data/raw/census_blocks/, so a re-run (or a run
# that stopped part way) only fetches what is missing.

census_api_key(census_key(), install = FALSE)

### which block groups are needed -----------------------------------------------
# Every 2010 block group in a county that touches some MSA's dot area (the
# area its dot map can show, msa_frame(scale = 1.25) in utils.R). Block groups
# nest in counties, so the 2010 counties overlapping the dot areas are enough.
msa_geom  <- read_rds(here("data", "processed", "geographies", "msa_geom.rds"))
dot_areas <- msa_frame(msa_geom, scale = 1.25)

counties_2010 <- tigris::counties(cb = TRUE, year = 2010, progress_bar = FALSE)
counties_2010 <- counties_2010 |>
  mutate(county = paste0(STATE, COUNTY)) |>
  st_filter(st_transform(dot_areas, st_crs(counties_2010)), .predicate = st_intersects)
target_counties <- sort(unique(counties_2010$county))   # 5-digit state+county FIPS
message(length(target_counties), " counties hold the block groups in view")

### crosswalks ---------------------------------------------------------------------
# Only the rows feeding the needed block groups are kept. Their source blocks
# then say exactly which 2000/2020 counties to pull: a block near a county line
# can belong to a 2010 block group across it, and a few counties were renamed
# or merged between censuses, so the 2010 county list alone would miss some.
read_block_crosswalk <- function(file, src_col) {
  cw <- read_csv(
    here("data", "raw", "crosswalks", file),
    col_select = all_of(c(src_col, "bg2010ge", "weight")),
    col_types  = cols(.default = col_character(), weight = col_double()),
    progress   = FALSE
  )
  cw |>
    rename(block = all_of(src_col), GEOID10 = bg2010ge) |>
    filter(weight > 0, substr(GEOID10, 1, 5) %in% target_counties)
}
cw00_10 <- read_block_crosswalk("nhgis_blk2000_bg2010.csv", "blk2000ge")
cw20_10 <- read_block_crosswalk("nhgis_blk2020_bg2010.csv", "blk2020ge")

source_counties <- list(
  "2000" = sort(unique(substr(cw00_10$block, 1, 5))),
  "2010" = target_counties,
  "2020" = sort(unique(substr(cw20_10$block, 1, 5)))
)

### pull from the Census API ------------------------------------------------------
pulls <- list(
  "2000" = list(geography = "block",       variables = "P001001", sumfile = "sf1"),
  "2010" = list(geography = "block group", variables = "P001001", sumfile = "sf1"),
  "2020" = list(geography = "block",       variables = "P1_001N", sumfile = "pl")
)

# Retry a request a few times, then stop. The tract version returned an empty
# table on failure, which with ~1,000 requests could leave unnoticed holes in
# the map; stopping keeps the cache of everything fetched so far, so the
# re-run picks up from the failed state.
get_decennial_retry <- function(..., tries = 3) {
  for (attempt in seq_len(tries)) {
    out <- tryCatch(get_decennial(...), error = function(e) e)
    if (!inherits(out, "error")) return(out)
    message("   attempt ", attempt, " failed: ", conditionMessage(out))
    Sys.sleep(5 * attempt)
  }
  stop("Census API request failed after ", tries, " attempts")
}

# One cached file per year and state: GEOID + population.
cache_dir <- here("data", "raw", "census_blocks")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

pull_year <- function(year) {
  spec     <- pulls[[year]]
  counties <- source_counties[[year]]
  by_state <- split(substr(counties, 3, 5), substr(counties, 1, 2))

  imap_dfr(by_state, function(ctys, st_fips) {
    cache <- file.path(cache_dir, paste0(year, "_", st_fips, ".rds"))
    if (file.exists(cache)) return(read_rds(cache))

    message(year, " ", spec$geography, "s: state ", st_fips, " (", length(ctys), " counties)")
    out <- get_decennial_retry(
      geography = spec$geography,
      variables = spec$variables,
      year      = as.integer(year),
      sumfile   = spec$sumfile,
      state     = st_fips,
      county    = ctys,
      geometry  = FALSE
    ) |>
      select(GEOID, pop = value)
    write_rds(out, cache)
    out
  })
}

blocks_2000 <- pull_year("2000")
bg_2010     <- pull_year("2010")
blocks_2020 <- pull_year("2020")

### harmonise to 2010 block groups -------------------------------------------------
# Each block's count times its weight, summed by 2010 block group. Blocks that
# feed only block groups outside the view were pulled (their county was
# needed) but drop out here.
to_bg2010 <- function(blocks, cw, name) {
  stopifnot(all(cw$block %in% blocks$GEOID))   # every needed block was pulled
  cw |>
    inner_join(blocks, by = c("block" = "GEOID")) |>
    group_by(GEOID10) |>
    summarise("{name}" := sum(pop * weight), .groups = "drop")
}

bg_pop_2010geom_wide <- bg_2010 |>
  select(GEOID10 = GEOID, pop_2010 = pop) |>
  filter(substr(GEOID10, 1, 5) %in% target_counties) |>
  full_join(to_bg2010(blocks_2000, cw00_10, "pop_2000"), by = "GEOID10") |>
  full_join(to_bg2010(blocks_2020, cw20_10, "pop_2020"), by = "GEOID10") |>
  mutate(
    # a block group with no people in a year has no crosswalk rows: count 0
    across(c(pop_2000, pop_2010, pop_2020), ~ coalesce(.x, 0)),
    chg_2000_2010 = pop_2010 - pop_2000,
    chg_2010_2020 = pop_2020 - pop_2010,
    chg_2000_2020 = pop_2020 - pop_2000
  )

### save -------------------------------------------------------------------------
saveRDS(
  bg_pop_2010geom_wide,
  here("data", "processed", "bg_pop_2010geom_wide.rds")
)
