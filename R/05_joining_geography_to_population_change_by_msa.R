library(tigris)
library(dplyr)
library(sf)
library(purrr)
library(here)

### load tract-level population data ----------------------
pop_data <- readRDS(
  here("data", "processed", "tracts_pop_2010geom_wide.rds")
)

### load 2010 tract shapes --------------------------------

# load states
states <- readRDS(here("data", "processed", "geographies", "state_abbr.rds"))

tracts_2010 <- map_dfr(
  states,
  ~ tracts(
    state = .x,
    year  = 2010,
    cb    = FALSE,   # detailed TIGER geometry; use TRUE for generalized
    class = "sf"
  )
)

### load counties ---------------------------------------
counties <- counties()

# select those intersecting msa
counties <- st_filter(counties, tracts_2010, .predicate = st_intersects)

# create list of county codes
county_code_list <- unique(counties$COUNTYFP)

### load water features ----------------------------------
state_lu <- fips_codes %>%
  select(state, state_code)%>%
  distinct()   # state_code is the 2-digit FIPS as a character

# 2) For your existing counties object, keep STATEFP & COUNTYFP
county_by_state <- counties %>%
  st_drop_geometry() %>%
  select(STATEFP, COUNTYFP) %>%
  distinct()

# Loop over states, then over that state's counties
water <- map_dfr(
  states,
  function(st_abbr) {
    message("State: ", st_abbr)
    
    # get this state's FIPS \
    st_fips <- state_lu %>%
      filter(state == st_abbr) %>%
      pull(state_code) %>%
      unique()
    
    if (length(st_fips) != 1) {
      warning("Could not find unique FIPS for state ", st_abbr)
      return(tibble())
    }
    
    # all county FIPS for this state
    this_ctys <- county_by_state %>%
      filter(STATEFP == st_fips) %>%
      pull(COUNTYFP)
    
    # loop over each county in this state
    map_dfr(
      this_ctys,
      function(cty_fp) {
        message("  County FIPS: ", cty_fp)
        area_water(
          state  = st_abbr,
          county = cty_fp,
          class  = "sf"
        )
      }
    )
  }
)

### load msa shapes --------------------------------------

msa_shapes <- readRDS(
  here("data", "processed", "geographies", "msa_geom.rds")
)

### select tracts intersecting msas ----------------------
tracts_2010_msa <- st_join(
  tracts_2010,
  msa_shapes,
  join = st_intersects,
  left = FALSE
)

# select only necessary variables
tracts_2010_msa <- tracts_2010_msa %>%
  select(GEOID10, MSA)

### crop out water extent -------------------------------

# Limit water to the MSA extent (speeds things up)
msa_union <- st_union(st_geometry(tracts_2010_msa))

# `water` is built by the county loop above. This previously read `water_msa`,
# an object that was never defined anywhere, so the script could not run.
water_in_msa <- st_intersection(water, msa_union)

water_union <- st_union(st_geometry(water_in_msa))

# 4) Subtract water from tracts -----------------------------------------
# Dots are sampled over tract geometry in script 06, so clipping water here
# keeps them off lakes and bays.
# Differencing the sf (not its bare geometry) keeps GEOID10/MSA attached to
# each row; tracts that were entirely water drop out, since there is nothing
# left to sample over.
st_agr(tracts_2010_msa) <- "constant"   # GEOID10/MSA hold for every piece
tracts_2010_msa <- st_difference(tracts_2010_msa, water_union) |>
  st_make_valid()
tracts_2010_msa <- tracts_2010_msa[!st_is_empty(tracts_2010_msa), ]

### join with population data ----------------------------

tracts_2010_msa_pop_data <- right_join(
  pop_data,
  tracts_2010_msa,
  by = "GEOID10"
)

### save pop data with geometry -------------------------
saveRDS(
  tracts_2010_msa_pop_data,
  here("data", "processed", "geographies", "tracts_2010_msa_pop_data.rds")
)