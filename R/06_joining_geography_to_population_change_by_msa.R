library(tigris)
library(dplyr)
library(sf)
library(purrr)
library(here)

source(here("R", "utils.R"))   # msa_frame(), frame_states()

### load block-group population data ----------------------
# from step 04: one row per 2010 block group, keyed by GEOID10
pop_data <- readRDS(
  here("data", "processed", "bg_pop_2010geom_wide.rds")
)

### dot areas ---------------------------------------------
# Each MSA's dot map shows every block group in view, not only the MSA's own,
# because the land-use GIF beside it shows all developed land in the frame.
# The "dot area" is the shared view frame enlarged 1.25x (msa_frame(),
# utils.R): it covers the app's dot-map panel with room to pan.
msa_shapes <- readRDS(
  here("data", "processed", "geographies", "msa_geom.rds")
)
dot_areas <- msa_frame(msa_shapes, scale = 1.25)

### load 2010 block group shapes --------------------------

# every state any dot area touches
states <- frame_states(dot_areas)

# The dots are counted and placed by 2010 block group (step 04 explains why).
bgs_2010 <- map_dfr(
  states,
  ~ block_groups(
    state = .x,
    year  = 2010,
    cb    = FALSE,   # detailed TIGER geometry; use TRUE for generalized
    class = "sf"
  )
) |>
  select(GEOID10)

dot_areas <- st_transform(dot_areas, st_crs(bgs_2010))

### load counties ---------------------------------------
counties <- counties()

# Only counties inside some dot area need water features. (This used to keep
# every county touching any block group, i.e. every county in every state, and so
# fetched far more water than any map shows.)
counties <- st_filter(counties, st_transform(dot_areas, st_crs(counties)), .predicate = st_intersects)

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

### select block groups in each MSA's dot area -----------
# One row per block group per dot area it touches. MSA here means "shown in
# this MSA's view", not membership: a block group outside the MSA, or in two metros'
# views, appears once for each view. Step 07 samples each row separately and
# the app filters dots by MSA, so each view gets its own complete set of dots.
bgs_2010_msa <- st_join(
  bgs_2010,
  dot_areas,
  join = st_intersects,
  left = FALSE
)

# select only necessary variables
bgs_2010_msa <- bgs_2010_msa %>%
  select(GEOID10, MSA)

### crop out water extent -------------------------------

# Limit water to the block groups in view (speeds things up)
msa_union <- st_union(st_geometry(bgs_2010_msa))

# `water` is built by the county loop above. This previously read `water_msa`,
# an object that was never defined anywhere, so the script could not run.
#
# Repair water geometry before and after clipping. Some TIGER water polygons
# have duplicate vertices; on lon/lat data sf uses s2, which rejects those
# ("Edge N is degenerate") and halts st_union(). st_make_valid() rebuilds
# them without s2's validity check, so the union below can proceed.
# Repairs and intersections can return GeometryCollections mixing polygons
# with stray lines/points, which s2's union rejects ("multidimensional
# collections not implemented"); only the polygon parts matter for clipping,
# so st_collection_extract() keeps those.
water <- st_make_valid(water)
water_in_msa <- st_intersection(water, msa_union) |>
  st_make_valid() |>
  st_collection_extract("POLYGON")
water_in_msa <- water_in_msa[!st_is_empty(water_in_msa), ]

water_union <- st_union(st_geometry(water_in_msa))

# 4) Subtract water from block groups --------------------------------------
# Dots are sampled over block-group geometry in script 07, so clipping water
# here keeps them off lakes and bays.
# Differencing the sf (not its bare geometry) keeps GEOID10/MSA attached to
# each row; block groups that were entirely water drop out, since there is nothing
# left to sample over.
st_agr(bgs_2010_msa) <- "constant"   # GEOID10/MSA hold for every piece
bgs_2010_msa <- st_difference(bgs_2010_msa, water_union) |>
  st_make_valid() |>
  st_collection_extract("POLYGON")   # same reason as above; 07 samples areas
bgs_2010_msa <- bgs_2010_msa[!st_is_empty(bgs_2010_msa), ]

### join with population data ----------------------------

# Keep the sf object on the left so the result stays sf with its CRS. With the
# plain pop_data table first (the earlier right_join), dplyr returned a tibble;
# script 07 then read st_crs() of that tibble as NA and stamped the dots with
# no CRS, which failed st_transform() in script 10. Same rows either way: every
# block-group piece, with population columns where GEOID10 matches.
bgs_2010_msa_pop_data <- left_join(
  bgs_2010_msa,
  pop_data,
  by = "GEOID10"
)

### save pop data with geometry -------------------------
saveRDS(
  bgs_2010_msa_pop_data,
  here("data", "processed", "geographies", "bg_2010_msa_pop_data.rds")
)