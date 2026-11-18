library(tidycensus)
library(dplyr)
library(sf)
library(here)

options(tigris_use_cache = TRUE)

# The MSA pull needs a Census API key: CENSUS_API_KEY in ~/.Renviron (see utils.R)
source(here::here("R", "utils.R"))
tidycensus::census_api_key(census_key(), install = FALSE)

### the 30 MSAs and their outlines -------------------------------------------------
# Chooses the project's MSAs, the 30 largest by population, and saves their
# outlines, which every later step reads.
#
# Ranked by the ACS 1-year total population (B01003_001) for geometry_year, the
# latest year when this was run; the outlines come with the same pull. This
# ranking is the only use of the ACS: the populations shown in the app come
# from step 05 (county estimates) and steps 04/06 (census counts).
#
# Output: data/processed/geographies/msa_geom.rds, one row per MSA:
#   MSA_GEOID (the CBSA code), MSA (its name, e.g. "Austin-Round Rock-San
#   Marcos, TX Metro Area"; slugify(MSA) names its folders and files), geometry
geometry_year <- 2024

msa_geom <- get_acs(
  geography = "cbsa",
  variables = "B01003_001",   # total population
  year      = geometry_year,
  survey    = "acs1",
  geometry  = TRUE,
  output    = "wide"
) |>
  arrange(desc(B01003_001E)) |>
  slice_head(n = 30) |>
  select(MSA_GEOID = GEOID, MSA = NAME, geometry)

dir.create(here("data", "processed", "geographies"), showWarnings = FALSE, recursive = TRUE)
saveRDS(msa_geom, here("data", "processed", "geographies", "msa_geom.rds"))
