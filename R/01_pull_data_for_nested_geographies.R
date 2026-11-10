library(here)
library(ggspatial)
library(scales)
library(tigris)
library(tidycensus)
library(mapview)
library(purrr)
library(dplyr)
library(stringr)
library(tibble)
library(sf)

options(tigris_use_cache = TRUE)

# ACS pulls need a Census API key: CENSUS_API_KEY in ~/.Renviron (see utils.R)
source(here::here("R", "utils.R"))
tidycensus::census_api_key(census_key(), install = FALSE)

#--------------------------------------------------
# PARAMETERS
#--------------------------------------------------
years         <- c(2005:2019, 2021:2024)   # skip 2020
geometry_year <- max(years)                # keep geometry from most recent year

#--------------------------------------------------
# 1. MSA GEOMETRY + TOP 30 MSA SET (ANCHOR BY GEOID)
#--------------------------------------------------
msa_geom_full <- get_acs(
  geography = "cbsa",
  variables = "B01003_001",   # total population
  year      = geometry_year,
  survey    = "acs1",
  geometry  = TRUE,
  output    = "wide"
)

msa_top <- msa_geom_full %>%
  dplyr::arrange(dplyr::desc(B01003_001E)) %>%
  dplyr::slice_head(n = 30) %>%
  dplyr::rename(
    MSA     = NAME,
    msa_pop = B01003_001E
  ) %>%
  dplyr::mutate(
    MSA_GEOID = GEOID,
    # parse city + state parts once for matching
    msa_clean      = stringr::str_replace(MSA, "\\s+(Metro|Micro) Area$", ""),
    msa_city_part  = stringr::str_split_fixed(msa_clean, ",\\s*", 2)[, 1],
    msa_state_part = stringr::str_split_fixed(msa_clean, ",\\s*", 2)[, 2]
  )

msa_codes <- msa_top$MSA_GEOID

# keep only geometry + ids
msa_geom <- msa_top %>%
  dplyr::select(MSA_GEOID, MSA, geometry)

#--------------------------------------------------
# 2. PLACE GEOMETRY (LATEST YEAR) + CITY/STATE PARSING
#--------------------------------------------------
places_geom_year <- get_acs(
  geography = "place",
  variables = "B01003_001",
  year      = geometry_year,
  survey    = "acs1",
  geometry  = TRUE,
  output    = "wide"
)

# build state name → state abbr lookup (includes DC, PR)
state_lookup <- tibble::tibble(
  state_name = c(state.name, "District of Columbia", "Puerto Rico"),
  state_abbr = c(state.abb, "DC", "PR")
)

# Census names end in a lowercase legal/statistical descriptor:
# "Arlington CDP", "Boise City city", "Lexington-Fayette urban county",
# "Nashville-Davidson metropolitan government (balance)"
place_base_name <- function(name) {
  sub("(\\s+([a-z]+|CDP|\\(balance\\)))+$", "", sub(",.*$", "", name))
}

places_geom_year <- places_geom_year %>%
  dplyr::mutate(
    city_name  = place_base_name(NAME),
    state_name = sub(".*,\\s*", "", NAME)
  ) %>%
  dplyr::left_join(state_lookup, by = "state_name")

#--------------------------------------------------
# 3. BUILD "KEY CITIES" PER MSA USING CITY + STATE
#--------------------------------------------------
# Places are matched against the title's city part ("Portland-Vancouver-Hillsboro")
# within the title's states ("OR-WA"), so twins like Portland, ME vs Portland, OR
# are disambiguated by state.

# A place matches if its name is a "-"-delimited run of the title, so
# hyphenated names ("Winston-Salem") match whole. OMB writes "--" between
# such names; collapsing it puts every title on one format.
title_position <- function(place, title_cities) {
  title <- paste0("-", gsub("--", "-", title_cities), "-")
  stringr::str_locate(title, stringr::fixed(paste0("-", place, "-")))
}

match_title_cities <- function(places, title_cities, msa) {
  pos  <- title_position(places$city_name, title_cities)
  keep <- !is.na(pos[, "start"])
  places <- places[keep, ]
  pos    <- pos[keep, , drop = FALSE]

  # a match inside a longer one ("Salem" within "Winston-Salem") is not a principal city
  len    <- pos[, "end"] - pos[, "start"]
  nested <- vapply(seq_len(nrow(pos)), function(i) {
    any(pos[, "start"] <= pos[i, "start"] & pos[, "end"] >= pos[i, "end"] & len > len[i])
  }, logical(1))
  places <- places[!nested, ]
  pos    <- pos[!nested, , drop = FALSE]

  title    <- paste0("-", gsub("--", "-", title_cities), "-")
  segments <- stringr::str_locate_all(title, "[^-]+")[[1]]
  covered  <- vapply(seq_len(nrow(segments)), function(j) {
    any(pos[, "start"] < segments[j, "start"] & pos[, "end"] > segments[j, "end"])
  }, logical(1))
  if (!all(covered)) {
    warning(
      msa, ": no ACS 1-year place matched ",
      paste(stringr::str_sub(title, segments[!covered, "start"], segments[!covered, "end"]), collapse = ", "),
      call. = FALSE
    )
  }

  places %>%
    dplyr::mutate(city_rank = rank(pos[, "start"], ties.method = "min"))
}

key_cities_geom_year <- purrr::map_dfr(
  seq_len(nrow(msa_top)),
  function(i) {
    row_i <- msa_top[i, ]

    state_tokens <- row_i$msa_state_part %>%
      stringr::str_split("-", simplify = TRUE) %>%
      as.character() %>%
      stringr::str_trim()

    places_geom_year %>%
      dplyr::filter(state_abbr %in% state_tokens) %>%
      match_title_cities(row_i$msa_city_part, row_i$MSA) %>%
      dplyr::mutate(
        MSA_GEOID = row_i$MSA_GEOID,
        MSA       = row_i$MSA
      )
  }
) %>%
  dplyr::distinct(MSA_GEOID, MSA, GEOID, NAME, city_name, state_abbr, .keep_all = TRUE) %>%
  dplyr::rename(
    CITY_GEOID = GEOID,
    CITY       = NAME,
    city_pop   = B01003_001E
  )

# geometry-only version for saving
city_geom <- key_cities_geom_year %>%
  dplyr::select(MSA_GEOID, MSA, CITY_GEOID, CITY, geometry)

# meta (no geometry) for joining later
city_meta <- key_cities_geom_year %>%
  sf::st_drop_geometry() %>%
  dplyr::select(MSA_GEOID, MSA, CITY_GEOID, CITY, city_name, city_rank, state_abbr)

city_codes <- unique(city_meta$CITY_GEOID)

# optional state list (if you still want it)
state_abbr_vec <- sort(unique(city_meta$state_abbr))

#--------------------------------------------------
# 4. FUNCTIONS: PULL ACS1 BY YEAR, USING GEOIDs
#--------------------------------------------------
get_msa_acs1 <- function(y) {
  message("Pulling MSA data for year ", y, " ...")
  
  acs <- get_acs(
    geography = "cbsa",
    variables = "B01003_001",
    year      = y,
    survey    = "acs1",
    geometry  = FALSE,
    output    = "wide"
  )
  
  acs %>%
    dplyr::filter(GEOID %in% msa_codes) %>%
    dplyr::transmute(
      MSA_GEOID = GEOID,
      year      = y,
      msa_pop   = B01003_001E
    )
}

get_city_acs1 <- function(y) {
  message("Pulling city data for year ", y, " ...")
  
  acs <- get_acs(
    geography = "place",
    variables = "B01003_001",
    year      = y,
    survey    = "acs1",
    geometry  = FALSE,
    output    = "wide"
  )
  
  acs %>%
    dplyr::filter(GEOID %in% city_codes) %>%
    dplyr::transmute(
      CITY_GEOID = GEOID,
      year       = y,
      city_pop   = B01003_001E
    )
}

#--------------------------------------------------
# 5. BUILD LONG PANEL FOR 2005–2019, 2021–2024
#--------------------------------------------------
msa_long  <- purrr::map_dfr(years, get_msa_acs1)
city_long <- purrr::map_dfr(years, get_city_acs1)

# glue on names / metadata
msa_city_long <- city_long %>%
  dplyr::left_join(city_meta, by = "CITY_GEOID") %>%
  dplyr::left_join(msa_long,  by = c("MSA_GEOID", "year")) %>%
  dplyr::select(
    MSA_GEOID, MSA,
    CITY_GEOID, CITY, city_name, city_rank, state_abbr,
    year,
    msa_pop, city_pop
  ) %>%
  dplyr::arrange(MSA, CITY, year)

#--------------------------------------------------
# 6. SAVE OUTPUTS
#--------------------------------------------------
# time-series df
saveRDS(
  msa_city_long,
  here::here("data", "processed", "acs_data", "msa_city_acs1_2005_2019_2021_2024.rds")
)

# geometries (single-year)
saveRDS(
  msa_geom,
  here::here("data", "processed", "geographies", "msa_geom.rds")
)

saveRDS(
  city_geom,
  here::here("data", "processed", "geographies", "city_geom.rds")
)

# state list if useful elsewhere
saveRDS(
  state_abbr_vec,
  here::here("data", "processed", "geographies", "state_abbr.rds")
)
