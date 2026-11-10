# A tiny stand-in for app_data: two MSAs, three cities, two years, two classes.
#   Alpha-Beta grows 1,000 -> 1,500; Gamma shrinks 2,000 -> 1,800.
#   Beta starts at population 0, so its percent change is undefined.

fixture_msas <- c("Alpha-Beta, XX Metro Area", "Gamma, YY Metro Area")

fixture_pop_dev <- function() {
  city <- tibble::tribble(
    ~MSA,            ~CITY_GEOID, ~city_name, ~city_rank, ~year, ~city_pop, ~msa_pop,
    fixture_msas[1], "001",       "Beta",     2L,         2010,  0,         1000,
    fixture_msas[1], "001",       "Beta",     2L,         2020,  100,       1500,
    fixture_msas[1], "002",       "Alpha",    1L,         2010,  600,       1000,
    fixture_msas[1], "002",       "Alpha",    1L,         2020,  900,       1500,
    fixture_msas[2], "003",       "Gamma",    1L,         2010,  1000,      2000,
    fixture_msas[2], "003",       "Gamma",    1L,         2020,  900,       1800
  )
  area <- tibble::tribble(
    ~CITY_GEOID, ~year, ~lc_class, ~city_area_mi2, ~msa_area_mi2,
    "001", 2010, 0L,  10, 100,
    "001", 2020, 0L,  15, 120,
    "001", 2010, 21L, 0,  50,
    "001", 2020, 21L, 5,  45,
    "002", 2010, 0L,  40, 100,
    "002", 2020, 0L,  50, 120,
    "002", 2010, 21L, 20, 50,
    "002", 2020, 21L, 20, 45,
    "003", 2010, 0L,  60, 200,
    "003", 2020, 0L,  62, 210,
    "003", 2010, 21L, 30, 80,
    "003", 2020, 21L, 31, 85
  )
  dplyr::left_join(city, area, by = c("CITY_GEOID", "year")) |>
    dplyr::mutate(CITY = city_name)
}

fixture_app_data <- function() {
  pop_dev <- fixture_pop_dev()
  square  <- function(x) st_polygon(list(rbind(c(x, 0), c(x + 1, 0), c(x + 1, 1), c(x, 1), c(x, 0))))

  msa_shapes <- st_sf(
    MSA      = fixture_msas,
    gif_file = c("alpha.gif", "gamma.gif"),
    geometry = st_sfc(square(0), square(5), crs = 4326)
  )
  st_agr(msa_shapes) <- "constant"

  list(
    msa_shapes  = msa_shapes,
    msa_centers = st_centroid(msa_shapes),
    city_shapes = st_sf(
      MSA = fixture_msas[c(1, 1, 2)], CITY_GEOID = c("001", "002", "003"),
      geometry = st_sfc(square(0), square(0.5), square(5), crs = 4326)
    ),
    pop_dev     = pop_dev,
    msa_pop_dev = pop_dev |>
      dplyr::distinct(MSA, year, msa_pop) |>
      dplyr::mutate(MSA_GEOID = "x", msa_area_mi2 = NA_real_),
    dots = st_sf(
      MSA        = fixture_msas[c(1, 1, 1, 2)],
      change_cat = c("Gain", "Gain", "Loss", "Loss"),
      geometry   = st_sfc(lapply(1:4, function(i) st_point(c(i, 0.5))), crs = 4326)
    ),
    ppd = 500
  )
}
