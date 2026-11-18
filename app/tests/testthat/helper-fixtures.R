# A tiny stand-in for app_data: two MSAs, two years, two land classes.
#   Alpha-Beta grows 1,000 -> 1,500; Gamma shrinks 2,000 -> 1,800.

fixture_msas <- c("Alpha-Beta, XX Metro Area", "Gamma, YY Metro Area")

# Like step 08's pop_dev: one row per MSA x year x land class, with the MSA's
# population repeated on each class row.
fixture_pop_dev <- tibble::tribble(
  ~MSA,            ~year, ~lc_class, ~msa_pop, ~msa_area_mi2,
  fixture_msas[1], 2010,  0L,        1000,     100,
  fixture_msas[1], 2020,  0L,        1500,     120,
  fixture_msas[1], 2010,  21L,       1000,     50,
  fixture_msas[1], 2020,  21L,       1500,     45,
  fixture_msas[2], 2010,  0L,        2000,     200,
  fixture_msas[2], 2020,  0L,        1800,     210,
  fixture_msas[2], 2010,  21L,       2000,     80,
  fixture_msas[2], 2020,  21L,       1800,     85
) |>
  dplyr::mutate(MSA_GEOID = "x", .before = 1)

fixture_app_data <- function() {
  pop_dev <- fixture_pop_dev
  square  <- function(x) st_polygon(list(rbind(c(x, 0), c(x + 1, 0), c(x + 1, 1), c(x, 1), c(x, 0))))

  msa_shapes <- st_sf(
    MSA      = fixture_msas,
    gif_file  = c("alpha.gif", "gamma.gif"),
    frame_dir = c("alpha", "gamma"),   # no images exist for these under www/frames
    # view frame and dot area corners, as step 10 stores them from msa_frame()
    frame_lng1 = c(-0.1, 4.9), frame_lat1 = -0.1, frame_lng2 = c(1.1, 6.1), frame_lat2 = 1.1,
    dots_lng1  = c(-0.25, 4.75), dots_lat1 = -0.25, dots_lng2 = c(1.25, 6.25), dots_lat2 = 1.25,
    geometry = st_sfc(square(0), square(5), crs = 4326)
  )
  st_agr(msa_shapes) <- "constant"

  list(
    msa_shapes  = msa_shapes,
    msa_centers = st_centroid(msa_shapes),
    state_shapes = st_sf(
      STUSPS = c("XX", "YY"), label_lng = c(0.5, 5.5), label_lat = c(0.5, 0.5),
      geometry = st_sfc(square(0), square(5), crs = 4326)
    ),
    neighbor_shapes = st_sf(ADMIN = "Elsewhere", geometry = st_sfc(square(10), crs = 4326)),
    national_imagery = list(url = "basemap/national_imagery.jpg",
                            xmin = -100, ymin = -50, xmax = 900, ymax = 400),
    national_pan_bounds = c(lng1 = -1, lat1 = -1, lng2 = 12, lat2 = 2),
    # shaded relief image corners, EPSG:5070 metres, as step 10 stores them
    relief_bounds = c(xmin = -4e6, ymin = -1e6, xmax = 5e6, ymax = 5e6),
    pop_dev     = pop_dev,
    msa_pop_dev = dplyr::distinct(pop_dev, MSA_GEOID, MSA, year, msa_pop),
    dots = st_sf(
      MSA        = fixture_msas[c(1, 1, 1, 2)],
      change_cat = c("Gain", "Gain", "Loss", "Loss"),
      geometry   = st_sfc(lapply(1:4, function(i) st_point(c(i, 0.5))), crs = 4326)
    ),
    ppd = 500,
    lc_years = 1999:2001
  )
}
