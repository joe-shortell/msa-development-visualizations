alpha_beta <- fixture_msas[1]
gamma      <- fixture_msas[2]

test_that("msa_map_data gives a density wherever land grew, negative for population loss", {
  fx  <- fixture_app_data()
  out <- msa_map_data(fx$msa_centers, fx$msa_pop_dev, fx$pop_dev, 2010, 2020, dev_class = 0L)

  ab <- out[out$MSA == alpha_beta, ]
  expect_equal(ab$pop_diff, 500)
  expect_equal(ab$area_diff, 20)
  expect_equal(ab$density, 500 / 20)

  g <- out[out$MSA == gamma, ]
  expect_equal(g$pop_diff, -200)
  expect_equal(g$density, -200 / 10)     # lost people while adding 10 sq mi

  # open space shrank in Alpha-Beta: people grew, land didn't -> no density
  os <- msa_map_data(fx$msa_centers, fx$msa_pop_dev, fx$pop_dev, 2010, 2020, dev_class = 21L)
  expect_equal(os$area_diff[os$MSA == alpha_beta], -5)
  expect_true(is.na(os$density[os$MSA == alpha_beta]))
})

test_that("circle_radius makes circle area proportional to |x|", {
  r <- circle_radius(c(-100, 25, 0, NA))
  expect_equal(r[1], 22)
  expect_equal(r[2], 11)                       # a quarter of the value -> half the radius
  expect_equal(r[3:4], c(3, 3))                # zero and NA get the floor
  expect_equal(circle_radius(c(0, 0)), c(3, 3))
  expect_equal(circle_radius(c(NA, NA)), c(3, 3))
  expect_equal(circle_radius(25, x_max = 100), 11)   # legend reuses the map's scale
})

test_that("diverging_position is linear on each side of zero", {
  expect_equal(diverging_position(c(-50, -25, 0, 500, 1000, NA), lo = -50, hi = 1000), c(-1, -0.5, 0, 0.5, 1, NA))
  # no negative values: lo = 0 must not divide by zero
  expect_equal(diverging_position(c(0, 10), lo = 0, hi = 10), c(0, 1))
})

test_that("bar_fraction places values proportionally along the bar", {
  expect_equal(bar_fraction(c(-311, 0, 16199), lo = -311, hi = 16199), c(0, 311 / 16510, 1))
  expect_equal(bar_fraction(0, lo = 0, hi = 0), 0.5)   # degenerate range
})

test_that("legend stops and ticks follow the linear bar", {
  pal <- density_palette(c(-311, 1000, 16199))
  expect_equal(pal$stops$fraction[1], 0)
  expect_equal(tail(pal$stops$fraction, 1), 1)
  zero <- bar_fraction(0, pal$lo, pal$hi)
  expect_equal(pal$stops$colour[pal$stops$fraction == zero], pal$fill(0))
  # a stop's colour is the colour a circle with that value would get
  val <- pal$lo + pal$stops$fraction * (pal$hi - pal$lo)
  expect_equal(pal$fill(val), pal$stops$colour)

  expect_true(all(pal$ticks > pal$lo & pal$ticks < pal$hi))
  expect_false(0 %in% pal$ticks)
})

test_that("density_palette colours loss, zero and growth apart and greys out NA", {
  pal  <- density_palette(c(-20, 0, 25, 1000, NA))
  fill <- pal$fill(c(-20, 0, 1000, NA))
  expect_equal(fill[4], colors$no_change)
  expect_length(unique(fill[1:3]), 3)
  # the extremes match the two ends of the legend bar
  expect_equal(fill[c(1, 3)], pal$stops$colour[c(1, nrow(pal$stops))])
  expect_equal(c(pal$min, pal$max), c(-20, 1000))
  expect_true(pal$has_values)

  growth_only <- density_palette(c(10, 100))
  expect_true(is.na(growth_only$min))

  none <- density_palette(c(NA_real_, NA_real_))
  expect_false(none$has_values)
  expect_equal(none$fill(c(1, NA)), rep(colors$no_change, 2))
})

test_that("formatters print NA as N/A and add separators", {
  expect_equal(fmt_int(c(1234567, NA)), c("1,234,567", "N/A"))
  expect_equal(fmt_num(1234.56), "1,234.6")
})

test_that("basemap_url carries the CARTO key", {
  expect_match(basemap_url("abc123"), "rastertiles/dark_all/\\{z\\}/\\{x\\}/\\{y\\}\\.png\\?key=abc123$")
  expect_warning(basemap_url(""), "CARTO_API_KEY")
})
