pop_dev <- fixture_pop_dev()
alpha_beta <- fixture_msas[1]
gamma      <- fixture_msas[2]

test_that("pop_change computes MSA and city change", {
  msa <- pop_change(pop_dev, alpha_beta, yr_min = 2010, yr_max = 2020)
  expect_equal(msa[c("from", "to", "diff", "pct")], list(from = 1000, to = 1500, diff = 500, pct = 50))

  city <- pop_change(pop_dev, alpha_beta, city_geoid = "002", yr_min = 2010, yr_max = 2020)
  expect_equal(city$diff, 300)
  expect_equal(city$pct, 50)
})

test_that("percent change is NA when the starting value is 0", {
  beta <- pop_change(pop_dev, alpha_beta, city_geoid = "001", yr_min = 2010, yr_max = 2020)
  expect_equal(beta$diff, 100)
  expect_true(is.na(beta$pct))
})

test_that("a missing year gives NA rather than an error", {
  out <- pop_change(pop_dev, alpha_beta, yr_min = 2010, yr_max = 2015)
  expect_true(is.na(out$to))
  expect_true(is.na(out$diff))
})

test_that("class_change uses MSA area for the MSA and city area for a city", {
  msa <- class_change(pop_dev, alpha_beta, yr_min = 2010, yr_max = 2020)
  expect_equal(msa$lc_class, c(0L, 21L))
  expect_equal(msa$diff, c(20, -5))
  expect_equal(msa$pct, c(20, -10))

  beta <- class_change(pop_dev, alpha_beta, city_geoid = "001", yr_min = 2010, yr_max = 2020)
  expect_equal(beta$diff, c(5, 5))
  expect_true(is.na(beta$pct[2]))   # open space started at 0
})

test_that("msa_cities orders cities as the MSA name lists them", {
  expect_equal(msa_cities(pop_dev, alpha_beta)$city_name, c("Alpha", "Beta"))
})

test_that("msa_map_data gives a ratio only where population grew", {
  fx  <- fixture_app_data()
  out <- msa_map_data(fx$msa_centers, fx$msa_pop_dev, fx$pop_dev, 2010, 2020, dev_class = 0L)

  ab <- out[out$MSA == alpha_beta, ]
  expect_equal(ab$pop_diff, 500)
  expect_equal(ab$area_diff, 20)
  expect_equal(ab$ratio, 20 / 500)

  g <- out[out$MSA == gamma, ]
  expect_equal(g$pop_diff, -200)
  expect_true(is.na(g$ratio))

  expect_equal(ratio_ok(out), c(TRUE, FALSE))
})

test_that("circle_radius scales to 4-12px and handles degenerate input", {
  expect_equal(circle_radius(c(-10, 0, 5)), c(12, 4, 8))
  expect_equal(circle_radius(c(NA, NA)), c(6, 6))
  expect_equal(circle_radius(c(3, 3)), c(8, 8))
})

test_that("circle_colors: red marks loss for population and open space only", {
  expect_equal(
    circle_colors(c(5, -5, 0, NA), "pop", 0L),
    c(colors$pop_gain, colors$pop_loss, colors$no_change, colors$no_change)
  )
  expect_equal(circle_colors(c(5, -5), "area", 21L), c(colors$area, colors$pop_loss))
  expect_equal(circle_colors(c(5, -5), "area", 22L), c(colors$area, colors$area))
})

test_that("formatters print NA as N/A and add separators", {
  expect_equal(fmt_int(c(1234567, NA)), c("1,234,567", "N/A"))
  expect_equal(fmt_num(1234.56), "1,234.6")
  expect_equal(fmt_pct(c(12.345, NA)), c("12.3%", "N/A"))
})

test_that("change_table has a population row plus one row per class", {
  html <- as.character(change_table(
    pop_change(pop_dev, alpha_beta, yr_min = 2010, yr_max = 2020),
    class_change(pop_dev, alpha_beta, yr_min = 2010, yr_max = 2020),
    2010, 2020
  ))
  expect_match(html, "Value in 2010")
  expect_match(html, "1,500")
  expect_match(html, "Open Space")
  expect_equal(lengths(regmatches(html, gregexpr("<tr>", html))), 1 + 3)  # header + 3 rows
})

test_that("basemap_url carries the CARTO key", {
  expect_match(basemap_url("abc123"), "rastertiles/light_all/\\{z\\}/\\{x\\}/\\{y\\}\\.png\\?key=abc123$")
  expect_warning(basemap_url(""), "CARTO_API_KEY")
})
