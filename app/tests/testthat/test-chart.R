# Comparison chart (chart_msa_ranges.R), on the fixture's pop_dev, 2010 -> 2020:
#   Alpha-Beta  1,000 -> 1,500 people; total developed 100 -> 120 sq mi,
#               open space (21) 50 -> 45                         (grows)
#   Gamma       2,000 -> 1,800 people; total developed 200 -> 210 sq mi
#                                                                (loses people)

chart_data <- function(dev_class = 0L) {
  msa_range_data(fixture_app_data()$pop_dev, 2010, 2020, dev_class)
}

value_of <- function(d, msa, measure_name) {
  d$value[d$MSA == msa & d$measure == range_measures$title[range_measures$measure == measure_name]]
}

test_that("msa_range_data() computes the three measures between the chosen years", {
  d <- chart_data()
  expect_equal(nrow(d), 2 * 3)   # one row per MSA x measure
  expect_equal(levels(d$measure), range_measures$title)

  a <- fixture_msas[1]
  expect_equal(value_of(d, a, "land_added"), 20)
  expect_equal(value_of(d, a, "pop_added"), 500)
  expect_equal(value_of(d, a, "sqft_per_resident"), 20 * 5280^2 / 500)
  expect_equal(unique(d$short_name[d$MSA == a]), "Alpha")
})

test_that("land per new resident is NA where the MSA lost people", {
  d <- chart_data()
  g <- fixture_msas[2]
  expect_equal(value_of(d, g, "pop_added"), -200)
  expect_true(is.na(value_of(d, g, "sqft_per_resident")))
})

test_that("the land measures use the chosen land class", {
  d <- chart_data(21L)
  expect_equal(value_of(d, fixture_msas[1], "land_added"), -5)
  expect_equal(value_of(d, fixture_msas[1], "pop_added"), 500)   # population doesn't depend on class
})

test_that("the period label names the years and the land type", {
  expect_equal(chart_period_label(2005, 2015, 21L), "2005\u20132015 \u00b7 Open space")
  expect_equal(chart_period_label(1999, 2024, 0L), "1999\u20132024 \u00b7 Total developed")
})

test_that("msa_range_plot() builds a chart for a growing and a shrinking MSA", {
  d <- chart_data()
  for (msa in fixture_msas) {
    p <- msa_range_plot(d, msa)
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot_build(p))
  }
})

test_that("each line has a median tick, labelled 'median'", {
  d <- chart_data()
  p <- msa_range_plot(d, fixture_msas[1])
  expect_no_error(ggplot_build(p))
  # two layers (the tick and its label) are drawn from the medians table, one
  # row per measure at the median of the metros with a value
  median_layers <- Filter(function(l) is.data.frame(l$data) && "median" %in% names(l$data), p$layers)
  expect_length(median_layers, 2)
  expected <- tapply(d$value, d$measure, median, na.rm = TRUE)
  got <- median_layers[[1]]$data
  expect_equal(got$median[match(names(expected), as.character(got$measure))], unname(as.numeric(expected)))
  expect_equal(median_layers[[1]]$aes_params$colour, colors$median)
  expect_equal(median_layers[[2]]$aes_params$colour, colors$median)
})

test_that("svg_chart() draws the chart as SVG text in Marcellus, at the given size", {
  svg <- svg_chart(msa_range_plot(chart_data(), fixture_msas[1]), 800, 200)
  expect_match(svg, "^<svg", perl = TRUE)                  # no XML declaration
  expect_match(svg, "viewBox='0 0 800.00 200.00'", fixed = TRUE)
  expect_match(svg, ">median<", fixed = TRUE)              # labels are real text
  expect_match(svg, 'font-family: "Marcellus"', fixed = TRUE)
})

test_that("svg_chart() always closes its drawing device, even when the plot fails", {
  open_devices <- function() length(grDevices::dev.list())
  before <- open_devices()
  svg_chart(msa_range_plot(chart_data(), fixture_msas[1]), 200, 100)
  expect_equal(open_devices(), before)
  expect_error(svg_chart(stop("boom"), 200, 100), "boom")        # fails while building
  expect_equal(open_devices(), before)
  bad <- ggplot2::ggplot(data.frame(x = 1), ggplot2::aes(x, y = no_such_column)) + ggplot2::geom_point()
  expect_error(svg_chart(bad, 200, 100))                          # fails while drawing
  expect_equal(open_devices(), before)
})
