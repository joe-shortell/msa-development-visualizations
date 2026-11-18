### change calculations --------------------------------------------------------
# Pure functions: data in, numbers out. No reactivity, so tests/testthat can
# call them directly.
#
# `pop_dev` is the long table from step 08: one row per MSA x year x NLCD class
# (msa_pop, msa_area_mi2), with the MSA's population repeated on each class row.
# `msa_pop_dev` is one row per MSA x year (msa_pop): July 1 county population
# estimates summed on 2023 MSA boundaries, 1999-2024 (step 05).

### national map circles -------------------------------------------------------

# One row per MSA centroid with pop_diff, area_diff (for one NLCD class) and
# density = new residents per square mile of new developed land.
#
# Density measures how efficiently new land absorbed new people. It is negative
# where the MSA lost people while still developing land. It is NA only where
# the class did not gain land, since a zero or negative denominator would give
# an infinite or sign-flipped value.
msa_map_data <- function(centers, msa_pop_dev, pop_dev, yr_min, yr_max, dev_class) {
  pop_at <- function(yr, nm) {
    msa_pop_dev |>
      dplyr::filter(year == yr) |>
      dplyr::transmute(MSA, "{nm}" := msa_pop)
  }
  pop <- dplyr::inner_join(pop_at(yr_min, "pop_from"), pop_at(yr_max, "pop_to"), by = "MSA") |>
    dplyr::mutate(pop_diff = pop_to - pop_from)

  area_at <- function(yr, nm) {
    pop_dev |>
      dplyr::filter(year == yr, lc_class == dev_class) |>
      dplyr::group_by(MSA) |>
      dplyr::summarise("{nm}" := dplyr::first(msa_area_mi2), .groups = "drop")
  }
  area <- dplyr::full_join(area_at(yr_min, "area_from"), area_at(yr_max, "area_to"), by = "MSA") |>
    dplyr::mutate(area_diff = area_to - area_from)

  centers |>
    dplyr::left_join(pop,  by = "MSA") |>
    dplyr::left_join(area, by = "MSA") |>
    dplyr::mutate(
      density = dplyr::if_else(
        !is.na(pop_diff) & !is.na(area_diff) & area_diff > 0,
        pop_diff / area_diff,
        NA_real_
      )
    )
}

# Circle radius in px, so that circle AREA is proportional to |x|.
#
# Graduated-symbol convention: readers judge circles by area, so the radius
# grows with the square root of the value, r = r_max * sqrt(|x| / max|x|).
# Values too small to see are floored at r_min (also used for NA and for an
# all-zero input). `x_max` lets the size legend reuse the map's scale.
circle_radius <- function(x, x_max = NULL, r_min = 3, r_max = 22) {
  x <- abs(x)
  if (is.null(x_max)) x_max <- if (all(is.na(x))) NA else max(x, na.rm = TRUE)
  if (!is.finite(x_max) || x_max == 0) return(rep(r_min, length(x)))
  r <- r_max * sqrt(x / x_max)
  r[is.na(r)] <- r_min
  pmax(r, r_min)
}

# Colour position on a -1..1 diverging scale: linear in value, zero at 0.
#
# Positive values are divided by the largest value (hi) and negative ones by
# the size of the most negative (|lo|), so each side reaches its full colour at
# its own extreme. That keeps a lone population-loss metro clearly red even
# when it is tiny next to the largest gain. A side with no values (lo = 0 or
# hi = 0) is left alone rather than divided by zero.
diverging_position <- function(x, lo, hi) {
  pos <- rep(NA_real_, length(x))
  up  <- which(x > 0)
  dn  <- which(x < 0)
  pos[which(x == 0)] <- 0
  if (hi > 0) pos[up] <- pmin(x[up] / hi, 1)
  if (lo < 0) pos[dn] <- -pmin(x[dn] / lo, 1)
  pos
}

# Where a value falls along the legend bar, as a 0..1 fraction of its width.
#
# Strictly linear, (x - lo) / (hi - lo), so the bar's length on each side of
# zero is proportional to the range of values on that side.
bar_fraction <- function(x, lo, hi) {
  if (hi == lo) return(rep(0.5, length(x)))
  (x - lo) / (hi - lo)
}

# Colour scale for density: red (population loss) through pale yellow (zero
# growth) to blue (growth), linear in value, plus what the legend bar needs.
#
# The extremes are rebuilt from the values passed in, so the scale follows the
# year range and land class. Returns
#   fill(d)    hex colours, grey (colors$no_change) where d is NA
#   lo, hi     the bar's ends: min(0, values) and max(0, values)
#   min, max   the most negative and most positive density (NA if that side
#              has no values), printed at the ends of the legend
#   stops      data frame of fraction (0..1 along the bar) and colour, for
#              the legend's CSS gradient. Stops sit at evenly spaced colour
#              positions on each side of zero, so the bar shows exactly the
#              colours the circles get.
#   ticks      round numbers to mark under the bar, excluding zero (marked
#              separately) and values too close to the ends to fit a label
#   has_values FALSE when every density is NA, so the legend can be skipped
density_palette <- function(density) {
  ok  <- is.finite(density)
  lo  <- if (any(ok & density < 0)) min(density[ok]) else 0
  hi  <- if (any(ok & density > 0)) max(density[ok]) else 0
  pal <- leaflet::colorNumeric(density_colors, domain = c(-1, 1), na.color = colors$no_change)

  # colour positions on each side of zero, converted back to values
  neg_pos <- if (lo < 0) seq(-1, -0.25, by = 0.25) else numeric(0)
  pos_pos <- if (hi > 0) seq(0.125, 1, by = 0.125) else numeric(0)
  stop_pos <- c(neg_pos, 0, pos_pos)
  stop_val <- ifelse(stop_pos < 0, -stop_pos * lo, stop_pos * hi)

  ticks <- pretty(c(lo, hi), n = 4)
  frac  <- bar_fraction(ticks, lo, hi)
  ticks <- ticks[ticks != 0 & frac > 0.15 & frac < 0.85]

  list(
    fill       = function(d) pal(diverging_position(d, lo, hi)),
    lo         = lo,
    hi         = hi,
    min        = if (lo < 0) lo else NA_real_,
    max        = if (hi > 0) hi else NA_real_,
    stops      = data.frame(fraction = bar_fraction(stop_val, lo, hi), colour = pal(stop_pos)),
    ticks      = ticks,
    has_values = any(ok)
  )
}
