### change between two years ---------------------------------------------------
# Pure functions: data in, numbers out. No reactivity, so tests/testthat can
# call them directly.
#
# `pop_dev` is the long table from script 07: one row per MSA x city x year x
# NLCD class, with the MSA totals repeated on every row of that MSA.

pct_change <- function(from, to) {
  ifelse(is.na(from) | from == 0, NA_real_, 100 * (to - from) / from)
}

# Population change for an MSA, or for one of its cities when city_geoid is
# given. Returns list(from, to, diff, pct); NA where a year is missing.
pop_change <- function(pop_dev, msa, city_geoid = NULL, yr_min, yr_max) {
  df  <- pop_dev[which(pop_dev$MSA == msa), ]
  col <- "msa_pop"
  if (!is.null(city_geoid)) {
    df  <- df[which(df$CITY_GEOID == city_geoid), ]
    col <- "city_pop"
  }

  at <- function(yr) {
    v <- df[[col]][df$year == yr]
    if (length(v) == 0) NA_real_ else v[1]
  }

  from <- at(yr_min)
  to   <- at(yr_max)
  list(from = from, to = to, diff = to - from, pct = pct_change(from, to))
}

# Developed area by NLCD class, for an MSA or one of its cities.
# One row per class: lc_class, from, to, diff, pct.
class_change <- function(pop_dev, msa, city_geoid = NULL, yr_min, yr_max) {
  area_col <- if (is.null(city_geoid)) "msa_area_mi2" else "city_area_mi2"

  df <- pop_dev |>
    dplyr::filter(
      MSA == .env$msa,
      year %in% c(yr_min, yr_max),
      lc_class %in% dev_classes
    )
  if (!is.null(city_geoid)) {
    df <- dplyr::filter(df, CITY_GEOID == .env$city_geoid)
  }

  area_at <- function(yr, nm) {
    df |>
      dplyr::filter(year == yr) |>
      dplyr::group_by(lc_class) |>
      dplyr::summarise("{nm}" := dplyr::first(.data[[area_col]]), .groups = "drop")
  }

  dplyr::full_join(area_at(yr_min, "from"), area_at(yr_max, "to"), by = "lc_class") |>
    dplyr::mutate(diff = to - from, pct = pct_change(from, to)) |>
    dplyr::arrange(lc_class)
}

# The MSA's cities, in the order the MSA name lists them (city_rank, set when
# script 01 matches cities to the title).
msa_cities <- function(pop_dev, msa) {
  pop_dev |>
    dplyr::filter(MSA == .env$msa) |>
    dplyr::distinct(CITY_GEOID, city_name, city_rank) |>
    dplyr::arrange(city_rank, city_name)
}

### national map circles -------------------------------------------------------

# One row per MSA centroid with pop_diff, area_diff (for one NLCD class) and
# ratio = developed square miles per new resident.
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
      ratio = dplyr::if_else(
        is.na(pop_diff) | pop_diff <= 0,
        NA_real_,
        area_diff / pop_diff
      )
    )
}

# MSAs that get a ratio circle: they gained people and did not lose developed
# land. The rest with a ratio are drawn as an X.
ratio_ok <- function(circles) {
  !is.na(circles$ratio) &
    !is.na(circles$area_diff) &
    circles$pop_diff > 0 &
    circles$area_diff >= 0
}

# Scale |x| to a 4-12px radius.
circle_radius <- function(x) {
  x <- abs(x)
  if (all(is.na(x))) return(rep(6, length(x)))
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(8, length(x)))
  4 + 8 * (x - rng[1]) / diff(rng)
}

# Population: green for growth, red for loss.
# Developed land: purple, except open space (21), where a loss is red.
circle_colors <- function(val, metric, dev_class) {
  shrink_col <- if (metric == "pop" || dev_class == 21L) colors$pop_loss else colors$area
  grow_col   <- if (metric == "pop") colors$pop_gain else colors$area

  ifelse(
    is.na(val) | val == 0, colors$no_change,
    ifelse(val > 0, grow_col, shrink_col)
  )
}
