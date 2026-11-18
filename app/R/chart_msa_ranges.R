### MSA comparison chart -----------------------------------------------------------
# Where the selected MSA ranks among all 30 metros, shown in the bar under the
# MSA view's two maps (mod_msa_view.R).
#
# The chart is three stacked horizontal number lines, one per measure of
# change over the years chosen on the national map (a sky-blue tick on each marks
# the median metro):
#   1. new developed land          square miles added
#   2. population growth           people added (negative = people lost)
#   3. land per new resident       square feet of new developed land per new
#                                  resident
# Each line runs from the lowest metro to the highest. Every metro is a small
# grey dot on it, and the selected metro is a larger dot in an accent colour,
# labelled with its name and value. That shows at a glance whether a metro
# grew a lot or a little, and whether it used land sparingly or generously
# compared with its peers.
#
# The input is app_data$pop_dev, the national map's table (step 08 of the
# pipeline): one row per MSA x year (1999-2024) x land class, with
#   msa_pop        July 1 population, the MSA's counties' Census estimates
#                  summed on 2023 MSA boundaries (step 05)
#   msa_area_mi2   developed land of that class inside the MSA (step 03)
# The years and land class come from the national map's controls (its year
# range and land type), so the chart always describes the same change as the
# circles the reader clicked. (The dot map beside it stays 2000-2020: it uses
# census block counts, which exist only for census years.)
#
# The code is split into two functions so each step can be checked on its own:
#   msa_range_data()  table in, table out: computes the three measures
#   msa_range_plot()  table in, ggplot out: draws the chart
# svg_chart() then draws the ggplot as SVG for the page (section 3).
# Both use ordinary dplyr, tidyr and ggplot2 calls. app.R (and the tests'
# setup.R) load those packages with library() before these functions run.

# Square feet in one square mile (5,280 ft x 5,280 ft), for converting new
# developed land into square feet per new resident.
sqft_per_mi2 <- 5280^2

# The three measures in the order they are stacked (top to bottom), with the
# title shown beside each line and the unit printed after the selected value.
# `measure` is the short name used inside the code.
range_measures <- data.frame(
  measure = c("land_added", "pop_added", "sqft_per_resident"),
  title   = c("New developed land", "Population growth", "Developed land\nper new resident"),
  unit    = c("sq mi", "people", "sq ft")
)

### 1. the data -------------------------------------------------------------------

# Turn the national map's table into one row per MSA x measure, ready to plot.
#
# Picks out each MSA's population and developed land (of land class
# `dev_class`: 0 = all developed, 21-24 = one NLCD class) in the years
# `yr_from` and `yr_to`, computes the three changes between them, then
# reshapes the table from wide (a column per measure) to long (a row per
# measure). ggplot draws one panel per measure from that long table.
#
# Returns columns:
#   MSA         full name, used to find the selected metro
#   short_name  the first city in the name, for the label ("Austin")
#   measure     a factor in the stacking order, holding the panel titles
#   unit        printed after the value in the label
#   value       the number placed on the line (NA = not defined, see below)
msa_range_data <- function(pop_dev, yr_from, yr_to, dev_class = 0L) {
  pop_dev |>
    # Step 0: one row per MSA with its start and end values side by side.
    # Keep the chosen land class in the two chosen years, then spread the
    # years into columns: pivot_wider() turns the two rows per MSA (one per
    # year) into one row with msa_pop_from, msa_pop_to, msa_area_mi2_from and
    # msa_area_mi2_to.
    filter(lc_class == dev_class, year %in% c(yr_from, yr_to)) |>
    mutate(when = if_else(year == yr_from, "from", "to")) |>
    select(MSA, when, msa_pop, msa_area_mi2) |>
    pivot_wider(names_from = when, values_from = c(msa_pop, msa_area_mi2)) |>
    rename(
      pop_from = msa_pop_from,      dev_from = msa_area_mi2_from,
      pop_to   = msa_pop_to,        dev_to   = msa_area_mi2_to
    ) |>
    # Step 1: compute the three changes for each MSA.
    # land_added and pop_added are the plain differences, end year minus start.
    # sqft_per_resident divides the new land (converted to square feet) by the
    # new people. That ratio only means something when the metro gained
    # people: with a loss (Detroit, Pittsburgh) it would be negative, and with
    # no change it would divide by zero. In those cases it is set to NA, so
    # the metro gets no dot on that line (and, if selected, a note instead).
    mutate(
      land_added        = dev_to - dev_from,
      pop_added         = pop_to - pop_from,
      sqft_per_resident = if_else(
        pop_added > 0,
        land_added * sqft_per_mi2 / pop_added,
        NA_real_
      )
    ) |>
    # Step 2: a short label name, the first city in the MSA's name.
    # The regular expression deletes everything from the first hyphen or comma
    # onward: "Austin-Round Rock-San Marcos, TX Metro Area" becomes "Austin",
    # "St. Louis, MO-IL Metro Area" becomes "St. Louis".
    mutate(short_name = sub("[-,].*", "", MSA)) |>
    # Step 3: reshape from wide to long.
    # The three measure columns become two columns: `measure` (which one it
    # is) and `value` (its number). Each MSA becomes three rows.
    select(MSA, short_name, land_added, pop_added, sqft_per_resident) |>
    pivot_longer(
      cols      = c(land_added, pop_added, sqft_per_resident),
      names_to  = "measure",
      values_to = "value"
    ) |>
    # Step 4: attach each measure's title and unit from range_measures.
    # Then turn `measure` into a factor of the titles, with its levels in
    # range_measures' order. ggplot stacks the panels in factor-level order,
    # so this fixes the top-to-bottom order and labels the panels in one go.
    left_join(range_measures, by = "measure") |>
    mutate(measure = factor(title, levels = range_measures$title)) |>
    select(MSA, short_name, measure, unit, value)
}

# What the chart shows: the years and the land type. The MSA view prints it
# above the chart, as page text styled like the map titles (mod_msa_view.R),
# e.g. "2005–2015 · Open space". The land type's name is its label in the
# national map's land type list (dev_class_choices, constants.R).
chart_period_label <- function(yr_from, yr_to, dev_class = 0L) {
  land <- names(dev_class_choices)[dev_class_choices == dev_class]
  paste0(yr_from, "\u2013", yr_to, " \u00b7 ", land)
}

### 2. the chart ------------------------------------------------------------------

# Draw the three number lines with the selected MSA highlighted.
#
# `range_data` is msa_range_data()'s output; `selected_msa` is the full name
# of the metro to highlight. Every layer is built from small tables made with
# dplyr below, so each one can be printed and inspected on its own.
msa_range_plot <- function(range_data, selected_msa) {

  # Step 1: the ends of each line, the lowest and highest metro per measure.
  # na.rm = TRUE skips the metros with no defined value (NA) on that measure.
  range_ends <- range_data |>
    group_by(measure) |>
    summarise(
      low  = min(value, na.rm = TRUE),
      high = max(value, na.rm = TRUE),
      .groups = "drop"
    )

  # Step 2: the selected metro's rows (one per measure), with its label.
  # Labels near either end of a line would run off the panel if centred on
  # the dot. So `position` (0 = the line's left end, 1 = its right end) picks
  # the alignment: labels in the left fifth start at the dot (hjust = 0),
  # labels in the right fifth end at it (hjust = 1), and the rest are centred.
  # label_comma() formats numbers with thousands separators ("1,033,666").
  selected <- range_data |>
    filter(MSA == selected_msa, !is.na(value)) |>
    left_join(range_ends, by = "measure") |>
    mutate(
      position = (value - low) / (high - low),
      hjust    = case_when(
        position < 0.2 ~ 0,
        position > 0.8 ~ 1,
        TRUE           ~ 0.5
      ),
      label    = paste0(short_name, ": ", scales::label_comma(accuracy = 1)(value), " ", unit)
    )

  # Step 3: a note for any measure where the selected metro has no value.
  # That only happens on land per new resident when the metro lost people. The
  # note sits at the middle of that line, where the dot would otherwise be.
  undefined <- range_data |>
    filter(MSA == selected_msa, is.na(value)) |>
    left_join(range_ends, by = "measure") |>
    mutate(
      middle = (low + high) / 2,
      label  = paste0(short_name, ": population fell, so not defined")
    )

  # Step 4: a dashed zero line on any measure whose line crosses zero
  # (population growth, which has metros on both sides), so gains and losses
  # can be told apart.
  zero_lines <- range_ends |>
    filter(low < 0, high > 0)

  # Step 4b: the median metro on each line, for a sense of the typical metro.
  # median() over all metros with a value (na.rm = TRUE skips the NAs, as in
  # Step 1). Marked with a short sky-blue tick (colors$median) across the line,
  # labelled "median" in the same blue just beneath it; below the line, so it never collides with the selected
  # metro's label above.
  medians <- range_data |>
    group_by(measure) |>
    summarise(median = median(value, na.rm = TRUE), .groups = "drop")

  # Step 5: build the plot, one layer at a time.
  # Every layer sits at y = 0, so each panel is a single horizontal line. The
  # y axis has no meaning and is hidden; its limits leave room above the line
  # for the selected metro's label and below it for the "median" label.
  ggplot() +
    # the line from the lowest to the highest metro
    geom_segment(
      data = range_ends,
      aes(x = low, xend = high, y = 0, yend = 0),
      colour = "#777777", linewidth = 0.6
    ) +
    # zero marker, where the line crosses zero
    geom_vline(
      data = zero_lines,
      aes(xintercept = 0),
      colour = "#999999", linetype = "dashed", linewidth = 0.4
    ) +
    # every metro, as a small grey dot (metros with NA have no dot)
    geom_point(
      data = filter(range_data, !is.na(value)),
      aes(x = value, y = 0),
      colour = "#AAAAAA", size = 2, alpha = 0.8
    ) +
    # the median: a short sky-blue tick across the line, over the grey dots
    geom_segment(
      data = medians,
      aes(x = median, xend = median, y = -0.25, yend = 0.25),
      colour = colors$median, linewidth = 0.8
    ) +
    # its label, small and in the same blue, just under the tick
    geom_text(
      data = medians,
      aes(x = median, y = -0.32, label = "median"),
      colour = colors$median, size = 2.8, vjust = 1, family = chart_font_family
    ) +
    # the selected metro, as a larger dot in the accent colour, drawn on top
    geom_point(
      data = selected,
      aes(x = value, y = 0),
      colour = colors$selected, size = 3.5
    ) +
    # its name and value, just above its dot. Text layers name their font
    # themselves (the theme's base_family below doesn't reach them): Marcellus,
    # the app's font (chart_font_family, registered by register_chart_font()).
    # It has one weight, so the label is set apart by its amber colour, not bold.
    geom_text(
      data = selected,
      aes(x = value, y = 0.45, label = label, hjust = hjust),
      colour = colors$selected, size = 3.6, vjust = 0, family = chart_font_family
    ) +
    # the "not defined" note, where a metro has no value
    geom_text(
      data = undefined,
      aes(x = middle, y = 0.45, label = label),
      colour = colors$selected, size = 3.6, vjust = 0, family = chart_font_family
    ) +
    # One panel per measure, stacked in one column. scales = "free_x" gives
    # each panel its own x range, since the measures have different units.
    # strip.position = "left" puts each panel's title to the left of its line
    # instead of above it, which saves height in the short chart bar.
    facet_wrap(~ measure, ncol = 1, scales = "free_x", strip.position = "left") +
    scale_x_continuous(labels = scales::label_comma()) +
    scale_y_continuous(limits = c(-0.85, 1.3)) +
    labs(x = NULL, y = NULL) +
    # Step 6: styling, to match the app's dark "darkly" theme.
    # theme_minimal() is a plain starting point. The overrides make the
    # background transparent (the chart bar's colour shows through), the text
    # light, and remove the y axis and all gridlines except the vertical lines
    # at the x-axis ticks. base_family sets the font for all the theme's text
    # (panel titles and axis numbers) to the app's Marcellus.
    theme_minimal(base_size = 12, base_family = chart_font_family) +
    theme(
      plot.background    = element_rect(fill = "transparent", colour = NA),
      panel.background   = element_rect(fill = "transparent", colour = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(colour = "#444444", linewidth = 0.3),
      axis.text.y        = element_blank(),
      axis.text.x        = element_text(colour = "#BBBBBB", size = 9),
      strip.placement    = "outside",
      strip.text.y.left  = element_text(colour = "#FFFFFF", angle = 0, hjust = 1, size = 11),
      panel.spacing.y    = grid::unit(2, "pt"),
      plot.margin        = margin(2, 12, 2, 4)
    )
}

### 3. sending it to the page -------------------------------------------------------

# Draw a chart as SVG, ready to put in the page.
#
# SVG keeps the lines as shapes and the labels as real text, which the browser
# draws in Marcellus (loaded on the page), so the small text stays sharp at any
# size or zoom; a PNG from renderPlot() was rasterised and looked soft.
# `width_px` and `height_px` are the chart slot's size in CSS pixels (reported
# by Shiny, mod_msa_view.R). svglite works in inches at 72 per inch, so the
# drawing has one unit per CSS pixel and the ggplot's point sizes come out as
# they did in the PNG. It measures each label with systemfonts (the bundled
# Marcellus, register_chart_font()) to place it.
# svglite also sizes the <svg> in points (a third larger than pixels); the
# chart slot's CSS (msaViewUI()) makes it fill the slot instead, which matches
# its viewBox exactly. The XML declaration is dropped: inside a page it isn't
# allowed.
#
# The drawing device must always be closed again: R allows only 63 open at
# once, and each one left open stays open for the life of the app. So the plot
# is built before the device is opened (force(); `plot` is otherwise built
# lazily, inside print(), where a req() or validate() that stops it would skip
# the closing step), and on.exit() closes the device if drawing fails.
svg_chart <- function(plot, width_px, height_px) {
  force(plot)
  svg    <- svglite::svgstring(width = width_px / 72, height = height_px / 72, bg = "transparent")
  dev    <- grDevices::dev.cur()
  closed <- FALSE
  on.exit(if (!closed) grDevices::dev.off(dev), add = TRUE)
  print(plot)
  grDevices::dev.off(dev)
  closed <- TRUE
  sub("^<\\?xml[^>]*>\\s*", "", as.character(svg()))
}
