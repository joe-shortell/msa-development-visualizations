### national side panel -------------------------------------------------------------
# A panel at the left of the national view, beside the map and the controls panel
# beneath it: a short guide to reading the map, then the data sources.
#
# Layout of the national view (app.R):
#   title                               starts at the side panel's left edge
#   side panel | gap | map              one centred row (national_row())
#              |     | controls panel
# The right-hand column keeps the national map's usual width, msa_pair_width()
# (the MSA view's two panels together), and the side panel is added to its
# left, so the national view is side_panel_width + gap wider than the MSA view.

# Width of the side panel, and the gap between it and the map.
side_panel_width  <- "300px"
side_panel_gap_px <- 16

# Width of the national view's row (side panel + gap + map column), as a CSS
# expression, shared by the row and the title above it.
# msa_pair_width() contains a 50% measured against its parent, so it is used
# only here, on the row, whose parent is the page (as for the MSA view); the
# map column inside the row takes what is left (flex:1) instead of reusing it.
# min(100%, ...) keeps the row on the page in a narrow window, where the map
# column shrinks and the side panel keeps its width.
national_row_width <- function() {
  paste0("min(100%, calc(", side_panel_width, " + ", side_panel_gap_px, "px + ", msa_pair_width(), "))")
}

# The row: side panel at the left, `main` (the map over the controls panel)
# filling the rest. align-items:stretch makes the side panel as tall as the map
# and controls together.
national_row <- function(main) {
  shiny::div(
    style = paste0(
      "width:", national_row_width(), "; margin:0 auto; display:flex; align-items:stretch;",
      " gap:", side_panel_gap_px, "px;"
    ),
    national_side_panel(),
    # min-width:0 lets the column shrink below the map's own width on a
    # narrow window, rather than pushing the row off the page.
    shiny::div(style = "flex:1; min-width:0;", main)
  )
}

# The side panel itself: guidance, then the data sources at the bottom.
# flex:none + an explicit width guard it from being squeezed by the map beside
# it; margin:0 drops the well's default bottom margin so its bottom lines up with
# the controls panel's; overflow-y:auto scrolls the text if the window is short.
national_side_panel <- function() {
  shiny::wellPanel(
    style = paste0(
      "flex:none; width:", side_panel_width, "; margin:0; box-sizing:border-box;",
      " display:flex; flex-direction:column; justify-content:space-between;",
      " overflow-y:auto;"
    ),
    guidance(),
    data_sources()
  )
}

# The introduction at the top of the side panel: three paragraphs at 17.5px
# (1.5px under the darkly theme's 19px h4). The last two are placeholders.
side_panel_text_px <- 17.5

guidance <- function() {
  para <- function(text, last = FALSE) {
    shiny::p(style = if (last) "margin-bottom:0;" else "margin-bottom:12px;", text)
  }
  shiny::tags$div(
    style = paste0("font-size:", side_panel_text_px, "px; line-height:1.35;"),
    para("In the first quarter of the 21st Century, the U.S. population grew by 60 million. Much of this growth occured in the country's largest metro-areas, centers of industry, institutions and most importatntly, jobs."),
    para("This map shows how the country's largest metro areas have absorbed population differently, visualizing the changes in their physical footprints and measuring how efficiently they have grown."),
    para("Select an MSA to view more data and to see how it compares to others.", last = TRUE)
  )
}

# Every source behind the app, at the bottom of the side panel: the only place
# the app credits its data (the MSA view's ribbons carry no credits). Each line
# says what the source is used for. The CARTO / OpenStreetMap credit also stays
# on the dot map's tiles, where their terms require it.
data_sources <- function() {
  src <- function(..., last = FALSE) {
    shiny::p(style = if (last) "margin-bottom:0;" else NULL, ...)
  }
  shiny::tags$div(
    shiny::h5("Data Sources"),
    shiny::tags$div(
      style = "font-size: 0.8rem; line-height: 1.2;",
      # population
      src("U.S. Census Bureau, Population Estimates Program, county population estimates, ",
          "1999\u20132024 (1990s, 2000s and 2010s intercensal series; Vintage 2025). ",
          "Metro populations on the national map and in the metro charts."),
      src("U.S. Office of Management and Budget, Metropolitan Statistical Area delineations, July 2023. ",
          "The counties that make up each metro."),
      src("U.S. Census Bureau, 2000 Census, Summary File 1, and 2020 Census, Redistricting Data ",
          "(P.L. 94-171). Block populations for the dot maps."),
      # The crosswalks' terms of use require this citation. Version as cited in
      # README.md; check it against NHGIS's citation page when updating.
      src("Steven Manson, Jonathan Schroeder, David Van Riper, Katherine Knowles, Tracy Kugler, ",
          "Finn Roberts, and Steven Ruggles. IPUMS National Historical Geographic Information System: ",
          "Version 19.0. Minneapolis, MN: IPUMS, 2024. http://doi.org/10.18128/D050.V19.0. ",
          "Block-to-block-group crosswalks for the dot maps."),
      src("U.S. Census Bureau, American Community Survey 1-Year Estimates, 2024. ",
          "Ranking the 30 largest metros."),
      src("U.S. Census Bureau, TIGER/Line and cartographic boundary files. ",
          "Metro, state, county, block group and water boundaries."),
      # land cover
      src("U.S. Geological Survey, Annual National Land Cover Database (NLCD), Collection 1.0, ",
          "Land Cover, 1999\u20132024. Developed land and the land cover images."),
      # basemaps
      src("Natural Earth (public domain). Shaded relief and the outlines of Canada and Mexico ",
          "on the national map."),
      src("Basemap tiles \u00a9 OpenStreetMap contributors, \u00a9 CARTO.", last = TRUE)
    )
  )
}
